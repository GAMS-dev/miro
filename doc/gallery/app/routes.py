from app import app, mail
from flask import request, render_template, jsonify
from flask_mail import Message
import re
import os
from app.app_data import AppData
from werkzeug.utils import secure_filename
import logging

REQUIRED_API_VERSION = "1"
appsDatabase = os.path.join("data", "apps.json")

EMAIL_REGEX = re.compile(r"[^@]+@[^@]+\.[^@]+")

logging.basicConfig(filename=os.path.join("data", "gallery.log"), level=logging.DEBUG)

app_data = AppData(appsDatabase)


class ValidationError(Exception):
    pass


def allowed_file(filename):
    return "." in filename and filename.rsplit(".", 1)[1].lower() in {"miroapp"}


@app.route("/api/vote-up", methods=["POST"])
def voteup():
    logging.info(f"New upvote request received for app id: {request.form['id']}")
    try:
        app_data.increment_upvote_count(request.form["id"])
    except ValueError:
        logging.warning(
            f"App id: {request.form['id']} of upvote request does not exist."
        )
        return "Error", 422

    return "OK", 200


@app.route("/api/refresh-data", methods=["POST"])
def refresh_data():
    logging.info("Refresh app data request received.")
    try:
        app_data.read_app_data()
    except Exception as e:
        logging.error(f"Problems refreshing app data. Error message {str(e)}")
        return "Error", 500

    return "OK", 200


@app.route("/api/add-app", methods=["POST"])
def add_app():
    logging.info("Add app request received.")

    miro_app_file = request.files["app-data"]
    miro_app_filename = miro_app_file.filename

    if not miro_app_filename or not allowed_file(miro_app_filename):
        logging.error(
            f"Invalid file uploaded ({miro_app_filename}). Looks like an attempt to tamper with the app."
        )
        return jsonify({"status": 1, "message": "An unexpected error occurred"}), 200

    app_title = request.form["app-title"]
    if not app_title:
        logging.error(
            "No app title was specified. Looks like an attempt to tamper with the app."
        )
        return jsonify({"status": 1, "message": "An unexpected error occurred"}), 200

    app_desc = request.form["app-desc"]

    author_name = request.form["app-author"]
    if not author_name:
        logging.error(
            "No app author was specified. Looks like an attempt to tamper with the app."
        )
        return jsonify({"status": 1, "message": "An unexpected error occurred"}), 200

    author_mail = request.form["app-mail"]
    if not author_mail:
        logging.error(
            "No E-mail was specified. Looks like an attempt to tamper with the app."
        )
        return jsonify({"status": 1, "message": "An unexpected error occurred"}), 200

    if not EMAIL_REGEX.match(author_mail):
        logging.info(f"No valid E-mail specified: {author_mail}.")
        return jsonify(
            {
                "status": 1,
                "message": "E-Mail entered is invalid. Please enter a valid E-mail address.",
            }
        ), 200

    new_app_dir_name = os.path.join(
        "data", "user_apps", secure_filename(author_name.lower() + "_" + app_title)
    )
    try:
        i = 1
        while os.path.isdir(new_app_dir_name):
            new_app_dir_name += str(i)
            i += 1

        os.makedirs(new_app_dir_name)
    except OSError as e:
        logging.error(
            f"Problems creating directory: {new_app_dir_name}. Error message: {str(e)}"
        )
        return jsonify({"status": 1, "message": "An unexpected error occurred"}), 200

    miro_app_filepath = os.path.join(
        new_app_dir_name, secure_filename(miro_app_filename)
    )
    try:
        miro_app_file.save(miro_app_filepath)

        with open(os.path.join(new_app_dir_name, "config.txt"), "w") as config_file:
            config_file.write(f"Title: {app_title}\ntitle: {app_title}\ndesc: ${app_desc}\n\
author: {author_name}\nemail: {author_mail}")
    except Exception as e:
        logging.error(
            f"Problems saving miro app to directory: {new_app_dir_name}. Error message: {str(e)}"
        )
        return jsonify({"status": 1, "message": "An unexpected error occurred"}), 200

    try:
        msg = Message(
            "New MIRO app", sender="noreply@gams.com", recipients=["miro@gams.com"]
        )
        msg.body = f"New MIRO app: {app_title} from: {author_name} ({author_mail}) uploaded. App description: {app_desc}"
        with app.open_resource(os.path.join("..", miro_app_filepath)) as fp:
            msg.attach(secure_filename(miro_app_filename), "application/zip", fp.read())
        mail.send(msg)
    except Exception as e:
        logging.error(
            f"Problems sending confirmation mail of app upload to miro@gams.com. Error message: {str(e)}"
        )

    return jsonify(
        {
            "status": 0,
            "message": "App successfully submitted. It is now being reviewed and you will be notified shortly.",
        }
    ), 200


@app.route("/", methods=["GET"])
def gallery():
    return render_template(
        "gallery.html.j2",
        apps=sorted(app_data.get_data(), key=lambda app: app["upvotes"], reverse=True),
    )
