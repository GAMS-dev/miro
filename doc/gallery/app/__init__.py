from flask import Flask
from flask_mail import Mail
import os

app = Flask(__name__)
app.config.update(
    MAIL_SERVER="smtp.googlemail.com",
    MAIL_PORT=465,
    MAIL_USE_TLS = False,
    MAIL_USE_SSL=True,
    MAIL_USERNAME = os.environ.get("MAIL_USER"),
    MAIL_PASSWORD = os.environ.get("MAIL_PASSWORD"),
    DOWNLOAD_FOLDER = os.path.join(app.root_path, '..', 'download')
    )

mail = Mail(app)

from app import routes
