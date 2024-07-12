from flask import Flask
from flask_mail import Mail
import os

app = Flask(__name__)
app.config.update(
    MAIL_SERVER=os.environ.get("MAIL_SERVER"),
    MAIL_PORT=2525,
    MAIL_USE_TLS=False,
    MAIL_USE_SSL=False,
    MAIL_USERNAME=os.environ.get("MAIL_USER"),
    MAIL_PASSWORD=os.environ.get("MAIL_PASSWORD"),
)

mail = Mail(app)

from app import routes
