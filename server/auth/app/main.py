import sys
import logging
from fastapi import FastAPI
from fastapi.logger import logger

from routers import login, apps

gunicorn_logger = logging.getLogger('gunicorn.error')

logger.handlers = gunicorn_logger.handlers
if __name__ != "main":
    logger.setLevel(gunicorn_logger.level)
else:
    handler = logging.StreamHandler(sys.stdout)
    logger.addHandler(handler)
    logger.setLevel(logging.DEBUG)

app = FastAPI()

public_api = FastAPI()

app.include_router(login.router)
public_api.include_router(apps.router)

app.mount("/api", public_api)
