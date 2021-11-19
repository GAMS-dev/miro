import sys
import logging
from fastapi import FastAPI
from fastapi.logger import logger

from app.routers import login, apps, scenarios

gunicorn_logger = logging.getLogger('gunicorn.error')

logger.handlers = gunicorn_logger.handlers
if __name__ != "main":
    logger.setLevel(gunicorn_logger.level)
else:
    handler = logging.StreamHandler(sys.stdout)
    logger.addHandler(handler)
    logger.setLevel(logging.DEBUG)

app = FastAPI()

public_api = FastAPI(
    title="MIRO Server API",
    description="REST API for managing MIRO apps and scenario data",
    version="1.0.0",
    contact={
        "name": "GAMS Support",
        "url": "https://gams.com/miro",
        "email": "support@gams.com",
    },
    license_info={
        "name": "GPL-3",
        "url": "https://www.gnu.org/licenses/gpl-3.0.de.html",
    },
    redoc_url=None
)

app.include_router(login.router)
public_api.include_router(apps.router)
public_api.include_router(scenarios.router)

app.mount("/api", public_api)
