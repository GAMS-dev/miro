from fastapi import FastAPI

from app.config import logger, settings_yml
from app.routers import login, apps, scenarios, configuration
from app.utils.utils import use_route_names_as_operation_ids

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
if settings_yml:
    public_api.include_router(configuration.router)
    public_api.include_router(apps.router)
    public_api.include_router(scenarios.router)
    public_api.include_router(login.router)
    use_route_names_as_operation_ids(public_api)
    app.mount("/api", public_api)
else:
    logger.warning(
        "MIRO Server REST API could not be enabled because an outdated docker-compose file was found. Please download the latest docker-compose file to activate the MIRO Server REST API.")
