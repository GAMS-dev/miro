import contextlib
import logging
import time

from fastapi import FastAPI, Request, Response
from starlette.middleware.base import BaseHTTPMiddleware

from app.config import settings, settings_yml
from app.routers import apps, configuration, health, login, scenarios
from app.utils.utils import SingletonAiohttp, use_route_names_as_operation_ids

logger = logging.getLogger(__name__)


async def log_requests(request: Request, call_next):
    start_time = time.perf_counter()
    response: Response = await call_next(request)
    response_time = time.perf_counter() - start_time
    logger.info(
        "%s %s %d %.3fs",
        request.method,
        request.url.path,
        response.status_code,
        response_time,
    )
    return response


@contextlib.asynccontextmanager
async def lifespan(app):
    SingletonAiohttp.get_aiohttp_client()
    yield
    await SingletonAiohttp.close_aiohttp_client()


app = FastAPI(lifespan=lifespan)
app.add_middleware(BaseHTTPMiddleware, dispatch=log_requests)

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
    redoc_url=None,
)
public_api.add_middleware(BaseHTTPMiddleware, dispatch=log_requests)

app.include_router(login.router)
app.include_router(health.router)
if settings_yml:
    public_api.include_router(configuration.router)
    public_api.include_router(apps.router)
    public_api.include_router(scenarios.router)
    public_api.include_router(login.router)
    use_route_names_as_operation_ids(public_api)
    app.mount(f"{settings.script_name}/api", public_api)
else:
    logger.warning(
        "MIRO Server REST API could not be enabled because an outdated docker-compose file was found. Please download the latest docker-compose file to activate the MIRO Server REST API."
    )
