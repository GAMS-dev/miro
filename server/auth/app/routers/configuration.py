from fastapi import APIRouter, status

from app.config import logger, settings
from app.utils.models import ConfigurationResponse

router = APIRouter(
    prefix="/configuration",
    tags=["configuration"],
    responses={500: {"description": "Internal Server Error"}},
)


@router.get("/", summary="Get MIRO Server configuration information", status_code=status.HTTP_200_OK, response_model=ConfigurationResponse)
async def get_configuration():
    """
    Return configuration information of MIRO Server instance

    - **version**: Version of GAMS MIRO Server
    - **authentication_mode**: Authentication mode (engine or oidc)
    """
    logger.info("Request to return MIRO Server config received")
    return ConfigurationResponse(version=settings.miro_server_version, authentication_mode=settings.authentication_mode)
