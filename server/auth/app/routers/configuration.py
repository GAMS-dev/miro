from fastapi import APIRouter, status
from fastapi.logger import logger

from app.config import settings
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
    """
    logger.info("Request to return MIRO Server config received")
    return ConfigurationResponse(version=settings.miro_server_version)
