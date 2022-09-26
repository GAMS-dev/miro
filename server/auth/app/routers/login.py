from fastapi import APIRouter, status
from fastapi.logger import logger

from app.config import settings
from app.utils.models import AuthRequest, AuthResponse
from app.dependencies import get_authenticated_user, get_bearer_token

router = APIRouter(
    prefix="/login",
    tags=["login"],
    responses={401: {"message": "Unauthorized"},
               500: {"description": "Internal Server Error"}},
)


@router.post("", summary="Log in user and return login info", status_code=status.HTTP_200_OK, response_model=AuthResponse)
async def login(auth_request: AuthRequest):
    """
    Log in user and return login info:

    - **token**: Bearer access token that can be used for authentication
    - **roles**: Labels of user groups that user can see
    - **permissions**: Permissions (octal) of the user in the MIRO Server namespace
    """
    logger.info("Login request received for user: %s.", auth_request.username)
    bearer_token = get_bearer_token(
        auth_request.username, auth_request.password, expires_in=settings.session_timeout)
    authenticated_user = get_authenticated_user(
        bearer_token, auth_request.username)
    logger.info("User: %s successfully logged in (is_admin: %s).",
                auth_request.username, str(authenticated_user.is_admin))
    return AuthResponse(token=bearer_token, roles=authenticated_user.groups, permissions=str(authenticated_user.permissions))
