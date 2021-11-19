from typing import List
from pydantic import BaseModel
from fastapi import APIRouter
from fastapi.logger import logger

from app.utils.models import AuthRequest, AuthResponse
from app.dependencies import get_authenticated_user, get_bearer_token

router = APIRouter(
    prefix="/login",
    tags=["login"]
)


@router.post("", status_code=200, response_model=AuthResponse, include_in_schema=False)
async def login(auth_request: AuthRequest):
    logger.info("Login request received for user: %s.", auth_request.username)
    bearer_token = get_bearer_token(
        auth_request.username, auth_request.password)
    authenticated_user = get_authenticated_user(
        bearer_token, auth_request.username)
    logger.info("User: %s successfully logged in (is_admin:%s).",
                auth_request.username, str(authenticated_user.is_admin))
    return AuthResponse(token=bearer_token, roles=authenticated_user.groups)
