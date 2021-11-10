import requests
from pydantic import BaseModel
from fastapi import APIRouter, Response, HTTPException, status
from fastapi.logger import logger

from config import settings
from dependencies import get_user_groups

router = APIRouter(
    prefix="/login",
    tags=["login"]
)


class AuthRequest(BaseModel):
    username: str
    password: str


@router.post("", status_code=200, include_in_schema=False)
async def login(auth_request: AuthRequest, response: Response):
    logger.info("Login request received for user: %s.", auth_request.username)
    try:
        r = requests.post(f"{settings.engine_url}/auth/login",
                          data={"expires_in": settings.session_timeout,
                                "username": auth_request.username,
                                "password": auth_request.password})
        if r.status_code != 200:
            logger.info("Invalid return code (%s) when requesting token from GAMS Engine",
                        str(r.status_code))
            response.status_code = r.status_code
            return r.json()
        token = r.json()["token"]
        auth_header = "Bearer " + r.json()["token"]
    except requests.exceptions.ConnectionError:
        logger.info("ConnectionError when requesting token from GAMS Engine.")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            message="Internal server error",
        )
    except:
        logger.exception(
            "Internal error when requesting token from GAMS Engine")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            message="Internal server error",
        )

    try:
        r = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/permissions",
            params={"username": auth_request.username},
            headers={"Authorization": auth_header})
        if r.status_code == 200:
            namespace_permissions = r.json()["permission"]
        else:
            namespace_permissions = 0

        if namespace_permissions == 0:
            logger.info("User '%s' has no permissions on namespace: %s",
                        auth_request.username, settings.engine_ns)
            # if user can see models in namespace, she is still authenticated
            r = requests.get(
                f"{settings.engine_url}/namespaces/{settings.engine_ns}",
                headers={"X-Fields": "name", "Authorization": auth_header})
            if r.status_code != 200:
                logger.info("Invalid return code (%s) when requesting models in namespace: %s",
                            str(r.status_code), settings.engine_ns)
                response.status_code = r.status_code
                return r.json()

            models = r.json()
            if not models:
                response.status_code = status.HTTP_403_FORBIDDEN
                return {"message": "Unauthorized access"}

        is_admin = namespace_permissions == 7
    except requests.exceptions.ConnectionError:
        logger.info(
            "ConnectionError when requesting permissions for namespace: %s.", settings.engine_ns)
        response.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"message": "Internal server error"}
    except:
        logger.exception(
            "Internal error when requesting permissions for namespace: %s.", settings.engine_ns)
        response.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"message": "Internal server error"}

    logger.info("User: %s successfully logged in (is_admin:%s).",
                auth_request.username, str(is_admin))
    return {"token": token, "roles": get_user_groups(auth_header, is_admin)}
