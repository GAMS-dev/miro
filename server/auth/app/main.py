from typing import Optional

from fastapi import FastAPI, Response, status
from fastapi.logger import logger
from pydantic import BaseModel
import requests
import logging
from pydantic import BaseSettings

gunicorn_logger = logging.getLogger('gunicorn.error')
logger.handlers = gunicorn_logger.handlers
logger.setLevel(gunicorn_logger.level)


class Settings(BaseSettings):
    engine_url: str
    engine_ns: str
    session_timeout: int = 3600*12


settings = Settings()


class AuthRequest(BaseModel):
    username: str
    password: str


app = FastAPI()


@app.post("/login", status_code=200)
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
    except requests.exceptions.ConnectionError:
        logger.info("ConnectionError when requesting token from GAMS Engine.")
        response.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"message": "Internal server error"}
    except:
        logger.exception(
            "Internal error when requesting token from GAMS Engine")
        response.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"message": "Internal server error"}

    try:
        r = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/permissions",
            params={"username": auth_request.username},
            auth=(auth_request.username, auth_request.password))
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
                auth=(auth_request.username, auth_request.password),
                headers={'X-Fields': 'name'})
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

    try:
        r = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/user-groups",
            auth=(auth_request.username, auth_request.password))
        if r.status_code != 200:
            logger.info("Invalid return code (%s) when requesting user groups for namespace: %s",
                        str(r.status_code), settings.engine_ns)
            response.status_code = r.status_code
            return r.json()

        user_groups = [x["label"] for x in r.json() if x["label"].lower() not in ["admins", "users"]]

        user_groups.append("users")

        if is_admin:
            user_groups.append("admins")
    except requests.exceptions.ConnectionError:
        logger.info(
            "ConnectionError when requesting user groups for namespace: %s.", settings.engine_ns)
        response.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"message": "Internal server error"}
    except:
        logger.exception(
            "Internal error when requesting user groups for namespace: %s.", settings.engine_ns)
        response.status_code = status.HTTP_500_INTERNAL_SERVER_ERROR
        return {"message": "Internal server error"}

    logger.info("User: %s successfully logged in (is_admin:%s).",
                auth_request.username, str(is_admin))
    return {"token": token, "roles": user_groups}
