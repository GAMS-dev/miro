import requests
from pydantic import BaseModel
from typing import List
from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBearer, HTTPBasic, HTTPBasicCredentials
from fastapi.logger import logger

from app.config import settings

bearer_auth = HTTPBearer(auto_error=False)
basic_auth = HTTPBasic(auto_error=False)


class AdminUser(BaseModel):
    name: str
    auth_header: str


async def get_current_admin_user(credentials_basic: HTTPBasicCredentials = Depends(basic_auth),
                                 credentials_bearer: HTTPBasicCredentials = Depends(bearer_auth)) -> AdminUser:
    if not (credentials_basic or credentials_bearer):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Basic"},
        )
    try:
        if credentials_basic:
            username = credentials_basic.username
            r = requests.post(f"{settings.engine_url}/auth/login",
                              data={"expires_in": 60,
                                    "username": credentials_basic.username,
                                    "password": credentials_basic.password})
            if r.status_code != 200:
                logger.info("Invalid return code (%s) when requesting token from GAMS Engine",
                            str(r.status_code))
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Could not validate credentials",
                    headers={"WWW-Authenticate": "Basic"},
                )
            auth_header = "Bearer " + r.json()["token"]
        else:
            auth_header = "Bearer " + credentials_bearer.credentials
            r = requests.get(
                f"{settings.engine_url}/users/",
                headers={"Authorization": auth_header})
            if r.status_code != 200:
                logger.info("Invalid return code (%s) when requesting user data from GAMS Engine",
                            str(r.status_code))
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Could not validate credentials",
                    headers={"WWW-Authenticate": "Bearer"},
                )
            if len(r.json()) < 1 or r.json()[0]["deleted"]:
                logger.info(
                    "Could not get user info when requesting user data from GAMS Engine")
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Could not validate credentials",
                    headers={"WWW-Authenticate": "Bearer"},
                )
            username = r.json()[0]["username"]

        r = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/permissions",
            params={"username": username},
            headers={"Authorization": auth_header})
        if r.status_code == 200:
            namespace_permissions = r.json()["permission"]
        else:
            namespace_permissions = 0

        is_admin = namespace_permissions == 7
        if not is_admin:
            logger.info("User '%s' does not have full permissions (is not MIRO admin) on namespace: %s",
                        username, settings.engine_ns)
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Unauthorized access",
            )
    except requests.exceptions.ConnectionError:
        logger.info(
            "ConnectionError when requesting user info from GAMS Engine.")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )
    except HTTPException as e:
        raise e
    except:
        logger.exception(
            "Internal error when requesting user info from GAMS Engine")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )

    return AdminUser(name=username, auth_header=auth_header)


def get_user_groups(auth_header: str, is_admin: bool) -> List[str]:
    try:
        r = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/user-groups",
            headers={"Authorization": auth_header})
        if r.status_code != 200:
            logger.info("Invalid return code (%s) when requesting user groups for namespace: %s",
                        str(r.status_code), settings.engine_ns)
            raise HTTPException(
                status_code=r.status_code,
                detail=r.json()["message"],
            )

        user_groups = [x["label"] for x in r.json() if x["label"].lower() not in [
            "admins", "users"]]

        user_groups.append("users")

        if is_admin:
            user_groups.append("admins")
    except requests.exceptions.ConnectionError:
        logger.info(
            "ConnectionError when requesting user groups for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )
    except HTTPException as e:
        raise e
    except:
        logger.exception(
            "Internal error when requesting user groups for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )
    return user_groups
