import requests
from typing import List
from fastapi import Depends, HTTPException, status, Path
from fastapi.security import HTTPBearer, HTTPBasic, HTTPBasicCredentials
from fastapi.logger import logger

from app.config import settings
from app.utils.app_utils import app_is_invisible
from app.utils.models import User

bearer_auth = HTTPBearer(auto_error=False)
basic_auth = HTTPBasic(auto_error=False)


def get_bearer_token(username: str, password: str, expires_in=3600) -> str:
    try:
        r = requests.post(f"{settings.engine_url}/auth/login",
                          data={"expires_in": expires_in,
                                "username": username,
                                "password": password})
    except requests.exceptions.ConnectionError:
        logger.info(
            "ConnectionError when requesting bearer token from GAMS Engine.")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            message="Internal server error",
        )
    if r.status_code != 200:
        logger.info("Invalid return code (%s) when requesting token from GAMS Engine",
                    str(r.status_code))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Basic"},
        )
    return r.json()["token"]


def get_username_bearer(bearer_token: str) -> str:
    try:
        auth_header = "Bearer " + bearer_token
        r = requests.get(
            f"{settings.engine_url}/users/",
            headers={"Authorization": auth_header})
    except requests.exceptions.ConnectionError:
        logger.info(
            "ConnectionError when requesting username of bearer token from GAMS Engine.")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            message="Internal server error",
        )
    if r.status_code != 200:
        logger.info("Invalid return code (%s) when requesting user data from GAMS Engine",
                    str(r.status_code))
        print(r.status_code)
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
    return r.json()[0]["username"]


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

        # MIRO Server doesn't support group labels with uppercase letters
        # because Shinyproxy converts group labels to uppercase before
        # setting the environment variable.
        # We might want to lift this limitation at some point by adjusting
        # Shinyproxy.
        user_groups = [x["label"] for x in r.json() if x["label"].lower(
        ) == x["label"] and x["label"] not in ["admins", "users"]]

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


def get_authenticated_user(bearer_token: str, username: str) -> User:
    try:
        auth_header = "Bearer " + bearer_token
        namespace_permissions = 0
        r = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/permissions",
            params={"username": username},
            headers={"Authorization": auth_header})
        if r.status_code == 200:
            namespace_permissions = r.json()["permission"]

        if namespace_permissions == 0:
            logger.info("User '%s' has no permissions on namespace: %s",
                        username, settings.engine_ns)
            # if user can see models in namespace, she is still authenticated
            r = requests.get(
                f"{settings.engine_url}/namespaces/{settings.engine_ns}",
                headers={"X-Fields": "name", "Authorization": auth_header})
            if r.status_code != 200:
                logger.info("Invalid return code (%s) when requesting models in namespace: %s",
                            str(r.status_code), settings.engine_ns)
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Could not validate credentials",
                    headers={"WWW-Authenticate": "Basic"},
                )
            models = r.json()
            if not models:
                raise HTTPException(
                    status_code=status.HTTP_403_FORBIDDEN,
                    detail="Unauthorized access",
                )
        is_admin = namespace_permissions == 7
        return User(name=username, auth_header=auth_header,
                    permissions=namespace_permissions, is_admin=is_admin,
                    groups=get_user_groups(auth_header, is_admin=is_admin))
    except HTTPException as e:
        raise e
    except requests.exceptions.ConnectionError:
        logger.info(
            "ConnectionError when requesting permissions for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )
    except Exception as e:
        logger.exception(
            "Internal error when requesting permissions for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )


async def get_current_user(credentials_basic: HTTPBasicCredentials = Depends(basic_auth),
                           credentials_bearer: HTTPBasicCredentials = Depends(
        bearer_auth)) -> User:
    if not (credentials_basic or credentials_bearer):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Basic"},
        )
    if credentials_basic:
        if credentials_bearer:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail='No unique authentication method: Make sure you provide only a single "Authorization" header.',
            )
        bearer_token = get_bearer_token(
            credentials_basic.username, credentials_basic.password)
        username = credentials_basic.username
    else:
        bearer_token = credentials_bearer.credentials
        username = get_username_bearer(bearer_token)

    return get_authenticated_user(bearer_token, username)


async def get_current_app_user(credentials_basic: HTTPBasicCredentials = Depends(basic_auth),
                               credentials_bearer: HTTPBasicCredentials = Depends(
                               bearer_auth),
                               app_id: str = Path(..., description="The ID of the app whose data you want to operate on.", max_length=60)) -> User:
    current_user = await get_current_user(credentials_basic, credentials_bearer)
    if app_is_invisible(current_user.groups, app_id):
        logger.info("%s is not visible", app_id)
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="An app with this ID does not exist"
        )
    return current_user


async def get_current_admin_user(credentials_basic: HTTPBasicCredentials = Depends(basic_auth),
                                 credentials_bearer: HTTPBasicCredentials = Depends(bearer_auth)) -> User:
    if not (credentials_basic or credentials_bearer):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Basic"},
        )
    try:
        namespace_permissions = 0
        if credentials_basic:
            if credentials_bearer:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail='No unique authentication method: Make sure you provide only a single "Authorization" header.',
                )
            username = credentials_basic.username
            auth_header = "Bearer " + get_bearer_token(
                credentials_basic.username, credentials_basic.password)
        else:
            auth_header = "Bearer " + credentials_bearer.credentials
            username = get_username_bearer(credentials_bearer.credentials)

        r = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/permissions",
            params={"username": username},
            headers={"Authorization": auth_header})
        if r.status_code == 200:
            namespace_permissions = r.json()["permission"]

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
    return User(name=username, auth_header=auth_header, permissions=namespace_permissions,
                is_admin=is_admin, groups=get_user_groups(auth_header, is_admin))