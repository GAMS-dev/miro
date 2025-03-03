import requests
from fastapi import Depends, HTTPException, status, Path
from fastapi.security import HTTPBearer, HTTPBasic, HTTPBasicCredentials

from app.config import logger, settings
from app.utils.app_utils import app_is_invisible
from app.utils.models import User, OidcLoginData

bearer_auth = HTTPBearer(auto_error=False)
basic_auth = HTTPBasic(auto_error=False)


def _send_token_request(username: str, password: str,
                        expires_in: int = 3600, scopes: list[str] | None = None) -> requests.Response:
    data = {"expires_in": expires_in,
            "username": username,
            "password": password}
    if scopes:
        data["scope"] = " ".join(scopes)
    try:
        return requests.post(f"{settings.engine_url}/auth/login",
                             data=data, timeout=settings.request_timeout)
    except requests.exceptions.ConnectionError as exc:
        logger.info(
            "ConnectionError when requesting bearer token from GAMS Engine.")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except requests.exceptions.Timeout as exc:
        logger.info(
            "Timeout (%s) when requesting bearer token from GAMS Engine.", str(settings.request_timeout))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc


def _send_id_token_request(id_token: str, expires_in: int = 3600,
                           scopes: list[str] | None = None) -> requests.Response:
    data = {"expires_in": expires_in,
            "id_token": id_token}
    if scopes:
        data["scope"] = " ".join(scopes)
    try:
        return requests.post(f"{settings.engine_url}/auth/oidc-providers/login",
                             data=data, timeout=settings.request_timeout)
    except requests.exceptions.ConnectionError as exc:
        logger.info(
            "ConnectionError when requesting bearer token from GAMS Engine (OIDC).")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except requests.exceptions.Timeout as exc:
        logger.info(
            "Timeout (%s) when requesting bearer token from GAMS Engine (OIDC).", str(settings.request_timeout))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc


def get_bearer_token(username: str, password: str, expires_in: int = 3600) -> str:
    response = _send_token_request(
        username, password, expires_in, scopes=["JOBS", "HYPERCUBE", "NAMESPACES", "USAGE", "USERS"])
    if response.status_code == 400:
        logger.info("Received bad request when trying to request bearer token. Most likely old Engine version that does not have 'scope' parameter. Trying without scopes.")
        response = _send_token_request(username, password, expires_in)
    if response.status_code != 200:
        logger.info("Invalid return code (%s) when requesting token from GAMS Engine",
                    str(response.status_code))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Basic"},
        )
    return response.json()["token"]


def login_user_oidc(id_token: str, expires_in: int = 3600) -> OidcLoginData:
    response = _send_id_token_request(
        id_token, expires_in, scopes=["JOBS", "HYPERCUBE", "NAMESPACES", "USAGE", "USERS"])
    if response.status_code != 200:
        logger.info("Invalid return code (%s) when requesting token from GAMS Engine (OIDC)",
                    str(response.status_code))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Basic"},
        )

    bearer_token = response.json()["token"]
    auth_header = "Bearer " + bearer_token

    try:
        response = requests.get(f"{settings.engine_url}/users/?everyone=false",
                                headers={"Authorization": auth_header,
                                         "X-Fields": "username,roles"},
                                timeout=settings.request_timeout)
    except requests.exceptions.ConnectionError as exc:
        logger.info(
            "ConnectionError when requesting user details from GAMS Engine (OIDC).")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except requests.exceptions.Timeout as exc:
        logger.info(
            "Timeout (%s) when requesting user details from GAMS Engine (OIDC).", str(settings.request_timeout))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    if response.status_code != 200:
        logger.info("Invalid return code (%s) when requesting user details from GAMS Engine (OIDC)",
                    str(response.status_code))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )
    user_info = response.json()
    if len(user_info) != 1:
        logger.info(
            "GAMS Engine did not return exactly one user when requesting user details (OIDC).")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        )
    return OidcLoginData(username=user_info[0]["username"], bearer_token=bearer_token)


def get_username_bearer(bearer_token: str) -> str:
    try:
        auth_header = "Bearer " + bearer_token
        response = requests.get(
            f"{settings.engine_url}/users/?everyone=false",
            headers={"Authorization": auth_header},
            timeout=settings.request_timeout,
        )
    except requests.exceptions.ConnectionError as exc:
        logger.info(
            "ConnectionError when requesting username of bearer token from GAMS Engine.")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except requests.exceptions.Timeout as exc:
        logger.info(
            "Timeout (%s) when requesting username of bearer token from GAMS Engine.",
            str(settings.request_timeout))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    if response.status_code != 200:
        logger.info("Invalid return code (%s) when requesting user data from GAMS Engine",
                    str(response.status_code))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Bearer"},
        )
    if len(response.json()) != 1 or response.json()[0]["deleted"]:
        logger.info(
            "Could not get user info when requesting user data from GAMS Engine")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Bearer"},
        )
    return response.json()[0]["username"]


def get_user_groups(auth_header: str, is_admin: bool) -> list[str]:
    try:
        response = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/user-groups",
            headers={"Authorization": auth_header}, timeout=settings.request_timeout)
        if response.status_code != 200:
            logger.info("Invalid return code (%s) when requesting user groups for namespace: %s",
                        str(response.status_code), settings.engine_ns)
            raise HTTPException(
                status_code=response.status_code,
                detail=response.json()["message"],
            )

        # MIRO Server doesn't support group labels with uppercase letters
        # because Shinyproxy converts group labels to uppercase before
        # setting the environment variable.
        # We might want to lift this limitation at some point by adjusting
        # Shinyproxy.
        user_groups = [x["label"] for x in response.json() if x["label"].lower(
        ) == x["label"] and x["label"] not in ["admins", "users"]]

        user_groups.append("users")

        if is_admin:
            user_groups.append("admins")
    except requests.exceptions.ConnectionError as exc:
        logger.info(
            "ConnectionError when requesting user groups for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except requests.exceptions.Timeout as exc:
        logger.info(
            "Timeout (%s) when requesting user groups for namespace: %s.", str(
                settings.request_timeout), settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except HTTPException as exc:
        raise exc
    except Exception as exc:
        logger.exception(
            "Internal error when requesting user groups for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    return user_groups


def get_authenticated_user(bearer_token: str, username: str) -> User:
    try:
        auth_header = "Bearer " + bearer_token
        namespace_permissions = 0
        response = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/permissions",
            params={"username": username},
            headers={"Authorization": auth_header}, timeout=settings.request_timeout)
        if response.status_code == 200:
            namespace_permissions = response.json()["permission"]

        if namespace_permissions == 0:
            logger.info("User '%s' has no permissions on namespace: %s",
                        username, settings.engine_ns)
            # if user can see models in namespace, she is still authenticated
            response = requests.get(
                f"{settings.engine_url}/namespaces/{settings.engine_ns}",
                headers={"X-Fields": "name", "Authorization": auth_header}, timeout=settings.request_timeout)
            if response.status_code != 200:
                logger.info("Invalid return code (%s) when requesting models in namespace: %s",
                            str(response.status_code), settings.engine_ns)
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Could not validate credentials",
                    headers={"WWW-Authenticate": "Basic"},
                )
            models = response.json()
            if not models:
                logger.info("User '%s' can not see any models in namespace: %s. Unauthorized!",
                            username, settings.engine_ns)
                raise HTTPException(
                    status_code=status.HTTP_403_FORBIDDEN,
                    detail="Unauthorized access",
                )
        is_admin = namespace_permissions == 7
        logger.info("User '%s' successfully authenticated on namespace: %s (permissions: %s)",
                    username, settings.engine_ns, str(namespace_permissions))
        return User(name=username, auth_header=auth_header,
                    permissions=namespace_permissions, is_admin=is_admin,
                    groups=get_user_groups(auth_header, is_admin=is_admin))
    except HTTPException as exc:
        raise exc
    except requests.exceptions.ConnectionError as exc:
        logger.info(
            "ConnectionError when requesting permissions for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except requests.exceptions.Timeout as exc:
        logger.info(
            "Timeout (%s) when requesting permissions for namespace: %s.", str(
                settings.request_timeout), settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except Exception as exc:
        logger.exception(
            "Internal error when requesting permissions for namespace: %s.", settings.engine_ns)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc


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


async def get_current_app_user(
        credentials_basic: HTTPBasicCredentials = Depends(basic_auth),
    credentials_bearer: HTTPBasicCredentials = Depends(bearer_auth),
    app_id: str = Path(
        description="The ID of the app whose data you want to operate on.", max_length=60)) -> User:
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

        response = requests.get(
            f"{settings.engine_url}/namespaces/{settings.engine_ns}/permissions",
            params={"username": username},
            headers={"Authorization": auth_header}, timeout=settings.request_timeout)
        if response.status_code == 200:
            namespace_permissions = response.json()["permission"]

        is_admin = namespace_permissions == 7
        if not is_admin:
            logger.info("User '%s' does not have full permissions (is not MIRO admin) on namespace: %s",
                        username, settings.engine_ns)
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Unauthorized access",
            )
    except requests.exceptions.ConnectionError as exc:
        logger.info(
            "ConnectionError when requesting user info from GAMS Engine.")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except requests.exceptions.Timeout as exc:
        logger.info(
            "Timeout (%s) when requesting user info from GAMS Engine.", str(settings.request_timeout))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    except HTTPException as exc:
        raise exc
    except Exception as exc:
        logger.exception(
            "Internal error when requesting user info from GAMS Engine")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal server error",
        ) from exc
    return User(name=username, auth_header=auth_header, permissions=namespace_permissions,
                is_admin=is_admin, groups=get_user_groups(auth_header, is_admin))
