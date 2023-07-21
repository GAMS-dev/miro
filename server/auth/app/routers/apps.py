from typing import Annotated
from fastapi.param_functions import Query

from fastapi import APIRouter, Depends, HTTPException, Path, File, Form, UploadFile, status
from fastapi.logger import logger

from app.utils.app_utils import AppConfig, app_is_invisible, get_apps_raw
from app.dependencies import User, get_current_admin_user, get_current_user
from app.utils.app_utils import add_or_update_app, delete_app_internal

router = APIRouter(
    prefix="/apps",
    tags=["apps"],
    dependencies=[Depends(get_current_admin_user)],
    responses={401: {"message": "Unauthorized"},
               403: {"message": "Unauthorized access"},
               404: {"message": "Not found"},
               500: {"description": "Internal Server Error"}},
)

metadata = {
    "summary": {
        "get": "Get all visible MIRO apps",
        "post": "Add new MIRO app",
        "delete": "Remove an existing MIRO app",
        "put": "Update an existing MIRO app"
    },
    "description": {
        "app_id": "The ID of this app. If no ID is specified, the default ID (filename of the main `.gms` file in lower case) is used. This ID must be unique among all apps registered on this instance of MIRO Server.",
        "display_name": "The name of the app as it appears in the library.",
        "description": "The description of the app as it appears in the library.",
        "access_groups": "User groups that can access this app. If no groups are specified, it is visible to everyone who has access to your MIRO Server instance.",
        "overwrite_data": "Whether to overwrite existing scenario data (from a previous installation of an app with the same ID).",
        "app_data": "A valid MIROAPP file deployed for a multi-user environment."
    }
}


@router.get("/", summary=metadata["summary"]["get"], response_model=list[AppConfig])
async def get_apps(admin_user: Annotated[User, Depends(get_current_user)]):
    """
    Get all apps registered for one of your user groups and their metadata.

    An app object contains the following fields:

    - **id**: The unique identifier of the app
    - **display_name**: The name of the app as it appears in the library
    - **description**: The description of the app as it appears in the library
    - **access_groups**: The user groups that can see this app. If no user groups are assigned (empty array), anyone with access to the MIRO Server instance can see the app. Displays only subset of user groups of which the logged in user is a member.
    """
    logger.info("%s requested list of apps", admin_user.name)
    return get_apps_raw(user_groups=admin_user.groups)


@router.put(
    "/{app_id}", summary=metadata["summary"]["put"],
    responses={
        200: {"description": "App successfully updated"},
        400: {"description": "You cannot update an app with this ID"},
        404: {"description": "An app with this ID does not exist"}}
)
async def update_app(app_data: Annotated[UploadFile, File(description=metadata["description"]["app_data"])],
                     admin_user: Annotated[User, Depends(get_current_admin_user)],
                     app_id: Annotated[str, Path(description="The ID of the app to update.", max_length=60)],
                     display_name: Annotated[str | None, Form(
                         description=metadata["description"]["display_name"], max_length=40)] = None,
                     description: Annotated[str | None, Form(
                         description=metadata["description"]["description"], max_length=200)] = None,
                     access_groups: Annotated[list[str], Form(
                         description=metadata["description"]["access_groups"])] = None,
                     overwrite_data: Annotated[bool, Form(description=metadata["description"]["overwrite_data"])] = False):
    """
    Update a MIRO app (requires write permissions on namespace).

    The app as well as its metadata will be overwritten. This means that you should specify all parameters.
    For example, if you do not specify `access_groups`, no access groups will be assigned to the app.
    This will result in it being visible to everyone.
    """
    logger.info(
        "%s requested to update existing app: %s", admin_user.name, app_id)
    if access_groups:
        nonempty_access_groups = [
            group for group in access_groups if group != ""]
    else:
        nonempty_access_groups = []
    invalid_user_groups = list(filter(
        lambda access_group: access_group in ["admins", "users"] or access_group not in admin_user.groups, nonempty_access_groups))
    if invalid_user_groups:
        logger.info("Invalid user groups when updating app: %s", app_id)
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid user group(s): {','.join(invalid_user_groups)}"
        )
    if app_is_invisible(admin_user.groups, app_id):
        logger.info("%s is not visible", app_id)
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="An app with this ID does not exist"
        )

    app_config = AppConfig(id=app_id, display_name=display_name, description=description,
                           access_groups=nonempty_access_groups)
    try:
        await add_or_update_app(admin_user, app_config, app_data, overwrite_data=overwrite_data, update=True)
    except HTTPException as exc:
        logger.info(
            "Problems updating MIRO app with id: %s. Status code: %s. Details: %s",
            app_id, exc.status_code, exc.detail)
        raise exc
    except Exception as exc:
        logger.info(
            "Problems updating MIRO app with id: %s. Details: %s", app_id, str(exc))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        ) from exc
    logger.info("App with id: %s successfully updated by: %s.",
                app_id, admin_user.name)
    return {"description": "App successfully updated"}


@router.post(
    "/", summary=metadata["summary"]["post"],
    responses={
        201: {"description": "App successfully added"},
        400: {"description": "You cannot add an app with this ID"},
        409: {"description": "An app with this id already exists"}},
    status_code=status.HTTP_201_CREATED
)
async def add_app(
        app_data: Annotated[UploadFile, File(description=metadata["description"]["app_data"])],
        admin_user: Annotated[User, Depends(get_current_admin_user)],
        app_id: Annotated[str | None, Form(
            description=metadata["description"]["app_id"], max_length=60)] = None,
        display_name: Annotated[str | None, Form(
            description=metadata["description"]["display_name"], max_length=40)] = None,
        description: Annotated[str | None, Form(
            description=metadata["description"]["description"], max_length=200)] = None,
        access_groups: Annotated[list[str], Form(
            description=metadata["description"]["access_groups"])] = None,
        overwrite_data: Annotated[bool, Form(description=metadata["description"]["overwrite_data"])] = False):
    """
    Add a new MIRO app (requires write permissions on namespace).
    """
    logger.info(
        "%s requested to add new app", admin_user.name)
    if access_groups:
        nonempty_access_groups = [
            group for group in access_groups if group != ""]
    else:
        nonempty_access_groups = []
    invalid_user_groups = list(filter(
        lambda access_group: access_group in ["admins", "users"] or access_group not in admin_user.groups, nonempty_access_groups))
    if invalid_user_groups:
        logger.info("Invalid user groups when adding new app")
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid user group(s): {','.join(invalid_user_groups)}"
        )

    app_config = AppConfig(id=app_id, display_name=display_name, description=description,
                           access_groups=nonempty_access_groups)
    try:
        await add_or_update_app(admin_user, app_config, app_data, overwrite_data=overwrite_data)
    except HTTPException as exc:
        logger.info(
            "Problems adding new MIRO app. Status code: %s. Details: %s", exc.status_code, exc.detail)
        raise exc
    except Exception as exc:
        logger.info(
            "Problems adding new MIRO app. Details: %s", str(exc))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        ) from exc
    logger.info("New app with successfully added by: %s.",
                admin_user.name)
    return {"description": "App successfully added"}


@router.delete(
    "/{app_id}", summary=metadata["summary"]["delete"],
    responses={
        200: {"description": "App successfully deleted"},
        404: {"description": "An app with this ID does not exist"}}
)
async def delete_app(app_id: Annotated[str, Path(description="The ID of the app to delete.", max_length=60)],
                     admin_user: Annotated[User, Depends(get_current_admin_user)],
                     delete_data: Annotated[bool, Query(description="Whether to delete all scenario data of this app.")] = False):
    """
    Remove an existing MIRO app (requires write permissions on namespace).
    """
    logger.info(
        "%s requested to add remove app with ID: %s", admin_user.name, app_id)
    if app_is_invisible(admin_user.groups, app_id):
        logger.info("%s is not visible", app_id)
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="An app with this ID does not exist"
        )
    try:
        await delete_app_internal(admin_user, app_id, delete_data)
    except HTTPException as exc:
        logger.info(
            "Problems deleting MIRO app with id: %s. Status code: %s. Details: %s",
            app_id, exc.status_code, exc.detail)
        raise exc
    except Exception as exc:
        logger.info(
            "Problems deleting MIRO app with id: %s. Details: %s", app_id, str(exc))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        ) from exc
    logger.info("App with id: %s successfully deleted by: %s.",
                app_id, admin_user.name)
    return {"description": "App successfully deleted"}
