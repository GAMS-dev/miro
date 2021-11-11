import os
import subprocess
import aiofiles
import json
from typing import Optional
from fastapi.param_functions import Query
from pydantic import BaseModel
import yaml
from typing import List
from fastapi import APIRouter, Depends, HTTPException, Path, File, Form, UploadFile, status
from fastapi.logger import logger

from app.config import settings
from app.dependencies import AdminUser, get_current_admin_user, get_user_groups

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
        "app_id": "The ID of this application. If no ID is specified, the default ID (filename of the main `.gms` file in lower case) is used. This ID must be unique among all applications registered on this instance of MIRO Server.",
        "display_name": "The name of the app as it appears in the library.",
        "description": "The description of the app as it appears in the library.",
        "access_groups": "User groups that can access this application. If no groups are specified, it is visible to everyone who has access to your MIRO Server instance.",
        "overwrite_data": "Whether to overwrite existing scenario data (from a previous installation of an app with the same ID).",
        "app_data": "A valid MIROAPP file deployed for a multi-user environment."
    }
}


class UserInfo(BaseModel):
    name: str
    groups: List[str]


class AppConfig(BaseModel):
    id: Optional[str]
    displayName: Optional[str]
    description: Optional[str]
    accessGroups: List[str]


def app_is_invisible(user_groups: List[str], app_id: str) -> bool:
    return not os.path.isdir(os.path.join(settings.model_dir, app_id)) or \
        app_id not in [app["id"] for app in get_apps_raw(
            user_groups=user_groups)]


def get_apps_raw(user_groups: Optional[List[str]] = None, all_apps: bool = False) -> List[AppConfig]:
    apps = []
    with open(os.path.join(settings.data_dir, "specs.yaml"), "r") as f:
        apps = yaml.load(f, Loader=yaml.CSafeLoader)["specs"]
    if all_apps:
        return apps

    user_groups_uppercase = [user_group.upper() for user_group in user_groups]
    visible_apps = []
    for app in apps:
        if app["id"] == "admin":
            continue
        if "accessGroups" not in app or \
                not app["accessGroups"]:
            app["accessGroups"] = []
            visible_apps.append(app)
            continue
        if len(set(app["accessGroups"]) - set(user_groups_uppercase)) < len(set(app["accessGroups"])):
            visible_apps.append(app)

    return visible_apps


def get_miro_proc_env(user_info, auth_header):
    proc_env = {}
    proc_env.update(os.environ)
    proc_env.update({"MIRO_DB_HOST": settings.gms_miro_database_host,
                     "MIRO_DB_PORT": str(settings.gms_miro_database_port),
                     "MIRO_DB_USERNAME": settings.gms_miro_database_user,
                     "MIRO_DB_NAME": settings.gms_miro_database,
                     "MIRO_DB_PASSWORD": settings.gms_miro_database_pwd,
                     "MIRO_ENGINE_HOST": settings.engine_url,
                     "MIRO_ENGINE_NAMESPACE": settings.engine_ns,
                     "MIRO_ENGINE_AUTH_HEADER": auth_header,
                     "ADD_DATA_TIMEOUT": str(settings.add_data_timeout),
                     "MIRO_ENFORCE_SIGNED_APPS": settings.force_signed_apps,
                     "SHINYPROXY_USERNAME": user_info.name,
                     "SHINYPROXY_USERGROUPS": ",".join(user_info.groups).upper()})
    return proc_env


async def add_or_update_app(user_info: UserInfo, app_config: AppConfig, data: UploadFile, auth_header: str, overwrite_data: bool = False, update=False) -> bool:
    async with aiofiles.tempfile.NamedTemporaryFile("wb", suffix=".miroapp") as out_file:
        while content := await data.read(1024):
            await out_file.write(content)
        await out_file.flush()
        proc_env = get_miro_proc_env(user_info, auth_header)
        proc_out = subprocess.run(["R", "--no-echo", "--no-restore", "--vanilla",
                                   "-f", os.path.join(settings.admin_app_dir, "scripts", "addApp.R")],
                                  capture_output=True,
                                  input=json.dumps({
                                      "id": app_config.id,
                                      "displayName": app_config.displayName,
                                      "description": app_config.description,
                                      "accessGroups": app_config.accessGroups,
                                      "appPath": out_file.name,
                                      "overwriteData": overwrite_data,
                                      "update": update}).encode(),
                                  cwd=settings.admin_app_dir, env=proc_env)
        logger.info("Stdout of addApp.R subprocess: %s",
                    proc_out.stdout.decode())
        for line in proc_out.stderr.decode().split("\n"):
            err = line.split(":::")
            if err[0] == "merr":
                err_details = "Internal error"
                err_code = 500
                try:
                    err_details = str(err[2])
                    err_code = int(err[1])
                except ValueError:
                    logger.warning(
                        "Invalid error message received from addApp.R subprocess: %s", line)
                except IndexError:
                    logger.warning(
                        "Invalid error message received from addApp.R subprocess: %s", line)
                if err_code == 500:
                    logger.info("Stderr of addApp.R subprocess: %s",
                                proc_out.stderr.decode())
                raise HTTPException(
                    status_code=err_code, detail=err_details
                )
        if proc_out.returncode != 0:
            logger.warning("Problems running addApp.R subprocess. Stderr: %s",
                           proc_out.stderr.decode())
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Internal Server Error"
            )
    return True


async def delete_app_internal(user_info: UserInfo, app_id: str, auth_header: str, delete_data: bool = False) -> bool:
    proc_env = get_miro_proc_env(user_info, auth_header)
    proc_out = subprocess.run(["R", "--no-echo", "--no-restore", "--vanilla",
                               "-f", os.path.join(settings.admin_app_dir, "scripts", "deleteApp.R")],
                              capture_output=True,
                              input=json.dumps({
                                  "id": app_id,
                                  "deleteData": delete_data}).encode(),
                              cwd=settings.admin_app_dir, env=proc_env)
    logger.info("Stdout of deleteApp.R subprocess: %s",
                proc_out.stdout.decode())
    for line in proc_out.stderr.decode().split("\n"):
        err = line.split(":::")
        if err[0] == "merr":
            err_details = "Internal error"
            err_code = 500
            try:
                err_details = str(err[2])
                err_code = int(err[1])
            except ValueError:
                logger.warning(
                    "Invalid error message received from deleteApp.R subprocess: %s", line)
            except IndexError:
                logger.warning(
                    "Invalid error message received from deleteApp.R subprocess: %s", line)
            if err_code == 500:
                logger.info("Stderr of deleteApp.R subprocess: %s",
                            proc_out.stderr.decode())
            raise HTTPException(
                status_code=err_code, detail=err_details
            )
    if proc_out.returncode != 0:
        logger.warning("Problems running deleteApp.R subprocess. Stderr: %s",
                       proc_out.stderr.decode())
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Internal Server Error"
        )
    return True


@ router.get("/", summary=metadata["summary"]["get"], response_model=List[AppConfig])
async def get_apps(admin_user: AdminUser = Depends(get_current_admin_user)):
    """
    Get all applications registered for one of your user groups and their metadata.

    An app object contains the following fields:

    - **id**: The unique identifier of the app
    - **displayName**: The name of the app as it appears in the library
    - **description**: The description of the app as it appears in the library
    - **accessGroups**: The user groups that can see this app. If no user groups are assigned (empty array), anyone with access to the MIRO Server instance can see the app
    """
    logger.info("%s requested list of apps", admin_user.name)
    return get_apps_raw(user_groups=get_user_groups(admin_user.auth_header, is_admin=True))


@ router.put(
    "/{app_id}", summary=metadata["summary"]["put"],
    responses={
        200: {"description": "App successfully updated"},
        400: {"description": "You cannot update an app with this ID"},
        404: {"description": "An app with this ID does not exist"}}
)
async def update_app(app_id: str = Path(..., description="The ID of the app to update."),
                     display_name: Optional[str] = Form(
                         None, description=metadata["description"]["display_name"]),
                     description: Optional[str] = Form(
                         None, description=metadata["description"]["description"]),
                     access_groups: Optional[List[str]] = Form(
                         [], description=metadata["description"]["access_groups"]),
                     overwrite_data: bool = Form(
                         False, description=metadata["description"]["overwrite_data"]),
                     app_data: UploadFile = File(
                         ..., description=metadata["description"]["app_data"]),
                     admin_user: AdminUser = Depends(get_current_admin_user)):
    """
    Update a MIRO application.

    The app as well as its metadata will be overwritten. This means that you should specify all parameters.
    For example, if you do not specify `access_groups`, no access groups will be assigned to the application.
    This will result in it being visible to everyone.
    """
    logger.info(
        "%s requested to add new app", admin_user.name)
    user_groups = get_user_groups(
        admin_user.auth_header, is_admin=True)
    nonempty_access_groups = [group for group in access_groups if group != ""]
    invalid_user_groups = list(filter(
        lambda access_group: access_group in ["admins", "users"] or access_group not in user_groups, nonempty_access_groups))
    if invalid_user_groups:
        logger.info("Invalid user groups when updating app: %s", app_id)
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Invalid user group(s): {}".format(
                ",".join(invalid_user_groups))
        )
    if app_is_invisible(user_groups, app_id):
        logger.info("%s is not visible", app_id)
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="An app with this ID does not exist"
        )

    user_info = UserInfo(name=admin_user.name, groups=user_groups)
    app_config = AppConfig(id=app_id, displayName=display_name, description=description,
                           accessGroups=nonempty_access_groups)
    try:
        await add_or_update_app(user_info, app_config, app_data, admin_user.auth_header, overwrite_data=overwrite_data, update=True)
    except HTTPException as e:
        logger.info(
            "Problems updating MIRO app with id: %s. Status code: %s. Details: %s", app_id, e.status_code, e.detail)
        raise e
    except Exception as e:
        logger.info(
            "Problems updating MIRO app with id: %s. Details: %s", app_id, str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        )
    logger.info("App with id: %s successfully updated by: %s.",
                app_id, admin_user.name)
    return {"description": "App successfully updated"}


@ router.post(
    "/", summary=metadata["summary"]["post"],
    responses={
        201: {"description": "App successfully added"},
        400: {"description": "You cannot add an app with this ID"},
        409: {"description": "An app with this id already exists"}},
    status_code=status.HTTP_201_CREATED
)
async def add_app(app_id: Optional[str] = Form(None, description=metadata["description"]["app_id"]),
                  display_name: Optional[str] = Form(
                      None, description=metadata["description"]["display_name"]),
                  description: Optional[str] = Form(
                      None, description=metadata["description"]["description"]),
                  access_groups: Optional[List[str]] = Form(
                      [], description=metadata["description"]["access_groups"]),
                  overwrite_data: bool = Form(
                      False, description=metadata["description"]["overwrite_data"]),
                  app_data: UploadFile = File(
                      ..., description=metadata["description"]["app_data"]),
                  admin_user: AdminUser = Depends(get_current_admin_user)):
    """
    Add a new MIRO application.
    """
    logger.info(
        "%s requested to add new app", admin_user.name)
    user_groups = get_user_groups(
        admin_user.auth_header, is_admin=True)
    nonempty_access_groups = [group for group in access_groups if group != ""]
    invalid_user_groups = list(filter(
        lambda access_group: access_group in ["admins", "users"] or access_group not in user_groups, nonempty_access_groups))
    if invalid_user_groups:
        logger.info("Invalid user groups when adding new app")
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Invalid user group(s): {}".format(
                ",".join(invalid_user_groups))
        )

    user_info = UserInfo(name=admin_user.name, groups=user_groups)
    app_config = AppConfig(id=app_id, displayName=display_name, description=description,
                           accessGroups=nonempty_access_groups)
    try:
        await add_or_update_app(user_info, app_config, app_data, admin_user.auth_header, overwrite_data=overwrite_data)
    except HTTPException as e:
        logger.info(
            "Problems adding new MIRO app. Status code: %s. Details: %s", e.status_code, e.detail)
        raise e
    except Exception as e:
        logger.info(
            "Problems adding new MIRO app. Details: %s", str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        )
    logger.info("New app with successfully added by: %s.",
                admin_user.name)
    return {"description": "App successfully added"}


@ router.delete(
    "/{app_id}", summary=metadata["summary"]["delete"],
    responses={
        200: {"description": "App successfully deleted"},
        404: {"description": "An app with this ID does not exist"}}
)
async def delete_app(app_id: str = Path(..., description="The ID of the app to delete."),
                     delete_data: Optional[bool] = Query(
                         False, description="Whether to delete all scenario data of this app."),
                     admin_user: AdminUser = Depends(get_current_admin_user)):
    logger.info(
        "%s requested to add remove app with ID: %s", admin_user.name, app_id)
    user_groups = get_user_groups(
        admin_user.auth_header, is_admin=True)
    if app_is_invisible(user_groups, app_id):
        logger.info("%s is not visible", app_id)
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="An app with this ID does not exist"
        )
    user_info = UserInfo(name=admin_user.name, groups=user_groups)
    try:
        await delete_app_internal(user_info, app_id, admin_user.auth_header, delete_data)
    except HTTPException as e:
        logger.info(
            "Problems deleting MIRO app with id: %s. Status code: %s. Details: %s", app_id, e.status_code, e.detail)
        raise e
    except Exception as e:
        logger.info(
            "Problems deleting MIRO app with id: %s. Details: %s", app_id, str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        )
    logger.info("App with id: %s successfully deleted by: %s.",
                app_id, admin_user.name)
    return {"description": "App successfully deleted"}
