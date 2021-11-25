import os
import json
from typing import List, Optional
from fastapi import UploadFile
from fastapi.exceptions import HTTPException
from starlette import status
import aiofiles
import yaml

from app.config import settings
from app.utils.models import AppConfig, User
from app.utils.miro_proc import run_miro_proc


def get_apps_raw(user_groups: Optional[List[str]] = None, all_apps: bool = False) -> List[AppConfig]:
    apps = []
    with open(os.path.join(settings.data_dir, "specs.yaml"), "r") as f:
        apps = yaml.load(f, Loader=yaml.CSafeLoader)["specs"]
    if all_apps:
        return apps

    user_groups_lowercase = [user_group.lower() for user_group in user_groups]
    visible_apps = []
    for app in apps:
        if app["id"] == "admin":
            continue
        if "displayName" in app:
            app["display_name"] = app.pop("displayName")
        if "accessGroups" in app:
            app["access_groups"] = app.pop("accessGroups")
        else:
            visible_apps.append(app)
            app["access_groups"] = []
            continue
        if not len(app["access_groups"]):
            visible_apps.append(app)
            continue
        access_groups_lowercase = [access_group.lower()
                                   for access_group in app["access_groups"]]
        app["access_groups"] = list(set(
            access_groups_lowercase) & set(user_groups_lowercase))
        if len(app["access_groups"]):
            visible_apps.append(app)

    return visible_apps


def app_is_invisible(user_groups: List[str], app_id: str) -> bool:
    return not os.path.isdir(os.path.join(settings.model_dir, app_id)) or \
        app_id not in [app["id"] for app in get_apps_raw(
            user_groups=user_groups)]


async def add_or_update_app(user_info: User, app_config: AppConfig, data: UploadFile, overwrite_data: bool = False, update=False) -> None:
    _, file_extension = os.path.splitext(data.filename)
    file_extension = file_extension.lower()
    if file_extension != ".miroapp":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file extension: {}. Please upload a valid miroapp file.".format(
                file_extension)
        )
    async with aiofiles.tempfile.NamedTemporaryFile("wb", suffix=".miroapp") as out_file:
        while content := await data.read(1024):
            await out_file.write(content)
        await out_file.flush()

        proc_input = json.dumps({
            "id": app_config.id,
            "displayName": app_config.display_name,
            "description": app_config.description,
            "accessGroups": app_config.access_groups,
            "appPath": out_file.name,
            "overwriteData": overwrite_data,
            "update": update}).encode()
        run_miro_proc(user_info, "addApp.R", input=proc_input)


async def delete_app_internal(user_info: User, app_id: str, delete_data: bool = False) -> None:
    proc_input = json.dumps({"id": app_id,
                             "deleteData": delete_data}).encode()
    run_miro_proc(user_info, "deleteApp.R", input=proc_input)
