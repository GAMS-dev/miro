import json
import os

import aiofiles
import yaml
from fastapi import UploadFile
from fastapi.exceptions import HTTPException
from pydantic import ValidationError
from starlette import status

from app.config import logger, settings
from app.utils.miro_proc import run_miro_proc
from app.utils.models import AppConfigInput, AppConfigOutput, User


def get_apps_internal(user_info: User) -> list[AppConfigOutput]:
    stderr = run_miro_proc(user_info, "listApps.R")
    json_start_pos = stderr.find("merr:::200:::")
    if json_start_pos == -1:
        logger.warning("Invalid stderr received from R process: %s", stderr[:3000])
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error",
        )
    json_start_pos += len("merr:::200:::")
    try:
        return AppConfigOutput.model_validate_json(stderr[json_start_pos:])
    except ValidationError as exc:
        logger.warning(
            "Invalid JSON received from R process: %s", stderr[json_start_pos:3000]
        )
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error",
        ) from exc


def get_apps_raw(user_groups: list[str] = None) -> list[AppConfigOutput]:
    apps = []
    with open(settings.specs_yaml_path, "r", encoding="utf-8") as f_apps:
        apps = yaml.load(f_apps, Loader=yaml.CSafeLoader)["specs"]

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
        if not app["access_groups"]:
            visible_apps.append(app)
            continue
        access_groups_lowercase = [
            access_group.lower() for access_group in app["access_groups"]
        ]
        app["access_groups"] = list(
            set(access_groups_lowercase) & set(user_groups_lowercase)
        )
        if len(app["access_groups"]):
            visible_apps.append(app)

    return visible_apps


def app_is_invisible(user_groups: list[str], app_id: str) -> bool:
    return app_id not in [app["id"] for app in get_apps_raw(user_groups=user_groups)]


async def add_or_update_app(
    user_info: User,
    app_config: AppConfigInput,
    data: UploadFile,
    overwrite_data: bool = False,
    update=False,
) -> None:
    _, file_extension = os.path.splitext(data.filename)
    file_extension = file_extension.lower()
    if file_extension != ".miroapp":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid file extension: {file_extension}. Please upload a valid miroapp file.",
        )
    async with aiofiles.tempfile.NamedTemporaryFile(
        "wb", suffix=".miroapp"
    ) as out_file:
        while content := await data.read(1024):
            await out_file.write(content)
        await out_file.flush()

        proc_input = json.dumps(
            {
                "id": app_config.id,
                "displayName": app_config.display_name,
                "description": app_config.description,
                "accessGroups": app_config.access_groups,
                "appPath": out_file.name,
                "overwriteData": overwrite_data,
                "update": update,
            }
        ).encode()
        run_miro_proc(user_info, "addApp.R", proc_input=proc_input)


async def delete_app_internal(
    user_info: User, app_id: str, delete_data: bool = False
) -> None:
    proc_input = json.dumps({"id": app_id, "deleteData": delete_data}).encode()
    run_miro_proc(user_info, "deleteApp.R", proc_input=proc_input)
