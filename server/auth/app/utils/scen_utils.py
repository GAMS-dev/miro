import os
import json
from typing import List
import aiofiles
from fastapi import UploadFile
from fastapi.logger import logger
from fastapi.exceptions import HTTPException
from starlette import status

from app.config import settings
from app.utils.models import ExportFileType, ScenarioConfig, ScenarioPermissions, User
from app.utils.miro_proc import run_miro_proc


async def add_data(user_info: User, app_id: str, data: UploadFile, permissions: ScenarioPermissions,
                   overwrite_data: bool = False) -> None:
    file_name, file_extension = os.path.splitext(data.filename)
    file_extension = file_extension.lower()
    if file_extension[1:] not in settings.supported_data_filetypes:
        logger.info(
            "Invalid file extension: %s for scenario data.", file_extension)
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid file extension: {}. Supported file extensions: {}.".format(
                file_extension, ",".join(settings.supported_data_filetypes))
        )
    proc_input = {
        "appId": app_id,
        "overwriteData": overwrite_data,
        "forceOverwrite": False}

    api_miro_perm_key_map = {
        "read_perm": "readPerm",
        "write_perm": "writePerm",
        "exec_perm": "execPerm"
    }

    for key, val in permissions:
        if val != None:
            for permission in val:
                if permission.startswith("#"):
                    if permission[1:] not in user_info.groups:
                        logger.info("Invalid user group (%s): %s",
                                    key, permission)
                        raise HTTPException(
                            status_code=status.HTTP_400_BAD_REQUEST, detail="Invalid user group: {}".format(
                                permission)
                        )
                elif permission != user_info.name:
                    logger.info(
                        "Invalid permission (%s): %s (only user groups supported for now).", key, permission)
                    raise HTTPException(
                        status_code=status.HTTP_400_BAD_REQUEST, detail="The GAMS MIRO Server REST API currently only supports assigning access permissions to user groups and not to users ({}). Please prefix group labels with a hash (#).".format(
                            permission)
                    )
            proc_input[api_miro_perm_key_map[key]] = list(
                set([user_info.name] + val))

    async with aiofiles.tempfile.NamedTemporaryFile("wb", suffix=file_extension) as out_file:
        while content := await data.read(1024):
            await out_file.write(content)
        await out_file.flush()

        proc_input["dataPath"] = out_file.name
        if file_extension != ".miroscen":
            proc_input["scenNameOverwrite"] = file_name

        try:
            run_miro_proc(user_info, "manageScenarios.R",
                          input=json.dumps(proc_input).encode())
        except HTTPException as e:
            logger.info("Problems adding data to app: %s. Detail: %s", app_id, e.detail)
            if e.status_code == 418:
                # need to change status code when scenario already exists
                raise HTTPException(
                    status_code=status.HTTP_409_CONFLICT, detail="A scenario with this name already exists")
            raise HTTPException(
                status_code=e.status_code, detail=e.detail)


async def delete_data(user_info: User, app_id: str, scen_name: str, scen_owner: str) -> None:
    proc_input = json.dumps({
        "mode": "delete",
        "appId": app_id,
        "deleteScenName": scen_name,
        "deleteScenOwner": scen_owner
    }).encode()
    run_miro_proc(user_info, "manageScenarios.R", input=proc_input)


async def download_data(user_info: User, app_id: str, file_path: str, file_type: ExportFileType, scen_name: str, scen_owner: str) -> None:
    proc_input = json.dumps({
        "mode": "download",
        "appId": app_id,
        "downloadPath": file_path,
        "downloadFileType": file_type,
        "downloadScenName": scen_name,
        "downloadScenOwner": scen_owner}).encode()
    run_miro_proc(user_info, "manageScenarios.R", input=proc_input)


async def get_scen_list(user_info: User, app_id: str) -> List[ScenarioConfig]:
    proc_input = json.dumps({
        "mode": "getList",
        "appId": app_id}).encode()
    stderr = run_miro_proc(user_info, "manageScenarios.R", input=proc_input)
    json_start_pos = stderr.find("merr:::200:::")
    if json_start_pos == -1:
        logger.warning(
            "Invalid stderr received from R process: %s", stderr[:3000])
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Internal Server Error"
        )
    json_start_pos += len("merr:::200:::")
    try:
        return json.loads(stderr[json_start_pos:])
    except json.decoder.JSONDecodeError:
        logger.warning("Invalid JSON received from R process: %s",
                       stderr[json_start_pos:3000])
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Internal Server Error"
        )
