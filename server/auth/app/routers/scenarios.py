import os
import tempfile
from fastapi.param_functions import Form, Query
from starlette.background import BackgroundTask
from starlette.responses import FileResponse
from typing import List, Optional
from fastapi import APIRouter, Depends, Path, File, HTTPException, UploadFile, status
from fastapi.logger import logger

from app.config import settings
from app.dependencies import get_current_app_user
from app.utils.models import ScenarioPermissions, User, ScenarioConfig, ExportFileType
from app.utils.scen_utils import delete_data, get_scen_list, download_data, add_data

router = APIRouter(
    prefix="/scenarios/{app_id}",
    tags=["scenarios"],
    dependencies=[Depends(get_current_app_user)],
    responses={401: {"message": "Unauthorized"},
               403: {"message": "Unauthorized access"},
               404: {"message": "Not found"},
               500: {"description": "Internal Server Error"}},
)

metadata = {
    "summary": {
        "get": "Get list of all visible scenarios",
        "post": "Add a new scenario",
        "delete": "Delete an existing scenario",
        "download": "Download a scenario"
    },
    "description": {
        "app_id": "The ID of the app whose data you want to operate on.",
        "name": "The name of the scenario you want to download.",
        "owner": "The owner of the scenario you want to download (by default, the logged in user).",
        "overwrite_data": "Whether to overwrite existing scenario data.",
        "read_perm": "Read permissions (by default all groups of logged in user). The MIRO Server REST API currently only supports assigning user groups, not individual users. Group names must be prefixed with a hash (`#`). The logged in user will always be appended to the specified permissions.",
        "write_perm": "Write permissions (by default logged in user). The MIRO Server REST API currently only supports assigning user groups, not individual users. Group names must be prefixed with a hash (`#`). The logged in user will always be appended to the specified permissions.",
        "exec_perm": "Execute permissions (by default all groups of logged in user). The MIRO Server REST API currently only supports assigning user groups, not individual users. Group names must be prefixed with a hash (`#`). The logged in user will always be appended to the specified permissions.",
        "file_type": "In which file type should the scenario be exported?",
        "scenario_data": f"A file that contains scenario data. Supported file types are: {','.join(settings.supported_data_filetypes)}"
    }
}


@ router.get("/", summary=metadata["summary"]["get"], response_model=List[ScenarioConfig])
async def get_scenario_list(app_id: str = Path(..., description=metadata['description']['app_id'], max_length=60),
                            user: User = Depends(get_current_app_user)):
    """
    Get all scenarios for this app that are visible to you.

    A scenario object contains the following fields:

    - **name**: The name of the scenario
    - **owner**: The owner of this scenario
    - **tags**: The tags of the scenario
    - **last_modified**: The timestamp when the scenario was last modified
    - **read_perm**: Array of users/groups that have permissions to read the scenario data (groups are indicated by a leading hash `#`)
    - **write_perm**: Array of users/groups that have permissions to modify the scenario data (groups are indicated by a leading hash `#`)
    - **exec_perm**: Array of users/groups that have permissions to execute the scenario (groups are indicated by a leading hash `#`)
    """
    logger.info("%s requested list of scenarios of app: %s",
                user.name, app_id)
    try:
        scen_list = await get_scen_list(user, app_id)
        logger.info("%s Scenario list of app: %s successfully returned.",
                    user.name, app_id)
        return scen_list
    except HTTPException as e:
        logger.info(
            "Problems adding new MIRO scenario. Status code: %s. Details: %s", e.status_code, e.detail)
        raise e
    except Exception as e:
        logger.info(
            "Problems adding new MIRO scenario. Details: %s", str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        )


@ router.delete("/", summary=metadata["summary"]["delete"],  responses={
    201: {"description": "Scenario successfully deleted"},
    404: {"description": "A scenario with this name does not exist"},
    423: {"description": "The scenario is locked and cannot be deleted"}})
async def delete_scenario(app_id: str = Path(..., description=metadata['description']['app_id'], max_length=60),
                          user: User = Depends(get_current_app_user),
                          name: str = Query(...,
                                            description=metadata['description']['name'], max_length=63),
                          owner: str = Query(None, description=metadata['description']['owner'], max_length=70)):
    """
    Delete an existing scenario.
    """
    logger.info("%s requested to delete scenario: %s of app: %s",
                user.name, name, app_id)
    try:
        await delete_data(user, app_id, name, owner)
    except HTTPException as e:
        logger.info(
            "Problems deleting MIRO scenario. Status code: %s. Details: %s", e.status_code, e.detail)
        raise e
    except Exception as e:
        logger.info(
            "Problems deleting MIRO scenario. Details: %s", str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        )
    logger.info("%s: Scenario successfully deleted", user.name)
    return {"detail": "Scenario successfully deleted"}


@ router.get("/download", summary=metadata["summary"]["download"], responses={
    422: {"description": "Duplicate records found when writing GDX file"}
}, response_class=FileResponse)
async def download_scenario(app_id: str = Path(..., description=metadata['description']['app_id'], max_length=60),
                            user: User = Depends(get_current_app_user),
                            name: str = Query(...,
                                              description=metadata['description']['name'], max_length=63),
                            owner: str = Query(
                                None, description=metadata['description']['owner'], max_length=70),
                            file_type: ExportFileType = Query(ExportFileType.miroscen, description=metadata['description']['file_type'])):
    """
    Download a scenario.
    """
    logger.info("%s requested to download scenario: %s of app: %s",
                user.name, name, app_id)

    temp_file = tempfile.NamedTemporaryFile(delete=False, suffix="."+file_type)
    temp_file.close()

    def cleanup():
        try:
            os.remove(temp_file.name)
        except OSError:
            pass

    try:
        await download_data(user, app_id, temp_file.name, file_type, name, owner)

        logger.info("%s: Scenario: %s of app: %s successfully downloaded",
                    user.name, name, app_id)
        return FileResponse(
            temp_file.name,
            filename=app_id + name + "." + file_type,
            background=BackgroundTask(cleanup),
        )
    except HTTPException as e:
        cleanup()
        logger.info(
            "Problems downloading MIRO scenario. Status code: %s. Details: %s", e.status_code, e.detail)
        raise e
    except Exception as e:
        cleanup()
        logger.info(
            "Problems downloading MIRO scenario. Details: %s", str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        )


@ router.post("/", summary=metadata["summary"]["post"], responses={
    201: {"description": "Scenario successfully added"},
    409: {"description": "A scenario with this name already exists"},
    423: {"description": "The scenario is locked and cannot be overwritten"}},
    status_code=status.HTTP_201_CREATED)
async def add_scenario(app_id: str = Path(..., description=metadata['description']['app_id'], max_length=60),
                       user: User = Depends(get_current_app_user),
                       scenario_data: UploadFile = File(
                           ..., description=metadata["description"]["scenario_data"]),
                       overwrite_data: Optional[bool] = Form(
                           False, description=metadata["description"]["overwrite_data"]),
                       read_perm: Optional[List[str]] = Form(
                           None, description=metadata["description"]["read_perm"]),
                       write_perm: Optional[List[str]] = Form(
                           None, description=metadata["description"]["write_perm"]),
                       exec_perm: Optional[List[str]] = Form(None, description=metadata["description"]["exec_perm"])):
    """
    Add a new scenario.
    """
    logger.info("%s requested to add scenario to app: %s", user.name, app_id)
    permissions = ScenarioPermissions(
        read_perm=read_perm, write_perm=write_perm, exec_perm=exec_perm)
    try:
        await add_data(user, app_id, scenario_data, permissions, overwrite_data=overwrite_data)
    except HTTPException as e:
        logger.info(
            "Problems adding new MIRO scenario. Status code: %s. Details: %s", e.status_code, e.detail)
        raise e
    except Exception as e:
        logger.info(
            "Problems adding new MIRO scenario. Details: %s", str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error"
        )
    logger.info("%s: Scenario successfully added", user.name)
    return {"detail": "Scenario successfully added"}
