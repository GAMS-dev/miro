import asyncio
import logging
import os
from typing import Dict

from fastapi import HTTPException, status

from app.config import settings
from app.utils.models import User

logger = logging.getLogger(__name__)


def get_miro_proc_env(user_info: User) -> Dict[str, str]:
    proc_env = {}
    proc_env.update(os.environ)
    proc_env.update(
        {
            "MIRO_DB_HOST": settings.gms_miro_database_host,
            "MIRO_DB_PORT": str(settings.gms_miro_database_port),
            "MIRO_DB_USERNAME": settings.gms_miro_database_user,
            "MIRO_DB_NAME": settings.gms_miro_database,
            "MIRO_DB_PASSWORD": settings.gms_miro_database_pwd,
            "MIRO_ENGINE_HOST": settings.engine_url,
            "MIRO_ENGINE_NAMESPACE": settings.engine_ns,
            "MIRO_ENGINE_AUTH_HEADER": user_info.auth_header,
            "ADD_DATA_TIMEOUT": str(settings.add_data_timeout),
            "MIRO_ENFORCE_SIGNED_APPS": settings.force_signed_apps,
            "SHINYPROXY_USERNAME": user_info.name,
            "SHINYPROXY_USERGROUPS": ",".join(user_info.groups).upper(),
        }
    )
    return proc_env


async def run_miro_proc_async(
    user_info: User,
    script_name: str,
    proc_input: bytes = b"",
    cwd: str = settings.admin_app_dir,
) -> str:
    proc_env = get_miro_proc_env(user_info)
    command = [
        "R",
        "--no-echo",
        "--no-save",
        "--no-restore",
        "--no-site-file",
        "--no-init-file",
        "-f",
        os.path.join(settings.admin_app_dir, "scripts", script_name),
    ]
    proc = await asyncio.create_subprocess_exec(
        *command,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        cwd=cwd,
        env=proc_env,
    )
    stdout_bytes, stderr_bytes = await proc.communicate(input=proc_input)
    stdout_decoded = stdout_bytes.decode()
    stderr_decoded = stderr_bytes.decode()

    logger.info("Stdout of %s subprocess: %s", script_name, stdout_decoded[:3000])
    for line in stderr_decoded.split("\n"):
        if not line:
            continue
        err = line.split(":::")
        if err[0] == "merr":
            err_details = "Internal error"
            err_code = 500
            try:
                err_details = str(err[2])
                err_code = int(err[1])
            except (ValueError, IndexError):
                logger.warning(
                    "Invalid error message received from %s subprocess: %s",
                    script_name,
                    line,
                )
            if err_code == 500:
                logger.info("Stderr of %s subprocess: %s", script_name, stderr_decoded)
            if err_code >= 500:
                logger.info(
                    "Internal error when running MIRO subprocess. Details: %s",
                    err_details,
                )
                raise HTTPException(
                    status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                    detail="Internal Server Error",
                )
            if err_code >= 300:
                raise HTTPException(status_code=err_code, detail=err_details)
    if proc.returncode != 0:
        logger.warning(
            "Problems running %s subprocess. Stderr: %s",
            script_name,
            stderr_decoded[:3000],
        )
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Internal Server Error",
        )

    return stderr_decoded
