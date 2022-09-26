import os
import re
from typing import List
import yaml
from pydantic import BaseSettings, Field

data_dir_tmp = os.environ.get('DATA_DIR')
if not data_dir_tmp:
    data_dir_tmp = '/home/miro/admin/data'

settings_yml = {}
try:
    with open(os.path.join(data_dir_tmp, "application.yml"), "r") as f:
        settings_yml = yaml.load(f, Loader=yaml.CSafeLoader)["proxy"]
except FileNotFoundError:
    pass

with open("/home/miro/admin/global.R", "r") as f:
    MIRO_SERVER_VERSION = re.search(
        'MIRO_VERSION\s*<-\s*"(\d+\.\d+\.\d+)"\s*', f.read()).group(1)

FORCE_SIGNED_APPS = "force-signed-apps" in settings_yml and settings_yml["force-signed-apps"] == True
FORCE_SIGNED_APPS = "true" if FORCE_SIGNED_APPS else "false"


class Settings(BaseSettings):
    engine_url: str
    engine_ns: str
    session_timeout: int = 3600*12
    add_data_timeout: int = 3600
    miro_server_version: str = Field(MIRO_SERVER_VERSION, const=True)
    force_signed_apps: str = Field(FORCE_SIGNED_APPS, const=True)
    supported_data_filetypes: List[str] = [
        'gdx', 'miroscen', 'xlsx', 'xlsm', 'xls', 'zip']
    model_dir: str = '/home/miro/admin/models'
    data_dir: str = '/home/miro/admin/data'
    admin_app_dir: str = '/home/miro/admin'
    gms_miro_database_host: str = 'miroserver-db'
    gms_miro_database_port: int = 5432
    gms_miro_database: str = 'gamsmiro'
    gms_miro_database_user: str = 'GMSMASTER'
    gms_miro_database_pwd: str = ''
    script_name: str = ''
    request_timeout: int = 10


settings = Settings()
