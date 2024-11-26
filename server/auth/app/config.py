import os
import re

import yaml
from pydantic import Field, ConfigDict
from pydantic_settings import BaseSettings

app_config_file_path = os.environ.get('APP_CONFIG_FILE_PATH')
if not app_config_file_path:
    data_dir_tmp = os.environ.get('DATA_DIR')
    if not data_dir_tmp:
        data_dir_tmp = '/home/miro/admin/data'
    app_config_file_path = os.path.join(data_dir_tmp, "application.yml")

settings_yml = {}
try:
    with open(app_config_file_path, "r", encoding="utf-8") as f:
        settings_yml = yaml.load(f, Loader=yaml.CSafeLoader)["proxy"]
except FileNotFoundError:
    pass

with open("/home/miro/admin/global.R", "r", encoding="utf-8") as f:
    MIRO_SERVER_VERSION = re.search(
        r'MIRO_VERSION\s*<-\s*"(\d+\.\d+\.\d+)"\s*', f.read()).group(1)

FORCE_SIGNED_APPS = "force-signed-apps" in settings_yml and settings_yml["force-signed-apps"] == True
FORCE_SIGNED_APPS = "true" if FORCE_SIGNED_APPS else "false"

OIDC_LOGIN = "authentication" in settings_yml and settings_yml["authentication"] == "openid"


class Settings(BaseSettings):
    engine_url: str
    engine_ns: str
    session_timeout: int = 3600*12
    add_data_timeout: int = 3600
    authentication_mode: str = Field(
        "oidc" if OIDC_LOGIN else "engine")
    miro_server_version: str = Field(MIRO_SERVER_VERSION, frozen=True)
    force_signed_apps: str = Field(FORCE_SIGNED_APPS, frozen=True)
    supported_data_filetypes: list[str] = [
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
    model_config = ConfigDict(
        protected_namespaces=('settings_', )
    )


settings = Settings()
