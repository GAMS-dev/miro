import os
import yaml
from pydantic import BaseSettings, Field

data_dir_tmp = os.environ.get('DATA_DIR')
if not data_dir_tmp:
    data_dir_tmp = '/home/miro/admin/data'

settings_yml = {}
with open(os.path.join(data_dir_tmp, "application.yml"), "r") as f:
    settings_yml = yaml.load(f, Loader=yaml.CSafeLoader)["proxy"]

FORCE_SIGNED_APPS = "force-signed-apps" in settings_yml and settings_yml["force-signed-apps"] == True
FORCE_SIGNED_APPS = "true" if FORCE_SIGNED_APPS else "false"


class Settings(BaseSettings):
    engine_url: str
    engine_ns: str
    session_timeout: int = 3600*12
    add_data_timeout: int = 3600
    force_signed_apps: str = Field(FORCE_SIGNED_APPS, const=True)
    model_dir: str = '/home/miro/admin/models'
    data_dir: str = '/home/miro/admin/data'
    admin_app_dir: str = '/home/miro/admin'
    gms_miro_database_host: str = 'miroserver-db'
    gms_miro_database_port: int = 5432
    gms_miro_database: str = 'gamsmiro'
    gms_miro_database_user: str = 'GMSMASTER'
    gms_miro_database_pwd: str


settings = Settings()
