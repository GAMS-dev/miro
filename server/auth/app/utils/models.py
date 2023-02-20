import datetime
from enum import Enum
from pydantic import BaseModel
from typing import List, Optional


class User(BaseModel):
    name: str
    auth_header: str
    permissions: Optional[int] = None
    is_admin: Optional[bool] = False
    groups: List[str]


class AuthRequest(BaseModel):
    username: str
    password: str


class OidcAuthRequest(BaseModel):
    id_token: str


class OidcLoginData(BaseModel):
    bearer_token: str
    username: str


class AuthResponse(BaseModel):
    token: str
    roles: List[str]
    permissions: str


class OidcAuthResponse(BaseModel):
    token: str
    roles: List[str]
    permissions: str
    username: str


class ConfigurationResponse(BaseModel):
    version: str
    authentication_mode: str


class AppConfig(BaseModel):
    id: Optional[str]
    display_name: Optional[str]
    description: Optional[str]
    access_groups: List[str]


class ScenarioConfig(BaseModel):
    name: str
    tags: List[str]
    owner: str
    last_modified: datetime.datetime
    read_perm: List[str]
    write_perm: List[str]
    exec_perm: List[str]


class ScenarioPermissions(BaseModel):
    read_perm: Optional[List[str]] = None
    write_perm: Optional[List[str]] = None
    exec_perm: Optional[List[str]] = None


class ExportFileType(str, Enum):
    miroscen = "miroscen"
    gdx = "gdx"
    csv = "csv"
    xlsx = "xlsx"
