import datetime
from enum import Enum
from typing import TypeVar, Generic

from pydantic import BaseModel, Field

T = TypeVar("T", bound=BaseModel)


class PaginatedResponse(BaseModel, Generic[T]):
    items: list[T]
    total_count: int


class User(BaseModel):
    name: str
    auth_header: str
    permissions: int | None = None
    is_admin: bool | None = False
    groups: list[str]


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
    roles: list[str]
    permissions: str


class OidcAuthResponse(BaseModel):
    token: str
    roles: list[str]
    permissions: str
    username: str


class ConfigurationResponse(BaseModel):
    version: str
    authentication_mode: str


class AppConfigInput(BaseModel):
    app_id: str | None
    display_name: str | None
    description: str | None
    access_groups: list[str]


class AppConfigOutput(BaseModel):
    id: str
    display_name: str | None = Field(validation_alias="alias")
    description: str | None = Field(validation_alias="desc")
    access_groups: list[str] = Field(validation_alias="groups")
    environment: str | None = Field(validation_alias="appEnv")
    version: str | None = None
    authors: list[str] | None = None


class ScenarioConfig(BaseModel):
    name: str
    tags: list[str]
    owner: str
    last_modified: datetime.datetime
    read_perm: list[str]
    write_perm: list[str]
    exec_perm: list[str]


class ScenarioPermissions(BaseModel):
    read_perm: list[str] | None = None
    write_perm: list[str] | None = None
    exec_perm: list[str] | None = None


class ExportFileType(str, Enum):
    MIROSCEN = "miroscen"
    GDX = "gdx"
    CSV = "csv"
    XLSX = "xlsx"
