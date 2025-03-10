import datetime
from enum import Enum
from typing import Annotated, TypeVar, Generic

from pydantic import BaseModel, Field, StringConstraints, TypeAdapter

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


class AppEnvironmentItem(BaseModel):
    value: Annotated[str, StringConstraints(max_length=1000)]
    description: Annotated[str, StringConstraints(max_length=1000)] | None = None


AppEnvironment = dict[
    Annotated[str, StringConstraints(pattern="^[A-Z_][A-Z0-9_]*$")],
    AppEnvironmentItem,
]


class AppConfigInput(BaseModel):
    id: str | None
    display_name: str | None
    description: str | None
    environment: AppEnvironment = {}
    access_groups: list[str] = []
    overwrite_data: bool = Field(False, description="test")


class AppConfigOutput(BaseModel):
    id: str
    display_name: str | None = Field(validation_alias="alias")
    description: str | None = Field(validation_alias="desc")
    access_groups: list[str] = Field(validation_alias="groups")
    environment: AppEnvironment = Field({}, validation_alias="appEnv")
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
