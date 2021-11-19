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


class AuthResponse(BaseModel):
    token: str
    roles: List[str]


class AppConfig(BaseModel):
    id: Optional[str]
    display_name: Optional[str]
    description: Optional[str]
    access_groups: List[str]
