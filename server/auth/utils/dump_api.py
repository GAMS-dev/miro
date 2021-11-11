from fastapi.openapi.utils import get_openapi
from main import public_api
import json

print(json.dumps(get_openapi(
    title=public_api.title,
    version=public_api.version,
    openapi_version=public_api.openapi_version,
    description=public_api.description,
    routes=public_api.routes
)))
