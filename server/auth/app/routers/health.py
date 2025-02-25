from fastapi import APIRouter

router = APIRouter(
    prefix="/health",
    tags=["health"],
    responses={200: {"message": "Ready"},
               503: {"description": "Not ready"}},
)


@router.get("/liveness")
async def liveness():
    """
      Check basic app health (always returns healthy)
    """
    return {"status": "alive"}
