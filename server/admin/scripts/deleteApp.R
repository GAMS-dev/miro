library(jsonlite)
stdin <- file("stdin")
metadata <- fromJSON(suppressWarnings(readLines(stdin)))
close(stdin)

appId <- metadata[["id"]]

deleteData <- identical(metadata[["deleteData"]], TRUE)
appPath <- metadata[["appPath"]]

dontcare <- lapply(c("global.R", list.files("./R", full.names = TRUE)), source)

tryCatch(
  {
    db <- MiroDb$new(list(
      host = Sys.getenv("MIRO_DB_HOST", "localhost"),
      port = as.integer(Sys.getenv("MIRO_DB_PORT", "5432")),
      name = Sys.getenv("MIRO_DB_NAME"),
      username = Sys.getenv("MIRO_DB_USERNAME"),
      password = Sys.getenv("MIRO_DB_PASSWORD")
    ))
    engineClient <- EngineClient$new()
    engineClient$setAuthHeader(Sys.getenv("MIRO_ENGINE_AUTH_HEADER"))
    modelConfig <- ModelConfig$new(SPECS_YAML_PATH)
  },
  error = function(e) {
    write(sprintf(
      "merr:::500:::Problems initializing required components. Error message: %s",
      conditionMessage(e)
    ), stderr())
    quit("no", 1L, FALSE)
  }
)

tryCatch(
  {
    appIndex <- modelConfig$getAppIndex(appId)
    if (deleteData) {
      db$removeAppSchema(appId)
      flog.info("Data for MIRO app: %s removed successfully.", appId)
    }

    removeAppData(
      appId, modelConfig$getAppLogo(appIndex),
      modelConfig$getAppFavicon(appIndex)
    )
    modelConfig$remove(appIndex)

    engineClient$deregisterModel(appId)
  },
  error = function(e) {
    write(sprintf(
      "merr:::500:::Problems removing MIRO app. Error message: %s",
      conditionMessage(e)
    ), stderr())
    quit("no", 1L, FALSE)
  }
)
