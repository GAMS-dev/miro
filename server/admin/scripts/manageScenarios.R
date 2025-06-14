library(jsonlite)
stdin <- file("stdin")
stdinContent <- suppressWarnings(readLines(stdin))
metadata <- fromJSON(stdinContent)
close(stdin)

appId <- metadata[["appId"]]


overwriteScen <- identical(metadata[["overwriteData"]], TRUE)
dataPath <- metadata[["dataPath"]]

dontcare <- lapply(c("global.R", list.files("./R", full.names = TRUE)), source)

ADD_DATA_TIMEOUT <- as.integer(Sys.getenv("ADD_DATA_TIMEOUT", "3600"))
if (is.na(ADD_DATA_TIMEOUT)) {
  ADD_DATA_TIMEOUT <- 3600L
}

tryCatch(
  {
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

appConfig <- modelConfig$getAppConfigFull(appId)
modelName <- basename(
  modelConfig$getEnvValue(appConfig$containerEnv[["MIRO_MODEL_PATH"]])
)
appDbCredentials <- modelConfig$getAppDbConf(appId)

procEnv <- as.list(Sys.getenv())
procEnv[["MIRO_DB_USERNAME"]] <- appDbCredentials$user
procEnv[["MIRO_DB_PASSWORD"]] <- appDbCredentials$password
procEnv[["MIRO_DB_SCHEMA"]] <- appDbCredentials$user
procEnv[["MIRO_POPULATE_DB"]] <- "true"
procEnv[["MIRO_VERSION_STRING"]] <- modelConfig$getEnvValue(appConfig$containerEnv[["MIRO_VERSION_STRING"]])
procEnv[["MIRO_MODEL_PATH"]] <- file.path(getModelPath(appId), modelName)
procEnv[["MIRO_DATA_DIR"]] <- dataPath
procEnv[["MIRO_OVERWRITE_SCEN_IMPORT"]] <- if (!identical(overwriteScen, TRUE)) "ask" else "true"
migrationConfigPath <- tempfile(fileext = ".json")
procEnv[["MIRO_MIGRATION_CONFIG_PATH"]] <- migrationConfigPath

stdin <- NULL
forwardStderr <- FALSE

if (identical(metadata[["mode"]], "download")) {
  tmpFile <- tempfile()
  writeLines(stdinContent, tmpFile)
  stdin <- tmpFile
  procEnv[["MIRO_API_DOWNLOAD_SCEN"]] <- "true"
} else if (identical(metadata[["mode"]], "delete")) {
  procEnv[["MIRO_API_DELETE_SCEN"]] <- "true"
  tmpFile <- tempfile()
  writeLines(stdinContent, tmpFile)
  stdin <- tmpFile
} else if (identical(metadata[["mode"]], "getList")) {
  procEnv[["MIRO_API_GET_SCEN_LIST"]] <- "true"
  procEnv[["MIRO_API_PAGE"]] <- metadata[["page"]]
  procEnv[["MIRO_API_PER_PAGE"]] <- metadata[["perPage"]]
  forwardStderr <- TRUE
} else {
  tmpFile <- tempfile()
  writeLines(stdinContent, tmpFile)
  stdin <- tmpFile
  forwardStderr <- TRUE
}

runMiroProcAPI(appId, procEnv, MIRO_APP_PATH, ADD_DATA_TIMEOUT,
  stdin = stdin,
  forwardStderr = forwardStderr
)
