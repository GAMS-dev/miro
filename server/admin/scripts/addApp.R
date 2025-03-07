library(jsonlite)
stdin <- file("stdin")
metadata <- fromJSON(suppressWarnings(readLines(stdin)))
close(stdin)

appId <- metadata[["id"]]
appUserGroups <- metadata[["accessGroups"]]
updateApp <- identical(metadata[["update"]], TRUE)

overwriteScen <- identical(metadata[["overwriteData"]], TRUE)
appPath <- metadata[["appPath"]]

dontcare <- lapply(c("global.R", list.files("./R", full.names = TRUE)), source)

ADD_DATA_TIMEOUT <- as.integer(Sys.getenv("ADD_DATA_TIMEOUT", "3600"))
if (is.na(ADD_DATA_TIMEOUT)) {
  ADD_DATA_TIMEOUT <- 3600L
}

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

miroAppValidator <- MiroAppValidator$new(engineClient)

tryCatch(
  {
    miroAppValidator$validate(appPath)
  },
  error = function(e) {
    write(sprintf(
      "merr:::400:::Invalid app bundle uploaded. Error message: %s",
      conditionMessage(e)
    ), stderr())
    quit("no", 1L, FALSE)
  }
)

tryRemoveAppSchema <- function(appId) {
  tryCatch(
    db$removeAppSchema(appId),
    error = function(e) {
      print(sprintf("Problems removing app schema. Error message: %s", conditionMessage(e)))
    }
  )
}
appDir <- NULL
cleanup <- function() {
  if (!updateApp) {
    if (!is.null(appDir)) {
      unlink(appDir, recursive = TRUE, force = TRUE)
    }
    tryRemoveAppSchema(appId)
  }
}
tryCatch(
  {
    if (!length(appId) || !nchar(appId)) {
      appId <- miroAppValidator$getAppId()
    } else {
      appId <- tolower(trimws(appId))
    }
    modelName <- miroAppValidator$getModelName()
    modelId <- miroAppValidator$getModelId()

    appVersion <- miroAppValidator$getAppVersion()
    appAuthors <- miroAppValidator$getAppAuthors()

    appDirTmp <- getModelPath(paste0("~$", appId))
    dataDirTmp <- getDataPath(paste0("~$", appId))

    if (updateApp) {
      removeTempDirs(appId)
      appConfig <- modelConfig$getAppConfigFull(appId)
      appDbCredentials <- modelConfig$getAppDbConf(appId)

      appConfig$containerEnv[["MIRO_VERSION_STRING"]] <- miroAppValidator$getMIROVersion()
      if (IN_KUBERNETES) {
        appConfig$containerEnv[["MIRO_MODEL_PATH"]] <- paste0("/home/miro/model/", modelName)
      } else {
        appConfig$containerEnv[["MIRO_MODEL_PATH"]] <- paste0("/home/miro/app/model/", appId, "/", modelName)
      }
      for (key in c("displayName", "description", "accessGroups")) {
        if (length(metadata[[key]])) {
          valueTrimmed <- trimws(metadata[[key]])
          valueTrimmed <- valueTrimmed[nchar(valueTrimmed) > 0L]
          if (length(valueTrimmed)) {
            appConfig[[key]] <- valueTrimmed
          }
        } else if (key == "accessGroups") {
          appConfig[[key]] <- c()
        } else if (key == "description") {
          appConfig[[key]] <- ""
        }
      }
      extractAppData(appPath, paste0("~$", appId), modelId)
      engineClient$updateModel(appId, userGroups = FALSE, modelDataPath = file.path(appDirTmp, paste0(modelId, ".zip")))
    } else {
      tryCatch(
        {
          createAppDir(appId)
        },
        error_model_dir_exists = function(e) {
          write("merr:::400:::An app with this id already exists", stderr())
          quit("no", 1L, FALSE)
        }
      )
      appDbCredentials <- db$createAppSchema(appId)
      logoPath <- miroAppValidator$getLogoFile()
      logoURL <- "default_logo.png"
      if (length(logoPath)) {
        logoURL <- getLogoName(appId, logoPath)
      } else {
        logoPath <- NULL
      }
      newAppConfig <- list(
        id = appId, displayName = miroAppValidator$getAppTitle(), description = miroAppValidator$getAppDesc(),
        logoURL = logoURL,
        containerEnv = list(
          MIRO_VERSION_STRING = miroAppValidator$getMIROVersion(),
          MIRO_MODE = "base",
          MIRO_ENGINE_MODELNAME = appId,
          MIRO_DB_USERNAME = appDbCredentials$user,
          MIRO_DB_PASSWORD = appDbCredentials$password,
          MIRO_DB_SCHEMA = appDbCredentials$user
        ),
        extraData = list(
          appVersion = appVersion,
          appAuthors = appAuthors
        )
      )
      if (IN_KUBERNETES) {
        newAppConfig$containerEnv$MIRO_MODEL_PATH <- paste0(
          "/home/miro/mnt/model/", modelName
        )
        newAppConfig$containerEnv$MIRO_DATA_DIR <- "/home/miro/data"
        newAppConfig$containerEnv$MIRO_CACHE_DIR <- "/home/miro/cache"
      } else {
        newAppConfig$containerVolumes <- c(
          sprintf("/%s:/home/miro/app/model/%s:ro", appId, appId),
          sprintf("/data_%s:/home/miro/app/data", appId)
        )
        newAppConfig$containerEnv$MIRO_MODEL_PATH <- paste0(
          "/home/miro/app/model/",
          appId, "/", modelName
        )
        newAppConfig$containerEnv$MIRO_DATA_DIR <- "/home/miro/app/data"
      }
      for (key in c("displayName", "description")) {
        if (length(metadata[[key]])) {
          valueTrimmed <- trimws(metadata[[key]])
          valueTrimmed <- valueTrimmed[nchar(valueTrimmed) > 0L]
          if (length(valueTrimmed)) {
            newAppConfig[[key]] <- valueTrimmed
          }
        }
      }
      if (length(appUserGroups)) {
        newAppConfig[["accessGroups"]] <- as.list(appUserGroups)
      }

      extractAppData(
        appPath, appId, modelId
      )
      addAppLogo(appId, logoPath, logoURL)
    }

    procEnv <- as.list(Sys.getenv())
    procEnv[["MIRO_DB_USERNAME"]] <- appDbCredentials$user
    procEnv[["MIRO_DB_PASSWORD"]] <- appDbCredentials$password
    procEnv[["MIRO_DB_SCHEMA"]] <- appDbCredentials$user
    procEnv[["MIRO_POPULATE_DB"]] <- "true"
    procEnv[["MIRO_VERSION_STRING"]] <- miroAppValidator$getMIROVersion()
    if (updateApp) {
      procEnv[["MIRO_MODEL_PATH"]] <- file.path(appDirTmp, modelName)
      procEnv[["MIRO_DATA_DIR"]] <- dataDirTmp
    } else {
      procEnv[["MIRO_MODEL_PATH"]] <- file.path(appDir, modelName)
      procEnv[["MIRO_DATA_DIR"]] <- dataDir
    }
    procEnv[["MIRO_OVERWRITE_SCEN_IMPORT"]] <- if (!identical(overwriteScen, TRUE)) "false" else "true"
    migrationConfigPath <- tempfile(fileext = ".json")
    procEnv[["MIRO_MIGRATION_CONFIG_PATH"]] <- migrationConfigPath
    runMiroProcAPI(appId, procEnv, MIRO_APP_PATH, ADD_DATA_TIMEOUT, cleanupFn = cleanup)

    if (updateApp) {
      moveFilesFromTemp(appId)
      modelConfig$update(modelConfig$getAppIndex(appId), list(
        containerEnv = appConfig$containerEnv,
        displayName = appConfig$displayName,
        description = appConfig$description,
        accessGroups = appConfig$accessGroups,
        extraData = list(appVersion = appVersion, appAuthors = appAuthors)
      ), allowUpdateRestrictedEnv = TRUE)
    }
  },
  error = function(e) {
    cleanup()
    write(sprintf(
      "merr:::500:::Problems adding MIRO app data. Error message: %s",
      conditionMessage(e)
    ), stderr())
    quit("no", 1L, FALSE)
  }
)

if (!updateApp) {
  tryCatch(
    {
      engineClient$registerModel(appId, modelId, modelName, appDir, appUserGroups)
      modelConfig$add(newAppConfig)
      quit("no", 0L, FALSE)
    },
    error = function(e) {
      cleanup()
      write(sprintf(
        "merr:::500:::Problems registering MIRO app at Engine. Error message: %s",
        conditionMessage(e)
      ), stderr())
      quit("no", 1L, FALSE)
    }
  )
}
