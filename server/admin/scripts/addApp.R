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

miroAppValidator <- MiroAppValidator$new()

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
    modelConfig <- ModelConfig$new(file.path(MIRO_DATA_DIR, "specs.yaml"))
  },
  error = function(e) {
    write(sprintf(
      "merr:::500:::Problems initializing required components. Error message: %s",
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
    appDir <- file.path(getwd(), MIRO_MODEL_DIR, appId)
    appDirTmp <- file.path(getwd(), MIRO_MODEL_DIR, paste0("~$", appId))
    appDirTmp2 <- file.path(getwd(), MIRO_MODEL_DIR, paste0("~$~$", appId))

    dataDir <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_", appId))
    dataDirTmp <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_~$", appId))
    dataDirTmp2 <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_~$~$", appId))

    if (updateApp) {
      for (dirPath in c(appDirTmp, appDirTmp2, dataDirTmp, dataDirTmp2)) {
        if (!identical(unlink(dirPath, recursive = TRUE), 0L)) {
          stop(sprintf("Could not remove directory: %s. Please try again or contact your system administrator.", dirPath),
            call. = FALSE
          )
        }
      }
      appConfig <- modelConfig$getAppConfigFull(appId)
      appDbCredentials <- modelConfig$getAppDbConf(appId)

      appConfig$containerEnv[["MIRO_VERSION_STRING"]] <- miroAppValidator$getMIROVersion()
      appConfig$containerEnv[["MIRO_MODEL_PATH"]] <- paste0("/home/miro/app/model/", appId, "/", modelName)
      for (key in c("displayName", "description", "accessGroups")) {
        if (length(metadata[[key]])) {
          valueTrimmed <- trimws(metadata[[key]])
          valueTrimmed <- valueTrimmed[nchar(valueTrimmed) > 0L]
          if (length(valueTrimmed)) {
            appConfig[[key]] <- valueTrimmed
          }
        } else if (key == "accessGroups") {
          appConfig[[key]] <- c()
        } else {
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
        containerVolumes = c(
          sprintf("/%s:/home/miro/app/model/%s:ro", appId, appId),
          sprintf("/data_%s:%s", appId, MIRO_CONTAINER_DATA_DIR)
        ),
        containerEnv = list(
          MIRO_MODEL_PATH = paste0("/home/miro/app/model/", appId, "/", modelName),
          MIRO_DATA_DIR = MIRO_CONTAINER_DATA_DIR,
          MIRO_VERSION_STRING = miroAppValidator$getMIROVersion(),
          MIRO_MODE = "base",
          MIRO_ENGINE_MODELNAME = appId,
          MIRO_DB_USERNAME = appDbCredentials$user,
          MIRO_DB_PASSWORD = appDbCredentials$password,
          MIRO_DB_SCHEMA = appDbCredentials$user
        )
      )
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
      addAppLogo(appId, modelId, logoPath)
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
      if (!file.rename(appDir, appDirTmp2)) {
        stop(sprintf(
          "Could not rename directory: %s to: %s. Please try again or contact your system administrator.",
          appDir, appDirTmp2
        ), call. = FALSE)
      }
      if (!file.rename(appDirTmp, appDir)) {
        stop(sprintf(
          "Could not rename directory: %s to: %s. Please try again or contact your system administrator.",
          appDirTmp, appDir
        ), call. = FALSE)
      }
      if (file.exists(dataDir) && !file.rename(dataDir, dataDirTmp2)) {
        stop(sprintf(
          "Could not rename directory: %s to: %s. Please try again or contact your system administrator.",
          dataDir, dataDirTmp2
        ), call. = FALSE)
      }
      if (file.exists(dataDirTmp) && !file.rename(dataDirTmp, dataDir)) {
        stop(sprintf(
          "Could not rename directory: %s to: %s. Please try again or contact your system administrator.",
          dataDirTmp, dataDir
        ), call. = FALSE)
      }
      modelConfig$update(modelConfig$getAppIndex(appId), list(
        containerEnv = appConfig$containerEnv,
        displayName = appConfig$displayName,
        description = appConfig$description,
        accessGroups = appConfig$accessGroups
      ))
      if (!identical(unlink(appDirTmp2, recursive = TRUE), 0L)) {
        flog.warn("Could not remove directory: %s", appDirTmp2)
      }
      if (!identical(unlink(dataDirTmp2, recursive = TRUE), 0L)) {
        flog.warn("Could not remove directory: %s", dataDirTmp2)
      }
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
