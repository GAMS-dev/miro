options(shiny.maxRequestSize = 500 * 1024^2)

source("../app/tools/db_migration/modules/form_db_migration.R", local = TRUE)

miroAppValidator <- MiroAppValidator$new()
miroscenParser <- MiroscenParser$new()
modelConfig <- ModelConfig$new(file.path("data", "specs.yaml"))
engineClient <- EngineClient$new()
db <- MiroDb$new(list(
  host = Sys.getenv("MIRO_DB_HOST", "localhost"),
  port = as.integer(Sys.getenv("MIRO_DB_PORT", "5432")),
  name = Sys.getenv("MIRO_DB_NAME"),
  username = Sys.getenv("MIRO_DB_USERNAME"),
  password = Sys.getenv("MIRO_DB_PASSWORD")
))
DEFAULT_LOGO_B64 <<- getLogoB64(file.path("www", "default_logo.png"))

initCallback <- function(session, appIds) {
  modelListEngine <- engineClient$getModelList()

  errors <- list()

  appIds <- appIds[appIds != "admin"]

  appIdsNotOnEngine <- !appIds %in% modelListEngine

  if (any(appIdsNotOnEngine)) {
    engineClient$setAppsNotOnEngine(appIds[appIdsNotOnEngine])
    errors$appsNotOnEngine <- I(engineClient$getAppsNotOnEngine())
    flog.info(
      "Some apps are not registered on Engine: '%s'. They will be marked CORRUPTED!",
      paste(errors$appsNotOnEngine, collapse = "', '")
    )
  }

  appIdsNotOnMIROServer <- !modelListEngine %in% appIds

  if (any(appIdsNotOnMIROServer)) {
    engineClient$setAppsNotOnMIRO(modelListEngine[appIdsNotOnMIROServer])
    errors$appsNotOnMIRO <- I(engineClient$getAppsNotOnMIRO())
    flog.info(
      "Some apps that are registered on Engine are not registered on MIRO Server: '%s'.",
      paste(errors$appsNotOnMIRO, collapse = "', '")
    )
  }

  if (length(errors)) {
    session$sendCustomMessage("onInitErrors", errors)
  }
}

server <- function(input, output, session) {
  isLoggedIn <- FALSE
  miroProc <- MiroProc$new(session)

  launchDbMigrationManager <- reactiveVal(0L)

  modelConfigList <- modelConfig$getConfigList()

  session$sendCustomMessage("onInit", list(
    loginRequired = LOGIN_REQUIRED,
    configList = modelConfigList,
    groupList = modelConfig$getAccessGroupUnion()
  ))

  if (LOGIN_REQUIRED) {
    observeEvent(input$loginRequest, {
      if (engineClient$loginUser(
        input$loginRequest$user,
        input$loginRequest$password
      )) {
        flog.info("User: %s successfully logged in.", input$loginRequest$user)
        isLoggedIn <<- TRUE
        session$sendCustomMessage("onLoginSuccessful", 1)
        initCallback(session, modelConfig$getAllAppIds())
        return()
      }
      flog.info("Wrong log in attempt.")
      session$sendCustomMessage("onError", list(
        requestType = "loginRequest",
        message = "Wrong username or password."
      ))
    })
  } else {
    engineClient$setAuthHeader(ENGINE_TOKEN)
    initCallback(session, modelConfig$getAllAppIds())
  }

  observeEvent(input$miroAppFile, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.info("Request to validate MIRO app received.")
    tryCatch(
      {
        miroAppValidator$validate(input$miroAppFile$datapath)

        session$sendCustomMessage(
          "onNewAppValidated",
          list(
            appTitle = htmltools::htmlEscape(miroAppValidator$getAppTitle()),
            appDesc = htmltools::htmlEscape(miroAppValidator$getAppDesc()),
            logoB64 = miroAppValidator$getLogoB64(),
            dataExists = db$schemaExists(miroAppValidator$getAppId())
          )
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Invalid app bundle uploaded. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(requestType = "addApp", message = errMsg))
      }
    )
  })
  observeEvent(input$miroAppLogo, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.info("Request to add app logo received.")
    tryCatch(
      {
        miroAppValidator$setLogoFile(input$miroAppLogo$datapath)
        session$sendCustomMessage(
          "onAddAppLogo",
          list(logoB64 = miroAppValidator$getLogoB64())
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Invalid app logo. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(requestType = "updateLogoAddApp", message = errMsg))
      }
    )
  })
  observeEvent(input$updateMiroAppLogo, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.info("Request to update app logo received.")
    tryCatch(
      {
        session$sendCustomMessage(
          "onAddAppLogo",
          list(logoB64 = getLogoB64(input$updateMiroAppLogo$datapath))
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Invalid app logo. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(requestType = "updateLogo", message = errMsg))
      }
    )
  })
  observeEvent(input$addApp, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.info("Request to add new MIRO app received.")
    tryCatch(
      {
        if (!length(miroAppValidator$getAppTitle())) {
          flog.error("Add App request sent without the miroapp file being validated. This should never happen and is likely an attempt to tamper with the app.")
          stop("Internal error. Check log for more information.", call. = FALSE)
        }
        currentConfigList <- modelConfig$getConfigList()
        newAppTitle <- trimws(input$addApp$title)
        newAppDesc <- trimws(input$addApp$desc)
        newAppEnv <- trimws(input$addApp$env)
        newGroups <- csv2Vector(input$addApp$groups)
        overwriteData <- identical(input$addApp$overwrite, TRUE)
        flog.info("Overwrite data: %s", overwriteData)
        if (!length(newAppTitle) || nchar(newAppTitle) == 0) {
          flog.error("Add app request with empty app title received. This should never happen and is likely an attempt to tamper with the app.")
          stop("App title must not be empty!", call. = FALSE)
        }

        appId <- miroAppValidator$getAppId()

        if (appId %in% modelConfig$getAllAppIds(includeNoAccess = TRUE)) {
          stop("A MIRO app with the same name already exists.", call. = FALSE)
        }

        logoPath <- miroAppValidator$getLogoFile()
        logoURL <- "default_logo.png"
        if (length(logoPath)) {
          logoURL <- getLogoName(appId, logoPath)
        } else {
          logoPath <- NULL
        }
        modelName <- miroAppValidator$getModelName()
        newAppConfig <- list(
          id = appId, displayName = newAppTitle, description = newAppDesc,
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
            MIRO_ENGINE_MODELNAME = appId
          )
        )

        if (length(newGroups)) {
          newAppConfig[["accessGroups"]] <- as.list(newGroups)
        }

        appDbCredentials <- db$createAppSchema(appId)
        newAppConfig[["containerEnv"]][["MIRO_DB_USERNAME"]] <- appDbCredentials$user
        newAppConfig[["containerEnv"]][["MIRO_DB_PASSWORD"]] <- appDbCredentials$password
        newAppConfig[["containerEnv"]][["MIRO_DB_SCHEMA"]] <- appDbCredentials$user

        if (identical(length(newAppEnv), 1L) && !identical(newAppEnv, "")) {
          if (!jsonlite::validate(newAppEnv)) {
            stop("Argument 'txt' is not a valid JSON string.")
          }
          newAppEnv <- jsonlite::fromJSON(newAppEnv)
          for (envName in names(newAppEnv)) {
            if (envName %in% names(newAppConfig[["containerEnv"]])) {
              flog.warn("Invalid environment variable name: %s in custom environment file. It was ignored.", envName)
            } else if (length(newAppEnv[[envName]]) != 1L) {
              flog.error("Invalid environment variable value for variable: %s. Probably an attempt to tamper with the app!", envName)
            } else {
              newAppConfig[["containerEnv"]][[envName]] <- as.character(newAppEnv[[envName]])
            }
          }
        }

        appDir <- file.path(getwd(), MIRO_MODEL_DIR, appId)
        dataDir <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_", appId))

        modelId <- miroAppValidator$getModelId()

        extractAppData(
          input$miroAppFile$datapath, appId, modelId
        )
        addAppLogo(appId, modelId, logoPath)

        miroProc$
          setDbCredentials(
          appDbCredentials$user,
          appDbCredentials$password
        )$
          run(appId, modelName, miroAppValidator$getMIROVersion(),
          appDir, dataDir,
          progressSelector = "#addAppProgress",
          overwriteScen = overwriteData, requestType = "addApp",
          launchDbMigrationManager = launchDbMigrationManager, function() {
            flog.trace("Data for app: %s added successfully", appId)
            tryCatch(
              {
                engineClient$registerModel(appId, modelId, modelName, appDir, newGroups, overwrite = TRUE)
                flog.debug("New MIRO app: %s registered at Engine.", appId)

                modelConfig$add(newAppConfig)
                flog.debug("New MIRO app: %s added.", appId)

                session$sendCustomMessage(
                  "onSuccess",
                  list(
                    requestType = "addApp",
                    configList = modelConfig$getConfigList(),
                    groupList = modelConfig$getAccessGroupUnion()
                  )
                )
              },
              error = function(e) {
                errMsg <- sprintf(
                  "Invalid MIRO app. Error message: %s",
                  conditionMessage(e)
                )
                flog.info(errMsg)
                session$sendCustomMessage("onError", list(requestType = "addApp", message = errMsg))
              }
            )
          }
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Invalid MIRO app. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(requestType = "addApp", message = errMsg))
      }
    )
  })
  observeEvent(input$deleteApp, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.info("Request to delete MIRO app received.")
    tryCatch(
      {
        appIndex <- suppressWarnings(as.integer(input$deleteApp$index))
        if (length(appIndex) != 1 || is.na(appIndex)) {
          stop("Bad request to delete app. This is likely an attempt to tamper with the app!",
            call. = FALSE
          )
        }

        appId <- modelConfig$getAppId(appIndex)

        if (isTRUE(input$deleteApp$removeData)) {
          db$removeAppSchema(appId)
          flog.info("Data for MIRO app: %s removed successfully.", appId)
        }

        removeAppData(appId, modelConfig$getAppLogo(appIndex))

        engineClient$deregisterModel(appId)

        modelConfig$remove(appIndex)

        flog.info("MIRO app: %s removed successfully.", appId)
        session$sendCustomMessage(
          "onSuccess",
          list(
            requestType = "deleteApp",
            configList = modelConfig$getConfigList(),
            groupList = modelConfig$getAccessGroupUnion()
          )
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Problems deleting app. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(requestType = "deleteApp", message = errMsg))
      }
    )
  })
  observeEvent(input$updateAppMeta, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.info("Request to update MIRO app received.")
    tryCatch(
      {
        appIndex <- suppressWarnings(as.integer(input$updateAppMeta$index))
        if (is.na(appIndex)) {
          stop(sprintf("Invalid app index: %s", appIndex), call. = FALSE)
        }
        appId <- modelConfig$getAppId(appIndex)

        newLogoName <- NULL
        if (isTRUE(input$updateAppMeta$newLogo)) {
          logoPath <- input$updateMiroAppLogo$datapath
          if (!length(logoPath)) {
            stop("Logo file not found.", call. = FALSE)
          }
          newLogoName <- getLogoName(appId, logoPath)
          addAppLogo(appId, logoPath)
        }

        newAppEnv <- NULL
        if (identical(length(input$updateAppMeta$env), 1L) && !identical(trimws(input$updateAppMeta$env), "")) {
          if (!jsonlite::validate(input$updateAppMeta$env)) {
            stop("Argument 'txt' is not a valid JSON string.")
          }
          newAppEnv <- jsonlite::fromJSON(input$updateAppMeta$env)
          for (envName in names(newAppEnv)) {
            if (length(newAppEnv[[envName]]) != 1L) {
              stop("Invalid environment variable value for variable: %s.", envName)
            } else {
              newAppEnv[[envName]] <- as.character(newAppEnv[[envName]])
            }
          }
        }
        newGroups <- csv2Vector(input$updateAppMeta$groups)
        engineClient$updateModel(appId, userGroups = newGroups)
        modelConfig$update(appIndex, list(
          displayName = input$updateAppMeta$title,
          logoURL = newLogoName,
          containerEnv = newAppEnv,
          description = input$updateAppMeta$desc,
          accessGroups = newGroups
        ))

        flog.info("MIRO app: %s updated successfully.", appId)
        session$sendCustomMessage(
          "onSuccess",
          list(
            requestType = "updateAppMeta",
            configList = modelConfig$getConfigList(),
            groupList = modelConfig$getAccessGroupUnion()
          )
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Problems updating app. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(requestType = "updateAppMeta", message = errMsg))
      }
    )
  })
  observeEvent(input$updateAppOrder, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.info("Request to update MIRO app order received.")
    tryCatch(
      {
        idFrom <- input$updateAppOrder$idFrom
        idTo <- input$updateAppOrder$idTo

        modelConfig$swapApps(idFrom, idTo)

        flog.info("Apps: %s and %s swapped positions.", idFrom, idTo)
        session$sendCustomMessage(
          "onSuccess",
          list(
            requestType = "updateOrder",
            configList = modelConfig$getConfigList()
          )
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Problems reordering apps. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(requestType = "updateOrder", message = errMsg))
      }
    )
  })
  addDataFiles <- function(appId, filePaths, progressSelector, requestType, overwriteExisting = FALSE,
                           additionalData = NULL, appConfig = NULL, customCallback = NULL) {
    tryCatch(
      {
        if (is.null(appConfig)) {
          appConfig <- modelConfig$getAppConfigFull(appId)
        }
        appModelName <- tools::file_path_sans_ext(basename(
          appConfig$containerEnv[["MIRO_MODEL_PATH"]]
        ))
        appDbCredentials <- modelConfig$getAppDbConf(appId)
        miroProc$
          setDbCredentials(
          appDbCredentials$user,
          appDbCredentials$password
        )
        progress <- 0
        if (length(additionalData)) {
          dataToSend <- additionalData
          dataToSend[["requestType"]] <- requestType
        } else {
          dataToSend <- list(requestType = requestType)
        }
        for (i in seq_along(filePaths)) {
          miroProc$run(appId, appModelName,
            appConfig$containerEnv[["MIRO_VERSION_STRING"]],
            file.path(getwd(), MIRO_MODEL_DIR, appId), filePaths[[i]],
            progressSelector = if (length(filePaths) > 1) progressSelector else NULL,
            requestType = requestType, overwriteScen = overwriteExisting,
            additionalDataOnError = additionalData,
            successCallback = if (length(customCallback)) {
              customCallback
            } else {
              function() {
                if (identical(i, length(filePaths))) {
                  flog.info("Scenario data added for app: %s.", appId)
                  session$sendCustomMessage(
                    "onSuccess",
                    dataToSend
                  )
                } else if (length(filePaths) > 1) {
                  progress <<- progress + i / length(filePaths)
                  private$session$sendCustomMessage(
                    "onProgress",
                    list(
                      selector = progressSelector,
                      progress = progress
                    )
                  )
                }
              }
            }
          )
        }
      },
      error = function(e) {
        errMsg <- sprintf(
          "Invalid data files uploaded. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        dataToSend$message <- errMsg
        session$sendCustomMessage("onError", dataToSend)
      }
    )
  }
  observeEvent(input$updateAppRequest, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    appId <- ""
    progressSelector <- ""
    tryCatch(
      {
        appId <- input$updateAppRequest$id
        overwriteData <- identical(input$updateAppRequest$overwrite, TRUE)
        flog.info(
          "Request to update app: '%s' received (Overwrite: '%s').",
          appId, overwriteData
        )

        progressSelector <- paste0("#appProgress_", appId)
        filePath <- input[[paste0("appFiles_", appId)]]$datapath

        appConfig <- modelConfig$getAppConfigFull(appId)

        newAppId <- miroAppValidator$
          validate(filePath)$
          getAppId()

        modelId <- miroAppValidator$getModelId()
        appConfig$containerEnv[["MIRO_VERSION_STRING"]] <- miroAppValidator$getMIROVersion()

        if (!identical(appId, newAppId)) {
          stop(sprintf(
            "The app ID of the new app (%s) does not match that of the app you dropped it on (%s).",
            newAppId, appId
          ), call. = FALSE)
        }

        appDir <- file.path(getwd(), MIRO_MODEL_DIR, appId)
        appDirTmp <- file.path(getwd(), MIRO_MODEL_DIR, paste0("~$", appId))
        appDirTmp2 <- file.path(getwd(), MIRO_MODEL_DIR, paste0("~$~$", appId))

        dataDir <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_", appId))
        dataDirTmp <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_~$", appId))
        dataDirTmp2 <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_~$~$", appId))

        for (dirPath in c(appDirTmp, appDirTmp2, dataDirTmp, dataDirTmp2)) {
          if (!identical(unlink(dirPath, recursive = TRUE), 0L)) {
            stop(sprintf("Could not remove directory: %s. Please try again or contact your system administrator.", dirPath),
              call. = FALSE
            )
          }
        }

        extractAppData(filePath, paste0("~$", appId), modelId)

        engineClient$updateModel(appId, userGroups = FALSE, modelDataPath = file.path(appDir, paste0(modelId, ".zip")))

        addDataFiles(appId, dataDirTmp, progressSelector,
          "updateApp",
          overwriteExisting = overwriteData,
          additionalData = list(progressSelector = progressSelector, spinnerSelector = paste0("#appSpinner_", appId)),
          appConfig = appConfig, customCallback = function() {
            tryCatch(
              {
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
                  containerEnv = appConfig$containerEnv
                ))
                if (!identical(unlink(appDirTmp2, recursive = TRUE), 0L)) {
                  flog.warn("Could not remove directory: %s", appDirTmp2)
                }
                if (!identical(unlink(dataDirTmp2, recursive = TRUE), 0L)) {
                  flog.warn("Could not remove directory: %s", dataDirTmp2)
                }
                session$sendCustomMessage(
                  "onSuccess",
                  list(
                    requestType = "updateApp", progressSelector = progressSelector,
                    spinnerSelector = paste0("#appSpinner_", appId)
                  )
                )
              },
              error = function(e) {
                errMsg <- sprintf(
                  "Problems updating app. Error message: %s",
                  conditionMessage(e)
                )
                flog.info(errMsg)
                session$sendCustomMessage("onError", list(
                  requestType = "updateApp", message = errMsg,
                  progressSelector = progressSelector, spinnerSelector = paste0("#appSpinner_", appId)
                ))
              }
            )
          }
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Problems updating app. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(
          requestType = "updateApp", message = errMsg,
          progressSelector = progressSelector, spinnerSelector = paste0("#appSpinner_", appId)
        ))
      }
    )
  })
  observeEvent(input$updateAppDataRequest, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    appId <- ""
    progressSelector <- ""
    tryCatch(
      {
        appId <- input$updateAppDataRequest$id
        overwriteData <- identical(input$updateAppDataRequest$overwrite, TRUE)
        flog.info(
          "Request to update app data of app: '%s' received (Overwrite: '%s').",
          appId, overwriteData
        )
        progressSelector <- paste0("#appProgress_", appId)
        filePaths <- input[[paste0("appFiles_", appId)]]$datapath
        fileNames <- input[[paste0("appFiles_", appId)]]$name
        isMIROAPPFile <- endsWith(tolower(fileNames), ".miroapp")
        if (!all(isMIROAPPFile)) {
          # we should rename data files since they get random name by Shiny
          # and MIRO determines scenario name by filename
          newPaths <- file.path(dirname(filePaths[!isMIROAPPFile]), fileNames[!isMIROAPPFile])
          renameStatus <- file.rename(filePaths[!isMIROAPPFile], newPaths)
          if (!all(renameStatus)) {
            stop(sprintf(
              "Could not rename directory: %s to: %s. Please try again or contact your system administrator.",
              filePaths[!isMIROAPPFile][!renameStatus], newPaths[!renameStatus]
            ), call. = FALSE)
          }
          filePaths[!isMIROAPPFile] <- newPaths
        }
        addDataFiles(appId, filePaths, progressSelector,
          "updateApp",
          overwriteExisting = overwriteData,
          additionalData = list(progressSelector = progressSelector, spinnerSelector = paste0("#appSpinner_", appId))
        )
      },
      error = function(e) {
        errMsg <- sprintf(
          "Problems updating app data. Error message: %s",
          conditionMessage(e)
        )
        flog.info(errMsg)
        session$sendCustomMessage("onError", list(
          requestType = "updateApp", message = errMsg,
          progressSelector = progressSelector, spinnerSelector = paste0("#appSpinner_", appId)
        ))
      }
    )
  })
  # database migration manager
  migrationObs <- NULL
  observe({
    if (launchDbMigrationManager() == 0L) {
      return()
    }
    migrationInfo <- miroProc$getMigrationInfo()
    migrationConfig <- dbMigrationServer("migrationForm",
      migrationInfo$inconsistentTablesInfo,
      migrationInfo$orphanedTablesInfo,
      standalone = FALSE
    )
    migrationObs <- observe({
      if (!is.null(migrationConfig())) {
        try(
          {
            migrationObs$destroy()
            migrationObs <<- NULL
          },
          silent = TRUE
        )

        session$sendCustomMessage(
          "onProgress",
          list(
            selector = "dbMigration",
            progress = 10
          )
        )

        migrationConfigPath <- file.path(tempdir(TRUE), "mig_conf.json")
        write_json(migrationConfig(), migrationConfigPath,
          auto_unbox = TRUE, null = "null"
        )

        session$sendCustomMessage("onProgress", list(progress = 20))

        miroProc$run(migrationInfo$appId, migrationInfo$modelName,
          migrationInfo$miroVersion,
          migrationInfo$appDir, migrationInfo$dataDir,
          migrationConfigPath = migrationConfigPath,
          progressSelector = "#addAppProgress",
          overwriteScen = TRUE, requestType = "migrateDb", function() {
            flog.debug(
              "Database for app: %s migrated successfully.",
              migrationInfo$appId
            )
            removeModal()
            session$sendCustomMessage(
              "onSuccess",
              list(requestType = "migrateDb")
            )
          }
        )
      }
    })
  })
  observeEvent(input$btCloseMigForm, {
    try(
      {
        migrationObs$destroy()
        migrationObs <<- NULL
      },
      silent = TRUE
    )
    removeModal()
  })
}
