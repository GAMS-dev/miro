options(shiny.maxRequestSize = as.integer(Sys.getenv("MIRO_MAX_UPLOAD_SIZE", "5000")) * 1024^2)

source("../app/tools/db_migration/modules/form_db_migration.R", local = TRUE)

miroscenParser <- MiroscenParser$new()
modelConfig <- ModelConfig$new(SPECS_YAML_PATH)
engineClient <- EngineClient$new()
miroAppValidator <- MiroAppValidator$new(engineClient)
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
    modelConfig$setAppsNotOnEngine(engineClient$getAppsNotOnEngine())
    errors$configList <- modelConfig$getConfigList()
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

  session$sendCustomMessage("onInitUserGroups", engineClient$getUserGroups())

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
    engineClient$setAuthHeader(paste0("Bearer ", ENGINE_TOKEN))
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
            appId = miroAppValidator$getAppId(),
            appTitle = htmltools::htmlEscape(miroAppValidator$getAppTitle()),
            appDesc = htmltools::htmlEscape(miroAppValidator$getAppDesc()),
            logoB64 = miroAppValidator$getLogoB64(),
            appEnv = miroAppValidator$getAppEnv(),
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
    appId <- NULL
    tryCatch(
      {
        if (!length(miroAppValidator$getAppTitle())) {
          flog.error("Add App request sent without the miroapp file being validated. This should never happen and is likely an attempt to tamper with the app.")
          stop("Internal error. Check log for more information.", call. = FALSE)
        }
        currentConfigList <- modelConfig$getConfigList()
        newAppTitle <- trimws(input$addApp$title)
        newAppDesc <- trimws(input$addApp$desc)
        newAppEnv <- miroAppValidator$validateAppEnv(input$addApp$env)
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

        createAppDir(appId)

        modelId <- miroAppValidator$getModelId()

        logoPath <- miroAppValidator$getLogoFile()
        logoURL <- "default_logo.png"
        if (length(logoPath)) {
          logoURL <- getLogoName(appId, logoPath)
        } else {
          logoPath <- NULL
        }

        extractAppData(
          input$miroAppFile$datapath, appId, modelId, miroProc
        )
        addAppLogo(appId, logoPath, logoURL)
        faviconPath <- addAppFavicon(appId, modelId)
        modelName <- miroAppValidator$getModelName()
        newAppConfig <- list(
          id = appId, displayName = newAppTitle, description = newAppDesc,
          logoURL = logoURL,
          faviconPath = faviconPath,
          containerEnv = list(
            MIRO_VERSION_STRING = miroAppValidator$getMIROVersion(),
            MIRO_MODE = "base",
            MIRO_ENGINE_MODELNAME = appId
          ),
          extraData = list(
            appVersion = miroAppValidator$getAppVersion(),
            appAuthors = miroAppValidator$getAppAuthors()
          )
        )
        if (IN_KUBERNETES) {
          newAppConfig$containerEnv$MIRO_MODEL_PATH <- paste0("/home/miro/model/", modelName)
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

        if (length(newGroups)) {
          newAppConfig[["accessGroups"]] <- as.list(newGroups)
        }

        appDbCredentials <- db$createAppSchema(appId)
        newAppConfig[["containerEnv"]][["MIRO_DB_USERNAME"]] <- appDbCredentials$user
        newAppConfig[["containerEnv"]][["MIRO_DB_PASSWORD"]] <- appDbCredentials$password
        newAppConfig[["containerEnv"]][["MIRO_DB_SCHEMA"]] <- appDbCredentials$user

        for (envName in names(newAppEnv)) {
          if (envName %in% names(newAppConfig[["containerEnv"]])) {
            flog.warn(
              "Environment variable name: %s is restricted and cannot be used. It will be ignored",
              envName
            )
            next
          }
          newAppConfig[["containerEnv"]][[envName]] <- newAppEnv[[envName]]
        }

        appDir <- getModelPath(appId)
        dataDir <- getDataPath(appId)


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
                tryCatch(
                  {
                    removeAppData(appId, logoURL, faviconPath)
                    flog.info("Files for MIRO app: %s removed successfully (cleaning up after error during app registration).", appId)
                  },
                  error = function(ei) {
                    flog.warn(
                      "Error while trying to clean up (due to error during app registration) already registered app files of MIRO app: %s. Error message: %s",
                      appId, conditionMessage(ei)
                    )
                  }
                )
                session$sendCustomMessage("onError", list(requestType = "addApp", message = errMsg))
              }
            )
          }
        )
      },
      error = function(e) {
        if (!is.null(appId)) {
          unlink(if (IN_KUBERNETES) dirname(getModelPath(appId)) else getModelPath(appId),
            recursive = TRUE, force = TRUE
          )
        }
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

        removeAppData(
          appId, modelConfig$getAppLogo(appIndex),
          modelConfig$getAppFavicon(appIndex)
        )

        modelConfig$remove(appIndex)

        engineClient$deregisterModel(appId)

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
        modelId <- tolower(modelConfig$getModelName(appIndex))

        newAppEnv <- miroAppValidator$validateAppEnv(input$updateAppMeta$env)
        newGroups <- csv2Vector(input$updateAppMeta$groups)
        newModelConfig <- list(
          displayName = input$updateAppMeta$title,
          containerEnv = newAppEnv,
          description = input$updateAppMeta$desc,
          accessGroups = newGroups
        )
        if (isTRUE(input$updateAppMeta$newLogo)) {
          logoPath <- input$updateMiroAppLogo$datapath
          if (!length(logoPath)) {
            stop("Logo file not found.", call. = FALSE)
          }
          removeAppLogo(
            appId, modelConfig$getAppLogo(appIndex),
            modelConfig$getAppFavicon(appIndex)
          )
          newLogoName <- getLogoName(appId, logoPath)
          addAppLogo(appId, logoPath, newLogoName)
          newModelConfig[["logoURL"]] <- newLogoName
        }
        engineClient$updateModel(appId, userGroups = newGroups)
        modelConfig$update(appIndex, newModelConfig)

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
                           additionalData = NULL, appConfig = NULL, customCallback = NULL, appDir = NULL, launchDbMigrationManager = NULL) {
    tryCatch(
      {
        if (is.null(appConfig)) {
          appConfig <- modelConfig$getAppConfigFull(appId)
        }
        appModelName <- tools::file_path_sans_ext(basename(
          modelConfig$getEnvValue(appConfig$containerEnv[["MIRO_MODEL_PATH"]])
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
        if (is.null(appDir)) {
          appDir <- getModelPath(appId)
        }
        for (i in seq_along(filePaths)) {
          miroProc$run(appId, appModelName,
            modelConfig$getEnvValue(appConfig$containerEnv[["MIRO_VERSION_STRING"]]),
            appDir, filePaths[[i]],
            progressSelector = progressSelector,
            requestType = requestType, overwriteScen = overwriteExisting,
            additionalDataOnError = additionalData,
            launchDbMigrationManager = launchDbMigrationManager,
            parallelSessionId = "addData", newSession = identical(i, 1L),
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
    spinnerSelector <- ""
    tryCatch(
      {
        appId <- input$updateAppRequest$id
        overwriteData <- identical(input$updateAppRequest$overwrite, TRUE)
        flog.info(
          "Request to update app: '%s' received (Overwrite: '%s').",
          appId, overwriteData
        )

        progressSelector <- input$updateAppRequest$progressSelector
        spinnerSelector <- input$updateAppRequest$spinnerSelector
        filePath <- input[[input$updateAppRequest$fileInputId]]$datapath

        appConfig <- modelConfig$getAppConfigFull(appId)

        newAppId <- miroAppValidator$
          validate(filePath)$
          getAppId()

        modelId <- miroAppValidator$getModelId()
        modelName <- miroAppValidator$getModelName()
        appVersion <- miroAppValidator$getAppVersion()
        appAuthors <- miroAppValidator$getAppAuthors()
        appConfig$containerEnv[["MIRO_VERSION_STRING"]] <- miroAppValidator$getMIROVersion()
        if (IN_KUBERNETES) {
          appConfig$containerEnv[["MIRO_MODEL_PATH"]] <- paste0("/home/miro/model/", modelName)
        } else {
          appConfig$containerEnv[["MIRO_MODEL_PATH"]] <- paste0(
            "/home/miro/app/model/",
            appId, "/", modelName
          )
        }

        if (!identical(appId, newAppId)) {
          stop(sprintf(
            "The app ID of the new app (%s) does not match that of the app you dropped it on (%s).",
            newAppId, appId
          ), call. = FALSE)
        }

        appDirTmp <- getModelPath(paste0("~$", appId))
        dataDirTmp <- getDataPath(paste0("~$", appId))

        removeTempDirs(appId)

        extractAppData(filePath, paste0("~$", appId), modelId, miroProc)

        engineClient$updateModel(appId,
          userGroups = FALSE,
          modelDataPath = file.path(appDirTmp, paste0(modelId, ".zip"))
        )

        addDataFiles(appId, dataDirTmp, progressSelector,
          "updateApp",
          appDir = appDirTmp,
          overwriteExisting = overwriteData,
          launchDbMigrationManager = launchDbMigrationManager,
          additionalData = list(progressSelector = progressSelector, spinnerSelector = spinnerSelector),
          appConfig = appConfig, customCallback = function() {
            tryCatch(
              {
                moveFilesFromTemp(appId)
                modelConfig$update(modelConfig$getAppIndex(appId), list(
                  containerEnv = appConfig$containerEnv,
                  faviconPath = addAppFavicon(appId, modelId),
                  extraData = list(appVersion = appVersion, appAuthors = appAuthors)
                ), allowUpdateRestrictedEnv = TRUE)
                session$sendCustomMessage(
                  "onSuccess",
                  list(
                    requestType = "updateApp", progressSelector = progressSelector,
                    spinnerSelector = spinnerSelector
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
                  progressSelector = progressSelector, spinnerSelector = spinnerSelector
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
          progressSelector = progressSelector, spinnerSelector = spinnerSelector
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
    spinnerSelector <- ""
    tryCatch(
      {
        appId <- input$updateAppDataRequest$id
        overwriteData <- identical(input$updateAppDataRequest$overwrite, TRUE)
        flog.info(
          "Request to update app data of app: '%s' received (Overwrite: '%s').",
          appId, overwriteData
        )
        progressSelector <- input$updateAppDataRequest$progressSelector
        spinnerSelector <- input$updateAppDataRequest$spinnerSelector
        filePaths <- input[[input$updateAppDataRequest$fileInputId]]$datapath
        fileNames <- input[[input$updateAppDataRequest$fileInputId]]$name
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
          additionalData = list(progressSelector = if (length(filePaths) > 1) progressSelector, spinnerSelector = spinnerSelector)
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
          progressSelector = progressSelector, spinnerSelector = spinnerSelector
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
    migrationConfig <- dbMigrationServer(paste0("migrationForm", launchDbMigrationManager()),
      migrationInfo$inconsistentTablesInfo,
      migrationInfo$orphanedTablesInfo,
      standalone = FALSE
    )
    migrationObs <- observe({
      if (loginRequired(session, isLoggedIn)) {
        return()
      }
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
          progressSelector = migrationInfo$progressSelector,
          additionalDataOnError = migrationInfo$additionalDataOnError,
          overwriteScen = migrationInfo$overwriteScen, requestType = "migrateDb", function() {
            flog.debug(
              "Database for app: %s migrated successfully.",
              migrationInfo$appId
            )
            removeModal()
            session$sendCustomMessage(
              "onSuccess",
              list(requestType = "migrateDb")
            )
            if (length(migrationInfo$successCallback)) {
              migrationInfo$successCallback()
            }
          }
        )
      }
    })
  })
  observeEvent(input$btCloseMigForm, {
    if (loginRequired(session, isLoggedIn)) {
      return()
    }
    flog.debug(
      "Button to close migration form clicked."
    )
    try(
      {
        migrationObs$destroy()
        migrationObs <<- NULL
      },
      silent = TRUE
    )
    migrationInfo <- miroProc$getMigrationInfo()
    session$sendCustomMessage("onError", list(
      requestType = "updateApp",
      progressSelector = migrationInfo$additionalDataOnError$progressSelector,
      spinnerSelector = migrationInfo$additionalDataOnError$spinnerSelector
    ))
    removeModal()
  })
}
