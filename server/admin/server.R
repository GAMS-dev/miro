miroAppValidator <- MiroAppValidator$new()
modelConfig      <- ModelConfig$new(file.path("data", "specs.yaml"))
engineClient     <- EngineClient$new()
db               <- MiroDb$new(list(host = Sys.getenv("MIRO_DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("MIRO_DB_PORT", "5432")),
    name = Sys.getenv("MIRO_DB_NAME"),
    username = Sys.getenv("MIRO_DB_USERNAME"),
    password = Sys.getenv("MIRO_DB_PASSWORD")))
DEFAULT_LOGO_B64 <<- getLogoB64(file.path("www", "default_logo.png"))

server <- function(input, output, session){
    isLoggedIn <- FALSE
    miroProc   <- MiroProc$new(session)

    session$sendCustomMessage("onInit", list(loginRequired = LOGIN_REQUIRED,
        configList = modelConfig$getConfigList(), 
        groupList = modelConfig$getAccessGroupUnion()))

    observeEvent(input$loginRequest, {
        if(identical(input$loginRequest$user, USERNAME) && 
            identical(input$loginRequest$password, PASSWORD)){
            flog.info("User: %s successfully logged in.", USERNAME)
            isLoggedIn <<- TRUE
            session$sendCustomMessage("onLoginSuccessful", 1)
            return()
        }
        flog.info("Wrong log in attempt.")
        session$sendCustomMessage("onError", list(requestType = "loginRequest", 
            message = "Wrong username or password."))
    })
    observeEvent(input$miroAppFile, {
        if(loginRequired(session, isLoggedIn)){
            return()
        }
        tryCatch({
            miroAppValidator$validate(input$miroAppFile$datapath)
            session$sendCustomMessage("onNewAppValidated", 
                list(appTitle = htmltools::htmlEscape(miroAppValidator$getAppTitle()),
                    appDesc = htmltools::htmlEscape(miroAppValidator$getAppDesc()),
                    logoB64 = miroAppValidator$getLogoB64()));
        }, error = function(e){
            errMsg <- sprintf("Invalid app bundle uploaded. Error message: %s", 
                    conditionMessage(e))
            flog.info(errMsg)
            session$sendCustomMessage("onError", list(requestType = "addApp", message = errMsg))
        })
    })
    observeEvent(input$miroAppLogo, {
        if(loginRequired(session, isLoggedIn)){
            return()
        }
        tryCatch({
            miroAppValidator$setLogoFile(input$miroAppLogo$datapath)
            session$sendCustomMessage("onAddAppLogo", 
                list(logoB64 = miroAppValidator$getLogoB64()));
        }, error = function(e){
            errMsg <- sprintf("Invalid app logo. Error message: %s", 
                    conditionMessage(e))
            flog.info(errMsg)
            session$sendCustomMessage("onError", list(requestType = "updateLogo", message = errMsg))
        })
    })
    observeEvent(input$updateMiroAppLogo, {
        if(loginRequired(session, isLoggedIn)){
            return()
        }
        tryCatch({
            session$sendCustomMessage("onAddAppLogo", 
                list(logoB64 = getLogoB64(input$updateMiroAppLogo$datapath)));
        }, error = function(e){
            errMsg <- sprintf("Invalid app logo. Error message: %s", 
                    conditionMessage(e))
            flog.info(errMsg)
            session$sendCustomMessage("onError", list(requestType = "updateLogo", message = errMsg))
        })
    })
    observeEvent(input$addApp, {
        if(loginRequired(session, isLoggedIn)){
            return()
        }
        tryCatch({
            if(!length(miroAppValidator$getAppTitle())){
                flog.error("Add App request sent without the miroapp file being validated. This should never happen and is likely an attempt to tamper with the app.")
                stop("Internal error.", call. = FALSE)
            }
            currentConfigList <- modelConfig$getConfigList()
            newAppTitle <- trimws(input$addApp$title)
            newAppDesc  <- trimws(input$addApp$desc)
            newGroups   <- csv2Vector(input$addApp$groups)
            if(!length(newAppTitle) || nchar(newAppTitle) == 0){
                flog.error("Add app request with empty app title received. This should never happen and is likely an attempt to tamper with the app.")
                stop("App title must not be empty!", call. = FALSE)
            }

            appId <- miroAppValidator$getAppId()

            if(appId %in% vapply(currentConfigList, "[[", character(1L), "id", USE.NAMES = FALSE)){
                stop("A MIRO App with the same id already exists.", call. = FALSE)
            }

            supportedModes <- miroAppValidator$getModesSupported()
            logoPath <- miroAppValidator$getLogoFile()
            logoURL  <- "default_logo.png"
            if(length(logoPath)){
                logoURL <- basename(logoPath)
            }else{
                logoPath <- NULL
            }
            modelName <- miroAppValidator$getModelName()
            newAppConfig <- list(id = appId, displayName = newAppTitle, description = newAppDesc,
                logoURL = logoURL,
                containerVolumes = c(sprintf("/%s:/home/miro/app/model/%s:ro", appId, appId), 
                    sprintf("/data_%s:%s", appId, MIRO_CONTAINER_DATA_DIR)),
                containerEnv = list(
                    MIRO_MODEL_PATH = paste0("/home/miro/app/model/", appId, "/", modelName, ".gms"), 
                    MIRO_DATA_DIR = MIRO_CONTAINER_DATA_DIR, 
                    MIRO_VERSION_STRING = miroAppValidator$getMIROVersion(),
                    MIRO_MODE = supportedModes[1]))

            if(length(newGroups)){
                newAppConfig[["accessGroups"]] <- as.list(newGroups)
            }

            if(isTRUE(input$addApp$removeInconsistentTables)){
                tablesToRemove <- miroProc$getTablesToRemove()
                if(!length(tablesToRemove)){
                    flog.error("Request to remove inconsistent tables received, even though there are no inconsistent tables. This looks like an attempt to tamper with the app.")
                    stop("Internal error", call. = FALSE)
                }
                db$removeTables(tablesToRemove)
            }

            appDir   <- file.path(getwd(), MIRO_MODEL_DIR, appId)
            dataDir  <- file.path(getwd(), MIRO_DATA_DIR, paste0("data_", appId))

            extractAppData(isolate(input$miroAppFile$datapath), appId,
                        logoPath)

            miroProc$run(appId, modelName, miroAppValidator$getMIROVersion(), appDir, dataDir, function(){
                tryCatch({
                    engineClient$registerModel(appId, modelName, appDir, overwrite = TRUE)
                    flog.debug("New MIRO app: %s registered at Engine.", modelName)

                    modelConfig$add(newAppConfig)
                    flog.debug("New MIRO app: %s added.", appId)

                    session$sendCustomMessage("onSuccess", 
                        list(requestType = "addApp", 
                            configList = modelConfig$getConfigList(), 
                            groupList = modelConfig$getAccessGroupUnion()))
                }, error = function(e){
                    errMsg <- sprintf("Invalid MIRO app. Error message: %s", 
                            conditionMessage(e))
                    flog.info(errMsg)
                    session$sendCustomMessage("onError", list(requestType = "addApp", message = errMsg))
                })
            })
        }, error = function(e){
            errMsg <- sprintf("Invalid MIRO app. Error message: %s", 
                    conditionMessage(e))
            flog.info(errMsg)
            session$sendCustomMessage("onError", list(requestType = "addApp", message = errMsg))
        })
    })
    observeEvent(input$deleteApp, {
        if(loginRequired(session, isLoggedIn)){
            return()
        }
        tryCatch({
            appIndex <- suppressWarnings(as.integer(input$deleteApp$index))
            if(length(appIndex) != 1 || is.na(appIndex)){
                stop("Bad request to delete app. This is likely an attempt to tamper with the app!",
                    call. = FALSE)
            }

            appId <- modelConfig$getAppId(appIndex)

            if(isTRUE(input$deleteApp$removeData)){
                db$removeAppDbTables(appId)
            }

            removeAppData(appIndex, modelConfig$getAppLogo(appIndex))

            engineClient$deregisterModel(modelConfig$getModelName(appIndex))

            modelConfig$remove(appIndex)

            flog.info("MIRO app: %s removed successfully.", appId)
            session$sendCustomMessage("onSuccess", 
                    list(requestType = "deleteApp", 
                        configList = modelConfig$getConfigList(), 
                        groupList = modelConfig$getAccessGroupUnion()))
        }, error = function(e){
            errMsg <- sprintf("Problems deleting app. Error message: %s", 
                    conditionMessage(e))
            flog.info(errMsg)
            session$sendCustomMessage("onError", list(requestType = "deleteApp", message = errMsg))
        })
    })
    observeEvent(input$updateApp, {
        if(loginRequired(session, isLoggedIn)){
            return()
        }
        tryCatch({
            appIndex <- suppressWarnings(as.integer(input$updateApp$index))
            if(is.na(appIndex)){
                stop(sprintf("Invalid app index: %s", appIndex), call. = FALSE)
            }
            appId    <- modelConfig$getAppId(appIndex)

            newLogoName <- NULL
            if(isTRUE(input$updateApp$newLogo)){
                logoPath <- input$updateMiroAppLogo$datapath
                if(!length(logoPath)){
                    stop("Logo file not found.", call. = FALSE)
                }
                newLogoName <- paste0("app_logo.", tools::file_ext(logoPath))
                addAppLogo(appId, logoPath,
                    newLogoName = newLogoName)
            }
            
            modelConfig$update(appIndex, list(displayName = input$updateApp$title,
                logoURL = newLogoName,
                description = input$updateApp$desc,
                accessGroups = csv2Vector(input$updateApp$groups)))

            flog.info("MIRO app: %s updated successfully.", appId)
            session$sendCustomMessage("onSuccess", 
                    list(requestType = "updateApp", 
                        configList = modelConfig$getConfigList(), 
                        groupList = modelConfig$getAccessGroupUnion()))
        }, error = function(e){
            errMsg <- sprintf("Problems updating app. Error message: %s", 
                    conditionMessage(e))
            flog.info(errMsg)
            session$sendCustomMessage("onError", list(requestType = "updateApp", message = errMsg))
        })
    })
    observeEvent(input$updateAppOrder, {
        if(loginRequired(session, isLoggedIn)){
            return()
        }
        tryCatch({
            idFrom <- input$updateAppOrder$idFrom
            idTo   <- input$updateAppOrder$idTo

            modelConfig$swapApps(idFrom, idTo)

            flog.info("Apps: %s and %s swapped positions.", idFrom, idTo)
            session$sendCustomMessage("onSuccess", 
                    list(requestType = "updateOrder", 
                        idFrom = idFrom,
                        idTo = idFrom,
                        idFromRaw = input$updateAppOrder$idFromRaw, 
                        idToRaw = input$updateAppOrder$idToRaw))
        }, error = function(e){
            errMsg <- sprintf("Problems reordering apps. Error message: %s", 
                    conditionMessage(e))
            flog.info(errMsg)
            session$sendCustomMessage("onError", list(requestType = "updateOrder", message = errMsg))
        })
    })
}
