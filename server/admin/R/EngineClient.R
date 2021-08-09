EngineClient <- R6::R6Class("EngineClient", public = list(
  initialize = function(){
    ret <- httr::GET(paste0(ENGINE_URL, "/version"))
    private$apiInfo <- content(ret, type = "application/json", 
                                 encoding = "utf-8")
    private$apiInfo$apiVersionInt <- suppressWarnings(
      as.integer(gsub(".", "", private$apiInfo$version, fixed = TRUE)))[1]
    return(invisible(self))
  },
  setAuthHeader = function(authToken){
    private$authHeader <- paste0("Bearer ", authToken)
    return(invisible(self))
  },
  loginUser = function(username, password){
    if(tryCatch({
        ret <- httr::POST(paste0(ENGINE_URL, "/auth/login"),
                    body = list(username = username, password = password,
                      expires_in = 3600))
        if(status_code(ret) != 200){
          flog.info("Invalid return code: %s when logging in user: %s.", status_code(ret), username)
          return(FALSE)
        }
        authToken <- content(ret, type = "application/json", 
                          encoding = "utf-8")[["token"]]
        self$setAuthHeader(authToken)
        ret <- httr::GET(paste0(ENGINE_URL, "/namespaces/", URLencode(ENGINE_NAMESPACE), "/permissions?username=", URLencode(username)),
                      add_headers(Authorization = private$getAuthHeader(), 
                                  Timestamp = as.character(Sys.time(), 
                                    usetz = TRUE)))
        if(status_code(ret) != 200){
          flog.info("Invalid return code: %s when trying to get namespace permissions on namespace: %s.", status_code(ret), ENGINE_NAMESPACE)
          return(FALSE)
        }
        namespacePermission <- as.integer(content(ret, type = "application/json", 
                                        encoding = "utf-8")[["permission"]])
        if(!identical(namespacePermission, 7L)){
          flog.info("User: %s does not have full permissions on namespace: %s", username, ENGINE_NAMESPACE)
          return(FALSE)
        }
        TRUE
      }, error = function(e){
        flog.info("Problems logging in user: %s. Error message: %s", username, conditionMessage(e))
        private$authHeader <- NULL
        return(FALSE)
      })){
      return(TRUE)
    }
    return(FALSE)
  },
  getModelList = function(){
    ret <- httr::GET(paste0(ENGINE_URL, "/namespaces/", ENGINE_NAMESPACE),
                  add_headers(Authorization = private$getAuthHeader(), 
                              Timestamp = as.character(Sys.time(), 
                                usetz = TRUE),
                              `X-Fields` = "name"))
    if(status_code(ret) != 200){
        stop(sprintf("Unexpected return code: %s from GAMS Engine when trying to fetch list of models. Error: %s",
            status_code(ret), private$getErrorMessage(ret)), call. = FALSE)
    }
    return(vapply(content(ret, type = "application/json", 
              encoding = "utf-8"), function(modelObj){
        return(modelObj[["name"]])
      }, character(1L), USE.NAMES = FALSE))
  },
  registerModel = function(appId, modelId, mainGMSName, modelPath, userGroups, overwrite = FALSE){
    flog.trace("Registering app: %s at Engine", appId)
    if(overwrite && (appId %in% private$appIdsNotOnMIRO)){
        # model already exists, so first deregister it
        self$deregisterModel(appId)
    }
    modelDataPath <- file.path(MIRO_MODEL_DIR, appId, paste0(modelId, ".zip"))
    requestData <- list(data = upload_file(modelDataPath, 
                          type = 'application/zip'),
                        run = mainGMSName)
    if(length(userGroups) &&
      (is.na(private$apiInfo$apiVersionInt) || private$apiInfo$apiVersionInt > 210809L)){
      requestData <- c(requestData, private$getGroupLabelsEngine(userGroups))
    }
    ret <- httr::POST(paste0(ENGINE_URL, "/namespaces/", URLencode(ENGINE_NAMESPACE), "/models/",
                          URLencode(appId)),
                  encode = "multipart",
                  body = requestData,
                  add_headers(Authorization = private$getAuthHeader(), 
                              Timestamp = as.character(Sys.time(), 
                                usetz = TRUE)),
                  timeout(600))
    if(status_code(ret) != 201){
        stop(sprintf("Unexpected return code: %s from GAMS Engine when trying to register model. Error: %s",
            status_code(ret), private$getErrorMessage(ret)), call. = FALSE)
    }
    flog.trace("App: %s successfully registered at Engine", appId)
    return(invisible(self))
  },
  updateModel = function(appId, userGroups = NULL){
    if(appId %in% private$appIdsNotOnEngine){
      flog.info("Can't update app: %s as it does not exist on Engine.", appId)
      return(invisible(self))
    }
    if(!is.na(private$apiInfo$apiVersionInt) && private$apiInfo$apiVersionInt <= 210809L){
      flog.info("Can't update app: %s as your Engine version does not support assigning user groups to models.",
        appId)
      return(invisible(self))
    }
    flog.trace("Updating app: %s at Engine", appId)
    if(length(userGroups)){
      requestData <- private$getGroupLabelsEngine(userGroups)
    }else{
      requestData <- list(delete_user_groups = TRUE)
    }
    ret <- httr::PATCH(paste0(ENGINE_URL, "/namespaces/", URLencode(ENGINE_NAMESPACE), "/models/",
                            URLencode(appId)),
                  encode = "multipart",
                  body = requestData,
                  add_headers(Authorization = private$getAuthHeader(), 
                                    Timestamp = as.character(Sys.time(), 
                                      usetz = TRUE)),
                  timeout(4))
    if(status_code(ret) != 200){
        stop(sprintf("Unexpected return code: %s from GAMS Engine when trying to update model. Error: %s",
            status_code(ret), private$getErrorMessage(ret)), call. = FALSE)
    }
    flog.trace("App: %s successfully updated at Engine", appId)
    return(invisible(self))
  },
  deregisterModel = function(appId){
    if(appId %in% private$appIdsNotOnEngine){
      flog.info("Can't deregister app: %s as it does not exist on Engine.", appId)
      return(invisible(self))
    }
    flog.trace("Deregistering app: %s at Engine", appId)
    ret <- httr::DELETE(paste0(ENGINE_URL, "/namespaces/", URLencode(ENGINE_NAMESPACE), "/models/", URLencode(appId)), 
            add_headers(Authorization = private$getAuthHeader(), 
                              Timestamp = as.character(Sys.time(), 
                                usetz = TRUE)),
            timeout(4))

    if(status_code(ret) != 200){
        stop(sprintf("Unexpected return code: %s from GAMS Engine when trying to deregister model. Error: %s",
            status_code(ret), private$getErrorMessage(ret)), call. = FALSE)
    }
    flog.trace("App: %s successfully deregistered from Engine", appId)
    return(invisible(self))
  },
  setAppsNotOnEngine = function(appIdsNotOnEngine){
    private$appIdsNotOnEngine <- appIdsNotOnEngine
    return(invisible(self))
  },
  getAppsNotOnEngine = function(){
    return(private$appIdsNotOnEngine)
  },
  setAppsNotOnMIRO = function(appIdsNotOnMIRO){
    private$appIdsNotOnMIRO <- appIdsNotOnMIRO
    return(invisible(self))
  },
  getAppsNotOnMIRO = function(){
    return(private$appIdsNotOnMIRO)
  }), private = list(
  apiInfo = NULL,
  labelsEngine = NULL,
  authHeader = NULL,
  appIdsNotOnEngine = character(0L),
  appIdsNotOnMIRO = character(0L),
  getAuthHeader = function(){
    return(private$authHeader)
  },
  getErrorMessage = function(ret){
    return(tryCatch(content(ret, type = "application/json", encoding = "utf-8")[["message"]], 
        error = function(e){
        return(conditionMessage(e))
    }))
  },
  fetchGroupLabels = function(){
    ret <- httr::GET(paste0(ENGINE_URL, "/namespaces/", URLencode(ENGINE_NAMESPACE), "/user-groups"),
      add_headers(Authorization = private$getAuthHeader(), 
                                  Timestamp = as.character(Sys.time(), 
                                    usetz = TRUE)))
    if(status_code(ret) != 200){
        stop(sprintf("Unexpected return code: %s from GAMS Engine when trying to fetch user groups. Error: %s",
            status_code(ret), private$getErrorMessage(ret)), call. = FALSE)
    }
    groupLabelsEngine <- content(ret, type = "application/json", 
                                 encoding = "utf-8")
    private$labelsEngine <- vapply(groupLabelsEngine, "[[", character(1L),
      "label", USE.NAMES = FALSE)
    return(invisible(self))
  },
  getGroupLabelsEngine = function(groupLabels){
    if(is.null(private$labelsEngine)){
      private$fetchGroupLabels()
    }
    labelIdx <- match(tolower(groupLabels), tolower(private$labelsEngine))
    if(any(is.na(labelIdx))){
      stop(sprintf("Invalid group(s): '%s'. This error occurs when you have been removed from these groups.",
            paste(groupLabels[is.na(labelIdx)], collapse = ",")), call. = FALSE)
    }
    groupLabelsTmp <- private$labelsEngine[labelIdx]
    return(setNames(as.list(groupLabelsTmp), rep.int("user_groups", length(groupLabelsTmp))))
  })
)
