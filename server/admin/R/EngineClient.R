EngineClient <- R6::R6Class("EngineClient", public = list(
  initialize = function(){
  },
  registerModel = function(appId, modelId, modelPath, overwrite = FALSE){
    modelDataPath <- file.path(MIRO_MODEL_DIR, appId, paste0(appId, ".zip"))

    ret <- httr::POST(paste0(ENGINE_URL, "/namespaces/", ENGINE_NAMESPACE, "/", modelId), 
                  encode = "multipart", 
                  body = list(data = upload_file(modelDataPath, 
                    type = 'application/zip')),
                  add_headers(Authorization = private$getAuthHeader(), 
                              Timestamp = as.character(Sys.time(), 
                                usetz = TRUE)),
                  timeout(10))
    if(overwrite && status_code(ret) == 400){
        # model already exists, so overwrite it
        self$deregisterModel(modelId)
        return(self$registerModel(appId, modelId, modelPath))
    }
    if(status_code(ret) != 201){
        stop(sprintf("Unexpected return code from executor: %s when trying to register model. Error: %s",
            status_code(ret), private$getErrorMessage(ret)), call. = FALSE)
    }
    return(invisible(self))
  },
  deregisterModel = function(modelId){
    ret <- httr::DELETE(paste0(ENGINE_URL, "/namespaces/", ENGINE_NAMESPACE, "/", modelId), 
            add_headers(Authorization = private$getAuthHeader(), 
                              Timestamp = as.character(Sys.time(), 
                                usetz = TRUE)),
            timeout(4))

    if(status_code(ret) != 200){
        stop(sprintf("Unexpected return code from executor: %s when trying to deregister model. Error: %s",
            status_code(ret), private$getErrorMessage(ret)), call. = FALSE)
    }

    return(invisible(self))
  }), private = list(
  getAuthHeader = function(){
    return(paste0("Basic ", jsonlite::base64_enc(charToRaw(
            paste0(ENGINE_ADMIN_USER, ":", ENGINE_ADMIN_PWD)))))
  },
  getErrorMessage = function(ret){
    return(tryCatch(content(ret, type = "application/json", encoding = "utf-8")[["message"]], 
        error = function(e){
        return(conditionMessage(e))
    }))
  })
)