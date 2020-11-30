ModelConfig <- R6::R6Class("ModelConfig", public = list(
  initialize = function(configPath){
    private$configPath <- configPath

    private$accessGroups <- Set$new(csv2Vector(Sys.getenv("SHINYPROXY_USERGROUPS")))

    if(file.exists(configPath)){
        configTmp <- tryCatch(yaml::read_yaml(configPath),
            error = function(e){
                stop(sprintf("Faulty yaml syntax in configuration file: %s. Error message: %s",
                    configPath, conditionMessage(e)), call. = FALSE)
        })
        adminConfigTmp <- configTmp[["specs"]][[1]]
        if(!identical(adminConfigTmp[["id"]], "admin")){
            private$adminConfig <- list()
            private$currentModelConfigs <- configTmp[["specs"]]
        }else{
            private$adminConfig <- adminConfigTmp
            private$adminConfig$accessGroups <- as.list(private$adminConfig$accessGroups)
            if(length(configTmp[["specs"]]) > 1L){
                private$currentModelConfigs <- configTmp[["specs"]][seq(2, length(configTmp[["specs"]]))]
            }else{
                private$currentModelConfigs <- list()
            }
        }
    }else{
        stop(sprintf("Could not find configuration file: %s", configPath), call. = FALSE)
    }
    return(invisible(self))
  },
  getAccessGroupUnion = function(){
        return(I(private$accessGroups$get()))
  },
  getModelIds = function(modelIndex){
    if(length(private$currentModelConfigs) < modelIndex){
        stop(sprintf("Model with index: %s does not exist", modelIndex), call. = FALSE)
    }
    return(list(private$currentModelConfigs[[modelIndex]][["id"]], 
        private$currentModelConfigs[[model_index]][["containerEnv"]][["MIRO_MODEL_PATH"]]))
  },
  getConfigList = function(){
    return(lapply(seq_along(private$currentModelConfigs), self$getAppConfig))
  },
  getAppId = function(appIndex){
    return(private$currentModelConfigs[[appIndex]]$id)
  },
  getModelName = function(appIndex){
    return(tools::file_path_sans_ext(basename(private$currentModelConfigs[[appIndex]][["containerEnv"]][["MIRO_MODEL_PATH"]])))
  },
  getAppLogo = function(appIndex){
    return(private$currentModelConfigs[[appIndex]]$logoURL)
  },
  add = function(newConfig){
    private$currentModelConfigs <- c(private$currentModelConfigs, list(newConfig))

    if(length(newConfig[["accessGroups"]])){
        private$accessGroups$join(newConfig[["accessGroups"]])
    }

    private$writeConfig()
    return(invisible(self))
  },
  remove = function(appIndex){
    if(appIndex > length(private$currentModelConfigs)){
        stop(sprintf("An app with index: %s does not exists.", as.character(appIndex)),
            call. = FALSE)
    }
    private$currentModelConfigs[[appIndex]] <- NULL
    private$writeConfig()

    return(invisible(self))
  },
  update = function(appIndex, newConfig){
    if(length(appIndex) != 1 || is.na(appIndex) ||
        appIndex > length(private$currentModelConfigs)){
        stop("Invalid app index.", call. = FALSE)
    }
    if(!is.null(newConfig[["displayName"]])){
        if(nchar(trimws(newConfig[["displayName"]])) < 1){
            stop("App title must contain at least 1 non-whitespace character.",
                call. = FALSE)
        }
        private$currentModelConfigs[[appIndex]][["displayName"]] <- newConfig[["displayName"]]
    }
    for(configId in c("description", "logoURL")){
        if(!is.null(newConfig[[configId]])){
            private$currentModelConfigs[[appIndex]][[configId]] <- newConfig[[configId]]
        }
    }
    
    if(length(newConfig[["accessGroups"]]) > 0){
        private$accessGroups$join(newConfig[["accessGroups"]])
        private$currentModelConfigs[[appIndex]][["accessGroups"]] <- as.list(newConfig[["accessGroups"]])
    }else{
        private$currentModelConfigs[[appIndex]][["accessGroups"]] <- NULL
    }

    private$writeConfig()

    return(invisible(self))
  },
  swapApps = function(appIdFrom, appIdTo){
    appIds <- vapply(private$currentModelConfigs, "[[", character(1), "id",
        USE.NAMES = FALSE)
    appIndices <- match(c(appIdFrom, appIdTo), appIds)
    if(any(is.na(appIndices))){
        stop(sprintf("One of the app IDs provided (%s, %s) was not found.",
            appIdFrom, appIdTo), call. = FALSE)
    }
    appConfigTo <- private$currentModelConfigs[[appIndices[2]]]
    private$currentModelConfigs[[appIndices[2]]] <- private$currentModelConfigs[[appIndices[1]]]
    private$currentModelConfigs[[appIndices[1]]] <- appConfigTo
    private$writeConfig()

    return(invisible(self))
  },
  getAppConfig = function(index){
    appConfig <- private$currentModelConfigs[[index]]
    if("logoURL" %in% names(appConfig)){
        logoB64 <- getLogoB64(file.path("data", 
          "logos", appConfig[["logoURL"]]))
    }else{
        logoB64 <- DEFAULT_LOGO_B64
    }

    accessGroups <- character()

    if("accessGroups" %in% names(appConfig) && length(appConfig[["accessGroups"]])){
        accessGroups <- appConfig[["accessGroups"]]
        private$accessGroups$join(accessGroups)
    }

    return(list(id = appConfig[["id"]], alias = appConfig[["displayName"]], 
        desc = appConfig[["description"]], logob64 = logoB64,
        groups = I(accessGroups)))
  }),
  private = list(
    configPath = NULL,
    accessGroups = NULL,
    adminConfig = NULL,
    currentModelConfigs = NULL,
    writeConfig = function(){
      yaml::write_yaml(list(specs = c(list(private$adminConfig), private$currentModelConfigs)), 
        private$configPath)
    }
  ) 
)
