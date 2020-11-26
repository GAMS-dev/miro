MiroAppValidator <- R6::R6Class("MiroAppValidator", public = list(
  initialize = function(){
    return(invisible(self))
  },
  getModesSupported = function(){
    return(private$modesSupported)
  },
  getMIROVersion = function(){
    return(private$miroVersion)
  },
  getModelName = function(){
    return(private$modelName)
  },
  getAppId = function(){
    return(tolower(private$modelName))
  },
  getLogoB64 = function(){
    return(private$logoB64)
  },
  getLogoFile = function(){
    return(private$logoFile)
  },
  getAppTitle = function(){
    if (length(private$appTitle)){
        return(private$appTitle)
    }
    return(private$modelName)
  },
  getAppDesc = function(){
    return(private$appDesc)
  },
  setLogoFile = function(logoPath){
    if(!length(logoPath) || !file.exists(logoPath)){
        stop("App logo does not exist.", call. = FALSE)
    }
    private$logoFile <- paste0("app_logo.", tools::file_ext(logoPath))
    private$logoB64  <- getLogoB64(logoPath)
    return(invisible(self))
  },
  validate = function(miroAppFile){
    private$modesSupported <- NULL
    private$modelName <- NULL
    private$appTitle <- NULL
    private$appDesc <- NULL
    private$apiVersion <- NULL
    private$miroVersion <- NULL
    private$logoB64 <- NULL
    private$logoFile <- NULL
    if(!is.character(miroAppFile) || length(miroAppFile) != 1 || 
        !endsWith(miroAppFile, ".miroapp")){
        flog.info("Invalid miroapp file uploaded.")
        stop("Not a valid MIRO app file.", call. = FALSE)
    }
    filesInBundle  <- tryCatch(zip::zip_list(miroAppFile)[["filename"]], error = function(e){
        flog.info("Invalid miroapp file uploaded. Files is not a valid zip archive. Error message: %s", conditionMessage(e))
        stop("Not a valid MIRO app file.", call. = FALSE)
    })
    if(any(grepl("..", filesInBundle, fixed = TRUE))){
        flog.warn("Invalid miroapp file uploaded. Archive includes files with .. in their filename: %s",
            paste(filesInBundle[grepl("..", filesInBundle, fixed = TRUE)], collapse = ", "))
        stop("Not a valid MIRO app file.", call. = FALSE)
    }
    miroConfFormat <- "(.*)_(\\d)_(\\d+)_(\\d+\\.\\d+\\.\\d+)(_hcube)?\\.miroconf$"
    miroconfFiles  <- stringi::stri_match_all_regex(filesInBundle, miroConfFormat,
        omit_no_match = TRUE)
    miroconfFiles  <- miroconfFiles[vapply(miroconfFiles, function(el){
        !is.na(el[1])
        }, logical(1L), USE.NAMES = FALSE)]
    
    if(!length(miroconfFiles)){
        stop("No valid miroconf file found in bundle.", call. = FALSE)
    }

    for(miroconfFile in miroconfFiles){
        if(!length(private$modelName)){
            if(!identical(miroconfFile[3], "1")){
                stop("Invalid MIRO app uploaded. Model not deployed for multi user environment!", call. = FALSE)
            }
            if(!identical(miroconfFile[4], as.character(REQUIRED_API_VERSION))){
                stop(sprintf("Invalid MIRO app uploaded. API version: %s of uploaded app does not match the version required by MIRO server: %s!",
                    miroconfFile[4], REQUIRED_API_VERSION), call. = FALSE)
            }
            appMiroVersion <- MiroVersion$new(miroconfFile[5])
            if(appMiroVersion$laterThan(MIRO_VERSION)){
                stop(sprintf("The MIRO app you uploaded was deployed with a MIRO version (%s) more recent than the one installed (%s). Please update MIRO Server and try again.",
                    miroconfFile[5], MIRO_VERSION), call. = FALSE)
            }
            private$modelName <- miroconfFile[2]
            private$apiVersion <- miroconfFile[4]
            private$miroVersion <- miroconfFile[5]
        }
        if(is.na(miroconfFile[6])){
            private$modesSupported <- c(private$modesSupported, "base")
        }else{
            private$modesSupported <- c(private$modesSupported, "hcube")
        }
    }
    if(!length(private$modesSupported)){
        stop("Invalid MIRO app uploaded: no miroconf file could be found.", call. = FALSE)
    }
    if(length(private$modesSupported) > 2){
        stop("Invalid MIRO app uploaded: too many miroconf files were found.", call. = FALSE)
    }
    if(!paste0(self$getAppId(), ".zip") %in% filesInBundle){
        stop(sprintf("Invalid MIRO app uploaded: %s.zip not in bundle."),
            self$getAppId(), call. = FALSE)
    }
    private$logoB64 <- private$readLogo(miroAppFile, filesInBundle)
    appInfo <- private$readAppInfo(miroAppFile, filesInBundle)
    private$appTitle <- appInfo$title
    private$appDesc <- paste(appInfo$description, collapse = "\n")
    return(invisible(self))
  }), private = list(
    modesSupported = NULL,
    modelName = NULL,
    appTitle = NULL,
    appDesc = NULL,
    apiVersion = NULL,
    miroVersion = NULL,
    logoB64 = NULL,
    logoFile = NULL,
    getStaticFilePath = function(){
        return(file.path(paste0("static_", self$getAppId())))
    },
    readLogo = function(miroAppFile, filesInBundle){
        logoCandidates <- startsWith(filesInBundle, file.path(private$getStaticFilePath(), 
            paste0(self$getAppId(), "_logo."))) | 
            startsWith(filesInBundle, file.path(private$getStaticFilePath(), "app_logo."))
        if(sum(logoCandidates) == 0){
            return(DEFAULT_LOGO_B64)
        }
        if(sum(logoCandidates) > 1){
            flog.info("Multiple app logos were found. The first one will be used.")
        }
        private$logoFile <- filesInBundle[logoCandidates][1]
        logoPath <- unzip(miroAppFile, files = private$logoFile, 
            junkpaths = TRUE, exdir = tempdir(check = TRUE))

        return(tryCatch(getLogoB64(logoPath), error = function(e){
            flog.info("Problems reading app logo. Default logo will be used. Error: %s", conditionMessage(e))
            return(DEFAULT_LOGO_B64)
        }))
    },
    readAppInfo = function(miroAppFile, filesInBundle){
        appInfoFile <- tolower(filesInBundle) == file.path(private$getStaticFilePath(), "app_info.json")
        if(sum(appInfoFile) == 0){
            return(list())
        }
        appInfoPath <- unzip(miroAppFile, files = filesInBundle[appInfoFile][1], 
            junkpaths = TRUE, exdir = tempdir(check = TRUE))

        return(tryCatch(jsonlite::fromJSON(appInfoPath), error = function(e){
            flog.info("Invalid App Info file in bundle. Error message: %s", conditionMessage(e))
            return(list())
        }))
    }
  )
)