source(file.path(MIRO_APP_PATH, "components", "json.R"), local = TRUE)

MiroAppValidator <- R6::R6Class("MiroAppValidator", public = list(
  initialize = function(engineClient) {
    private$engineClient <- engineClient
    private$jsonValidator <- JSONValidator$new(miroRootDir = MIRO_APP_PATH)
    return(invisible(self))
  },
  getMIROVersion = function() {
    return(private$miroVersion)
  },
  getModelName = function() {
    return(private$modelName)
  },
  getAppId = function() {
    return(private$appId)
  },
  getModelId = function() {
    return(tolower(tools::file_path_sans_ext(private$modelName)))
  },
  getLogoB64 = function() {
    return(private$logoB64)
  },
  getLogoFile = function() {
    return(private$logoFile)
  },
  getAppTitle = function() {
    if (length(private$appTitle)) {
      return(private$appTitle)
    }
    return(private$appId)
  },
  getAppDesc = function() {
    return(private$appDesc)
  },
  getAppEnv = function() {
    return(private$appEnv)
  },
  setLogoFile = function(logoPath) {
    if (!length(logoPath) || !file.exists(logoPath)) {
      stop("App logo does not exist.", call. = FALSE)
    }
    if (file.size(logoPath) > MAX_LOGO_SIZE) {
      stop(sprintf(
        "Logo exceeds maximum size of %s bytes.",
        as.character(MAX_LOGO_SIZE)
      ), call. = FALSE)
    }
    private$logoFile <- logoPath
    private$logoB64 <- getLogoB64(logoPath)
    return(invisible(self))
  },
  validate = function(miroAppFile) {
    private$modelName <- NULL
    private$appTitle <- NULL
    private$appDesc <- NULL
    private$apiVersion <- NULL
    private$miroVersion <- NULL
    private$logoB64 <- NULL
    private$logoFile <- NULL
    if (!is.character(miroAppFile) || length(miroAppFile) != 1 ||
      !endsWith(miroAppFile, ".miroapp")) {
      flog.info("Invalid miroapp file uploaded.")
      stop("Not a valid MIRO app file.", call. = FALSE)
    }
    filesInBundleRaw <- tryCatch(zip::zip_list(miroAppFile), error = function(e) {
      flog.info("Invalid miroapp file uploaded. Files is not a valid zip archive. Error message: %s", conditionMessage(e))
      stop("Not a valid MIRO app file.", call. = FALSE)
    })
    filesInBundle <- filesInBundleRaw[["filename"]]
    if (any(grepl("..", filesInBundle, fixed = TRUE))) {
      flog.warn(
        "Invalid miroapp file uploaded. Archive includes files with .. in their filename: %s",
        paste(filesInBundle[grepl("..", filesInBundle, fixed = TRUE)], collapse = ", ")
      )
      stop("Not a valid MIRO app file.", call. = FALSE)
    }
    appMetadata <- private$readAppMeta(miroAppFile, filesInBundle)

    if (!identical(appMetadata[["use_temp_dir"]], TRUE)) {
      stop("Invalid MIRO app uploaded. Model not deployed for multi-user environment!", call. = FALSE)
    }

    if (identical(tolower(as.character(appMetadata[["modes_included"]])), "hcube")) {
      stop("MIRO Server does not support the MIRO Hypercube Mode. Please deploy your app for the Base Mode!", call. = FALSE)
    }

    if (!identical(as.character(appMetadata[["api_version"]]), as.character(REQUIRED_API_VERSION))) {
      stop(sprintf(
        "Invalid MIRO app uploaded. API version: %s of uploaded app does not match the version required by MIRO server: %s!",
        as.character(appMetadata[["api_version"]]), REQUIRED_API_VERSION
      ), call. = FALSE)
    }

    appMiroVersion <- MiroVersion$new(as.character(appMetadata[["miro_version"]]))
    if (appMiroVersion$laterThan(MIRO_VERSION)) {
      stop(sprintf(
        "The MIRO app you uploaded was deployed with a MIRO version (%s) more recent than the one installed (%s). Please update MIRO Server and try again.",
        as.character(appMetadata[["miro_version"]]), MIRO_VERSION
      ), call. = FALSE)
    }

    private$modelName <- private$validateModelname(appMetadata[["main_gms_name"]])
    private$apiVersion <- as.character(appMetadata[["api_version"]])
    private$miroVersion <- as.character(appMetadata[["miro_version"]])

    miroConfFormat <- "(.*)_(\\d)_(\\d+)_(\\d+\\.\\d+\\.\\d+)(_hcube)?\\.miroconf$"
    miroconfFiles <- stringi::stri_match_all_regex(filesInBundle, miroConfFormat,
      omit_no_match = TRUE
    )
    miroconfFiles <- miroconfFiles[vapply(miroconfFiles, function(el) {
      !is.na(el[1])
    }, logical(1L), USE.NAMES = FALSE)]

    if (!length(miroconfFiles)) {
      miroconfFiles <- filesInBundle[filesInBundle == ".miroconf"]
      if (!length(miroconfFiles)) {
        stop("No valid miroconf file found in bundle.", call. = FALSE)
      }
    }
    if (length(miroconfFiles) > 2) {
      stop("Invalid MIRO app uploaded: too many miroconf files were found.", call. = FALSE)
    }
    if (!paste0(self$getModelId(), ".zip") %in% filesInBundle) {
      stop(sprintf("Invalid MIRO app uploaded: %s.zip not in bundle."),
        self$getModelId(),
        call. = FALSE
      )
    }
    private$logoB64 <- private$readLogo(miroAppFile, filesInBundleRaw)
    appInfo <- private$readAppInfo(miroAppFile, filesInBundle)
    private$appTitle <- appInfo$title
    private$appDesc <- paste(appInfo$description, collapse = "\n")

    private$appEnv <- self$validateAppEnv(appInfo$environment)
    appIdTmp <- appInfo$appId
    if (is.null(appIdTmp)) {
      appIdTmp <- self$getModelId()
    }
    self$validateAppId(appIdTmp)
    private$appId <- appIdTmp
    return(invisible(self))
  },
  validateAppEnv = function(appEnvRaw) {
    if (is.null(appEnvRaw)) {
      return(appEnvRaw)
    }
    if (length(appEnvRaw) > MAX_ENV_VARS) {
      stop(sprintf("Only up to %d environment variables allowed", MAX_ENV_VARS), call. = FALSE)
    }
    if (!all(grepl("^[A-Z_][A-Z0-9_]*$", names(appEnvRaw), perl = TRUE))) {
      stop("Invalid app environment. Variable name(s) do not follow the pattern: ^[A-Z_][A-Z0-9_]*$", call. = FALSE)
    }
    if (any(names(appEnvRaw) %in% RESTRICTED_ENV_KEYS)) {
      stop("Environment variable names must not use restricted names (starting with 'MIRO_').", call. = FALSE)
    }
    for (envName in names(appEnvRaw)) {
      envConfig <- appEnvRaw[[envName]]
      for (configKey in names(envConfig)) {
        if (configKey %in% c("description", "value")) {
          if (!(is.character(envConfig[[configKey]]) &&
            length(envConfig[[configKey]]) == 1L &&
            nchar(envConfig[[configKey]]) <= 1000L)) {
            stop(
              sprintf(
                "Invalid app environment. '%s' key (variable: '%s') must be character and no longer than 1000 characters.",
                configKey, envName
              ),
              call. = FALSE
            )
          }
          if (envName %in% paste0(
            "MIRO_DEFAULT_SCEN_PERM_",
            c("READ", "WRITE", "EXECUTE")
          )) {
            tryCatch(private$validateUserAccessGroups(envConfig[["value"]]),
              error = function(err) {
                stop(
                  sprintf(
                    "Invalid app environment. '%s' key is invalid: %s.",
                    envName, conditionMessage(err)
                  ),
                  call. = FALSE
                )
              }
            )
          }
        } else {
          stop("Invalid app environment. Configuration of variable(s) include invalid keys.", call. = FALSE)
        }
      }
    }
    return(appEnvRaw)
  },
  validateAppId = function(appIdRaw) {
    if (is.null(appIdRaw) || !is.character(appIdRaw) || length(appIdRaw) != 1L) {
      stop("Invalid app id in app_info.json", call. = FALSE)
    }
    if (!grepl("^[a-z0-9][a-z0-9-_]{0,59}$", appIdRaw, perl = TRUE)) {
      stop("The App ID may only contain ASCII lowercase letters, digits, '-' and '_', must not start with '-' or '_' and may not be longer than 60 characters!", call. = FALSE)
    }
    return(TRUE)
  }
), private = list(
  engineClient = NULL,
  appId = NULL,
  appEnv = NULL,
  modelName = NULL,
  appTitle = NULL,
  appDesc = NULL,
  apiVersion = NULL,
  miroVersion = NULL,
  logoB64 = NULL,
  logoFile = NULL,
  jsonValidator = NULL,
  getStaticFilePath = function() {
    return(file.path(paste0("static_", self$getModelId())))
  },
  readLogo = function(miroAppFile, filesInBundleRaw) {
    filesInBundle <- filesInBundleRaw[["filename"]]
    logoCandidates <- startsWith(filesInBundle, file.path(
      private$getStaticFilePath(),
      paste0(self$getAppId(), "_logo.")
    )) | startsWith(filesInBundle, file.path(
      private$getStaticFilePath(),
      paste0(self$getModelId(), "_logo.")
    )) |
      startsWith(filesInBundle, file.path(private$getStaticFilePath(), "app_logo."))
    if (sum(logoCandidates) == 0) {
      return(DEFAULT_LOGO_B64)
    }
    if (sum(logoCandidates) > 1) {
      flog.info("Multiple app logos were found. The first one will be used.")
    }
    if (filesInBundleRaw[["uncompressed_size"]][which(logoCandidates)[1]] > MAX_LOGO_SIZE) {
      flog.warn(
        "The logo exceeds the maximum logo size of %s bytes. The default logo will be used.",
        as.character(MAX_LOGO_SIZE)
      )
      return(DEFAULT_LOGO_B64)
    }
    private$logoFile <- filesInBundle[logoCandidates][1]
    logoPath <- unzip(miroAppFile,
      files = private$logoFile,
      junkpaths = TRUE, exdir = tempdir(check = TRUE)
    )

    return(tryCatch(getLogoB64(logoPath), error = function(e) {
      flog.info("Problems reading app logo. Default logo will be used. Error: %s", conditionMessage(e))
      private$logoFile <- NULL
      return(DEFAULT_LOGO_B64)
    }))
  },
  readAppMeta = function(miroAppFile, filesInBundle) {
    appMetaFile <- tolower(filesInBundle) == "miroapp.json"
    if (sum(appMetaFile) == 0) {
      stop("No app metadata found. Please make sure to deploy your app with MIRO 2.0 or later!", call. = FALSE)
    }
    appMetaPath <- unzip(miroAppFile,
      files = filesInBundle[appMetaFile][1],
      junkpaths = TRUE, exdir = tempdir(check = TRUE)
    )

    return(tryCatch(jsonlite::fromJSON(appMetaPath), error = function(e) {
      stop(sprintf("Invalid App Meta file in bundle. Error message: %s", conditionMessage(e)), call. = FALSE)
    }))
  },
  readAppInfo = function(miroAppFile, filesInBundle) {
    appInfoFile <- tolower(filesInBundle) == file.path(private$getStaticFilePath(), "app_info.json")
    if (sum(appInfoFile) == 0) {
      return(list())
    }
    appInfoPath <- unzip(miroAppFile,
      files = filesInBundle[appInfoFile][1],
      junkpaths = TRUE, exdir = tempdir(check = TRUE)
    )
    return(private$jsonValidator$validate(
      appInfoPath,
      file.path(MIRO_APP_PATH, "conf", "app_info_schema.json"),
      returnRawData = TRUE
    ))
  },
  validateModelname = function(modelNameRaw) {
    if (!identical(length(modelNameRaw), 1L) || !is.character(modelNameRaw) ||
      !identical(
        tools::file_path_sans_ext(modelNameRaw),
        gsub("[/\\\\\\?%*:|\"<>]", "", tools::file_path_sans_ext(modelNameRaw))
      )) {
      stop("Invalid model name found in MIRO metadata file!", call. = FALSE)
    }
    return(modelNameRaw)
  },
  validateUserAccessGroups = function(accessGroupString) {
    userGroups <- private$engineClient$getUserGroups()
    for (accessGroup in strsplit(accessGroupString, ",", fixed = TRUE)[[1L]]) {
      if (startsWith(accessGroup, "#")) {
        if (!substring(accessGroup, 2L) %in% userGroups$groups) {
          stop(sprintf("Invalid user group specified: '%s'.", substring(accessGroup, 2L)), call. = FALSE)
        }
        return()
      }
      if (!accessGroup %in% userGroups$users) {
        stop(sprintf("Invalid user specified: '%s'.", accessGroup), call. = FALSE)
      }
    }
  }
))
