MiroAppValidator <- R6::R6Class("MiroAppValidator", public = list(
  initialize = function() {
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
      stop("No valid miroconf file found in bundle.", call. = FALSE)
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

    appIdTmp <- self$validateAppId(appInfo$appId)
    if (is.null(appIdTmp)) {
      private$appId <- self$getModelId()
    } else {
      private$appId <- appIdTmp
    }
    if (nchar(private$appId) > 60L) {
      stop("The App ID must not be longer than 60 characters!", call. = FALSE)
    }
    if (startsWith(private$appId, "~$")) {
      stop("The App ID must not start with the characters: '~$'!", call. = FALSE)
    }
    return(invisible(self))
  },
  validateAppId = function(appIdRaw) {
    if (is.null(appIdRaw)) {
      return(NULL)
    }
    if (!identical(length(appIdRaw), 1L) || !is.character(appIdRaw)) {
      stop("Invalid app id in app_info.json", call. = FALSE)
    }
    if (!identical(gsub("[/\\\\\\?%*:|\"<>]", "", appIdRaw), appIdRaw)) {
      stop("Invalid app id in app_info.json", call. = FALSE)
    }
    if (!identical(appIdRaw, tolower(appIdRaw))) {
      stop("App ID must not contain capital letters", call. = FALSE)
    }
    if (!identical(trimws(appIdRaw), appIdRaw)) {
      stop("App ID must not contain leading or trailing spaces", call. = FALSE)
    }
    if (identical(appIdRaw, "admin")) {
      stop("You cannot add an app with this ID", call. = FALSE)
    }
    return(appIdRaw)
  }
), private = list(
  appId = NULL,
  modelName = NULL,
  appTitle = NULL,
  appDesc = NULL,
  apiVersion = NULL,
  miroVersion = NULL,
  logoB64 = NULL,
  logoFile = NULL,
  getStaticFilePath = function() {
    return(file.path(paste0("static_", self$getModelId())))
  },
  readLogo = function(miroAppFile, filesInBundleRaw) {
    filesInBundle <- filesInBundleRaw[["filename"]]
    logoCandidates <- startsWith(filesInBundle, file.path(
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

    return(tryCatch(jsonlite::fromJSON(appInfoPath), error = function(e) {
      flog.info("Invalid App Info file in bundle. Error message: %s", conditionMessage(e))
      return(list())
    }))
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
  }
))
