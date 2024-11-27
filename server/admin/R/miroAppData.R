getLogoName <- function(appId, logoFile) {
  return(paste0(appId, "_logo.", tools::file_ext(logoFile)))
}

createAppDir <- function(appId) {
  modelPath <- file.path(MIRO_MODEL_DIR, appId)
  failedDirCreate <- !dir.create(modelPath)
  if (exists("last.warning") && endsWith(names(last.warning)[1], "already exists")) {
    flog.info(
      "App with id: %s is found on the file system but not in specs.yaml. This is either because another process is currently adding an app with this id or because it was not properly cleaned up. In the latter case, please remove the directory: '%s' manually.",
      appId, paste0("./models/", appId)
    )
    stop_custom("error_model_dir_exists", "An app with this id already exists", call. = FALSE)
  }
  if (failedDirCreate) {
    if (dir.exists(modelPath)) {
      flog.info(
        "App with id: %s is found on the file system but not in specs.yaml. This is either because another process is currently adding an app with this id or because it was not properly cleaned up. In the latter case, please remove the directory: '%s' manually.",
        appId, paste0("./models/", appId)
      )
      stop_custom("error_model_dir_exists", "An app with this id already exists", call. = FALSE)
    }
    stop(sprintf("Could not create directory: %s", modelPath), call. = FALSE)
  }
}

validateAppSignature <- function(appPath, pubKeyPaths = character(0L)) {
  pubKeyArgs <- unlist(lapply(pubKeyPaths, function(pubKeyPath) {
    c("-p", pubKeyPath)
  }), use.names = FALSE)
  procArgs <- c(
    "--no-echo", "--no-restore", "--vanilla", "-f",
    file.path(MIRO_APP_PATH, "tools", "verify_app", "verify.R"),
    "--args", "-m", appPath, pubKeyArgs
  )
  procResult <- processx::run("R", procArgs,
    env = character(0L), wd = MIRO_APP_PATH, stderr = "|", stdout = "|",
    timeout = 30L, error_on_status = FALSE
  )
  flog.info(
    "Process to verify app signature ended with return code: %s (timeout: %s).\nStdout: %s\nStderr: %s",
    procResult$status, procResult$timeout, procResult$stdout, procResult$stderr
  )
  if (!identical(procResult$timeout, FALSE) || is.na(procResult$status)) {
    stop("Validating app signature timed out after 30 seconds.", call. = FALSE)
  }
  if (identical(procResult$status, 0L)) {
    return()
  }
  if (identical(procResult$status, 1L)) {
    stop("Unexpected error while verifying the signature of the app! Check the logs for more information.", call. = FALSE)
  }
  if (identical(procResult$status, 3L)) {
    stop("App is not signed!", call. = FALSE)
  }
  stop("App signature invalid!", call. = FALSE)
}

extractAppData <- function(miroAppPath, appId, modelId, miroProc) {
  modelPath <- file.path(MIRO_MODEL_DIR, appId)
  dataPath <- file.path(MIRO_DATA_DIR, paste0("data_", appId))

  if (dir.exists(dataPath)) {
    flog.info("Data files for the app: %s already exist. They will be removed.", appId)
    if (unlink(dataPath, recursive = TRUE, force = TRUE) == 1) {
      stop("Removing existing app data failed", call. = FALSE)
    }
  }
  unzip(miroAppPath, overwrite = FALSE, exdir = modelPath)
  if (ENFORCE_SIGNED_APPS) {
    pubKeyPaths <- character(0L)
    if (dir.exists(file.path(MIRO_DATA_DIR, "known_keys"))) {
      pubKeyPaths <- list.files(file.path(MIRO_DATA_DIR, "known_keys"),
        all.files = TRUE, full.names = TRUE, no.. = TRUE
      )
      pubKeyPaths <- pubKeyPaths[!vapply(pubKeyPaths, dir.exists, logical(1L), USE.NAMES = FALSE)]
    }
    validateAppSignature(modelPath, pubKeyPaths)
  }
  dataDirSource <- file.path(modelPath, paste0("data_", modelId))
  if (dir.exists(dataDirSource)) {
    dir.create(dataPath)
    dataFiles <- list.files(dataDirSource)
    dataFilesCopied <- file.copy(file.path(dataDirSource, dataFiles), dataPath,
      overwrite = TRUE
    )
    if (any(!dataFilesCopied)) {
      flog.warn(
        "Problems moving app data file(s): '%s' from: '%s' to: '%s'",
        paste(dataFiles[!dataFilesCopied], collapse = ","), dataDirSource, dataPath
      )
    }
    if (unlink(dataDirSource, recursive = TRUE, force = TRUE) == 1) {
      flog.warn("Problems removing directory: %s", dataDirSource)
    }
  }
}

addAppLogo <- function(appId, logoFile = NULL) {
  logoDir <- file.path(MIRO_DATA_DIR, "logos")
  modelPath <- file.path(MIRO_MODEL_DIR, appId)
  if (length(logoFile)) {
    if (startsWith(logoFile, "/")) {
      logoPath <- logoFile
    } else {
      logoPath <- file.path(modelPath, logoFile)
    }
    newLogoName <- getLogoName(appId, logoFile)
    if (!file.exists(logoPath)) {
      stop(sprintf("Logo: %s does not exist.", logoPath), call. = FALSE)
    }
    if (file.size(logoPath) > MAX_LOGO_SIZE) {
      stop(sprintf(
        "Logo exceeds maximum size of %s bytes.",
        as.character(MAX_LOGO_SIZE)
      ), call. = FALSE)
    }
    file.copy2(
      logoPath,
      file.path(logoDir, newLogoName)
    )
  } else if (!file.exists(file.path(logoDir, "default_logo.png"))) {
    file.copy2(
      file.path("www", "default_logo.png"),
      file.path(logoDir, "default_logo.png")
    )
  }
}

removeAppData <- function(appId, logoFilename) {
  modelPath <- file.path(MIRO_MODEL_DIR, appId)
  dataPath <- file.path(MIRO_DATA_DIR, paste0("data_", appId))
  if (!identical(logoFilename, "default_logo.png")) {
    logoPath <- file.path(MIRO_DATA_DIR, "logos", logoFilename)
    if (file.exists(logoPath)) {
      if (unlink(logoPath) == 1) {
        flog.warn(
          "Removing logo: %s for app: %s failed.",
          logoPath, appId
        )
      }
    }
  }
  for (dirToRemove in c(modelPath, dataPath)) {
    if (dir.exists(dirToRemove)) {
      if (unlink(dirToRemove, recursive = TRUE, force = TRUE) == 1) {
        flog.warn(
          "Removing: %s for app: %s failed",
          dirToRemove, appId
        )
      }
    }
  }
}
