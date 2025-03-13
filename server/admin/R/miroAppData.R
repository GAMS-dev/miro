getModelPath <- function(appId) {
  if (IN_KUBERNETES) {
    if (startsWith(appId, "~$")) {
      return(file.path(tempdir(check = TRUE), appId, "model"))
    }
    return(file.path(SHARED_FS_MNT_DIR, "data", appId, "model"))
  }
  return(file.path(MIRO_MODEL_DIR, appId))
}

getDataPath <- function(appId) {
  if (IN_KUBERNETES) {
    if (startsWith(appId, "~$")) {
      return(file.path(tempdir(check = TRUE), appId, "data"))
    }
    return(file.path(SHARED_FS_MNT_DIR, "data", appId, "data"))
  }
  return(file.path(MIRO_DATA_DIR, paste0("data_", appId)))
}

removeTempDirs <- function(appId, warnOnly = FALSE) {
  if (IN_KUBERNETES) {
    tempDirs <- c(
      dirname(getModelPath(paste0("~$", appId))),
      dirname(getModelPath(paste0("~$~$", appId)))
    )
  } else {
    tempDirs <- c(
      getModelPath(paste0("~$", appId)), getModelPath(paste0("~$~$", appId)),
      getDataPath(paste0("~$", appId)), getDataPath(paste0("~$~$", appId))
    )
  }
  for (dirPath in tempDirs) {
    if (!identical(unlink(dirPath, recursive = TRUE), 0L)) {
      errorMessage <- sprintf(
        "Could not remove temporary directory: %s. Please try again or contact your system administrator.",
        dirPath
      )
      if (warnOnly) {
        flog.warn(dirPath)
        next
      }
      stop(errorMessage, call. = FALSE)
    }
  }
}

moveFilesFromTemp <- function(appId) {
  if (IN_KUBERNETES) {
    listOfDirsToMove <- list(c(
      dirname(getModelPath(appId)),
      dirname(getModelPath(paste0("~$~$", appId)))
    ), c(
      dirname(getModelPath(paste0("~$", appId))),
      dirname(getModelPath(appId))
    ))
  } else {
    listOfDirsToMove <- list(c(
      getModelPath(appId),
      getModelPath(paste0("~$~$", appId))
    ), c(
      getModelPath(paste0("~$", appId)),
      getModelPath(appId)
    ), c(
      getDataPath(appId),
      getDataPath(paste0("~$~$", appId)),
      TRUE
    ), c(
      getDataPath(paste0("~$", appId)),
      getDataPath(appId),
      TRUE
    ))
  }
  for (dirsToMove in listOfDirsToMove) {
    if (!is.na(dirsToMove[3]) && !file.exists(dirsToMove[[1]])) {
      # data directories don't have to exist
      next
    }
    if (!file.rename(dirsToMove[[1]], dirsToMove[[2]])) {
      stop(sprintf(
        "Could not rename directory: %s to: %s. Please try again or contact your system administrator.",
        dirsToMove[[1]], dirsToMove[[2]]
      ), call. = FALSE)
    }
  }
}

getLogoName <- function(appId, logoFile) {
  return(paste0(appId, "_", stringi::stri_rand_strings(1L, 15L)[[1L]], "_logo.", tools::file_ext(logoFile)))
}

createAppDir <- function(appId) {
  modelPath <- getModelPath(appId)
  failedDirCreate <- !dir.create(modelPath, recursive = TRUE)
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
  modelPath <- getModelPath(appId)
  dataPath <- getDataPath(appId)

  if (dir.exists(dataPath)) {
    flog.info("Data files for the app: %s already exist. They will be removed.", appId)
    if (unlink(dataPath, recursive = TRUE, force = TRUE) == 1) {
      stop("Removing existing app data failed", call. = FALSE)
    }
  }
  unzip(miroAppPath, overwrite = FALSE, exdir = modelPath)
  if (ENFORCE_SIGNED_APPS) {
    pubKeyPaths <- character(0L)
    if (dir.exists(file.path(getwd(), "data", "known_keys"))) {
      pubKeyPaths <- list.files(file.path(getwd(), "data", "known_keys"),
        all.files = TRUE, full.names = TRUE, no.. = TRUE
      )
      pubKeyPaths <- pubKeyPaths[!vapply(pubKeyPaths, dir.exists, logical(1L), USE.NAMES = FALSE)]
    }
    validateAppSignature(modelPath, pubKeyPaths)
  }
  dataDirSource <- file.path(modelPath, paste0("data_", modelId))
  if (dir.exists(dataDirSource)) {
    if (!dir.create(dataPath)) {
      flog.warn(
        "Failed to create data path for app: '%s'",
        dataPath, appId
      )
    }
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

addAppLogo <- function(appId, logoFile = NULL, newLogoName = NULL) {
  modelPath <- getModelPath(appId)
  if (length(logoFile)) {
    if (startsWith(logoFile, "/")) {
      logoPath <- logoFile
    } else {
      logoPath <- file.path(modelPath, logoFile)
    }
    if (is.null(newLogoName)) {
      newLogoName <- getLogoName(appId, logoFile)
    }
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
      file.path(LOGO_DIR, newLogoName)
    )
  } else if (!file.exists(file.path(LOGO_DIR, "default_logo.png"))) {
    file.copy2(
      file.path("www", "default_logo.png"),
      file.path(LOGO_DIR, "default_logo.png")
    )
  }
}

addAppFavicon <- function(appId, modelId) {
  appFaviconPath <- file.path(getModelPath(appId), paste0("static_", modelId), "favicon.ico")
  if (!file.exists(appFaviconPath)) {
    return(NULL)
  }
  if (file.size(appFaviconPath) > MAX_FAVICON_SIZE) {
    stop(sprintf(
      "Favicon exceeds maximum size of %s bytes.",
      as.character(MAX_FAVICON_SIZE)
    ), call. = FALSE)
  }
  faviconFileName <- getLogoName(appId, appFaviconPath)
  newFaviconPath <- file.path(LOGO_DIR, faviconFileName)
  file.copy2(
    appFaviconPath,
    newFaviconPath
  )
  return(file.path(LOGO_DIR_SP_CONTAINER, faviconFileName))
}

removeAppLogo <- function(appId, logoFilename) {
  if (!identical(logoFilename, "default_logo.png")) {
    logoPath <- file.path(LOGO_DIR, logoFilename)
    if (file.exists(logoPath)) {
      if (unlink(logoPath) == 1) {
        flog.warn(
          "Removing logo: %s for app: %s failed.",
          logoPath, appId
        )
      }
    }
  }
}

removeAppData <- function(appId, logoFilename) {
  if (IN_KUBERNETES) {
    dirsToRemove <- getModelPath(appId)
  } else {
    dirsToRemove <- c(getModelPath(appId), getDataPath(appId))
  }
  removeAppLogo(appId, logoFilename)
  for (dirToRemove in dirsToRemove) {
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
