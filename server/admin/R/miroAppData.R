getLogoName <- function(modelId, logoFile) {
  return(paste0(modelId, "_logo.", tools::file_ext(logoFile)))
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

extractAppData <- function(miroAppPath, appId, modelId) {
  modelPath <- file.path(MIRO_MODEL_DIR, appId)
  dataPath <- file.path(MIRO_DATA_DIR, paste0("data_", appId))

  if (dir.exists(dataPath)) {
    flog.info("Data files for the app: %s already exist. They will be removed.", appId)
    if (unlink(dataPath, recursive = TRUE, force = TRUE) == 1) {
      stop("Removing existing app data failed", call. = FALSE)
    }
  }
  unzip(miroAppPath, overwrite = FALSE, exdir = modelPath)
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

addAppLogo <- function(appId, modelId, logoFile = NULL) {
  logoDir <- file.path(MIRO_DATA_DIR, "logos")
  modelPath <- file.path(MIRO_MODEL_DIR, appId)
  if (length(logoFile)) {
    if (startsWith(logoFile, "/")) {
      logoPath <- logoFile
    } else {
      logoPath <- file.path(modelPath, logoFile)
    }
    newLogoName <- getLogoName(modelId, logoFile)
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
