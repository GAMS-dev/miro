extractAppData <- function(miroAppPath, appId, logoFile = NULL){
    modelPath <- file.path(MIRO_MODEL_DIR, appId)
    dataPath  <- file.path(MIRO_DATA_DIR, paste0("data_", appId))

    if(dir.exists(modelPath)){
        flog.info("The model files for the app: %s already exists. They will be removed.", appId)
        if(unlink(modelPath, recursive = TRUE, force = TRUE) == 1){
            stop("Removing existing app data failed", call. = FALSE)
        }
        if(unlink(dataPath, recursive = TRUE, force = TRUE) == 1){
            stop("Removing existing app data failed", call. = FALSE)
        }
    }
    unzip(miroAppPath, overwrite = FALSE, exdir = modelPath)
    dataDirSource <- file.path(modelPath, paste0("data_", appId))
    if(dir.exists(dataDirSource)){
        file.move(dataDirSource, dataPath)
    }
    addAppLogo(appId, logoFile)
}

addAppLogo <- function(appId, logoFile = NULL, newLogoName = NULL){
    logoDir <- file.path(MIRO_DATA_DIR, "logos")
    modelPath <- file.path(MIRO_MODEL_DIR, appId)
    if(length(logoFile)){
        if(length(newLogoName)){
            logoPath <- logoFile
        }else{
            logoPath <- file.path(modelPath, logoFile)
            newLogoName <- basename(logoFile)
        }
        if(!file.exists(logoPath)){
            stop("Logo does not exist.", call. = FALSE)
        }
        file.copy2(logoPath,
            file.path(logoDir, newLogoName))
    }else if(!file.exists(file.path(logoDir, "default_logo.png"))){
        file.copy2(file.path("www", "default_logo.png"),
            file.path(logoDir, "default_logo.png"))
    }
}

removeAppData <- function(appId, logoFilename){
    modelPath <- file.path(MIRO_MODEL_DIR, appId)
    dataPath  <- file.path(MIRO_DATA_DIR, paste0("data_", appId))
    if(!identical(logoFilename, "default_logo.png")){
        logoPath <- file.path(MIRO_DATA_DIR, "logos", logoFilename)
        if(file.exists(logoPath)){
            if(unlink(logoPath) == 1){
                flog.warn("Removing logo: %s for app: %s failed.",
                    logoPath, appId)
            }
        }
    }
    for(dirToRemove in c(modelPath, dataPath)){
        if(dir.exists(dirToRemove)){
            if(unlink(dirToRemove, recursive = TRUE, force = TRUE) != 1){
                flog.warn("Removing: %s for app: %s faield",
                    dirToRemove, appId)
            }
        }
    }
}
