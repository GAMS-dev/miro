#version number
MIROVersion <- "0.9.95"
APIVersion  <- "1"
MIRORDate   <- "Mar 11 2020"
#####packages:
# processx        #MIT
# dplyr           #MIT
# rhandsontable   #MIT
# plotly          #MIT
# V8              #MIT
# jsonlite        #MIT
# jsonvalidate    #MIT
# rpivotTable     #MIT
# R6              #MIT
# dygraphs        #MIT
# tidyr           #MIT
# zip             #CC0
# futile.logger   #LGPL-3
# readxl          #GPL-3
# DT              #GPL-3
# shiny           #GPL-3
# xts             #GPL >=v2
# shinydashboard  #GPL >= v2
# writexl         #BSD-2-clause
# stringi         #BSD-3-clause
# leaflet         #GPL-3
# leaflet.minicharts #GPL >=v2

# RPostgres (Scenario mode) #GPL-3
# DBI (Scenario mode)  #LGPL >=2
# RSQLite(Scenario mode) #LGPL >=2
# digest (Hypercube mode) #GPL >=2

# specify CRAN mirror (for list of mirrors, see: https://cran.r-project.org/mirrors.html)
CRANMirror <- "http://cran.us.r-project.org"
errMsg <- NULL
warningMsg <- NULL
loggerInitialised <- FALSE
if(R.version[["major"]] < 3 || 
   R.version[["major"]] == 3 && gsub("\\..$", "", 
                                     R.version[["minor"]]) < 6){
  stop("The R version you are using is not supported. At least version 3.6 is required to run GAMS MIRO.", call. = FALSE)
}
isShinyProxy <<- !identical(Sys.getenv("SHINYPROXY_USERNAME"), "")
debugMode <- TRUE
RLibPath <- NULL
miroBuildonly <- identical(Sys.getenv("MIRO_BUILD"), "true")
miroDeploy <- miroBuildonly
if(identical(Sys.getenv("MIRO_TEST_DEPLOY"), "true")){
  miroDeploy <- TRUE
  miroBuildonly <- FALSE
}
logToConsole <- TRUE
if(identical(Sys.getenv("MIRO_NO_DEBUG"), "true") && !miroDeploy){
  debugMode <- FALSE
  logToConsole <- FALSE
}else if(isShinyProxy){
  debugMode <- FALSE
}
tmpFileDir <- tempdir(check = TRUE)
# required packages
suppressMessages(library(R6))
requiredPackages <- c("jsonlite", "zip", "tibble", "readr")

config <- list()
gamsSysDir <- Sys.getenv("GAMS_SYS_DIR")

installedPackages <<- installed.packages()[, "Package"]
useGdx <<- FALSE
if("gdxrrwMIRO" %in% installedPackages){
  useGdx <<- TRUE
  requiredPackages <- c(requiredPackages, "gdxrrwMIRO")
}
# vector of required files
filesToInclude <- c("./global.R", "./components/util.R", if(useGdx) "./components/gdxio.R", 
                    "./components/json.R", "./components/load_scen_data.R", 
                    "./components/data_instance.R", "./components/worker.R", 
                    "./components/dataio.R", "./components/hcube_data_instance.R", 
                    "./components/miro_tabsetpanel.R", "./modules/render_data.R", 
                    "./modules/generate_data.R", "./components/script_output.R")
LAUNCHCONFIGMODE <- FALSE
LAUNCHHCUBEMODE <<- FALSE
if(debugMode && identical(tolower(Sys.info()[["sysname"]]), "windows")){
  pb <- winProgressBar(title = "Loading GAMS MIRO", label = "Loading required packages",
                       min = 0, max = 1, initial = 0, width = 300)
  setWinProgressBar(pb, 0.1)
}else{
  pb <- txtProgressBar(file = stderr())
}
source("./components/install_packages.R", local = TRUE)
if(debugMode && identical(tolower(Sys.info()[["sysname"]]), "windows")){
  setWinProgressBar(pb, 0.3, label= "Initializing GAMS MIRO")
}else{
  setTxtProgressBar(pb, 0.3)
}
if(is.null(errMsg)){
  # include custom functions and modules
  lapply(filesToInclude, function(file){
    if(!file.exists(file)){
      errMsg <<- paste(errMsg, paste0("Include file '", file, "' could not be located."), sep = "\n")
    }else{
      tryCatch({
        source(file)
      }, error = function(e){
        errMsg <<- paste(errMsg, paste0("Some error occurred while sourcing file '", 
                                        file, "'. Error message: ", e), sep = "\n")
      }, warning = function(w){
        errMsg <<- paste(errMsg, paste0("Some error occurred while sourcing file '", 
                                        file, "'. Error message: ", w), sep = "\n")
      })
    }
  })
  # set maximum upload size
  options(shiny.maxRequestSize = maxUploadSize*1024^2)
  # get model path and name
  modelPath    <- getModelPath(modelPath, isShinyProxy, "MIRO_MODEL_PATH",
                               file.path(getwd(), modelDir))
  modelNameRaw <- modelPath[[4]]
  modelName    <- modelPath[[3]]
  modelName    <<- modelName
  modelGmsName <- modelPath[[2]]
  modelPath    <- modelPath[[1]]
}

if(is.null(errMsg)){
  miroWorkspace <- NULL
  miroDbDir     <- NULL
  
  if(isShinyProxy){
    miroWorkspace <- file.path(getwd(), "ws")
  }else{
    # initialise MIRO workspace
    miroWorkspace <- Sys.getenv("MIRO_WS_PATH")
    if (identical(miroWorkspace, "")){
      miroWorkspace <- file.path(path.expand("~"), miroWorkspaceDir)
    }
    miroDbDir     <- Sys.getenv("MIRO_DB_PATH")
    if(identical(miroDbDir, "")){
      miroDbDir <- miroWorkspace
    }
  }
  if(!identical(Sys.getenv("MIRO_LOG_PATH"), "")){
    logFileDir <- Sys.getenv("MIRO_LOG_PATH")
  }else if(isShinyProxy){
    logFileDir <- file.path(tmpFileDir, logFileDir)
  }else{
    logFileDir <- file.path(miroWorkspace, logFileDir)
  }
  # set user ID (user name) and user groups
  ugroups <- NULL
  if(isShinyProxy){
    uid <- Sys.getenv("SHINYPROXY_USERNAME")
    if(is.null(uid) || grepl("^\\s*$", uid)){
      errMsg <- "No user ID specified (shinyproxy)."
    }
    ugroups <- csv2Vector(tolower(Sys.getenv("SHINYPROXY_USERGROUPS")))
    if(!length(ugroups) || grepl("^\\s*$", ugroups)){
      errMsg <- paste(errMsg, "No user groups specified (shinyproxy).", sep = "\n")
    }
    if(!identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true") && 
       any(!grepl("^[a-zA-Z0-9][a-zA-Z0-9!%\\(\\)\\-~]{3,19}$", c(uid, ugroups), perl = TRUE))){
      errMsg <- paste(errMsg, 
                      "Invalid user ID or user group specified. The following rules apply for user IDs and groups:\n- must be at least 4 and not more than 20 characters long\n- must start with a number or letter (upper or lowercase) {a-z}, {A-Z}, {0-9}\n- may container numbers, letters and the following additional characters: {!%()-~}",
                      sep = "\n")
    }
  }else{
    if(length(uid) != 1 || !is.character(uid)){
      errMsg <- "Invalid user ID specified."
    }
    if(!length(ugroups)){
      ugroups <- defaultGroup
    }
  }
}
if(is.null(errMsg)){
  # name of the R save file
  useTempDir <- !identical(Sys.getenv("MIRO_USE_TMP"), "false")
  # check if GAMS model file exists
  currentModelDir  <- modelPath
  if(!useTempDir && !file.exists(file.path(modelPath, modelGmsName))){
    errMsg <- sprintf("The GAMS model file: '%s' could not be found in the directory: '%s'." %+%
                        "Please make sure you specify a valid gms file path.", modelGmsName, modelPath)
  }
}
if(!miroDeploy &&
   identical(tolower(Sys.getenv("MIRO_MODE")), "config")){
  LAUNCHCONFIGMODE <- TRUE
}else if(identical(tolower(Sys.getenv("MIRO_MODE")), "hcube")){
  LAUNCHHCUBEMODE <<- TRUE
}
if(is.null(errMsg)){
  rSaveFilePath <- file.path(currentModelDir, 
                             paste0(modelNameRaw, "_",
                                    if(useTempDir) "1_" else "0_",
                                    APIVersion, "_",
                                    if(identical(Sys.getenv("MIRO_VERSION_STRING"), ""))
                                      MIROVersion
                                    else
                                      Sys.getenv("MIRO_VERSION_STRING"), 
                                    if(identical(Sys.getenv("MIRO_MODE"), "hcube")) "_hcube",
                                    ".miroconf"))
  if(debugMode){
    source("./modules/init.R", local = TRUE)
  }else if(!file.exists(rSaveFilePath)){
    errMsg <- sprintf("Miroconf file: '%s' does not exist.", 
                      rSaveFilePath)
  }else{
    load(rSaveFilePath)
    for (customRendererName  in customRendererNames){
      assign(customRendererName, get(customRendererName), envir = .GlobalEnv)
    }
  }
  if(!useGdx && identical(config$fileExchange, "gdx") && !miroBuildonly){
    errMsg <- paste(errMsg, 
                    sprintf("Can not use 'gdx' as file exchange with GAMS if gdxrrw library is not installed.\n
Please make sure you have a valid gdxrrwMIRO (https://github.com/GAMS-dev/gdxrrw-miro) installation in your R library: '%s'.", .libPaths()[1]),
                    sep = "\n")
  }
  GAMSClArgs <- c(paste0("execMode=", gamsExecMode),
                  paste0('IDCGDXOutput="', MIROGdxOutName, '"'))
  
  if(LAUNCHHCUBEMODE){
    # in Hypercube mode we have to run in a temporary directory
    if(!identical(useTempDir, TRUE)){
      errMsg <- paste(errMsg, "In Hypercube mode, MIRO must be executed in a temporary directory! MIRO_USE_TMP=false not allowed!",
                      sep = "\n")
    }
    GAMSClArgs <- c(GAMSClArgs, paste0('IDCGenerateGDXInput="', 
                                       MIROGdxInName, '"'))
  }
  if(isShinyProxy || identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
    dbConfig <- setDbConfig()
    
    if(isShinyProxy || identical(Sys.getenv("MIRO_REMOTE_EXEC"), "true")){
      config$activateModules$remoteExecution <- TRUE
    }
    
    if(length(dbConfig$errMsg)){
      errMsg <- dbConfig$errMsg
    }else{
      dbConfig <- dbConfig$data
    }
  }else{
    dbConfig <- list(type = "sqlite",
                     name = file.path(miroDbDir, 
                                      "miro.sqlite3"))
    if(identical(Sys.getenv("MIRO_REMOTE_EXEC"), "true")){
      config$activateModules$remoteExecution <- TRUE
    }
  }
  if(isTRUE(config$activateModules$remoteExecution)){
    useTempDir <- TRUE
  }
  if(debugMode){
    if(file.exists(file.path(currentModelDir, paste0(modelName, "_files.txt")))){
      tryCatch({
        modelFiles <- gsub("^[.][/\\\\]", "", readLines(file.path(currentModelDir, 
                                                                  paste0(modelName, "_files.txt")),
                                                        warn = FALSE))
      }, error = function(e){
        errMsg <<- paste(errMsg, sprintf("Problems reading file: '%s_files.txt'. Error message: '%s'.", 
                                         modelName, conditionMessage(e)),
                         sep = "\n")
      })
      buildArchive <- !identical(Sys.getenv("MIRO_BUILD_ARCHIVE"), "false")
      if(is.null(errMsg) && useTempDir && buildArchive){
        tryCatch({
          zipMiro(file.path(currentModelDir, paste0(modelName, ".zip")),
                  modelFiles, currentModelDir)
          modelFiles <- paste0(modelName, ".zip")
        }, error = function(e){
          errMsg <<- paste(errMsg, sprintf("Problems creating file: '%s'. Error message: '%s'.", 
                                           paste0(modelName, ".zip"), conditionMessage(e)),
                           sep = "\n")
        })
      }else if(miroDeploy){
        if(LAUNCHHCUBEMODE){
          errMsg <- paste(errMsg, "Hypercube mode requires to be executed in temporary directory. Set the environment variable MIRO_BUILD_ARCHIVE to 'true'!", 
                          sep = "\n")
        }else if(config$activateModules$remoteExecution){
          errMsg <- paste(errMsg, "Remote execution mode requires to be executed in temporary directory. Set the environment variable MIRO_BUILD_ARCHIVE to 'true'!", 
                          sep = "\n")
        }
      }
    }else if(miroDeploy){
      errMsg <- paste(errMsg, paste0("No model data ('", modelName, "_files.txt') found."), 
                      sep = "\n")
    }
    if(miroDeploy){
      if(file.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))){
        modelFiles <- c(modelFiles, paste0("static_", modelName))
      }
      if(file.exists(file.path(currentModelDir, 
                               paste0(miroDataDirPrefix, modelName)))){
        modelFiles <- c(modelFiles, paste0(miroDataDirPrefix, modelName))
      }
      if(file.exists(file.path(currentModelDir, 
                               paste0("renderer_", modelName)))){
        modelFiles <- c(modelFiles, paste0("renderer_", modelName))
      }
      if(is.null(errMsg) && identical(Sys.getenv("MIRO_TEST_DEPLOY"), "true")){
        modelPath <- file.path(tmpFileDir, modelName, "test_deploy")
        if(dir.exists(modelPath) && 
           unlink(modelPath, recursive = TRUE, force = TRUE) != 0L){
          errMsg <- sprintf("Problems removing temporary directory: '%s'. No write permissions?",
                            modelPath)
        }
        if(is.null(errMsg) && any(!file.copy2(file.path(currentModelDir, modelFiles), 
                                             file.path(modelPath, modelFiles)))){
          errMsg <- sprintf("Problems copying files from: '%s' to: '%s'. No write permissions?",
                            currentModelDir, modelPath)
        }
        if(is.null(errMsg)){
          currentModelDir <- modelPath
        }
      }
    }
  }
  overwriteLang <- Sys.getenv("MIRO_LANG")
  if(!identical(overwriteLang, "") && !identical(overwriteLang, config$language)){
    if(file.exists(file.path(".", "conf", paste0(overwriteLang, ".json")))){
      lang <- fromJSON(file.path(".", "conf", paste0(overwriteLang, ".json")),
                       simplifyDataFrame = FALSE, 
                       simplifyMatrix = FALSE)
      config$language <- overwriteLang
    }
  }
}
if(is.null(errMsg)){
  modelData <- NULL
  if(useTempDir && 
     file.exists(file.path(currentModelDir, paste0(modelName, ".zip")))){
    modelData <- file.path(currentModelDir, paste0(modelName, ".zip"))
  }else if(config$activateModules$remoteExecution || LAUNCHHCUBEMODE){
    errMsg <- paste(errMsg, sprintf("No model data ('%s.zip') found. Please make sure that you specify the files that belong to your model in a text file named '%s_files.txt'.", 
                                    modelName, modelName), 
                    sep = "\n")
  }else{
    GAMSClArgs <- c(GAMSClArgs, paste0('idir1="', gmsFilePath(currentModelDir), '"'))
  }
}

if(is.null(errMsg)){
  # load default and custom renderers (output data)
  rendererFiles <- list.files("./modules/renderers/", pattern = "\\.R$")
  for(file in rendererFiles){
    if(!file.access("./modules/renderers/" %+% file, mode = 4)){
      tryCatch({
        source("./modules/renderers/" %+% file)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing renderer file '%s'. Error message: '%s'.", 
                                 file, e), sep = "\n")
      }, warning = function(w){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing renderer file '%s'. Error message: '%s'.", 
                                 file, w), sep = "\n")
      })
    }else{
      errMsg <- "File: '" %+% file %+% "' could not be found or user has no read permissions."
    }
  }
}
if(is.null(errMsg) && debugMode){
  customRendererDirs <<- file.path(c(file.path(currentModelDir, ".."),
                                     currentModelDir), paste0("renderer_", modelName))
  for(customRendererDir in customRendererDirs){
    rendererFiles <- list.files(customRendererDir, pattern = "\\.R$")
    lapply(rendererFiles, function(file){
      if(!file.access(file.path(customRendererDir, file), mode = 4)){
        tryCatch({
          source(file.path(customRendererDir, file))
        }, error = function(e){
          errMsg <<- paste(errMsg, 
                           sprintf("Some error occurred while sourcing custom renderer file '%s'. Error message: %s.", 
                                   file, e), sep = "\n")
        }, warning = function(w){
          errMsg <<- paste(errMsg, 
                           sprintf("Some error occurred while sourcing custom renderer file '%s'. Error message: %s.", 
                                   file, w), sep = "\n")
        })
      }else{
        errMsg <<- paste(errMsg, sprintf("Custom renderer file: '%s' could not be found or user has no read permissions.",
                                         file), sep = "\n")
      }
    })
  }
  listOfCustomRenderers <- Set$new()
  requiredPackagesCR <<- NULL
  
  if(!LAUNCHCONFIGMODE){
    for(customRendererConfig in c(configGraphsOut, configGraphsIn, config$inputWidgets)){
      # check whether non standard renderers were defined in graph config
      if(!is.null(customRendererConfig$rendererName)){
        customRendererConfig$outType <- customRendererConfig$rendererName
      }
      if(any(is.na(match(tolower(customRendererConfig$outType), standardRenderers)))){
        customRendererName <- "render" %+% toupper(substr(customRendererConfig$outType, 1, 1)) %+% 
          substr(customRendererConfig$outType, 2, nchar(customRendererConfig$outType))
        customRendererOutput <- customRendererConfig$outType %+% "Output"
        # find render function
        tryCatch({
          match.fun(customRendererName)
          listOfCustomRenderers$push(customRendererName)
        }, error = function(e){
          errMsg <<- paste(errMsg, 
                           sprintf("A custom renderer function: '%s' was not found. Please make sure first define such a function.", 
                                   customRendererName), sep = "\n")
        })
        # find output function
        tryCatch({
          match.fun(customRendererOutput)
          listOfCustomRenderers$push(customRendererOutput)
        }, error = function(e){
          errMsg <<- paste(errMsg, 
                           sprintf("No output function for custom renderer function: '%s' was found. Please make sure you define such a function.", 
                                   customRendererName), sep = "\n")
        })
        # find packages to install and install them
        if(length(customRendererConfig$packages)){
          requiredPackagesCR <- c(requiredPackagesCR, customRendererConfig$packages)
        }
      }
    }
  }
  requiredPackagesCR <- unique(requiredPackagesCR)
  customRendererNames <- listOfCustomRenderers$get()
  rm(listOfCustomRenderers)
}
if(miroBuildonly){
  if(!is.null(errMsg)){
    warning(errMsg)
    if(interactive())
      stop()
    if(identical(errMsg, paste0("\nNo model data ('", modelName, "_files.txt') found."))){
      quit("no", status = 2)
    }else{
      quit("no", status = 1) 
    }
  }
  save(list = c("customRendererNames", customRendererNames, "modelIn", "modelInRaw", 
                "modelOut", "config", "lang", "inputDsNames", "inputDsAliases", 
                "outputTabTitles", "modelInTemplate", "scenDataTemplate", 
                "modelInTabularData", "modelInTabularDataBase", "externalInputConfig", "tabSheetMap",
                "modelInFileNames", "ddownDep", "aliasesNoDep", "idsIn",
                "choicesNoDep", "sliderValues", "configGraphsOut", 
                "configGraphsIn", "hotOptions", "inputTabs", "inputTabTitles", 
                "scenInputTabs", "scenInputTabTitles", "isGroupOfSheets", 
                "groupSheetToTabIdMap", "scalarsInTemplate", "modelInWithDep",
                "modelOutAlias", "colsWithDep", "scalarsInMetaData",
                "modelInMustImport", "modelInAlias", "DDPar", "GMSOpt", 
                "modelInToImportAlias", "modelInToImport", "inputDsNamesNotToDisplay",
                "scenTableNames", "modelOutTemplate", "scenTableNamesToDisplay", 
                "GAMSReturnCodeMap", "dependentDatasets", "outputTabs", 
                "installPackage", "dbSchema", "scalarInputSym", "scalarInputSymToVerify",
                "requiredPackagesCR", "datasetsRemoteExport", "dropdownAliases"), 
       file = rSaveFilePath)
  if(identical(Sys.getenv("MIRO_COMPILE_ONLY"), "true")){
    quit("no")
  }
  if(identical(Sys.getenv("MIRO_MODE"), "full")){
    buildArchive <- !identical(Sys.getenv("MIRO_BUILD_ARCHIVE"), "false")
    Sys.setenv(MIRO_COMPILE_ONLY = "true")
    Sys.setenv(MIRO_USE_TMP = "true")
    Sys.setenv(MIRO_BUILD_ARCHIVE = "true")
    Sys.setenv(MIRO_MODE = "hcube")
    buildProcHcube <- processx::process$new(file.path(R.home(), 'bin', 'Rscript'), 
                                            c('--vanilla', './app.R'),
                                            stderr = "|")
    Sys.setenv(MIRO_COMPILE_ONLY = "")
    Sys.setenv(MIRO_MODE = "full")
    Sys.setenv(MIRO_BUILD_ARCHIVE = if(buildArchive) "true" else "false")
    Sys.setenv(MIRO_USE_TMP = if(useTempDir) "true" else "false")
    buildProcHcube$wait()
    procHcubeRetC <- buildProcHcube$get_exit_status()
    if(!identical(procHcubeRetC, 0L)){
      warning(buildProcHcube$read_error())
      if(interactive())
        stop()
      quit("no", procHcubeRetC)
    }
    rSaveFilePath <- c(rSaveFilePath, 
                       file.path(currentModelDir, 
                                 paste0(modelNameRaw, "_1_",
                                        APIVersion, "_",
                                        MIROVersion, 
                                        "_hcube.miroconf")))
    if(!paste0(modelName, ".zip") %in% modelFiles){
      modelFiles <- c(modelFiles, paste0(modelName, ".zip"))
    }
  }
  tryCatch({
    zipMiro(file.path(currentModelDir, paste0(modelNameRaw, ".miroapp")), 
            c(modelFiles, basename(rSaveFilePath)), currentModelDir)
  }, error = function(e){
    stop(sprintf("Problems creating app bundle. Error message: '%s'.", 
                 conditionMessage(e)), call. = FALSE)
  })
  if(!identical(unlink(rSaveFilePath), 0L)){
    flog.warn("Could not remove miroconf file: '%s'.", rSaveFilePath)
  }
  if(interactive())
    stop()
  quit("no")
}
if(is.null(errMsg)){
  if(!dir.exists(miroWorkspace) &&
     !dir.create(miroWorkspace, showWarnings = FALSE)[1]){
    errMsg <- paste(errMsg, sprintf("Could not create MIRO workspace directory: '%s'. Please make sure you have sufficient permissions. '", 
                                    miroWorkspace), sep = "\n")
  }else{
    if(isWindows()){
      tryCatch(
        processx::run("attrib", args = c("+h", miroWorkspace))
        , error = function(e){
          warningMsg <<- paste(warningMsg, 
                               sprintf("Failed to hide MIRO workspace directory: '%s'. Error message: '%s'.", 
                                       miroWorkspace, conditionMessage(e)), sep = "\n")
        })
    }
  }
  #initialise loggers
  if(!dir.exists(logFileDir)){
    tryCatch({
      if(!dir.create(logFileDir, showWarnings = FALSE))
        stop()
    }, error = function(e){
      errMsg <<- "Log file directory could not be created. Check that you have sufficient read/write permissions in application folder."
    })
  }
}
requiredPackages <- c("stringi", "shiny", "shinydashboard", "processx", 
                      "dplyr", "readxl", "writexl", "rhandsontable", 
                      "rpivotTable", "futile.logger", "tidyr")
source("./components/install_packages.R", local = TRUE)

if(is.null(errMsg)){
  flog.appender(do.call(if(identical(logToConsole, TRUE)) "appender.tee" else "appender.file", 
                        list(file = file.path(logFileDir, 
                                              paste0(modelName, "_", uid, "_", 
                                                     format(Sys.time(), 
                                                            "%y.%m.%d_%H.%M.%S"), ".log")))))
  flog.threshold(loggingLevel)
  flog.trace("Logging facility initialised.")
  loggerInitialised <- TRUE
  if(!is.null(requiredPackagesCR)){
    requiredPackages <- requiredPackagesCR
    source("./components/install_packages.R", local = TRUE)
    rm(requiredPackagesCR)
  }
  if(config$activateModules$remoteExecution){
    requiredPackages <- c("future", "httr")
  }else if(length(externalInputConfig) || length(datasetsRemoteExport)){
    requiredPackages <- "httr"
  }else{
    requiredPackages <- character(0L)
  }
  if(LAUNCHCONFIGMODE){
    requiredPackages <- c(requiredPackages, "plotly", "xts", "dygraphs", "leaflet",
                          "leaflet.minicharts", "timevis", "DT")
  }else{
    requiredPackages <- c(requiredPackages, 
                          if(identical(installPackage$plotly, TRUE)) "plotly",
                          if(identical(installPackage$dygraphs, TRUE)) c("xts", "dygraphs"),
                          if(identical(installPackage$leaflet, TRUE)) c("leaflet", "leaflet.minicharts"),
                          if(identical(installPackage$timevis, TRUE)) c("timevis"))
  }
  if(identical(installPackage$DT, TRUE) || ("DT" %in% installedPackages)){
    requiredPackages <- c(requiredPackages, "DT")
  }
  source("./components/install_packages.R", local = TRUE)
  options("DT.TOJSON_ARGS" = list(na = "string", na_as_null = TRUE))
  
  if(config$activateModules$remoteExecution && !LAUNCHCONFIGMODE){
    plan(multiprocess)
  }
  # try to create the DB connection (PostgreSQL)
  auth <- NULL
  db <- NULL
  if(identical(tolower(dbConfig$type), "sqlite")){
    requiredPackages <- c("DBI", "RSQLite")
  }else{
    requiredPackages <- c("DBI", "RPostgres")
  }
  source("./components/install_packages.R", local = TRUE)
  
  source("./components/db.R")
  source("./components/db_scen.R")
  tryCatch({
    scenMetadataTable <- scenMetadataTablePrefix %+% modelName
    db   <- Db$new(uid = uid, dbConf = dbConfig, dbSchema = dbSchema,
                   slocktimeLimit = slocktimeLimit, modelName = modelName,
                   attachmentConfig = if(config$activateModules$attachments) 
                     list(maxSize = attachMaxFileSize, maxNo = attachMaxNo)
                   else NULL,
                   hcubeActive = LAUNCHHCUBEMODE, ugroups = ugroups)
    conn <- db$getConn()
    flog.debug("Database connection established.")
  }, error = function(e){
    flog.error("Problems initialising database class. Error message: %s", e)
    errMsg <<- conditionMessage(e)
  })
  # initialise access management
  source("./components/db_auth.R")
  tryCatch({
    auth <- Auth$new(conn, uid, defaultGroup = defaultGroup, 
                     tableNameGroups = amTableNameGroups, 
                     tableNameElements = amTableNameElements, 
                     tableNameHierarchy = amTableNameHierarchy, 
                     tableNameMetadata = scenMetadataTable, 
                     uidIdentifier = uidIdentifier, 
                     accessIdentifier = accessIdentifier, 
                     accessElIdentifier = accessElIdentifier)
    flog.debug("Access Control initialised.")
  }, error = function(e){
    flog.error("Problems initialising authorisation class. Error message: %s", e)
    errMsg <<- paste(errMsg, conditionMessage(e), sep = '\n')
  })
  tryCatch({
    dataio <- DataIO$new(config = list(modelIn = modelIn, modelOut = modelOut, 
                                       modelName = modelName),
                         db = db, auth = auth)
  }, error = function(e) {
    flog.error("Problems initialising dataio class. Error message: %s", e)
    errMsg <<- paste(errMsg, conditionMessage(e), sep = '\n')
  })
  
  if(LAUNCHHCUBEMODE){
    hcubeDirName <<- file.path(miroWorkspace, hcubeDirName, modelName)
    if(!dir.exists(hcubeDirName) && 
       !dir.create(hcubeDirName, showWarnings = TRUE, recursive = TRUE)){
      msg <- sprintf("Problems creating Hypercube jobs directory: '%s'. Do you miss write permissions?",
                     hcubeDirName)
      flog.error(errMsgTmp)
      errMsg <- paste(msg, errMsgTmp, sep = '\n')
    }
    requiredPackages <- c("digest", "DT")
    source("./components/install_packages.R", local = TRUE)
    source("./components/db_hcubeimport.R")
    source("./components/db_hcubeload.R")
  }
}
inconsistentTableNames <- NULL
if(is.null(errMsg) && debugMode){
  # checking database inconsistencies
  local({
    orphanedTables <- NULL
    tryCatch({
      orphanedTables <- db$getOrphanedTables(hcubeScalars = getHcubeScalars(modelIn))
    }, error = function(e){
      flog.error("Problems fetching orphaned database tables. Error message: '%s'.", e)
      errMsg <<- paste(errMsg, sprintf("Problems fetching orphaned database tables. Error message: '%s'.", 
                                       conditionMessage(e)), sep = '\n')
    })
    if(length(orphanedTables)){
      msg <- sprintf("There are orphaned tables in your database: '%s'.\n
This could be caused because you used a different database schema in the past (e.g. due to different inputs and/or outputs). 
Note that you can remove orphaned database tables using the configuration mode ('Database management' section).",
                     paste(orphanedTables, collapse = "', '"))
      flog.warn(msg)
    }
    inconsistentTables <- NULL
    tryCatch({
      inconsistentTables <- db$getInconsistentTables()
    }, error = function(e){
      flog.error("Problems fetching database tables (for inconsistency checks).\nDetails: '%s'.", e)
      errMsg <<- paste(errMsg, sprintf("Problems fetching database tables (for inconsistency checks). Error message: '%s'.", 
                                       conditionMessage(e)), sep = '\n')
    })
    if(length(inconsistentTables$names)){
      inconsistentTableNames <<- paste0(gsub("_", "", modelName, fixed = TRUE), 
                                        "_", inconsistentTables$names)
      flog.error(sprintf("There are tables in your database that do not match the current database schema of your model.\n
Those tables are: '%s'.\nError message: '%s'.",
                         paste(inconsistentTables$names, collapse = "', '"), inconsistentTables$errMsg))
      msg <- paste(errMsg, sprintf("There are tables in your database that do not match the current database schema of your model.\n
Those tables are: '%s'.\nError message: '%s'.",
                                   paste(inconsistentTables$names, collapse = "', '"), inconsistentTables$errMsg),
                   collapse = "\n")
      errMsg <<- paste(errMsg, msg, sep = "\n")
    }
  })
}

aboutDialogText <- paste0("<b>GAMS MIRO v.", MIROVersion, "</b><br/><br/>",
                          "Release Date: ", MIRORDate, "<br/>", 
                          "Copyright (c) 2020 GAMS Software GmbH &lt;support@gams.com&gt;<br/>",
                          "Copyright (c) 2020 GAMS Development Corp. &lt;support@gams.com&gt;<br/><br/>",
                          "This program is free software: you can redistribute it and/or modify ",
                          "it under the terms of version 3 of the GNU General Public License as published by ",
                          "the Free Software Foundation.<br/><br/>",
                          "This program is distributed in the hope that it will be useful, ", 
                          "but WITHOUT ANY WARRANTY; without even the implied warranty of ",
                          "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ",
                          "GNU General Public License for more details.<br/><br/>",
                          "You should have received a copy of the GNU General Public License ",
                          "along with this program. If not, see ",
                          "<a href=\\'http://www.gnu.org/licenses/\\' target=\\'_blank\\'>http://www.gnu.org/licenses/</a>.",
                          "For more information about third-party software included in MIRO, see ",
                          "<a href=\\'http://www.gams.com/miro/license.html\\' target=\\'_blank\\'>here</a>.")

if(is.null(errMsg)){
  tryCatch({
    gdxio <<- NULL
    if(useGdx){
      gdxio <<- GdxIO$new(file.path(.libPaths()[1], "gdxrrwMIRO", 
                                    if(identical(tolower(Sys.info()[["sysname"]]), "windows")) 
                                      file.path("bin", "x64") else "bin"), 
        c(modelInRaw, modelOut), scalarsFileName,
        scalarsOutName, scalarEquationsName, scalarEquationsOutName,
        dropdownAliases)
    }
  }, error = function(e){
    flog.error(e)
    errMsg <<- paste(errMsg, e, sep = '\n')
  })
  remoteUser <- uid
  rememberMeFileName <- paste0(miroWorkspace, .Platform$file.sep, ".cred_", 
                               modelName)
  credConfig <- NULL
  if(isShinyProxy){
    usernameTmp <- if(identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true")) "user" else uid
    credConfig <- list(url = Sys.getenv("MIRO_GAMS_HOST"), 
                       username = usernameTmp,
                       password = usernameTmp,
                       namespace = "global",
                       useRegistered = TRUE,
                       registerUser = TRUE)
  }else if(config$activateModules$remoteExecution){
    tryCatch({
      if(file.exists(file.path(miroWorkspace, "pinned_pub_keys"))){
        pinnedPublicKeys <- paste0("sha256//", 
                                   read_lines(file.path(miroWorkspace, 
                                                        "pinned_pub_keys")),
                                   collapse = ";")
        httr::set_config(httr::config(pinnedpublickey = pinnedPublicKeys))
      }
    }, error = function(e){
      errMsg <<- paste(errMsg, sprintf("Could not read pinned certificates file. Error message: '%s'.", 
                                       conditionMessage(e)), sep = '\n')
    })
    tryCatch({
      credConfigTmp <- NULL
      if(file.exists(rememberMeFileName)){
        credConfigTmp <- suppressWarnings(fromJSON(rememberMeFileName, 
                                                   simplifyDataFrame = FALSE, 
                                                   simplifyMatrix = FALSE))
        if(!is.list(credConfigTmp) || !all(c('url', 'username', 'password', 'namespace', 'reg') 
                                           %in% names(credConfigTmp)) || 
           !all(vapply(credConfigTmp[1:4], is.character, 
                       logical(1L), USE.NAMES = FALSE)) ||
           !is.logical(credConfigTmp[[5L]])){
          errMsg <<- "Malformatted credential file. Looks like someone tried to tamper with the app!"
          flog.error(errMsg)
          return(NULL)
        }
        credConfig <- list(url = credConfigTmp$url, 
                           username = credConfigTmp$username,
                           password = credConfigTmp$password,
                           namespace = credConfigTmp$namespace,
                           useRegistered = credConfigTmp$reg)
      }
    }, error = function(e){
      errMsg <<- "Problems reading JSON file: '%s'. Please make sure you have sufficient access permissions."
      flog.error(errMsg)
    }, finally = rm(credConfigTmp))
  }
}
if(!is.null(errMsg)){
  if(loggerInitialised){
    if(length(warningMsg)) flog.warn(warningMsg)
    flog.fatal(errMsg)
  }else{
    warning(errMsg)
  }
  if(isShinyProxy){
    stop('An error occured. Check log for more information!', call. = FALSE)
  }
  if(debugMode && identical(tolower(Sys.info()[["sysname"]]), "windows")){
    setWinProgressBar(pb, 1, label= "GAMS MIRO initialised")
  }else{
    setTxtProgressBar(pb, 1)
  }
  close(pb)
  pb <- NULL
  ui_initError <- fluidPage(
    tags$head(
      if(!is.list(config) || !is.character(config$theme)){
        tags$link(type = "text/css", rel = "stylesheet", href = "skin_light.css")
      }else{
        tags$link(type = "text/css", rel = "stylesheet", href = paste0("skin_", config$theme, ".css"))
      },
      tags$script(src = "miro.js", type = "application/javascript")
    ),
    titlePanel(
      if(!exists("lang") || is.null(lang$errMsg$initErrors$title)){
        "Some errors occurred"
      }else{
        lang$errMsg$initErrors$title
      }),
    fluidRow(align="center",
             tags$div(id = "removeSuccess", class = "gmsalert gmsalert-success",
                      if(!exists("lang") || is.null(lang$adminMode$database$removeSuccess)){
                        "Database tables removed successfully"
                      }else{
                        lang$adminMode$database$removeSuccess
                      }),
             tags$div(id = "unknownError", class = "gmsalert gmsalert-error",
                      if(!exists("lang") || is.null(lang$errMsg$unknownError)){
                        "An unexpected error occurred."
                      }else{
                        lang$errMsg$unknownError
                      }),
             HTML("<br>"),
             div(
               if(!exists("lang") || is.null(lang$errMsg$initErrors$desc)){
                 "Please fix the errors mentioned below and restart GAMS MIRO:"
               }else{
                 lang$errMsg$initErrors$desc
               }
               , class = "initErrors"),
             HTML("<br>"),
             verbatimTextOutput("errorMessages"),
             if(length(inconsistentTableNames)){
               actionButton("removeDbTablesPre", lang$adminMode$database$remove)
               tagList(
                 tags$div(id = "db_remove_wrapper",
                          if(!exists("lang") || is.null(lang$adminMode$database$removeInconsistent)){
                            "You want to remove all the inconsistent tables?"
                          }else{
                            lang$adminMode$database$removeInconsistent
                          },
                          actionButton("removeInconsistentDbTables", 
                                       "Delete inconsistent database tables")
                 ),
                 tags$div(id = "db_remove_wrapper",
                          if(!exists("lang") || is.null(lang$adminMode$database$removeWrapper)){
                            "You want to remove all the tables that belong to your model (e.g. because the schema changed)?"
                          }else{
                            lang$adminMode$database$removeWrapper
                          },
                          actionButton("removeDbTablesPre", 
                                       if(!exists("lang") || is.null(lang$adminMode$database$removeDialogBtn)){
                                         "Delete all database tables"
                                       }else{
                                         lang$adminMode$database$removeDialogBtn
                                       })
                 )
               )
             },
             tableOutput("JSONErrorMessages"),
             tags$div(style = "text-align:center;margin-top:20px;", 
                      actionButton("btCloseInitErrWindow", if(!exists("lang") || is.null(lang$errMsg$initErrors$okButton))
                        "Ok" else lang$errMsg$initErrors$okButton))
    )
  )
  server_initError <- function(input, output, session){
    if(length(inconsistentTableNames)){
      if(!exists("lang") || is.null(lang$adminMode$database$removeDialogTitle)){
        removeDbTabLang <- list(title = "Remove database tables",
                                desc = "Are you sure that you want to delete all database tables? This can not be undone! You might want to save the database first before proceeding.",
                                cancel = "Cancel",
                                confirm = "")
      }else{
        removeDbTabLang <- list(title = lang$adminMode$database$removeDialogTitle,
                                desc = lang$adminMode$database$removeDialogDesc,
                                cancel = lang$adminMode$database$removeDialogCancel,
                                confirm = lang$adminMode$database$removeDialogConfirm)
      }
      observeEvent(input$removeInconsistentDbTables, {
        showModal(modalDialog(title = removeDbTabLang$title,
                                     if(!exists("lang") || is.null(lang$adminMode$database$removeInconsistentConfirm) || 
                                        is.null(lang$adminMode$database$cannotBeUndone)){
                                       "Are you sure that you want to delete all inconsistent database tables? This can not be undone! You might want to save the database first before proceeding."
                                     }else{
                                       paste0(lang$adminMode$database$removeInconsistentConfirm, lang$adminMode$database$cannotBeUndone)
                                     }, footer = tagList(
                                modalButton(removeDbTabLang$cancel),
                                actionButton("removeInconsistentDbTablesConfirm", label = removeDbTabLang$confirm, 
                                             class = "bt-highlight-1"))))
      })
      observeEvent(input$removeInconsistentDbTablesConfirm, {
        tryCatch({
          if(length(inconsistentTableNames)){
            db$removeTablesModel(inconsistentTableNames)
          }
        }, error = function(e){
          flog.error("Unexpected error: '%s'. Please contact GAMS if this error persists.", e)
          showHideEl(session, "#unknownError", 6000L)
        })
        removeModal()
        showHideEl(session, "#removeSuccess", 3000L)
      })
      observeEvent(input$removeDbTablesPre, {
        showModal(modalDialog(title = removeDbTabLang$title,
                              removeDbTabLang$desc, footer = tagList(
                                modalButton(removeDbTabLang$cancel),
                                actionButton("removeDbTables", label = removeDbTabLang$confirm, 
                                             class = "bt-highlight-1"))))
      })
      source(file.path("tools", "config", "db_management.R"), local = TRUE)
    }
    output$errorMessages <- renderText(
      errMsg
    )
    output$JSONErrorMessages <- renderTable(
      if(exists("jsonErrors")) jsonErrors, bordered = TRUE
    )
    observeEvent(input$btCloseInitErrWindow, {
      if(!interactive()){
        stopApp()
      }
    })
    session$onSessionEnded(function() {
      if(!interactive()){
        stopApp()
      }
    })
  }
  
  shinyApp(ui = ui_initError, server = server_initError)
}else{
  uidAdmin <<- Sys.getenv("MIRO_ADMIN_USER", uid)
  local({
    if(debugMode && identical(tolower(Sys.info()[["sysname"]]), "windows")){
      setWinProgressBar(pb, 0.6, label= "Importing new data")
    }
    miroDataDir   <- Sys.getenv("MIRO_DATA_DIR")
    if(identical(miroDataDir, "")){
      miroDataDir   <- file.path(currentModelDir, paste0(miroDataDirPrefix, modelName))
    }
    miroDataFiles <- list.files(miroDataDir)
    dataFileExt   <- tolower(tools::file_ext(miroDataFiles))
    miroDataFiles <- miroDataFiles[dataFileExt %in% c(if(useGdx) "gdx", "xlsx", "xls", "zip")]
    newScen <- NULL
    tryCatch({
      if(length(miroDataFiles)){
        tabularDatasetsToFetch <- modelInTabularData
        tabularIdsToFetchId    <- names(modelIn) %in% tabularDatasetsToFetch
        metaDataTmp            <- modelIn[tabularIdsToFetchId]
        namesScenInputData     <- names(modelIn)[tabularIdsToFetchId]
        modelInTemplateTmp     <- modelInTemplate[tabularIdsToFetchId]
        if(length(scalarsInMetaData) && !scalarsFileName %in% tabularDatasetsToFetch){
          tabularDatasetsToFetch <- c(tabularDatasetsToFetch, scalarsFileName)
          namesScenInputData <- c(namesScenInputData, scalarsFileName)
          modelInTemplateTmp[[length(metaDataTmp) + 1L]] <- scalarsInTemplate
          metaDataTmp <- c(metaDataTmp, scalarsInMetaData)
        }
        inputIdsTmp        <- match(inputDsNames, names(metaDataTmp))
        inputIdsTmp        <- inputIdsTmp[!is.na(inputIdsTmp)]
        metaDataTmp        <- metaDataTmp[inputIdsTmp]
        modelInTemplateTmp <- modelInTemplateTmp[inputIdsTmp]
        
        tmpDirToRemove     <- character(0L)
        
        for(i in seq_along(miroDataFiles)){
          miroDataFile <- miroDataFiles[i]
          flog.info("New data: '%s' is being stored in the database. Please wait a until the import is finished.", miroDataFile)
          if(dataFileExt[i] %in% c("xls", "xlsx")){
            method <- "xls"
            tmpDir <- miroDataDir
          }else if(dataFileExt[i] == "zip"){
            method <- "csv"
            tmpDir <- tryCatch(
              getValidCsvFromZip(file.path(miroDataDir, miroDataFile), 
                                 c(names(modelOut), 
                                   inputDsNames), uid)$tmpDir
              , error = function(e){
                flog.error(conditionMessage(e))
                return("e")
              })
            if(identical(tmpDir, "e")){
              next
            }
            tmpDirToRemove <- tmpDir
          }else{
            method <- dataFileExt[i]
            tmpDir <- miroDataDir
          }
          newScen <- Scenario$new(db = db, sname = gsub("\\.[^\\.]*$", "", miroDataFile), isNewScen = TRUE,
                                  readPerm = c(uidAdmin, ugroups), writePerm = uidAdmin,
                                  execPerm = c(uidAdmin, ugroups), uid = uidAdmin)
          dataOut <- loadScenData(scalarsOutName, modelOut, tmpDir, modelName, scalarsFileHeaders,
                                  modelOutTemplate, method = method, fileName = miroDataFile)$tabular
          dataIn  <- loadScenData(scalarsName = scalarsFileName, metaData = metaDataTmp, 
                                  workDir = tmpDir, 
                                  modelName = modelName, errMsg = lang$errMsg$GAMSInput$badInputData,
                                  scalarsFileHeaders = scalarsFileHeaders,
                                  templates = modelInTemplateTmp, method = method,
                                  fileName = miroDataFile, DDPar = DDPar, GMSOpt = GMSOpt)$tabular
          if(!scalarsFileName %in% names(metaDataTmp) && length(c(DDPar, GMSOpt))){
            # additional command line parameters that are not GAMS symbols
            scalarsTemplate <- tibble(a = character(0L), b = character(0L), c = character(0L))
            names(scalarsTemplate) <- scalarsFileHeaders
            newScen$save(c(dataOut, dataIn, list(scalarsTemplate)))
          }else{
            newScen$save(c(dataOut, dataIn))
          }
          
          if(!debugMode && !file.remove(file.path(miroDataDir, miroDataFile))){
            flog.info("Could not remove file: '%s'.", miroDataFile)
          }
        }
        if(length(tmpDirToRemove)){
          if(identical(unlink(tmpDirToRemove, recursive = TRUE), 0L)){
            flog.debug("Temporary directory: '%s' removed.", tmpDirToRemove)
          }else{
            flog.error("Problems removing temporary directory: '%s'.", tmpDirToRemove)
          }
        }
      }
    }, error = function(e){
      flog.error("Problems saving MIRO data to database. Error message: '%s'.", e)
      gc()
      if(identical(Sys.getenv("MIRO_POPULATE_DB"), "true")){
        if(interactive())
          stop()
        quit("no", 1L)
      }
    })
    if(identical(Sys.getenv("MIRO_POPULATE_DB"), "true")){
      if(interactive())
        stop()
      quit("no", 0L)
    }
  })
  
  if(debugMode && identical(tolower(Sys.info()[["sysname"]]), "windows")){
    setWinProgressBar(pb, 1, label= "GAMS MIRO initialised")
  }else{
    setTxtProgressBar(pb, 1)
  }
  close(pb)
  pb <- NULL
  if(LAUNCHCONFIGMODE){
    source("./tools/config/server.R", local = TRUE)
    source("./tools/config/ui.R", local = TRUE)
    shinyApp(ui = ui_admin, server = server_admin)
  }else{
    
    #______________________________________________________
    #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    #                   Server
    #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    #______________________________________________________
    server <- function(input, output, session){
      newTab <- vector("list", maxNumberScenarios + 3L)
      btSortNameDesc     <- FALSE
      btSortTimeDesc     <- TRUE
      btSortTime         <- TRUE
      btSortNameDescBase <- FALSE
      btSortTimeDescBase <- TRUE
      btSortTimeBase     <- TRUE
      jobImportID        <- NULL
      resetWidgetsOnClose <- TRUE
      # boolean that specifies whether output data should be saved
      saveOutput         <- TRUE
      # count number of open scenario tabs
      numberScenTabs     <- 0L
      # boolean that specifies whether input data shall be overridden
      overwriteInput      <- FALSE
      # boolean that specifies whether data shall be saved under a new name 
      # or existing scenario shall be overridden
      saveAsFlag         <- TRUE
      # boolean that specifies whether output data is included in currently loaded dataset
      noOutputData       <- TRUE
      # count number of prepared scenarios for asynchronous solve
      asyncCount         <- 1L
      asyncLogLoaded     <- vector(mode = "logical", 3L)
      asyncResObs        <- NULL
      # parameters used for saving scenario data
      scenData           <- list()
      scenData[["scen_1_"]] <- scenDataTemplate
      # parameter used for saving (hidden) scalar data
      scalarData         <- list()
      traceData          <- data.frame()
      # boolean that specifies whether handsontable is initialised
      hotInit            <- vector("logical", length = length(modelIn))
      # boolean that specifies whether check if data is unsaved should be skipped
      noCheck            <- vector("logical", length = length(modelIn))
      noCheck[]          <- TRUE
      # when inputs change values quickly, they sometimes lag in updating its element in 
      # 'input' list. Thus, we remember currently selected values in this list
      selectedDepEl      <- vector(mode = "list", length = length(modelIn))
      # list of attachments for active scenario
      attachmentList     <- tibble(name = vector("character", attachMaxNo),
                                   execPerm = vector("logical", attachMaxNo))
      # boolean that specifies whether input data does not match output data
      dirtyFlag          <- FALSE
      isInSplitView      <- if(identical(config$defCompMode, "split")) TRUE else FALSE
      if(isInSplitView){
        enableEl(session, "#btCompareScen")
      }
      isInCompareMode    <- FALSE
      isInSolveMode      <- TRUE
      modelStatus        <- NULL
      
      # currently active scenario (R6 object)
      activeScen         <- Scenario$new(db = db, sname = lang$nav$dialogNewScen$newScenName, 
                                         isNewScen = TRUE)
      exportFileType     <- if(useGdx) "gdx" else "xls"
      
      # scenId of tabs that are loaded in ui (used for shortcuts) (in correct order)
      sidCompOrder     <- NULL
      
      worker <- Worker$new(metadata = list(uid = uid, modelName = modelName, noNeedCred = isShinyProxy,
                                           tableNameTracePrefix = tableNameTracePrefix, maxSizeToRead = 5000,
                                           modelDataFiles = if(identical(config$fileExchange, "gdx")) 
                                             c(MIROGdxInName, MIROGdxOutName) else 
                                               paste0(c(names(modelOut), inputDsNames), ".csv"),
                                           MIROGdxInName = MIROGdxInName,
                                           clArgs = GAMSClArgs, 
                                           text_entities = c(paste0(modelName, ".lst"), 
                                                             if(config$activateModules$miroLogFile) config$miroLogFile),
                                           gamsExecMode = gamsExecMode,
                                           extraClArgs = config$extraClArgs, 
                                           saveTraceFile = config$saveTraceFile,
                                           modelGmsName = modelGmsName, gamsSysDir = gamsSysDir, csvDelim = config$csvDelim,
                                           timeout = 10L, serverOS = getOS(), modelData = modelData, hcubeMode = LAUNCHHCUBEMODE,
                                           rememberMeFileName = rememberMeFileName), 
                           remote = config$activateModules$remoteExecution,
                           hcube = LAUNCHHCUBEMODE,
                           db = db)
      if(length(credConfig)){
        do.call(worker$setCredentials, credConfig)
      }
      
      scenMetaData     <- list()
      # scenario metadata of scenario saved in database
      scenMetaDb       <- NULL
      scenMetaDbBase   <- NULL
      scenMetaDbBaseList <- NULL
      scenTags         <- NULL
      scenMetaDbSubset <- NULL
      scenMetaDbBaseSubset <- NULL
      # save the scenario ids loaded in UI
      scenCounterMultiComp <- 4L
      sidsInComp       <- vector("integer", length = maxNumberScenarios + 1)
      sidsInSplitComp  <- vector("integer", length = 2L)
      # occupied slots (scenario is loaded in ui with this rv$scenId)
      occupiedSidSlots <- vector("logical", length = maxNumberScenarios)
      loadInLeftBoxSplit <- TRUE
      # trigger navigation through tabs by shortcuts
      shortcutNest     <- 0L
      nestTabsetsViaShortcuts <- function(direction){
        shortcutNest <<- min(2L, shortcutNest + direction)
      }
      observeEvent(input$tabsetShortcutNest, {
        nestTabsetsViaShortcuts(direction = +1L)
      })
      observeEvent(input$tabsetShortcutUnnest, {
        nestTabsetsViaShortcuts(direction = -1L)
      })
      navigateTabsViaShortcuts <- function(direction){
        
        if(isolate(input$sidebarMenuId) == "inputData"){
          flog.debug("Navigated %d input tab (using shortcut).", direction)
          currentGroup <- as.numeric(gsub("\\D", "", isolate(input$inputTabset)))
          if(shortcutNest && length(inputTabs[[currentGroup]]) > 1L){
            currentSheet <- as.integer(strsplit(isolate(input[[paste0("inputTabset", 
                                                                      currentGroup)]]), "_")[[1]][2])
            updateTabsetPanel(session, paste0("inputTabset", currentGroup), 
                              paste0("inputTabset", currentGroup, "_", 
                                     currentSheet + direction))
          }else{
            updateTabsetPanel(session, "inputTabset", paste0("inputTabset_", 
                                                             currentGroup + direction))
          }
        }else if(isolate(input$sidebarMenuId) == "outputData"){
          flog.debug("Navigated %d output tabs (using shortcut).", direction)
          currentGroup <- as.numeric(gsub("\\D", "", isolate(input$outputTabset)))
          if(shortcutNest && length(outputTabs[[currentGroup]]) > 1L){
            currentSheet <- as.integer(strsplit(isolate(input[[paste0("outputTabset", 
                                                                      currentGroup)]]), "_")[[1]][2])
            updateTabsetPanel(session, paste0("outputTabset", currentGroup), 
                              paste0("outputTabset", 
                                     currentGroup, "_", currentSheet + direction))
          }else{
            updateTabsetPanel(session, "outputTabset", 
                              paste0("outputTabset_", currentGroup + direction))
          }
        }else if(isolate(input$sidebarMenuId) == "scenarios"){
          if(isInSplitView){
            flog.debug("Navigated %d data tabs in split view scenario comparison view (using shortcut).", direction)
            currentScen <- 2
            currentSheet <- as.integer(strsplit(isolate(input[[paste0("contentScen_", currentScen)]]),
                                                "_", fixed = TRUE)[[1L]][[3L]])
            if(shortcutNest > 0L && isGroupOfSheets[[currentSheet]]){
              # nest to group of sheets
              currentGroup <- currentSheet
              currentSheet <- as.integer(strsplit(isolate(input[[paste0("contentScen_", currentScen, "_", 
                                                                        currentGroup)]]),
                                                  "_", fixed = TRUE)[[1L]][[4L]])
              updateTabsetPanel(session, paste0("contentScen_", currentScen, 
                                                "_", currentGroup), 
                                paste0("contentScen_", currentScen, "_", 
                                       currentGroup, "_", currentSheet + direction))
              
              updateTabsetPanel(session, paste0("contentScen_", currentScen + 1L, 
                                                "_", currentGroup), 
                                paste0("contentScen_", currentScen + 1L, "_", 
                                       currentGroup, "_", currentSheet + direction))
            }else{
              # switch to next group
              updateTabsetPanel(session, paste0("contentScen_", currentScen), 
                                paste0("contentScen_", currentScen, "_", currentSheet + direction))
              updateTabsetPanel(session, paste0("contentScen_", currentScen + 1L), 
                                paste0("contentScen_", currentScen + 1L, "_", currentSheet + direction))
            }
          }else{
            if(is.null(sidCompOrder)){
              return()
            }
            currentScen <- as.integer(strsplit(isolate(input$scenTabset), "_", fixed = TRUE)[[1L]][[2L]])
            if(shortcutNest > 0L){
              flog.debug("Navigated %d data tabs in scenario comparison view (using shortcut).", direction)
              currentSheet <- as.integer(strsplit(isolate(input[[paste0("contentScen_", currentScen)]]),
                                                  "_", fixed = TRUE)[[1L]][[3L]])
              if(shortcutNest > 1L && isGroupOfSheets[[currentSheet]]){
                # nest to group of sheets
                currentGroup <- currentSheet
                currentSheet <- as.integer(strsplit(isolate(input[[paste0("contentScen_", currentScen, "_", 
                                                                          currentGroup)]]),
                                                    "_", fixed = TRUE)[[1L]][[4L]])
                updateTabsetPanel(session, paste0("contentScen_", currentScen, "_", currentGroup), 
                                  paste0("contentScen_", currentScen, "_", 
                                         currentGroup, "_", currentSheet + direction))
              }else{
                # switch to next group
                updateTabsetPanel(session, paste0("contentScen_", currentScen), 
                                  paste0("contentScen_", currentScen, "_", currentSheet + direction))
              }
            }else{
              flog.debug("Navigated %d scenario tabs in scenario comparison view (using shortcut).", direction)
              # go to next scenario tab
              idx <- which(sidCompOrder == currentScen)[1]
              updateTabsetPanel(session, "scenTabset", paste0("scen_", sidCompOrder[idx + direction], "_"))
            }
          }
        }else if(isolate(input$sidebarMenuId) == "hcubeAnalyze"){
          flog.debug("Navigated %d data tabs in paver output tabpanel (using shortcut).", direction)
          # go to next data sheet
          local({
            tabsetName <- isolate(input$analysisResults)
            currentSheet <- suppressWarnings(as.numeric(substr(tabsetName, 
                                                               nchar(tabsetName), nchar(tabsetName))))
            if(is.na(currentSheet))
              return()
            updateTabsetPanel(session, "analysisResults", 
                              paste0("analysisResults_", currentSheet + direction))
          })
        }
      }
      observeEvent(input$tabsetShortcutNext, {
        navigateTabsViaShortcuts(direction = +1L)
      })
      observeEvent(input$tabsetShortcutPrev,{
        navigateTabsViaShortcuts(direction = -1L)
      })
      
      # initially set rounding precision to default
      roundPrecision <- config$roundingDecimals
      
      # set local working directory
      unzipModelFilesProcess <- NULL
      if(useTempDir){
        workDir <- file.path(tmpFileDir, session$token)
        if(!config$activateModules$remoteExecution && length(modelData)){
          tryCatch({
            if(isWindows() && !identical(substring(workDir, 1L, 1L),
                                         substring(.libPaths()[1L], 1L, 1L))){
              # workaround as cmdunzip crashed on Windows when on different drive  than exdir
              # see https://github.com/r-lib/zip/issues/45
              unzip(modelData, exdir = workDir)
            }else{
              unzipModelFilesProcess <- unzip_process()$new(modelData, exdir = workDir, 
                                                            stderr = NULL)
            }
          }, error = function(e){
            flog.error("Problems creating process to extract model file archive. Error message: '%s'.", 
                       conditionMessage(e))
          })
        }
      }else{
        workDir <- currentModelDir
      }
      flog.info("Session started (model: '%s', user: '%s', workdir: '%s').", 
                modelName, uid, workDir)
      
      worker$setWorkDir(workDir)
      scriptOutput <- NULL
      
      if(LAUNCHHCUBEMODE){
        if(length(config$scripts)){
          scriptOutput <- ScriptOutput$new(session, file.path(workDir, paste0("scripts_", modelName)), 
                                           config$scripts, lang$nav$scriptOutput$errMsg)
        }
      }else{
        if(length(config$scripts$base)){
          scriptOutput <- ScriptOutput$new(session, file.path(workDir, paste0("scripts_", modelName)),
                                           config$scripts, lang$nav$scriptOutput$errMsg)
          observeEvent(input$runScript, {
            scriptId <- suppressWarnings(as.integer(input$runScript))
            if(is.na(scriptId) || scriptId < 1 || 
               scriptId > length(config$scripts$base)){
              flog.error("A script with id: '%s' was attempted to be executed. However, this script does not exist. Looks like an attempt to tamper with the app!",
                         input$runScript)
              return()
            }
            if(scriptOutput$isRunning(scriptId)){
              flog.debug("Button to interrupt script: '%s' clicked.", scriptId)
              scriptOutput$interrupt(scriptId)
              hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
              hideEl(session, paste0("#scriptOutput_", scriptId, " .script-output"))
              showEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
              
              showElReplaceTxt(session, paste0("#scriptOutput_", scriptId, " .btn-run-script"), 
                               lang$nav$scriptOutput$runButton)
              return()
            }
            flog.debug("Button to execute script: '%s' clicked.", scriptId)
            
            if(!dir.exists(paste0(workDir, .Platform$file.sep, "scripts_", modelName))){
              if(dir.exists(paste0(currentModelDir, .Platform$file.sep, "scripts_", modelName))){
                if(!file.copy2(paste0(currentModelDir, .Platform$file.sep, "scripts_", modelName),
                               paste0(workDir, .Platform$file.sep, "scripts_", modelName))){
                  flog.error("Problems copying files from: '%s' to: '%s'.",
                             paste0(workDir, .Platform$file.sep, "scripts_", modelName),
                             paste0(currentModelDir, .Platform$file.sep, "scripts_", modelName))
                  hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
                  hideEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
                  return(scriptOutput$sendContent(lang$errMsg$unknownError, scriptId, isError = TRUE))
                }
              }else{
                flog.info("No 'scripts_%s' directory was found. Did you forget to include it in '%s_files.txt'?",
                          modelName, modelName)
                hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
                showEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
                return(scriptOutput$sendContent(lang$nav$scriptOutput$errMsg$noScript, scriptId, isError = TRUE))
              }
            }
            showEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
            hideEl(session, paste0("#scriptOutput_", scriptId, " .script-output"))
            hideEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
            
            errMsg <- NULL
            
            tryCatch({
              saveAsFlag <<- FALSE
              source("./modules/scen_save.R", local = TRUE)
              data <- scenData[[scenIdLong]]
              names(data) <- c(names(modelOut), inputDsNames)
              gdxio$wgdx(paste0(workDir, .Platform$file.sep, 
                                "scripts_", modelName, .Platform$file.sep, "data.gdx"), 
                         data, squeezeZeros = 'n')
            }, error = function(e){
              flog.error("Problems writing gdx file for script: '%s'. Error message: '%s'.", 
                         scriptId, conditionMessage(e))
              errMsg <<- sprintf(lang$errMsg$fileWrite$desc, "data.gdx")
              hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
              hideEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
              scriptOutput$sendContent(errMsg, scriptId, isError = TRUE)
            })
            if(!is.null(errMsg)){
              return()
            }
            tryCatch({
              scriptOutput$run(scriptId)
              showElReplaceTxt(session, paste0("#scriptOutput_", scriptId, " .btn-run-script"), 
                               lang$nav$scriptOutput$interruptButton)
            }, error = function(e){
              flog.info("Script: '%s' crashed during startup. Error message: '%s'.",
                        scriptId, conditionMessage(e))
              scriptOutput$sendContent(lang$nav$scriptOutput$errMsg$crash, scriptId, 
                                       hcube = FALSE, isError = TRUE)
            })
          })
          observeEvent(input$outputGenerated,{
            noOutputData <<- FALSE
          })
        }
      }
      
      if(!dir.exists(workDir) && !dir.create(workDir, recursive = TRUE)){
        if(!dir.exists(workDir)){
          flog.fatal("Working directory: '%s' could not be initialised.", workDir)
          showErrorMsg(lang$errMsg$fileWrite$title, lang$errMsg$fileWrite$desc)
          stop()
        }
      }else{
        flog.debug("Working directory was created: '%s'.", workDir)
      }
      # initialization of several variables
      rv <- reactiveValues(scenId = 4L, unsavedFlag = FALSE, btLoadScen = 0L, btOverwriteScen = 0L, btSolve = 0L,
                           btOverwriteInput = 0L, btSaveAs = 0L, btSaveConfirm = 0L, btRemoveOutputData = 0L, 
                           btLoadLocal = 0L, btCompareScen = 0L, activeSname = NULL, clear = TRUE, btSave = 0L, 
                           btSplitView = 0L, noInvalidData = 0L, uploadHcube = 0L, btSubmitJob = 0L,
                           jobListPanel = 0L, importJobConfirm = 0L, importJobNew = 0L)
      suppressCloseModal <- FALSE
      # list of scenario IDs to load
      sidsToLoad <- list()
      loadIntoSandbox <- FALSE
      # list with input data
      modelInputData  <- vector(mode = "list", length = length(modelIn))
      modelInputDataVisible <- vector(mode = "list", length = length(modelIn))
      modelInputGraphVisible <- vector(mode = "logical", length = length(modelIn))
      modelInputDataHcube <- vector(mode = "list", length = length(modelIn))
      externalInputData <- vector(mode = "list", length = length(modelIn))
      externalInputData_filtered <- vector(mode = "list", length = length(modelIn))
      # list with input data before new data was loaded as shiny is lazy when data is equal and wont update
      previousInputData <- vector(mode = "list", length = length(modelIn))
      # initialize model input data
      modelInputData <- modelInTemplate
      
      tryCatch({
        if(length(config$defaultScenName) && nchar(trimws(config$defaultScenName))){
          defSid <- db$getSid(config$defaultScenName)
          if(identical(defSid, 0L) && !identical(uid, uidAdmin)){
            defSid <- db$getSid(config$defaultScenName, uid = uidAdmin)
          }
          if(!identical(defSid, 0L)){
            sidsToLoad <- list(defSid)
            suppressCloseModal <- TRUE
            rv$btOverwriteScen <- isolate(rv$btOverwriteScen) + 1L
          }
        }
      }, error = function(e){
        flog.warn("Problems loading default scenario. Error message: '%s'.", e)
      })
      
      # initialise list of reactive expressions returning data for model input
      dataModelIn <- vector(mode = "list", length = length(modelIn))
      # auxiliary vector that specifies whether data frame has no data or data was overwritten
      isEmptyInput <- vector(mode = "logical", length = length(modelIn))
      # input is empty in the beginning
      isEmptyInput[] <- TRUE
      # list of data frames which save changes made in handsontable
      hotInput <- vector(mode = "list", length = length(modelIn))
      tableContent <- vector(mode = "list", length = length(modelIn))
      # gams process object
      gams <- NULL
      # boolean that specifies whether input data should be overridden
      inputOverwriteConfirmed <- FALSE
      
      observeEvent(input$sidebarMenuId,{
        flog.debug("Sidebar menu item: '%s' selected.", isolate(input$sidebarMenuId))
        # reset nest level
        shortcutNest <<- 0L
        if(identical(input$sidebarMenuId, "scenarios")){
          isInSolveMode <<- FALSE
        }else if(identical(input$sidebarMenuId, "importData")){
          rv$jobListPanel <- rv$jobListPanel + 1L
        }
      })
      
      lapply(seq_along(modelIn), function(i){
        if(modelIn[[i]]$type == "hot"){
          observeEvent(input[["in_" %+% i %+% "_select"]], {
            hotInit[[i]] <<- TRUE
            isEmptyInput[[i]] <<- FALSE
            if(noCheck[i]){
              noCheck[i] <<- FALSE
            }
          })
          return()
        }
        observe({
          switch(modelIn[[i]]$type,
                 hot = {
                   input[["in_" %+% i]]
                 },
                 dt ={
                   input[[paste0("in_", i, "_cell_edit")]]
                 }, 
                 slider = {
                   input[["slider_" %+% i]]
                   input[["hcubeStep_" %+% i]]
                   input[["hcubeMode_" %+% i]]
                 },
                 textinput ={
                   input[["text_" %+% i]]
                 }, 
                 numericinput ={
                   input[["numeric_" %+% i]]
                 }, 
                 dropdown = {
                   input[["dropdown_" %+% i]]
                 },
                 date = {
                   input[["date_" %+% i]]
                 },
                 daterange = {
                   input[["daterange_" %+% i]]
                 },
                 checkbox = {
                   input[["cb_" %+% i]]
                 })
          if(noCheck[i]){
            noCheck[i] <<- FALSE
            return()
          }
          # if scenario includes output data set dirty flag
          if(!noOutputData){
            dirtyFlag <<- TRUE
            showEl(session, "#dirtyFlagIcon")
            showEl(session, "#dirtyFlagIconO")
          }
          rv$unsavedFlag <<- TRUE
        }, priority = 1000L)
      })
      
      markUnsaved <- function(markDirty = FALSE){
        if(markDirty && !noOutputData){
          showEl(session, "#dirtyFlagIcon")
          showEl(session, "#dirtyFlagIconO")
          dirtyFlag     <<- TRUE
        }else{
          hideEl(session, "#dirtyFlagIcon")
          hideEl(session, "#dirtyFlagIconO")
          dirtyFlag     <<- FALSE
        }
        rv$unsavedFlag <<- TRUE
        return(invisible())
      }
      
      markSaved <- function(){
        hideEl(session, "#dirtyFlagIcon")
        hideEl(session, "#dirtyFlagIconO")
        dirtyFlag     <<- FALSE
        rv$unsavedFlag <<- FALSE
        return(invisible())
      }
      
      # print scenario title in input and output sheets
      getScenTitle <- reactive({
        nameSuffix <- ""
        if(rv$unsavedFlag){
          nameSuffix <- " (*)"
        }
        if(is.null(activeScen) || !length(activeScen$getSid())){
          if(length(rv$activeSname)){
            if(length(activeScen))
              activeScen$updateMetadata(newName = rv$activeSname)
            return(tags$i(paste0("<", htmltools::htmlEscape(rv$activeSname), ">", nameSuffix)))
          }
          return(tags$i(paste0("<", htmltools::htmlEscape(lang$nav$dialogNewScen$newScenName), ">", nameSuffix)))
        }else{
          return(paste0(htmltools::htmlEscape(rv$activeSname), nameSuffix))
        }
      })
      output$inputDataTitle <- renderUI(getScenTitle())
      output$outputDataTitle <- renderUI(getScenTitle())
      
      # activate solve button once all model input files are imported
      observe({
        datasetsImported <- vapply(names(modelInMustImport), function(el){
          i <- match(el, names(modelIn))[[1]]
          if(length(rv[["in_" %+% i]])){
            return(TRUE)
          }else{
            return(FALSE)
          }
        }, logical(1), USE.NAMES = FALSE)
        if(all(datasetsImported)){
          addClassEl(session, "#btSolve", "glow-animation")
        }else{
          removeClassEl(session, "#btSolve", "glow-animation")
        }
      })
      # UI elements (modalDialogs)
      source("./UI/dialogs.R", local = TRUE)
      ####### Model input
      # render tabular input datasets
      source("./modules/input_render_tab.R", local = TRUE)
      # render non tabular input datasets (e.g. slider, dropdown)
      source("./modules/input_render_nontab.R", local = TRUE)
      # generate import dialogue
      source("./modules/input_ui.R", local = TRUE)
      # load input data from Excel sheet
      source("./modules/scen_import.R", local = TRUE)
      
      ####### GAMS interaction
      # solve button clicked
      source("./modules/gams_run.R", local = TRUE)
      # Interrupt button clicked
      source("./modules/gams_interrupt.R", local = TRUE)
      
      
      ####### Model output
      # render output graphs
      source("./modules/output_render.R", local = TRUE)
      obsCompare <- vector("list", maxNumberScenarios)
      # switch between tabular view and output graphs
      source("./modules/output_table_view.R", local = TRUE)
      if(isTRUE(config$hasSymbolLinks)){
        source("./modules/symbol_links.R", local = TRUE)
      }
      
      ####### Advanced options
      if(isTRUE(config$activateModules$downloadTempFiles)){
        source("./modules/download_tmp.R", local = TRUE)
      }
      
      ####### Paver interaction
      if(LAUNCHHCUBEMODE){
        source("./modules/gams_job_list.R", local = TRUE)
        ####### Hcube import module
        source("./modules/hcube_import.R", local = TRUE)
        ####### Hcube load module
        source("./modules/hcube_load.R", local = TRUE)
        # analyze button clicked
        source("./modules/analysis_run.R", local = TRUE)
      }else if(config$activateModules$remoteExecution){
        source("./modules/gams_job_list.R", local = TRUE)
        # remote job import
        source("./modules/job_import.R", local = TRUE)
      }
      
      # delete scenario 
      source("./modules/db_scen_remove.R", local = TRUE)
      # scenario module
      #load shared datasets
      source("./modules/db_external_load.R", local = TRUE)
      # load scenario
      source("./modules/db_scen_load.R", local = TRUE)
      # save scenario
      source("./modules/db_scen_save.R", local = TRUE)
      # scenario split screen mode
      source("./modules/scen_split.R", local = TRUE)
      skipScenCompObserve <- vector("logical", maxNumberScenarios + 3L)
      
      scenCompUpdateTab <- function(scenId, sheetId, groupId = NULL){
        if(is.null(groupId)){
          if(!identical(isolate(input[[paste0("contentScen_", scenId)]]), 
                        paste0("contentScen_", scenId, "_", sheetId)))
            skipScenCompObserve[scenId] <<- TRUE
          updateTabsetPanel(session, paste0("contentScen_", scenId),
                            paste0("contentScen_", scenId, "_", sheetId))
        }else{
          updateTabsetPanel(session, paste0("contentScen_", scenId),
                            paste0("contentScen_", scenId, "_", groupId))
          updateTabsetPanel(session, paste0("contentScen_", scenId, "_", groupId),
                            paste0("contentScen_", scenId, "_", groupId, "_", sheetId))
        }
      }
      
      source("./modules/scen_compare_actions.R", local = TRUE)
      
      lapply(seq_len(maxNumberScenarios  + 3L), function(i){
        scenIdLong <- paste0("scen_", i, "_")
        # compare scenarios
        obsCompare[[i]] <<- observe({
          if(is.null(input[[paste0("contentScen_", i)]]) || 
             skipScenCompObserve[i]){
            skipScenCompObserve[i] <<- FALSE
            return(NULL)
          }
          j <- as.integer(strsplit(isolate(input[[paste0("contentScen_", i)]]), 
                                   "_", fixed = TRUE)[[1]][[3L]])
          groupId <- NULL
          if(isGroupOfSheets[[j]]){
            groupId <- j
            j <- strsplit(input[[paste0("contentScen_", i, "_", groupId)]], 
                          "_", fixed = TRUE)[[1L]][[4L]]
          }
          if(identical(i, 2L)){
            scenCompUpdateTab(scenId = i + 1L, sheetId = j, groupId = groupId)
          }else if(identical(i, 3L)){
            scenCompUpdateTab(scenId = i - 1L, sheetId = j, groupId = groupId)
          }else{
            lapply(names(scenData), function(scen){
              scenCompUpdateTab(scenId = as.integer(strsplit(scen, "_")[[1]][[2L]]), 
                                sheetId = j, groupId = groupId)
            })
          }
        }, suspended = TRUE)
      })
      
      # scenario comparison
      source("./modules/scen_compare.R", local = TRUE)
      
      observeEvent(input$btExportScen, {
        if(useGdx && !LAUNCHHCUBEMODE){
          exportTypes <- c(gdx = "gdx", xlsx = "xls", csv = "csv")
        }else{
          exportTypes <- c(csv = "csv", xlsx = "xls")
        }
        if(length(datasetsRemoteExport)){
          exportTypes <- c(exportTypes, setNames(names(datasetsRemoteExport), 
                                                 names(datasetsRemoteExport)))
        }
        showScenExportDialog(input$btExportScen, exportTypes)
      })
      observeEvent(input$exportFileType, {
        stopifnot(length(input$exportFileType) > 0L)
        
        if(length(datasetsRemoteExport) && 
           input$exportFileType %in% names(datasetsRemoteExport)){
          hideEl(session, ".file-export")
          showEl(session, ".remote-export")
          return()
        }
        
        switch(input$exportFileType,
               xls = exportFileType <<- "xlsx",
               gdx = exportFileType <<- "gdx",
               csv = exportFileType <<- "csv",
               flog.warn("Unknown export file type: '%s'.", input$exportFileType))
      })
      hideEl(session, "#loading-screen")
      
      # This code will be run after the client has disconnected
      session$onSessionEnded(function() {
        # remove temporary files and folders
        if(useTempDir){
          unlink(workDir, recursive=TRUE)
        }
        suppressWarnings(rm(activeScen))
        try(flog.info("Session ended (model: '%s', user: '%s').", modelName, uid), 
            silent = TRUE)
        if(identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true")){
          # clean up
          try(db$deleteRows(scenMetadataTable, 
                            uidIdentifier, 
                            uid), silent = TRUE)
          
        }
        if(config$activateModules$attachments && 
           config$storeLogFilesDuration > 0L){
          db$removeExpiredAttachments(paste0(modelName, c(".log", ".lst")), 
                                      config$storeLogFilesDuration)
        }
        gc()
        if(!interactive() && !isShinyProxy){
          tryCatch({
            if(length(unzipModelFilesProcess) && 
               !length(unzipModelFilesProcess$get_exit_status())){
              unzipModelFilesProcess$kill()
            }
          }, error = function(e){
            flog.error("Problems killing process to extract model files. Error message: '%s'.", 
                       conditionMessage(e))
          }, finally = {
            unzipModelFilesProcess <- NULL
          })
          stopApp()
        }
      })
    }
    
    #______________________________________________________
    #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    #                 UI
    #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    #______________________________________________________
    
    
    source("./UI/scen_tabset.R", local = TRUE)
    source("./UI/header.R", local = TRUE)
    source("./UI/sidebar.R", local = TRUE)
    source("./UI/body.R", local = TRUE)
    
    ui <- dashboardPage(header, sidebar, body, skin = "black")
    
    app <- shinyApp(ui = ui, server = server)
  }
}