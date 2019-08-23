#version number
MIROVersion <- "0.8.7"
MIRORDate   <- "Aug 23 2019"
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

# odbc (Scenario mode) #MIT
# DBI (Scenario mode)  #LGPL >=2
# RSQLite(Scenario mode) #LGPL >=2
# digest (Hypercube mode) #GPL >=2


# specify CRAN mirror (for list of mirrors, see: https://cran.r-project.org/mirrors.html)
CRANMirror <- "http://cran.us.r-project.org"
errMsg <- NULL
if(R.version[["major"]] < 3 || 
   R.version[["major"]] == 3 && gsub("\\..$", "", 
                                     R.version[["minor"]]) < 5){
  errMsg <- "The R version you are using is not supported. At least version 3.5 is required to run GAMS MIRO."
}
tmpFileDir <- tempdir(check = TRUE)
# directory of configuration files
configDir <- "./conf/"
# required packages
suppressMessages(library(R6))
requiredPackages <- c("stringi", "shiny", "shinydashboard", "processx", 
                      "V8", "dplyr", "readr", "readxl", "writexl", "rhandsontable", 
                      "jsonlite", "jsonvalidate", "rpivotTable", 
                      "futile.logger", "zip", "tidyr")

gamsSysDir   <- ""
getCommandArg <- function(argName, exception = TRUE){
  # local mode
  args <- commandArgs(trailingOnly = TRUE)
  matches <- grepl(paste0("^-+", argName, "\\s?=\\s?"), args, 
                   ignore.case = TRUE)
  if(any(matches)){
    return(gsub(paste0("^-+", argName, "\\s?=\\s?"), "", args[matches][1], 
                ignore.case = TRUE))
  }else{
    if(exception){
      stop()
    }else{
      return("")
    }
  }
}
try(gamsSysDir <- paste0(getCommandArg("gamsSysDir"), .Platform$file.sep), silent = TRUE)
if(identical(gamsSysDir, "") || !dir.exists(file.path(gamsSysDir, "GMSR", "library"))){
  
  RLibPath = NULL
  
}else{
  RLibPath = file.path(gamsSysDir, "GMSR", "library")
  assign(".lib.loc", RLibPath, envir = environment(.libPaths))
}

installedPackages <- installed.packages(lib.loc = RLibPath)[, "Package"]
useGdx <- FALSE
if("gdxrrw" %in% installedPackages){
  useGdx <- TRUE
  requiredPackages <- c(requiredPackages, "gdxrrw")
}
# vector of required files
filesToInclude <- c("./global.R", "./R/util.R", if(useGdx) "./R/gdxio.R", "./R/json.R", "./R/output_load.R", 
                    "./R/data_instance.R", "./R/worker.R", "./R/dataio.R", "./R/hcube_data_instance.R", "./R/miro_tabsetpanel.R",
                    "./modules/render_data.R", "./modules/generate_data.R")
LAUNCHADMINMODE <- FALSE
if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
  pb <- winProgressBar(title = "Loading GAMS MIRO", label = "Loading required packages",
                       min = 0, max = 1, initial = 0, width = 300)
  setWinProgressBar(pb, 0.1)
}else{
  pb <- txtProgressBar(file = stderr())
}
source("./R/install_packages.R", local = TRUE)
if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
  setWinProgressBar(pb, 0.3, label= "Initialising GAMS MIRO")
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
  # check whether shiny proxy is used to access this file
  isShinyProxy <- isShinyProxy()
  # get model path and name
  tryCatch({
    modelPath <- paste0(getwd(), .Platform$file.sep, modelDir, modelName, 
                        .Platform$file.sep, modelName, ".gms")
    modelPath    <- getModelPath(modelPath, isShinyProxy, spModelPathEnvVar, 
                                 paste0(getwd(), .Platform$file.sep, modelDir))
    modelNameRaw <- modelPath[[4]]
    modelName    <- modelPath[[3]]
    modelGmsName <- modelPath[[2]]
    modelPath    <- modelPath[[1]]
  }, error = function(e){
    errMsg <<- paste(errMsg,
                     "The GAMS model name could not be identified. Please make sure you specify the name of the model you want to solve.",
                     sep = "\n")
  })
}

if(is.null(errMsg)){
  # check if GAMS model file exists
  if(file.exists(modelPath %+% modelGmsName)){
    currentModelDir  <- modelPath
  }else{
    errMsg <- sprintf("The GAMS model file: '%s' could not be found in the directory: '%s'." %+%
"Please make sure you specify a valid gms file path.", modelGmsName, modelPath)
  }
}

if(is.null(errMsg)){
  if(isShinyProxy){
    logFileDir <- file.path(tmpFileDir, logFileDir)
  }else{
    logFileDir <- file.path(currentModelDir, logFileDir)
    # initialise MIRO workspace
    miroWorkspace <- file.path(path.expand("~"), miroWorkspaceDir)
    if(!dir.exists(miroWorkspace)){
      if(!dir.create(miroWorkspace, showWarnings = FALSE)[1]){
        errMsg <- paste(errMsg, sprintf("Could not create MIRO workspace directory: '%s'. Please make sure you have sufficient permissions. '", 
                                        miroWorkspace), sep = "\n")
      }
      if(isWindows()){
        tryCatch(
          processx::run("attrib", args = c("+h", miroWorkspace))
        , error = function(e){
          errMsg <- paste(errMsg, sprintf("Failed to hide MIRO workspace directory: '%s'. Error message: '%s'.", 
                                          miroWorkspace, conditionMessage(e)), sep = "\n")
        })
        
      }
    }
  }
  # name of the R save file
  rSaveFilePath <- paste0(currentModelDir, modelName, '_', MIROVersion, 
                          if(identical(tolower(Sys.getenv(spModelModeEnvVar)), "hcube") ||
                             "LAUNCHHCUBE" %in% commandArgs(TRUE)) "_hcube",
                          '.miroconf')
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
  }else{
    if(length(uid) != 1 || !is.character(uid)){
      errMsg <- "Invalid user ID specified."
    }
    if(!length(ugroups)){
      ugroups <- defaultGroup
    }
  }
  if(!identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true") && 
     any(!grepl("^[a-zA-Z0-9][a-zA-Z0-9!%\\(\\)\\-~]{3,19}$", c(uid, ugroups), perl = TRUE))){
    errMsg <- paste(errMsg, 
                    "Invalid user ID or user group specified. The following rules apply for user IDs and groups:\n- must be at least 4 and not more than 20 characters long\n- must start with a number or letter (upper or lowercase) {a-z}, {A-Z}, {0-9}\n- may container numbers, letters and the following additional characters: {!%()-~}",
                    sep = "\n")
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
if(is.null(errMsg)){
  flog.appender(do.call(if(identical(logToConsole, TRUE)) "appender.tee" else "appender.false", 
                        list(file = file.path(logFileDir, paste0(modelName, "_", uid, "_", 
                                              format(Sys.time(), "%y.%m.%d_%H.%M.%S"), ".log")))))
  flog.threshold(loggingLevel)
  flog.trace("Logging facility initialised.")
  if(isShinyProxy || "NODEBUG" %in% commandArgs(TRUE)){
    debugMode <- FALSE
  }
  if(identical(tolower(Sys.getenv(spModelModeEnvVar)), "admin") ||
     "LAUNCHADMIN" %in% commandArgs(TRUE)){
    LAUNCHADMINMODE <- TRUE
  }
  if(!file.exists(rSaveFilePath) || debugMode){
    source("./modules/init.R", local = TRUE)
  }else{
    load(rSaveFilePath, envir = .GlobalEnv)
    if(isShinyProxy){
      dbConfig <- setDbConfig(paste0(configDir, "db_config.json"))
      config$activateModules$remoteExecution <- TRUE
      if(length(dbConfig$errMsg)){
        errMsg <- dbConfig$errMsg
      }else{
        config$db <- dbConfig$data
      }
    }
  }
}
if(is.null(errMsg)){
  # load default and custom renderers (output data)
  requiredPackages <- c(if(identical(installPackage$plotly, TRUE)) "plotly",
                        if(identical(installPackage$dygraphs, TRUE)) c("xts", "dygraphs"),
                        if(identical(installPackage$leaflet, TRUE)) c("leaflet", "leaflet.minicharts"),
                        if(identical(installPackage$timevis, TRUE)) c("timevis"))
  if(identical(installPackage$DT, TRUE) || ("DT" %in% installedPackages)){
    requiredPackages <- c(requiredPackages, "DT")
  }
  if(config$activateModules$remoteExecution){
    requiredPackages <- c(requiredPackages, "future", "httr")
  }else if(length(externalInputConfig) || length(datasetsRemoteExport)){
    requiredPackages <- c(requiredPackages, "httr")
  }
  source("./R/install_packages.R", local = TRUE)
  options("DT.TOJSON_ARGS" = list(na = "string"))
  
  if(config$activateModules$remoteExecution && identical(LAUNCHADMINMODE, FALSE)){
    plan(multiprocess)
  }
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
  if(debugMode){
    customRendererDirs <<- paste0(c(paste0(currentModelDir, "..", .Platform$file.sep),
                                    currentModelDir), customRendererDirName, .Platform$file.sep)
    for(customRendererDir in customRendererDirs){
      rendererFiles <- list.files(customRendererDir, pattern = "\\.R$")
      lapply(rendererFiles, function(file){
        if(!file.access(customRendererDir %+% file, mode = 4)){
          tryCatch({
            source(customRendererDir %+% file)
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
    for(customRendererConfig in c(configGraphsOut, configGraphsIn)){
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
    requiredPackagesCR <- unique(requiredPackagesCR)
    if(!is.null(requiredPackagesCR)){
      requiredPackages <- requiredPackagesCR
      source("./R/install_packages.R", local = TRUE)
    }
  }else if(!is.null(requiredPackagesCR)){
    requiredPackages <- requiredPackagesCR
    source("./R/install_packages.R", local = TRUE)
    rm(requiredPackagesCR)
  }
}
if(is.null(errMsg)){ 
  # try to create the DB connection (PostgreSQL)
  if(config$activateModules$scenario){
    if(identical(tolower(config$db$type), "sqlite")){
      requiredPackages <- c("DBI", "RSQLite")
    }else{
      requiredPackages <- c("DBI", "odbc")
    }
    source("./R/install_packages.R", local = TRUE)
    
    source("./R/db.R")
    source("./R/db_scen.R")
    tryCatch({
      scenMetadataTable <- scenMetadataTablePrefix %+% modelName
      db   <- Db$new(uid = uid, dbConf = config$db, dbSchema = dbSchema,
                     slocktimeLimit = slocktimeLimit, modelName = modelName,
                     attachmentConfig = if(config$activateModules$attachments) 
                       list(maxSize = attachMaxFileSize, maxNo = attachMaxNo)
                     else NULL,
                     hcubeActive = config$activateModules$hcubeMode)
      conn <- db$getConn()
      flog.debug("Database connection established.")
    }, error = function(e){
      flog.error("Problems initialising database class. Error message: %s", e)
      errMsg <<- conditionMessage(e)
    })
    # initialise access management
    source("./R/db_auth.R")
    tryCatch({
      auth <- Auth$new(conn, uid, defaultGroup = defaultGroup, 
                       tableNameGroups = amTableNameGroups, 
                       tableNameElements = amTableNameElements, 
                       tableNameHierarchy = amTableNameHierarchy, 
                       tableNameMetadata = scenMetadataTable, 
                       uidIdentifier = uidIdentifier, 
                       accessIdentifier = accessIdentifier, 
                       accessElIdentifier = accessElIdentifier)
      db$accessGroups <- auth$getAccessGroups()
      flog.debug("Access Control initialised.")
    }, error = function(e){
      flog.error("Problems initialising authorisation class. Error message: %s", e)
      errMsg <<- paste(errMsg, conditionMessage(e), sep = '\n')
    })
  }
  dataio <- DataIO$new(config = list(modelIn = modelIn, modelOut = modelOut, 
                                     modelName = modelName),
                       db = db, auth = auth)
  if(config$activateModules$hcubeMode){
    hcubeDirName <<- paste0(modelName, "_", hcubeDirName)
    requiredPackages <- c("digest", "DT")
    source("./R/install_packages.R", local = TRUE)
    source("./R/hcube.R")
    source("./R/db_hcubeimport.R")
    source("./R/db_hcubeload.R")
  }
}
showRemoveDbTablesBtn <- FALSE
if(is.null(errMsg) && debugMode && config$activateModules$scenario && identical(LAUNCHADMINMODE, FALSE)){
  # checking database inconsistencies
  local({
    orphanedTables <- NULL
    tryCatch({
      orphanedTables <- db$getOrphanedTables(hcubeScalars = names(modelIn)[vapply(seq_along(modelIn), 
                                                                                  function(i) 
                                                                                    identical(modelIn[[i]]$type, "dropdown"), 
                                                                                 logical(1L), USE.NAMES = FALSE)])
    }, error = function(e){
      flog.error("Problems fetching orphaned database tables. Error message: '%s'.", e)
      errMsg <<- paste(errMsg, sprintf("Problems fetching orphaned database tables. Error message: '%s'.", 
                                       conditionMessage(e)), sep = '\n')
    })
    if(length(orphanedTables)){
      msg <- sprintf("There are orphaned tables in your database: '%s'.\n
This could be caused because you used a different database schema in the past (e.g. due to different inputs and/or outputs).",
                     paste(orphanedTables, collapse = "', '"))
      flog.warn(msg)
    }
    inconsistentTables <- NULL
    
    tryCatch({
      inconsistentTables <- db$getInconsistentTables(strictMode = config$activateModules$strictmode)
    }, error = function(e){
      flog.error("Problems fetching database tables (for inconsistency checks).\nDetails: '%s'.", e)
      errMsg <<- paste(errMsg, sprintf("Problems fetching database tables (for inconsistency checks). Error message: '%s'.", 
                                       conditionMessage(e)), sep = '\n')
    })
    if(length(inconsistentTables$names)){
      showRemoveDbTablesBtn <<- TRUE
      flog.error(sprintf("There are tables in your database that do not match the current database schema of your model.\n
Those tables are: '%s'.\nError message: '%s'.",
                         paste(inconsistentTables$names, collapse = "', '"), inconsistentTables$errMsg))
      msg <- paste(errMsg, sprintf("There are tables in your database that do not match the current database schema of your model.\n
Those tables are: '%s'.\nError message: '%s'.",
                                   paste(inconsistentTables$names, collapse = "', '"), inconsistentTables$errMsg),
                   collapse = "\n")
      if(config$activateModules$strictmode || length(inconsistentTables$errMsg)){
        errMsg <<- paste(errMsg, msg, sep = "\n")
      }else{
        for(i in seq_along(inconsistentTables$headers)){
          tabName <- names(inconsistentTables$headers)[i]
          if(is.null(inconsistentTables$headers[[i]])){
            next
          }
          if(!is.null(names(modelIn[[tabName]]$headers))){
            names(modelIn[[tabName]]$headers) <<- inconsistentTables$headers[[tabName]]
          }else if(!identical(tabName, scalarsFileName)){
            names(modelOut[[tabName]]$headers) <<- inconsistentTables$headers[[tabName]]
          }
        }
      }
    }
  })
}

MIROVersionLatest <- NULL
if(is.null(errMsg) && !isShinyProxy && curl::has_internet()){
  try(
    local({
      verCon <- url("https://gams.com/miro/latest.ver")
      on.exit(close(verCon))
      MIROVersionLatestTmp <- suppressWarnings(read.csv(
        verCon, 
        header = FALSE))
      currentMIROVersion <- strsplit(MIROVersion, ".", fixed = TRUE)[[1]]
      if(MIROVersionLatestTmp[[1]][1] > currentMIROVersion[1] ||
         (MIROVersionLatestTmp[[1]][1] == currentMIROVersion[1] && 
          MIROVersionLatestTmp[[2]][1] > currentMIROVersion[2]) ||
         (MIROVersionLatestTmp[[1]][1] == currentMIROVersion[1] && 
          MIROVersionLatestTmp[[2]][1] == currentMIROVersion[2] && 
          MIROVersionLatestTmp[[3]][1] > currentMIROVersion[3])){
        MIROVersionLatest <<- paste0("<br/><br/><b style=\\'color:#f90;\\'>A new version of GAMS MIRO is available! The latest version is: v.",
                                    MIROVersionLatestTmp[[1]][1], ".",
                                    MIROVersionLatestTmp[[2]][1], ".",
                                    MIROVersionLatestTmp[[3]][1], 
                                    "</b><br/>To download the latest version, click <a href=\\'https://gams.com/miro/\\' target=\\'_blank\\'>here</a>")
        
        
      }
    })
  , silent = TRUE)
}

aboutDialogText <- paste0("<b>GAMS MIRO v.", MIROVersion, "</b><br/><br/>",
                          "Release Date: ", MIRORDate, "<br/>", 
                          "Copyright (c) 2019 GAMS Software GmbH <support@gams.com><br/>",
                          "Copyright (c) 2019 GAMS Development Corp. <support@gams.com><br/><br/>",
                          "This program is free software: you can redistribute it and/or modify ",
                          "it under the terms of the GNU General Public License as published by ",
                          "the Free Software Foundation, either version 3 of the License, or ",
                          "(at your option) any later version.<br/><br/>",
                          "This program is distributed in the hope that it will be useful, ", 
                          "but WITHOUT ANY WARRANTY; without even the implied warranty of ",
                          "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ",
                          "GNU General Public License for more details.<br/><br/>",
                          "You should have received a copy of the GNU General Public License ",
                          "along with this program. If not, see ",
                          "<a href=\\'http://www.gnu.org/licenses/\\' target=\\'_blank\\'>http://www.gnu.org/licenses/</a>.",
                          MIROVersionLatest)
if(is.null(errMsg)){
  tryCatch({
    if(useGdx){
      gdxio <<- GdxIO$new(gamsSysDir, c(modelInRaw, modelOut))
    }
  }, error = function(e){
    flog.error(e)
    errMsg <<- paste(errMsg, e, sep = '\n')
  })
  remoteUser <- uid
  modelData <- modelFiles
  rememberMeFileName <- paste0(miroWorkspace, .Platform$file.sep, ".cred_", 
                               modelName)
  credConfig <- NULL
  if(isShinyProxy){
    credConfig <- list(url = Sys.getenv("MIRO_GAMS_HOST"), 
                       username = uid,
                       password = "",
                       namespace = "global",
                       useRegistered = TRUE)
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
  
  worker <- Worker$new(metadata = list(uid = uid, modelName = modelName, noNeedCred = isShinyProxy,
                                       tableNameTracePrefix = tableNameTracePrefix, maxSizeToRead = maxSizeToRead,
                                       text_entities = c(paste0(modelName, ".lst"), 
                                                         if(config$activateModules$miroLogFile) config$miroLogFile),
                                       currentModelDir = currentModelDir, gamsExecMode = gamsExecMode,
                                       MIROSwitch = config$MIROSwitch, extraClArgs = config$extraClArgs, 
                                       includeParentDir = config$includeParentDir, saveTraceFile = config$saveTraceFile,
                                       modelGmsName = modelGmsName, gamsSysDir = gamsSysDir, csvDelim = config$csvDelim,
                                       timeout = 8L, serverOS = getOS(), modelData = modelData, hcubeMode = config$activateModules$hcubeMode,
                                       rememberMeFileName = rememberMeFileName, includeParentDir = config$includeParentDir), 
                       remote = config$activateModules$remoteExecution,
                       hcube = config$activateModules$hcubeMode,
                       db = db)
  if(length(credConfig)){
    do.call(worker$setCredentials, credConfig)
  }
}
if(!is.null(errMsg)){
  if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
    setWinProgressBar(pb, 1, label= "GAMS MIRO initialised")
  }else{
    setTxtProgressBar(pb, 1)
  }
  close(pb)
  pb <- NULL
  ui_initError <- fluidPage(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = "miro.css"),
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
             if(identical(isShinyProxy, FALSE) && identical(showRemoveDbTablesBtn, TRUE)){
               actionButton("removeDbTablesPre", lang$adminMode$database$remove)
               tagList(
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
             tableOutput("JSONErrorMessages")
    )
  )
  server_initError <- function(input, output, session){
    if(identical(isShinyProxy, FALSE) && identical(showRemoveDbTablesBtn, TRUE)){
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
      observeEvent(input$removeDbTablesPre, {
        showModal(modalDialog(title = removeDbTabLang$title,
                              removeDbTabLang$desc, footer = tagList(
                      modalButton(removeDbTabLang$cancel),
                      actionButton("removeDbTables", label = removeDbTabLang$confirm, 
                                   class = "bt-highlight-1"))))
      })
      source(file.path('tools', 'admin', 'db_management.R'), local = TRUE)
    }
    output$errorMessages <- renderText(
      errMsg
    )
    output$JSONErrorMessages <- renderTable(
      if(exists("jsonErrors")) jsonErrors, bordered = TRUE
    )
    session$onSessionEnded(function() {
      if(!interactive()){
        stopApp()
      }
    })
  }
  
  shinyApp(ui = ui_initError, server = server_initError)
}else if(identical(LAUNCHADMINMODE, TRUE)){
  if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
    setWinProgressBar(pb, 1, label= "GAMS MIRO initialised")
  }else{
    setTxtProgressBar(pb, 1)
  }
  close(pb)
  pb <- NULL
  
  source("./tools/admin/server.R", local = TRUE)
  source("./tools/admin/ui.R", local = TRUE)
  shinyApp(ui = ui_admin, server = server_admin)
}else{
  if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
    setWinProgressBar(pb, 0.6, label= "Importing new data")
  }
  rm(LAUNCHADMINMODE, installedPackages)
  if(debugMode){
    save(list = c(listOfCustomRenderers$get(), "modelIn", "modelInRaw", 
                  "modelOut", "config", "lang", "inputDsNames", "outputTabs", 
                  "outputTabTitles", "modelInTemplate", "scenDataTemplate", 
                  "modelInTabularData", "externalInputConfig",
                  "modelInFileNames", "ddownDep", "aliasesNoDep", "idsIn",
                  "choicesNoDep", "sliderValues", "configGraphsOut", 
                  "configGraphsIn", "hotOptions", "inputTabs", "inputTabTitles", 
                  "scenInputTabs", "scenInputTabTitles", "isGroupOfSheets", 
                  "groupSheetToTabIdMap", "scalarsInTemplate", "modelInWithDep",
                  "modelOutAlias", "colsWithDep", "scalarsInMetaData",
                  "modelInMustImport", "modelInAlias", "DDPar", "GMSOpt", 
                  "currentModelDir", "modelInToImportAlias", "modelInToImport", 
                  "scenTableNames", "modelOutTemplate", "scenTableNamesToDisplay", 
                  "GAMSReturnCodeMap", "dependentDatasets",
                  "modelInGmsString", "installPackage", "dbSchema", "scalarInputSym",
                  "requiredPackagesCR", "modelFiles"), 
         file = rSaveFilePath)
    rm(listOfCustomRenderers)
    if(identical(getCommandArg("buildonly", FALSE), "true")){
      quit("no")
    }
  }
  local({
    miroDataDir   <- file.path(currentModelDir, paste0(miroDataDirPrefix, modelName))
    miroDataFiles <- list.files(miroDataDir)
    dataFileExt   <- tolower(tools::file_ext(miroDataFiles))
    miroDataFiles <- miroDataFiles[dataFileExt %in% c(if(useGdx) "gdx", "xlsx", "xls")]
    newScen <- NULL
    tryCatch({
      if(length(miroDataFiles)){
        modelInTabularDataNoScalar <- modelInTabularData[!modelInTabularData %in% scalarsFileName]
        dataModelIn <- modelIn[names(modelIn) %in% modelInTabularDataNoScalar]
        modelInTemplateTmp <- modelInTemplate[!vapply(modelInTemplate, is.null, logical(1L), USE.NAMES = FALSE)]
        if(scalarsFileName %in% names(modelInRaw)){
          dataModelIn <- c(dataModelIn, modelInRaw[scalarsFileName])
          modelInTemplateTmp <- c(modelInTemplateTmp, list(scalarsInTemplate))
        }
        for(i in seq_along(miroDataFiles)){
          miroDataFile <- miroDataFiles[i]
          flog.info("New data: '%s' is being stored in the database. Please wait a until the import is finished.", miroDataFile)
          if(dataFileExt[i] %in% c("xls", "xlsx")){
            method <- "xls"
          }else{
            method <- dataFileExt[i]
          }
          newScen <- Scenario$new(db = db, sname = gsub("\\.[^\\.]*$", "", miroDataFile))
          dataOut <- loadScenData(scalarsOutName, modelOut, miroDataDir, modelName, scalarsFileHeaders,
                                  modelOutTemplate, method = method, fileName = miroDataFile)$tabular
          dataIn  <- loadScenData(scalarsFileName, dataModelIn, miroDataDir, modelName, scalarsFileHeaders,
                                  modelInTemplateTmp, method = method, fileName = miroDataFile)$tabular
          
          if(!scalarsFileName %in% names(modelInRaw) && length(scalarInputSym)){
            # additional command line parameters that are not GAMS symbols
            scalarsTemplate <- tibble(a = character(0L), b = character(0L), c = character(0L))
            names(scalarsTemplate) <- scalarsFileHeaders
            newScen$save(c(dataOut, dataIn, list(scalarsTemplate)))
          }else{
            newScen$save(c(dataOut, dataIn))
          }
          
          if(!file.remove(file.path(miroDataDir, miroDataFile))){
            flog.info("Could not remove file: '%s'.", miroDataFile)
          }
        }
      }
    }, error = function(e){
      flog.error("Problems saving MIRO data to database. Error message: '%s'.", e)
      rm(newScen)
      gc()
    })
  })
  if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
    setWinProgressBar(pb, 1, label= "GAMS MIRO initialised")
  }else{
    setTxtProgressBar(pb, 1)
  }
  close(pb)
  pb <- NULL
  interruptShutdown <<- FALSE
  #______________________________________________________
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #                   Server
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #______________________________________________________
  server <- function(input, output, session){
    newTab <- vector("list", maxNumberScenarios + 3L)
    flog.info("Session started (model: '%s', user: '%s').", modelName, uid)
    btSortNameDesc     <- FALSE
    btSortTimeDesc     <- TRUE
    btSortTime         <- TRUE
    btSortNameDescBase <- FALSE
    btSortTimeDescBase <- TRUE
    btSortTimeBase     <- TRUE
    interruptShutdown  <<- TRUE
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
    activeScen         <- NULL
    exportFileType     <- if(useGdx) "gdx" else "xls"
    
    # scenId of tabs that are loaded in ui (used for shortcuts) (in correct order)
    sidCompOrder     <- NULL
    if(config$activateModules$scenario){
      scenMetaData     <- list()
      # scenario metadata of scenario saved in database
      scenMetaDb       <- NULL
      scenMetaDbBase   <- NULL
      scenMetaDbBaseList <- NULL
      # temporary name and sid of the scenario currently active in the UI
      activeSnameTmp   <- NULL
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
    }
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
        currentGroup <- as.numeric(gsub("\\D", "", isolate(input$contentCurrent)))
        if(shortcutNest && length(outputTabs[[currentGroup]]) > 1L){
          currentSheet <- as.integer(strsplit(isolate(input[[paste0("contentCurrent", 
                                                                    currentGroup)]]), "_")[[1]][2])
          updateTabsetPanel(session, paste0("contentCurrent", currentGroup), 
                            paste0("contentCurrent", 
                                   currentGroup, "_", currentSheet + direction))
        }else{
          updateTabsetPanel(session, "contentCurrent", 
                            paste0("contentCurrent_", currentGroup + direction))
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
          tabsetName <- isolate(input$tabs_paver_results)
          currentSheet <- suppressWarnings(as.numeric(substr(tabsetName, 
                                                             nchar(tabsetName), nchar(tabsetName))))
          if(is.na(currentSheet))
            return()
          updateTabsetPanel(session, "tabs_paver_results", 
                            paste0("tabs_paver_", currentSheet + direction))
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
    workDir <- paste0(tmpFileDir, .Platform$file.sep, session$token, .Platform$file.sep)
    worker$setWorkDir(workDir)
    if(!dir.create(file.path(workDir), recursive = TRUE)){
      flog.fatal("Working directory could not be initialised.")
      showErrorMsg(lang$errMsg$fileWrite$title, lang$errMsg$fileWrite$desc)
      return(NULL)
    }else{
      flog.debug("Working directory was created: '%s'.", workDir)
    }
    # initialization of several variables
    rv <- reactiveValues(scenId = 4L, unsavedFlag = TRUE, btLoadScen = 0L, btOverwriteScen = 0L, btSolve = 0L,
                         btOverwriteInput = 0L, btSaveAs = 0L, btSaveConfirm = 0L, btRemoveOutputData = 0L, 
                         btLoadLocal = 0L, btCompareScen = 0L, activeSname = NULL, clear = TRUE, btSave = 0L, 
                         btSplitView = 0L, noInvalidData = 0L, uploadHcube = 0L, 
                         loadHcubeHashSid = 0L, jobListPanel = 0L, importJobConfirm = 0L, importJobNew = 0L,
                         datasetsModified = vector(mode = "logical", length = length(modelIn)))
    # list of scenario IDs to load
    sidsToLoad <- list()
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
        if(!identical(defSid, 0L)){
          sidsToLoad <- list(defSid)
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
      if((config$activateModules$scenario || config$activateModules$hcubeMode)
          && input$sidebarMenuId == "scenarios"){
        isInSolveMode <<- FALSE
      }else if(identical(input$sidebarMenuId, "importData")){
        rv$jobListPanel <- rv$jobListPanel + 1L
      }
    })
    
    # activate solve button when all datasets that have to be 
    # imported are actually imported
    lapply(seq_along(modelIn), function(i){
      if(modelIn[[i]]$type == "hot"){
        observeEvent(input[["in_" %+% i %+% "_select"]], {
          hotInit[[i]] <<- TRUE
          isEmptyInput[[i]] <<- FALSE
          if(noCheck[i]){
            noCheck[i] <<- FALSE
          }
        })
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
        isolate(rv$datasetsModified[i] <- TRUE)
        # if scenario includes output data set dirty flag
        if(!noOutputData){
          dirtyFlag <<- TRUE
          showEl(session, "#dirtyFlagIcon")
          showEl(session, "#dirtyFlagIconO")
        }
        rv$unsavedFlag <<- TRUE
      }, priority = 1000L)
    })
    
    markUnsaved <- function(){
      hideEl(session, "#dirtyFlagIcon")
      hideEl(session, "#dirtyFlagIconO")
      dirtyFlag     <<- FALSE
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
    getScenTitle <- reactive(
      if(is.null(rv$activeSname)){
        return("")
      }else{
        nameSuffix <- ""
        if(rv$unsavedFlag){
          nameSuffix <- " (*)"
        }
        return(paste0(htmltools::htmlEscape(rv$activeSname), nameSuffix))
      }
    )
    output$inputDataTitle <- renderUI(htmltools::htmlEscape(getScenTitle()))
    output$outputDataTitle <- renderUI(htmltools::htmlEscape(getScenTitle()))
    
    # activate solve button once all model input files are imported
    observe({
      datasetsImported <- vapply(names(modelInMustImport), function(el){
        i <- match(el, names(modelIn))[[1]]
        if(length(rv[["in_" %+% i]])){
          return(TRUE)
        }else if(rv$datasetsModified[i]){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }, logical(1), USE.NAMES = FALSE)
      if(all(datasetsImported)){
        addClassEl(session, "#btSolve", "glow-animation")
        enableEl(session, "#btSolve")
      }else{
        removeClassEl(session, "#btSolve", "glow-animation")
        disableEl(session, "#btSolve")
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
    
    ####### Advanced options
    source("./modules/download_tmp.R", local = TRUE)
    
    ####### Paver interaction
    if(config$activateModules$hcubeMode){
      source("./modules/gams_job_list.R", local = TRUE)
      ####### Hcube import module
      source("./modules/hcube_import.R", local = TRUE)
      ####### Hcube load module
      source("./modules/hcube_load.R", local = TRUE)
      # analyze button clicked
      source("./modules/paver_run.R", local = TRUE)
      # Interrupt button clicked
      source("./modules/paver_interrupt.R", local = TRUE)
    }else if(config$activateModules$remoteExecution){
      source("./modules/gams_job_list.R", local = TRUE)
      # remote job import
      source("./modules/job_import.R", local = TRUE)
    }
    
    # delete scenario 
    source("./modules/db_scen_remove.R", local = TRUE)
    # scenario module
    if(config$activateModules$scenario){
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
      
      lapply(seq_len(maxNumberScenarios  + 3L), function(i){
        scenIdLong <- paste0("scen_", i, "_")
        # table view
        source("./modules/scen_table_view.R", local = TRUE)
        
        # close scenario tab
        source("./modules/scen_close.R", local = TRUE)
        
        # export Scenario to Excel spreadsheet
        source("./modules/scen_export.R", local = TRUE)
        
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
      if(!isShinyProxy){
        # switch to Hypercube mode
        hcubeProcess <- NULL
        observeEvent(input$switchToHcube, {
          flog.debug("Switch to Hypercube mode button clicked.")
          if(!is.null(hcubeProcess)){
            if(hcubeProcess$is_alive()){
              flog.debug("Hypercube mode already running.")
              showHideEl(session, "#hcubeRunning", 4000L)
              return()
            }
            if(!identical(hcubeProcess$get_exit_status(), 0L)){
              flog.error("Problems launching Hypercube mode. Error message: '%s'.", 
                         hcubeProcess$read_error())
              showHideEl(session, "#hcubeLaunchError", 4000L)
              return()
            }
          }
         
          hcubeProcess <<- process$new(file.path(R.home("bin"), "RScript"), 
                                       c("--vanilla", file.path(currentModelDir, "runApp.R"), 
                                         "LAUNCHHCUBE", commandArgs(TRUE)), stderr = "|")
        })
      }
    }else{
      id <- 1
      # export output data to Excel spreadsheet
      source("./modules/scen_export.R", local = TRUE)
    }
    observeEvent(input$btExportScen, {
      exportTypes <- c(if(useGdx) "gdx", "xls")
      if(length(datasetsRemoteExport)){
        exportTypes <- c(exportTypes, names(datasetsRemoteExport))
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
             flog.warn("Unknown export file type: '%s'.", input$exportFileType))
    })
    if(!isShinyProxy && 
       curl::has_internet() && 
       file.exists(file.path(currentModelDir, ".crash.zip"))){
      showModal(modalDialog(title = "Unexpected behavior",
                            paste0("MIRO discovered it was behaving unexpectedly. We are constantly striving to improve MIRO.
                            Would you like to send the error report to GAMS in order to avoid such crashes in the future?
                            The ONLY files that we send (encrypted via HTTPS) are the configuration files: '", modelName, 
                            "_io.json' and '", modelName, ".json' as well as the error log. 
                            None of your .gms model files will be sent!"), 
                            footer = tagList(actionButton("crash_dontsend", "Don't send"),
                                             actionButton("crash_send", "Send", class = "bt-highlight-1"))))
      observeEvent(input$crash_dontsend, {
        unlink(file.path(currentModelDir, ".crash.zip"))
        removeModal()
      })
      observeEvent(input$crash_send, {
        on.exit(unlink(file.path(currentModelDir, ".crash.zip")))
        try(
          uploadFile(
            file = file.path(currentModelDir, ".crash.zip"), 
            url = paste0(bugReportUrl$url, format(Sys.time(), "%y.%m.%d_%H.%M.%S"), 
                         "_", substr(modelName, 1L, 3L), ".zip"), 
            userpwd = paste0(bugReportUrl$dir, ":")
          )
        , silent = TRUE)
        
        removeModal()
      })
    }
    hideEl(session, "#loading-screen")
    # This code will be run after the client has disconnected
    session$onSessionEnded(function() {
      # remove temporary files and folders
      unlink(file.path(workDir), recursive=TRUE)
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
      
      if(!interactive()){
        if(isShinyProxy){
          interruptShutdown <<- FALSE
          promises::then(future(Sys.sleep(SERVER_SHUTDOWN_DELAY)),
                         function(val) {
                           if(identical(interruptShutdown, FALSE)){
                             stopApp()
                           }}, function(val){
                             stopApp()
                           })
        }else{
          stopApp()
        }
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