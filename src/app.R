# version number
MIROVersion <- "2.2.2"
APIVersion <- "1"
MIRORDate <- "Jan 05 2021"

# specify CRAN mirror
CRANMirror <- "https://cloud.r-project.org/"
errMsg <- NULL
warningMsg <- NULL
loggerInitialised <- FALSE

isShinyProxy <<- !identical(Sys.getenv("SHINYPROXY_USERNAME"), "")
debugMode <- TRUE
RLibPath <- NULL
miroBuildOnly <- identical(Sys.getenv("MIRO_BUILD"), "true")
miroStoreDataOnly <- identical(Sys.getenv("MIRO_POPULATE_DB"), "true")
miroDeploy <- miroBuildOnly
if (identical(Sys.getenv("MIRO_TEST_DEPLOY"), "true")) {
  miroDeploy <- TRUE
  miroBuildOnly <- FALSE
}
logToConsole <- TRUE
if (identical(Sys.getenv("MIRO_NO_DEBUG"), "true") && !miroDeploy) {
  debugMode <- FALSE
  logToConsole <- FALSE
} else if (isShinyProxy) {
  debugMode <- FALSE
}
tmpFileDir <- tempdir(check = TRUE)
# required packages
requiredPackages <- c("R6", "jsonlite", "zip", "tibble", "readr", "futile.logger")
if (!miroBuildOnly) {
  requiredPackages <- c(
    requiredPackages, "shiny", "shinydashboard", "rhandsontable",
    "rpivotTable", "stringi", "processx",
    "dplyr", "readxl", "writexl", "tidyr",
    "DT", "sortable", "chartjs"
  )
}
config <- list()
modelFiles <- character()
gamsSysDir <- Sys.getenv("GAMS_SYS_DIR")

installedPackages <<- installed.packages()[, "Package"]
useGdx <<- FALSE
if ("gdxrrwMIRO" %in% installedPackages) {
  useGdx <<- TRUE
  requiredPackages <- c(requiredPackages, "gdxrrwMIRO")
}
source("./components/install_packages.R")
errMsg <- installAndRequirePackages(requiredPackages, installedPackages, RLibPath, CRANMirror, miroWorkspace, TRUE)
installedPackages <<- installed.packages()[, "Package"]
# vector of required files
filesToInclude <- c(
  "./global.R", "./components/util.R", if (useGdx) "./components/gdxio.R",
  "./components/json.R", "./components/scenario_metadata.R", "./components/views.R",
  "./components/attachments.R", "./components/miroscenio.R",
  "./components/load_scen_data.R", "./components/localfileio.R",
  "./components/xlsio.R", "./components/csvio.R",
  "./components/input_data_instance.R", "./components/worker.R",
  "./components/dataio.R", "./components/scen_export.R",
  "./components/miro_tabsetpanel.R", "./modules/render_data.R",
  "./modules/generate_data.R", "./components/script_output.R",
  "./components/js_util.R", "./components/scen_data.R", "./components/batch_loader.R"
)
LAUNCHCONFIGMODE <- FALSE
if (is.null(errMsg)) {
  # include custom functions and modules
  lapply(filesToInclude, function(file) {
    if (!file.exists(file)) {
      errMsg <<- paste(errMsg, paste0("Include file '", file, "' could not be located."), sep = "\n")
    } else {
      tryCatch(
        {
          source(file)
        },
        error = function(e) {
          errMsg <<- paste(errMsg, paste0(
            "Some error occurred while sourcing file '",
            file, "'. Error message: ",
            conditionMessage(e)
          ), sep = "\n")
        },
        warning = function(w) {
          errMsg <<- paste(errMsg, paste0(
            "Some error occurred while sourcing file '",
            file, "'. Error message: ", w
          ), sep = "\n")
        }
      )
    }
  })
  # set maximum upload size
  options(shiny.maxRequestSize = maxUploadSize * 1024^2)
  # get model path and name
  modelPath <<- getModelPath(modelPath, "MIRO_MODEL_PATH")
  modelNameRaw <<- modelPath[[4]]
  modelName <<- modelPath[[3]]
  modelName <<- modelName
  modelGmsName <<- modelPath[[2]]
  modelPath <<- modelPath[[1]]
}

if (is.null(errMsg)) {
  miroWorkspace <- NULL
  miroDbDir <- NULL

  if (isShinyProxy) {
    miroWorkspace <- file.path(getwd(), "ws")
  } else {
    # initialise MIRO workspace
    miroWorkspace <- Sys.getenv("MIRO_WS_PATH")
    if (identical(miroWorkspace, "")) {
      miroWorkspace <- file.path(path.expand("~"), miroWorkspaceDir)
    }
    miroDbDir <- Sys.getenv("MIRO_DB_PATH")
    if (identical(miroDbDir, "")) {
      miroDbDir <- file.path(miroWorkspace, "app_data")
    }
  }
  if (!identical(Sys.getenv("MIRO_LOG_PATH"), "")) {
    logFileDir <- Sys.getenv("MIRO_LOG_PATH")
  } else if (isShinyProxy) {
    logFileDir <- file.path(tmpFileDir, logFileDir)
  } else {
    logFileDir <- file.path(miroWorkspace, logFileDir)
  }
  # set user ID (user name) and user groups
  ugroups <- NULL
  if (isShinyProxy) {
    uid <- Sys.getenv("SHINYPROXY_USERNAME")
    if (is.null(uid) || grepl("^\\s*$", uid)) {
      errMsg <- "No user ID specified (shinyproxy)."
    }
    ugroups <- csv2Vector(tolower(Sys.getenv("SHINYPROXY_USERGROUPS")))
    if (!length(ugroups) || grepl("^\\s*$", ugroups)) {
      errMsg <- paste(errMsg, "No user groups specified (shinyproxy).", sep = "\n")
    }
    if (!identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true") &&
      any(!grepl("^[a-zA-Z0-9_]{4,70}$", c(uid, ugroups), perl = TRUE))) {
      errMsg <- paste(errMsg,
        "Invalid user ID or user group specified. The following rules apply for user IDs and groups:\n- must be at least 4 and not more than 70 characters long\n- may contain only a-z A-Z 0-9 _",
        sep = "\n"
      )
    }
  } else {
    uidTmp <- Sys.getenv("MIRO_USERNAME")
    if (!identical(uidTmp, "")) {
      uid <- uidTmp
      ugroupsTmp <- Sys.getenv("MIRO_USERGROUPS")
      if (!identical(ugroupsTmp, "")) {
        ugroups <- ugroupsTmp
      }
    }
    if (length(uid) != 1 || !is.character(uid)) {
      errMsg <- "Invalid user ID specified."
    }
    if (!length(ugroups)) {
      ugroups <- defaultGroup
    }
  }
}
if (is.null(errMsg)) {
  if (!dir.exists(miroWorkspace) &&
    !dir.create(miroWorkspace, showWarnings = FALSE)[1]) {
    errMsg <- sprintf(
      "Could not create MIRO workspace directory: '%s'. Please make sure you have sufficient permissions. '",
      miroWorkspace
    )
  }
  # initialise loggers
  if (!dir.exists(logFileDir)) {
    tryCatch(
      {
        if (!dir.create(logFileDir, showWarnings = FALSE)) {
          stop()
        }
      },
      error = function(e) {
        errMsg <<- paste(errMsg, "Log file directory could not be created. Check that you have sufficient read/write permissions in application folder.", sep = "\n")
      }
    )
  }
}
if (is.null(errMsg)) {
  loggingLevel <<- c(FATAL, ERROR, WARN, INFO, DEBUG, TRACE)[[loggingLevel]]
  flog.appender(do.call(
    if (identical(logToConsole, TRUE)) "appender.miro" else "appender.file",
    list(file = file.path(
      logFileDir,
      paste0(
        modelName, "_", uid, "_",
        format(
          Sys.time(),
          "%y.%m.%d_%H.%M.%S"
        ), ".log"
      )
    ))
  ))
  flog.threshold("TRACE")
  flog.trace("Logging facility initialised.")
  loggerInitialised <- TRUE
  # name of the R save file
  useTempDir <- !identical(Sys.getenv("MIRO_USE_TMP"), "false")
  # check if GAMS model file exists
  currentModelDir <- modelPath
  if (!useTempDir && !file.exists(file.path(modelPath, modelGmsName))) {
    errMsg <- sprintf("The GAMS model file: '%s' could not be found in the directory: '%s'." %+%
      "Please make sure you specify a valid gms file path.", modelGmsName, modelPath)
  }
  customRendererDir <<- file.path(currentModelDir, paste0("renderer_", modelName))
}
if (!miroDeploy &&
  identical(tolower(Sys.getenv("MIRO_MODE")), "config")) {
  LAUNCHCONFIGMODE <- TRUE
}
if (is.null(errMsg)) {
  rSaveFilePath <- file.path(
    currentModelDir,
    paste0(
      modelNameRaw, "_",
      if (useTempDir) "1_" else "0_",
      APIVersion, "_",
      if (identical(Sys.getenv("MIRO_VERSION_STRING"), "")) {
        MIROVersion
      } else {
        Sys.getenv("MIRO_VERSION_STRING")
      },
      ".miroconf"
    )
  )
  lang <<- fromJSON(file.path(".", "conf", paste0(miroLanguage, ".json")),
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  if (debugMode) {
    source("./modules/init.R", local = TRUE)
  } else if (!file.exists(rSaveFilePath)) {
    errMsg <- sprintf(
      "Miroconf file: '%s' does not exist.",
      rSaveFilePath
    )
  } else {
    load(rSaveFilePath)
    if (!exists("dbSchemaModel")) {
      # legacy app, need to convert to new format
      dbSchemaModel <- substring(
        dbSchema$tabName[-seq_len(6)],
        nchar(gsub("_", "", modelName, fixed = TRUE)) + 2L
      )
      dbSchemaModel <- lapply(seq_along(dbSchemaModel), function(i) {
        if (dbSchemaModel[i] %in% c(scalarsFileName, scalarsOutName)) {
          return(NA)
        }
        list(
          tabName = dbSchemaModel[i],
          colNames = dbSchema$colNames[[i + 6L]],
          colTypes = dbSchema$colTypes[[i + 6L]]
        )
      })
      dbSchemaModel[is.na(dbSchemaModel)] <- NULL
      dbSchemaModel <- list(
        schema = setNames(
          dbSchemaModel,
          vapply(dbSchemaModel, "[[", character(1L),
            "tabName",
            USE.NAMES = FALSE
          )
        ),
        views = list()
      )
      if (scalarsOutName %in% names(modelOut)) {
        scalarMeta <- setNames(
          modelOut[[scalarsOutName]]$symtypes,
          modelOut[[scalarsOutName]]$symnames
        )
        dbSchemaModel$schema <- c(
          dbSchemaModel$schema,
          setNames(lapply(names(scalarMeta), function(scalarName) {
            list(
              tabName = scalarName,
              colNames = scalarName,
              colTypes = if (identical(scalarMeta[[scalarName]], "set")) "c" else "d"
            )
          }), names(scalarMeta))
        )
        dbSchemaModel$views[[scalarsOutName]] <- c(dbSchemaModel$views[[scalarsOutName]], names(scalarMeta))
      }
      if (scalarsFileName %in% names(modelInRaw)) {
        scalarMeta <- setNames(
          modelInRaw[[scalarsFileName]]$symtypes,
          modelInRaw[[scalarsFileName]]$symnames
        )
        dbSchemaModel$schema <- c(
          dbSchemaModel$schema,
          setNames(lapply(names(scalarMeta), function(scalarName) {
            list(
              tabName = scalarName,
              colNames = scalarName,
              colTypes = if (identical(scalarMeta[[scalarName]], "set")) "c" else "d"
            )
          }), names(scalarMeta))
        )
        dbSchemaModel$views[[scalarsFileName]] <- c(dbSchemaModel$views[[scalarsFileName]], names(scalarMeta))
      }
      if (length(GMSOpt) || length(DDPar)) {
        dbSchemaModel$schema <- c(
          dbSchemaModel$schema,
          setNames(lapply(c(GMSOpt, DDPar), function(parName) {
            list(
              tabName = parName,
              colNames = parName,
              colTypes = "c"
            )
          }), c(GMSOpt, DDPar))
        )
        dbSchemaModel$views[[scalarsFileName]] <- c(dbSchemaModel$views[[scalarsFileName]], c(GMSOpt, DDPar))
      }
      if (length(dbSchemaModel$views) &&
        scalarsFileName %in% names(dbSchemaModel$views) &&
        length(modelInTemplate) && is.null(modelInTemplate[[length(modelInTemplate)]])) {
        modelInTemplate[[length(modelInTemplate)]] <- scalarsInTemplate
        scenDataTemplate <- c(scenDataTemplate, list(scalarsInTemplate))
      }
      rm(dbSchema)
    }
    suppressWarnings(rm(lang))
    for (customRendererName in customRendererNames) {
      assign(customRendererName, get(customRendererName), envir = .GlobalEnv)
    }
  }
  if (isShinyProxy || identical(Sys.getenv("MIRO_REMOTE_EXEC"), "true")) {
    config$activateModules$remoteExecution <- TRUE
  }
  if (identical(config$activateModules$hcube, TRUE) &&
    !config$activateModules$remoteExecution && !miroBuildOnly) {
    flog.info("The Hypercube module is only available with a GAMS Engine backend. Therefore, it has been disabled.")
    config$activateModules$hcube <- FALSE
  }
}
if (is.null(errMsg)) {
  hcubeScalars <- getHcubeScalars(modelIn)
  ioConfig <<- list(
    modelIn = modelIn,
    modelOut = modelOut,
    modelInRaw = modelInRaw,
    inputDsNames = inputDsNames,
    hcubeScalars = hcubeScalars,
    DDPar = DDPar,
    GMSOpt = GMSOpt,
    inputDsNamesBase = inputDsNames[!inputDsNames %in% hcubeScalars],
    scenTableNamesToDisplay = scenTableNamesToDisplay,
    textOnlySymbols = config$textOnlySymbols
  )
  if (!useGdx && identical(config$fileExchange, "gdx") && !miroBuildOnly) {
    errMsg <- paste(errMsg,
      sprintf("Can not use 'gdx' as file exchange with GAMS if gdxrrw library is not installed.\n
Please make sure you have a valid gdxrrwMIRO (https://github.com/GAMS-dev/gdxrrw-miro) installation in your R library: '%s'.", .libPaths()[1]),
      sep = "\n"
    )
  }
  GAMSClArgs <- c(
    paste0("execMode=", gamsExecMode),
    paste0('IDCGDXOutput="', MIROGdxOutName, '"')
  )

  if (isShinyProxy || identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) {
    dbConfig <- setDbConfig()

    if (length(dbConfig$errMsg)) {
      errMsg <- dbConfig$errMsg
    } else {
      dbConfig <- dbConfig$data
    }
  } else {
    dbConfig <- list(
      type = "sqlite",
      name = file.path(
        miroDbDir,
        paste0(modelName, ".sqlite3")
      )
    )
    if (!dir.exists(miroDbDir)) {
      if (identical(basename(miroDbDir), "app_data") &&
        identical(dirname(miroDbDir), miroWorkspace) &&
        file.exists(file.path(miroWorkspace, "miro.sqlite3"))) {
        # new MIRO app_data directory in MIRO workspace
        dbConfig$dbPathToMigrate <- file.path(miroWorkspace, "miro.sqlite3")
      }
      if (!dir.create(miroDbDir, showWarnings = FALSE)) {
        errMsg <- paste(errMsg, sprintf("App data directory: '%s' could not be created. Check that you have sufficient read/write permissions.", miroDbDir),
          sep = "\n"
        )
      }
    } else if (!identical(dirname(miroDbDir), miroWorkspace) &&
      file.exists(file.path(miroDbDir, "miro.sqlite3")) &&
      !file.exists(dbConfig$name)) {
      # custom database location with existing legacy database
      dbConfig$dbPathToMigrate <- file.path(miroDbDir, "miro.sqlite3")
    }
  }
  if (isTRUE(config$activateModules$remoteExecution)) {
    useTempDir <- TRUE
  }
  if (debugMode) {
    if (file.exists(file.path(currentModelDir, paste0(modelName, "_files.txt")))) {
      tryCatch(
        {
          modelFiles <- gsub("^[.][/\\\\]", "", readLines(file.path(
            currentModelDir,
            paste0(modelName, "_files.txt")
          ),
          encoding = "UTF-8", warn = FALSE
          ))
        },
        error = function(e) {
          errMsg <<- paste(errMsg, sprintf(
            "Problems reading file: '%s_files.txt'. Error message: '%s'.",
            modelName, conditionMessage(e)
          ),
          sep = "\n"
          )
        }
      )
      if (!modelGmsName %in% modelFiles) {
        errMsg <- paste(errMsg, sprintf(
          "Problems reading file: '%s_files.txt'.
                                        Main GAMS model file not found in model assembly file.",
          modelName
        ),
        sep = "\n"
        )
      }
      buildArchive <- !identical(Sys.getenv("MIRO_BUILD_ARCHIVE"), "false")
      if (is.null(errMsg) && useTempDir && buildArchive) {
        tryCatch(
          {
            flog.info("Compressing model files...")
            zipMiro(
              file.path(currentModelDir, paste0(modelName, ".zip")),
              modelFiles, currentModelDir
            )
            modelFiles <- paste0(modelName, ".zip")
          },
          error = function(e) {
            errMsg <<- paste(errMsg, sprintf(
              "Problems creating file: '%s'. Error message: '%s'.",
              paste0(modelName, ".zip"), conditionMessage(e)
            ),
            sep = "\n"
            )
          }
        )
      } else if (miroDeploy) {
        if (isTRUE(config$activateModules$remoteExecution)) {
          errMsg <- paste(errMsg, "Remote execution mode requires to be executed in temporary directory. Set the environment variable MIRO_BUILD_ARCHIVE to 'true'!",
            sep = "\n"
          )
        }
      }
    } else if (miroDeploy) {
      errMsg <- paste(errMsg, paste0("No model assembly file ('", modelName, "_files.txt') found."),
        sep = "\n"
      )
      attr(errMsg, "noMA") <- TRUE
    } else if (config$activateModules$remoteExecution) {
      flog.warn("No model assembly file ('%s_files.txt') found.", modelName)
    }
    if (miroDeploy) {
      if (file.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
        modelFiles <- c(modelFiles, paste0("static_", modelName))
      }
      if (file.exists(file.path(
        currentModelDir,
        paste0(miroDataDirPrefix, modelName)
      ))) {
        modelFiles <- c(modelFiles, paste0(miroDataDirPrefix, modelName))
      }
      if (file.exists(customRendererDir)) {
        modelFiles <- c(modelFiles, paste0("renderer_", modelName))
      }
      if (file.exists(file.path(
        currentModelDir,
        paste0("scripts_", modelName)
      ))) {
        modelFiles <- c(modelFiles, paste0("scripts_", modelName))
      }
      if (is.null(errMsg) && identical(Sys.getenv("MIRO_TEST_DEPLOY"), "true")) {
        modelPath <<- file.path(tmpFileDir, modelName, "test_deploy")
        if (dir.exists(modelPath) &&
          unlink(modelPath, recursive = TRUE, force = TRUE) != 0L) {
          errMsg <- sprintf(
            "Problems removing temporary directory: '%s'. No write permissions?",
            modelPath
          )
        }
        if (is.null(errMsg) && any(!file.copy2(
          file.path(currentModelDir, modelFiles),
          file.path(modelPath, modelFiles)
        ))) {
          errMsg <- sprintf(
            "Problems copying files from: '%s' to: '%s'. No write permissions?",
            currentModelDir, modelPath
          )
        }
        if (is.null(errMsg)) {
          currentModelDir <- modelPath
        }
      }
    }
  }
  overwriteLang <- Sys.getenv("MIRO_LANG")
  if (!identical(overwriteLang, "") && !identical(overwriteLang, config$language)) {
    if (file.exists(file.path(".", "conf", paste0(overwriteLang, ".json")))) {
      lang <<- fromJSON(file.path(".", "conf", paste0(overwriteLang, ".json")),
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE
      )
      config$language <- overwriteLang
    }
  }
}
if (is.null(errMsg)) {
  modelData <- NULL
  if (useTempDir &&
    file.exists(file.path(currentModelDir, paste0(modelName, ".zip")))) {
    modelData <- file.path(currentModelDir, paste0(modelName, ".zip"))
  } else if (config$activateModules$remoteExecution) {
    errMsg <- paste(errMsg, sprintf(
      "No model data ('%s.zip') found.\nPlease make sure that you specify the files that belong to your model in a text file named '%s_files.txt' (model assembly file).",
      modelName, modelName
    ),
    sep = "\n"
    )
  } else {
    GAMSClArgs <- c(GAMSClArgs, paste0('idir1="', gmsFilePath(currentModelDir), '"'))
  }
}

if (is.null(errMsg)) {
  # load default and custom renderers (output data)
  rendererFiles <- list.files("./modules/renderers/", pattern = "\\.R$")
  for (file in rendererFiles) {
    if (!file.access("./modules/renderers/" %+% file, mode = 4)) {
      tryCatch(
        {
          source("./modules/renderers/" %+% file)
        },
        error = function(e) {
          errMsg <<- paste(errMsg,
            sprintf(
              "Some error occurred while sourcing renderer file '%s'. Error message: '%s'.",
              file, conditionMessage(e)
            ),
            sep = "\n"
          )
        },
        warning = function(w) {
          errMsg <<- paste(errMsg,
            sprintf(
              "Some error occurred while sourcing renderer file '%s'. Error message: '%s'.",
              file, w
            ),
            sep = "\n"
          )
        }
      )
    } else {
      errMsg <- "File: '" %+% file %+% "' could not be found or user has no read permissions."
    }
  }
}
if (is.null(errMsg) && debugMode) {
  if (!LAUNCHCONFIGMODE) {
    rendererFiles <- list.files(customRendererDir, pattern = "\\.R$", ignore.case = TRUE)
    lapply(rendererFiles, function(file) {
      if (!file.access(file.path(customRendererDir, file), mode = 4)) {
        tryCatch(
          {
            eval(parse(file.path(customRendererDir, file), encoding = "UTF-8"), envir = .GlobalEnv)
          },
          error = function(e) {
            errMsg <<- paste(errMsg,
              sprintf(
                "Some error occurred while sourcing custom renderer file '%s'. Error message: %s.",
                file, conditionMessage(e)
              ),
              sep = "\n"
            )
          },
          warning = function(w) {
            errMsg <<- paste(errMsg,
              sprintf(
                "Some error occurred while sourcing custom renderer file '%s'. Error message: %s.",
                file, w
              ),
              sep = "\n"
            )
          }
        )
      } else {
        errMsg <<- paste(errMsg, sprintf(
          "Custom renderer file: '%s' could not be found or user has no read permissions.",
          file
        ), sep = "\n")
      }
    })
  }
  listOfCustomRenderers <- Set$new()
  requiredPackagesCR <<- NULL

  for (customRendererConfig in c(configGraphsOut, configGraphsIn, modelIn)) {
    # check whether non standard renderers were defined in graph config
    if (!is.null(customRendererConfig$rendererName)) {
      customRendererConfig$outType <- customRendererConfig$rendererName
    }
    if (any(is.na(match(tolower(customRendererConfig$outType), standardRenderers)))) {
      if (!LAUNCHCONFIGMODE) {
        customRendererName <- "render" %+% toupper(substr(customRendererConfig$outType, 1, 1)) %+%
          substr(customRendererConfig$outType, 2, nchar(customRendererConfig$outType))
        customRendererOutput <- customRendererConfig$outType %+% "Output"
        # find render function
        tryCatch(
          {
            match.fun(customRendererName)
            listOfCustomRenderers$push(customRendererName)
          },
          error = function(e) {
            errMsg <<- paste(errMsg,
              sprintf(
                "A custom renderer function: '%s' was not found. Please make sure first define such a function.",
                customRendererName
              ),
              sep = "\n"
            )
          }
        )
        # find output function
        tryCatch(
          {
            match.fun(customRendererOutput)
            listOfCustomRenderers$push(customRendererOutput)
          },
          error = function(e) {
            errMsg <<- paste(errMsg,
              sprintf(
                "No output function for custom renderer function: '%s' was found. Please make sure you define such a function.",
                customRendererName
              ),
              sep = "\n"
            )
          }
        )
      }
      # find packages to install them
      if (length(customRendererConfig$packages)) {
        requiredPackagesCR <- c(requiredPackagesCR, customRendererConfig$packages)
      }
    }
  }
  if (miroBuildOnly) {
    for (el in c(externalInputConfig, datasetsRemoteExport)) {
      for (sym in el) {
        if (length(sym$functionName)) {
          listOfCustomRenderers$push(sym$functionName)
        }
      }
    }
  }
  requiredPackagesCR <- unique(requiredPackagesCR)
  customRendererNames <- listOfCustomRenderers$get()
  rm(listOfCustomRenderers)
}
aboutDialogText <- paste0(
  "<b>GAMS MIRO v.", MIROVersion, "</b><br/><br/>",
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
  "<a href=\\'http://www.gams.com/miro/license.html\\' target=\\'_blank\\'>here</a>."
)
if (miroBuildOnly) {
  if (!is.null(errMsg)) {
    if (file.exists(file.path(
      currentModelDir,
      paste0(modelNameRaw, ".miroapp")
    )) &&
      unlink(file.path(
        currentModelDir,
        paste0(modelNameRaw, ".miroapp")
      ), force = TRUE) == 1) {
      warning("Problems removing corrupted miroapp file", call. = FALSE)
    }
    warning(errMsg, call. = FALSE)
    if (interactive()) {
      stop()
    }
    if (isTRUE(attr(errMsg, "noMA"))) {
      quit("no", status = 2)
    } else {
      quit("no", status = 1)
    }
  }

  save(
    list = c(
      "customRendererNames", customRendererNames, "modelIn", "modelInRaw",
      "modelOut", "config", "inputDsNames", "inputDsAliases",
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
      "dbSchemaModel", "modelOutTemplate", "scenTableNamesToDisplay",
      "dependentDatasets", "outputTabs",
      "installPackage", "scalarInputSym", "scalarInputSymToVerify",
      "requiredPackagesCR", "datasetsRemoteExport", "dropdownAliases",
      # TODO: Update API version when dataContract is used elsewhere than in Configuration mode
      "dataContract"
    ),
    file = rSaveFilePath
  )

  if (identical(Sys.getenv("MIRO_COMPILE_ONLY"), "true")) {
    if (interactive()) {
      stop()
    }
    quit("no")
  }
  tryCatch(
    {
      # create metadata file
      tmpd <- tempdir(check = TRUE)
      metadataContent <- list(
        version = 1L,
        api_version = APIVersion,
        miro_version = MIROVersion,
        main_gms_name = modelGmsName,
        timestamp = as.character(as.POSIXlt(Sys.time(), tz = "UTC"), usetz = TRUE),
        host_os = getOS(),
        modes_included = "base",
        use_temp_dir = useTempDir
      )
      appMetadataFile <- file.path(tmpd, "miroapp.json")
      write_json(metadataContent, appMetadataFile,
        auto_unbox = TRUE, null = "null"
      )
      # assemble MIROAPP
      miroAppPath <- file.path(currentModelDir, paste0(modelNameRaw, ".miroapp"))
      flog.info("Generating miroapp file...")
      zipMiro(
        miroAppPath,
        c(modelFiles, basename(rSaveFilePath)), currentModelDir
      )
      zipr_append(miroAppPath, appMetadataFile, mode = "cherry-pick")
    },
    error = function(e) {
      stop(sprintf(
        "Problems creating app bundle. Error message: '%s'.",
        conditionMessage(e)
      ), call. = FALSE)
    }
  )
  if (!identical(unlink(rSaveFilePath), 0L)) {
    flog.warn("Could not remove miroconf file: '%s'.", rSaveFilePath)
  }
  if (interactive()) {
    stop()
  }
  quit("no")
}

if (is.null(errMsg)) {
  if (config$activateModules$remoteExecution) {
    requiredPackages <- c("future", "httr")
  } else if (length(externalInputConfig) || length(datasetsRemoteExport)) {
    requiredPackages <- "httr"
  } else {
    requiredPackages <- character(0L)
  }
  if (LAUNCHCONFIGMODE) {
    requiredPackages <- c(
      requiredPackages, "plotly", "xts", "dygraphs", "leaflet", "chartjs", "sortable",
      "leaflet.minicharts", "timevis", "shinyAce"
    )
  } else {
    requiredPackages <- c(
      requiredPackages,
      if (identical(installPackage$plotly, TRUE)) "plotly",
      if (identical(installPackage$dygraphs, TRUE)) c("xts", "dygraphs"),
      if (identical(installPackage$leaflet, TRUE)) c("leaflet", "leaflet.minicharts"),
      if (identical(installPackage$timevis, TRUE)) c("timevis")
    )
  }
  errMsg <- installAndRequirePackages(unique(requiredPackages), installedPackages, RLibPath, CRANMirror, miroWorkspace)

  if (!is.null(requiredPackagesCR) && !isShinyProxy && !miroBuildOnly) {
    # add custom library path to libPaths
    .libPaths(c(.libPaths(), file.path(miroWorkspace, "custom_packages")))
    installedPackages <<- installed.packages()[, "Package"]
    if (!identical(Sys.getenv("MIRO_AGREE_INSTALL_PACKAGES"), "true")) {
      newPackages <- requiredPackagesCR[!requiredPackagesCR %in% installedPackages]
      if (length(newPackages)) {
        if (interactive()) {
          stop(sprintf(
            "New packages: '%s' need to be installed. Please set MIRO_AGREE_INSTALL_PACKAGES to 'true'.",
            paste(newPackages, collapse = ", ")
          ))
        }
        write("\n", stderr())
        write(paste0("merr:::426:::", CRANMirror, ":::", paste(newPackages, collapse = ", ")), stderr())
        quit("no", 0L)
      }
    }
    errMsgTmp <- installAndRequirePackages(requiredPackagesCR, installedPackages,
      RLibPath, Sys.getenv("MIRO_CRAN_MIRROR", CRANMirror), miroWorkspace,
      attachPackages = FALSE
    )
    if (length(errMsgTmp)) {
      errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
    }
    rm(requiredPackagesCR)
  }
  options("DT.TOJSON_ARGS" = list(na = "string", na_as_null = TRUE))

  if (config$activateModules$remoteExecution && !LAUNCHCONFIGMODE) {
    if (isWindows()) {
      plan(multisession)
    } else {
      plan(multicore)
    }
  }
  # try to create the DB connection (PostgreSQL)
  db <- NULL
  if (identical(tolower(dbConfig$type), "sqlite")) {
    requiredPackages <- c("DBI", "RSQLite")
  } else {
    requiredPackages <- c("DBI", "RPostgres")
  }
  errMsgTmp <- installAndRequirePackages(
    requiredPackages, installedPackages,
    RLibPath, CRANMirror, miroWorkspace
  )
  if (length(errMsgTmp)) {
    errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
  }

  source("./components/db_schema.R")
  source("./components/db.R")
  source("./components/db_scen.R")
  tryCatch(
    {
      dbSchema <<- DbSchema$new(dbSchemaModel)
      db <- Db$new(
        uid = uid, dbConf = dbConfig,
        slocktimeLimit = slocktimeLimit, modelName = modelName,
        ugroups = ugroups
      )
      conn <- db$getConn()
      dbSchema$setConn(conn)
      flog.debug("Database connection established.")
    },
    error = function(e) {
      flog.error("Problems initialising database class. Error message: %s", conditionMessage(e))
      errMsg <<- paste(errMsg, conditionMessage(e), sep = "\n")
    }
  )
  tryCatch(
    {
      dataio <- DataIO$new(
        config = list(
          modelIn = modelIn, modelOut = modelOut,
          modelName = modelName
        ),
        db = db
      )
    },
    error = function(e) {
      flog.error("Problems initialising dataio class. Error message: %s", conditionMessage(e))
      errMsg <<- paste(errMsg, conditionMessage(e), sep = "\n")
    }
  )

  if (identical(config$activateModules$hcube, TRUE)) {
    source("./components/hcube_builder.R")
    source("./components/hcube_results.R")
  }
}

if (is.null(errMsg)) {
  tryCatch(
    {
      gdxio <<- NULL
      if (useGdx) {
        gdxio <<- GdxIO$new(
          file.path(
            .libPaths()[1], "gdxrrwMIRO",
            if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
              file.path("bin", "x64")
            } else {
              "bin"
            }
          ),
          c(modelInRaw, modelOut), scalarsFileName,
          scalarsOutName, scalarEquationsName, scalarEquationsOutName,
          dropdownAliases
        )
      }
    },
    error = function(e) {
      flog.error(conditionMessage(e))
      errMsg <<- conditionMessage(e)
    }
  )
  remoteUser <- uid
  rememberMeFileName <- paste0(
    miroWorkspace, .Platform$file.sep, ".cred_",
    modelName
  )
  credConfig <- NULL
  if (isShinyProxy) {
    if (identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true")) {
      userCredentials <- list(
        username = Sys.getenv("MIRO_ENGINE_ANONYMOUS_USER", "anonymous"),
        password = Sys.getenv("MIRO_ENGINE_ANONYMOUS_PASS"),
        useBearer = FALSE
      )
    } else {
      userCredentials <- list(
        username = uid,
        password = Sys.getenv("SHINYPROXY_WEBSERVICE_ACCESS_TOKEN"),
        useBearer = TRUE
      )
    }
    credConfig <- list(
      url = Sys.getenv("MIRO_ENGINE_HOST"),
      username = userCredentials$username,
      password = userCredentials$password,
      namespace = Sys.getenv("MIRO_ENGINE_NAMESPACE"),
      useRegistered = TRUE,
      useBearer = userCredentials$useBearer
    )
  } else if (config$activateModules$remoteExecution) {
    tryCatch(
      {
        if (file.exists(file.path(miroWorkspace, "pinned_pub_keys"))) {
          pinnedPublicKeys <- paste0("sha256//",
            read_lines(file.path(
              miroWorkspace,
              "pinned_pub_keys"
            )),
            collapse = ";"
          )
          httr::set_config(httr::config(pinnedpublickey = pinnedPublicKeys))
        }
      },
      error = function(e) {
        errMsg <<- paste(errMsg, sprintf(
          "Could not read pinned certificates file. Error message: '%s'.",
          conditionMessage(e)
        ), sep = "\n")
      }
    )
    tryCatch(
      {
        credConfigTmp <- NULL
        if (file.exists(rememberMeFileName)) {
          credConfigTmp <- suppressWarnings(fromJSON(rememberMeFileName,
            simplifyDataFrame = FALSE,
            simplifyMatrix = FALSE
          ))
          if (!is.list(credConfigTmp) || !all(c("url", "username", "password", "namespace", "reg")
          %in% names(credConfigTmp)) ||
            !all(vapply(credConfigTmp[1:4], is.character,
              logical(1L),
              USE.NAMES = FALSE
            )) ||
            !is.logical(credConfigTmp[[5L]])) {
            errMsgTmp <- "Malformatted credential file. Looks like someone tried to tamper with the app!"
            flog.error(errMsgTmp)
            errMsg <<- paste(errMsg, errMsgTmp, sep = "\n")
            return(NULL)
          }
          credConfig <- list(
            url = credConfigTmp$url,
            username = credConfigTmp$username,
            password = credConfigTmp$password,
            namespace = credConfigTmp$namespace,
            useRegistered = credConfigTmp$reg,
            refreshToken = TRUE
          )
        }
      },
      error = function(e) {
        errMsgTmp <- "Problems reading JSON file: '%s'. Please make sure you have sufficient access permissions."
        flog.error(errMsgTmp)
        errMsg <<- paste(errMsg, errMsgTmp, sep = "\n")
      },
      finally = rm(credConfigTmp)
    )
  }
}
if (is.null(errMsg) && (debugMode || miroStoreDataOnly)) {
  # checking database inconsistencies
  source("./components/db_migrator.R")
  migApp <- NULL
  local({
    tryCatch(
      {
        dbMigrator <- DbMigrator$new(db)
        inconsistentTablesInfo <- dbMigrator$getInconsistentTablesInfo()
        orphanedTablesInfo <- dbMigrator$getOrphanedTablesInfo()
        isNewTable <- vapply(inconsistentTablesInfo, function(tableInfo) {
          if (length(tableInfo$currentColNames)) {
            return(FALSE)
          }
          return(TRUE)
        }, logical(1L), USE.NAMES = FALSE)
        if (!length(orphanedTablesInfo)) {
          inconsistentTablesInfo <- inconsistentTablesInfo[!isNewTable]
        }
      },
      error = function(e) {
        flog.error("Problems initialising dbMigrator. Error message: '%s'.", conditionMessage(e))
        if (miroStoreDataOnly) {
          write("\n", stderr())
          write("merr:::500", stderr())
        }
        if (interactive()) {
          stop()
        }
        quit("no", 1L)
      }
    )
    if (length(inconsistentTablesInfo) || length(orphanedTablesInfo)) {
      source("./tools/db_migration/modules/bt_delete_database.R", local = TRUE)
      source("./tools/db_migration/modules/form_db_migration.R", local = TRUE)
      source("./tools/db_migration/server.R", local = TRUE)
      source("./tools/db_migration/ui.R", local = TRUE)
      if (isShinyProxy) {
        if (identical(Sys.getenv("MIRO_MIGRATE_DB"), "true")) {
          quit("no", migrateFromConfig(Sys.getenv("MIRO_MIGRATION_CONFIG_PATH")))
        } else {
          write_json(list(
            inconsistentTablesInfo = inconsistentTablesInfo,
            orphanedTablesInfo = orphanedTablesInfo,
            uiContent = as.character(
              dbMigrationForm(Sys.getenv("MIRO_MIGRATE_DB_FORM_ID", "migrationForm"),
                inconsistentTablesInfo,
                orphanedTablesInfo,
                standalone = FALSE
              )
            )
          ),
          path = Sys.getenv("MIRO_MIGRATION_CONFIG_PATH"),
          auto_unbox = TRUE, null = "null"
          )
          write("\n", stderr())
          write("merr:::409", stderr())
          quit("no", 1L)
        }
      }
      if (miroStoreDataOnly) {
        write("\n", stderr())
        write("merr:::409", stderr())
      }
      migApp <<- shinyApp(ui = uiDbMig, server = serverDbMig)
    } else {
      tryCatch(
        {
          dbMigrator$createMissingScalarTables()
        },
        error = function(e) {
          flog.error("Problems creating scalar tables. Error message: '%s'.", conditionMessage(e))
          if (miroStoreDataOnly) {
            write("\n", stderr())
            write("merr:::500", stderr())
          }
          if (interactive()) {
            stop()
          }
          quit("no", 1L)
        }
      )
    }
  })
  if (length(migApp)) {
    return(migApp)
  }
}
if (!is.null(errMsg)) {
  if (loggerInitialised) {
    if (length(warningMsg)) flog.warn(warningMsg)
    flog.fatal(errMsg)
  } else {
    warning(errMsg, call. = FALSE)
  }
  if (isShinyProxy) {
    if (miroStoreDataOnly) {
      stop(sprintf(
        "An error occured. Error message: %s",
        errMsg
      ), call. = FALSE)
    }
    stop("An error occured. Check log for more information!", call. = FALSE)
  } else if (miroStoreDataOnly) {
    if (interactive()) {
      stop()
    }
    quit("no", 1L)
  }
  ui_initError <- fluidPage(
    tags$head(
      if (!is.list(config) || !is.character(config$theme)) {
        tags$link(type = "text/css", rel = "stylesheet", href = "skin_light.css")
      } else {
        tags$link(type = "text/css", rel = "stylesheet", href = paste0("skin_", config$theme, ".css"))
      },
      tags$script(src = "miro.js", type = "application/javascript")
    ),
    titlePanel(
      if (!exists("lang") || is.null(lang$errMsg$initErrors$title)) {
        "Some errors occurred"
      } else {
        lang$errMsg$initErrors$title
      }
    ),
    fluidRow(
      align = "center",
      HTML("<br>"),
      div(
        if (!exists("lang") || is.null(lang$errMsg$initErrors$desc)) {
          "Please fix the errors mentioned below and restart GAMS MIRO:"
        } else {
          lang$errMsg$initErrors$desc
        },
        class = "initErrors"
      ),
      HTML("<br>"),
      verbatimTextOutput("errorMessages"),
      tags$div(
        style = "text-align:center;margin-top:20px;",
        actionButton("btCloseInitErrWindow", if (!exists("lang") || is.null(lang$errMsg$initErrors$okButton)) {
          "Ok"
        } else {
          lang$errMsg$initErrors$okButton
        })
      )
    )
  )
  server_initError <- function(input, output, session) {
    output$errorMessages <- renderText(
      errMsg
    )
    observeEvent(input$btCloseInitErrWindow, {
      if (!interactive()) {
        stopApp()
      }
    })
    session$onSessionEnded(function() {
      if (!interactive()) {
        stopApp()
      }
    })
  }

  shinyApp(ui = ui_initError, server = server_initError)
} else {
  uidAdmin <<- if (identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true")) "admin" else uid
  if (isShinyProxy && miroStoreDataOnly) {
    if (identical(Sys.getenv("MIRO_API_GET_SCEN_LIST"), "true")) {
      source("./tools/api/util.R")
      write("merr:::200:::", stderr())
      write(scenMetaTibbleToJSON(db$fetchScenList(scode = SCODEMAP[["scen"]])), stderr())
      if (interactive()) {
        stop()
      }
      quit("no", 0L)
    } else if (identical(Sys.getenv("MIRO_API_DOWNLOAD_SCEN"), "true")) {
      source("./tools/api/util.R", local = TRUE)
      downloadMIROScenario(uid, excelConfig = list(
        includeMeta = config$excelIncludeMeta,
        includeEmpty = config$excelIncludeEmptySheets
      ))
      if (interactive()) {
        stop()
      }
      quit("no", 0L)
    } else if (identical(Sys.getenv("MIRO_API_DELETE_SCEN"), "true")) {
      source("./tools/api/util.R")
      tryCatch(
        deleteMIROScenario(db, uid),
        error_not_found = function(e) {
          write("\n", stderr())
          write("merr:::404:::Scenario not found", stderr())
          if (interactive()) {
            stop()
          }
          quit("no", 1L)
        },
        error_scen_locked = function(e) {
          write("\n", stderr())
          write("merr:::423:::Scenario is locked", stderr())
          if (interactive()) {
            stop()
          }
          quit("no", 1L)
        }
      )
    }
  }
  local({
    miroDataDir <- Sys.getenv("MIRO_DATA_DIR")
    removeDataFile <- !debugMode
    scenImportConfig <- list()
    if (identical(miroDataDir, "")) {
      miroDataDir <- file.path(currentModelDir, paste0(miroDataDirPrefix, modelName))
      miroDataFilesRaw <- list.files(miroDataDir)
    } else if (isFALSE(file.info(miroDataDir)$isdir)) {
      miroDataFilesRaw <- basename(miroDataDir)
      miroDataDir <- dirname(miroDataDir)
      stdin <- NULL
      scenImportConfig <- tryCatch(
        {
          stdin <- file("stdin", blocking = FALSE)
          fromJSON(suppressWarnings(readLines(stdin)))
        },
        error = function(e) {
          return(list())
        },
        finally = {
          if (!is.null(stdin)) {
            close(stdin)
          }
        }
      )
      removeDataFile <- FALSE
    } else {
      miroDataFilesRaw <- list.files(miroDataDir)
    }
    if (is.null(scenImportConfig[["readPerm"]])) {
      scenImportConfig[["readPerm"]] <- c(uidAdmin, db$getUserAccessGroups()[-1])
    }
    if (is.null(scenImportConfig[["writePerm"]])) {
      scenImportConfig[["writePerm"]] <- uidAdmin
    }
    if (is.null(scenImportConfig[["execPerm"]])) {
      scenImportConfig[["execPerm"]] <- c(uidAdmin, db$getUserAccessGroups()[-1])
    }
    dataFileExt <- tolower(tools::file_ext(miroDataFilesRaw))
    # IMPORTANT!!!!!!
    # if list of valid data file types is updated, it also has to be updated in
    # supportedDataFileTypes array in /renderer/index.js
    miroDataFiles <- miroDataFilesRaw[dataFileExt %in% c(if (useGdx) c("gdx", "miroscen"), "xlsx", "xlsm", "xls", "zip")]
    dataFileExt <- tolower(tools::file_ext(miroDataFiles))
    newScen <- NULL
    tryCatch(
      {
        if (length(miroDataFiles)) {
          tabularDatasetsToFetch <- modelInTabularData
          tabularIdsToFetchId <- names(modelIn) %in% tabularDatasetsToFetch
          metaDataTmp <- modelIn[tabularIdsToFetchId]
          namesScenInputData <- names(modelIn)[tabularIdsToFetchId]
          modelInTemplateTmp <- modelInTemplate[tabularIdsToFetchId]
          if (length(scalarsInMetaData) && !scalarsFileName %in% tabularDatasetsToFetch) {
            tabularDatasetsToFetch <- c(tabularDatasetsToFetch, scalarsFileName)
            namesScenInputData <- c(namesScenInputData, scalarsFileName)
            modelInTemplateTmp[[length(metaDataTmp) + 1L]] <- scalarsInTemplate
            metaDataTmp <- c(metaDataTmp, scalarsInMetaData)
          }
          inputIdsTmp <- match(inputDsNames, names(metaDataTmp))
          inputIdsTmp <- inputIdsTmp[!is.na(inputIdsTmp)]
          metaDataTmp <- metaDataTmp[inputIdsTmp]
          modelInTemplateTmp <- modelInTemplateTmp[inputIdsTmp]

          tmpDirToRemove <- character(0L)

          if (debugMode) {
            forceScenImport <- identical(Sys.getenv("MIRO_FORCE_SCEN_IMPORT"), "true")
            if (!forceScenImport) {
              currentDataHashesDf <- db$importDataset("_dataHash")
              currentDataHashes <- list()
              if (length(currentDataHashesDf) && nrow(currentDataHashesDf)) {
                currentDataHashes <- currentDataHashesDf[["hash"]]
                names(currentDataHashes) <- currentDataHashesDf[["filename"]]
              }
              newDataHashes <- currentDataHashes
            }
          }
          overwriteScenToImport <- Sys.getenv("MIRO_OVERWRITE_SCEN_IMPORT")
          abortIfScenExists <- identical(overwriteScenToImport, "ask")
          overwriteScenToImport <- !(abortIfScenExists || identical(overwriteScenToImport, "false"))
          xlsio <- NULL
          attachments <- NULL

          for (i in seq_along(miroDataFiles)) {
            miroDataFile <- miroDataFiles[i]
            dfClArgs <- NULL
            viewsFileId <- NULL
            if (debugMode && !forceScenImport) {
              dataHash <- digest::digest(
                file = file.path(miroDataDir, miroDataFile),
                algo = "sha1", serialize = FALSE
              )
              if (!identical(dataFileExt[i], "miroscen")) {
                if (is.null(scenImportConfig[["scenNameOverwrite"]])) {
                  scenName <- tools::file_path_sans_ext(miroDataFile)
                } else {
                  scenName <- scenImportConfig[["scenNameOverwrite"]]
                }
                viewsFileId <- match(
                  paste0(scenName, "_views.json"),
                  miroDataFilesRaw
                )
                if (!is.na(viewsFileId)) {
                  dataHash <- paste0(dataHash, digest::digest(
                    file = file.path(
                      miroDataDir,
                      paste0(
                        scenName,
                        "_views.json"
                      )
                    ),
                    algo = "sha1", serialize = FALSE
                  ))
                }
              }
              if (miroDataFile %in% names(currentDataHashes) &&
                identical(dataHash, currentDataHashes[[miroDataFile]])) {
                flog.info("Data: '%s' skipped because it has not changed since the last start.", miroDataFile)
                next
              }
              newDataHashes[[miroDataFile]] <- dataHash
            }
            flog.info("New data: '%s' is stored in the database. Please wait until the import is completed.", miroDataFile)
            if (dataFileExt[i] %in% c("xls", "xlsx", "xlsm")) {
              method <- "xls"
              tmpDir <- miroDataDir
              if (is.null(xlsio)) {
                xlsio <- XlsIO$new()
              }
              xlsio$readIndex(file.path(tmpDir, miroDataFile))
            } else if (dataFileExt[i] == "zip") {
              method <- "csv"
              tmpDir <- tryCatch(
                getValidCsvFromZip(
                  file.path(miroDataDir, miroDataFile),
                  c(
                    names(modelOut),
                    inputDsNames
                  ), uid
                )$tmpDir,
                error = function(e) {
                  flog.error(conditionMessage(e))
                  return("e")
                }
              )
              if (identical(tmpDir, "e")) {
                next
              }
              tmpDirToRemove <- tmpDir
            } else {
              method <- dataFileExt[i]
              tmpDir <- miroDataDir
            }
            if (dataFileExt[i] == "miroscen") {
              method <- "gdx"
              tmpDir <- tempdir(check = TRUE)
              views <- Views$new(
                names(modelIn),
                names(modelOut),
                ioConfig$inputDsNamesBase
              )
              if (is.null(attachments)) {
                attachments <- Attachments$new(
                  db, list(
                    maxSize = attachMaxFileSize, maxNo = attachMaxNo,
                    forbiddenFNames = c(
                      if (identical(config$fileExchange, "gdx")) {
                        c(MIROGdxInName, MIROGdxOutName)
                      } else {
                        paste0(c(names(modelOut), inputDsNames), ".csv")
                      },
                      paste0(modelNameRaw, c(".log", ".lst"))
                    )
                  ),
                  tmpDir,
                  names(modelIn),
                  names(modelOut),
                  ioConfig$inputDsNamesBase
                )
              }

              newScen <- Scenario$new(
                db = db, sname = lang$nav$dialogNewScen$newScenName, isNewScen = TRUE,
                readPerm = scenImportConfig[["readPerm"]],
                writePerm = scenImportConfig[["writePerm"]],
                execPerm = scenImportConfig[["execPerm"]],
                uid = uidAdmin,
                views = views, attachments = attachments,
                forceOverwrite = !identical(scenImportConfig[["forceOverwrite"]], FALSE)
              )
              if (!tryCatch(validateMiroScen(file.path(miroDataDir, miroDataFile)), error = function(e) {
                flog.error("Invalid miroscen file. Error message: '%s'.", conditionMessage(e))
                return(FALSE)
              })) {
                next
              }
              dfClArgs <- tryCatch(loadMiroScen(file.path(miroDataDir, miroDataFile),
                newScen, attachments, views,
                names(modelIn),
                exdir = tmpDir
              ),
              error = function(e) {
                flog.info(
                  "Problems reading miroscen file. Error message: '%s'.",
                  conditionMessage(e)
                )
                return(FALSE)
              }
              )
              if (isFALSE(dfClArgs)) {
                next
              }
              if (!is.null(scenImportConfig[["scenNameOverwrite"]])) {
                activeScen$updateMetadata(
                  newName = scenImportConfig[["scenNameOverwrite"]]
                )
              }
              miroDataFile <- "data.gdx"
            } else {
              views <- NULL
              if (is.null(viewsFileId)) {
                if (is.null(scenImportConfig[["scenNameOverwrite"]])) {
                  scenName <- tools::file_path_sans_ext(miroDataFile)
                } else {
                  scenName <- scenImportConfig[["scenNameOverwrite"]]
                }
                viewsFileId <- match(
                  paste0(scenName, "_views.json"),
                  miroDataFilesRaw
                )
              }
              if (!is.na(viewsFileId)) {
                flog.debug("Found view data for scenario: %s.", scenName)
                views <- Views$new(
                  names(modelIn),
                  names(modelOut),
                  ioConfig$inputDsNamesBase
                )
                views$addConf(safeFromJSON(read_file(file.path(miroDataDir, miroDataFilesRaw[viewsFileId])),
                  simplifyDataFrame = FALSE, simplifyVector = FALSE
                ))
              }
              newScen <- Scenario$new(
                db = db, sname = scenName, isNewScen = TRUE,
                readPerm = scenImportConfig[["readPerm"]],
                writePerm = scenImportConfig[["writePerm"]],
                execPerm = scenImportConfig[["execPerm"]],
                uid = uidAdmin, views = views,
                forceOverwrite = !identical(scenImportConfig[["forceOverwrite"]], FALSE)
              )
            }
            if (!overwriteScenToImport && db$checkSnameExists(newScen$getScenName(), newScen$getScenUid())) {
              flog.info(
                "Scenario: %s already exists and overwrite is set to FALSE. Skipping...",
                newScen$getScenName()
              )
              if (miroStoreDataOnly) {
                if (abortIfScenExists) {
                  flog.info("Scenario already exists and overwrite is set to FALSE. Aborting...")
                  write("\n", stderr())
                  write(paste0("merr:::418:::", newScen$getScenName()), stderr())
                  if (interactive()) {
                    stop()
                  }
                  quit("no", 0L)
                } else {
                  if (removeDataFile && !file.remove(file.path(miroDataDir, miroDataFiles[i]))) {
                    flog.info("Could not remove file: '%s'.", miroDataFiles[i])
                  }
                }
              }
              next
            }

            dataOut <- loadScenData(modelOut, tmpDir,
              modelOutTemplate,
              method = method,
              fileName = miroDataFile,
              xlsio = xlsio
            )
            if (length(dataOut$errors)) {
              flog.warn(
                "Some problems occurred while reading output data: %s",
                paste(dataOut$errors, collapse = ", ")
              )
            }
            dataOut <- dataOut$tabular
            dataIn <- loadScenData(
              metaData = metaDataTmp,
              workDir = tmpDir,
              templates = modelInTemplateTmp, method = method,
              fileName = miroDataFile, DDPar = DDPar, GMSOpt = GMSOpt,
              dfClArgs = dfClArgs, xlsio = xlsio
            )
            if (length(dataIn$errors)) {
              flog.warn(
                "Some problems occurred while reading input data: %s",
                paste(dataIn$errors, collapse = ", ")
              )
            }
            dataIn <- dataIn$tabular
            if (!scalarsFileName %in% names(metaDataTmp) && length(c(DDPar, GMSOpt))) {
              # additional command line parameters that are not GAMS symbols
              scalarsTemplate <- tibble(a = character(0L), b = character(0L), c = character(0L))
              names(scalarsTemplate) <- scalarsFileHeaders
              newScen$save(c(dataOut, dataIn, list(scalarsTemplate)))
            } else {
              newScen$save(c(dataOut, dataIn))
            }
            if (removeDataFile && !file.remove(file.path(miroDataDir, miroDataFiles[i]))) {
              flog.info("Could not remove file: '%s'.", miroDataFiles[i])
            }
            if (miroStoreDataOnly) {
              write("\n", stderr())
              write(
                paste0("mprog:::", round(i / length(miroDataFiles) * 100)),
                stderr()
              )
            }
          }
          if (length(tmpDirToRemove)) {
            if (identical(unlink(tmpDirToRemove, recursive = TRUE), 0L)) {
              flog.debug("Temporary directory: '%s' removed.", tmpDirToRemove)
            } else {
              flog.error("Problems removing temporary directory: '%s'.", tmpDirToRemove)
            }
          }
          if (debugMode && !forceScenImport && !identical(currentDataHashes, newDataHashes)) {
            db$deleteRows("_dataHash", force = TRUE)
            db$exportDataset(
              "_dataHash",
              tibble(
                filename = names(newDataHashes),
                hash = unlist(newDataHashes, use.names = FALSE)
              )
            )
          }
        }
      },
      error_scen_locked = function(e) {
        flog.warn(
          "Problems saving MIRO data to database: Scenario is locked"
        )
        gc()
        if (miroStoreDataOnly) {
          write("\n", stderr())
          write("merr:::423:::Scenario is locked", stderr())
          if (interactive()) {
            stop()
          }
          quit("no", 1L)
        }
      },
      error = function(e) {
        flog.error(
          "Problems saving MIRO data to database. Error message: '%s'.",
          conditionMessage(e)
        )
        gc()
        if (miroStoreDataOnly) {
          write("\n", stderr())
          write("merr:::500", stderr())
          if (interactive()) {
            stop()
          }
          quit("no", 1L)
        }
      }
    )
    if (miroStoreDataOnly) {
      if (interactive()) {
        stop()
      }
      quit("no", 0L)
    }
  })

  if (LAUNCHCONFIGMODE) {
    source("./tools/db_migration/modules/bt_delete_database.R", local = TRUE)
    source("./tools/config/server.R", local = TRUE)
    source("./tools/config/ui.R", local = TRUE)
    shinyApp(ui = ui_admin, server = server_admin)
  } else {

    # ______________________________________________________
    # \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    #                   Server
    # \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    # ______________________________________________________
    server <- function(input, output, session) {
      isMobileDevice <- isolate(session$clientData$screenwidth)
      isMobileDevice <- length(isMobileDevice) && isMobileDevice < 768L
      if (isMobileDevice) {
        hideEl(session, ".miro-show-on-desktop-devices")
        showEl(session, ".miro-show-on-mobile-devices")
      }
      newTab <- vector("list", maxNumberScenarios + 3L)
      btSortNameDesc <- FALSE
      btSortTimeDesc <- TRUE
      btSortTime <- TRUE
      btSortNameDescBase <- FALSE
      btSortTimeDescBase <- TRUE
      btSortTimeBase <- TRUE
      jobImportID <- NULL
      resetWidgetsOnClose <- TRUE
      # boolean that specifies whether output data should be saved
      saveOutput <- TRUE
      # count number of open scenario tabs
      numberScenTabs <- 0L
      # boolean that specifies whether input data shall be overridden
      overwriteInput <- 0L
      # boolean that specifies whether data shall be saved under a new name
      # or existing scenario shall be overridden
      saveAsFlag <- TRUE
      # boolean that specifies whether output data is included in currently loaded dataset
      noOutputData <- TRUE
      # count number of prepared scenarios for asynchronous solve
      asyncCount <- 1L
      asyncLogLoaded <- vector(mode = "logical", 3L)
      asyncResObs <- NULL
      traceData <- data.frame()
      # boolean that specifies whether handsontable is initialised
      hotInit <- vector("logical", length = length(modelIn))
      # counter that controls whether observer that checks whether custom widget was
      # modified should be skipped
      widgetModifiedSkipCount <- vector("integer", length = length(modelIn))
      # boolean that specifies whether check if data is unsaved should be skipped
      noCheck <- vector("logical", length = length(modelIn))
      noCheck[] <- TRUE
      # when inputs change values quickly, they sometimes lag in updating its element in
      # 'input' list. Thus, we remember currently selected values in this list
      selectedDepEl <- vector(mode = "list", length = length(modelIn))
      # list of attachments for active scenario
      attachmentList <- tibble(
        name = vector("character", attachMaxNo),
        execPerm = vector("logical", attachMaxNo)
      )
      # boolean that specifies whether input data does not match output data
      dirtyFlag <- FALSE
      inconsistentOutput <- FALSE
      if (identical(config$defCompMode, "tab")) {
        currentCompMode <- "tab"
      } else if (identical(config$defCompMode, "pivot")) {
        currentCompMode <- "pivot"
      } else {
        currentCompMode <- "split"
      }
      if (identical(currentCompMode, "split")) {
        enableEl(session, "#btCompareScen")
      }
      isInCompareMode <- FALSE
      isInRefreshMode <- FALSE
      isInSolveMode <- TRUE
      modelStatus <- NULL
      modelStatusObs <- NULL
      miroLogAnnotations <- NULL

      dynamicUILoaded <- list(
        inputGraphs = vector("logical", length(modelIn)),
        outputTablesUI = vector("logical", length(configGraphsOut)),
        outputTables = vector("logical", length(configGraphsOut)),
        compareModeTabsets = vector("logical", 3L),
        dynamicTabsets = list()
      )

      # set local working directory
      unzipModelFilesProcess <- NULL
      if (useTempDir) {
        workDir <- file.path(tmpFileDir, session$token)
        if (!config$activateModules$remoteExecution && length(modelData)) {
          tryCatch(
            {
              unzipModelFilesProcess <- unzip_process()$new(modelData,
                exdir = workDir,
                stderr = NULL
              )
            },
            error = function(e) {
              flog.error(
                "Problems creating process to extract model file archive. Error message: '%s'.",
                conditionMessage(e)
              )
            }
          )
        }
      } else {
        workDir <- currentModelDir
      }

      rv <- reactiveValues(
        unsavedFlag = FALSE, btLoadScen = 0L, btOverwriteScen = 0L, btSolve = 0L,
        btOverwriteInput = 0L, btSaveAs = 0L, btSaveConfirm = 0L, btRemoveOutputData = 0L,
        btLoadLocal = 0L, btCompareScen = 0L, activeSname = NULL, clear = TRUE, btSave = 0L,
        noInvalidData = 0L, uploadHcube = 0L, btSubmitJob = 0L, updateBatchLoadData = 0L,
        jobListPanel = 0L, importJobConfirm = 0L, importJobNew = 0L, importCSV = 0L,
        refreshHcubeHashes = 0L, submitHCJobConfirm = 0L,
        refreshLogs = NULL, triggerAsyncProcObserver = NULL
      )

      xlsio <- XlsIO$new()
      csvio <- CsvIO$new()
      views <- Views$new(
        names(modelIn),
        names(modelOut),
        ioConfig$inputDsNamesBase, rv
      )
      attachments <- Attachments$new(
        db, list(
          maxSize = attachMaxFileSize, maxNo = attachMaxNo,
          forbiddenFNames = c(
            if (identical(config$fileExchange, "gdx")) {
              c(MIROGdxInName, MIROGdxOutName)
            } else {
              paste0(c(names(modelOut), inputDsNames), ".csv")
            },
            paste0(modelNameRaw, c(".log", ".lst"))
          )
        ),
        workDir,
        names(modelIn),
        names(modelOut),
        ioConfig$inputDsNamesBase, rv
      )
      scenData <- ScenData$new(
        db = db,
        scenDataTemplate = scenDataTemplate,
        hiddenOutputScalars = config$hiddenOutputScalars
      )
      # currently active scenario (R6 object)
      activeScen <- Scenario$new(
        db = db, sname = lang$nav$dialogNewScen$newScenName,
        isNewScen = TRUE, views = views, attachments = attachments
      )
      exportFileType <- if (useGdx) "miroscen" else "csv"

      # This code will be run after the client has disconnected
      session$onSessionEnded(function() {
        # remove temporary files and folders
        if (useTempDir) {
          unlink(workDir, recursive = TRUE)
        }
        suppressWarnings(rm(activeScen))
        try(flog.info("Session ended (model: '%s', user: '%s').", modelName, uid),
          silent = TRUE
        )
        if (identical(Sys.getenv("SHINYPROXY_NOAUTH"), "true")) {
          # clean up
          try(db$deleteRows("_scenMeta", "_uid", uid), silent = TRUE)
        }
        if (config$activateModules$attachments &&
          config$storeLogFilesDuration > 0L) {
          tryCatch(
            attachments$removeExpired(
              paste0(modelNameRaw, c(".log", ".lst")),
              config$storeLogFilesDuration
            ),
            error = function(e) {
              flog.error(
                "Problems removing expired attachments. Error message: '%s'.",
                conditionMessage(e)
              )
            }
          )
        }
        activeScen$finalize()
        if (!interactive() && !isShinyProxy) {
          tryCatch(
            {
              if (length(unzipModelFilesProcess) &&
                !length(unzipModelFilesProcess$get_exit_status())) {
                unzipModelFilesProcess$kill()
              }
            },
            error = function(e) {
              flog.error(
                "Problems killing process to extract model files. Error message: '%s'.",
                conditionMessage(e)
              )
            },
            finally = {
              unzipModelFilesProcess <- NULL
            }
          )
          stopApp()
        }
      })

      if (config$activateModules$remoteExecution) {
        remoteModelId <- Sys.getenv("MIRO_ENGINE_MODELNAME", modelName)
      } else {
        remoteModelId <- modelName
      }

      worker <- Worker$new(
        metadata = list(
          uid = uid, modelName = modelName, noNeedCred = isShinyProxy,
          modelId = remoteModelId,
          maxSizeToRead = 100000,
          modelDataFiles = c(
            if (identical(config$fileExchange, "gdx")) {
              c(MIROGdxInName, MIROGdxOutName)
            } else {
              paste0(c(names(modelOut), inputDsNames), ".csv")
            },
            vapply(config$outputAttachments, "[[", character(1L), "filename", USE.NAMES = FALSE)
          ),
          MIROGdxInName = MIROGdxInName,
          clArgs = GAMSClArgs,
          textEntries = c(
            if (config$activateModules$logFile) paste0(modelNameRaw, ".log"),
            if (config$activateModules$lstFile) paste0(modelNameRaw, ".lst"),
            if (config$activateModules$miroLogFile) config$miroLogFile
          ),
          miroLogFile = config$miroLogFile,
          extraClArgs = config$extraClArgs,
          saveTraceFile = config$saveTraceFile,
          modelGmsName = modelGmsName, modelNameRaw = modelNameRaw,
          gamsSysDir = gamsSysDir, csvDelim = config$csvDelim,
          timeout = 10L, serverOS = getOS(), modelData = modelData,
          rememberMeFileName = rememberMeFileName,
          hiddenLogFile = !config$activateModules$logFile
        ),
        remote = config$activateModules$remoteExecution,
        db = db
      )
      if (length(credConfig)) {
        do.call(worker$setCredentials, credConfig)
      }
      rendererEnv <- new.env(parent = emptyenv())

      # scenario metadata of scenario saved in database
      scenMetaDb <- NULL
      scenMetaDbBase <- NULL
      scenMetaDbBaseList <- NULL
      scenTags <- NULL
      scenMetaDbSubset <- NULL
      scenMetaDbBaseSubset <- NULL
      occupiedSidSlots <- vector("logical", length = maxNumberScenarios)
      loadInLeftBoxSplit <- TRUE
      pivotCompRefreshAll <- FALSE

      # initially set rounding precision to default
      roundPrecision <- config$roundingDecimals

      flog.info(
        "Session started (model: '%s', user: '%s', workdir: '%s').",
        modelName, uid, workDir
      )

      worker$setWorkDir(workDir)
      scriptOutput <- NULL

      if (length(config$scripts$base) || length(config$scripts$hcube)) {
        source("./modules/analysis_scripts.R", local = TRUE)
      }

      if (!dir.exists(workDir) && !dir.create(workDir, recursive = TRUE)) {
        if (!dir.exists(workDir)) {
          flog.fatal("Working directory: '%s' could not be initialised.", workDir)
          showErrorMsg(lang$errMsg$fileWrite$title, lang$errMsg$fileWrite$desc)
          stop()
        }
      } else {
        flog.debug("Working directory was created: '%s'.", workDir)
      }
      # initialization of several variables
      suppressCloseModal <- FALSE
      # list of scenario IDs to load
      sidsToLoad <- list()
      loadIntoSandbox <- FALSE
      # list with input data
      modelInputData <- vector(mode = "list", length = length(modelIn))
      modelInputDataVisible <- vector(mode = "list", length = length(modelIn))
      modelInputGraphVisible <- vector(mode = "logical", length = length(modelIn))
      modelInputDataHcube <- vector(mode = "list", length = length(modelIn))
      externalInputData <- vector(mode = "list", length = length(modelIn))
      externalInputData_filtered <- vector(mode = "list", length = length(modelIn))
      # list with input data before new data was loaded as shiny is lazy when data is equal and wont update
      previousInputData <- vector(mode = "list", length = length(modelIn))
      # initialize model input data
      modelInputData <- modelInTemplate

      tryCatch(
        {
          if (length(config$defaultScenName) && nchar(trimws(config$defaultScenName))) {
            defSid <- db$getSid(config$defaultScenName)
            if (identical(defSid, 0L) && !identical(uid, uidAdmin)) {
              defSid <- db$getSid(config$defaultScenName, uid = uidAdmin)
            }
            if (!identical(defSid, 0L)) {
              sidsToLoad <- list(defSid)
              suppressCloseModal <- TRUE
              rv$btOverwriteScen <- isolate(rv$btOverwriteScen) + 1L
            }
          }
        },
        error = function(e) {
          flog.warn("Problems loading default scenario. Error message: '%s'.", conditionMessage(e))
        }
      )

      # initialise list of reactive expressions returning data for model input
      dataModelIn <- setNames(vector(mode = "list", length = length(modelIn)), names(modelIn))
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

      observeEvent(input$miroSidebar, {
        flog.debug("Sidebar menu item: '%s' selected.", isolate(input$miroSidebar))
        if (identical(input$miroSidebar, "scenarios")) {
          isInSolveMode <<- FALSE
        } else if (identical(input$miroSidebar, "importData")) {
          rv$jobListPanel <- rv$jobListPanel + 1L
        }
      })

      lapply(seq_along(modelIn), function(i) {
        widgetType <- modelIn[[i]]$type
        if (isMobileDevice && identical(widgetType, "hot")) {
          widgetType <- "dt"
        }
        if (widgetType == "hot") {
          observeEvent(input[["in_" %+% i %+% "_select"]], {
            hotInit[[i]] <<- TRUE
            isEmptyInput[[i]] <<- FALSE
            if (noCheck[i]) {
              noCheck[i] <<- FALSE
            }
          })
        }
        observe(
          {
            switch(widgetType,
              hot = {
                input[["in_" %+% i]]
              },
              dt = {
                rv[[paste0("wasModified_", i)]]
              },
              slider = {
                input[["slider_" %+% i]]
                input[["hcubeStep_" %+% i]]
                input[["hcubeMode_" %+% i]]
              },
              textinput = {
                input[["text_" %+% i]]
              },
              numericinput = {
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
              },
              custom = {
                rv[[paste0("wasModified_", i)]]
              }
            )
            if (noCheck[i]) {
              noCheck[i] <<- FALSE
              return()
            }
            # if scenario includes output data set dirty flag
            if (!noOutputData) {
              dirtyFlag <<- TRUE
              showEl(session, "#dirtyFlagIcon")
              showEl(session, "#dirtyFlagIconO")
            }
            rv$unsavedFlag <<- TRUE
            inconsistentOutput <<- TRUE
          },
          priority = 1000L
        )
      })

      markUnsaved <- function(markDirty = FALSE) {
        if (markDirty && !noOutputData) {
          showEl(session, "#dirtyFlagIcon")
          showEl(session, "#dirtyFlagIconO")
          dirtyFlag <<- TRUE
        } else {
          hideEl(session, "#dirtyFlagIcon")
          hideEl(session, "#dirtyFlagIconO")
          dirtyFlag <<- FALSE
        }
        rv$unsavedFlag <<- TRUE
        return(invisible())
      }

      markSaved <- function() {
        hideEl(session, "#dirtyFlagIcon")
        hideEl(session, "#dirtyFlagIconO")
        dirtyFlag <<- FALSE
        rv$unsavedFlag <<- FALSE
        return(invisible())
      }

      # print scenario title in input and output sheets
      getScenTitle <- reactive({
        nameSuffix <- ""
        if (rv$unsavedFlag) {
          nameSuffix <- " (*)"
        }
        if (is.null(activeScen) || !length(activeScen$getSid())) {
          if (length(rv$activeSname)) {
            if (length(activeScen)) {
              activeScen$updateMetadata(newName = rv$activeSname)
            }
            return(tags$i(paste0("<", rv$activeSname, ">", nameSuffix)))
          }
          return(tags$i(paste0("<", lang$nav$dialogNewScen$newScenName, ">", nameSuffix)))
        } else {
          scenUid <- activeScen$getScenUid()
          if (!identical(scenUid, uid)) {
            nameSuffix <- paste0(nameSuffix, ' <span class="badge badge-info">', scenUid, "</span>")
          }
          if (activeScen$isReadonlyOrLocked) {
            nameSuffix <- paste0(nameSuffix, ' <i class="fas fa-lock" role="presentation" aria-label="Readonly icon"></i>')
          }
          return(HTML(paste0(htmltools::htmlEscape(rv$activeSname), nameSuffix)))
        }
      })
      output$inputDataTitle <- renderUI(getScenTitle())
      output$outputDataTitle <- renderUI(getScenTitle())

      # activate solve button once all model input files are imported
      observe({
        datasetsImported <- vapply(names(modelInMustImport), function(el) {
          i <- match(el, names(modelIn))[[1]]
          if (length(rv[["in_" %+% i]])) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }, logical(1), USE.NAMES = FALSE)
      })
      # UI elements (modalDialogs)
      source("./UI/dialogs.R", local = TRUE)
      ####### Model input
      # get sandbox data
      source("./modules/input_save.R", local = TRUE)
      # render non tabular input datasets (e.g. slider, dropdown)
      source("./modules/input_render_nontab.R", local = TRUE)
      # render tabular input datasets
      source("./modules/input_render_tab.R", local = TRUE)
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
      if (isTRUE(config$hasSymbolLinks)) {
        source("./modules/symbol_links.R", local = TRUE)
      }

      ####### Advanced options
      if (isTRUE(config$activateModules$downloadTempFiles)) {
        source("./modules/download_tmp.R", local = TRUE)
      }

      ####### Batch Load module
      source("./modules/batch_load.R", local = TRUE)

      if (config$activateModules$remoteExecution) {
        source("./modules/gams_job_list.R", local = TRUE)
        # remote job import
        source("./modules/job_import.R", local = TRUE)
        if (identical(config$activateModules$hcube, TRUE)) {
          source("./modules/hcube_import_results.R", local = TRUE)
        }
      }

      # delete scenario
      source("./modules/db_scen_remove.R", local = TRUE)
      # scenario module
      # render scenarios (comparison mode)
      source("./modules/scen_render.R", local = TRUE)
      # load shared datasets
      source("./modules/db_external_load.R", local = TRUE)
      # load scenario
      source("./modules/db_scen_load.R", local = TRUE)
      # save scenario
      source("./modules/db_scen_save.R", local = TRUE)
      # scenario split screen mode
      source("./modules/scen_split.R", local = TRUE)
      skipScenCompObserve <- vector("logical", maxNumberScenarios + 3L)

      scenCompUpdateTab <- function(tabsetId, sheetId, groupId = NULL) {
        if (is.null(groupId)) {
          if (!identical(
            isolate(input[[paste0("contentScen_", tabsetId)]]),
            paste0("contentScen_", tabsetId, "_", sheetId)
          )) {
            skipScenCompObserve[tabsetId] <<- TRUE
          }
          updateTabsetPanel(
            session, paste0("contentScen_", tabsetId),
            paste0("contentScen_", tabsetId, "_", sheetId)
          )
        } else {
          updateTabsetPanel(
            session, paste0("contentScen_", tabsetId),
            paste0("contentScen_", tabsetId, "_", groupId)
          )
          updateTabsetPanel(
            session, paste0("contentScen_", tabsetId, "_", groupId),
            paste0("contentScen_", tabsetId, "_", groupId, "_", sheetId)
          )
        }
      }

      source("./modules/scen_compare_actions.R", local = TRUE)
      source("./modules/load_dynamic_tab_content.R", local = TRUE)

      observeEvent(input$btScenPivot_close, {
        resetCompTabset("0")
        showEl(session, "#pivotCompBtWrapper")
        hideEl(session, "#pivotCompScenWrapper")
        isInRefreshMode <<- FALSE
        scenData$clear("cmpPivot")
        if (!is.null(dynamicUILoaded$dynamicTabsets[["tab_0"]])) {
          dynamicUILoaded$dynamicTabsets[["tab_0"]][["content"]][] <<- FALSE
        }
        disableEl(session, "#btClosePivotComp")
      })
      lapply(seq(0L, maxNumberScenarios + 3L), function(i) {
        scenIdLong <- paste0("scen_", i, "_")
        # compare scenarios
        observe({
          # we need to duplicate code from getSheetnamesByTabsetId here to have
          # reactive dependencies
          if (identical(i, 1L)) {
            tabsetName <- "outputTabset"
            isOutputTabset <- TRUE
          } else {
            tabsetName <- paste0("contentScen_", i)
            isOutputTabset <- FALSE
          }
          tabIdFull <- input[[tabsetName]]
          if (is.null(tabIdFull)) {
            return()
          }
          groupId <- as.integer(strsplit(tabIdFull, "_", fixed = TRUE)[[1]][3L - isOutputTabset])
          tabId <- NULL
          if (groupId <= length(isGroupOfSheets) && isGroupOfSheets[[groupId]]) {
            tabId <- as.integer(strsplit(input[[paste0(tabsetName, "_", groupId)]],
              "_",
              fixed = TRUE
            )[[1L]][[4L - isOutputTabset]])
          }
          if (!length(scenData$getRefScenMap(tabIdToRef(i)))) {
            return()
          }
          if (groupId > length(outputTabs)) {
            if (isOutputTabset) {
              # is script tab
              return()
            }
            groupId <- groupId - length(outputTabs)
            if (groupId > length(scenInputTabs)) {
              # is script tab
              return()
            }
            sheetName <- scenInputTabs[[groupId]]
            if (identical(sheetName, 0L)) {
              sheetNames <- scalarsFileName
            } else {
              sheetNames <- names(modelIn)[sheetName]
            }
          } else {
            sheetNames <- names(modelOut)[outputTabs[[groupId]]]
          }
          loadDynamicTabContent(session, i, if (is.null(tabId)) sheetNames else sheetNames[tabId])
        })
        if (i > 0L) {
          obsCompare[[i]] <<- observe(
            {
              if (is.null(input[[paste0("contentScen_", i)]]) ||
                skipScenCompObserve[i]) {
                skipScenCompObserve[i] <<- FALSE
                return(NULL)
              }
              j <- as.integer(strsplit(isolate(input[[paste0("contentScen_", i)]]),
                "_",
                fixed = TRUE
              )[[1]][[3L]])
              groupId <- NULL
              if (isGroupOfSheets[[j]]) {
                groupId <- j
                j <- strsplit(input[[paste0("contentScen_", i, "_", groupId)]],
                  "_",
                  fixed = TRUE
                )[[1L]][[4L]]
              }
              if (identical(i, 2L)) {
                scenCompUpdateTab(i + 1L, sheetId = j, groupId = groupId)
              } else if (identical(i, 3L)) {
                scenCompUpdateTab(i - 1L, sheetId = j, groupId = groupId)
              } else {
                lapply(which(occupiedSidSlots) + 3L, function(tabsetId) {
                  scenCompUpdateTab(tabsetId,
                    sheetId = j, groupId = groupId
                  )
                })
              }
            },
            suspended = TRUE
          )
        }
      })

      # scenario comparison
      source("./modules/scen_compare.R", local = TRUE)

      observeEvent(input$btExportScen, {
        stopifnot(is.integer(input$btExportScen), length(input$btExportScen) == 1L)
        if (useGdx) {
          exportTypes <- setNames(c("miroscen", "gdx", "csv", "xls"), lang$nav$fileExport$fileTypes)
        } else {
          exportTypes <- setNames(c("csv", "xls"), lang$nav$fileExport$fileTypes[-c(1, 2)])
        }
        if (length(datasetsRemoteExport)) {
          exportTypes <- c(exportTypes, setNames(
            names(datasetsRemoteExport),
            names(datasetsRemoteExport)
          ))
        }
        showScenExportDialog(input$btExportScen, exportTypes)
      })
      observeEvent(input$exportFileType, {
        stopifnot(length(input$exportFileType) > 0L)

        if (length(datasetsRemoteExport) &&
          input$exportFileType %in% names(datasetsRemoteExport)) {
          hideEl(session, ".file-export")
          showEl(session, ".remote-export")
          return()
        }

        switch(input$exportFileType,
          xls = exportFileType <<- "xlsx",
          gdx = exportFileType <<- "gdx",
          csv = exportFileType <<- "csv",
          miroscen = exportFileType <<- "miroscen",
          flog.warn("Unknown export file type: '%s'.", input$exportFileType)
        )
      })
      hideEl(session, "#loading-screen")
    }

    # ______________________________________________________
    # \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    #                 UI
    # \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    # ______________________________________________________


    source("./UI/scen_tabset.R", local = TRUE)
    source("./UI/header.R", local = TRUE)
    source("./UI/sidebar.R", local = TRUE)
    source("./UI/body.R", local = TRUE)

    ui <- dashboardPage(header, sidebar, miroBody, skin = "black")

    app <- shinyApp(ui = ui, server = server)
  }
}
