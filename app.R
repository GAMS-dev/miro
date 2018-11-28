#version number
webuiVersion <- "0.2.6.1"
webuiRDate   <- "Oct 30 2018"
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

# RPostgres       #GPL-2
# DBI (database)  #LGPL >=2
# RSQLite(database) #LGPL >=2
# openssl (batch) #MIT


# specify CRAN mirror (for list of mirrors, see: https://cran.r-project.org/mirrors.html)
CRANMirror <- "http://cran.us.r-project.org"
errMsg <- NULL
if(R.version[["major"]] < 3 || 
   R.version[["major"]] == 3 && gsub("\\..$", "", R.version[["minor"]]) < 5){
  errMsg <- "The R version you are using is not supported. At least version 3.5 is required to run the GAMS WebUI."
}
tmpFileDir <- tempdir(TRUE)
# directory of configuration files
configDir <- "./conf/"
# files that require schema file
jsonFilesWithSchema <- c("config", "GMSIO_config", "db_config")
# vector of required files
filesToInclude <- c("./global.R", "./R/util.R", "./R/json.R", "./R/output_load.R", "./modules/render_data.R")
# required packages
requiredPackages <- c("R6", "stringi", "shiny", "shinydashboard", "DT", "processx", 
                      "V8", "dplyr", "readr", "readxl", "writexl", "rhandsontable", 
                      "plotly", "jsonlite", "jsonvalidate", "rpivotTable", 
                      "futile.logger", "dygraphs", "xts", "zip", "tidyr")

if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
  pb <- winProgressBar(title = "Loading WebUI", label = "Loading required packages",
                       min = 0, max = 1, initial = 0, width = 300)
  setWinProgressBar(pb, 0.1)
  on.exit(close(pb))
}
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
if(identical(gamsSysDir, "") || !dir.exists(paste0(gamsSysDir, "GMSWebUI", 
                                                   .Platform$file.sep, "library"))){

  RLibPath = NULL

}else{
  RLibPath = paste0(gamsSysDir, "GMSWebUI", .Platform$file.sep, "library") 
}
source("./R/install_packages.R", local = TRUE)
if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
  setWinProgressBar(pb, 0.6, label= "Initialising GAMS WebUI")
}
if(is.null(errMsg)){
  # include custom functions and modules
  lapply(filesToInclude, function(file){
    if(!file.exists(file)){
      errMsg <<- paste0("Include file '", file, "' could not be located.")
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
  # check whether shiny proxy is used to access this file
  isShinyProxy <- isShinyProxy()
  # get model path and name
  tryCatch({
    modelPath <- paste0(getwd(), .Platform$file.sep, modelDir, modelName, 
                        .Platform$file.sep, modelName, ".gms")
    modelPath <- getModelPath(modelPath, isShinyProxy, spModelPathEnvVar, paste0(getwd(), .Platform$file.sep, modelDir))
    modelGmsName <- modelPath[[2]]
    modelName <- modelPath[[3]]
    modelPath <- modelPath[[1]]
  }, error = function(e){
    errMsg <<- "The GAMS model name could not be identified. Please make sure you specify the name of the model you want to solve."
  })
}

if(is.null(errMsg)){
  logFileDir <- file.path(tmpFileDir, logFileDir)
  # check if GAMS model file exists
  if(file.exists(modelPath %+% modelGmsName)){
    currentModelDir  <- modelPath
  }else{
    errMsg <- sprintf("The GAMS model file: '%s' could not be found in the directory: '%s'." %+%
"Please make sure you specify a valid gms file path.", modelGmsName, modelPath)
  }
}
if(is.null(errMsg)){
  # name of the R save file
  rSaveFilePath <- paste0(currentModelDir, modelName, '_', webuiVersion, '.RData')
  # set user ID (user name) and user groups
  if(isShinyProxy){
    uid <- Sys.getenv("SHINYPROXY_USERNAME")
    if(is.null(uid) || grepl("^\\s*$", uid)){
      errMsg <- "No user ID specified (shinyproxy)."
    }
    ugroups <- Sys.getenv("SHINYPROXY_USERGROUPS")
    if(is.null(ugroups) || grepl("^\\s*$", ugroups)){
      errMsg <- paste(errMsg, "No user groups specified (shinyproxy).", sep = "\n")
    }
  }else{
    if(length(uid) != 1 || !is.character(uid)){
      errMsg <- "Invalid user ID specified."
    }
  }
  #initialise loggers
  if(!dir.exists(logFileDir)){
    tryCatch({
      dir.create(file.path(logFileDir), showWarnings = FALSE)
    }, warning = function(w){
      errMsg <<- "Log file directory could not be created. Check that you have sufficient read/write permissions in application folder."
    })
  }
  flog.appender(appender.file(file.path(logFileDir, paste0(modelName, "_", uid, "_", 
                                     format(Sys.time(), "%y.%m.%d_%H.%M.%S"), ".log"))))
  flog.threshold(loggingLevel)
  flog.trace("Logging facility initialised.")
  
  if(!file.exists(rSaveFilePath) || developMode){
    source("./modules/init.R", local = TRUE)
  }else{
    load(rSaveFilePath, envir = .GlobalEnv)
  }
}
if(is.null(errMsg)){
  # load default and custom renderers (output data)
  customRendererDirs <<- paste0(c(modelDir, currentModelDir), customRendererDirName, .Platform$file.sep)
  rendererFiles <- list.files("./modules/renderers/", pattern = "\\.R$")
  for(file in rendererFiles){
    if(!file.access("./modules/renderers/" %+% file, mode = 4)){
      tryCatch({
        source("./modules/renderers/" %+% file)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing renderer file '%s'. Error message: %s.", 
                                 file, e), sep = "\n")
      }, warning = function(w){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing renderer file '%s'. Error message: %s.", 
                                 file, w), sep = "\n")
      })
    }else{
      errMsg <- "File: '" %+% file %+% "' could not be found or user has no read permissions."
    }
  }
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
  
  requiredPackages <- NULL
  for(customRendererConfig in configGraphsOut){
    # check whether non standard renderers were defined in graph config
    if(any(is.na(match(tolower(customRendererConfig$outType), standardRenderers)))){
      customRendererName <- "render" %+% toupper(substr(customRendererConfig$outType, 1, 1)) %+% 
        substr(customRendererConfig$outType, 2, nchar(customRendererConfig$outType))
      customRendererOutput <- customRendererConfig$outType %+% "Output"
      # find render function
      tryCatch({
        match.fun(customRendererName)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("A custom renderer function: '%s' was not found. Please make sure first define such a function.", 
                                 customRendererName), sep = "\n")
      })
      # find output function
      tryCatch({
        match.fun(customRendererOutput)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("No output function for custom renderer function: '%s' was found. Please make sure you define such a function.", 
                                 customRendererName), sep = "\n")
      })
      # find packages to install and install them
      if(!is.null(customRendererConfig$packages) && length(customRendererConfig$packages)){
        requiredPackages <- c(requiredPackages, customRendererConfig$packages)
      }
    }
  }
  if(!is.null(requiredPackages)){
    requiredPackages <- unique(requiredPackages)
    source("./R/install_packages.R", local = TRUE)
  }
}

if(is.null(errMsg)){ 
  # try to create the DB connection (PostgreSQL)
  if(config$activateModules$scenario){
    requiredPackages <- c("DBI", "RPostgres")
    source("./R/install_packages.R", local = TRUE)
    
    source("./R/db.R")
    source("./R/db_scen.R")
    tryCatch({
      scenMetadataTable <- scenMetadataTablePrefix %+% modelName
      db   <- Db$new(uid = uid, host = config$db$host, username = config$db$username, 
                     password = config$db$password, dbname = config$db$name,
                     uidIdentifier = uidIdentifier, sidIdentifier = sidIdentifier, 
                     snameIdentifier = snameIdentifier, stimeIdentifier = stimeIdentifier,
                     slocktimeIdentifier = slocktimeIdentifier, stagIdentifier = stagIdentifier,
                     accessIdentifier = accessIdentifier, tableNameMetadata = scenMetadataTable, 
                     tableNameScenLocks = scenLockTablePrefix %+% modelName, 
                     tableNamesScenario = scenTableNames, 
                     slocktimeLimit = slocktimeLimit, port = config$db$port, type = config$db$type,
                     tableNameTrace = tableNameTracePrefix %+% modelName, traceColNames = traceColNames,
                     attachmentConfig = if(config$activateModules$attachments) list(tabName = tableNameAttachPrefix %+% modelName,
                                                                                   maxSize = attachMaxFileSize, maxNo = attachMaxNo)
                     else NULL)
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
  if(config$activateModules$batchMode){
    requiredPackages <- c("openssl")
    source("./R/install_packages.R", local = TRUE)
    
    source("./R/batch.R")
    source("./R/db_batchimport.R")
    source("./R/db_batchload.R")
  }
}
if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
  setWinProgressBar(pb, 1, label= "GAMS WebUI initialised")
  close(pb)
}
if(!is.null(errMsg)){
  ui_initError <- fluidPage(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = "webui.css")
    ),
    titlePanel(
      if(!exists("lang") || is.null(lang$errMsg$initErrors$title)){
        "Some errors occurred"
      }else{
        lang$errMsg$initErrors$title
      }),
    fluidRow(align="center",
             HTML("<br>"),
             div(
               if(!exists("lang") || is.null(lang$errMsg$initErrors$desc)){
                 "Please fix the errors mentioned below and restart the GAMS WebUI:"
               }else{
                 lang$errMsg$initErrors$desc
               }
               , class = "initErrors"),
             HTML("<br>"),
             verbatimTextOutput("errorMessages"),
             tableOutput("JSONErrorMessages")
    )
  )
  server_initError <- function(input, output, session){
    output$errorMessages <- renderText(
      errMsg
    )
    output$JSONErrorMessages <- renderTable(
      if(exists("jsonErrors")) jsonErrors, bordered = TRUE
    )
    session$onSessionEnded(function() {
      if(!interactive()){
        stopApp()
        q("no")
      }
    })
  }
  
  shinyApp(ui = ui_initError, server = server_initError)
  
}else{
  #______________________________________________________
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #                   Server
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #______________________________________________________
  server <- function(input, output, session){
    options(shiny.maxRequestSize=100*1024^2) 
    newTab <- vector("list", maxNumberScenarios + 3)
    flog.info("Session started (model: '%s').", modelName)
    btSortNameDesc     <- FALSE
    btSortTimeDesc     <- TRUE
    btSortTime         <- TRUE
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
    # list of attachments for active scenario
    attachmentList     <- vector("character", attachMaxNo)
    # boolean that specifies whether input data does not match output data
    dirtyFlag          <- FALSE
    isInSplitView      <- if(identical(config$defCompMode, "split")) TRUE else FALSE
    if(isInSplitView){
      enableEl(session, "#btCompareScen")
    }
    isInCompareMode    <- FALSE
    isInSolveMode      <- TRUE
    
    if(config$activateModules$scenario){
      scenMetaData     <- list()
      # scenario metadata of scenario saved in database
      scenMetaDb       <- NULL
      # currently active scenario (R6 object)
      activeScen       <- NULL
      # temporary name and sid of the scenario currently active in the UI
      activeSnameTmp   <- NULL
      scenTags         <- NULL
      scenMetaDbSubset <- NULL
      # save the scenario ids loaded in UI
      scenCounterMultiComp <- 4L
      sidsInComp       <- vector("integer", length = maxNumberScenarios + 1)
      sidsInSplitComp  <- vector("integer", length = 2)
      # occupied slots (scenario is loaded in ui with this rv$scenId)
      occupiedSidSlots <- vector("logical", length = maxNumberScenarios)
      # scenId of tabs that are loaded in ui (used for shortcuts) (in correct order)
      sidCompOrder     <- NULL
      loadInLeftBoxSplit <- TRUE
      # boolean that specifies whether nested tabset is active or not
      shortcutNest     <- FALSE
      observeEvent(input$tabsetShortcutNest, {
        if(isolate(input$sidebarMenuId) == "scenarios" && !is.null(sidCompOrder)){
          shortcutNest <<- TRUE
        }
      })
      observeEvent(input$tabsetShortcutUnnest, {
        if(isolate(input$sidebarMenuId) == "scenarios" && !is.null(sidCompOrder)){
          shortcutNest <<- FALSE
        }
      })
    }
    # trigger navigation through tabs by shortcuts
    observeEvent(input$tabsetShortcutNext, {
      if(isolate(input$sidebarMenuId) == "inputData"){
        flog.debug("Navigated to next input tab (using shortcut).")
        currentSheet <- as.numeric(gsub("\\D", "", isolate(input$inputTabset)))
        updateTabsetPanel(session, "inputTabset", paste0("inputTabset_", currentSheet + 1))
      }else if(isolate(input$sidebarMenuId) == "outputData"){
        flog.debug("Navigated to next output tab (using shortcut).")
        currentSheet <- as.numeric(gsub("\\D", "", isolate(input$contentCurrent)))
        updateTabsetPanel(session, "contentCurrent", paste0("contentCurrent_", currentSheet + 1))
      }else if(isolate(input$sidebarMenuId) == "scenarios"){
        if(!is.null(sidCompOrder) && !isInSplitView){
          currentScen <- as.numeric(gsub("\\D", "", isolate(input$scenTabset)))
          if(shortcutNest){
            flog.debug("Navigated to next data tab in scenario comparison view (using shortcut).")
            # go to next data sheet
            currentSheet <- as.numeric(gsub("\\D+_\\d+_", "", 
                                            isolate(input[[paste0("contentScen_", currentScen)]])))
            updateTabsetPanel(session, paste0("contentScen_", currentScen), 
                              paste0("contentScen_", currentScen, "_", currentSheet + 1))
          }else{
            flog.debug("Navigated to next scenario tab in scenario comparison view (using shortcut).")
            # go to next scenario tab
            idx <- which(sidCompOrder == currentScen)[1]
            if(identical(idx, length(sidCompOrder))){
              return(NULL)
            }
            updateTabsetPanel(session, "scenTabset", paste0("scen_", sidCompOrder[idx + 1], "_"))
          }
        }else if(isInSplitView){
          flog.debug("Navigated to next data tab in split view scenario comparison view (using shortcut).")
          currentScen <- 2
          currentSheet <- as.numeric(gsub("\\D+_\\d+_", "", 
                                          isolate(input[[paste0("contentScen_", currentScen)]])))
          updateTabsetPanel(session, paste0("contentScen_", currentScen), 
                            paste0("contentScen_", currentScen, "_", currentSheet + 1))
          updateTabsetPanel(session, paste0("contentScen_", currentScen + 1), 
                            paste0("contentScen_", currentScen + 1, "_", currentSheet + 1))
        }
      }
    })
    observeEvent(input$tabsetShortcutPrev,{
      if(isolate(input$sidebarMenuId) == "inputData"){
        flog.debug("Navigated to previous input tab (using shortcut).")
        currentSheet <- as.numeric(gsub("\\D", "", isolate(input$inputTabset)))
        updateTabsetPanel(session, "inputTabset", paste0("inputTabset_", currentSheet - 1))
      }else if(isolate(input$sidebarMenuId) == "outputData"){
        flog.debug("Navigated to previous output tab (using shortcut).")
        currentSheet <- as.numeric(gsub("\\D", "", isolate(input$contentCurrent)))
        updateTabsetPanel(session, "contentCurrent", paste0("contentCurrent_", currentSheet - 1))
      }else if(isolate(input$sidebarMenuId) == "scenarios"){
        if(!is.null(sidCompOrder) && !isInSplitView){
          currentScen <- as.numeric(gsub("\\D", "", isolate(input$scenTabset)))
          if(shortcutNest){
            flog.debug("Navigated to previous data tab in single scenario comparison view (using shortcut).")
            # go to previous data sheet
            currentSheet <- as.numeric(gsub("\\D+_\\d+_", "", 
                                            isolate(input[[paste0("contentScen_", currentScen)]])))
            updateTabsetPanel(session, paste0("contentScen_", currentScen), 
                              paste0("contentScen_", currentScen, "_", currentSheet - 1))
          }else{
            flog.debug("Navigated to previous scenario tab in scenario comparison view (using shortcut).")
            # go to previous scenario tab
            idx <- which(sidCompOrder == currentScen)[1]
            if(identical(idx, 1)){
              return(NULL)
            }
            updateTabsetPanel(session, "scenTabset", paste0("scen_", sidCompOrder[idx - 1], "_"))
          }
        }else if(isInSplitView){
          flog.debug("Navigated to previous data tab in split view scenario comparison view (using shortcut).")
          currentScen <- 2
          currentSheet <- as.numeric(gsub("\\D+_\\d+_", "", 
                                          isolate(input[[paste0("contentScen_", currentScen)]])))
          updateTabsetPanel(session, paste0("contentScen_", currentScen), 
                            paste0("contentScen_", currentScen, "_", currentSheet - 1))
          updateTabsetPanel(session, paste0("contentScen_", currentScen + 1), 
                            paste0("contentScen_", currentScen + 1, "_", currentSheet - 1))
        }
        
      }
    })
    
    observeEvent(input$aboutDialog, {
      showModal(modalDialog(HTML(paste0("<b>GAMS WebUI v.", webuiVersion, "</b><br/><br/>",
                                   "Release Date: ", webuiRDate, "<br/>", 
                                   "Copyright (c) 2018 GAMS Software GmbH <support@gams.com><br/>
                                   Copyright (c) 2018 GAMS Development Corp. <support@gams.com><br/><br/>
                                   This program is free software: you can redistribute it and/or modify 
                                   it under the terms of the GNU General Public License as published by
                                   the Free Software Foundation, either version 3 of the License, or 
                                   (at your option) any later version.<br/><br/>
                                   This program is distributed in the hope that it will be useful, 
                                   but WITHOUT ANY WARRANTY; without even the implied warranty of
                                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
                                   GNU General Public License for more details.<br/><br/>
                                   You should have received a copy of the GNU General Public License 
                                   along with this program. If not, see 
                                   <a href=\"http://www.gnu.org/licenses/\" target=\"_blank\">http://www.gnu.org/licenses/</a>.<br/><br/>
                                   The source code of the program can be accessed at 
                                   <a href=\"https://github.com/GAMS-dev/webui\" target=\"_blank\">
                                   https://github.com/GAMS-dev/webui/</a>.")),
                            title = "About GAMS WebUI"))
    })
    
    # initially set rounding precision to default
    roundPrecision <- config$roundingDecimals
    
    # set local working directory
    workDir <- paste0(tmpFileDir, .Platform$file.sep, session$token, .Platform$file.sep)
    if(!dir.create(file.path(workDir), recursive = TRUE)){
      flog.fatal("Working directory could not be initialised.")
      showErrorMsg(lang$errMsg$fileWrite$title, lang$errMsg$fileWrite$desc)
      return(NULL)
    }else{
      flog.debug("Working directory was created: '%s'.", workDir)
    }
    
    # initialization of several variables
    rv <- reactiveValues(scenId = 4L, datasetsImported = vector(mode = "logical", 
                                                                length = length(modelInMustImport)), 
                         unsavedFlag = TRUE, btLoadScen = 0L, btOverwriteScen = 0L, btOverwriteInput = 0L, 
                         btSaveAs = 0L, btSaveConfirm = 0L, btRemoveOutputData = 0L, btLoadLocal = 0L, 
                         btCompareScen = 0L, activeSname = NULL, clear = TRUE, btSave = 0L, 
                         btSplitView = 0L, btPaver = 0L, noInvalidData = 0L)
    # list of scenario IDs to load
    sidsToLoad <- list()
    # list with input data
    modelInputData  <- vector(mode = "list", length = length(modelIn))
    sharedInputData <- vector(mode = "list", length = length(modelIn))
    sharedInputData_filtered <- vector(mode = "list", length = length(modelIn))
    # list with input data before new data was loaded as shiny is lazy when data is equal and wont update
    previousInputData <- vector(mode = "list", length = length(modelIn))
    noDataChanges     <- vector(mode = "logical", length = length(modelIn))
    # initialize model input data
    modelInputData <- modelInTemplate
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
      if((config$activateModules$scenario || config$activateModules$batchMode)
          && input$sidebarMenuId == "scenarios"){
        # reset nest level
        shortcutNest <<- FALSE
        isInSolveMode <<- FALSE
      }
    })
    
    # activate solve button when all datasets that have to be 
    # imported are actually imported
    lapply(seq_along(modelIn), function(i){
      if(modelIn[[i]]$type == "hot"){
        observeEvent(input[["in_" %+% i %+% "_select"]], {
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
        if(isolate(rv$unsavedFlag)){
          return()
        }
        # set unsaved flag
        rv$unsavedFlag <<- TRUE
        # if scenario includes output data set dirty flag
        if(!noOutputData){
          dirtyFlag <<- TRUE
          showEl(session, "#dirtyFlagIcon")
          showEl(session, "#dirtyFlagIconO")
        }
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
    output$inputDataTitle <- renderText(getScenTitle())
    output$outputDataTitle <- renderText(getScenTitle())
    
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
    source("./modules/excel_input_load.R", local = TRUE)
    
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
    if(config$activateModules$batchMode){
      ####### Batch import module
      source("./modules/batch_import.R", local = TRUE)
      ####### Batch load module
      source("./modules/batch_load.R", local = TRUE)
      # analyze button clicked
      source("./modules/paver_run.R", local = TRUE)
      # Interrupt button clicked
      source("./modules/paver_interrupt.R", local = TRUE)
    }
    
    # delete scenario 
    source("./modules/db_scen_remove.R", local = TRUE)
    # scenario module
    if(config$activateModules$scenario){
      #load shared datasets
      source("./modules/db_shared_load.R", local = TRUE)
      # load scenario
      source("./modules/db_scen_load.R", local = TRUE)
      # save scenario
      source("./modules/db_scen_save.R", local = TRUE)
      # scenario split screen mode
      source("./modules/scen_split.R", local = TRUE)

      lapply(seq_len(maxNumberScenarios + 3), function(i){
        scenIdLong <- paste0("scen_", i, "_")
        # table view
        source("./modules/scen_table_view.R", local = TRUE)
        
        # close scenario tab
        source("./modules/scen_close.R", local = TRUE)
        
        # export Scenario to Excel spreadsheet
        source("./modules/excel_scen_save.R", local = TRUE)
        
        # compare scenarios
        obsCompare[[i]] <<- observe({
          if(is.null(input[[paste0("contentScen_", i)]])){
            return(NULL)
          }
          j <- strsplit(input[[paste0("contentScen_", i)]], "_")[[1]][3]
          
          if(i < maxNumberScenarios + 2){
            lapply(isolate(names(scenData)), function(scen){
              k <- strsplit(scen, "_")[[1]][2]
              updateTabsetPanel(session, paste0("contentScen_", k),
                                paste0(paste0("contentScen_", k, "_", j)))
            })
          }else if(i == maxNumberScenarios + 2){
            updateTabsetPanel(session, paste0("contentScen_", i + 1),
                              paste0(paste0("contentScen_", i + 1, "_", j)))
          }else{
            updateTabsetPanel(session, paste0("contentScen_", i - 1),
                              paste0(paste0("contentScen_", i - 1, "_", j)))
          }
        }, suspended = TRUE)
      })
      
      # scenario comparison
      source("./modules/scen_compare.R", local = TRUE)
    }else{
      id <- 1
      # export output data to Excel spreadsheet
      source("./modules/excel_scen_save.R", local = TRUE)
    }
    hideEl(session, "#loading-screen")
    # This code will be run after the client has disconnected
    session$onSessionEnded(function() {
      # remove temporary files and folders
      unlink(file.path(workDir), recursive=TRUE)
      suppressWarnings(rm(activeScen))
      try(flog.info("Session ended (model: '%s').", modelName))
      gc()
      if(!interactive()){
        stopApp()
        q("no")
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
  
  ui <- dashboardPage(header, sidebar, body, skin = config$pageSkin)
  
  app <- shinyApp(ui = ui, server = server)
}