#version number
webuiVersion <- '0_2_1'
#####packages:
# processx        #MIT
# dplyr           #MIT
# readxl          #GPL-3
# writexl         #BSD2-clause
# rhandsontable   #MIT
# shiny           #GPL-3
# shinydashboard  #GPL v2
# shinyjs         #AGPL
# plotly          #MIT
# DT              #GPL-3
# V8              #MIT
# jsonlite        #MIT
# jsonvalidate    #MIT
# RPostgres       #GPL-2
# rpivotTable     #MIT
# futile.logger   #LGPL-3
# specify CRAN mirror (for list of mirrors, see: https://cran.r-project.org/mirrors.html)

CRANMirror <- "http://cran.us.r-project.org"
errMsg <- NULL
# directory of configuration files
configDir <- "./conf/"
# files that require schema file
jsonFilesWithSchema <- c("config", "GMSIO_config", "db_config")
# vector of required files
filesToInclude <- c("./global.R", "./R/util.R", "./R/shiny_proxy.R", 
                    "./R/json.R", "./R/output_load.R", "./modules/render_data.R")
# required packages
requiredPackages <- c("R6", "shiny", "shinydashboard", "shinyjs", "DT", "processx", 
                      "V8", "dplyr", "readr", "readxl", "writexl", "rhandsontable", 
                      "plotly", "jsonlite", "jsonvalidate", "rpivotTable", 
                      "futile.logger", "dygraphs", "reshape2", "xts")
source("./R/install_packages.R", local = TRUE)

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
  # get model Name
  tryCatch({
    modelName <- getModelName(modelName, isShinyProxy, sp.modelName.env.var)
  }, error = function(e){
    errMsg <<- "The GAMS model name could not be identified. Please make sure you specify the name of the model you want to solve."
  })
  # check if GAMS model file exists
  if(file.exists(modelDir %+% tolower(modelName) %+% .Platform$file.sep %+% tolower(modelName) %+% ".gms")){
    currentModelDir  <- modelDir %+% tolower(modelName) %+% .Platform$file.sep
  }else{
    errMsg <- paste0("The GAMS model file could not be found. Please make sure you create a new directory for your model in : '",
                     modelDir, "', so that it looks like this: '", modelDir, tolower(modelName), 
                     .Platform$file.sep, tolower(modelName), ".gms'")
  }
  # name of the R save file
  rSaveFilePath <- paste0(currentModelDir, tolower(modelName), '_', webuiVersion, '.RData')
}
if(is.null(errMsg)){
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
  flog.appender(appender.file(logFileDir %+% modelName %+% "_" %+% uid %+% "_" %+% format(Sys.time(), "%y.%m.%d_%H.%M.%S") %+% ".log"))
  flog.threshold(loggingLevel)
  flog.trace("Logging facility initialised.")
  
  if(!file.exists(rSaveFilePath) || debug.mode){
    source("./modules/init.R", local = TRUE)
  }else{
    load(rSaveFilePath, envir = .GlobalEnv)
  }
}
if(is.null(errMsg)){ 
  # load default and custom renderers (output data)
  customRendererDir <<- paste0(currentModelDir, customRendererDirName, .Platform$file.sep)
  rendererFiles <- list.files("./modules/renderers/", pattern = "\\.R$")
  for(file in rendererFiles){
    if(!file.access("./modules/renderers/" %+% file, mode = 4)){
      tryCatch({
        source("./modules/renderers/" %+% file)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing renderer file '%s'. Error message: %s.", file, e), sep = "\n")
      }, warning = function(w){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing renderer file '%s'. Error message: %s.", file, w), sep = "\n")
      })
    }else{
      errMsg <- "File: '" %+% file %+% "' could not be found or user has no read permissions."
    }
  }

  rendererFiles <- list.files(customRendererDir, pattern = "\\.R$")
  lapply(rendererFiles, function(file){
    if(!file.access(customRendererDir %+% file, mode = 4)){
      tryCatch({
        source(customRendererDir %+% file)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing custom renderer file '%s'. Error message: %s.", file, e), sep = "\n")
      }, warning = function(w){
        errMsg <<- paste(errMsg, 
                         sprintf("Some error occurred while sourcing custom renderer file '%s'. Error message: %s.", file, w), sep = "\n")
      })
    }else{
      errMsg <<- paste(errMsg, sprintf("Custom renderer file: '%s' could not be found or user has no read permissions.",
                                       file), sep = "\n")
    }
  })
  requiredPackages <- NULL
  for(customRendererConfig in config.graphs.out){
    # check whether non standard renderers were defined in graph config
    if(any(is.na(match(tolower(customRendererConfig$outType), standardRenderers)))){
      customRendererName <- "render" %+% toupper(substr(customRendererConfig$outType, 1, 1)) %+% 
        tolower(substr(customRendererConfig$outType, 2, nchar(customRendererConfig$outType)))
      customRendererOutput <- tolower(customRendererConfig$outType) %+% "Output"
      # find render function
      tryCatch({
        match.fun(customRendererName)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("A custom renderer function: '%s' was not found. Please make sure first define such a function.", customRendererName), sep = "\n")
      })
      # find output function
      tryCatch({
        match.fun(customRendererOutput)
      }, error = function(e){
        errMsg <<- paste(errMsg, 
                         sprintf("No output function for custom renderer function: '%s' was found. Please make sure you define such a function.", customRendererName), sep = "\n")
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
    requiredPackages <- c("RPostgres", "DBI")
    source("./R/install_packages.R", local = TRUE)
    
    source("./R/db.R")
    source("./R/db_scen.R")
    tryCatch({
      scen.metadata.table <- scen.metadata.table.prefix %+% modelName
      db   <- Db$new(uid = uid, host = config$db$host, username = config$db$username, 
                     password = config$db$password, dbname = config$db$name,
                     uidIdentifier = uid.identifier, sidIdentifier = sid.identifier, 
                     snameIdentifier = sname.identifier, stimeIdentifier = stime.identifier,
                     slocktimeIdentifier = slocktime.identifier, stagIdentifier = stagIdentifier,
                     accessIdentifier = access.identifier, tableNameMetadata = scen.metadata.table, 
                     tableNameScenLocks = scenLock.table.prefix %+% modelName, 
                     tableNamesScenario = scen.table.names, 
                     slocktimeLimit = slocktimeLimit, port = config$db$port, type = config$db$type)
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
                       tableNameGroups = am.tableName.groups, 
                       tableNameElements = am.tableName.elements, 
                       tableNameHierarchy = am.tableName.hierarchy, 
                       tableNameMetadata = scen.metadata.table, 
                       uidIdentifier = uid.identifier, 
                       accessIdentifier = access.identifier, 
                       accessElIdentifier = access.el.identifier)
      db$accessGroups <- auth$getAccessGroups()
      flog.debug("Access Control initialised.")
    }, error = function(e){
      flog.error("Problems initialising authorisation class. Error message: %s", e)
      errMsg <<- paste(errMsg, conditionMessage(e), sep = '\n')
    })
  }
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
                 "Please fix the errors mentioned below and restart Shiny:"
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
  }
  
  shinyApp(ui = ui_initError, server = server_initError)
  
}else{
  #______________________________________________________
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #                   Server
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #______________________________________________________
  server <- function(input, output, session){
    newTab <- vector("list", maxNumberScenarios + 3)
    #modelSolved <- 0
    #noError <- 1
    flog.info("Session started (model: '%s').", modelName)
    bt.sortName.desc   <- FALSE
    bt.sortTime.desc   <- TRUE
    # boolean that specifies whether output data should be saved
    save.output        <- TRUE
    # count number of open scenario tabs
    number.scen.tabs   <- 0L
    # boolean that specifies whether input data shall be overridden
    overrideInput      <- FALSE
    # boolean that specifies whether data shall be saved under a new name or existing scenario shall be overridden
    saveAsFlag         <- TRUE
    # boolean that specifies whether output data is included in currently loaded dataset
    no.output.data     <- TRUE
    # count number of prepared scenarios for asynchronous solve
    async.count        <- 1L
    # parameters used for saving scenario data
    scenData           <- list()
    scenData[["scen_1_"]] <- scenDataTemplate
    # parameter used for saving (hidden) scalar data
    scalarData         <- list()
    # boolean that specifies whether handsontable is initialised
    hot.init           <- vector("logical", length = length(modelIn))
    # boolean that specifies whether check if data is unsaved should be skipped
    no.check      <- vector("logical", length = length(modelIn))
    no.check[]    <- TRUE
    # boolean that specifies whether input data does not match output data
    dirty.flag    <- FALSE
    isInSplitView      <- if(identical(config$defCompMode, "split")) TRUE else FALSE
    isInCompareMode    <- FALSE
    isInSolveMode      <- TRUE
    if(config$activateModules$scenario){
      scenMetaData     <- list()
      # scenario metadata of scenario saved in database
      scenMetadata     <- NULL
      # currently active scenario (R6 object)
      activeScen       <- NULL
      # temporary name and sid of the scenario currently active in the UI
      active.sname.tmp <- NULL
      # save the scenario ids loaded in UI
      scenCounterMultiComp <- 4L
      sidsInComp       <- vector("integer", length = maxNumberScenarios + 1)
      sidsInSplitComp  <- vector("integer", length = 2)
      # occupied slots (scenario is loaded in ui with this rv$scenId)
      occupied.sid.slots <- vector("logical", length = maxNumberScenarios)
      # scenId of tabs that are loaded in ui (used for shortcuts) (in correct order)
      sid.comp.order     <- NULL
      loadInLeftBoxSplit <- TRUE
      # boolean that specifies whether nested tabset is active or not
      shortcut.nest <- FALSE
      observeEvent(input$tabset.shortcut.nest, {
        if(isolate(input$sidebar.menu) == "scenarios" && !is.null(sid.comp.order)){
          shortcut.nest <<- TRUE
        }
      })
      observeEvent(input$tabset.shortcut.unnest, {
        if(isolate(input$sidebar.menu) == "scenarios" && !is.null(sid.comp.order)){
          shortcut.nest <<- FALSE
        }
      })
    }
    # trigger navigation through tabs by shortcuts
    observeEvent(input$tabset.shortcut.next, {
      if(isolate(input$sidebar.menu) == "inputData"){
        flog.debug("Navigated to next input tab (using shortcut).")
        current.sheet <- as.numeric(gsub("\\D", "", isolate(input$input.tabset)))
        updateTabsetPanel(session, "input.tabset", paste0("input.tabset_", current.sheet + 1))
      }else if(isolate(input$sidebar.menu) == "outputData"){
        flog.debug("Navigated to next output tab (using shortcut).")
        current.sheet <- as.numeric(gsub("\\D", "", isolate(input$content.current)))
        updateTabsetPanel(session, "content.current", paste0("content.current_", current.sheet + 1))
      }else if(isolate(input$sidebar.menu) == "scenarios"){
        if(!is.null(sid.comp.order) && !isInSplitView){
          current.scen <- as.numeric(gsub("\\D", "", isolate(input$scenTabset)))
          if(shortcut.nest){
            flog.debug("Navigated to next data tab in scenario comparison view (using shortcut).")
            # go to next data sheet
            current.sheet <- as.numeric(gsub("\\D+_\\d+_", "", isolate(input[[paste0("content.scen_", current.scen)]])))
            updateTabsetPanel(session, paste0("content.scen_", current.scen), paste0("content.scen_", current.scen, "_", current.sheet + 1))
          }else{
            flog.debug("Navigated to next scenario tab in scenario comparison view (using shortcut).")
            # go to next scenario tab
            idx <- which(sid.comp.order == current.scen)[1]
            if(identical(idx, length(sid.comp.order))){
              return(NULL)
            }
            updateTabsetPanel(session, "scenTabset", paste0("scen_", sid.comp.order[idx + 1], "_"))
          }
        }else if(isInSplitView){
          flog.debug("Navigated to next data tab in split view scenario comparison view (using shortcut).")
          current.scen <- 2
          current.sheet <- as.numeric(gsub("\\D+_\\d+_", "", isolate(input[[paste0("content.scen_", current.scen)]])))
          updateTabsetPanel(session, paste0("content.scen_", current.scen), paste0("content.scen_", current.scen, "_", current.sheet + 1))
          updateTabsetPanel(session, paste0("content.scen_", current.scen + 1), paste0("content.scen_", current.scen + 1, "_", current.sheet + 1))
        }
      }
    })
    observeEvent(input$tabset.shortcut.prev,{
      if(isolate(input$sidebar.menu) == "inputData"){
        flog.debug("Navigated to previous input tab (using shortcut).")
        current.sheet <- as.numeric(gsub("\\D", "", isolate(input$input.tabset)))
        updateTabsetPanel(session, "input.tabset", paste0("input.tabset_", current.sheet - 1))
      }else if(isolate(input$sidebar.menu) == "outputData"){
        flog.debug("Navigated to previous output tab (using shortcut).")
        current.sheet <- as.numeric(gsub("\\D", "", isolate(input$content.current)))
        updateTabsetPanel(session, "content.current", paste0("content.current_", current.sheet - 1))
      }else if(isolate(input$sidebar.menu) == "scenarios"){
        if(!is.null(sid.comp.order) && !isInSplitView){
          current.scen <- as.numeric(gsub("\\D", "", isolate(input$scenTabset)))
          if(shortcut.nest){
            flog.debug("Navigated to previous data tab in single scenario comparison view (using shortcut).")
            # go to previous data sheet
            current.sheet <- as.numeric(gsub("\\D+_\\d+_", "", isolate(input[[paste0("content.scen_", current.scen)]])))
            updateTabsetPanel(session, paste0("content.scen_", current.scen), paste0("content.scen_", current.scen, "_", current.sheet - 1))
          }else{
            flog.debug("Navigated to previous scenario tab in scenario comparison view (using shortcut).")
            # go to previous scenario tab
            idx <- which(sid.comp.order == current.scen)[1]
            if(identical(idx, 1)){
              return(NULL)
            }
            updateTabsetPanel(session, "scenTabset", paste0("scen_", sid.comp.order[idx - 1], "_"))
          }
        }else if(isInSplitView){
          flog.debug("Navigated to previous data tab in split view scenario comparison view (using shortcut).")
          current.scen <- 2
          current.sheet <- as.numeric(gsub("\\D+_\\d+_", "", isolate(input[[paste0("content.scen_", current.scen)]])))
          updateTabsetPanel(session, paste0("content.scen_", current.scen), paste0("content.scen_", current.scen, "_", current.sheet - 1))
          updateTabsetPanel(session, paste0("content.scen_", current.scen + 1), paste0("content.scen_", current.scen + 1, "_", current.sheet - 1))
        }
        
      }
    })
    
    # initially set rounding precision to default
    roundPrecision <- config$roundingDecimals
    
    # set local working directory
    workDir <- paste0(getwd(), .Platform$file.sep, tmpFileDir, session$token, .Platform$file.sep)
    if(!dir.create(file.path(workDir), recursive = TRUE)){
      flog.fatal("Working directory could not be initialised.")
      showErrorMsg(lang$errMsg$fileWrite$title, lang$errMsg$fileWrite$desc)
      return(NULL)
    }else{
      flog.debug("Working directory was created: '%s'.", workDir)
    }
    
    # initialization of several variables
    rv <- reactiveValues(scenId = 4L, datasets.imported = vector(mode = "logical", length = length(modelIn.must.import)), 
                         unsavedFlag = TRUE, btLoadScen = 0L, btOverrideScen = 0L, btOverrideInput = 0L, btSaveAs = 0L, 
                         btSaveConfirm = 0L, btRemoveOutputData = 0L, btLoadLocal = 0L, btCompareScen = 0L, active.sname = NULL)
    # list of scenario IDs to load
    sidsToLoad <- list()
    # list with input data
    model.input.data  <- vector(mode = "list", length = length(modelIn))
    shared.input.data <- vector(mode = "list", length = length(modelIn))
    # list with input data before new data was loaded as shiny is lazy when data is equal and wont update
    previous.input.data <- vector(mode = "list", length = length(modelIn))
    no.data.changes     <- vector(mode = "logical", length = length(modelIn))
    # initialize model input data
    model.input.data <- modelInTemplate
    # initialise list of reactive expressions returning data for model input
    data.modelIn <- vector(mode = "list", length = length(modelIn))
    # auxiliary vector that specifies whether data frame has no data or data was overwritten
    is.empty.input <- vector(mode = "logical", length = length(modelIn))
    # input is empty in the beginning
    is.empty.input[] <- TRUE
    # list of data frames which save changes made in handsontable
    hot.input <- vector(mode = "list", length = length(modelIn))
    # gams process object
    gams <- NULL
    # boolean that specifies whether input data should be overridden
    input.override.confirmed <- FALSE
    # trigger sidebar menu by shortcuts
    observeEvent(input$sidebar.menu.shortcut, {
      switch(input$sidebar.menu.shortcut,
             inputData = {
               flog.debug("Navigated to input menu (using shortcut).")
               updateTabsetPanel(session, "sidebar.menu", selected = "inputData")
             },
             outputData = {
               flog.debug("Navigated to output menu (using shortcut).")
               updateTabsetPanel(session, "sidebar.menu", selected = "outputData")
             },
             gamsinter = {
               flog.debug("Navigated to gams interaction menu (using shortcut).")
               updateTabsetPanel(session, "sidebar.menu", selected = "gamsinter")
             }
      )
      if(config$activateModules$scenario){
        if(input$sidebar.menu.shortcut == "scenarios"){
          flog.debug("Navigated to scenario comparison view menu (using shortcut).")
          updateTabsetPanel(session, "sidebar.menu", selected = "scenarios")
        }else if(input$sidebar.menu.shortcut == "advanced" ){
          flog.debug("Navigated to advanced options menu (using shortcut).")
          updateTabsetPanel(session, "sidebar.menu", selected = "advanced")
        }
      }else{
        if(input$sidebar.menu.shortcut %in% c("scenarios", "advanced")){
          flog.debug("Navigated to advanced options menu (using shortcut).")
          updateTabsetPanel(session, "sidebar.menu", selected = "advanced")
        }
      }
    })

    # show/hide buttons in sidebar depending on menu item selected
    observeEvent(input$sidebar.menu,{
      flog.debug("Sidebar menu item: '%s' selected.", isolate(input$sidebar.menu))
      switch (input$sidebar.menu,
              inputData = {
                shinyjs::show("btImport")
                shinyjs::show("btSolve")
                shinyjs::hide("btInterrupt")
                if(config$activateModules$scenario){
                  shinyjs::hide("btSplitView")
                  shinyjs::hide("btCompareScen")
                  shinyjs::show("btSave")
                  shinyjs::show("btSaveAs")
                  shinyjs::hide("btLoadScen")
                  shinyjs::show("btDelete")
                }
              },
              outputData = {
                shinyjs::show("btImport")
                shinyjs::hide("btSolve")
                shinyjs::hide("btInterrupt")
                if(config$activateModules$scenario){
                  shinyjs::hide("btSplitView")
                  shinyjs::hide("btCompareScen")
                  shinyjs::show("btSave")
                  shinyjs::show("btSaveAs")
                  shinyjs::hide("btLoadScen")
                  shinyjs::show("btDelete")
                }
              },
              gamsinter = {
                shinyjs::hide("btImport")
                shinyjs::hide("btSolve")
                shinyjs::show("btInterrupt")
                if(config$activateModules$scenario){
                  shinyjs::hide("btSplitView")
                  shinyjs::hide("btCompareScen")
                  shinyjs::hide("btSave")
                  shinyjs::hide("btSaveAs")
                  shinyjs::hide("btLoadScen")
                  shinyjs::hide("btDelete")
                }
              },
              advanced = {
                shinyjs::hide("btImport")
                shinyjs::hide("btSolve")
                shinyjs::hide("btInterrupt")
                if(config$activateModules$scenario){
                  shinyjs::hide("btSplitView")
                  shinyjs::hide("btCompareScen")
                  shinyjs::hide("btSave")
                  shinyjs::hide("btSaveAs")
                  shinyjs::hide("btLoadScen")
                  shinyjs::hide("btDelete")
                }
              }
      )
      if(config$activateModules$scenario && input$sidebar.menu == "scenarios"){
        # reset nest level
        shortcut.nest <<- FALSE
        isInSolveMode <<- FALSE
        shinyjs::hide("btImport")
        shinyjs::hide("btSolve")
        shinyjs::hide("btInterrupt")
        shinyjs::show("btSplitView")
        shinyjs::show("btCompareScen")
        shinyjs::hide("btSave")
        shinyjs::hide("btSaveAs")
        shinyjs::show("btLoadScen")
        shinyjs::hide("btDelete")
      }
    })
    
    # activate solve button when all datasets that have to be 
    # imported are actually imported
    lapply(seq_along(modelIn), function(i){
      observe({
        switch(modelIn[[i]]$type,
               hot = {
                 input[[paste0("in_", i)]]
               }, 
               slider = {
                 input[[paste0("slider_", i)]]
               },
               dropdown = {
                 input[[paste0("dropdown_", i)]]
               },
               date = {
                 input[[paste0("date_", i)]]
               },
               daterange = {
                 input[[paste0("daterange_", i)]]
               },
               checkbox = {
                 input[[paste0("cb_", i)]]
               })
        if(isolate(rv$unsavedFlag)){
          return(NULL)
        }
        if(no.check[i]){
          no.check[i] <<- FALSE
          return(NULL)
        }
        # set unsaved flag
        rv$unsavedFlag <<- TRUE
        # if scenario includes output data set dirty flag
        if(!no.output.data){
          dirty.flag <<- TRUE
          shinyjs::show("dirtyFlagIcon")
          shinyjs::show("dirtyFlagIconO")
        }
      })
    })
    
    markUnsaved <- function(){
      shinyjs::hide("dirtyFlagIcon")
      shinyjs::hide("dirtyFlagIconO")
      dirty.flag     <<- FALSE
      rv$unsavedFlag <<- TRUE
      return(invisible())
    }
    
    markSaved <- function(){
      shinyjs::hide("dirtyFlagIcon")
      shinyjs::hide("dirtyFlagIconO")
      dirty.flag     <<- FALSE
      rv$unsavedFlag <<- FALSE
      return(invisible())
    }
    
    # print scenario title in input and output sheets
    getScenTitle <- reactive(
      if(is.null(rv$active.sname)){
        shinyjs::disable("btDelete")
        shinyjs::disable("btSave")
        return("")
      }else{
        shinyjs::enable("btDelete")
        shinyjs::enable("btSave")
        name.suffix <- ""
        if(rv$unsavedFlag){
          name.suffix <- "*"
        }
        return(paste0(rv$active.sname, name.suffix))
      }
    )
    output$inputDataTitle <- renderText(getScenTitle())
    output$outputDataTitle <- renderText(getScenTitle())
    
    # activate solve button once all model input files are imported
    observe({
      datasetsImported <- vapply(names(modelIn.must.import), function(el){
        i <- match(el, names(modelIn))[[1]]
        if(length(rv[[paste0("in_", i)]])){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }, logical(1), USE.NAMES = FALSE)
      if(all(datasetsImported)){
        shinyjs::enable("btSolve")
      }else{
        shinyjs::disable("btSolve")
      }
    })
    # UI elements (modalDialogs)
    source("./UI/dialogs.R", local = TRUE)
    ####### Model input
    # generate import dialogue
    source("./modules/input_ui.R", local = TRUE)
    # load input data from Excel sheet
    source("./modules/excel_input_load.R", local = TRUE)
    # render tabular input datasets
    source("./modules/input_render_tab.R", local = TRUE)
    # render non tabular input datasets (e.g. slider, dropdown)
    source("./modules/input_render_nontab.R", local = TRUE)
    
    ####### GAMS interaction
    # solve button clicked
    source("./modules/gams_run.R", local = TRUE)
    # Interrupt button clicked
    source("./modules/gams_interrupt.R", local = TRUE)
    

    ####### Model output
    # render output graphs
    source("./modules/output_render.R", local = TRUE)
    obs.compare <- vector("list", maxNumberScenarios)
    # switch between tabular view and output graphs
    source("./modules/output_table_view.R", local = TRUE)
    
    ####### Advanced options
    source("./modules/download_tmp.R", local = TRUE)
    
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
        scen.str <- "scen_" %+% i %+% "_"
        # table view
        source("./modules/scen_table_view.R", local = TRUE)
        
        # close scenario tab
        source("./modules/scen_close.R", local = TRUE)
        
        # export Scenario to Excel spreadsheet
        source("./modules/excel_scen_save.R", local = TRUE)
        
        # compare scenarios
        obs.compare[[i]] <<- observe({
          if(is.null(input[[paste0("content.scen_", i)]])){
            return(NULL)
          }
          j <- strsplit(input[[paste0("content.scen_", i)]], "_")[[1]][3]
          
          if(i < maxNumberScenarios + 2){
            lapply(isolate(names(scenData)), function(scen){
              k <- strsplit(scen, "_")[[1]][2]
              updateTabsetPanel(session, paste0("content.scen_", k),
                                paste0(paste0("content.scen_", k, "_", j)))
            })
          }else if(i == maxNumberScenarios + 2){
            updateTabsetPanel(session, paste0("content.scen_", i + 1),
                              paste0(paste0("content.scen_", i + 1, "_", j)))
          }else{
            updateTabsetPanel(session, paste0("content.scen_", i - 1),
                              paste0(paste0("content.scen_", i - 1, "_", j)))
          }
        }, suspended = TRUE)
      })
      
      # scenario comparison
      source("./modules/scen_compare.R", local = TRUE)
    }else{
      i <- 1
      # export output data to Excel spreadsheet
      source("./modules/excel_scen_save.R", local = TRUE)
    }
    
    # internal database module
    #if(config$activateModules$scenario){
      
      # upload model input data from database 
      #source("./modules/db_input_load.R", local = TRUE)
      
      # save model input data to database
      #source("./modules/db_input_save.R", local = TRUE)
      
    #}
    
    # This code will be run after the client has disconnected
    session$onSessionEnded(function() {
      # remove temporary files and folders
      unlink(file.path(workDir), recursive=TRUE)
      suppressWarnings(rm(activeScen))
      try(flog.info("Session ended (model: '%s').", modelName))
      gc()
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
  
  ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = config$pageSkin)
  
  app <- shiny::shinyApp(ui = ui, server = server)
}