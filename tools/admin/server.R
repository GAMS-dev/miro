appDisconnected <- FALSE
oneLayerEl <- c("dygraphs")
twoLayerEl <- c("pie", "hist")
configJSONFileName <- paste0(currentModelDir, configDir, 
                             modelName, ".json")
addArrayEl <- function(session, arrayID, plotly_chart_type = ""){
  arrayID <- paste0(arrayID, plotly_chart_type)
  session$sendCustomMessage("gms-addArrayEl", arrayID)
  HTML(paste0('<div id="', arrayID, '_wrapper" class="shiny-input-container" style="margin:20px;">\n
 <hr>\n
 <div class="array-wrapper"></div>\n
 <button onclick="addArrayDataEl(\'', arrayID, '\')" type="button" class="btn btn-default bt-icon btn-add-array-el" style="font-size:20px;">\n
   <i class="far fa-plus-square"></i>\n
 </button>\n
</div>'))
}
optionSection <- function(title, ..., collapsed = FALSE){
  tags$div(class = "shiny-input-container",
           tags$h4(class = "box-title option-section-header", title, icon("plus"), style = "cursor:pointer", 
                   onclick = "$(this).next().toggle();"),
           tags$div(class = "option-section", ..., style = if(collapsed) "display:none;" else "")
           )
}

server_admin <- function(input, output, session){
  # ------------------------------------------------------
  #     CUSTOMIZE GRAPHS
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_graphs.R"), local = TRUE)
  # ------------------------------------------------------
  #     Input widgets
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_widgets.R"), local = TRUE)
  
  # ------------------------------------------------------
  #     DB MANAGEMENT
  # ------------------------------------------------------
  observeEvent(input$removeDbTables, {
    showModal(modalDialog(paste0(
      "Please confirm that you want to remove all database tables that belong to the model: ",
      modelName, " by typing \"confirm\" in the text field below."),
      textInput("removeDbConfirmTxt", NULL),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("removeDbTablesConfirm", "Confirm", class = "bt-highlight-1 bt-gms-confirm")
      ),
      title = "Remove database tables"))
  })
  observeEvent(input$removeDbTablesConfirm, {
    if(!identical(input$removeDbConfirmTxt, "confirm")){
      return()
    }
    disableEl(session, "#removeDbTablesConfirm")
    tryCatch({
      db$removeTablesModel()
    }, error = function(e){
      flog.error("Unexpected error: '%s'. Please contact GAMS if this error persists.", e)
      showHideEl(session, "#unknownError", 6000L)
    })
    removeModal()
    showHideEl(session, "#removeSuccess", 3000L)
  })
  output$dbSaveAll <- downloadHandler(
    filename = function() {
      paste0("db_save_", tolower(modelName), ".zip")
    },
    content = function(file) {
      tryCatch({
        prog <- Progress$new()
        on.exit(prog$close(), add = TRUE)
        prog$set(message = "Database is being saved...", value = 0.2)
        tempDir <- file.path(tempdir(), "db_save")
        dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
        wd      <- getwd()
        setwd(tempDir)
        on.exit(setwd(wd), add = TRUE)
        on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)
        db$saveTablesModel(tempDir)
        prog$inc(amount = 0.8, detail = "Compressing files...")
        zip(file, list.files(recursive = FALSE), compression_level = 9)
      }, error = function(e){
        switch(conditionMessage(e),
               'maxRowException' = {
                 flog.info("Maximum number of rows to export were exceeded. You need to backup your database manually.")
                 showHideEl(session, "#maxRowError", 6000L)
               },
               {
                 flog.error("Unexpected error: '%s'. Please contact GAMS if this error persists.", e)
                 showHideEl(session, "#unknownError", 6000L)
               })
      })
    },
    contentType = "application/zip"
  )
  observeEvent(input$restoreDb, {
    req(input$dbBackupZip)
    noErr <- TRUE
    tryCatch({
      prog <- Progress$new()
      on.exit(prog$close(), add = TRUE)
      prog$set(message = "Data is being uploaded...", value = 0.2)
      tempDir <- file.path(tempdir(), "db_restore")
      if(dir.exists(tempDir) && 
         unlink(tempDir, recursive = TRUE, force = TRUE) == 1){
        stop(sprinft("Can't remove existing directory: '%s'.", tempDir), 
             call. = FALSE)
      }
      if(!all(dir.create(tempDir, showWarnings = FALSE, recursive = TRUE))){
        stop(sprinft("Can't create directory: '%s'.", tempDir), 
             call. = FALSE)
      }
      on.exit(unlink(tempDir, recursive = TRUE, force = TRUE), add = TRUE)
      grepEx <- "^((?!\\.\\.).)*\\.csv$"
      prog$inc(amount = 0.2, detail = "Decompressing files...")
      zipFilePath <- isolate(input$dbBackupZip$datapath)
      filesToUnzip <- grep(grepEx, unzip(zipFilePath, list = TRUE)$Name, 
                           ignore.case = TRUE, value = TRUE, perl = TRUE)
      if(!length(filesToUnzip)){
        stop("noData", call. = FALSE)
      }
      unzip(zipFilePath, filesToUnzip,
            exdir = gsub("/?$", "", tempDir))
      unzippedFiles  <- list.files(tempDir, pattern = "\\.csv$", full.names = TRUE)
      dbSchema       <- db$getDbSchema()
      tableNames     <- tolower(gsub(".csv", "", 
                                     basename(unzippedFiles), fixed = TRUE))
      metaId         <- match(dbSchema$tabName[["_scenMeta"]], tableNames)[[1L]]
      if(!is.na(metaId)){
        # make sure to read metadata table first
        tableNames[c(1L, metaId)]    <- tableNames[c(metaId, 1L)]
        unzippedFiles[c(1L, metaId)] <- unzippedFiles[c(metaId, 1L)]
      }
      scenTableNames <- db$getTableNamesScenario()
      if(!all(tableNames %in% dbSchema$tabName)){
        flog.info("Some tables in your archive do not exist in the current schema: '%s'.", 
                  paste(tableNames[!tableNames %in% scenTableNames], collapse = "', '"))
        stop("valErr", call. = FALSE)
      }
      prog$inc(amount = 0.2, detail = "Validating files...")
      validatedTables <- vector("list", length(unzippedFiles))
      validatedTables <- lapply(seq_along(unzippedFiles), function(i){
        data      <- read_csv(unzippedFiles[i], col_names = TRUE, 
                              col_types = cols(), na = character())
        tableName <- tableNames[i]
        if(!tableName %in% scenTableNames){
          tabID    <- match(tableName, dbSchema$tabName)[[1L]]
          
          if(is.na(tabID)){
            flog.info("Table name: '%s' could not be found in current database schema. Thus, it was rejected.", 
                      tableName)
            stop("valErr", call. = FALSE)
          }
          colNames <- dbSchema$colNames[[tabID]]
          colTypes <- dbSchema$colTypes[[tabID]]
        }else{
          tabID <- match(tableName, dbSchema$tabName)[[1L]]
          colNames <- c(sidIdentifier, dbSchema$colNames[[tabID]])
          colTypes <- "i" %+% dbSchema$colTypes[[tabID]]
        }
        if(!validateHeaders(data, colNames,
                            headerTypes = colTypes)){
          flog.warn("Dataset: '%s' has invalid headers.\nHeaders are: '%s'.\nHeaders should be: '%s'.\n
  Column types are: '%s'.\n Column types should be: '%s'.", 
                    tableName, paste(names(data), collapse = "', '"), 
                    paste(colNames, collapse = "', '"),
                    paste(vapply(data, function(el) return(class(el)[[1L]]), 
                                 character(1L), USE.NAMES = FALSE), collapse = "', '"),
                    colTypes)
                    
          stop("valErr", call. = FALSE)
        }
        
        return(data)
      })
      prog$inc(amount = 0.2, detail = "Uploading files...")
      lapply(seq_along(validatedTables), function(i){
        tableName <- tableNames[i]
        if(i == 1L){
          db$writeMetadata(validatedTables[[i]])
          return()
        }else if(identical(tableName, dbSchema$tabName[["_hcubeMeta"]])){
          db$writeMetadata(validatedTables[[i]], hcubeMetadata = TRUE)
          return()
        }
        db$exportScenDataset(validatedTables[[i]], tableName, 
                             addForeignKey = tableName %in% scenTableNames)
        prog$inc(amount = 0.2/length(validatedTables), detail = "Finished...")
      })
      prog$inc(amount = 0.2, detail = "Finished...")
      
    }, error = function(e){
      noErr <<- FALSE
      switch(conditionMessage(e),
             noData = {
               flog.info("No data found in archive. Nothing was restored.")
               showHideEl(session, "#restoreNoData", 6000L)
             },
             valErr = {
               flog.info("At least one of the tables is invalid. Nothing was restored.")
               showHideEl(session, "#restoreInvalidData", 6000L)
             },
             {
               flog.error("Unexpected error: '%s'.", e)
               showHideEl(session, "#unknownError", 6000L)
             })
    })
    if(!noErr)
      return()
    showHideEl(session, "#restoreSuccess", 3000L)
  })
  #configGenData <- config[!names(config) %in% c("pageTitle", 
  #                                              "MIROSwitch", 
  #                                              "gamsMetaDelim", 
  #                                              "fileExchange", 
  #                                              "csvDelim", "db")]
  #session$sendCustomMessage("parseConfig", 
  #                          list(config = configGenData[!names(configGenData) %in% c("gamsInputFiles",
  #                                                                                  "gamsOutputFiles")],
  #                               gmsio = configGenData[c("gamsInputFiles", "gamsOutputFiles")]))
  #observe({
  #  if(appDisconnected){
  #    stop("Please don't refresh the page via the browser as the JSON does not refresh.")
  #  }
  #  data <- input$updatedConfig
  #  if(!length(data))
  #    return()
  #  noErr <- TRUE
  #  tryCatch({
  #    confFilePath    <- file.path(currentModelDir, configDir, "config.json")
  #    confFilePathOld <- file.path(currentModelDir, configDir, "config_old.json")
  #    if(file.exists(confFilePath)[1]){
  #      file.copy(confFilePath, confFilePathOld, overwrite = TRUE)
  #      unlink(confFilePath, force = TRUE)
  #    }
  #    if(!length(data$datatable$options$columnDefs[[1]])){
  #      data$datatable$options$columnDefs <- NULL
  #    }
  #    jsonlite::write_json(data, confFilePath, pretty = TRUE, auto_unbox = TRUE,
  #                         null = "null")
  #  }, error = function(e){
  #    flog.error("Problems writing config.json file. Error message: '%s'.", e)
  #    showHideEl(session, "#updateConfigError", 4000L)
  #    noErr <<- FALSE
  #  })
  #  if(!noErr)
  #    return()
  #  hideEl(session, "#configGenForm")
  #  showEl(session, "#btConfigGenNew")
  #  showHideEl(session, "#updateConfigSuccess", 4000L)
  #})
  hideEl(session, "#loading-screen")
  session$onSessionEnded(function() {
    appDisconnected <<- TRUE
    if(!interactive()){
      stopApp()
    }
  })
}