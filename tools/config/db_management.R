observeEvent(input$removeDbTables, {
  showModal(modalDialog(
    lang$adminMode$database$removeDialogDesc,
    checkboxInput_MIRO("removeDbConfirmCb", lang$adminMode$database$removeCheck),
    footer = tagList(
      modalButton(lang$adminMode$database$cancel),
      actionButton("removeDbTablesConfirm", lang$adminMode$database$confirm, class = "bt-highlight-1 bt-gms-confirm")
    ),
    title = lang$adminMode$database$removeTitle))
})
observeEvent(input$removeDbTablesConfirm, {
  if(isFALSE(input$removeDbConfirmCb)){
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
observeEvent(input$removeDbOrphans, {
  showModal(modalDialog(
    lang$adminMode$database$removeOrphansDialogDesc,
    checkboxInput_MIRO("removeDbOrphansConfirmCb", lang$adminMode$database$removeOrphansCheck),
    footer = tagList(
      modalButton(lang$adminMode$database$cancel),
      actionButton("removeDbOrphansConfirm", lang$adminMode$database$confirm, class = "bt-highlight-1 bt-gms-confirm")
    ),
    title = lang$adminMode$database$removeOrphansTitle))
})
observeEvent(input$removeDbOrphansConfirm, {
  if(isFALSE(input$removeDbOrphansConfirmCb)){
    return()
  }
  disableEl(session, "#removeDbOrphansConfirmCb")
  tryCatch({
    orphanedTables <- db$getOrphanedTables(hcubeScalars = getHcubeScalars(modelIn))
    if(length(orphanedTables)){
      db$removeTablesModel(orphanedTables)
    }
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
    prog <- Progress$new()
    on.exit(prog$close(), add = TRUE)
    prog$set(message = lang$adminMode$database$saveMsg, value = 0.2)
    tryCatch({
      tempDir <- file.path(tempdir(), "db_save")
      dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
      on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)
      db$saveTablesModel(tempDir)
      prog$inc(amount = 0.8, detail = lang$adminMode$database$compressMsg)
      zipr(file, list.files(tempDir, recursive = FALSE, full.names = TRUE), compression_level = 9)
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
    prog$inc(amount = 0.2, detail = lang$adminMode$database$decompressMsg)
    zipFilePath <- isolate(input$dbBackupZip$datapath)
    filesToUnzip <- grep(grepEx, zip_list(zipFilePath)$filename, 
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
    scenTableNames <- c(db$getTableNamesScenario(), dbSchema$tabName[['_scenTrc']])
    if(!all(tableNames %in% dbSchema$tabName)){
      flog.info("Some tables in your archive do not exist in the current schema: '%s'.", 
                paste(tableNames[!tableNames %in% scenTableNames], collapse = "', '"))
      stop("valErr", call. = FALSE)
    }
    prog$inc(amount = 0.2, detail = lang$adminMode$database$validateMsg)
    validatedTables <- vector("list", length(unzippedFiles))
    validatedTables <- lapply(seq_along(unzippedFiles), function(i){
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
      data      <- read_csv(unzippedFiles[i], col_names = TRUE, 
                            col_types = colTypes, na = character())
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
    prog$inc(amount = 0.2, detail = lang$adminMode$database$uploadMsg)
    lapply(seq_along(validatedTables), function(i){
      tableName <- tableNames[i]
      if(identical(tableName, dbSchema$tabName[["_scenMeta"]])){
        db$writeMetadata(validatedTables[[i]])
        return()
      }else if(identical(tableName, dbSchema$tabName[["_jobMeta"]])){
        db$createJobMeta()
        db$exportDataset(dbSchema$tabName[["_jobMeta"]], validatedTables[[i]], 
                         checkColNames = FALSE)
        return()
      }
      db$exportScenDataset(validatedTables[[i]], tableName, 
                           addForeignKey = tableName %in% scenTableNames)
      prog$inc(amount = 0.2/length(validatedTables), detail = "Finished...")
    })
    prog$inc(amount = 0.2, detail = lang$adminMode$database$finishedMsg)
    
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
