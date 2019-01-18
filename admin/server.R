server_admin <- function(input, output, session){
  observeEvent(input$removeDbTables, {
    showModal(modalDialog(paste0(
      "Please confirm that you want to remove all database tables that belong to the model: ",
      modelName, " by typing \"confirm\" in the text field below."),
      textInput("removeDbConfirmTxt", NULL),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("removeDbTablesConfirm", "Confirm", class = "bt-highlight-1")
      ),
      title = "Remove database tables"))
  })
  observeEvent(input$removeDbTablesConfirm, {
    if(!identical(input$removeDbConfirmTxt, "confirm")){
      return()
    }
    tryCatch({
      db$removeTablesModel()
    }, error = function(e){
      flog.error("Unexpected error: '%s'. Please contact GAMS if this error persists.", e)
      showHideEl(session, "#unknownError", 6000L)
    })
    
  })
  output$dbDumpAll <- downloadHandler(
    filename = function() {
      paste0("db_dump_", tolower(modelName), ".zip")
    },
    content = function(file) {
      tryCatch({
        prog <- Progress$new()
        on.exit(prog$close(), add = TRUE)
        prog$set(message = "Database is being dumped...", value = 0.2)
        tempDir <- file.path(tempdir(), "db_dump")
        dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
        wd      <- getwd()
        setwd(tempDir)
        on.exit(setwd(wd), add = TRUE)
        on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)
        db$dumpTablesModel(tempDir)
        prog$inc(amount = 0.8, detail = "Compressing files...")
        zip(file, list.files(recursive = FALSE), compression_level = 9)
      }, error = function(e){
        switch(conditionMessage(e),
               'maxRowException' = {
                 flog.error("Maximum number of rows to export were exceeded. You need to backup your database manually.")
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
      filesToUnzip <- grep(grepEx, unzip(zipFilePath, list = TRUE)$Name, 
                           ignore.case = TRUE, value = TRUE, perl = TRUE)
      if(!length(filesToUnzip)){
        stop("noData", call. = FALSE)
      }
      unzip(zipFilePath, filesToUnzip,
            exdir = gsub("/?$", "", tempDir))
      unzippedFiles <- list.files(tempDir, pattern = "\\.csv$", full.names = TRUE)
      tableNames <- tolower(gsub(".csv", "", 
                                basename(unzippedFiles), fixed = TRUE))
      if(!all(tableName %in% scenTableNames)){
        stop("valErr", call. = FALSE)
      }
      lapply(seq_along(unzippedFiles), function(i){
        # TODO: validation of headers/types!!
        db$exportScenDataset()
      })
      
      prog$inc(amount = 0.2, detail = "Uploading files...")
      
    }, error = function(e){
      switch(conditionMessage(e),
             noData = {
               flog.error("No data found in archive. Nothing was restored.")
               showHideEl(session, "#restoreNoData", 6000L)
             },
             valErr = {
               flog.error("At least one of the tables is invalid. Nothing was restored.")
               showHideEl(session, "#restoreInvalidData", 6000L)
             },
             {
               flog.error("Unexpected error: '%s'. Please contact GAMS if this error persists.", e)
               showHideEl(session, "#unknownError", 6000L)
             })
    })
  })
  hideEl(session, "#loading-screen")
  session$onSessionEnded(function() {
    if(!interactive()){
      stopApp()
      q("no")
    }
  })
}