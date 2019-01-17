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
        on.exit(prog$close())
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
    contentType = "application/zip")
  observeEvent(input$fillDb, {
    req(input$dbBackupZip)
    
  })
    hideEl(session, "#loading-screen")
    session$onSessionEnded(function() {
    if(!interactive()){
      stopApp()
      q("no")
    }
  })
}