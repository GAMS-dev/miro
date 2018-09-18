# elements that must be saved in scalar table
scalarInToVerify <- names(modelIn)[!names(modelIn) %in% modelIn.tabular.data]
rv <- reactiveValues(clear = TRUE, btSave = 0L, noInvalidData = 0L)
shinyjs::disable("btUploadBatch")

# table names that must exist in order for scenario to be valid
tableNamesToVerify <- gsub(modelName %+% "_", "", scen.table.names, fixed = TRUE)
# initialise batch import class
batchImport <- BatchImport$new(db, scalars.file.name, scalars.out.name, 
                               tableNamesToVerify, config$csvDelim, workDir)
duplicatedScenIds <- vector("character", 0L)
batchTags         <- character(0L)


##############
#      1
##############
observeEvent(input$btUploadBatch, {
  req(input$batchImport)
  req(rv$clear)
  
  if(grepl("^\\s*$", input[["batchTags"]])){
    showErrorMsg("No tags", "Please provide a batch tag")
  }else{
    batchTags <<- input[["batchTags"]]
  }
  
  disable("btUploadBatch")
  prog <- shiny::Progress$new()
  prog$set(message = "Extracting zip file", value = 1/8)
  on.exit(prog$close())
  
  zipFilePath       <- input$batchImport$datapath
  errMsg            <- NULL
  
  tryCatch({
    batchImport$unzipScenCsvData(zipFilePath, extractDir = workDir)
  }, error = function(e){
    flog.error("Problems unzipping the file. Error message: %s.", e)
    errMsg <<- "Problems unzipping the file. Please make sure you upload a valid zip file."
  })
  if(is.null(showErrorMsg("Problems uploading scenarios", errMsg))){
    return(NULL)
  }
  prog$set(message = "Validating zip file", value = 1/6)
  # validate here so only valid scenarios will be read
  batchImport$validateScenFiles()
  
  tryCatch({  
    batchImport$readAllScenData()
  }, error = function(e){
    flog.error("Problems reading scenario data. Error message: %s.", e)
    errMsg <<- "Problems reading scenario data. If this problem persists, please contact the system administrator."
  })
  if(is.null(showErrorMsg("Problems reading files", errMsg))){
    return(NULL)
  }
  
  prog$inc(amount = 0, message = "Validating scenario data")
  
  batchImport$validateScenTables(scalarInToVerify)
  invalidScenIds <- batchImport$getInvalidScenIds()
  if(length(batchImport$getScenNames()) - length(invalidScenIds) == 0){
    showErrorMsg("No valid scenarios", "There are no valid scenarios in your zip file. Please upload valid data.")
    return(NULL)
  }else if(length(invalidScenIds) >= 1){
    showInvalidScenIdsDialog(invalidScenIds)
  }else{
    rv$noInvalidData <- rv$noInvalidData + 1L
  }
})

##############
#      2
##############
observeEvent(input$btBatchImportInvalid,
             rv$noInvalidData <- isolate(rv$noInvalidData + 1L)
             )
observeEvent(virtualActionButton(rv$noInvalidData), {
  prog <- shiny::Progress$new()
  prog$set(message = "Reading scenario data", value = 2/6)
  on.exit(prog$close())
  errMsg <- NULL
  removeModal()
  
  prog$inc(amount = 1/6, message = "Checking for duplicated scenarios")
  tryCatch({
    duplicatedScen    <- batchImport$getScenDuplicates()
    duplicatedScenIds <<- duplicatedScen[[sname.identifier]]
    if(nrow(duplicatedScen)){
      dupScenTags <- paste(unique(duplicatedScen[[stagIdentifier]]), collapse = ", ")
      noDupScen   <- length(unique(duplicatedScenIds))
      showDuplicatedScenDialog(noDupScen, dupScenTags, noScen = length(batchImport$getScenNames()))
    }else{
      rv$btSave <- isolate(rv$btSave + 1L)
    }
  }, error = function(e){
    flog.error("Problems fetching duplicated Scenarios from database. Error message: %s.", e)
    errMsg <<- "Problems fetching duplicated Scenarios from database."
  })
  if(is.null(showErrorMsg("Invalid data", errMsg))){
    return(NULL)
  }
})


observeEvent(input$btBatchImportNew, {
  batchImport$removeDuplicates()
  rv$btSave <- isolate(rv$btSave + 1L)
})
observeEvent(input$btBatchImportAll,
             rv$btSave <- isolate(rv$btSave + 1L))

##############
#      3
##############
observeEvent(virtualActionButton(rv$btSave), {
  prog <- shiny::Progress$new()
  prog$set(message = "Uploading scenarios to database", value = 1/2)
  on.exit(prog$close())
  errMsg <- NULL
  removeModal()
  print('hi')
  tryCatch({
    batchImport$saveScenarios(batchTags, readPerm = uid, 
                              writePerm = uid, progressBar = prog)
  }, error = function(e){
    flog.error("Problems exporting scenarios. Error message: %s.", e)
    errMsg <<- "Problems uploading the scenarios. Please try again later." %+%
      " If this problem persists, please contact the system administrator."
  })
  if(is.null(showErrorMsg("Problems uploading scenarios", errMsg))){
    return(NULL)
  }
  prog$inc(amount = 1/2, message = "Done!")
  
  # clean up
  reset('batchImport')
  rv$clear <- FALSE
})

observeEvent(input$batchImport, {
  enable("btUploadBatch")
  enable("batchTags")
  updateTextInput(session, "batchTags", value = gsub("\\..+$", "", input$batchImport$name))
  rv$clear <- TRUE
}, priority = 1000)
  
showInvalidScenIdsDialog <- function(invalidScenIds){
  showModal(modalDialog(
    title = "Invalid scenarios",
    sprintf("%d of the scenarios you want to upload are invalid (%s). Do you still want to proceed?", 
            length(invalidScenIds), paste(invalidScenIds, collapse = ", ")),
    footer = tagList(
      modalButton("Abort"),
      actionButton("btBatchImportInvalid", label = "Import all")
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
showDuplicatedScenDialog <- function(noDupScen, dupScenTags, noScen){
  showModal(modalDialog(
    title = "Duplicated Scenarios found",
    if(noScen == noDupScen){
      sprintf("All of the scenarios you want to import already exist in the database with the tag(s): '%s'. Do you still want to proceed?", dupScenTags)
    }else{
      sprintf("%d of the scenarios you want to import already exist in the database with the tag(s): '%s'. Do you still want to proceed or only import those scenarios that do not yet exist.", noDupScen, dupScenTags)
    },
    footer = tagList(
      modalButton("Abort"),
      if(noScen != noDupScen){
        actionButton("btBatchImportNew", label = "Import only new")
      },
      actionButton("btBatchImportAll", label = "Import all")
    ),
    fade = TRUE, easyClose = FALSE
  ))
}