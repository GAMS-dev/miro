# elements that must be saved in scalar table
scalarInToVerify <- unlist(lapply(names(modelIn)[!names(modelIn) %in% modelInTabularData], function(el){
  if((!is.null(modelIn[[el]]$slider) && identical(modelIn[[el]]$slider$double, TRUE)) || 
     !is.null(modelIn[[el]]$daterange)){
    return(paste0(el, c("_lo", "_up")))
  }else{
    return(el)
  }
}), use.names = FALSE)
scalarOutToVerify <- NULL
if(scalarsOutName %in% names(modelOut)){
  scalarOutToVerify <- modelOut[[scalarsOutName]]$symnames
}
gmsColTypes <- unlist(lapply(c(modelIn, modelOut), "[[", "colTypes"))
gmsColTypes <- gmsColTypes[!is.null(gmsColTypes)]
gmsColTypes[[scalarsFileName]] <- "ccc"
gmsFileHeaders <- lapply(c(modelIn, modelOut), function(el){
  names(el$headers)
})
gmsFileHeaders[[scalarsFileName]] <- scalarsFileHeaders

disableEl(session, "#btUploadBatch")

# initialise batch import class
batchImport <- BatchImport$new(db, scalarsFileName, scalarsOutName, tableNamesCanHave = names(modelOut),
                               tableNamesMustHave = c(inputDsNames, if(scalarsOutName %in% names(modelOut)) scalarsOutName, 
                                                      if(config$saveTraceFile) tableNameTracePrefix %+% modelName),
                               config$csvDelim, workDir, gmsColTypes = gmsColTypes, gmsFileHeaders = gmsFileHeaders,
                               strictmode = config$activateModules$strictmode)
rm(gmsColTypes)
duplicatedScenIds <- vector("character", 0L)
batchTags         <- character(0L)

##############
#      1
##############
observeEvent(input$btUploadBatch, {
  req(input$batchImport)
  req(rv$clear)
  
  if(length(input$batchTags)){
    batchTags <<- vector2Csv(input$batchTags)
  }else{
    showErrorMsg("No tags", "Please provide a batch tag")
  }
  
  disableEl(session, "#btUploadBatch")
  prog <- shiny::Progress$new()
  prog$set(message = "Extracting zip file", value = 1/8)
  on.exit(prog$close())
  
  zipFilePath       <- input$batchImport$datapath
  errMsg            <- NULL
  
  tryCatch({
    batchImport$unzipScenData(zipFilePath, extractDir = workDir)
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
  
  batchImport$validateScenTables(scalarInToVerify, scalarOutToVerify)
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
    duplicatedScenIds <<- duplicatedScen[[snameIdentifier]]
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
  rv$clear <- FALSE
})

observeEvent(input$batchImport, {
  enableEl(session, "#btUploadBatch")
  tag <- gsub("\\..+$", "", input$batchImport$name)
  updateSelectInput(session, "batchTags", choices = tag, selected = tag)
  rv$clear <- TRUE
}, priority = 1000)