# elements that must be saved in scalar table
scalarInToVerify <- unlist(lapply(names(modelIn)[!names(modelIn) %in% modelInTabularData], function(el){
  if((!is.null(modelIn[[el]]$slider) && identical(modelIn[[el]]$slider$double, TRUE)) || 
     !is.null(modelIn[[el]]$daterange)){
    return(paste0(el, c("_lo", "_up")))
  }else{
    return(el)
  }
}), use.names = FALSE)
additionalInputScalars <- inputDsNames[vapply(inputDsNames, function(el){
  return(identical(modelIn[[el]]$dropdown$single, TRUE) || 
           identical(modelIn[[el]]$dropdown$checkbox, TRUE))
}, logical(1L), USE.NAMES = FALSE)]
scalarInToVerify <- c(scalarInToVerify, additionalInputScalars)
scalarOutToVerify <- NULL
if(scalarsOutName %in% names(modelOut)){
  scalarOutToVerify <- modelOut[[scalarsOutName]]$symnames
}

gmsColTypes <- db$getDbSchema()$colTypes
gmsFileHeaders <- db$getDbSchema()$colNames

disableEl(session, "#btUploadHcube")

# initialise hcube import class
hcubeImport <- HcubeImport$new(db, scalarsFileName, scalarsOutName, 
                               tableNamesCanHave = c(setdiff(inputDsNames, additionalInputScalars), 
                                                     names(modelOut)),
                               tableNamesMustHave = c(if(scalarsFileName %in% inputDsNames) scalarsFileName, 
                                                      if(scalarsOutName %in% names(modelOut)) scalarsOutName, 
                                                      if(config$saveTraceFile) tableNameTracePrefix %+% modelName),
                               config$csvDelim, workDir, gmsColTypes = gmsColTypes, gmsFileHeaders = gmsFileHeaders,
                               strictmode = config$activateModules$strictmode)
rm(gmsColTypes)
duplicatedScenIds <- vector("character", 0L)
hcubeTags         <- character(0L)
zipFilePath       <- NULL

##############
#      1
##############
observeEvent(rv$uploadHcube, {
  req(hcubeImport)
  req(zipFilePath)
  zipFilePathTmp <- zipFilePath
  zipFilePath    <<- NULL
  prog           <- Progress$new()
  prog$set(message = lang$progressBar$hcubeImport$title, 
           detail = lang$progressBar$hcubeImport$zipExtract, value = 1/8)
  on.exit(prog$close())

  errMsg            <- NULL
  tryCatch({
    hcubeImport$unzipScenData(zipFilePathTmp, extractDir = workDir)
  }, error = function(e){
    if(identical(conditionMessage(e), "invalidFiles")){
      flog.error("The zip file you are trying to upload contains invalid files. Only trace and CSV files allowed! No path traversals!")
      errMsg <<- lang$errMsg$hcubeImport$extract$invalidFiles
    }else{
      flog.error("Problems unzipping the file. Error message: %s.", e)
      errMsg <<- lang$errMsg$hcubeImport$extract$desc
    }
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$extract$title, errMsg))){
    return(NULL)
  }
  prog$set(detail = lang$progressBar$hcubeImport$zipValidation, value = 1/6)
  # validate here so only valid scenarios will be read
  hcubeImport$validateScenFiles()
  
  tryCatch({  
    hcubeImport$readAllScenData()
  }, error = function(e){
    flog.error("Problems reading scenario data. Error message: %s.", e)
    errMsg <<- lang$errMsg$hcubeImport$scenRead$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$scenRead$title, errMsg))){
    return(NULL)
  }
  
  prog$inc(amount = 0, detail = lang$progressBar$hcubeImport$scenValidation)
  
  hcubeImport$validateScenTables(scalarInToVerify, scalarOutToVerify)
  invalidScenIds <- hcubeImport$getInvalidScenIds()
  if(length(hcubeImport$getScenNames()) - length(invalidScenIds) == 0){
    showErrorMsg(lang$errMsg$hcubeImport$invalidJob$title, 
                 lang$errMsg$hcubeImport$invalidJob$desc)
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
observeEvent(input$btHcubeImportInvalid,
             rv$noInvalidData <- isolate(rv$noInvalidData + 1L)
             )
observeEvent(virtualActionButton(rv$noInvalidData), {
  prog <- Progress$new()
  prog$set(message = lang$progressBar$hcubeImport$scenRead, value = 2/6)
  on.exit(prog$close())
  errMsg <- NULL
  removeModal()
  
  prog$inc(amount = 1/6, detail = lang$progressBar$hcubeImport$duplicateCheck)
  tryCatch({
    duplicatedScen    <- hcubeImport$getScenDuplicates()
    duplicatedScenIds <<- duplicatedScen[[snameIdentifier]]
    if(nrow(duplicatedScen)){
      dupScenTags <- paste(unique(duplicatedScen[[stagIdentifier]]), collapse = ", ")
      noDupScen   <- length(unique(duplicatedScenIds))
      showDuplicatedScenDialog(noDupScen, dupScenTags, noScen = length(hcubeImport$getScenNames()))
    }else{
      rv$btSave <- isolate(rv$btSave + 1L)
    }
  }, error = function(e){
    flog.error("Problems fetching duplicated Scenarios from database. Error message: %s.", e)
    errMsg <<- lang$errMsg$hcubeImport$duplicateFetch$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$duplicateFetch$title, errMsg))){
    return(NULL)
  }
})


observeEvent(input$btHcubeImportNew, {
  hcubeImport$removeDuplicates()
  rv$btSave <- isolate(rv$btSave + 1L)
})
observeEvent(input$btHcubeImportAll,
             rv$btSave <- isolate(rv$btSave + 1L))

##############
#      3
##############
observeEvent(virtualActionButton(rv$btSave), {
  prog <- Progress$new()
  prog$set(message = lang$progressBar$hcubeImport$dbUpload, value = 1/2)
  on.exit(prog$close())
  errMsg <- NULL
  removeModal()
  tryCatch({
    hcubeImport$saveScenarios(hcubeTags, jobID = jobImportID, readPerm = uid, 
                              writePerm = uid, execPerm = uid, progressBar = prog)
  }, error = function(e){
    flog.error("Problems exporting scenarios. Error message: %s.", e)
    errMsg <<- lang$errMsg$hcubeImport$dbUpload$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$dbUpload$title, errMsg))){
    return(NULL)
  }
  prog$inc(amount = 1/2, detail = lang$progressBar$hcubeImport$success)
  # clean up
  if(manualJobImport){
    statusCode <- JOBSTATUSMAP[["imported(man)"]]
  }else{
    statusCode <- JOBSTATUSMAP[["imported"]]
  }
  tryCatch(worker$updateJobStatus(statusCode, jobImportID, tags = hcubeTags),
           error = function(e){
             flog.error(e)
             showHideEl(session, "#fetchJobsError")
           })
  rv$jobListPanel <- rv$jobListPanel + 1L
})

observeEvent(input$btManualImport, {
  flog.trace("Import manual Hypercube job button clicked.")
  showManualJobImportDialog()
})

observeEvent(input$hcubeImport, {
  enableEl(session, "#btUploadHcube")
  tag <- gsub("\\..+$", "", input$hcubeImport$name)
  updateSelectInput(session, "hcubeTags", choices = tag, selected = tag)
}, priority = 1000)

observeEvent(input$btUploadHcube, {
  req(input$hcubeImport)
  flog.trace("Upload manual Hypercube job button clicked.")

  hcubeTags         <<- vector2Csv(input$hcubeTags)
  zipFilePath       <<- input$hcubeImport$datapath
  noErr <- TRUE
  tryCatch({
    jIDtmp <- worker$addJobDb("", NULL, tags = hcubeTags, 
                              status = JOBSTATUSMAP[['corrupted(man)']])
    if(identical(jIDtmp, -1L)){
      stop(call. = FALSE)
    }
    jobImportID    <<- jIDtmp
  }, error = function(e){
    showHideEl(session, "#manHcubeImportUnknownError")
    flog.error("Problems writing Hypercube job metadata to database. Error message: '%s'.", e)
    noErr <<- FALSE
  })
  if(!noErr)
    return()
  rv$jobListPanel <- rv$jobListPanel + 1L
  manualJobImport <<- TRUE
  removeModal()
  rv$uploadHcube <- rv$uploadHcube + 1L
})

getJobProgress <- function(jID){
  if(!identical(worker$getStatus(jID), JOBSTATUSMAP[['running']])){
    return(NULL)
  }
  jobProgress <- worker$getHcubeJobProgress(jID)
  
  noJobsCompleted <- jobProgress[[1L]]
  noJobs <- jobProgress[[2L]]
  
  if(identical(noJobsCompleted, noJobs)){
    rv$jobListPanel <- rv$jobListPanel + 1L
    removeModal()
    return(NULL)
  }
  return(list(noCompleted = noJobsCompleted, noJobs = noJobs))
}
observeEvent(input$updateJobProgress, {
  jID <- suppressWarnings(as.integer(isolate(input$updateJobProgress)))
  flog.trace("Update Hypercube progress requested. Job ID: '%s'.", jID)
  currentProgress <- tryCatch(getJobProgress(jID), error = function(e){
    flog.error(conditionMessage(e))
    return(NULL)
  })
  if(is.null(currentProgress)){
    return()
  }
  session$sendCustomMessage("gms-updateJobProgress", 
                            list(id = jID, progress = currentProgress))
})
observeEvent(input$showJobProgress, {
  jID <- suppressWarnings(as.integer(isolate(input$showJobProgress)))
  flog.trace("Show Hypercube progress button clicked. Job ID: '%s'.", jID)
  currentProgress <- tryCatch(getJobProgress(jID), error = function(e){
    errMsg <- conditionMessage(e)
    flog.error(errMsg)
    showHideEl(session, "#fetchJobsError")
    return()
  })
  if(is.null(currentProgress)){
    return()
  }
  
  showJobProgressDialog(jID, currentProgress)
  session$sendCustomMessage("gms-startUpdateJobProgress", 
                            jID)
})

observeEvent(input$importJob, {
  jID <- isolate(input$importJob)
  flog.trace("Import Hypercube job button clicked. Job ID: '%s'.", jID)
  if(!is.integer(jID) || length(jID) != 1L){
    flog.error("Invalid job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  if(!identical(worker$getStatus(jID), JOBSTATUSMAP[['completed']])){
    flog.error("Import button was clicked but job is not yet marked as 'completed' (Job ID: '%s'). The user probably tampered with the app.", jID)
    showHideEl(session, "#fetchJobsError")
  }
  hcubeTags    <<- vector2Csv(isolate(input[["jTag_" %+% jID]]))
  zipFilePath  <<- file.path(currentModelDir, hcubeDirName, jID, "4upload.zip")
  jobImportID  <<- jID
  if(file.access(zipFilePath, 4L) == -1L){
    flog.error("Zip file with Hypercube job results was removed during import process (Job ID: '%s').", jID)
    showHideEl(session, "#fetchJobsError")
  }
  manualJobImport <<- FALSE
  disableEl(session, "#jImport_" %+% jID)
  rv$uploadHcube <- rv$uploadHcube + 1L
})
