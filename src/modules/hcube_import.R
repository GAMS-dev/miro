# elements that must be saved in scalar table
scalarInToVerify <- unlist(lapply(scalarInputSym, function(el){
  if((!is.null(modelIn[[el]]$slider) && identical(modelIn[[el]]$slider$double, TRUE)) || 
     !is.null(modelIn[[el]]$daterange)){
    return(paste0(el, c("_lo", "_up")))
  }else{
    return(el)
  }
}), use.names = FALSE)
additionalInputScalars <- inputDsNames[vapply(inputDsNames, function(el){
  return(isTRUE(modelIn[[el]]$dropdown$single) || 
           isTRUE(modelIn[[el]]$dropdown$checkbox))
}, logical(1L), USE.NAMES = FALSE)]
if(length(scalarInToVerify))
  scalarInToVerify <- scalarInToVerify[!startsWith(scalarInToVerify, "_")]

# initialise hcube import class
hcubeImport <- HcubeImport$new(db, scalarsFileName, scalarsOutName, 
                               tableNamesCanHave = c(setdiff(c(inputDsNames,
                                                               modelIn[[scalarsFileName]]$symnames,
                                                               modelIn[[scalarEquationsName]]$symnames), 
                                                             c(additionalInputScalars,
                                                               scalarsFileName,
                                                               scalarEquationsName)), 
                                                     setdiff(c(names(modelOut),
                                                               modelOut[[scalarsOutName]]$symnames,
                                                               modelOut[[scalarEquationsOutName]]$symnames), 
                                                             c(scalarsOutName,
                                                               scalarEquationsOutName))),
                               tableNamesMustHave = c(scalarInToVerify, 
                                                      if(config$saveTraceFile) "_scenTrc"),
                               config$csvDelim, workDir,
                               gdxio = gdxio, inputSym = setdiff(inputDsNames, additionalInputScalars), 
                               outputSym = names(modelOut),
                               templates = setNames(c(modelInTemplate, modelOutTemplate, scalarsInTemplate), 
                                                    c(names(modelIn), names(modelOut), scalarsFileName)))
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
      flog.error("The zip file you are trying to upload contains invalid files. Only trace and gdx files allowed! No path traversals!")
      errMsg <<- lang$errMsg$hcubeImport$extract$invalidFiles
    }else{
      flog.error("Problems unzipping the file. Error message: %s.", conditionMessage(e))
      errMsg <<- lang$errMsg$hcubeImport$extract$desc
    }
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$extract$title, errMsg))){
    return(NULL)
  }
  prog$set(detail = lang$progressBar$hcubeImport$zipValidation, value = 1/6)
  # validate here so only valid scenarios will be read
  tryCatch({
    invalidScenIds <- hcubeImport$validateScenFiles()
    flog.trace("Scenario files validated.")
  }, error = function(e){
    flog.error("Problems validating results. Error message: %s.", conditionMessage(e))
    errMsg <<- lang$errMsg$hcubeImport$extract$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$extract$title, errMsg))){
    return(NULL)
  }
  if(length(hcubeImport$getScenNames()) - length(invalidScenIds) == 0){
    showErrorMsg(lang$errMsg$hcubeImport$invalidJob$title, 
                 lang$errMsg$hcubeImport$invalidJob$desc)
    return(NULL)
  }
  if(length(invalidScenIds) >= 1){
    showInvalidScenIdsDialog(invalidScenIds)
    return(NULL)
  }
  rv$noInvalidData <- rv$noInvalidData + 1L
})

##############
#      2
##############
observeEvent(input$btHcubeImportInvalid,
             rv$noInvalidData <- isolate(rv$noInvalidData + 1L)
             )
observeEvent(virtualActionButton(rv$noInvalidData), {
  prog <- Progress$new()
  prog$set(message = lang$progressBar$hcubeImport$scenValidation, value = 2/6)
  on.exit(prog$close())
  errMsg <- NULL
  removeModal()
  
  tryCatch({
    hcubeImport$readAllScenData()
    flog.trace("Scenario data read into memory.")
  }, error = function(e){
    flog.error("Problems reading scenario data. Error message: %s.", conditionMessage(e))
    errMsg <<- lang$errMsg$hcubeImport$scenRead$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$scenRead$title, errMsg))){
    return(NULL)
  }
  
  prog$inc(amount = 0, detail = lang$progressBar$hcubeImport$scenRead)
  
  prog$inc(amount = 1/6, detail = lang$progressBar$hcubeImport$duplicateCheck)
  tryCatch({
    duplicatedScen    <- hcubeImport$getScenDuplicates()
    duplicatedScenIds <<- duplicatedScen[["_sname"]]
    flog.trace("Duplicated scenarios identified.")
    if(nrow(duplicatedScen)){
      dupScenTags <- paste(unique(duplicatedScen[["_stag"]]), collapse = ", ")
      noDupScen   <- length(unique(duplicatedScenIds))
      showDuplicatedScenDialog(noDupScen, dupScenTags, noScen = length(hcubeImport$getScenNames()))
    }else{
      rv$btSave <- rv$btSave + 1L
    }
  }, error = function(e){
    flog.error("Problems fetching duplicated Scenarios from database. Error message: %s.",
               conditionMessage(e))
    errMsg <<- lang$errMsg$hcubeImport$duplicateFetch$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$hcubeImport$duplicateFetch$title, errMsg))){
    return(NULL)
  }
})


observeEvent(input$btHcubeImportNew, {
  hcubeImport$removeDuplicates()
  rv$btSave <- rv$btSave + 1L
})
observeEvent(input$btHcubeImportAll,
             rv$btSave <- rv$btSave + 1L)

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
    flog.error("Problems importing scenarios. Error message: %s.",
               conditionMessage(e))
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
  tryCatch({
    worker$updateJobStatus(statusCode, jobImportID, tags = hcubeTags)
    worker$removeActiveDownload(jobImportID)
    },
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
                              status = JOBSTATUSMAP[['corrupted(man)']],
                              isHcube = TRUE)
    if(identical(jIDtmp, -1L)){
      stop(call. = FALSE)
    }
    jobImportID    <<- jIDtmp
  }, error = function(e){
    showHideEl(session, "#manHcubeImportUnknownError")
    flog.error("Problems writing Hypercube job metadata to database. Error message: '%s'.",
               conditionMessage(e))
    noErr <<- FALSE
  })
  if(!noErr)
    return()
  rv$jobListPanel <- rv$jobListPanel + 1L
  manualJobImport <<- TRUE
  removeModal()
  rv$uploadHcube <- rv$uploadHcube + 1L
})

observeEvent(input$importJob, {
  jID <- isolate(input$importJob)
  flog.trace("Import Hypercube job button clicked. Job ID: '%s'.", jID)
  if(!is.integer(jID) || length(jID) != 1L){
    flog.error("Invalid job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  if(!worker$getStatus(jID) %in% c(JOBSTATUSMAP[['completed']], JOBSTATUSMAP[['downloaded']])){
    flog.error("Import button was clicked but job is not yet marked as 'completed' (Job ID: '%s'). The user probably tampered with the app.", jID)
    showHideEl(session, "#fetchJobsError")
  }
  hcubeTags    <<- vector2Csv(isolate(input[["jTag_" %+% jID]]))
  zipFilePath  <<- worker$getJobResultsPath(jID)
  jobImportID  <<- jID
  if(file.access(zipFilePath, 4L) == -1L){
    flog.error("Zip file with Hypercube job results was removed during import process (Job ID: '%s').", jID)
    showHideEl(session, "#fetchJobsError")
  }
  manualJobImport <<- FALSE
  disableEl(session, "#btImportJob_" %+% jID)
  rv$uploadHcube <- rv$uploadHcube + 1L
})
