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

hcubeMeta <- "e"
hcubeMetaHistory <- "e"

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
currentJobID      <- NULL

interruptProcess <- function(pid){
  currentOs <- tolower(getOS()[[1]])
  if(currentOs == 'windows'){
    processx::run(command = 'taskkill', args = c("/F", "/PID", pid, "/T"), 
                  windows_hide_window = TRUE, timeout = 10L)
  }else if (currentOs %in% c('linux', 'osx')){
    processx::run(command = 'kill', args = c("-SIGKILL", -pid), timeout = 10L)
  }else{
    stop(sprintf("Operating system: '%s' not supported.", currentOs), call. = FALSE)
  }
}
updateJobMetadata <- function(jID, status = NULL, tags = NULL, scode = NULL){
  rowId                      <- hcubeMeta[[1]] == jID
  jobMeta                    <- hcubeMeta[rowId, ]
  if(identical(status, "discard")){
    currentStatus <- strsplit(jobMeta[[snameIdentifier]], "_", fixed = TRUE)[[1]]
    jPid                       <- currentStatus[1]
    currentStatus              <- currentStatus[2]
    if(identical(currentStatus, "running")){
      tryCatch(interruptProcess(jPid), error = function(e){
        flog.info("Running process (pid: '%s') could not be stopped. Maybe process already terminated?.", jPid)
      })
    }
    status <- paste0("_discarded(", currentStatus, ")_")
    if(unlink(file.path(currentModelDir, hcubeDirName, jID), recursive = TRUE)){
      flog.warn("Could not delete working directory for Hypercube job: '%s'. Please remove manually!", jID)
    }
  }else if(identical(status, "imported")){
    if(unlink(file.path(currentModelDir, hcubeDirName, jID), recursive = TRUE)){
      flog.warn("Could not delete working directory for Hypercube job: '%s'. Please remove manually!", jID)
    }
    status <- "_imported_"
  }
  jobMeta[, snameIdentifier]   <- status
  if(length(tags)){
    jobMeta[, stagIdentifier]  <- vector2Csv(tags)
  }
  if(endsWith(status, "_")){
    hcubeMeta                  <<- hcubeMeta[!rowId, ]
    hcubeMetaHistory           <<- bind_rows(hcubeMetaHistory, jobMeta)
  }else{
    hcubeMeta[rowId, ]         <<- jobMeta
  }
  tryCatch({
    db$updateHypercubeJob(jID, tags = tags, status = status, scode = scode)
  }, error = function(e){
    stop(sprintf("Problems updating Hypercube job with job ID: '%s'. Error message: '%s'.", jID, e), 
         call. = FALSE)
  })
  return(invisible())
}

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
    if(identical(e, "invalidFiles")){
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
    hcubeImport$saveScenarios(hcubeTags, jobID = currentJobID, readPerm = uid, 
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
    statusCode <- "_imported(man)_"
  }else{
    statusCode <- "imported"
  }
  tryCatch(updateJobMetadata(currentJobID, status = statusCode, 
                             tags = hcubeTags, scode = hcubeImport$getNoScen()),
           error = function(e){
             flog.error(e)
             showHideEl(session, "#fetchJobsError")
           })
  showEl(session, "#jImport_load")
  hcubeMetaDisplay      <- arrange(hcubeMeta, desc(!!as.name(stimeIdentifier)))
  output$jImport_output <- renderUI(getHypercubeJobsTable(hcubeMetaDisplay))
  hideEl(session, "#jImport_load")
  showHideEl(session, "#fetchJobsImported")
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
    currentJobID    <<- db$writeMetaHcube(hcubeTags, manual = TRUE)
  }, error = function(e){
    showHideEl(session, "#manHcubeImportUnknownError")
    flog.error("Problems writing Hypercube job metadata to database. Error message: '%s'.", e)
    noErr <<- FALSE
  })
  if(!noErr)
    return()
  rv$refreshActiveJobs <- rv$refreshActiveJobs + 1L
  manualJobImport <<- TRUE
  removeModal()
  rv$uploadHcube <- rv$uploadHcube + 1L
})

observeEvent(virtualActionButton(input$refreshActiveJobs, rv$refreshActiveJobs), {
  flog.trace("Refresh active jobs button clicked.")
  showEl(session, "#jImport_load")
  hcubeMetaDisplay <- NULL
  tryCatch({
    hcubeMeta_tmp    <- db$getMetaHcube()
    hcubeMeta        <<- filter(hcubeMeta_tmp, !endsWith(!!as.name(snameIdentifier), "_")) 
    hcubeMetaHistory <<- dplyr::setdiff(hcubeMeta_tmp, hcubeMeta)
  }, error = function(e){
    flog.error("Problems fetching Hypercube job metadata. Error message: '%s'.", e)
  })
  jobsCompleted <- FALSE
  jobsCorrupted <- FALSE
  if(length(nrow(hcubeMeta)) && nrow(hcubeMeta)){
    tryCatch({
      for(jID in hcubeMeta[[1L]]){
        jobDir <- file.path(currentModelDir, hcubeDirName, jID)
        jStatus <- strsplit(hcubeMeta[hcubeMeta[[1L]] == jID, ][[snameIdentifier]], 
                            "_", fixed = TRUE)[[1L]][2L]
        if(dir.exists(jobDir)[1]){
          if(!identical(jStatus, "completed") && file.exists(file.path(jobDir, "4upload.zip"))){
            updateJobMetadata(jID, status = "_completed")
          }else if(!startsWith(jStatus, "corrupted")){
            pid <- strsplit(hcubeMeta[hcubeMeta[[1L]] == jID, , drop = FALSE][[snameIdentifier]], 
                            "_", fixed = TRUE)[[1L]][1L]
            if(!identical(pid, "") && !pidExists(pid)){
              flog.info("Job with ID: '%s' is not running anymore, but results archive could not be found. Job was marked: 'corrupted'.", jID)
              updateJobMetadata(jID, status = "_corrupted(noProcess)")
            }
          } 
        }else if(!startsWith(jStatus, "corrupted")){
          flog.info("Job with ID: '%s' could not be accessed (directory is missing or lacks read permissions). Job was marked: 'corrupted'.", jID)
          updateJobMetadata(jID, status = "_corrupted(noDir)")
        }
      }
    }, error = function(e){
      flog.error("Problems refreshing Hypercube job statuses. Error message: '%s'.", e)
      showHideEl(session, "#fetchJobsError")
    })
  }
  if(length(nrow(hcubeMeta))){
    hcubeMetaDisplay <- arrange(hcubeMeta, desc(!!as.name(stimeIdentifier)))
  }
  output$jImport_output <- renderUI(getHypercubeJobsTable(hcubeMetaDisplay))
  hideEl(session, "#jImport_load")
  if(jobsCompleted){
    showJobsCompletedDialog()
  }
}, ignoreNULL = FALSE)

observeEvent(input$showHypercubeLog, {
  jID <- isolate(input$showHypercubeLog)
  flog.trace("Show Hypercube log button clicked. Job ID: '%s'.", jID)
  if(!is.integer(jID) || length(jID) != 1L){
    flog.error("Invalid job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  hasReadPerm <- jID %in% hcubeMeta[[1]]
  if(!hasReadPerm){
    flog.error("A Hypercube job that user has no read permissions was attempted to fetch. Job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  jobDir      <- file.path(currentModelDir, hcubeDirName, jID)
  logFilePath <- file.path(jobDir, jID %+% ".log")
  logContent  <- NULL
  if(file.access(logFilePath, 4L) != -1L){
    try(
      logContent <- readChar(logFilePath, file.info(logFilePath)$size)
    )
    logSize    <- file.info(logFilePath)$size
    logContent <- paste0(if(logSize > (3e4 + 1)) "[...]\n", 
                         substr(logContent, logSize - 3e4, logSize))
  }
  
  showHypercubeLogFileDialog(logContent)
})

observeEvent(input$importHypercubeJob, {
  jID <- isolate(input$importHypercubeJob)
  flog.trace("Import Hypercube job button clicked. Job ID: '%s'.", jID)
  if(!is.integer(jID) || length(jID) != 1L){
    flog.error("Invalid job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  hasReadPerm <- jID %in% hcubeMeta[[1]]
  if(!hasReadPerm){
    flog.error("A Hypercube job that user has no read permissions was attempted to fetch. Job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  isCompleted <- grepl("completed", hcubeMeta[hcubeMeta[[1]] == jID, snameIdentifier])[[1L]]
  if(!isCompleted){
    flog.error("Import button was clicked but job is not yet marked as 'completed' (Job ID: '%s'). The user probably tampered with the app.", jID)
    showHideEl(session, "#fetchJobsError")
  }
  hcubeTags    <<- vector2Csv(isolate(input[["jTag_" %+% jID]]))
  zipFilePath  <<- file.path(currentModelDir, hcubeDirName, jID, "4upload.zip")
  currentJobID <<- jID
  if(file.access(zipFilePath, 4L) == -1L){
    flog.error("Zip file with Hypercube job results was removed during import process (Job ID: '%s').", jID)
    showHideEl(session, "#fetchJobsError")
  }
  manualJobImport <<- FALSE
  disableEl(session, "#jImport_" %+% jID)
  rv$uploadHcube <- rv$uploadHcube + 1L
})

observeEvent(input$discardHypercubeJob, {
  removeModal()
  jID <- isolate(input$discardHypercubeJob)
  flog.trace("Discard Hypercube job button clicked. Job ID: '%s'.", jID)
  if(!is.integer(jID) || length(jID) != 1L){
    flog.error("Invalid job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  hasReadPerm <- jID %in% hcubeMeta[[1]]
  if(!hasReadPerm){
    flog.error("A Hypercube job that user has no read permissions was attempted to be fetched. Job ID: '%s'.", jID)
    showHideEl(session, "#fetchJobsError")
    return()
  }
  noErr <- TRUE
  tryCatch(updateJobMetadata(jID, status = "discard", tags = isolate(input[["jTag_" %+% jID]])),
           error = function(e){
             flog.error(e)
             showHideEl(session, "#fetchJobsError")
             noErr <<- FALSE
           })
  showEl(session, "#jImport_load")
  hcubeMetaDisplay           <- arrange(hcubeMeta, desc(!!as.name(stimeIdentifier)))
  output$jImport_output      <- renderUI(getHypercubeJobsTable(hcubeMetaDisplay))
  hideEl(session, "#jImport_load")
  showHideEl(session, "#fetchJobsDiscarded")
})
observeEvent(input$btShowHistory, {
  hcubeMetaDisplay <- tibble()
  if(length(hcubeMetaHistory)){
    hcubeMetaDisplay <- arrange(hcubeMetaHistory, desc(!!as.name(stimeIdentifier)))
  }
  showJobHistoryDialog(hcubeMetaDisplay)
})
