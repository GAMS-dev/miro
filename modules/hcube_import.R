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
hcubeMeta <- "e"
hcubeMetaHistory <- "e"

disableEl(session, "#btUploadHcube")

# initialise hcube import class
hcubeImport <- HcubeImport$new(db, scalarsFileName, scalarsOutName, tableNamesCanHave = names(modelOut),
                               tableNamesMustHave = c(inputDsNames, if(scalarsOutName %in% names(modelOut)) scalarsOutName, 
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
updateJobMetadata <- function(jID, status = NULL, tags = NULL){
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
  jobMeta[, snameIdentifier] <- status
  if(length(tags)){
    jobMeta[, stagIdentifier] <- vector2Csv(tags)
  }
  if(endsWith(status, "_")){
    hcubeMeta                  <<- hcubeMeta[!rowId, ]
    hcubeMetaHistory           <<- bind_rows(hcubeMetaHistory, jobMeta)
  }else{
    hcubeMeta[rowId, ]         <<- jobMeta
  }
  tryCatch({
    db$updateHypercubeJob(jID, tags = tags, status = status)
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
  
  prog <- shiny::Progress$new()
  prog$set(message = "Extracting zip file", value = 1/8)
  on.exit(prog$close())

  errMsg            <- NULL
  
  tryCatch({
    hcubeImport$unzipScenData(zipFilePath, extractDir = workDir)
  }, error = function(e){
    flog.error("Problems unzipping the file. Error message: %s.", e)
    errMsg <<- "Problems unzipping the file. Please make sure you upload a valid zip file."
  })
  if(is.null(showErrorMsg("Problems uploading scenarios", errMsg))){
    return(NULL)
  }
  prog$set(message = "Validating zip file", value = 1/6)
  # validate here so only valid scenarios will be read
  hcubeImport$validateScenFiles()
  
  tryCatch({  
    hcubeImport$readAllScenData()
  }, error = function(e){
    flog.error("Problems reading scenario data. Error message: %s.", e)
    errMsg <<- "Problems reading scenario data. If this problem persists, please contact the system administrator."
  })
  if(is.null(showErrorMsg("Problems reading files", errMsg))){
    return(NULL)
  }
  
  prog$inc(amount = 0, message = "Validating scenario data")
  
  hcubeImport$validateScenTables(scalarInToVerify, scalarOutToVerify)
  invalidScenIds <- hcubeImport$getInvalidScenIds()
  if(length(hcubeImport$getScenNames()) - length(invalidScenIds) == 0){
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
observeEvent(input$btHcubeImportInvalid,
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
    errMsg <<- "Problems fetching duplicated Scenarios from database."
  })
  if(is.null(showErrorMsg("Invalid data", errMsg))){
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
  prog <- shiny::Progress$new()
  prog$set(message = "Uploading scenarios to database", value = 1/2)
  on.exit(prog$close())
  errMsg <- NULL
  removeModal()
  tryCatch({
    hcubeImport$saveScenarios(hcubeTags, readPerm = uid, 
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
  if(!length(currentJobID)){
    rv$clear <- FALSE
  }else{
    tryCatch(updateJobMetadata(currentJobID, status = "imported", 
                               tags = hcubeTags),
             error = function(e){
               flog.error(e)
               showHideEl(session, "#fetchJobsError")
             })
    showEl(session, "#jImport_load")
    hcubeMetaDisplay      <- arrange(hcubeMeta, desc(!!as.name(stimeIdentifier)))
    output$jImport_output <- renderUI(getHypercubeJobsTable(hcubeMetaDisplay))
    hideEl(session, "#jImport_load")
    showHideEl(session, "#fetchJobsImported")
  }
})

observeEvent(input$btManualImport, {
  showManualJobImportDialog()
})
observeEvent(input$hcubeImport, {
  enableEl(session, "#btUploadHcube")
  tag <- gsub("\\..+$", "", input$hcubeImport$name)
  updateSelectInput(session, "hcubeTags", choices = tag, selected = tag)
  rv$clear <- TRUE
}, priority = 1000)
observeEvent(input$btUploadHcube, {
  req(input$hcubeImport)
  req(rv$clear)
  
  if(length(input$hcubeTags)){
    hcubeTags <<- vector2Csv(input$hcubeTags)
  }else{
    showErrorMsg("No tags", "Please provide a job tag")
    return()
  }
  zipFilePath       <<- input$hcubeImport$datapath
  currentJobID      <<- NULL
  disableEl(session, "#btUploadHcube")
  rv$uploadHcube <- rv$uploadHcube + 1L
})

observeEvent(input$refreshActiveJobs, {
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
      if(getNoLinesInFile(logFilePath) > maxNoLinesToRead){
        logContent <- lang$errMsg$readLst$fileSize
      }else{
        logContent <- readChar(logFilePath, file.info(logFilePath)$size)
      }
    )
  }
  
  showHypercubeLogFileDialog(logContent)
})

observeEvent(input$importHypercubeJob, {
  removeModal()
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
    flog.error("Import button was clicked but job is not yet marked as 'completed' (Job ID: '%s'). The user probably tempered with the UI.", jID)
    showHideEl(session, "#fetchJobsError")
  }
  hcubeTags    <<- vector2Csv(isolate(input[["jTag_" %+% jID]]))
  zipFilePath  <<- file.path(currentModelDir, hcubeDirName, jID, "4upload.zip")
  currentJobID <<- jID
  if(file.access(zipFilePath, 4L) == -1L){
    flog.error("Zip file with Hypercube job results was removed during import process (Job ID: '%s').", jID)
    showHideEl(session, "#fetchJobsError")
  }
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
