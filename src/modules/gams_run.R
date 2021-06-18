# run GAMS
clearLogs <- function(session){
  if(config$activateModules$logFile ||
     config$activateModules$miroLogFile){
    emptyEl(session, "#logStatusContainer")
  }
  rv$refreshLogs <- NULL
  emptyEl(session, "#modelStatus")
  hideEl(session, ".input-validation-error")
}
storeGAMSOutputFiles <- function(workDir){
  if(config$activateModules$attachments && 
     !is.null(activeScen) && 
     (config$storeLogFilesDuration > 0L || length(config$outputAttachments))){
    errMsg <- NULL
    tryCatch({
      filesToStore <- character(0L)
      if(config$storeLogFilesDuration > 0L){
        if(any(c(config$activateModules$logFile, config$activateModules$lstFile)))
          filesToStore  <- file.path(workDir, 
                                     paste0(modelNameRaw, 
                                            c(if(config$activateModules$logFile) ".log", 
                                              if(config$activateModules$lstFile) ".lst")))
        if(config$activateModules$miroLogFile)
          filesToStore <- c(filesToStore, file.path(workDir, config$miroLogFile))
      }
      
      enforceFileAccess <- rep.int(TRUE, length(filesToStore))
      fileAccessPerm    <- rep.int(FALSE, length(filesToStore))
      
      if(length(config$outputAttachments)){
        fnOutputAttachments <- file.path(workDir, vapply(config$outputAttachments, "[[", character(1L),
                                                         "filename", USE.NAMES = FALSE))
        if(any(fnOutputAttachments %in% filesToStore)){
          # overwrite settings of attachments
          # if they are also declared as output attachments
          filesNotInOA <- !filesToStore %in% fnOutputAttachments
          filesToStore <- filesToStore[filesNotInOA]
          enforceFileAccess <- enforceFileAccess[filesNotInOA]
          fileAccessPerm <- fileAccessPerm[filesNotInOA]
        }
        filesToStore <- c(filesToStore, fnOutputAttachments)
        enforceFileAccess <- c(enforceFileAccess, vapply(config$outputAttachments, function(el) {
          return(!isFALSE(el[["throwError"]]))}, logical(1L), USE.NAMES = FALSE))
        fileAccessPerm <- c(fileAccessPerm, vapply(config$outputAttachments, function(el) {
          return(isTRUE(el[["execPerm"]]))}, logical(1L), USE.NAMES = FALSE))
      }
      
      if(!length(filesToStore))
        return()
      
      filesNoAccess <- file.access(filesToStore) == -1L
      if(any(filesNoAccess[enforceFileAccess])){
        if(all(filesNoAccess[enforceFileAccess]))
          stop("fileAccessException", call. = FALSE)
        flog.info("GAMS output files: '%s' could not be accessed. Check file permissions.", 
                  paste(filesToStore[filesNoAccess & enforceFileAccess], collapse = "', '"))
        errMsg <- sprintf(lang$errMsg$saveAttachments$noFileAccess, 
                          paste(basename(filesToStore[filesNoAccess & enforceFileAccess]), collapse = "', '"))
      }
      if(any(filesNoAccess)){
        flog.debug("File(s): '%s' were not added as attachment as they could not be accessed.",
                   paste(filesToStore[filesNoAccess], collapse = "', '"))
      }
      filesToStore <- filesToStore[!filesNoAccess]
      fileAccessPerm <- fileAccessPerm[!filesNoAccess]
      filesTooLarge <- file.size(filesToStore) > attachMaxFileSize
      if(any(filesTooLarge)){
        if(all(filesTooLarge))
          stop("fileSizeException", call. = FALSE)
        flog.info("GAMS output files: '%s' are too large. They will not be saved.", 
                  paste(filesToStore[filesTooLarge], collapse = "', '"))
        errMsg <- paste(errMsg, sprintf(lang$errMsg$saveAttachments$fileSizeExceeded, 
                                        paste(basename(filesToStore[filesTooLarge]), collapse = "', '")), sep = "\n")
      }
      attachments$add(session = NULL, filesToStore[!filesTooLarge], overwrite = TRUE, 
                      execPerm = fileAccessPerm[!filesTooLarge])
    }, error = function(e){
      switch(conditionMessage(e),
             fileAccessException = {
               flog.info("No GAMS output files could not be accessed. Check file permissions.")
               errMsg <<- sprintf(lang$errMsg$saveAttachments$noFileAccess, 
                                  paste(basename(filesToStore), collapse = "', '"))
             },
             fileSizeException = {
               flog.info("All GAMS output files are too large to be saved.")
               errMsg <<- sprintf(lang$errMsg$saveAttachments$fileSizeExceeded, 
                                  paste(basename(filesToStore), collapse = "', '"))
             },
             {
               flog.error("Problems while trying to store GAMS output files in the database. Error message: '%s'.",
                          conditionMessage(e))
               errMsg <<- lang$errMsg$unknownError
             })
    })
    showErrorMsg(lang$errMsg$saveAttachments$title, errMsg)
  }
}
prepareModelRun <- function(async = FALSE){
  prog <- Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$prepRun$title, value = 0)
  
  prog$inc(amount = 0.5, detail = lang$progressBar$prepRun$sendInput)
  # save input data
  if(tryCatch({
    dataTmp <- getInputDataFromSandbox()
    FALSE
  }, no_data = function(e){
    flog.error(conditionMessage(e))
    showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
    return(TRUE)
  }, error = function(e){
    flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
    showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
    return(TRUE)
  })){
    return()
  }
  scenData$loadSandbox(dataTmp, if(length(modelInFileNames)) modelInFileNames else character(),
                       activeScen$getMetadata())
  inputData <- DataInstance$new(modelInFileNames, fileExchange = config$fileExchange,
                                gdxio = gdxio, csvDelim = config$csvDelim,
                                activeScen = activeScen, attachments = attachments,
                                views = views)
  lapply(seq_along(dataTmp), function(id){
    # write compile time variable file and remove compile time variables from scalar dataset
    if(is.null(dataTmp[[id]])){
      return()
    }
    if(identical(tolower(names(dataTmp)[[id]]), scalarsFileName)){
      # scalars file exists, so remove compile time variables from it
      DDParIdx           <- dataTmp[[id]][[1]] %in% DDPar
      GMSOptIdx          <- dataTmp[[id]][[1]] %in% GMSOpt
      if(any(c(DDParIdx, GMSOptIdx))){
        isClArg <- (DDParIdx | GMSOptIdx)
        inputData$pushClArgs(dataTmp[[id]][isClArg, ])
        # remove those rows from scalars file that are compile time variables
        inputData$push(names(dataTmp)[[id]], dataTmp[[id]][!isClArg, ])
      }else{
        inputData$push(names(dataTmp)[[id]], dataTmp[[id]])
      }
    }else if(identical(modelIn[[names(dataTmp)[[id]]]]$type, "dropdown") &&
             names(dataTmp)[[id]] %in% modelInTabularDataBase){
      inputData$push(names(dataTmp)[[id]],
                     ddToTibble(dataTmp[[id]][[1L]], modelIn[[names(dataTmp)[[id]]]]))
    }else{
      inputData$push(names(dataTmp)[[id]], dataTmp[[id]])
    }
  })
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  tryCatch({
    if(config$activateModules$attachments && attachAllowExec && !is.null(activeScen)){
      prog$inc(amount = 0, detail = lang$progressBar$prepRun$downloadAttach)
      inputData$addFilePaths(attachments$download(workDir, allExecPerm = TRUE))
    }
    prog$close()
  }, error = function(e) {
    errMsg <<- lang$errMsg$gamsExec$desc
    flog.error("Attachment data could not be downloaded. Error message: %s.", conditionMessage(e))
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
    return(NULL)
  }
  return(inputData)
}
if(LAUNCHHCUBEMODE){
  idsToSolve <- NULL
  idsSolved <- NULL
  scenGmsPar <- NULL
  attachmentFilePaths <- NULL
  staticData <- NULL
  hcubeData <- NULL
  
  getHcubeParPrefix <- function(id){
    if(names(modelIn)[id] %in% GMSOpt){
      return(substring(names(modelIn)[id],
                       nchar(prefixGMSOpt) + 1L))
    }else if(names(modelIn)[id] %in% DDPar){
      return(paste0("--", substring(names(modelIn)[id],
                                    nchar(prefixDDPar) + 1L)))
    }else if(names(modelIn)[id] %in% inputDsNames){
      if(isTRUE(modelIn[[id]]$dropdown$single) || 
         isTRUE(modelIn[[id]]$dropdown$checkbox)){
        return(paste0("--HCUBE_SCALARV_", names(modelIn)[id]))
      }
      return(paste0("--HCUBE_STATIC_", names(modelIn)[id]))
    }else{
      return(paste0("--HCUBE_SCALARV_", names(modelIn)[id]))
    }
  }
  getNoScenToSolve <- function(modelInIds = seq_along(modelIn)){
    numberScenPerElement <- vapply(modelInIds, function(i){
      switch(modelIn[[i]]$type,
             slider = {
               value <- input[[paste0("slider_", i)]]
               if(length(value) > 1){
                 if(identical(modelIn[[i]]$slider$double, TRUE)){
                   # double slider in single run mode
                   if (!identical(input[[paste0("hcubeMode_", i)]], TRUE)){
                     return(1L)
                   }
                 }else if(!identical(modelIn[[i]]$slider$single, TRUE)){
                   # double slider in single run mode and noHcube=TRUE
                   return(1L)
                 }
                 
                 stepSize <- input[[paste0("hcubeStep_", i)]]
                 range <- floor((value[2] - value[1])/stepSize) + 1
                 if(!is.numeric(stepSize) || stepSize <= 0){
                   # non valid step size selected
                   return(-1L)
                 }
                 if(length(modelIn[[i]]$slider$minStep)){
                   if(stepSize < modelIn[[i]]$slider$minStep){
                     return(-1L)
                   }
                 }else if(stepSize < modelIn[[i]]$slider$step){
                   return(-1L)
                 }
                 if(identical(modelIn[[i]]$slider$single, TRUE)){
                   return(as.integer(range))
                 }
                 # double slider all combinations
                 return(as.integer(range*(range + 1) / 2))
               }
               return(1L)
             },
             dt = ,
             hot = ,
             custom = {
               return(1L)
             },
             dropdown = {
               if(isTRUE(modelIn[[i]]$dropdown$single) || 
                  isTRUE(modelIn[[i]]$dropdown$checkbox)){
                 return(length(input[[paste0("dropdown_", i)]]))
               }
               return(1L)
             },
             date = {
               return(1L)
             },
             daterange = {
               return(1L)
             },
             checkbox = {
               return(length(input[[paste0("cb_", i)]]))
             },
             textinput = {
               return(1L)
             },
             numericinput = {
               return(1L)
             })
    }, integer(1L), USE.NAMES = FALSE)
    if(any(numberScenPerElement == -1L)){
      return(-1L)
    }
    return(prod(numberScenPerElement))
  }
  
  scenToSolve <- reactive({
    prog <- Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    hcubeData <<- HcubeDataInstance$new(modelGmsName)
    staticData <<- DataInstance$new(fileExchange = config$fileExchange,
                                    gdxio = gdxio, csvDelim = config$csvDelim,
                                    sortedNames = names(modelIn))
    modelInSorted <- sort(names(modelIn))
    elementValues <- lapply(seq_along(modelIn), function(j){
      updateProgress(incAmount = 1/(length(modelIn) + 18), detail = lang$nav$dialogHcube$waitDialog$desc)
      i <- match(names(modelIn)[j], modelInSorted)
      parPrefix <- getHcubeParPrefix(i)
      switch(modelIn[[i]]$type,
             slider = {
               value <- input[["slider_" %+% i]]
               if(length(value) > 1){
                 if(identical(modelIn[[i]]$slider$double, TRUE)){
                   # double slider in single run mode
                   if (!identical(input[["hcubeMode_" %+% i]], TRUE)){
                     return(paste0(parPrefix, "_lo= ", value[1], 
                                   '|"""|', parPrefix, "_up= ", value[2]))
                   }
                 }else if(!identical(modelIn[[i]]$slider$single, TRUE)){
                   # double slider in base mode with noHcube=FALSE
                   return(paste0(parPrefix, "_lo= ", value[1], 
                                 '|"""|', parPrefix, "_up= ", value[2]))
                 }
                 
                 stepSize <- input[["hcubeStep_" %+% i]]
                 if(isTRUE(modelIn[[i]]$slider$single)){
                   return(paste0(parPrefix, "= ", seq(value[1], value[2], stepSize)))
                 }
                 # double slider all combinations
                 value <- getCombinationsSlider(value[1], value[2], stepSize)
                 return(paste0(parPrefix, "_lo= ", value$min, 
                               '|"""|', parPrefix, "_up= ", value$max))
               }else{
                 return(paste0(parPrefix, "= ", value))
               }
             },
             dropdown = {
               if(isTRUE(modelIn[[i]]$dropdown$checkbox)){
                 convertNumeric <- TRUE
               }else if(!isTRUE(modelIn[[i]]$dropdown$single) && isTRUE(modelIn[[i]]$dropdown$multiple)){
                 data <- ddToTibble(sort(input[["dropdown_" %+% i]]), modelIn[[i]])
                 staticData$push(names(modelIn)[[i]], data)
                 return(paste0(parPrefix, "= ", digest(data, algo = "md5")))
               }else{
                 convertNumeric <- FALSE
               }
               value <- input[["dropdown_" %+% i]]
               
               if(names(modelIn)[i] %in% c(GMSOpt, DDPar) &&
                  all(value %in% CLARG_MISSING_VALUES)){
                 return(NA)
               }
               if(!names(modelIn)[i] %in% c(DDPar, GMSOpt) && 
                  length(modelIn[[i]]$dropdown$aliases)){
                 text <- paste0('|"""|--HCUBE_SCALART_', names(modelIn)[i], 
                                "= ", escapeGAMSCL(modelIn[[i]]$dropdown$
                                                     aliases[match(value, 
                                                                   modelIn[[i]]$
                                                                     dropdown$choices)]))
                 if(isTRUE(modelIn[[i]]$dropdown$clearValue)){
                   return(substring(text, 6L))
                 }
               }else{
                 text <- ""
               }
               if(convertNumeric){
                 return(paste0(parPrefix, "= ", as.numeric(value), text))
               }
               return(paste0(parPrefix, "= ", escapeGAMSCL(value), text))
             },
             date = {
               value <- as.character(isolate(input[[paste0("date_", i)]]))
               if(length(value) != 1L || is.na(value)){
                 value <- ""
               }
               return(paste0(parPrefix, "= ", escapeGAMSCL(value)))
             },
             daterange = {
               value <- as.character(input[["daterange_" %+% i]])
               emptyDate <- is.na(value)
               if(any(emptyDate)){
                 value[emptyDate] <- ""
               }
               return(paste0(parPrefix, "_lo= ", escapeGAMSCL(value[1]), 
                             '|"""|', parPrefix, "_up= ", escapeGAMSCL(value[2])))
             },
             checkbox = {
               return(paste0(parPrefix, "= ", 
                             if(identical(isolate(input[[paste0("cb_", i)]]), TRUE)) 1L else 0L))
             },
             numericinput = {
               valueTmp <- input[["numeric_" %+% i]]
               if(length(valueTmp) != 1L || identical(valueTmp, "")){
                 # user removed input
                 if(length(modelIn[[i]]$numericinput$value) == 1L){
                   valueTmp <- modelIn[[i]]$numericinput$value
                 }else{
                   valueTmp <- 0L
                 }
               }
               return(paste0(parPrefix, "= ", valueTmp))
             },
             textinput = {
               val <- input[["text_" %+% i]]
               if(length(val) != 1L){
                 val <- ""
               }
               if(names(modelIn)[i] %in% c(GMSOpt, DDPar) &&
                  val %in% CLARG_MISSING_VALUES){
                 return(NA)
               }
               return(paste0(parPrefix, "= ", escapeGAMSCL(val)))
             },
             dt =,
             hot = ,
             custom = {
               input[['in_' %+% i]]
               errMsg <- NULL
               tryCatch({
                 data <- fixColTypes(getInputDataset(i), modelIn[[i]]$colTypes)
               }, error = function(e){
                 flog.error("Dataset: '%s' could not be loaded. Error message: '%s'.", 
                            modelInAlias[i], conditionMessage(e))
                 errMsg <<- sprintf(lang$errMsg$GAMSInput$noData, 
                                    modelInAlias[i])
               })
               if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
                 return(paste0(parPrefix, "= NA"))
               }
               staticData$push(names(modelIn)[[i]], data)
               return(paste0(parPrefix, "= ", digest(data, algo = "md5")))
             },
             {
               stop(sprintf("Widget type: '%s' not supported", modelIn[[i]]$type), call. = FALSE)
             })
    })
    elementValues <- elementValues[!is.na(elementValues)]
    gmsString <- hcubeData$genGmsString(val = elementValues, 
                                        modelName = modelName)
    attachmentFilePaths <- NULL
    if(config$activateModules$attachments && attachAllowExec && !is.null(activeScen)){
      attachmentFilePaths <- attachments$download(workDir, allExecPerm = TRUE)
      attachmentFilePaths <- attachmentFilePaths[match(basename(attachmentFilePaths), 
                                                       sort(basename(attachmentFilePaths)))]
      if(length(attachmentFilePaths)){
        staticData$addFilePaths(attachmentFilePaths)
        gmsString <- paste(gmsString, paste(vapply(seq_along(attachmentFilePaths), function(i){
          return(paste0("--HCUBE_STATIC_", i, "= ", digest(file = attachmentFilePaths[i], algo = "md5")))
        }, character(1L), USE.NAMES = FALSE), collapse = " "))
      }
    }
    updateProgress(incAmount = 15/(length(modelIn) + 18), detail = lang$nav$dialogHcube$waitDialog$desc)
    scenIds <- hcubeData$pushJobIDs(vapply(gmsString, digest, character(1L), algo = "sha256", 
                                           serialize = FALSE, USE.NAMES = FALSE))
    
    updateProgress(incAmount = 3/(length(modelIn) + 18), detail = lang$nav$dialogHcube$waitDialog$desc)
    gmsString <- paste0(scenIds, ": gams ", modelName, ".gms ", gmsString)
    
    return(list(ids = scenIds, gmspar = gmsString, attachmentFilePaths = attachmentFilePaths))
  })
  
  prevJobSubmitted <- Sys.time() - 5L
  
  runHcubeJob <- function(scenGmsPar, downloadFile = NULL){
    if(!length(scenGmsPar)){
      flog.debug("No scenarios selected to be solved in Hypercube mode.")
      return()
    }
    tryCatch({
      sid <- NULL
      if(length(activeScen) && length(activeScen$getSid())){
        if(!activeScen$hasExecPerm())
          stop("User attempted to execute a scenario that he/she has no access to! This looks like an attempt to tamper with the app!", 
               call. = FALSE)
        sid <- activeScen$getSid()
      }
      hideEl(session, "#jobSubmissionWrapper")
      showEl(session, "#jobSubmissionLoad")
      prog <- Progress$new()
      on.exit(prog$close(), add = TRUE)
      prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
      updateProgress <- function(incAmount, detail = NULL) {
        prog$inc(amount = incAmount, detail = detail)
      }
      if(!is.null(downloadFile)){
        workDirHcube <- file.path(tempdir(), "hcube")
        on.exit(unlink(workDirHcube, recursive = TRUE, force = TRUE), 
                add = TRUE)
        worker$
          setInputData(staticData)$
          createHcubeWorkDir(workDirHcube, hcubeData, 
                             attachmentFilePaths)
        removeModal()
        if(!file.copy(file.path(getwd(), "resources", hcubeSubmissionFile %+% ".gms"), 
                      workDirHcube)){
          flog.error("Problems copying Hypercube submission file from: '%s' to: '%s'.",
                     file.path(getwd(), "resources", hcubeSubmissionFile %+% ".gms"),
                     workDirHcube)
        }
        updateProgress(incAmount = 0.9, detail = lang$nav$dialogHcube$waitDialog$desc)
        return(zipr(downloadFile, workDirHcube, compression_level = 6))
      }
      worker$
        setInputData(staticData)$
        runHcube(dynamicPar = hcubeData, 
                 sid = sid, tags = isolate(input$newHcubeTags), 
                 attachmentFilePaths = attachmentFilePaths)
      updateProgress(incAmount = 1, detail = lang$nav$dialogHcube$waitDialog$desc)
      showHideEl(session, "#hcubeSubmitSuccess", 2000)
    }, error = function(e){
      errMsg <- conditionMessage(e)
      if(identical(errMsg, '404') || startsWith(errMsg, "Could not") || 
         startsWith(errMsg, "Timeout"))
        return(showHideEl(session, "#hcubeSubmitUnknownHost", 6000))
      
      if(errMsg %in% c(401L, 403L))
        return(showHideEl(session, "#hcubeSubmitUnauthorized", 6000))
      flog.error("Some problem occurred while executing Hypercube job. Error message: '%s'.", errMsg)
      showHideEl(session, "#hcubeSubmitUnknownError", 6000)
    }, finally = {
      hideEl(session, "#jobSubmissionLoad")
      hideModal(session, 2L)
    })
  }
  
  observeEvent(input$btHcubeAll, {
    disableEl(session, "#btHcubeAll")
    flog.trace("Button to schedule all scenarios for Hypercube submission was clicked.")
    now <- Sys.time()
    if(difftime(now, prevJobSubmitted, units = "secs") < 5L){
      showHideEl(session, "#hcubeSubmitWait", 6000)
      flog.info("Hypercube job submit button was clicked too quickly in a row. Please wait some seconds before submitting a new job.")
      return()
    }
    prevJobSubmitted <<- Sys.time()
    
    runHcubeJob(scenGmsPar)
  })
  
  observeEvent(input$btHcubeNew, {
    disableEl(session, "#btHcubeNew")
    flog.trace("Button to schedule only new scenarios for Hypercube submission was clicked.")
    now <- Sys.time()
    if(difftime(now, prevJobSubmitted, units = "secs") < 5L){
      showHideEl(session, "#hcubeSubmitWait", 6000)
      flog.info("Hypercube job submit button was clicked too quickly in a row. Please wait some seconds before submitting a new job.")
      return()
    }
    prevJobSubmitted <<- Sys.time()
    
    hcubeData$subsetJobIDs(idsSolved)
    runHcubeJob(scenGmsPar[!idsToSolve %in% idsSolved])
  })
  
  
  output$btHcubeAll_dl <- downloadHandler(
    filename = function() {
      tolower(modelName) %+% ".zip"
    },
    content = function(file) {
      # solve all scenarios in Hypercube run
      runHcubeJob(scenGmsPar, downloadFile = file)
    },
    contentType = "application/zip")
  
  output$btHcubeNew_dl <- downloadHandler(
    filename = function() {
      tolower(modelName) %+% ".zip"
    },
    content = function(file) {
      # solve only scenarios that do not yet exist
      hcubeData$subsetJobIDs(idsSolved)
      runHcubeJob(scenGmsPar[!idsToSolve %in% idsSolved], downloadFile = file)
    },
    contentType = "application/zip")
}else{
  loadOutputData <- function(){
    storeGAMSOutputFiles(workDir)
    
    scenData$loadSandbox(loadScenData(metaData = modelOut, workDir = workDir,
                                      fileName = MIROGdxOutName,
                                      templates = modelOutTemplate,
                                      method = config$fileExchange,
                                      csvDelim = config$csvDelim)$tabular, names(modelOut))
    if(config$saveTraceFile){
      tryCatch({
        traceData <<- readTraceData(file.path(workDir, "_scenTrc.trc"))
      }, error = function(e){
        flog.info("Problems loading trace data. Error message: %s.", conditionMessage(e))
      })
    }
    tryCatch(
      worker$updateJobStatus(JOBSTATUSMAP['imported']), 
      error = function(e){
        flog.warn("Failed to update job status. Error message: '%s'.", 
                  conditionMessage(e))
      })
  }
  observeEvent(input$btLoadInconsistentOutput, {
    removeModal()
    
    errMsg <- NULL
    tryCatch({
      loadOutputData()
    }, error = function(e){
      flog.error("Problems loading output data. Error message: %s.", conditionMessage(e))
      errMsg <<- lang$errMsg$readOutput$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
      return(htmltools::htmlEscape(statusText))
    }
    
    #select first tab in current run tabset
    switchTab(session, "output")
    updateTabsetPanel(session, "scenTabset",
                      selected = "results.current")
    renderOutputData()
    markUnsaved()
  })
  observeEvent(virtualActionButton(input$btSubmitJob, rv$btSubmitJob), {
    flog.debug("Submit new asynchronous job button clicked.")
    jobNameTmp <- character(1L)
    if(!verifyCanSolve(async = TRUE, buttonId = "btSubmitJob")){
      return()
    }
    if(length(activeScen)){
      jobNameTmp <- activeScen$getScenName()
    }
    inputData <- prepareModelRun(async = TRUE)
    if(is.null(inputData)){
      return(NULL)
    }
    worker$setInputData(inputData)
    tryCatch({
      scenHashTmp <- inputData$generateScenHash()
      scenWithSameHash <- db$getScenWithSameHash(scenHashTmp)
      activeScen$setScenHash(scenHashTmp)
    }, error = function(e) {
      flog.error("Scenario hash could not be looked up. Error message: %s.",
                 conditionMessage(e))
      errMsg <<- lang$errMsg$unknownError
    })
    if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
      return(NULL)
    }
    showJobSubmissionDialog(jobNameTmp, scenWithSameHash)
  })
  observeEvent(virtualActionButton(input$btSubmitAsyncJob, rv$btSubmitAsyncJob), {
    flog.debug("Confirm new asynchronous job button clicked.")
    sid <- NULL
    jobName <- input$jobSubmissionName
    if(isBadScenName(jobName)){
      showHideEl(session, "#jobSubmitBadName", 4000L)
      return()
    }
    if(length(activeScen) && length(activeScen$getSid())){
      if(!activeScen$hasExecPerm()){
        flog.error("User has no execute permission for this scenario. This looks like an attempt to tamper with the app!")
        return()
      }
      sid <- activeScen$getSid()
    }
    if(identical(worker$validateCredentials(), FALSE)){
      flog.error("User has no valid credentials. This looks like an attempt to tamper with the app!")
      return(NULL)
    }
    # submit job
    tryCatch({
      hideEl(session, "#jobSubmissionWrapper")
      showEl(session, "#jobSubmissionLoad")
      worker$runAsync(sid, name = jobName)
      showHideEl(session, "#jobSubmitSuccess", 2000)
    }, error = function(e){
      errMsg <- conditionMessage(e)
      flog.error("Some problem occurred while executing job. Error message: '%s'.", errMsg)
      
      if(identical(errMsg, '404') || startsWith(errMsg, "Could not") || 
         startsWith(errMsg, "Timeout"))
        return(showHideEl(session, "#jobSubmitUnknownHost", 6000))
      
      if(errMsg %in% c(401L, 403L))
        return(showHideEl(session, "#jobSubmitUnauthorized", 6000))
      
      showHideEl(session, "#jobSubmitUnknownError", 6000)
    }, finally = {
      hideEl(session, "#jobSubmissionLoad")
      hideModal(session, 2L)
    })
  })
}

if(config$activateModules$lstFile){
  output$listFileContainer <- renderText({
    req(rv$refreshLogs)
    errMsg <- NULL
    tryCatch({
      fileSize <- file.size(file.path(workDir, modelNameRaw %+% ".lst"))
      if(is.na(fileSize))
        stop("Could not access listing file", call. = FALSE)
      if(fileSize > maxSizeToRead){
        lang$errMsg$readLst$fileSize
      }else if(file.exists(file.path(workDir, modelNameRaw %+% ".lst"))){
        read_file(file.path(workDir, modelNameRaw %+% ".lst"))
      }else{
        lang$errMsg$readLst$fileNotFound
      }
    }, error = function(e) {
      flog.warn("GAMS listing file could not be read (model: '%s'). Error message: %s.", 
                modelName, conditionMessage(e))
      showErrorMsg(lang$errMsg$readLst$title, errMsg)
      return("")
    })
  })
}
if(config$activateModules$miroLogFile){
  renderMiroLogContent <- function(){
    miroLogAnnotations <<- NULL
    miroLogContent <- ""
    miroLogPath <- file.path(workDir, config$miroLogFile)
    tryCatch({
      if(file.exists(miroLogPath)[1]){
        inputScalarsTmp <- NULL
        if(scalarsFileName %in% names(modelIn))
          inputScalarsTmp  <- modelIn[[scalarsFileName]]$symnames
        miroLogContent     <- parseMiroLog(session, miroLogPath, 
                                           names(modelIn), inputScalarsTmp)
        miroLogAnnotations <<- miroLogContent$annotations
        miroLogContent <- miroLogContent$content
      }
    }, error = function(e){
      flog.warn("MIRO log file could not be read. Error message: '%s'.", conditionMessage(e))
    })
    return(HTML(paste(miroLogContent, collapse = "\n")))
  }
  if(config$activateModules$logFile){
    output$miroLogContainer <- renderUI({
      req(rv$refreshLogs)
      return(renderMiroLogContent())
    })
  }
}

logFilePath <- NULL
if(config$activateModules$logFile ||
   config$activateModules$miroLogFile){
  logfileObs <- NULL
  logfile <- NULL
  if(config$activateModules$logFile){
    logFilePath <- file.path(workDir, modelNameRaw %+% ".log")
  }else{
    logFilePath <- file.path(workDir, config$miroLogFile)
  }
  writeLogFileToDisk <- TRUE
  if(config$activateModules$attachments && 
     config$storeLogFilesDuration > 0L &&
     !is.null(activeScen)){
    if(!config$activateModules$logFile){
      writeLogFileToDisk <- FALSE
    }
  }else{
    writeLogFileToDisk <- FALSE
  }
  
  logObs <- observe({
    req(rv$triggerAsyncProcObserver)
    
    logText    <- NULL
    try(logText <- logfile(), silent = TRUE)
    
    if(!length(logText)) return()
    
    if(writeLogFileToDisk){
      write_file(logText, logFilePath, append = TRUE)
    }
    appendEl(session, "#logStatusContainer", logText, 
             scroll = identical(isolate(input$logUpdate), TRUE))
  })
}

output$modelStatus <- renderUI({
  req(rv$triggerAsyncProcObserver)
  
  currModelStat <- modelStatus()
  if(is.null(currModelStat)){
    return(lang$nav$gamsModelStatus$exec)
  }else if(identical(currModelStat, "s")){
    return(lang$nav$gamsModelStatus$submission)
  }else if(identical(currModelStat, "q")){
    return(lang$nav$gamsModelStatus$queued)
  }else if(identical(currModelStat, "d")){
    return(lang$nav$gamsModelStatus$collection)
  }
  modelStatusObs$destroy()
  modelStatus <<- NULL
  enableEl(session, "#btSolve")
  disableEl(session, "#btInterrupt")
  
  if(config$activateModules$logFile ||
     config$activateModules$miroLogFile){
    logfileObs$destroy()
    logfileObs <- NULL
    logfile <- NULL
    if(config$activateModules$miroLogFile){
      if(config$activateModules$logFile){
        containerId <- "#miroLogContainer"
      }else{
        containerId <- "#logStatusContainer"
      }
      setContent(session, containerId, renderMiroLogContent())
    }
  }
  
  if(currModelStat < 0){
    returnCodeText <- GAMSRCMAP[as.character(currModelStat)]
    if(is.na(returnCodeText)){
      returnCodeText <- as.character(currModelStat)
    }
    statusText <- lang$nav$gamsModelStatus$error %+% returnCodeText
    flog.debug("GAMS model was not solved successfully (model: '%s'). Model status: %s.", 
               modelName, statusText)
    return(htmltools::htmlEscape(statusText))
  }
  isolate({
    if(is.null(rv$refreshLogs)){
      rv$refreshLogs <- 1L
    }else{
      rv$refreshLogs <- rv$refreshLogs + 1L
    }
  })
  
  if(currModelStat != 0){
    returnCodeText <- GAMSRCMAP[as.character(currModelStat)]
    if(is.na(returnCodeText)){
      returnCodeText <- as.character(currModelStat)
    }
    statusText <- lang$nav$gamsModelStatus$error %+% returnCodeText
    if(config$activateModules$miroLogFile && length(miroLogAnnotations)){
      session$sendCustomMessage("gms-showValidationErrors", miroLogAnnotations)
      valIdHead <- match(names(miroLogAnnotations)[[1L]], names(modelIn))
      if(length(valIdHead) && !is.na(valIdHead)){
        valTabId <- 0L
        inputTabId <- tabSheetMap$input[[valIdHead]]
        updateTabsetPanel(session, "inputTabset", paste0("inputTabset_", inputTabId[1]))
        if(length(inputTabId) > 1L){
          updateTabsetPanel(session, paste0("inputTabset", inputTabId[1]), 
                            paste0("inputTabset", inputTabId[1], "_", 
                                   inputTabId[2]))
        }
      }
    }
    flog.debug("GAMS model was not solved successfully (model: '%s'). Model status: %s.", 
               modelName, statusText)
    tryCatch(
      worker$updateJobStatus(JOBSTATUSMAP['imported']), 
      error = function(e){
        flog.warn("Failed to update job status. Error message: '%s'.", 
                  conditionMessage(e))
      })
  }else{
    # run terminated successfully
    statusText <- lang$nav$gamsModelStatus$success
    
    if(inconsistentOutput){
      showInconsistentOutputDialog()
      return(statusText)
    }
    
    errMsg <- NULL
    tryCatch({
      loadOutputData()
    }, error = function(e){
      flog.error("Problems loading output data. Error message: %s.", conditionMessage(e))
      errMsg <<- lang$errMsg$readOutput$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
      return(htmltools::htmlEscape(statusText))
    }
    #select first tab in current run tabset
    switchTab(session, "output")
    updateTabsetPanel(session, "scenTabset",
                      selected = "results.current")
    isolate(renderOutputData())
    
    markUnsaved()
  }
  return(statusText)
})
# refresh even when modelStatus message is hidden (i.e. user is on another tab)
outputOptions(output, "modelStatus", suspendWhenHidden = FALSE)

verifyCanSolve <- function(async = FALSE, buttonId = "btSolve"){
  if(!async && length(modelStatus)){
    showErrorMsg(lang$errMsg$jobRunning$title, 
                 lang$errMsg$jobRunning$desc)
    return(FALSE)
  }
  if(length(unzipModelFilesProcess)){
    if(length(unzipModelFilesProcess$get_exit_status())){
      unzipModelFilesProcess <<- NULL
    }else{
      showErrorMsg(lang$errMsg$unzipProcessRunning$title, 
                   lang$errMsg$unzipProcessRunning$desc)
      return(FALSE)
    }
  }
  if(length(activeScen) && !activeScen$hasExecPerm()){
    if(LAUNCHHCUBEMODE){
      modeDescriptor <- "dialogNoExecPermHC"
    }else{
      modeDescriptor <- "dialogNoExecPerm"
    }
    showErrorMsg(lang$nav[[modeDescriptor]]$title, 
                 lang$nav[[modeDescriptor]]$desc)
    flog.info("User has no execute permission for this scenario.")
    return(FALSE)
  }
  if(identical(worker$validateCredentials(), FALSE)){
    showLoginDialog(cred = worker$getCredentials(), 
                    forwardOnSuccess = buttonId)
    return(FALSE)
  }
  return(TRUE)
}

runGAMSJob <- function(){
  clearLogs(session)
  # run GAMS
  errMsg <- NULL
  inconsistentOutput <<- FALSE
  tryCatch({
    jobSid <- NULL
    if(length(activeScen) && length(activeScen$getSid())){
      jobSid <- activeScen$getSid()
    }
    worker$run(jobSid, name = activeScen$getScenName())
  }, error_duplicate_records = function(e){
    flog.info("Problems writing GDX file. Duplicate records found: %s", conditionMessage(e))
    errMsg <<- conditionMessage(e)
  }, error = function(e) {
    flog.error("GAMS did not execute successfully. Error message: %s.",
               conditionMessage(e))
    errMsg <<- lang$errMsg$gamsExec$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
    return(NULL)
  }
  if(config$activateModules$remoteExecution)
    updateTabsetPanel(session, "jobListPanel", selected = "current")
  if(config$activateModules$logFile){
    updateTabsetPanel(session, "logFileTabsset", selected = "log")
  }else if(config$activateModules$miroLogFile){
    updateTabsetPanel(session, "logFileTabsset", selected = "mirolog")
  }
  
  #activate Interrupt button as GAMS is running now
  updateActionButton(session, "btInterrupt", icon = character(0L))
  enableEl(session, "#btInterrupt")
  switchTab(session, "gamsinter")
  
  if(!is.null(logFilePath) &&
     !identical(unlink(logFilePath, force = TRUE), 0L)){
    flog.warn("Could not remove log file: '%s'.", logFilePath)
  }
  
  errMsg <- NULL
  tryCatch({
    modelStatusRE  <- worker$getReactiveStatus(session)
    modelStatusObs <<- modelStatusRE$obs
    modelStatus    <<- modelStatusRE$re
  }, error = function(e) {
    flog.error("GAMS status could not be retrieved. Error message: %s.",
               conditionMessage(e))
    errMsg <<- lang$errMsg$readLog$desc
  })
  
  if(is.null(showErrorMsg(lang$errMsg$readLog$title, errMsg))){
    return()
  }
  
  tryCatch({
    logRE       <- worker$getReactiveLog(session)
    logfileObs  <<- logRE$obs
    logfile     <<- logRE$re
  }, error = function(e) {
    flog.error("GAMS log file could not be read (model: '%s'). Error message: %s.",
               modelName, conditionMessage(e))
    errMsg <<- lang$errMsg$readLog$desc
  })
  showErrorMsg(lang$errMsg$readLog$title, errMsg)
  
  if(is.null(rv$triggerAsyncProcObserver)){
    rv$triggerAsyncProcObserver <- 1L
  }else{
    rv$triggerAsyncProcObserver <- rv$triggerAsyncProcObserver + 1L
  }
}

observeEvent(virtualActionButton(input$btSolve, rv$btSolve), {
  flog.debug("Solve button clicked (model: '%s').", modelName)
  
  if(!verifyCanSolve()){
    return()
  }
  if(LAUNCHHCUBEMODE){
    numberScenarios <- getNoScenToSolve()
    if(numberScenarios > MAX_NO_HCUBE){
      showModal(modalDialog(title = lang$nav$dialogHcube$exceedMaxNoDialog$title, 
                            sprintf(lang$nav$dialogHcube$exceedMaxNoDialog$desc, 
                                    format(numberScenarios, big.mark=","),
                                    format(MAX_NO_HCUBE, big.mark=","))))
      return(NULL)
    }else if(numberScenarios == -1){
      showModal(modalDialog(title = lang$nav$dialogHcube$badStepSizeDialog$title, 
                            lang$nav$dialogHcube$badStepSizeDialog$desc))
      return(NULL)
    }else if(numberScenarios == 0){
      showModal(modalDialog(title = lang$nav$dialogHcube$noScenSelectedDialog$title, 
                            lang$nav$dialogHcube$noScenSelectedDialog$desc))
      return(NULL)
    }
    disableEl(session, "#btSolve")
    prog <- Progress$new()
    on.exit(suppressWarnings(prog$close()))
    prog$set(message = lang$progressBar$prepRun$title, value = 0)
    
    idsSolved <<- db$importDataset("_scenMeta", colNames = "_sname", 
                                   tibble("_scode", SCODEMAP[['scen']], ">"))
    if(length(idsSolved)){
      idsSolved <<- unique(idsSolved[[1L]])
    }
    errMsg <- NULL
    prog$inc(amount = 0.5, detail = lang$progressBar$prepRun$sendInput)
    tryCatch(
      scenToSolve <- scenToSolve(),
      error = function(e){
        flog.error("Problems getting list of scenarios to solve in Hypercube mode. Error message: '%s'.",
                   conditionMessage(e))
        errMsg <<- lang$errMsg$GAMSInput$desc
      })
    if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
      return(NULL)
    }
    
    idsToSolve          <<- scenToSolve$ids
    attachmentFilePaths <<- scenToSolve$attachmentFilePaths
    
    if(length(config$extraClArgs)){
      scenGmsPar <<- paste(scenToSolve$gmspar, 
                           paste(config$extraClArgs, collapse = " "),
                           "lo=3")
    }else{
      scenGmsPar <<- paste(scenToSolve$gmspar,
                           "lo=3")
    }
    if(config$saveTraceFile){
      scenGmsPar <<- paste0(scenGmsPar, ' trace="_scenTrc.trc" traceopt=3')
    }
    
    sidsDiff <- setdiff(idsToSolve, idsSolved)
    prog$close()
    showHcubeSubmitDialog(noIdsToSolve = length(idsToSolve), noIdsExist = length(idsToSolve) - length(sidsDiff))
    
    enableEl(session, "#btSolve")
    
    return(NULL)
  }
  inputData <- prepareModelRun(async = FALSE)
  if(is.null(inputData)){
    return(NULL)
  }
  worker$setInputData(inputData)
  tryCatch({
    scenHashTmp <- inputData$generateScenHash()
    scenWithSameHash <- db$getScenWithSameHash(scenHashTmp)
    activeScen$setScenHash(scenHashTmp)
  }, error = function(e) {
    flog.error("Scenario hash could not be looked up. Error message: %s.",
               conditionMessage(e))
    errMsg <<- lang$errMsg$unknownError
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
    return(NULL)
  }
  if(length(scenWithSameHash) && nrow(scenWithSameHash) > 0L){
    showHashExistsDialog(scenWithSameHash)
    return(NULL)
  }
  runGAMSJob()
})
observeEvent(input$btRunNoCheckHash, {
  flog.debug("Solve button (no check scen hash) clicked (model: '%s').", modelName)
  if(!verifyCanSolve()){
    return()
  }
  removeModal()
  runGAMSJob()
})
if(!isShinyProxy && config$activateModules$remoteExecution){
  observeEvent(input$btRemoteExecLogin, {
    showLoginDialog(cred = worker$getCredentials())
  })
  observeEvent(input$btSaveCredentials, {
    tryCatch({
      worker$login(url = input$remoteCredUrl,
                   username = input$remoteCredUser,
                   password = input$remoteCredPass,
                   namespace = input$remoteCredNs,
                   useRegistered = input$remoteCredReg,
                   rememberMe = input$remoteCredRemember)
      hideEl(session, "#btRemoteExecLogin")
      showEl(session, "#remoteExecLogoutDiv")
      removeModal()
      if(input$btSaveCredentials %in% names(rv)){
        rv[[input$btSaveCredentials]] <- rv[[input$btSaveCredentials]] + 1L
      }
    }, error = function(e){
      errMsg <- conditionMessage(e)
      flog.info("Problems logging in. Return code: %s", errMsg)
      if(identical(errMsg, '404') || startsWith(errMsg, "Could not") || 
         startsWith(errMsg, "Timeout"))
        return(showHideEl(session, "#remoteLoginHostNotFound", 6000))
      
      if(identical(errMsg, '444'))
        return(showHideEl(session, "#remoteLoginNsNotFound", 6000))
      
      if(identical(errMsg, '445'))
        return(showHideEl(session, "#remoteLoginModelNotFound", 6000))
      
      if(identical(errMsg, '400'))
        return(showHideEl(session, "#remoteLoginNsNotFound", 6000))
      
      if(identical(errMsg, '401'))
        return(showHideEl(session, "#remoteLoginInvalidCred", 6000))
      
      if(identical(errMsg, '403'))
        return(showHideEl(session, "#remoteLoginInsuffPerm", 6000))
      
      if(identical(errMsg, '426'))
        return(showHideEl(session, "#remoteLoginInvalidProt", 6000))
      
      return(showErrorMsg(lang$errMsg$fileWrite$title, lang$errMsg$unknownError))
    })
  })
  observeEvent(input$btRemoteExecLogout, {
    removeModal()
    tryCatch({
      worker$logout()
      showEl(session, "#btRemoteExecLogin")
      hideEl(session, "#remoteExecLogoutDiv")
    }, error = function(e){
      flog.error("Problems setting credentials: %s", conditionMessage(e))
      showErrorMsg(lang$errMsg$fileWrite$title, sprintf(lang$errMsg$fileWrite$desc,
                                                        rememberMeFileName))
    })
  })
}
if(identical(config$activateModules$hcube, TRUE)){
  hcubeBuilder <- NULL
  generateHcInterface <- function(){
    tagList(
      tags$div(class = "container-fluid",
               style = "max-height:600px;max-height:60vh;overflow:auto;padding-bottom:50px;",
               tags$div(class = "row",
                        lapply(seq_along(config$hcModule$scalarsConfig), function(widgetId){
                          scalarConfig <- config$hcModule$scalarsConfig[[widgetId]]
                          symId <- match(scalarConfig$name, names(modelIn))
                          dependencyId <- match(scalarConfig$name, names(modelInWithDep))
                          tags$div(class = "col-sm-6", {
                            if(identical(scalarConfig$type, "dropdown")){
                              if(identical(scalarConfig$baseType, "checkbox")){
                                dropdownVal <- suppressWarnings(as.integer(isolate(input[[paste0("cb_", symId)]])))
                              }else{
                                dropdownVal <- isolate(input[[paste0("dropdown_", symId)]])
                              }
                              if(!is.na(dependencyId)){
                                scalarConfig$choices <- isolate(getData[[dependencyId]]())
                              }
                              selectInput(paste0("hcWidget_", widgetId), 
                                          label = scalarConfig$label, 
                                          choices = scalarConfig$choices, 
                                          selected = dropdownVal, 
                                          multiple = TRUE,
                                          width = "100%")
                            }else if(identical(scalarConfig$type, "slider")){
                              sliderVal <- isolate(input[[paste0("slider_", symId)]])
                              if(length(sliderVal)){
                                sliderVal <- c(sliderVal, sliderVal)
                              }else{
                                sliderVal <- c(scalarConfig$default, scalarConfig$default)
                              }
                              if(!is.na(dependencyId)){
                                configTmp <- isolate(getData[[dependencyId]]())
                                scalarConfig$min  <- configTmp$min
                                scalarConfig$max  <- configTmp$max
                                scalarConfig$step <- configTmp$step
                              }
                              tags$div(class = "row", style = "overflow:hidden;background:",
                                       tags$div(class = if(scalarConfig$single) "col-sm-10" else "col-sm-8",
                                                sliderInput(paste0("hcWidget_", widgetId), 
                                                            label = scalarConfig$label,
                                                            min = scalarConfig$min,
                                                            max = scalarConfig$max,
                                                            value = sliderVal,
                                                            step = scalarConfig$step,
                                                            ticks = scalarConfig$ticks,
                                                            width = "100%")),
                                       if(scalarConfig$single){
                                         tags$div(class = "col-sm-2",
                                                  numericInput(paste0("hcWidget_", widgetId, "_step"),
                                                               lang$nav$hcubeMode$stepsize, 
                                                               scalarConfig$step, min = scalarConfig$minStep,
                                                               width = "100%"))
                                       }else{
                                         tagList(
                                           tags$div(class = "col-sm-2",
                                                    checkboxInput_MIRO(paste0("hcWidget_", widgetId, "_combinations"),
                                                                       lang$nav$hcubeMode$sliderAllCombinations,
                                                                       value = FALSE)),
                                           tags$div(class = "col-sm-2",
                                                    conditionalPanel(paste0("input.hcWidget_", widgetId, "_combinations===true"),
                                                                     numericInput(paste0("hcWidget_", widgetId, "_step"),
                                                                                  lang$nav$hcubeMode$stepsize, 
                                                                                  scalarConfig$step, min = scalarConfig$minStep,
                                                                                  width = "100%")))
                                         )
                                       }
                              )
                            }else{
                              stop("HC widget type  not implemented (should never happen)", call. = FALSE)
                            }
                          })
                        }))),
      tags$div(class = "row", style = "text-align:center;",
               actionButton("btSubmitHcJobConfirm", lang$nav$hcModule$submissionDialog$btSubmitAll,
                            class = "bt-highlight-1 bt-gms-confirm bt-no-disable"),
               actionButton("btSubmitHcJobConfirmUnsolved", lang$nav$hcModule$submissionDialog$btSubmitUnsolved,
                            class = "bt-highlight-1 bt-gms-confirm bt-no-disable",
                            style = "display:none;")))
  }
  noHcubeScen <- throttle(reactive({
    req(input$btSubmitHcJob)
    noScenTmp <- vapply(seq_along(config$hcModule$scalarsConfig), function(widgetId){
      widgetVal <- input[[paste0("hcWidget_", widgetId)]]
      if(is.null(widgetVal)){
        return(NA_integer_)
      }
      scalarConfig <- config$hcModule$scalarsConfig[[widgetId]]
      if(identical(scalarConfig$type, "slider")){
        if(!scalarConfig$single){
          if(!identical(input[[paste0("hcWidget_", widgetId, "_combinations")]], TRUE)){
            hcubeBuilder$pushRange(paste0(scalarConfig$name, "_lo"),
                                   paste0(scalarConfig$name, "_up"), widgetVal)
            return(1L)
          }
        }
        
        stepSize <- input[[paste0("hcWidget_", widgetId, "_step")]]
        if(is.null(stepSize)){
          return(NA_integer_)
        }
        if(!is.numeric(stepSize) || stepSize <= 0){
          # non valid step size selected
          return(-1L)
        }
        hcRange <- floor((widgetVal[2] - widgetVal[1])/stepSize) + 1
        
        if(length(scalarConfig$minStep)){
          if(stepSize < scalarConfig$minStep){
            return(-1L)
          }
        }else if(stepSize < scalarConfig$step){
          return(-1L)
        }
        if(scalarConfig$single){
          hcubeBuilder$push(scalarConfig$name, seq(widgetVal[1], widgetVal[2], stepSize))
          return(as.integer(hcRange))
        }
        # double slider all combinations
        hcubeBuilder$pushRange(paste0(scalarConfig$name, "_lo"),
                               paste0(scalarConfig$name, "_up"),
                               getCombinationsSlider(widgetVal[1], widgetVal[2], stepSize),
                               allCombinations = TRUE)
        return(as.integer(hcRange*(hcRange + 1) / 2))
      }else{
        hcubeBuilder$push(scalarConfig$name, widgetVal, ddChoices = scalarConfig$choices)
        return(length(widgetVal))
      }
    }, integer(1L), USE.NAMES = FALSE)
    if(any(is.na(noScenTmp))){
      return()
    }
    if(any(noScenTmp == -1L)){
      return(-1L)
    }
    return(prod(noScenTmp))
  }), 1000L)
  noHcubeScenSolved <- throttle(reactive({
    req(rv$refreshHcubeHashes > 0L)
    if(!length(noHcubeScen()) || noHcubeScen() < 1L || noHcubeScen() > MAX_NO_HCUBE){
      return(0L)
    }
    tryCatch({
      scenHashes <- hcubeBuilder$generateScenHashes()
      return(db$getScenWithSameHash(scenHashes,
                                    limit = NULL, count = TRUE)[[1]][1])
    }, error = function(e) {
      flog.error("Scenario hashes could not be looked up. Error message: %s.",
                 conditionMessage(e))
      return(-1L)
    })
  }), 1200L)
  output$newHcJobInfo <- renderText({
    noScenTmp <- noHcubeScen()
    disableEl(session, "#btSubmitHcJobConfirm")
    disableEl(session, "#btSubmitHcJobConfirmUnsolved")
    hideEl(session, "#btSubmitHcJobConfirmUnsolved")
    if(is.null(noScenTmp)){
      return()
    }
    if(noScenTmp == -1){
      showElReplaceTxt(session, "#newHcJobError",
                       lang$nav$dialogHcube$badStepSizeDialog$desc)
    }else if(noScenTmp == 0){
      showElReplaceTxt(session, "#newHcJobError",
                       lang$nav$dialogHcube$noScenSelectedDialog$desc)
    }else if(noScenTmp > MAX_NO_HCUBE){
      showElReplaceTxt(session, "#newHcJobError",
                       sprintf(lang$nav$dialogHcube$exceedMaxNoDialog$desc, 
                               format(noScenTmp, big.mark=","), format(MAX_NO_HCUBE, big.mark=",")))
    }else{
      enableEl(session, "#btSubmitHcJobConfirm")
      hideEl(session, "#newHcJobError")
    }
    if(noHcubeScenSolved() > 0L){
      showEl(session, "#btSubmitHcJobConfirmUnsolved")
      if(noHcubeScenSolved() < noScenTmp){
        enableEl(session, "#btSubmitHcJobConfirmUnsolved")
      }
    }
    sprintf(lang$nav$dialogHcube$desc, noScenTmp, noHcubeScenSolved())
  })
  observeEvent(input$btSubmitHcJob, {
    flog.trace("Submit new Hypercube job button clicked.")
    jobNameTmp <- character(1L)
    if(!verifyCanSolve(async = TRUE, buttonId = "btSubmitHcJob")){
      return()
    }
    if(length(activeScen)){
      jobNameTmp <- activeScen$getScenName()
    }
    inputData <- prepareModelRun(async = TRUE)
    if(is.null(inputData)){
      return(NULL)
    }
    worker$setInputData(inputData)
    if(is.null(hcubeBuilder)){
      hcubeBuilder <<- HcubeBuilder$new(inputData$getDataHashes(), inputData$getScalarData())
    }else{
      hcubeBuilder$setDataHashes(inputData$getDataHashes(), inputData$getScalarData())
    }
    rv$refreshHcubeHashes <- rv$refreshHcubeHashes + 1L
    
    showModal(modalDialog(
      title = lang$nav$hcModule$submissionDialog$title,
      tags$div(id = "newHcJobError", class = "gmsalert gmsalert-error",
               style = "position: relative;max-width: unset;"),
      tags$div(id = "newHcJobInfo", class = "shiny-text-output lead"),
      selectizeInput("newHcubeTags", lang$nav$dialogHcube$newTags, c(),
                     multiple = TRUE, options = list(
                       'create' = TRUE,
                       'persist' = FALSE)),
      tags$div(id = "newHcWrapper",
               genSpinner(absolute = FALSE)),
      easyClose = FALSE,
      size = "l",
      footer = tagList(
        modalButton(lang$nav$hcModule$submissionDialog$btCancel)
      )
    ))
    emptyEl(session, "#newHcWrapper")
    tryCatch({
      insertUI("#newHcWrapper", ui = generateHcInterface())
    }, error = function(e){
      flog.error("Unexpected error while generating Hypercube job interface. Error message: '%s'",
                 conditionMessage(e))
      showElReplaceTxt(session, "#newHcJobError", lang$errMsg$unknownError)
    })
  })
  observeEvent(input$btSubmitHcJobConfirmUnsolved, {
    flog.trace("Button to confirm submission of new Hypercube job (unsolved scenarios only) clicked.")
    if(!verifyCanSolve(async = TRUE, buttonId = "btSubmitHcJob")){
      return()
    }
    if(!length(noHcubeScen()) || noHcubeScen() < 1L || noHcubeScen() > MAX_NO_HCUBE){
      return()
    }
    if(tryCatch({
      existingHashes <- db$getScenWithSameHash(hcubeBuilder$getScenHashes(),
                                               limit = NULL, count = FALSE, distinctHashes = TRUE)[[1]]
      FALSE
    }, error = function(e){
      flog.error("Problems fetching existing hashes from database. Error message: %s",
                 conditionMessage(e))
      return(TRUE)
    })){
      return()
    }
    hcubeBuilder$removeScen(existingHashes)
    rv$submitHCJobConfirm <- rv$submitHCJobConfirm + 1L
  })
  observeEvent(virtualActionButton(input$btSubmitHcJobConfirm, rv$submitHCJobConfirm), {
    flog.trace("Button to confirm submission of new Hypercube job (all scenarios) clicked.")
    if(!verifyCanSolve(async = TRUE, buttonId = "btSubmitHcJob")){
      return()
    }
    if(!length(noHcubeScen()) || noHcubeScen() < 1L || noHcubeScen() > MAX_NO_HCUBE){
      return()
    }
    if(hcubeBuilder$getNoScen() < 1L){
      flog.error("Hypercube submission button clicked without scenarios available in HcubeBuilder. This should never happen and is likely an attempt to tamper with the app!")
      showElReplaceTxt(session, "#newHcJobError", lang$errMsg$unknownError)
      return()
    }
    disableEl(session, "#btSubmitHcJobConfirmUnsolved")
    disableEl(session, "#btSubmitHcJobConfirm")
    hideEl(session, "#newHcJobError")
    tryCatch({
      prog <- Progress$new()
      on.exit(prog$close(), add = TRUE)
      prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
      currentMetaTmp <- activeScen$getMetadataInfo()
      attachmentsTmp <- Attachments$new(db, list(maxSize = attachMaxFileSize, maxNo = attachMaxNo),
                                        workDir,
                                        names(ioConfig$modelIn),
                                        names(ioConfig$modelOut),
                                        ioConfig$inputDsNamesBase)
      if(length(currentMetaTmp$attach) && length(config$outputAttachments)){
        outputAttachNames <- vapply(config$outputAttachments, "[[", character(1L),
                                    "filename", USE.NAMES = FALSE)
        currentMetaTmp$attach$attachmentsToRemove <- c(currentMetaTmp$attach$attachmentsToRemove,
                                                       outputAttachNames)
        localOutputAttach <- basename(currentMetaTmp$attach$localAttachments$filePaths) %in% outputAttachNames
        if(any(localOutputAttach)){
          currentMetaTmp$attach$localAttachments$filePaths <- currentMetaTmp$attach$localAttachments$filePaths[!localOutputAttach]
          currentMetaTmp$attach$localAttachments$execPerm <- currentMetaTmp$attach$localAttachments$execPerm[!localOutputAttach]
          updatesOutputAttach <- currentMetaTmp$attach$attachmentsUpdateExec$name %in% outputAttachNames
          if(any(updatesOutputAttach)){
            currentMetaTmp$attach$attachmentsUpdateExec$name <- currentMetaTmp$attach$attachmentsUpdateExec$name[!updatesOutputAttach]
            currentMetaTmp$attach$attachmentsUpdateExec$execPerm <- currentMetaTmp$attach$attachmentsUpdateExec$execPerm[!updatesOutputAttach]
          }
        }
      }
      hcJobConfig <- Scenario$new(db = db, sname = paste(rv$activeSname, as.numeric(Sys.time())), 
                                  tags = input$newHcubeTags, overwrite = FALSE,
                                  isNewScen = TRUE,
                                  duplicatedMetadata = currentMetaTmp,
                                  views = views, attachments = attachmentsTmp,
                                  scode = SCODEMAP[['hcube_inputs']])
      hcScalarsTmp <- hcubeBuilder$getHcubeScalars()
      hcJobConfig$save(scenData$get("sb", symNames = ioConfig$inputDsNames),
                       msgProgress = lang$progressBar$saveScenDb)
      sid <- hcJobConfig$getSid()
      db$exportScenDataset(bind_cols(`_sid` = rep.int(sid, nrow(hcScalarsTmp)), 
                                     hcScalarsTmp),
                           "_hcScalars")
      hcJobConfig$finalize()
      worker$runHcube(dynamicPar = hcubeBuilder, 
                      sid = sid, tags = input$newHcubeTags)
      prog$inc(amount = 1, detail = lang$nav$dialogHcube$waitDialog$desc)
      removeModal()
      showHideEl(session, "#hcubeSubmitSuccess", 2000)
    }, error = function(e){
      flog.error("Unexpected error while generating new Hypercube job. Error message: '%s'",
                 conditionMessage(e))
      enableEl(session, "#btSubmitHcJobConfirm")
      enableEl(session, "#btSubmitHcJobConfirmUnsolved")
      showElReplaceTxt(session, "#newHcJobError", lang$errMsg$unknownError)
    })
  })
}
