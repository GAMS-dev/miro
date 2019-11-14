# run GAMS
storeGAMSOutputFiles <- function(workDir){
  if(config$activateModules$attachments && 
     config$storeLogFilesDuration > 0L && !is.null(activeScen)){
    errMsg <- NULL
    tryCatch({
      filesToStore <- character(0L)
      if(any(c(config$activateModules$logFile, config$activateModules$lstFile)))
        filesToStore  <- c(file.path(workDir, 
                                     paste0(modelNameRaw, 
                                            c(if(config$activateModules$logFile) ".log", 
                                              if(config$activateModules$lstFile) ".lst"))))
      if(config$activateModules$miroLogFile)
        filesToStore <- c(filesToStore, file.path(workDir, config$miroLogFile))
      
      if(!length(filesToStore))
        return()
      
      filesNoAccess <- file.access(filesToStore) == -1L
      if(any(filesNoAccess)){
        if(all(filesNoAccess))
          stop("fileAccessException", call. = FALSE)
        flog.info("GAMS output files: '%s' could not be accessed. Check file permissions.", 
                  paste(filesToStore[filesNoAccess], collapse = "', '"))
        errMsg <- lang$errMsg$saveGAMSLog$noFileAccess
      }
      filesToStore <- filesToStore[!filesNoAccess]
      filesTooLarge <- file.size(filesToStore) > attachMaxFileSize
      if(any(filesTooLarge)){
        if(all(filesTooLarge))
          stop("fileSizeException", call. = FALSE)
        flog.info("GAMS output files: '%s' are too large. They will not be saved.", 
                  paste(filesToStore[filesTooLarge], collapse = "', '"))
        errMsg <<- paste(errMsg, lang$errMsg$saveGAMSLog$fileSizeExceeded, sep = "\n")
      }
      filesToStore <- filesToStore[!filesTooLarge]
      activeScen$addAttachments(filesToStore, overwrite = TRUE, 
                                execPerm = rep.int(FALSE, length(filesToStore)))
    }, error = function(e){
      switch(conditionMessage(e),
             fileAccessException = {
               flog.info("No GAMS output files could not be accessed. Check file permissions.")
               errMsg <<- lang$errMsg$saveGAMSLog$noFileAccess
             },
             fileSizeException = {
               flog.info("All GAMS output files are too large to be saved.")
               errMsg <<- lang$errMsg$saveGAMSLog$fileSizeExceeded
             },
             {
               flog.error("Problems while trying to store GAMS output files in the database. Error message: '%s'.", e)
               errMsg <<- lang$errMsg$unknownError
             })
    })
    showErrorMsg(lang$errMsg$saveGAMSLog$title, errMsg)
  }
}
prepareModelRun <- function(async = FALSE){
  prog <- Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$prepRun$title, value = 0)
  
  prog$inc(amount = 0.5, detail = lang$progressBar$prepRun$sendInput)
  # save input data 
  saveInputDb <- FALSE
  source("./modules/input_save.R", local = TRUE)
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  pfFileContent <- NULL
  inputData <- DataInstance$new(modelInFileNames, fileExchange = config$fileExchange,
                                gdxio = gdxio, csvDelim = config$csvDelim)
  lapply(seq_along(dataTmp), function(i){
    # write compile time variable file and remove compile time variables from scalar dataset
    if(identical(tolower(names(dataTmp)[[i]]), scalarsFileName)){
      # scalars file exists, so remove compile time variables from it
      DDParIdx           <- grepl(paste("^", DDPar, "(_lo|_up)?$", sep = "", collapse = "|"), dataTmp[[i]][[1]])
      GMSOptIdx          <- grepl(paste("^", GMSOpt, "(_lo|_up)?$", sep = "", collapse = "|"), dataTmp[[i]][[1]])
      DDParValues        <- dataTmp[[i]][DDParIdx, , drop = FALSE]
      GMSOptValues       <- dataTmp[[i]][GMSOptIdx, , drop = FALSE]
      if(nrow(DDParValues) || nrow(GMSOptValues)){
        pfGMSPar      <- vapply(seq_along(DDParValues[[1]]), 
                                function(i){
                                  if(!DDParValues[[3]][i] %in% c("_", "system.empty", "")) 
                                    paste0('--', substring(DDParValues[[1]][i], nchar(prefixDDPar) + 1L), '=', 
                                           escapeGAMSCL(DDParValues[[3]][i]))
                                  else
                                    NA_character_
                                }, character(1L), USE.NAMES = FALSE)
        pfGMSPar      <- pfGMSPar[!is.na(pfGMSPar)]
        # do not write '_' in pf file (no selection)
        pfGMSOpt      <- vapply(seq_along(GMSOptValues[[1]]), 
                                function(i){
                                  if(!GMSOptValues[[3]][i] %in% c("_", "system.empty", "")) 
                                    paste0(substring(GMSOptValues[[1]][i], nchar(prefixGMSOpt) + 1L), '=', 
                                           escapeGAMSCL(GMSOptValues[[3]][i]))
                                  else
                                    NA_character_
                                }, character(1L), USE.NAMES = FALSE)
        pfGMSOpt      <- pfGMSOpt[!is.na(pfGMSOpt)]
        pfFileContent <<- c(pfGMSPar, pfGMSOpt)
        # remove those rows from scalars file that are compile time variables
        csvData <- dataTmp[[i]][!(DDParIdx | GMSOptIdx), ]
      }else{
        csvData <- dataTmp[[i]]
      }
      rm(GMSOptValues, DDParValues)
    }else if(identical(modelIn[[names(dataTmp)[[i]]]]$dropdown$multiple, TRUE)){
      # append alias column
      csvData           <- dataTmp[[i]]
      choiceIdx         <- match(csvData[[1L]], 
                                 modelIn[[names(dataTmp)[[i]]]]$dropdown$choices)
      if(length(choiceIdx)){
        if(any(is.na(choiceIdx)) || 
           !length(modelIn[[names(dataTmp)[[i]]]]$dropdown$aliases)){
          csvData[["text"]] <- ""
        }else{
          aliasCol          <- modelIn[[names(dataTmp)[[i]]]]$dropdown$aliases[choiceIdx]
          aliasCol[is.na(aliasCol)] <- ""
          csvData[["text"]] <- aliasCol
        }
      }else{
        csvData[["text"]] <- character(0L)
      }
    }else{
      csvData <- dataTmp[[i]]
    }
    
    inputData$push(names(dataTmp)[[i]], csvData)
  })
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  tryCatch({
    if(config$activateModules$attachments && attachAllowExec && !is.null(activeScen)){
      prog$inc(amount = 0, detail = lang$progressBar$prepRun$downloadAttach)
      inputData$addFilePaths(activeScen$downloadAttachmentData(workDir, allExecPerm = TRUE))
    }
    prog$close()
  }, error = function(e) {
    errMsg <<- lang$errMsg$gamsExec$desc
    flog.error("Attachment data could not be downloaded. Error message: %s.", conditionMessage(e))
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
    return(NULL)
  }
  return(list(inputData = inputData, pfFileContent = pfFileContent, dataTmp = if(!async) dataTmp))
}
if(LAUNCHHCUBEMODE){
  idsToSolve <- NULL
  idxDiff <- NULL
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
      return(paste0("--HCUBE_STATIC_", names(modelIn)[id]))
    }else{
      return(paste0("--HCUBE_SCALARV_", names(modelIn)[id]))
    }
  }
  noScenToSolve <- reactive({
    numberScenPerElement <- vapply(seq_along(modelIn), function(i){
      switch(modelIn[[i]]$type,
             slider = {
               value <- input[["slider_" %+% i]]
               if(length(value) > 1){
                 if(identical(modelIn[[i]]$slider$double, TRUE)
                    && !identical(input[["hcubeMode_" %+% i]], TRUE)){
                   # double slider in single run mode
                   return(1L)
                 }
                 
                 stepSize <- input[["hcubeStep_" %+% i]]
                 range <- floor((value[2] - value[1])/stepSize) + 1
                 if(!is.numeric(stepSize) || stepSize <= 0 
                    || is.numeric(modelIn[[i]]$slider$step) 
                    && stepSize < modelIn[[i]]$slider$step){
                   # non valid step size selected
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
             hot = {
               return(1L)
             },
             dt = {
               return(1L)
             },
             dropdown = {
               if(identical(modelIn[[i]]$dropdown$single, TRUE)){
                 return(length(input[["dropdown_" %+% i]]))
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
               return(length(input[["cb_" %+% i]]))
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
  })
  
  getUniqueCombinations <- function(vector){
    combinations <- expand.grid(vector, vector, stringsAsFactors = FALSE)
    combinations[combinations[, 1] <= combinations[, 2], ] 
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
                                    gdxio = gdxio, csvDelim = config$csvDelim)
    modelInSorted <- sort(names(modelIn))
    elementValues <- lapply(seq_along(modelIn), function(j){
      updateProgress(incAmount = 1/(length(modelIn) + 18), detail = lang$nav$dialogHcube$waitDialog$desc)
      i <- match(names(modelIn)[j], modelInSorted)
      parPrefix <- getHcubeParPrefix(i)
      switch(modelIn[[i]]$type,
             slider = {
               value <- input[["slider_" %+% i]]
               if(length(value) > 1){
                 if(identical(modelIn[[i]]$slider$double, TRUE)
                    && !identical(input[["hcubeMode_" %+% i]], TRUE)){
                   # double slider in single run mode
                   return(paste0(parPrefix, "_lo=", value[1], 
                                 " ", parPrefix, "_up=", value[2]))
                 }
                 
                 stepSize <- input[["hcubeStep_" %+% i]]
                 if(identical(modelIn[[i]]$slider$single, TRUE)){
                   return(paste0(parPrefix, "=", seq(value[1], value[2], stepSize)))
                 }
                 # double slider all combinations
                 value <- getCombinationsSlider(value[1], value[2], stepSize)
                 return(paste0(parPrefix, "_lo=", value$min, 
                               " ", parPrefix, "_up=", value$max))
               }else{
                 return(paste0(parPrefix, "=", value))
               }
             },
             dropdown = {
               if(identical(modelIn[[i]]$dropdown$single, FALSE)){
                 data <- tibble(input[["dropdown_" %+% i]])
                 names(data) <- tolower(names(modelIn))[i]
                 return(paste0(parPrefix, "=", digest(data, algo = "md5")))
               }
               value <- strsplit(input[["dropdown_" %+% i]], "||", fixed = TRUE)
               text <- vapply(value, function(valEl){
                 if(length(valEl) > 1L) 
                   return(paste0(" --HCUBE_SCALART_", tolower(names(modelIn)[i]), 
                                 "=", paste(valEl[-1], collapse = "||")))
                 return("")}, character(1L), USE.NAMES = FALSE)
               value <- paste0(vapply(value, "[[", character(1L), 1L, USE.NAMES = FALSE),
                               text)
               
               if("_" %in% value){
                 value <- value[value != "_"]
                 return(c(if(length(value)) paste0(parPrefix, "=", escapeGAMSCL(value)), ""))
               }
               return(paste0(parPrefix, "=", escapeGAMSCL(value)))
             },
             date = {
               return(paste0(parPrefix, "=", input[["date_" %+% i]]))
             },
             daterange = {
               value <- as.character(input[["daterange_" %+% i]])
               
               return(paste0(parPrefix, "_lo=", escapeGAMSCL(value[1]), 
                             " ", parPrefix, "_up=", escapeGAMSCL(value[2])))
             },
             checkbox = {
               return(paste0(parPrefix, "=", input[["cb_" %+% i]]))
             },
             numericinput = {
               return(paste0(parPrefix, "=", input[["numeric_" %+% i]]))
             },
             textinput = {
               val <- input[["text_" %+% i]]
               if(!length(val) || !nchar(val))
                 val <- NA
               else 
                 val <- escapeGAMSCL(val)
               return(paste0(parPrefix, "=", val))
             },
             dt =,
             hot = {
               input[['in_' %+% i]]
               errMsg <- NULL
               tryCatch({
                 data <- getInputDataset(i)
               }, error = function(e){
                 flog.error("Dataset: '%s' could not be loaded. Error message: '%s'.", 
                            modelInAlias[i], e)
                 errMsg <<- sprintf(lang$errMsg$GAMSInput$noData, 
                                    modelInAlias[i])
               })
               if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
                 return(paste0(parPrefix, "=NA"))
               }
               staticData$push(names(modelIn)[[i]], data)
               return(paste0(parPrefix, "=", digest(data, algo = "md5")))
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
      attachmentFilePaths <- activeScen$downloadAttachmentData(workDir, allExecPerm = TRUE)
      attachmentFilePaths <- attachmentFilePaths[match(basename(attachmentFilePaths), 
                                                       sort(basename(attachmentFilePaths)))]
      if(length(attachmentFilePaths)){
        staticData$addFilePaths(attachmentFilePaths)
        gmsString <- paste(gmsString, paste(vapply(seq_along(attachmentFilePaths), function(i){
          return(paste0("--HCUBE_STATIC_", i, "=", digest(file = attachmentFilePaths[i], algo = "md5")))
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
  
  prevJobSubmitted <- Sys.time()
  
  genHcubeJobFolder <- function(fromDir, submFileDir, toDir, scenGmsPar){
    prog <- Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    
    if(config$includeParentDir){
      parentDirName <- basename(dirname(fromDir))
      scenGmsPar    <- paste0(scenGmsPar, ' idir1="', 
                              gmsFilePath(file.path("..", "..", parentDirName, basename(fromDir))),
                              '" idir2="', gmsFilePath(file.path("..", "..", parentDirName, '"')))
      fromDir <- dirname(fromDir)
    }else{
      scenGmsPar <- paste0(scenGmsPar, ' idir1="', gmsFilePath(file.path("..", "..", 
                                                             basename(fromDir))), '"')
    }
    
    writeLines(scenGmsPar, file.path(toDir, tolower(modelName) %+% ".hcube"))
    
    # Copy files that are needed to solve model
    file.copy(fromDir, toDir, recursive = TRUE)
    staticFilePath <- file.path(currentModelDir, hcubeDirName, "static")
    if(dir.exists(staticFilePath))
      file.copy(staticFilePath, toDir, recursive = TRUE)
    unlink(staticFilePath, recursive = TRUE, force = TRUE)
    file.copy(file.path(submFileDir, hcubeSubmissionFile %+% ".gms"), toDir)
    updateProgress(incAmount = 1, detail = lang$nav$dialogHcube$waitDialog$desc)
  }
  
  runHcubeJob <- function(scenGmsPar){
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
      worker$runHcube(staticData, hcubeData, 
                      sid, tags = isolate(input$newHcubeTags), 
                      attachmentFilePaths = attachmentFilePaths)
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
    
    hcubeData$subsetJobIDs(idxDiff)
    runHcubeJob(scenGmsPar[idxDiff])
  })
  
  
  output$btHcubeAll_dl <- downloadHandler(
    filename = function() {
      tolower(modelName) %+% ".zip"
    },
    content = function(file) {
      # solve all scenarios in Hypercube run
      
      workDirHcube <- file.path(tempdir(), "hcube")
      unlink(workDirHcube, recursive = TRUE, force = TRUE)
      dir.create(workDirHcube, showWarnings = FALSE, recursive = TRUE)
      homeDir <- getwd()
      setwd(workDirHcube)
      on.exit(setwd(homeDir), add = TRUE)
      on.exit(unlink(workDirHcube, recursive = TRUE, force = TRUE), add = TRUE)
      
      genHcubeJobFolder(fromDir = currentModelDir, 
                        submFileDir = file.path(homeDir, "resources"),
                        toDir = workDirHcube, scenGmsPar = scenGmsPar)
      filesToInclude <- list.files(recursive = TRUE)
      idsToExclude   <- startsWith(filesToInclude, 
                                   file.path(basename(currentModelDir), 
                                             hcubeDirName)) | 
        endsWith(filesToInclude, ".miroconf")
      filesToInclude <- filesToInclude[!idsToExclude]
      removeModal()
      zip(file, filesToInclude, compression_level = 6)
    },
    contentType = "application/zip")
  
  output$btHcubeNew_dl <- downloadHandler(
    filename = function() {
      tolower(modelName) %+% ".zip"
    },
    content = function(file) {
      # solve only scenarios that do not yet exist
      
      workDirHcube <- file.path(tempdir(), "hcube")
      unlink(workDirHcube, recursive = TRUE, force = TRUE)
      dir.create(workDirHcube, showWarnings = FALSE, recursive = TRUE)
      homeDir <- getwd()
      setwd(workDirHcube)
      on.exit(setwd(homeDir), add = TRUE)
      on.exit(unlink(workDirHcube, recursive = TRUE, force = TRUE), add = TRUE)
      
      genHcubeJobFolder(fromDir = currentModelDir, 
                        submFileDir = file.path(homeDir, "resources"),
                        toDir = workDirHcube, scenGmsPar = scenGmsPar[idxDiff])
      
      removeModal()
      zip(file, list.files(recursive = TRUE), compression_level = 6)
    },
    contentType = "application/zip")
}else{
  observeEvent(virtualActionButton(input$btSubmitJob, rv$btSubmitJob), {
    flog.debug("Submit new asynchronous job button clicked.")
    jobNameTmp <- character(1L)
    if(length(activeScen)){
      if(!activeScen$hasExecPerm()){
        showErrorMsg(lang$nav$dialogNoExecPerm$title, 
                     lang$nav$dialogNoExecPerm$desc)
        flog.info("User has no execute permission for this scenario.")
        return(NULL)
      }
      jobNameTmp <- activeScen$getScenName()
    }
    if(identical(worker$validateCredentials(), FALSE)){
      showLoginDialog(cred = worker$getCredentials(), 
                      forwardOnSuccess = "btSubmitJob")
      return(NULL)
    }
    showJobSubmissionDialog(jobNameTmp)
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
    dataModelRun <- prepareModelRun(async = TRUE)
    if(is.null(dataModelRun)){
      return(NULL)
    }
    # submit job
    tryCatch({
      hideEl(session, "#jobSubmissionWrapper")
      showEl(session, "#jobSubmissionLoad")
      worker$runAsync(dataModelRun$inputData, dataModelRun$pfFileContent, sid, 
                      name = jobName)
      showHideEl(session, "#jobSubmitSuccess", 2000)
    }, error = function(e){
      errMsg <- conditionMessage(e)
      flog.error("Some problem occurred while executing Hypercube job. Error message: '%s'.", errMsg)
      
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

logObs <- NULL
observeEvent(virtualActionButton(input$btSolve, rv$btSolve), {
  flog.debug("Solve button clicked (model: '%s').", modelName)
  
  if(length(modelStatus)){
    showErrorMsg(lang$errMsg$jobRunning$title, 
                 lang$errMsg$jobRunning$desc)
    return(NULL)
  }
  if(length(unzipModelFilesProcess)){
    if(length(unzipModelFilesProcess$get_exit_status())){
      unzipModelFilesProcess <- NULL
    }else{
      showErrorMsg(lang$errMsg$unzipProcessRunning$title, 
                   lang$errMsg$unzipProcessRunning$desc)
      return(NULL)
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
    return()
  }
  if(identical(worker$validateCredentials(), FALSE)){
    showLoginDialog(cred = worker$getCredentials(), 
                    forwardOnSuccess = "btSolve")
    return(NULL)
  }
  if(LAUNCHHCUBEMODE){
    numberScenarios <- noScenToSolve()
    if(numberScenarios > maxNoHcube){
      showModal(modalDialog(title = lang$nav$dialogHcube$exceedMaxNoDialog$title, 
                            sprintf(lang$nav$dialogHcube$exceedMaxNoDialog$desc, 
                                    numberScenarios, maxNoHcube)))
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
    
    idsSolved <- db$importDataset(scenMetadataTable, colNames = snameIdentifier, 
                                  tibble(scodeIdentifier, SCODEMAP[['scen']], ">"))
    if(length(idsSolved)){
      idsSolved <- unique(idsSolved[[1L]])
    }
    errMsg <- NULL
    prog$inc(amount = 0.5, detail = lang$progressBar$prepRun$sendInput)
    tryCatch(
      scenToSolve <- scenToSolve(),
      error = function(e){
        flog.error("Problems getting list of scenarios to solve in Hypercube mode. Error message: '%s'.", e)
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
      scenGmsPar <<- paste0(scenGmsPar, ' trace="', tableNameTracePrefix, modelName, '.trc"',
                           " traceopt=3")
    }
    
    sidsDiff <- setdiff(idsToSolve, idsSolved)
    idxDiff  <<- match(sidsDiff, idsToSolve)
    prog$close()
    showHcubeSubmitDialog(noIdsToSolve = length(idsToSolve), noIdsExist = length(idsToSolve) - length(sidsDiff))
  
    enableEl(session, "#btSolve")
    
    return(NULL)
  }
  dataModelRun <- prepareModelRun(async = FALSE)
  if(is.null(dataModelRun)){
    return(NULL)
  }
  dataTmp <- dataModelRun$dataTmp
  # run GAMS
  tryCatch({
    jobSid <- NULL
    if(length(activeScen) && length(activeScen$getSid())){
      jobSid <- activeScen$getSid()
    }
    worker$run(dataModelRun$inputData, dataModelRun$pfFileContent, jobSid)
  }, error = function(e) {
    errMsg <<- lang$errMsg$gamsExec$desc
    flog.error("GAMS did not execute successfully (model: '%s'). Error message: %s.", modelName, e)
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
    return(NULL)
  }
  if(config$activateModules$remoteExecution)
    updateTabsetPanel(session, "jobListPanel", selected = "current")
  updateTabsetPanel(session, "sidebarMenuId", selected = "gamsinter")
  updateTabsetPanel(session, "logFileTabsset", selected = "log")
  
  #activate Interrupt button as GAMS is running now
  updateActionButton(session, "btInterrupt", icon = character(0L))
  enableEl(session, "#btInterrupt")
  switchTab(session, "gamsinter")
  # read log file
  if(config$activateModules$logFile){
    tryCatch({
      logfile <- worker$getReactiveLog(session)
      logfileObs <- logfile$obs
      logfile <- logfile$re
    }, error = function(e) {
      flog.error("GAMS log file could not be read (model: '%s'). Error message: %s.", modelName, e)
      errMsg <<- lang$errMsg$readLog$desc
    })
    showErrorMsg(lang$errMsg$readLog$title, errMsg)
  }
  errMsg <- NULL
  tryCatch({
    modelStatusRE  <- worker$getReactiveStatus(session)
    modelStatusObs <- modelStatusRE$obs
    modelStatus    <<- modelStatusRE$re
  }, error = function(e) {
    flog.error("GAMS status could not be retrieved (model: '%s'). Error message: %s.", modelName, e)
    errMsg <<- lang$errMsg$readLog$desc
  })
  showErrorMsg(lang$errMsg$readLog$title, errMsg)
  
  logFilePath <- NULL
  if(config$activateModules$logFile){
    if(config$activateModules$attachments && 
       config$storeLogFilesDuration > 0L && !is.null(activeScen)){
      logFilePath <- file.path(workDir, modelName %+% ".log")
      if(!identical(unlink(logFilePath, force = TRUE), 0L)){
        flog.warn("Could not remove log file: '%s'.", logFilePath)
      }
    }
    emptyEl(session, "#logStatus")
    logObs <<- observe({
      logText    <- logfile()
      if(length(logFilePath)){
        write_file(logText, logFilePath, append = TRUE)
      }
      return(appendEl(session, "#logStatus", logText, 
                      scroll = identical(isolate(input$logUpdate), TRUE)))
    })
  }
  # reset listing file when new solve is started
  output$listFile <- renderText("")
  emptyEl(session, "#miroLogFile")
  hideEl(session, ".input-validation-error")
  # print model status
  output$modelStatus <- renderUI({
    currModelStat <- modelStatus()
    if(is.null(currModelStat)){
      statusText <- lang$nav$gamsModelStatus$exec
    }else if(identical(currModelStat, "s")){
      statusText <- lang$nav$gamsModelStatus$submission
    }else if(identical(currModelStat, "q")){
      statusText <- lang$nav$gamsModelStatus$queued
    }else if(identical(currModelStat, "d")){
      statusText <- lang$nav$gamsModelStatus$collection
    }else{
      modelStatusObs$destroy()
      modelStatus <<- NULL
      enableEl(session, "#btSolve")
      disableEl(session, "#btInterrupt")
      
      if(config$activateModules$logFile){
        logfileObs$destroy()
        logfileObs <- NULL
        logObs$destroy()
        logfile <- NULL
      }
      
      if(currModelStat < 0){
        returnCodeText <- GAMSReturnCodeMap[as.character(currModelStat)]
        if(is.na(returnCodeText)){
          returnCodeText <- as.character(currModelStat)
        }
        statusText <- lang$nav$gamsModelStatus$error %+% returnCodeText
        flog.debug("GAMS model was not solved successfully (model: '%s'). Model status: %s.", 
                   modelName, statusText)
        return(htmltools::htmlEscape(statusText))
      }
      
      if(config$activateModules$lstFile){
        errMsg <- NULL
        tryCatch({
          fileSize <- file.size(file.path(workDir, modelNameRaw %+% ".lst"))
          if(is.na(fileSize))
            stop("Could not access listing file", call. = FALSE)
          if(fileSize > maxSizeToRead){
            output$listFile <- renderText(lang$errMsg$readLst$fileSize)
          }else { 
            output$listFile <- renderText({
              if(file.exists(file.path(workDir, modelNameRaw %+% ".lst"))){
                read_file(file.path(workDir, modelNameRaw %+% ".lst"))
              }else{
                lang$errMsg$readLst$fileNotFound
              }
            })
          }
        }, error = function(e) {
          errMsg <<- lang$errMsg$readLst$desc
          flog.warn("GAMS listing file could not be read (model: '%s'). Error message: %s.", 
                    modelName, e)
        })
        showErrorMsg(lang$errMsg$readLst$title, errMsg)
      }
      if(config$activateModules$miroLogFile){
        miroLogContent <- ""
        miroLogPath <- file.path(workDir, config$miroLogFile)
        miroLogAnnotations <- ""
        tryCatch({
          if(file.exists(miroLogPath)[1]){
            inputScalarsTmp <- NULL
            if(scalarsFileName %in% names(modelIn))
              inputScalarsTmp  <- modelIn[[scalarsFileName]]$symnames
            miroLogContent     <- parseMiroLog(session, miroLogPath, 
                                               names(modelIn), inputScalarsTmp)
            miroLogAnnotations <- miroLogContent$annotations
            miroLogContent <- miroLogContent$content
          }
        }, error = function(e){
          flog.warn("MIRO log file could not be read. Error message: '%s'.", e)
        })
        output$miroLogFile <- renderUI(HTML(paste(miroLogContent, collapse = "\n")))
      }
      if(currModelStat != 0){
        returnCodeText <- GAMSReturnCodeMap[as.character(currModelStat)]
        if(is.na(returnCodeText)){
          returnCodeText <- as.character(currModelStat)
        }
        statusText <- lang$nav$gamsModelStatus$error %+% returnCodeText
        if(config$activateModules$miroLogFile && length(miroLogAnnotations)){
          session$sendCustomMessage("gms-showValidationErrors", miroLogAnnotations)
          switchTab(session, "input")
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
        
        storeGAMSOutputFiles(workDir)
        
        #select first tab in current run tabset
        switchTab(session, "output")
        updateTabsetPanel(session, "scenTabset",
                          selected = "results.current")
        errMsg <- NULL
        tryCatch({
          GAMSResults <- loadScenData(scalarsName = scalarsOutName, metaData = modelOut, workDir = workDir, 
                                      modelName = modelName, errMsg = lang$errMsg$GAMSOutput$badOutputData,
                                      scalarsFileHeaders = scalarsFileHeaders, fileName = MIROGdxOutName,
                                      templates = modelOutTemplate, method = config$fileExchange, 
                                      csvDelim = config$csvDelim, hiddenOutputScalars = config$hiddenOutputScalars)
        }, error = function(e){
          flog.error("Problems loading output data. Error message: %s.", e)
          errMsg <<- lang$errMsg$readOutput$desc
        })
        if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
          return(htmltools::htmlEscape(statusText))
        }
        if(!is.null(GAMSResults$scalar)){
          scalarData[["scen_1_"]] <<- GAMSResults$scalar
        }
        scalarIdTmp <- match(scalarsFileName, tolower(names(dataTmp)))[[1L]]
        if(!is.na(scalarIdTmp)){
          scalarData[["scen_1_"]] <<- bind_rows(dataTmp[[scalarIdTmp]], scalarData[["scen_1_"]])
        }
        if(!is.null(GAMSResults$tabular)){
          scenData[["scen_1_"]] <<- GAMSResults$tabular
        }
        if(config$saveTraceFile){
          tryCatch({
            traceData <<- readTraceData(file.path(workDir, 
                                                  paste0(tableNameTracePrefix,
                                                         modelName, ".trc")), 
                                        traceColNames)
          }, error = function(e){
            flog.info("Problems loading trace data. Error message: %s.", e)
          })
        }
        tryCatch(
          worker$updateJobStatus(JOBSTATUSMAP['imported']), 
          error = function(e){
            flog.warn("Failed to update job status. Error message: '%s'.", 
                      conditionMessage(e))
          })
        
        
        GAMSResults <- NULL
        
        renderOutputData()
        
        markUnsaved()
      }
    }
    # print model status
    return(htmltools::htmlEscape(statusText))
  })
  # refresh even when modelStatus message is hidden (i.e. user is on another tab)
  outputOptions(output, "modelStatus", suspendWhenHidden = FALSE)
})
if(config$activateModules$remoteExecution){
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
      flog.error("Problems setting credentials: %s", e)
      showErrorMsg(lang$errMsg$fileWrite$title, sprintf(lang$errMsg$fileWrite$desc,
                                                        rememberMeFileName))
    })
  })
}

