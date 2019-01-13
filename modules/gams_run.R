# run GAMS

if(identical(config$activateModules$batchMode, TRUE)){
  idsToSolve <- NULL
  idxDiff <- NULL
  scenGmsPar <- NULL
  noScenToSolve <- reactive({
    numberScenPerElement <- vapply(seq_along(modelIn), function(i){
      switch(modelIn[[i]]$type,
             slider = {
               value <- input[["slider_" %+% i]]
               if(length(value) > 1){
                 if(identical(modelIn[[i]]$slider$double, TRUE)
                    && !identical(input[["batchMode_" %+% i]], TRUE)){
                   # double slider in non batch mode
                   return(1L)
                 }
                 
                 stepSize <- input[["batchStep_" %+% i]]
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
             dropdown = {
               return(length(input[["dropdown_" %+% i]]))
             },
             date = {
               return(length(input[["date_" %+% i]]))
             },
             daterange = {
               return(length(input[["daterange_" %+% i]]))
             },
             checkbox = {
               return(length(input[["cb_" %+% i]]))
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
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogBatch$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    elementValues <- lapply(seq_along(modelIn), function(i){
      updateProgress(incAmount = 1/(length(modelIn) + 18), detail = lang$nav$dialogBatch$waitDialog$desc)
      switch(modelIn[[i]]$type,
             slider = {
               value <- input[["slider_" %+% i]]
               if(length(value) > 1){
                 if(identical(modelIn[[i]]$slider$double, TRUE)
                    && !identical(input[["batchMode_" %+% i]], TRUE)){
                   # double slider in non batch mode
                   return(paste0("--", names(modelIn)[[i]], "_lo=", value[1], 
                                 " --", names(modelIn)[[i]], "_up=", value[2]))
                 }
                 
                 stepSize <- input[["batchStep_" %+% i]]
                 if(identical(modelIn[[i]]$slider$single, TRUE)){
                   return(seq(value[1], value[2], stepSize))
                 }
                 # double slider all combinations
                 value <- getCombinationsSlider(value[1], value[2], stepSize)
                 return(paste0("--", names(modelIn)[[i]], "_lo=", value$min, 
                               " --", names(modelIn)[[i]], "_up=", value$max))
               }
             },
             dropdown = {
               if(names(modelIn)[i] %in% DDPar){
                 parPrefix <- "--"
               }else if(names(modelIn)[i] %in% GMSOpt){
                 parPrefix <- ""
               }else{
                 parPrefix <- "-+"
               }
               value <- input[["dropdown_" %+% i]]
               if("_" %in% value){
                 value <- value[value != "_"]
                 return(c(if(length(value)) paste0(parPrefix, names(modelIn)[[i]], "=", value), ""))
               }
               return(paste0(parPrefix, names(modelIn)[[i]], "=", value))
             },
             date = {
               value <- input[["date_" %+% i]]
             },
             checkbox = {
               value <- input[["cb_" %+% i]]
             },
             hot = {
               value <- NA
             },
             {
               stop(sprintf("Widget type: '%s' not supported", modelIn[[i]]$type), call. = FALSE)
             })
    })
    par <- modelInGmsString[!is.na(elementValues)]
    elementValues <- elementValues[!is.na(elementValues)]
    gmsString <- genGmsString(par = par, val = elementValues, modelName = modelName)
    updateProgress(incAmount = 15/(length(modelIn) + 18), detail = lang$nav$dialogBatch$waitDialog$desc)
    scenIds <- as.character(sha256(gmsString))
    updateProgress(incAmount = 3/(length(modelIn) + 18), detail = lang$nav$dialogBatch$waitDialog$desc)
    gmsString <- scenIds %+% ": " %+% gmsString 
    if(config$saveTraceFile){
      gmsString <- paste0(gmsString, " trace=", tableNameTracePrefix, modelName, ".trc", " traceopt=3 lo=3 idir1=..", 
                          .Platform$file.sep, "..", .Platform$file.sep, ".. ", config$gamsWEBUISwitch)
    }
    return(list(ids = scenIds, gmspar = gmsString))
  })
  
  prevJobSubmitted <- Sys.time()
  
  genBatchJobFolder <- function(fromDir, modelDir, toDir, scenGmsPar){
    prog <- Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogBatch$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    writeLines(scenGmsPar, file.path(toDir, tolower(modelName) %+% ".gmsb"))
    
    # Copy files that are needed to solve model
    file.copy(fromDir, toDir, recursive = TRUE)
    file.copy(file.path(modelDir %+% hypercubeSubmissionFile), toDir)
    do.call(file.remove, list(list.files(toDir, pattern = "\\.gmsconf$", full.names = TRUE, recursive = TRUE)))
    updateProgress(incAmount = 1, detail = lang$nav$dialogBatch$waitDialog$desc)
  }
  executeBatchJob <- function(scenGmsPar){
    bid <- as.integer(db$writeMetaBatch(batchTags = isolate(input$newBatchTags)))
    flog.trace("Metadata for batch job was written to database. Batch ID: '%d' was assigned to job.", bid)
    batchDir <- file.path(currentModelDir, batchDirName, bid)
    if(dir.exists(batchDir)){
      flog.error("Batch directory: '%s' already exists.", batchDir)
      stop(sprintf("Batch directory: '%s' already exists.", batchDir), call. = FALSE)
    }
    dir.create(batchDir, recursive = TRUE)
    writeLines(scenGmsPar, file.path(batchDir, tolower(modelName) %+% ".gmsb"))
    
    flog.trace("New folder for batch job was created: '%s'.", batchDir)
    # create daemon to execute batch job
    batchSubmDir <- file.path(getwd(), modelDir, batchSubmissionFile)
    curDir <- batchDir
    if(isWindows()){
      batchSubmDir <- gsub("/", "\\", batchSubmDir, fixed = TRUE)
      curdir <- gsub("/", "\\", curDir, fixed = TRUE)
    }
    p <- process$new(gamsSysDir %+% "gams", 
                     args = c(batchSubmDir, "curdir=" %+% curdir, "lo=2", "--exec=true"),  
                     cleanup = TRUE, cleanup_tree = FALSE, supervise = FALSE,
                     windows_hide_window = TRUE)
    pid <- p$get_pid()
    flog.trace("Batch job submitted successfuly. Batch job process ID: '%d'.", pid)
    db$updateHypercubeJob(bid, pid = pid)
    flog.trace("Process ID: '%d' added to batch job ID: '%d'.", pid, bid)
  }
  observeEvent(input$btBatchAll, {
    flog.trace("Button to schedule all scenarios for batch submission was clicked.")
    now <- Sys.time()
    if(now - prevJobSubmitted < 5L){
      showHideEl(session, "#batchSubmitWait", 6000)
      flog.info("Batch submit button was clicked too quickly in a row. Please wait some seconds before submitting a new job.")
      return()
    }
    prevJobSubmitted <<- Sys.time()
    tryCatch({
      executeBatchJob(scenGmsPar)
      showHideEl(session, "#batchSubmitSuccess", 3000)
      hideModal(session, 3L)
    }, error = function(e){
      flog.error("Some problem occurred while executing batch job. Error message: '%s'.", e)
      showHideEl(session, "#batchSubmitUnknownError", 6000)
    })
    
  })
  
  observeEvent(input$btBatchNew, {
    flog.trace("Button to schedule only new scenarios for batch submission was clicked.")
    now <- Sys.time()
    if(now - prevJobSubmitted < 5L){
      showHideEl(session, "#batchSubmitWait", 6000)
      flog.info("Batch submit button was clicked too quickly in a row. Please wait some seconds before submitting a new job.")
      return()
    }
    prevJobSubmitted <<- Sys.time()
    tryCatch({
      executeBatchJob(scenGmsPar[idxDiff])
      showHideEl(session, "#batchSubmitSuccess", 3000)
      hideModal(session, 3L)
    }, error = function(e){
      flog.error("Some problem occurred while executing batch job. Error message: '%s'.", e)
      showHideEl(session, "#batchSubmitUnknownError", 6000)
    })
  })
  
  if(file.exists("./modules/gams_run_epigrids.R"))
    source("./modules/gams_run_epigrids.R", local = TRUE)
}


observeEvent(input$btSolve, {
  flog.debug("Solve button clicked (model: '%s').", modelName)
  removeModal()
  if(identical(config$activateModules$batchMode, TRUE)){
    numberScenarios <- noScenToSolve()
    if(numberScenarios > maxNoBatch){
      showModal(modalDialog(title = lang$nav$dialogBatch$exceedMaxNoDialog$title, 
                            sprintf(lang$nav$dialogBatch$exceedMaxNoDialog$desc, 
                                    numberScenarios, maxNoBatch)))
      return(NULL)
    }else if(numberScenarios == -1){
      showModal(modalDialog(title = lang$nav$dialogBatch$badStepSizeDialog$title, 
                            lang$nav$dialogBatch$badStepSizeDialog$desc))
      return(NULL)
    }else if(numberScenarios == 0){
      showModal(modalDialog(title = lang$nav$dialogBatch$noScenSelectedDialog$title, 
                            lang$nav$dialogBatch$noScenSelectedDialog$desc))
      return(NULL)
    }
    disableEl(session, "#btSolve")
    idsSolved <- db$importDataset(scenMetadataTable, colNames = snameIdentifier)
    if(length(idsSolved)){
      idsSolved <- unique(idsSolved[[1L]])
    }
    scenToSolve <- scenToSolve()
    idsToSolve <<- scenToSolve$ids
    scenGmsPar <<- scenToSolve$gmspar
    
    sidsDiff <- setdiff(idsToSolve, idsSolved)
    idxDiff  <<- match(sidsDiff, idsToSolve)
    showBatchSubmitDialog(noIdsToSolve = length(idsToSolve), noIdsExist = length(idsToSolve) - length(sidsDiff))
  
    enableEl(session, "#btSolve")
    
    return(NULL)
  }
  prog <- shiny::Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$prepRun$title, value = 0)
  
  updateTabsetPanel(session, "sidebarMenuId", selected = "gamsinter")
  
  prog$inc(amount = 0.5, detail = lang$progressBar$prepRun$sendInput)
  # save input data 
  source("./modules/input_save.R", local = TRUE)
  pfFileContent <- NULL
  lapply(seq_along(dataTmp), function(i){
    # write compile time variable file and remove compile time variables from scalar dataset
    if(identical(tolower(names(dataTmp)[[i]]), tolower(scalarsFileName))){
      # scalars file exists, so remove compile time variables from it
      DDParIdx           <- grepl(paste("^", DDPar, "(_lo|_up)?$", sep = "", collapse = "|"), dataTmp[[i]][[1]])
      GMSOptIdx          <- grepl(paste("^", GMSOpt, "(_lo|_up)?$", sep = "", collapse = "|"), dataTmp[[i]][[1]])
      DDParValues        <- dataTmp[[i]][DDParIdx, , drop = FALSE]
      GMSOptValues       <- dataTmp[[i]][GMSOptIdx, , drop = FALSE]
      if(nrow(DDParValues) || nrow(GMSOptValues)){
        pfFileContent <<- '--' %+% DDParValues[[1]] %+% '="' %+% DDParValues[[3]] %+% '"'
        # do not write '_' in pf file (no selection)
        pfGMSOpt      <- vapply(seq_along(GMSOptValues[[1]]), 
                                function(i){
                                  if(!identical(GMSOptValues[[3]][i], "_")) 
                                    GMSOptValues[[1]][i] %+% '="' %+% GMSOptValues[[3]][i] %+% '"'
                                  else
                                    NA_character_
                                }, character(1L), USE.NAMES = FALSE)
        pfGMSOpt      <- pfGMSOpt[!is.na(pfGMSOpt)]
        pfFileContent <<- c(pfFileContent, pfGMSOpt)
        # remove those rows from scalars file that are compile time variables
        csvData <- dataTmp[[i]][!(DDParIdx | GMSOptIdx), ]
      }else{
        csvData <- dataTmp[[i]]
      }
      rm(GMSOptValues)
      rm(DDParValues)
    }else{
      csvData <- dataTmp[[i]]
    }
    
    # write csv files used to communicate with GAMS
    tryCatch({
      write_delim(csvData, workDir %+% names(dataTmp)[[i]] %+% ".csv", 
                  delim = config$csvDelim, na = "")
    }, error = function(e) {
      fileName <- paste0(names(dataTmp)[[i]], ".csv")
      flog.error("Error writing csv file: '%s' (model: '%s').", fileName, modelName)
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$writeCsv, fileName), sep = "\n")
    })
    
  })
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  # run GAMS
  tryCatch({
    gamsArgs <- c("idir1=" %+% currentModelDir, paste0("idir2=", currentModelDir, "..", .Platform$file.sep), 
                  "curdir=" %+% workDir, "logOption=3", "execMode=" %+% gamsExecMode, config$gamsWEBUISwitch)
    if(config$saveTraceFile){
      gamsArgs <- c(gamsArgs, "trace=" %+% tableNameTracePrefix %+% modelName %+% ".trc", "traceopt=3")
    }
    pfFilePath <- workDir %+% tolower(modelName) %+% ".pf"
    if(isWindows()){
      gamsArgs <- gsub("/", "\\", gamsArgs, fixed = TRUE)
      pfFilePath <- gsub("/", "\\", pfFilePath, fixed = TRUE)
    }
    writeLines(c(pfFileContent, gamsArgs), pfFilePath)
    
    if(config$activateModules$attachments && attachAllowExec && !is.null(activeScen)){
      prog$inc(amount = 0, detail = lang$progressBar$prepRun$downloadAttach)
      activeScen$downloadAttachmentData(workDir, allExecPerm = TRUE)
    }
    prog$close()
    gams <<- process$new(gamsSysDir %+% "gams", args = c(modelGmsName, 
                                                         "pf=" %+% pfFilePath), 
                         stdout = workDir %+% modelName %+% ".log", windows_hide_window = TRUE)
  }, error = function(e) {
    errMsg <<- lang$errMsg$gamsExec$desc
    flog.error("GAMS did not execute successfully (model: '%s'). Error message: %s.", modelName, e)
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
    return(NULL)
  }
  
  #activate Interrupt button as GAMS is running now
  enableEl(session, "#btInterrupt")
  switchTab(session, "gamsinter")
  # read log file
  if(config$activateModules$logFile){
    tryCatch({
      logfile <- reactiveFileReader2(300, session, workDir %+% modelName %+% ".log", readLines, warn = FALSE)
      logfileObs <- logfile$obs
      logfile <- logfile$re
    }, error = function(e) {
      flog.error("GAMS log file could not be read (model: '%s'). Error message: %s.", modelName, e)
      errMsg <<- lang$errMsg$readLog$desc
    })
    showErrorMsg(lang$errMsg$readLog$title, errMsg)
  }
  
  modelStatus <- reactivePoll2(1000, session, checkFunc = function(){
    gams$get_exit_status()
  }, valueFunc = function(){
    gams$get_exit_status()
  })
  modelStatusObs <- modelStatus$obs
  modelStatus <- modelStatus$re
  
  if(config$activateModules$logFile){
    output$logStatus <- renderText({
      # read log file 
      logText <- paste(logfile(), collapse = "\n")
      if(!is.null(modelStatus())){
        return(logText)
      }
      if(input$logUpdate){
        scrollDown(session, "#logStatus")
      }
      return(logText)
    })
  }
  
  # print model status
  output$modelStatus <- renderText({
    
    statusText <- lang$nav$gamsModelStatus$exec
    # model got solved successfully
    if(!is.null(modelStatus())){
      modelStatusObs$destroy()
      modelStatus <- NULL
      enableEl(session, "#btSolve")
      disableEl(session, "#btInterrupt")
      
      if(config$activateModules$logFile){
        logfileObs$destroy()
        logfile <- NULL
      }
      if(config$activateModules$lstFile){
        errMsg <- NULL
        tryCatch({
          if(getNoLinesInFile(workDir %+% modelName %+% ".lst") > lstMaxNoLinesToRead){
            output$listFile <- renderText(lang$errMsg$readLst$fileSize)
          }else{
            output$listFile <- renderText(read_file(workDir %+% modelName %+% ".lst"))
          }
        }, error = function(e) {
          errMsg <<- lang$errMsg$readLst$desc
          flog.warn("GAMS listing file could not be read (model: '%s'). Error message: %s.", 
                    modelName, e)
        })
        showErrorMsg(lang$errMsg$readLst$title, errMsg)
      }
      
      if(modelStatus() != 0){
        returnCodeText <- GAMSReturnCodeMap[as.character(modelStatus())]
        if(is.na(returnCodeText)){
          returnCodeText <- as.character(modelStatus())
        }
        statusText <- lang$nav$gamsModelStatus$error %+% returnCodeText
        flog.debug("GAMS model was not solved successfully (model: '%s'). Model status: %s.", modelName, statusText)
      }else{
        # run terminated successfully
        
        #select first tab in current run tabset
        statusText <- lang$nav$gamsModelStatus$success
        updateTabsetPanel(session, "sidebarMenuId",
                          selected = "outputData")
        updateTabsetPanel(session, "scenTabset",
                          selected = "results.current")
        updateTabsetPanel(session, "contentCurrent",
                          selected = "contentCurrent_1")
        errMsg <- NULL
        tryCatch({
          GAMSResults <- loadGAMSResults(scalarsOutName = scalarsOutName, modelOut = modelOut, workDir = workDir, 
                                         modelName = modelName, errMsg = lang$errMsg$GAMSOutput,
                                         scalarsFileHeaders = scalarsFileHeaders,
                                         method = "csv", csvDelim = config$csvDelim, 
                                         hiddenMarker = config$gamsMetaDelim, strictmode = config$activateModules$strictmode) 
        }, error = function(e){
          flog.error("Problems loading output data. Error message: %s.", e)
          errMsg <<- lang$errMsg$readOutput$desc
        })
        if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
          return()
        }
        if(!is.null(GAMSResults$scalar)){
          scalarData[["scen_1_"]] <<- GAMSResults$scalar
        }
        if(!is.null(GAMSResults$tabular)){
          scenData[["scen_1_"]] <<- GAMSResults$tabular
        }
        if(config$saveTraceFile){
          tryCatch({
            traceData <<- readTraceData(workDir %+% tableNameTracePrefix %+% modelName %+%".trc", 
                                        traceColNames)
          }, error = function(e){
            flog.error("Problems loading trace data. Error message: %s.", e)
            errMsg <<- lang$errMsg$readOutput$desc
          })
          if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
            return()
          }
        }
        
        GAMSResults <- NULL
        # rendering tables and graphs
        renderOutputData()
        
        # enable download button for saving scenario to Excel file
        enableEl(session, "#export_1")
        
        # show load button after solve
        switchTab(session, "output")
        
        # mark scenario as unsaved
        markUnsaved()
      }
    }
    # print model status
    return(statusText)
  })
  # refresh even when modelStatus message is hidden (i.e. user is on another tab)
  outputOptions(output, "modelStatus", suspendWhenHidden = FALSE)
})