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
                   return(paste0("--", names(modelIn)[[i]], "_min=", value[1], 
                                 " --", names(modelIn)[[i]], "_max=", value[2]))
                 }
                 
                 stepSize <- input[["batchStep_" %+% i]]
                 if(identical(modelIn[[i]]$slider$single, TRUE)){
                   return(seq(value[1], value[2], stepSize))
                 }
                 # double slider all combinations
                 value <- getCombinationsSlider(value[1], value[2], stepSize)
                 return(paste0("--", names(modelIn)[[i]], "_min=", value$min, 
                               " --", names(modelIn)[[i]], "_max=", value$max))
               }
             },
             dropdown = {
               value <- input[["dropdown_" %+% i]]
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
    return(list(ids = scenIds, gmspar = gmsString))
  })
  observeEvent(input$btBatchAll, {
    # solve all scenarios in batch run
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogBatch$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    
    # BEGIN EPIGRIDS specific
    tryCatch({
      writeLines(scenGmsPar, workDir %+% tolower(modelName) %+% ".gmsb")
      updateProgress(incAmount = 1, detail = lang$nav$dialogBatch$waitDialog$desc)
    }, error = function(e) {
      errMsg <<- lang$errMsg$gamsExec$desc
      flog.error("GAMS batch file was not written successfully. Error message: %s.", e)
    })
    if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
      return(NULL)
    }
    # END EPIGRIDS specific
    
    showModal(modalDialog(title = lang$nav$dialogBatch$successDialog$title, 
                          lang$nav$dialogBatch$successDialog$desc))
  })
  observeEvent(input$btBatchNew, {
    # solve only scenarios that do not yet exist
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogBatch$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    
    # BEGIN EPIGRIDS specific
    tryCatch({
      writeLines(scenGmsPar[idxDiff], workDir %+% tolower(modelName) %+% ".gmsb")
      updateProgress(incAmount = 1, detail = lang$nav$dialogBatch$waitDialog$desc)
    }, error = function(e) {
      errMsg <<- lang$errMsg$gamsExec$desc
      flog.error("GAMS batch file was not written successfully. Error message: %s.", e)
    })
    if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
      return(NULL)
    }
    # END EPIGRIDS specific
    
    showModal(modalDialog(title = lang$nav$dialogBatch$successDialog$title, 
                          lang$nav$dialogBatch$successDialog$desc))
  })
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
    
    idsSolved <- unique(db$importDataset(scenMetadataTable, colNames = snameIdentifier))
    scenToSolve <- scenToSolve()
    idsToSolve <<- scenToSolve$ids
    scenGmsPar <<- scenToSolve$gmspar
    
    sidsDiff <- setdiff(idsToSolve, idsSolved)
    idxDiff  <<- match(sidsDiff, idsToSolve)
    showModal(modalDialog(sprintf(lang$nav$dialogBatch$desc, length(idsToSolve), 
                                  length(idsToSolve) - length(sidsDiff)), 
                          title = lang$nav$dialogBatch$title,
                          footer = tagList(
                            modalButton(lang$nav$dialogBatch$cancelButton),
                            actionButton("btBatchAll", label = lang$nav$dialogBatch$processAllButton),
                            actionButton("btBatchNew", label = lang$nav$dialogBatch$processUnsolvedButton, 
                                         class = "btHighlight1")),
                          fade = TRUE, easyClose = FALSE))
    enableEl(session, "#btSolve")
    
    return(NULL)
  }
  
  updateTabsetPanel(session, "sidebarMenuId", selected = "gamsinter")
  
  # save input data 
  source("./modules/input_save.R", local = TRUE)
  pfFileContent <- NULL
  lapply(seq_along(dataTmp), function(i){
    # write compile time variable file and remove compile time variables from scalar dataset
    if(identical(tolower(names(dataTmp)[[i]]), tolower(scalarsFileName))){
      # scalars file exists, so remove compile time variables from it
      DDParIdx           <- grepl(paste("^", DDPar, "(_min|_max)?$", sep = "", collapse = "|"), dataTmp[[i]][[1]])
      GMSOptIdx          <- grepl(paste("^", GMSOpt, "(_min|_max)?$", sep = "", collapse = "|"), dataTmp[[i]][[1]])
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
    homeDir <- getwd()
    gamsArgs <- c(paste0("idir1=", homeDir, .Platform$file.sep, modelDir), "idir2=" %+% currentModelDir, 
                  "curdir=" %+% workDir, "logOption=3")
    if(config$saveTraceFile){
      gamsArgs <- c(gamsArgs, "trace=" %+% tableNameTracePrefix %+% ".trc", "traceopt=3")
    }
    pfFilePath <- workDir %+% tolower(modelName) %+% ".pf"
    if(isWindows()){
      gamsArgs <- gsub("/", "\\", gamsArgs, fixed = TRUE)
      pfFilePath <- gsub("/", "\\", pfFilePath, fixed = TRUE)
    }
    writeLines(c(gamsArgs, pfFileContent), paste0(workDir, tolower(modelName), ".pf"))
    gams <<- process$new(gamsSysDir %+% "gams", args = c(modelGmsName, 
                                                         "pf=" %+% pfFilePath, 
                                                         config$gamsWEBUISwitch), 
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
      logfile <- reactiveFileReader(500, session, workDir %+% modelName %+% ".log", readLines, warn = FALSE)
    }, error = function(e) {
      flog.error("GAMS log file could not be read (model: '%s'). Error message: %s.", modelName, e)
      errMsg <<- lang$errMsg$readLog$desc
    })
    showErrorMsg(lang$errMsg$readLog$title, errMsg)
  }
  
  modelStatus <- reactivePoll(1000, session, checkFunc = function(){
    gams$get_exit_status()
  }, valueFunc = function(){
    gams$get_exit_status()
  })
  
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
      enableEl(session, "#btSolve")
      disableEl(session, "#btInterrupt")
      
      if(config$activateModules$lstFile){
        errMsg <- NULL
        tryCatch({
          output$listFile <- renderText(paste(readLines(workDir %+% modelName %+% ".lst", 
                                                        warn = FALSE), collapse = "\n"))
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
                                         modelName = modelName, method = "csv", csvDelim = config$csvDelim, 
                                         hiddenMarker = config$gamsMetaDelim) 
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
            traceData <<- readTraceData(workDir %+% tableNameTracePrefix %+% ".trc", 
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
  
})