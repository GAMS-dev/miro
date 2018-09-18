# run GAMS

if(identical(config$activateModules$batchMode, TRUE)){
  idsToSolve <- NULL
  idxDiff <- NULL
  scenGmsPar <- NULL
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
    shinyjs::disable("btSolve")
    
    idsSolved <- unique(db$importDataset(scen.metadata.table, cols = sname.identifier))
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
                                         class = "btOrange")),
                          fade = TRUE, easyClose = FALSE))
    shinyjs::enable("btSolve")
    
    return(NULL)
  }
  
  updateTabsetPanel(session, "sidebar.menu", selected = "gamsinter")
  
  # save input data 
  source("./modules/input_save.R", local = TRUE)
  pfFileContent <- NULL
  lapply(seq_along(data.tmp), function(i){
    # write compile time variable file and remove compile time variables from scalar dataset
    if(identical(tolower(names(data.tmp)[[i]]), tolower(scalars.file.name))){
      # scalars file exists, so remove compile time variables from it
      DDParIdx           <- grepl(paste("^", DDPar, "(_min|_max)?$", sep = "", collapse = "|"), data.tmp[[i]][[1]])
      GMSOptIdx          <- grepl(paste("^", GMSOpt, "(_min|_max)?$", sep = "", collapse = "|"), data.tmp[[i]][[1]])
      DDParValues        <- data.tmp[[i]][DDParIdx, , drop = FALSE]
      GMSOptValues       <- data.tmp[[i]][GMSOptIdx, , drop = FALSE]
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
        csvData <- data.tmp[[i]][!any(DDParIdx, GMSOptIdx), ]
      }else{
        csvData <- data.tmp[[i]]
      }
      rm(GMSOptValues)
      rm(DDParValues)
    }else{
      csvData <- data.tmp[[i]]
    }
    
    # write csv files used to communicate with GAMS
    tryCatch({
      write_delim(csvData, workDir %+% names(data.tmp)[[i]] %+% ".csv", 
                  delim = config$csvDelim, na = "")
    }, error = function(e) {
      fileName <- paste0(names(data.tmp)[[i]], ".csv")
      flog.error("Error writing csv file: '%s' (model: '%s').", fileName, modelName)
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$writeCsv, fileName), sep = "\n")
    })
    
  })
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  # run GAMS
  tryCatch({
    homeDir <- ".." %+% .Platform$file.sep %+% ".." %+% .Platform$file.sep
    gamsArgs <- c("idir1=" %+% homeDir %+% modelDir, "idir2=" %+% homeDir %+% currentModelDir, 
                  "curdir=" %+% workDir, "logOption=3")
    if(isWindows()){
      gamsArgs <- gsub("/", "\\", gamsArgs, fixed = TRUE)
    }
    writeLines(c(gamsArgs, pfFileContent), workDir %+% tolower(modelName) %+% ".pf")
    gams <<- processx::process$new("gams", args = c(paste0(modelName, ".gms"), "pf=" %+% workDir %+% tolower(modelName) %+% ".pf", config$gamsWEBUISwitch), 
                                   stdout = workDir %+% modelName %+% ".log", windows_hide_window = TRUE)
  }, error = function(e) {
    errMsg <<- lang$errMsg$gamsExec$desc
    flog.error("GAMS did not execute successfully (model: '%s'). Error message: %s.", modelName, e)
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsExec$title, errMsg))){
    return(NULL)
  }
  
  
  #activate Interrupt button as GAMS is running now
  shinyjs::enable("btInterrupt")
  # read log file
  tryCatch({
    logfile <- reactiveFileReader(500, session, workDir %+% modelName %+% ".log", readLines, warn = FALSE)
  }, error = function(e) {
    flog.error("GAMS log file could not be read (model: '%s'). Error message: %s.", modelName, e)
    errMsg <<- lang$errMsg$readLog$desc
  })
  showErrorMsg(lang$errMsg$readLog$title, errMsg)
  
  # read status file
  modelStatus <- reactivePoll(1000, session, checkFunc = function(){
    gams$get_exit_status()
  }, valueFunc = function(){
    gams$get_exit_status()
  })
  
  # print log status
  output$logStatus <- renderText({
    # read log file 
    log.text <- paste(logfile(), collapse = "\n")
    if(input$logUpdate){
      js$scrollDown("logStatus")
    }
    return(log.text)
  })
  
  # print model status
  output$modelStatus <- renderText({
    
    statusText <- lang$nav$gamsModelStatus$exec
    # model got solved successfully
    if(!is.null(modelStatus())){
      shinyjs::enable("btSolve")
      shinyjs::disable("btInterrupt")
      # load listing file
      errMsg <- NULL
      tryCatch({
        output$listFile <- renderText(paste(readLines(paste0(workDir, modelName, ".lst"), warn = FALSE), collapse = "\n"))
      }, error = function(e) {
        errMsg <<- lang$errMsg$readLst$desc
        flog.warn("GAMS listing file could not be read (model: '%s'). Error message: %s.", modelName, e)
        # do not interrupt here
      })
      showErrorMsg(lang$errMsg$readLst$title, errMsg)
      
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
        updateTabsetPanel(session, "sidebar.menu",
                          selected = "outputData")
        updateTabsetPanel(session, "scenTabset",
                          selected = "results.current")
        updateTabsetPanel(session, "content.current",
                          selected = "content.current_1")
        errMsg <- NULL
        tryCatch({
          GAMSResults <- loadGAMSResults(scalarsOutName = scalars.out.name, modelOut = modelOut, workDir = workDir, 
                                         modelName = modelName, method = "csv", csvDelim = config$csvDelim, hiddenMarker = config$gamsMetaDelim) 
        }, error = function(e){
          flog.error("Problems loading output data. Error message: %s.", e)
          errMsg <<- lang$errMsg$readOutput$desc
        })
        if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
          return(NULL)
        }
        if(!is.null(GAMSResults$scalar)){
          scalarData[["scen_1_"]] <<- GAMSResults$scalar
        }
        if(!is.null(GAMSResults$tabular)){
          scenData[["scen_1_"]] <<- GAMSResults$tabular
        }
        GAMSResults <- NULL
        # rendering tables and graphs
        renderOutputData()
        
        # enable download button for saving scenario to Excel file
        shinyjs::enable("export_1")
        
        # mark scenario as unsaved
        markUnsaved()
      }
    }
    # print model status
    return(statusText)
  })
  
})