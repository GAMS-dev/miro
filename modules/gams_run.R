# run GAMS

if(identical(config$activateModules$hcubeMode, TRUE)){
  idsToSolve <- NULL
  idxDiff <- NULL
  scenGmsPar <- NULL
  getHcubeStaticCSVMd5 <- function(id, data, hcubeStaticFilePath){
    filePath <- file.path(hcubeStaticFilePath, tolower(names(modelIn)[id]) %+% ".csv")
    if(!inherits(data, "data.frame"))
      return(NA)
    write_csv(data, filePath, na = "", append = FALSE, col_names = TRUE, 
              quote_escape = "double")
    flog.debug("Static file: '%s' was written to:'%s'.", names(modelIn)[id], filePath)
    return(getHcubeStaticElMd5(filePath))
  }
  getHcubeStaticElMd5 <- function(filePath){
    con <- file(filePath, open = "r")
    on.exit(close(con), add = TRUE)
    return(as.character(openssl::md5(con)))
  }
  getHcubeParPrefix <- function(id){
    if(names(modelIn)[id] %in% GMSOpt){
      return("")
    }else if(names(modelIn)[id] %in% DDPar){
      return("--")
    }else{
      return("--HCUBE_SCALAR_")
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
    hcubeStaticFilePath <- file.path(currentModelDir, hcubeDirName, "static")
    if(dir.exists(hcubeStaticFilePath) &&
       unlink(hcubeStaticFilePath, recursive = TRUE, force = TRUE) == 1L){
      stop("Problems removing existing directory for static Hypercube job files.", 
           call. = FALSE)
    }
    if(!dir.create(hcubeStaticFilePath, showWarnings = TRUE, recursive = TRUE)[1]){
      stop("Problems creating directory for static Hypercube job files.", 
           call. = FALSE)
    }
    elementValues <- lapply(seq_along(modelIn), function(i){
      updateProgress(incAmount = 1/(length(modelIn) + 18), detail = lang$nav$dialogHcube$waitDialog$desc)
      switch(modelIn[[i]]$type,
             slider = {
               value <- input[["slider_" %+% i]]
               if(length(value) > 1){
                 parPrefix <- getHcubeParPrefix(i)
                   
                 if(identical(modelIn[[i]]$slider$double, TRUE)
                    && !identical(input[["hcubeMode_" %+% i]], TRUE)){
                   # double slider in single run mode
                   return(paste0(parPrefix, names(modelIn)[[i]], "_lo=", value[1], 
                                 " ", parPrefix, names(modelIn)[[i]], "_up=", value[2]))
                 }
                 
                 stepSize <- input[["hcubeStep_" %+% i]]
                 if(identical(modelIn[[i]]$slider$single, TRUE)){
                   return(seq(value[1], value[2], stepSize))
                 }
                 # double slider all combinations
                 value <- getCombinationsSlider(value[1], value[2], stepSize)
                 return(paste0(parPrefix, names(modelIn)[[i]], "_lo=", value$min, 
                               " ", parPrefix, names(modelIn)[[i]], "_up=", value$max))
               }
             },
             dropdown = {
               if(identical(modelIn[[i]]$dropdown$single, FALSE)){
                 data <- tibble(input[["dropdown_" %+% i]])
                 names(data) <- tolower(names(modelIn))[i]
                 return(getHcubeStaticCSVMd5(i, data, hcubeStaticFilePath))
               }
               parPrefix <- getHcubeParPrefix(i)
               value <- input[["dropdown_" %+% i]]
               if("_" %in% value){
                 value <- value[value != "_"]
                 return(c(if(length(value)) paste0(parPrefix, names(modelIn)[[i]], "=", escapeGAMSCL(value)), ""))
               }
               return(paste0(parPrefix, names(modelIn)[[i]], "=", escapeGAMSCL(value)))
             },
             date = {
               return(input[["date_" %+% i]])
             },
             daterange = {
               value <- as.character(input[["daterange_" %+% i]])
               parPrefix <- getHcubeParPrefix(i)
               
               return(paste0(parPrefix, names(modelIn)[[i]], "_lo=", escapeGAMSCL(value[1]), 
                             " ", parPrefix, names(modelIn)[[i]], "_up=", escapeGAMSCL(value[2])))
             },
             checkbox = {
               return(input[["cb_" %+% i]])
             },
             textinput = {
               val <- input[["text_" %+% i]]
               if(!length(val) || !nchar(val))
                 val <- NA
               else 
                 val <- escapeGAMSCL(val)
               return(val)
             },
             dt =,
             hot = {
               input[['in_' %+% i]]
               data <- getInputDataset(i)
               return(getHcubeStaticCSVMd5(i, data, hcubeStaticFilePath))
             },
             {
               stop(sprintf("Widget type: '%s' not supported", modelIn[[i]]$type), call. = FALSE)
             })
    })
    par <- modelInGmsString[!is.na(elementValues)]
    elementValues <- elementValues[!is.na(elementValues)]
    gmsString <- genGmsString(par = par, val = elementValues, modelName = modelName)
    if(config$activateModules$attachments && attachAllowExec && !is.null(activeScen)){
      filePaths     <- activeScen$downloadAttachmentData(hcubeStaticFilePath, 
                                                         allExecPerm = TRUE)
      filePaths     <- filePaths[match(basename(filePaths), sort(basename(filePaths)))]
      if(length(filePaths)){
        gmsString <- paste(gmsString, paste(vapply(seq_along(filePaths), function(i){
          return(paste0("--HCUBE_STATIC_", i, "=", getHcubeStaticElMd5(filePaths[i])))
        }, character(1L), USE.NAMES = FALSE), collapse = " "))
      }
    }
    updateProgress(incAmount = 15/(length(modelIn) + 18), detail = lang$nav$dialogHcube$waitDialog$desc)
    scenIds <- as.character(sha256(gmsString))
    updateProgress(incAmount = 3/(length(modelIn) + 18), detail = lang$nav$dialogHcube$waitDialog$desc)
    gmsString <- paste0(scenIds, ": ", gmsString)
    
    return(list(ids = scenIds, gmspar = gmsString))
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
  executeHcubeJob <- function(scenGmsPar){
    jID <- as.integer(db$writeMetaHcube(hcubeTags = isolate(input$newHcubeTags)))
    flog.trace("Metadata for Hypercube job was written to database. Hypercube job ID: '%d' was assigned to job.", jID)
    hcubeDir <- file.path(currentModelDir, hcubeDirName, jID)
    if(dir.exists(hcubeDir)){
      stop(sprintf("Hypercube job directory: '%s' already exists.", hcubeDir), call. = FALSE)
    }
    if(!dir.create(hcubeDir, recursive = TRUE, showWarnings = FALSE)){
      stop(sprintf("Problems creating Hypercube job directory: '%s'.", hcubeDir), call. = FALSE)
    }
    staticFiles <- list.files(file.path(currentModelDir, hcubeDirName, "static"), 
                              full.names = TRUE)
    if(length(staticFiles)){
      local({
        staticDir <- file.path(hcubeDir, "static")
        if(!dir.create(staticDir) || !all(file.copy(staticFiles, staticDir))){
          stop(sprintf("Problems copying static Hypercube job files from: '%s' to: '%s'.", 
                       file.path(currentModelDir, hcubeDirName, "static"), 
                       file.path(hcubeDir, "static")), call. = FALSE)
        }
      })
    }
    
    if(config$includeParentDir){
      scenGmsPar <- paste0(scenGmsPar, ' idir1="', gmsFilePath(currentModelDir),
                           '" idir2="', gmsFilePath(dirname(currentModelDir)), '"')
    }else{
      scenGmsPar <- paste0(scenGmsPar, ' idir1="', gmsFilePath(currentModelDir), '"')
    }
    writeLines(scenGmsPar, file.path(hcubeDir, tolower(modelName) %+% ".hcube"))
    
    flog.trace("New folder for Hypercube job was created: '%s'.", hcubeDir)
    # create daemon to execute Hypercube job
    hcubeSubmDir <- gmsFilePath(file.path(getwd(), "resources", hcubeSubmissionFile %+% ".gms"))
    curdir       <- gmsFilePath(hcubeDir)
    
    tryCatch({
      writeChar(paste0("Job ID: ", jID, "\n"), file.path(hcubeDir, jID %+% ".log"), 
                eos = NULL)
    }, error = function(e){
      flog.warn("Log file: '%s' could not be written. Check whether you have sufficient permissions to write files to: '%s'.",
                 jID %+% ".log", hcubeDir)
    })
    p <- process$new(gamsSysDir %+% "gams", 
                     args = c(hcubeSubmDir, 'curdir', curdir, "lo=3",
                              "--jobID=" %+% jID),
                     cleanup = FALSE, cleanup_tree = FALSE, supervise = FALSE,
                     windows_hide_window = TRUE)
    pid <- p$get_pid()
    p <- NULL
    flog.trace("Hypercube job submitted successfuly. Hypercube job process ID: '%d'.", pid)
    db$updateHypercubeJob(jID, pid = pid)
    flog.trace("Process ID: '%d' added to Hypercube job ID: '%d'.", pid, jID)
    rv$refreshActiveJobs <- rv$refreshActiveJobs + 1L
  }
  observeEvent(input$btHcubeAll, {
    flog.trace("Button to schedule all scenarios for Hypercube submission was clicked.")
    now <- Sys.time()
    if(difftime(now, prevJobSubmitted, units = "secs") < 5L){
      showHideEl(session, "#hcubeSubmitWait", 6000)
      flog.info("Hypercube job submit button was clicked too quickly in a row. Please wait some seconds before submitting a new job.")
      return()
    }
    prevJobSubmitted <<- Sys.time()
    if(!length(scenGmsPar)){
      flog.debug("No scenarios selected to be solved in Hypercube mode.")
      return()
    }
    tryCatch({
      executeHcubeJob(scenGmsPar)
      showHideEl(session, "#hcubeSubmitSuccess", 3000)
      hideModal(session, 3L)
    }, error = function(e){
      flog.error("Some problem occurred while executing Hypercube job. Error message: '%s'.", e)
      showHideEl(session, "#hcubeSubmitUnknownError", 6000)
    })
    
  })
  
  observeEvent(input$btHcubeNew, {
    flog.trace("Button to schedule only new scenarios for Hypercube submission was clicked.")
    now <- Sys.time()
    if(difftime(now, prevJobSubmitted, units = "secs") < 5L){
      showHideEl(session, "#hcubeSubmitWait", 6000)
      flog.info("Hypercube job submit button was clicked too quickly in a row. Please wait some seconds before submitting a new job.")
      return()
    }
    prevJobSubmitted <<- Sys.time()
    hcubeScen <- scenGmsPar[idxDiff]
    if(!length(hcubeScen)){
      flog.debug("No scenarios selected to be solved in Hypercube mode.")
      return()
    }
    tryCatch({
      executeHcubeJob(hcubeScen)
      showHideEl(session, "#hcubeSubmitSuccess", 3000)
      hideModal(session, 3L)
    }, error = function(e){
      flog.error("Some problem occurred while executing Hypercube job. Error message: '%s'.", e)
      showHideEl(session, "#hcubeSubmitUnknownError", 6000)
    })
  })
  
  
  output$btHcubeAll_dl <- downloadHandler(
    filename = function() {
      tolower(modelName) %+% ".zip"
    },
    content = function(file) {
      # solve all scenarios in Hypercube run
      
      # BEGIN EPIGRIDS specific
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
      # END EPIGRIDS specific
    },
    contentType = "application/zip")
  
  output$btHcubeNew_dl <- downloadHandler(
    filename = function() {
      tolower(modelName) %+% ".zip"
    },
    content = function(file) {
      # solve only scenarios that do not yet exist
      
      # BEGIN EPIGRIDS specific
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
      # END EPIGRIDS specific
    },
    contentType = "application/zip")
}


observeEvent(input$btSolve, {
  flog.debug("Solve button clicked (model: '%s').", modelName)
  removeModal()
  if(identical(config$activateModules$hcubeMode, TRUE)){
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
                                  tibble(scodeIdentifier, 1L, ">="))
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
    
    idsToSolve <<- scenToSolve$ids
    if(length(config$extraClArgs)){
      scenGmsPar <<- paste(scenToSolve$gmspar, config$MIROSwitch, 
                           paste(config$extraClArgs, collapse = " "),
                           "--HCUBE=1", "execMode=" %+% gamsExecMode, "lo=3")
    }else{
      scenGmsPar <<- paste(scenToSolve$gmspar, config$MIROSwitch,
                           "--HCUBE=1", "execMode=" %+% gamsExecMode, "lo=3")
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
  prog <- Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$prepRun$title, value = 0)
  
  updateTabsetPanel(session, "sidebarMenuId", selected = "gamsinter")
  
  prog$inc(amount = 0.5, detail = lang$progressBar$prepRun$sendInput)
  # save input data 
  saveInputDb <- FALSE
  source("./modules/input_save.R", local = TRUE)
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
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
        pfGMSPar      <- vapply(seq_along(DDParValues[[1]]), 
                                   function(i){
                                     if(!DDParValues[[3]][i] %in% c("_", "system.empty", "")) 
                                       paste0('--', DDParValues[[1]][i], '=', escapeGAMSCL(DDParValues[[3]][i]))
                                     else
                                       NA_character_
                                   }, character(1L), USE.NAMES = FALSE)
        pfGMSPar      <- pfGMSPar[!is.na(pfGMSPar)]
        # do not write '_' in pf file (no selection)
        pfGMSOpt      <- vapply(seq_along(GMSOptValues[[1]]), 
                                function(i){
                                  if(!GMSOptValues[[3]][i] %in% c("_", "system.empty", "")) 
                                    paste0(GMSOptValues[[1]][i], '=', escapeGAMSCL(GMSOptValues[[3]][i]))
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
      choiceIdx         <- match(dataTmp[[i]][[1L]], 
                                 modelIn[[names(dataTmp)[[i]]]]$dropdown$choices)
      csvData           <- dataTmp[[i]]
      aliasCol          <- modelIn[[names(dataTmp)[[i]]]]$dropdown$aliases[choiceIdx]
      aliasCol[is.na(aliasCol)] <- ""
      csvData[["text"]] <- aliasCol
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
    gamsArgs <- c(config$extraClArgs, paste0('idir1="', gmsFilePath(currentModelDir), '"'),
                  if(config$includeParentDir) paste0('idir2="', gmsFilePath(dirname(currentModelDir)), '"'), 
                  paste0('curdir="', workDir, '"'), "lo=3", "execMode=" %+% gamsExecMode, 
                  config$MIROSwitch, "LstTitleLeftAligned=1")
    if(config$saveTraceFile){
      gamsArgs <- c(gamsArgs, paste0('trace="', tableNameTracePrefix, modelName, '.trc"'), "traceopt=3")
    }
    pfFilePath <- paste0(workDir, tolower(modelName), ".pf")
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
                                                         "pf", pfFilePath), 
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
      logfile <- reactiveFileReader2(300, session, file.path(workDir, modelName %+% ".log"))
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
      logText    <- logfile()
      logSize    <- nchar(logText)
      logText    <- paste0(if(logSize > (3e4 + 1)) "[...]\n", 
                           substr(logText, logSize - 3e4, logSize))
      if(!is.null(modelStatus())){
        return(logText)
      }
      if(input$logUpdate){
        scrollDown(session, "#logStatus")
      }
      return(logText)
    })
  }
  # reset listing file when new solve is started
  output$listFile <- renderText("")
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
          fileSize <- file.size(file.path(workDir, modelName %+% ".lst")) 
          if(is.na(fileSize))
            stop("Could not access listing file.", call. = FALSE)
          if(fileSize > maxSizeToRead){
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
        # check whether GAMS output files shall be stored
        if(config$activateModules$attachments && 
           config$storeLogFilesDuration > 0L && !is.null(activeScen)){
          tryCatch({
            filesToStore  <- c(file.path(workDir, paste0(modelName, c(".log", ".lst"))))
            filesNoAccess <- file.access(filesToStore) == -1L
            errMsg <- NULL
            if(any(filesNoAccess)){
              if(all(filesNoAccess))
                stop("fileAccessException", call. = FALSE)
              flog.info("GAMS output files: '%s' could not be accessed. Check file permissions.", 
                        paste(filesToStore[filesNoAccess], collapse = "', '"))
              errMsg <<- lang$errMsg$saveGAMSLog$noFileAccess
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
                                      noSave = TRUE, execPerm = rep.int(FALSE, length(filesToStore)))
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
                                         scalarsFileHeaders = scalarsFileHeaders, colTypes = db$getDbSchema()$colTypes,
                                         modelOutTemplate = modelOutTemplate, method = "csv", csvDelim = config$csvDelim, 
                                         hiddenOutputScalars = config$hiddenOutputScalars) 
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