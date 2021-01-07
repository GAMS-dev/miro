# load input data from local file
observeEvent(input$localInput, {
  if(useGdx &&
     identical(tolower(tools::file_ext(basename(input$localInput$datapath))),
               "miroscen")){
    hideEl(session, "#localInputSelectManually")
  }else{
    showEl(session, "#localInputSelectManually")
  }
})
observeEvent(input$btImportLocal, {
  if(identical(config$activateModules$loadLocal, FALSE)){
    flog.error("Try to load local data even though the loadLocal module is disabled! This is most likely because the user is trying to tamper with the app!")
    return()
  }
  flog.debug("Load local data button clicked.")
  errMsg <- NULL
  
  # check whether current input datasets are empty
  if(input$cbSelectManuallyLoc && length(input$selInputDataLoc)){
    idsToFetch <- match(tolower(input$selInputDataLoc), names(modelIn))
    # remove NAs
    idsToFetch <- idsToFetch[!is.na(idsToFetch)]
  }else{
    idsToFetch <- seq_along(modelIn)
  }
  datasetsImported <- vapply(idsToFetch, function(i){
    if(length(isolate(rv[[paste0("in_", i)]]))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }, logical(1L))
  
  if(any(datasetsImported)){
    hideEl(session, "#importDataTabset")
    showEl(session, "#btOverwriteInput")
    showEl(session, "#importDataOverwrite")
  }else{
    overwriteInput <<- FALSE
    rv$btOverwriteInput <<- rv$btOverwriteInput + 1L
  }
})

observeEvent(input$btOverwriteInput, {
  overwriteInput <<- TRUE
  rv$btOverwriteInput <<- rv$btOverwriteInput + 1L
})

observeEvent(virtualActionButton(rv$btOverwriteInput),{
  if(is.null(input$localInput$datapath)){
    return(NULL)
  }
  if(identical(config$activateModules$loadLocal, FALSE)){
    flog.error("Try to load local data even though the loadLocal module is disabled! This is most likely because the user is trying to tamper with the app!")
    return()
  }
  
  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  
  loadModeFileName <- basename(input$localInput$datapath)
  loadModeWorkDir  <- dirname(input$localInput$datapath)
  fileType <- tolower(tools::file_ext(loadModeFileName))
  
  prog <- Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$importScen$title, value = 0.1)
  
  dfClArgs <- NULL
  
  if(identical(fileType, "miroscen") && useGdx){
    resetWidgetsOnClose <<- FALSE
    if(!closeScenario(clearMeta = TRUE)){
      showHideEl(session, "#importScenError", 4000L)
      return()
    }
    if(!tryCatch(validateMiroScen(input$localInput$datapath), error = function(e){
      flog.info("Invalid miroscen file. Error message: '%s'.", conditionMessage(e))
      showHideEl(session, "#importScenInvalidFile", 4000L)
      return(FALSE)
    })){
      return()
    }
    dfClArgs <- tryCatch(loadMiroScen(input$localInput$datapath, activeScen, attachments, views,
                                      names(modelIn)),
                         error = function(e){
                           showHideEl(session, "#importScenError", 4000L)
                           flog.info("Problems reading miroscen file. Error message: '%s'.",
                                     conditionMessage(e))
                           return(FALSE)
                         })
    if(isFALSE(dfClArgs)){
      return()
    }
    loadModeFileName <- "data.gdx"
    loadMode <- "gdx"
    datasetsToFetch <- names(modelIn)
  }else if(identical(fileType, "gdx") && useGdx){
    loadMode <- "gdx"
    datasetsToFetch <- names(modelIn)
  }else if(identical(fileType, "zip")){
    loadMode <- "csv"
    csvFiles <- tryCatch(
      getValidCsvFromZip(input$localInput$datapath, 
                         c(names(modelOut), 
                           inputDsNames), uid)
      , error = function(e){
        errMsg <- conditionMessage(e)
        if(startsWith(errMsg, "e:")){
          showHideEl(session, "#importScenError", 4000L)
          flog.error(errMsg)
        }else{
          flog.info(errMsg)
          showHideEl(session, "#importScenInvalidFile", 4000L)
        }
        return("e")
      })
    if(identical(csvFiles,"e")){
      return()
    }
    on.exit({
      if(identical(unlink(csvFiles$tmpDir, recursive = TRUE), 0L)){
        flog.debug("Temporary directory: '%s' removed.", csvFiles$tmpDir)
      }else{
        flog.error("Problems removing temporary directory: '%s'.", csvFiles$tmpDir)
      }}, add = TRUE)
    datasetsToFetch <- substr(csvFiles$validFileNames, 1L, 
                              nchar(csvFiles$validFileNames) - 4L)
    loadModeWorkDir <- csvFiles$tmpDir
  }else if(identical(fileType, "csv")){
    loadMode <- "csv"
    fileNameRaw <- character(0L)
    if(is.character(input$localInput$name) && 
       length(input$localInput$name) == 1L) {
      fileNameRaw <- tolower(tools::file_path_sans_ext(input$localInput$name))
    }
    if(isTRUE(input$cbSelectManuallyLoc) && length(input$selInputDataLoc) > 1L){
      if(any(!input$selInputDataLoc %in% names(modelInToImport))){
        flog.error("Selected input dataset(s) is not in list of model data to import. This looks like an attempt to tamper with the app!")
        showHideEl(session, "#importScenNoDsSelected", 4000L)
        return()
      }
      if(length(input$selInputDataLoc) > 1L &&
         any(tolower(input$selInputDataLoc) %in% modelInTabularData)){
        flog.debug("Local file import stopped as multiple datasets were selected and not all of them are scalar datasets.")
        showHideEl(session, "#symNotInDataSrc", 4000L)
        return()
      }
      datasetsToFetch <- input$selInputDataLoc
    }else if(length(fileNameRaw) && fileNameRaw %in% c(modelInTabularData, scalarsFileName)){
      datasetsToFetch <- fileNameRaw
    }else{
      flog.debug("Local file import stopped as no datasheet was specified (must be specified when uploading csv files).")
      showHideEl(session, "#importScenNoDsSelected", 4000L)
      return()
    }
    if(any(tolower(datasetsToFetch) %in% modelInTabularData)){
      fnTmp <- datasetsToFetch
    }else{
      fnTmp <- scalarsFileName
    }
    if(!file.rename(input$localInput$datapath, 
                    paste0(loadModeWorkDir, .Platform$file.sep, 
                           fnTmp, ".csv"))){
      showHideEl(session, "#importScenError", 4000L)
      return()
    }
  }else if(fileType %in% c("xls", "xlsx")){
    loadMode <- "xls"
    # read Excel file
    tryCatch({
      xlsWbNames <- excel_sheets(input$localInput$datapath)
    }, error = function(e) {
      flog.error("Some error occurred reading the file: '%s'. Error message: %s.", as.character(isolate(input$localInput$name)), e)
      errMsg <<- sprintf(lang$errMsg$GAMSInput$excelRead, as.character(isolate(input$localInput$name)))
    })
    if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
      return()
    }
    xlsWbNames <- vapply(strsplit(xlsWbNames, " ", fixed = TRUE), "[[", character(1L), 1L)
    # extract only sheets which are also in list of input parameters
    datasetsToFetch <- xlsWbNames[tolower(xlsWbNames) %in% 
                                    c(modelInTabularData, scalarsFileName, 
                                      scalarInputSym)]
  }else{
    removeModal()
    showErrorMsg(lang$errMsg$invalidFileType$title, 
                 sprintf(lang$errMsg$invalidFileType$desc, paste0(c("xls", "xlsx", "csv", if(useGdx) c("miroscen", "gdx")),
                                                                  collapse = ",")))
    flog.info("Invalid file type: '%s' attempted to be imported. Import interrupted.", fileType)
    return()
  }
  removeModal()
  datasetsToFetch <- datasetsToFetch[datasetsToFetch %in% c(names(modelInToImport), 
                                                            scalarsFileName)]
  
  # extract scalar sheets
  if(!identical(loadMode, "gdx") &&
     (length(modelIn) > length(modelInTabularData)  || !scalarsFileName %in% names(modelIn))){
    # atleast one scalar input element that is not in tabular form
    i <- match(scalarsFileName, tolower(datasetsToFetch))[[1]]
    if(!is.na(i)){
      # scalar table in workbook
      # add scalar datasets (e.g. slider/dropdown)
      if(scalarsFileName %in% tolower(modelInTabularData)){
        # scalars is also amongst tabular data
        datasetsToFetch <- c(datasetsToFetch, names(modelIn)[!(names(modelIn) %in% modelInTabularData)])
      }else{
        # all scalar values are dropdown/slider etc. so remove scalar table from datasetsToFetch
        datasetsToFetch <- c(datasetsToFetch[-i], names(modelIn)[!(names(modelIn) %in% modelInTabularData)])
      }
    }
  }
  
  # find out which datasets to import from Excel sheet
  if(input$cbSelectManuallyLoc && length(input$selInputDataLoc)){
    datasetsToFetch <- datasetsToFetch[tolower(datasetsToFetch) %in% 
                                         tolower(isolate(input$selInputDataLoc))]
  }
  prog$set(detail = lang$progressBar$importScen$renderInput, value = 0.4)
  
  # reset input data
  modelInputGraphVisible[] <<- FALSE
  lapply(seq_along(modelIn)[names(modelIn) %in% datasetsToFetch], function(i){
    hideEl(session, "#graph-in_" %+% i)
    showEl(session, "#data-in_" %+% i)
  })
  
  source("./modules/input_load.R", local = TRUE)
  if(identical(fileType, "miroscen")){
    scenMetaData[["scen_1_"]] <<- activeScen$
      getMetadata(lang$nav$excelExport$metadataSheet)
    # update scenario name
    rv$activeSname  <<- activeScen$getScenName()
  }else{
    markUnsaved()
  }
  if(!is.null(errMsg)){
    return(NULL)
  }
  errMsg <- NULL
  # save input data
  idxMap <- match(modelInFileNames, names(scenInputData))
  lapply(seq_along(modelInFileNames), function(i){
    if(is.na(idxMap[i]) || is.null(scenInputData[[idxMap[i]]])){
      scenData[["scen_1_"]][[i + length(modelOut)]] <<- scenDataTemplate[[i]]
    }else{
      scenData[["scen_1_"]][[i + length(modelOut)]] <<- scenInputData[[idxMap[i]]]
    }
  })
  if(LAUNCHHCUBEMODE){
    noOutputData <<- TRUE
  }else{
    prog$set(detail = lang$progressBar$importScen$renderOutput, value = 0.8)
    tryCatch({
      outputData <- loadScenData(scalarsName = scalarsOutName, metaData = modelOut, 
                                 workDir = loadModeWorkDir, 
                                 modelName = modelName, errMsg = lang$errMsg$GAMSOutput$badOutputData,
                                 scalarsFileHeaders = scalarsFileHeaders,
                                 templates = modelOutTemplate, method = loadMode,
                                 hiddenOutputScalars = config$hiddenOutputScalars, 
                                 fileName = loadModeFileName) 
    }, error = function(e){
      flog.info("Problems loading output data. Error message: %s.", 
                conditionMessage(e))
      errMsg <<- conditionMessage(e)
    })
    if(is.null(showErrorMsg(lang$errMsg$GAMSOutput$title, errMsg))){
      return()
    }
    scenData[["scen_1_"]][seq_along(modelOut)] <<- outputData$tabular
    if(isFALSE(outputData$noTabularData)){
      scalarData[["scen_1_"]] <<- outputData$scalar
      renderOutputData(rendererEnv, views)
      noOutputData <<- FALSE
    }else{
      noOutputData <<- TRUE
    }
    
    scalarIdTmp <- match(scalarsFileName, tolower(names(scenInputData)))[[1L]]
    if(!is.na(scalarIdTmp)){
      scalarData[["scen_1_"]] <<- bind_rows(scenInputData[[scalarIdTmp]], scalarData[["scen_1_"]])
    }
    outputData <- NULL
  }
  
  if(newInputCount){
    showNotification(sprintf(lang$nav$notificationNewInput$new, newInputCount))
  }else{
    showNotification(lang$nav$notificationNewInput$noNew, type = "error")
  }
})