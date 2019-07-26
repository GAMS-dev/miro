# load input data from excel sheet
observeEvent(input$btOverwriteLocal, {
  scenName <- isolate(input$local_newScenName)
  flog.debug("Overwrite existing scenario (name: '%s') button clicked.", scenName)
  errMsg <- NULL
  tryCatch({
    activeScen <<- Scenario$new(db = db, sname = scenName, overwrite = TRUE)
  }, error = function(e){
    flog.error("Problems creating scenario. Error message: '%s'.", e)
    errMsg <<- lang$errMsg$saveScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))){
    return(NULL)
  }
  rv$activeSname <- scenName
  rv$btLoadLocal <- isolate(rv$btLoadLocal + 1L)
})
observeEvent(input$btCheckSnameLocalConfirm, {
  if(length(isolate(rv$activeSname))){
    rv$btLoadLocal <- isolate(rv$btLoadLocal + 1L)
  }
  scenNameTmp <- isolate(input$local_newScenName)
  flog.debug("Button to upload local dataset clicked. Validating if scenario name: '%s' is valid and does not yet exist.", 
             scenNameTmp)
  if(is.null(isolate(rv$activeSname))){
    scenNameTmp <- isolate(input$local_newScenName)
    if(isBadScenName(scenNameTmp)){
      flog.debug("Scenario name is not valid.")
      showHideEl(session, "#local_badScenName", 4000L)
      return()
    }else{
      if(identical(config$activateModules$scenario, TRUE)){
        if(db$checkSnameExists(scenNameTmp)){
          flog.debug("Scenario name is valid, but already exists.")
          showEl(session, "#loadLocal_scenNameExists")
          hideEl(session, "#loadLocal_content")
          return()
        }
        activeScen <<- Scenario$new(db = db, sname = scenNameTmp)
      }
      rv$activeSname <- scenNameTmp
      rv$btLoadLocal <- isolate(rv$btLoadLocal + 1L)
    }
  }
})
observeEvent(virtualActionButton(rv$btLoadLocal),{
  flog.debug("Load local data button clicked.")
  
  if(identical(config$activateModules$loadLocal, FALSE)){
    return()
  }
  
  # check whether current input datasets are empty
  if(isolate(input$cbSelectManuallyLoc) && length(isolate(input$selInputDataLoc))){
    idsToFetch <- match(tolower(isolate(input$selInputDataLoc)), tolower(names(modelIn)))
    # remove NAs
    idsToFetch <- idsToFetch[!is.na(idsToFetch)]
  }else{
    idsToFetch <- seq_along(modelIn)
  }
  datasetsImported <- vapply(idsToFetch, function(i){
    if(length(isolate(rv[[paste0("in_", i)]])) || 
       isolate(rv$datasetsModified[i])){
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
    rv$btOverwriteInput <<- isolate(rv$btOverwriteInput + 1L)
}
})

observeEvent(input$btOverwriteInput, {
  overwriteInput <<- TRUE
  rv$btOverwriteInput <<- isolate(rv$btOverwriteInput + 1L)
})

observeEvent(virtualActionButton(rv$btOverwriteInput),{
  if(is.null(input$localInput$datapath)){
    flog.error("Load Excel event was triggered but no datapath specified. This should not happen!")
    return(NULL)
  }
  
  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  
  fileType <- tolower(tools::file_ext(isolate(input$localInput$datapath)))
  if(identical(fileType, "gdx") && useGdx){
    loadMode <- "gdx"
    datasetsToFetch <- c(modelInTabularData, scalarsFileName)
  }else if(fileType %in% c("xls", "xlsx")){
    loadMode <- "xls"
    # read Excel file
    tryCatch({
      xlsWbNames <- excel_sheets(input$localInput$datapath)
    }, error = function(e) {
      flog.error("Some error occurred reading the file: '%s'. Error message: %s.", as.character(isolate(input$localInput$name)), e)
      errMsg <<- sprintf(lang$errMsg$GAMSInput$excelRead, as.character(isolate(input$localInput$name)))
    })
    xlsWbNames <- vapply(strsplit(xlsWbNames, " ", fixed = TRUE), "[[", character(1L), 1L)
    if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
      return()
    }
    # extract only sheets which are also in list of input parameters
    datasetsToFetch <- xlsWbNames[tolower(xlsWbNames) %in% c(modelInTabularData, scalarsFileName)]
  }else{
    showErrorMsg(lang$errMsg$readOutput$title, lang$errMsg$readOutput$desc)
    return()
  }
  datasetsToFetch <- datasetsToFetch[datasetsToFetch %in% names(modelInToImport)]
  # extract scalar sheets
  if(length(modelIn) > length(modelInTabularData)){
    # atleast one scalar input element that is not in tabular form
    i <- match(tolower(scalarsFileName), tolower(datasetsToFetch))[[1]]
    if(!is.na(i)){
      # scalar table in workbook
      # add scalar datasets (e.g. slider/dropdown)
      if(tolower(scalarsFileName) %in% tolower(modelInTabularData)){
        # scalars is also amongst tabular data
        datasetsToFetch <- c(datasetsToFetch, names(modelIn)[!(names(modelIn) %in% modelInTabularData)])
      }else{
        # all scalar values are dropdown/slider etc. so remove scalar table from datasetsToFetch
        datasetsToFetch <- c(datasetsToFetch[-i], names(modelIn)[!(names(modelIn) %in% modelInTabularData)])
      }
    }
  }
  
  # find out which datasets to import from Excel sheet
  if(isolate(input$cbSelectManuallyLoc) && length(isolate(input$selInputDataLoc))){
    datasetsToFetch <- datasetsToFetch[tolower(datasetsToFetch) %in% tolower(isolate(input$selInputDataLoc))]
  }
  removeModal()
  
  source("./modules/input_load.R", local = TRUE)
  if(!is.null(errMsg)){
    return(NULL)
  }
  if(!config$activateModules$hcubeMode){
    tryCatch({
      outputData <- loadScenData(scalarsName = scalarsOutName, metaData = modelOut, 
                                 workDir = dirname(isolate(input$localInput$datapath)), 
                                 modelName = modelName, errMsg = lang$errMsg$GAMSOutput$badOutputData,
                                 scalarsFileHeaders = scalarsFileHeaders,
                                 templates = modelOutTemplate, method = loadMode,
                                 hiddenOutputScalars = config$hiddenOutputScalars, 
                                 fileName = basename(isolate(input$localInput$datapath))) 
    }, error = function(e){
      flog.error("Problems loading output data. Error message: %s.", e)
      errMsg <<- lang$errMsg$readOutput$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
      return()
    }
    if(any(vapply(outputData$tabular, function(el){length(el) > 0L}, logical(1L), USE.NAMES = FALSE))){
      scenData[["scen_1_"]] <<- outputData$tabular
      if(!is.null(outputData$scalar)){
        scalarData[["scen_1_"]] <<- outputData$scalar
      }
      renderOutputData()
      noOutputData <<- FALSE
    }else{
      noOutputData <<- TRUE
    }
    scalarIdTmp <- match(scalarsFileName, tolower(names(scenInputData)))[[1L]]
    if(!is.na(scalarIdTmp)){
      scalarData[["scen_1_"]] <<- bind_rows(scenInputData[[scalarIdTmp]], scalarData[["scen_1_"]])
    }
    outputData <- NULL
  }else{
    noOutputData <<- TRUE
  }
  
  if(newInputCount){
    showNotification(sprintf(lang$nav$notificationNewInput$new, newInputCount))
  }else{
    showNotification(lang$nav$notificationNewInput$noNew, type = "error")
  }
})