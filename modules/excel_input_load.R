# load input data from excel sheet
observeEvent(input$btOverrideLocal, {
  scenName <- isolate(input$local_newScenName)
  flog.debug("Override existing scenario (name: '%s') button clicked.", scenName)
  activeScen <<- Scenario$new(db = db, sname = scenName)
  rv$activeSname <- scenName
  rv$btLoadLocal <- isolate(rv$btLoadLocal + 1L)
})
observeEvent(input$btCheckSnameLocal, {
  if(length(isolate(rv$activeSname))){
    rv$btLoadLocal <- isolate(rv$btLoadLocal + 1L)
  }
  scenNameTmp <- isolate(input$local_newScenName)
  flog.debug("Button to upload local dataset clicked. Validating if scenario name: '%s' is valid and does not yet exist.", 
             scenNameTmp)
  if(is.null(isolate(rv$activeSname))){
    scenNameTmp <- isolate(input$local_newScenName)
    if(grepl("^\\s*$", scenNameTmp)){
      flog.debug("Scenario name is not valid.")
      shinyjs::show("local_badScenName")
      return()
    }else{
      if(identical(config$activateModules$scenario, TRUE)){
        flog.debug("Scenario name is valid, but already exists.")
        if(db$checkSnameExists(scenNameTmp)){
          shinyjs::show("loadLocal_scenNameExists")
          hide("loadLocal_content")
          hide("local_badScenName")
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
    ids.to.fetch <- match(tolower(isolate(input$selInputDataLoc)), tolower(names(modelIn)))
    # remove NAs
    ids.to.fetch <- ids.to.fetch[!is.na(ids.to.fetch)]
  }else{
    ids.to.fetch <- seq_along(modelIn)
  }
  datasetsImported <- vapply(ids.to.fetch, function(i){
    if(length(isolate(rv[[paste0("in_", i)]]))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }, logical(1L))
  
  if(any(datasetsImported)){
    hide("importDataTabset")
    shinyjs::show("btOverrideInput")
    shinyjs::show("importDataOverride")
  }else{
    overrideInput <<- FALSE
    rv$btOverrideInput <<- isolate(rv$btOverrideInput + 1L)
}
})

observeEvent(input$btOverrideInput, {
  overrideInput <<- TRUE
  rv$btOverrideInput <<- isolate(rv$btOverrideInput + 1L)
})

observeEvent(virtualActionButton(rv$btOverrideInput),{
  if(is.null(input$localInput$datapath)){
    flog.error("Load Excel event was triggered but no datapath specified. This should not happen!")
    return(NULL)
  }
  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  # read Excel file
  tryCatch({
    xlsWbNames <- excel_sheets(input$localInput$datapath)
  }, error = function(e) {
    flog.error("Some error occurred reading the file: '%s'. Error message: %s.", as.character(isolate(input$localInput$name)), e)
    errMsg <<- sprintf(lang$errMsg$GAMSInput$excelRead, as.character(isolate(input$localInput$name)))
  })
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  # extract only sheets which are also in list of input parameters
  datasetsToFetch <- xlsWbNames[tolower(xlsWbNames) %in% c(modelInTabularData, scalarsFileName)]
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
  # load input data 
  loadMode <- "xls"
  source("./modules/input_load.R", local = TRUE)
  if(!is.null(errMsg)){
    return(NULL)
  }
  
  # set no output identifier
  noOutputData <<- T
  if(newInputCount){
    showNotification(paste0(newInputCount, lang$nav$notificationNewInput$new))
  }else{
    showNotification(lang$nav$notificationNewInput$noNew, type = "error")
  }
})