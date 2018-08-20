# load input data from excel sheet

observeEvent(input$btLoadLocal,{
  if(identical(config$activateModules$loadLocal, FALSE)){
    return(NULL)
  }
  if(is.null(isolate(rv$active.sname))){
    active.sname.tmp <<- gsub("\\.[^\\.]+$", "", isolate(input$localInput$name))
  }
  # check whether current input datasets are empty
  if(isolate(input$cbSelectManuallyLoc) && length(isolate(input$selInputDataLoc))){
    ids.to.fetch <- match(tolower(isolate(input$selInputDataLoc)), tolower(names(modelIn)))
    # remove NAs
    ids.to.fetch <- ids.to.fetch[!is.na(ids.to.fetch)]
  }else{
    ids.to.fetch <- seq_along(modelIn)
  }
  datasets.imported <- vapply(ids.to.fetch, function(i){
    if(length(isolate(rv[[paste0("in_", i)]]))){
      return(T)
    }else{
      return(F)
    }
  }, logical(1))

  if(any(datasets.imported)){
    showModal(modalDialog(
      title = lang$nav$dialogLoadScen$titleOverrideInput,
      lang$nav$dialogLoadScen$descOverrideInput,
      footer = tagList(
        modalButton(lang$nav$dialogLoadScen$cancelButton),
        actionButton("btOverrideLocal", label = lang$nav$dialogLoadScen$okButton, class = "btOrange")),
      fade=FALSE, easyClose=FALSE))
  }else{
    overrideInput <<- F
    if(is.null(isolate(rv$btOverrideLocal))){
      rv$btOverrideLocal <<- 1
    }else{
      rv$btOverrideLocal <<- isolate(rv$btOverrideLocal + 1)
    }
}
})

observeEvent(input$btOverrideLocal, {
  overrideInput <<- T
  if(is.null(isolate(rv$btOverrideLocal))){
    rv$btOverrideLocal <<- 1
  }else{
    rv$btOverrideLocal <<- isolate(rv$btOverrideLocal + 1)
  }
})

observeEvent(virtualActionButton(rv$btOverrideLocal),{
  if(is.null(input$localInput$datapath)){
    flog.error("Load Excel event was triggered but no datapath specified. This should not happen!")
    return(NULL)
  }
  # initialize new imported sheets counter
  count.new.input <- 0
  errMsg <- NULL
  scalar.dataset <- NULL
  # read Excel file
  tryCatch({
    xlsWbNames <- readxl::excel_sheets(input$localInput$datapath)
  }, error = function(e) {
    flog.error("Some error occurred reading the file: '%s'. Error message: %s.", as.character(isolate(input$localInput$name)), e)
    errMsg <<- sprintf(lang$errMsg$GAMSInput$excelRead, as.character(isolate(input$localInput$name)))
  })
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  # extract only sheets which are also in list of input parameters
  datasets.to.fetch <- xlsWbNames[tolower(xlsWbNames) %in% c(modelIn.tabular.data, scalars.file.name)]
  
  # extract scalar sheets
  if(length(modelIn) > length(modelIn.tabular.data)){
    # atleast one scalar input element that is not in tabular form
    i <- match(tolower(scalars.file.name), tolower(datasets.to.fetch))[[1]]
    if(!is.na(i)){
      # scalar table in workbook
      # add scalar datasets (e.g. slider/dropdown)
      if(tolower(scalars.file.name) %in% tolower(modelIn.tabular.data)){
        # scalars is also amongst tabular data
        datasets.to.fetch <- c(datasets.to.fetch, names(modelIn)[!(names(modelIn) %in% modelIn.tabular.data)])
      }else{
        # all scalar values are dropdown/slider etc. so remove scalar table from datasets.to.fetch
        datasets.to.fetch <- c(datasets.to.fetch[-i], names(modelIn)[!(names(modelIn) %in% modelIn.tabular.data)])
      }
    }
  }
  
  # find out which datasets to import from Excel sheet
  if(isolate(input$cbSelectManuallyLoc) && length(isolate(input$selInputDataLoc))){
    datasets.to.fetch <- datasets.to.fetch[tolower(datasets.to.fetch) %in% tolower(isolate(input$selInputDataLoc))]
  }
  removeModal()
  # load input data 
  load.mode <- "xls"
  source("./modules/input_load.R", local = TRUE)
  if(!is.null(errMsg)){
    return(NULL)
  }
  
  # set no output identifier
  no.output.data <<- T
  if(count.new.input){
    showNotification(paste0(count.new.input, lang$nav$notificationNewInput$new))
  }else{
    showNotification(lang$nav$notificationNewInput$noNew, type = "error")
  }
})