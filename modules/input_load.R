# load model input data
lapply(datasetsToFetch, function(dataset){
  i <- match(tolower(dataset), names(modelIn))[[1]]
  dataTmp <- NULL
  if(is.na(i)){
    return()
  }
  inputVerified <- FALSE
  # execute only if dataframe has not yet been imported or already imported data shall be overridden
  if(!length(isolate(rv[["in_" %+% i]])) || overrideInput){
    # handsontable, multi dropdown, or daterange
    if(tolower(dataset) %in% modelInTabularData){
      if(identical(loadMode, "xls")){
        # load from Excel workbook
        tryCatch({
          dataTmp <- read_excel(input$localInput$datapath, dataset)
        }, error = function(e) {
          flog.warn("Problems reading Excel file: '%s' (user: '%s', datapath: '%s', dataset: '%s'). Error message: %s.", 
                    isolate(input$localInput$name), uid, isolate(input$localInput$datapath), dataset, e)
          errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$excelRead, dataset), sep = "\n")
        })
      }else if(identical(loadMode, "scen")){
        dataTmp <- scenInputData[[dataset]]
      }
      if(!is.null(errMsg)){
        return(NULL)
      }
      # assign new input data here as assigning it directly inside the tryCatch environment would result in deleting list elements
      # rather than setting them to NULL
      if(nrow(dataTmp)){
        if(verifyInput(dataTmp, modelIn[[i]]$headers)){
          if(identical(names(modelIn)[[i]], tolower(scalarsFileName))){
            # remove those rows from scalar dataset that are represented as a slider or dropdown menu
            scalarDataset <<- dataTmp 
            modelInputData[[i]] <<- dataTmp[!(tolower(dataTmp[[1]]) %in% names(modelIn)), , drop = F]
          }else{
            modelInputData[[i]] <<- dataTmp
          }
          inputVerified <- TRUE
        }
      }else{
        # empty dataset
        modelInputData[[i]] <<- modelInTemplate[[i]]
        isEmptyInput[[i]]   <<- TRUE
        inputVerified         <- TRUE
      }
      
    }else{
      # single dropdown, slider or date
      
      # get row names that need to be extracted from scalar table
      row.name <- tolower(names(modelIn)[[i]])
      # get column name of ID and value column
      colId    <- scalarsFileHeaders[1]
      colValue <- scalarsFileHeaders[3]
      
      # check whether scalar dataset has already been imported
      if(is.null(scalarDataset)){
        if(loadMode == "xls"){
          # load from excel workbook
          tryCatch({
            # make read of excel sheets case insensitive by selecting sheet via ID
            sheet.id <- match(tolower(scalarsFileName), tolower(readxl::excel_sheets(isolate(input$localInput$datapath))))[1]
            dataTmp <- readxl::read_excel(input$localInput$datapath, sheet.id)
            #set names of scalar sheet to scalar headers
            names(dataTmp) <- scalarsFileHeaders
          }, error = function(e) {
            flog.warn("Problems reading Excel file: '%s' (datapath: '%s', dataset: '%s'). 
                      Error message: %s.", isolate(input$localInput$name), isolate(input$localInput$datapath), dataset, e)
            errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$excelRead, dataset), sep = "\n")
          })
        }
        if(!is.null(errMsg)){
          return(NULL)
        }
        # assign new input data here as assigning it directly inside the tryCatch environment would result in deleting list elements
        # rather than setting them to NULL
        if(!is.null(dataTmp)){
          scalarDataset <<- dataTmp
        }
      }
      if(!is.null(scalarDataset) && nrow(scalarDataset)){
        # double slider has two scalar values saved
        if((modelIn[[i]]$type == "slider" && length(modelIn[[i]]$slider$default) > 1) || (modelIn[[i]]$type == "daterange")){
          row.name <- paste0(row.name, c("_min", "_max"))
          dataTmp <- unlist(scalarDataset[tolower(scalarDataset[[colId]]) %in% row.name, colValue, drop = F], use.names = F)
          if(!is.null(dataTmp) && length(dataTmp)){
            modelInputData[[i]] <<- dataTmp
            inputVerified <- TRUE
          }
        }else{
          dataTmp <- unlist(scalarDataset[tolower(scalarDataset[[colId]]) == row.name, 
                                            colValue, drop = FALSE], use.names = FALSE)
          if(!is.null(dataTmp) && length(dataTmp)){
            modelInputData[[i]] <<- dataTmp
            inputVerified <- TRUE
          }
        }
      }
    }
    
    
    # check if input data is valid
    if(inputVerified){
      flog.debug("Dataset: %s loaded successfully (mode: %s, override: %s)", dataset, loadMode, overrideInput)
      newInputCount <<- newInputCount + 1
      # set identifier that data was overwritten 
      isEmptyInput[i] <<- TRUE
      if(!identical(loadMode, "scen")){
        # set unsaved flag
        rv$unsavedFlag <<- TRUE
        # if scenario includes output data set dirty flag
        if(!noOutputData){
          dirtyFlag <<- TRUE
          shinyjs::show("dirtyFlagIcon")
          shinyjs::show("dirtyFlagIconO")
        }
      }
      # reset dependent elements
      inputInitialized[dependentDatasets[[i]]] <<- FALSE
      
      if(!is.null(modelInWithDep[[tolower(names(modelIn)[[i]])]])){
        id <- match(tolower(names(modelIn)[[i]]), tolower(names(modelInWithDep)))[1]
        if(inputInitialized[id]){
          # only update when initialized
          if(length(isolate(rv[[paste0("in_", i)]]))){
            rv[[paste0("in_", i)]] <<- isolate(rv[[paste0("in_", i)]]) + 1
          }else{
            rv[[paste0("in_", i)]] <<- 1
          }
        }
      }else{
        # no dependencies, so update anyway
        if(length(isolate(rv[[paste0("in_", i)]]))){
          rv[[paste0("in_", i)]] <<- isolate(rv[[paste0("in_", i)]]) + 1
        }else{
          rv[[paste0("in_", i)]] <<- 1
        }
      }
    }else{
      if(tolower(dataset) %in% names(modelInMustImport)){
        flog.info("The uploaded dataset: '%s' could not be verified.", modelInAlias[i])
        errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$badInputData, modelInAlias[i]), sep = "\n")
      }
    }
  }
})
showErrorMsg(lang$errMsg$GAMSInput$title, errMsg)

flog.trace("%d new input datasets loaded (load mode: %s, override: %s)", count.new.input, loadMode, overrideInput)
if(!is.null(isolate(rv$activeSname))){
  shinyjs::enable("btSave")
}
shinyjs::enable("btSaveAs")
# set initialisation flags for handsontables to FALSE
if(any(hotInit)){
  hotInit[]     <<- FALSE
}