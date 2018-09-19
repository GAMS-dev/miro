# load model input data
lapply(datasets.to.fetch, function(dataset){
  i <- match(tolower(dataset), names(modelIn))[[1]]
  data.tmp <- NULL
  if(is.na(i)){
    return(NULL)
  }
  inputVerified <- FALSE
  # execute only if dataframe has not yet been imported or already imported data shall be overridden
  if(!length(isolate(rv[[paste0("in_", i)]])) || overrideInput){
    # handsontable, multi dropdown, or daterange
    if(tolower(dataset) %in% modelInTabularData){
      if(identical(load.mode, "xls")){
        # load from Excel workbook
        tryCatch({
          data.tmp <- readxl::read_excel(input$localInput$datapath, dataset)
        }, error = function(e) {
          flog.warn("Problems reading Excel file: '%s' (user: '%s', datapath: '%s', dataset: '%s'). Error message: %s.", isolate(input$localInput$name), uid, isolate(input$localInput$datapath), dataset, e)
          errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$excelRead, dataset), sep = "\n")
        })
      }else if(identical(load.mode, "scen")){
        data.tmp <- scenInputData[[dataset]]
      }
      if(!is.null(errMsg)){
        return(NULL)
      }
      
      # assign new input data here as assigning it directly inside the tryCatch environment would result in deleting list elements
      # rather than setting them to NULL
      if(nrow(data.tmp)){
        if(verify.input(data.tmp, modelIn[[i]]$headers)){
          if(identical(names(modelIn)[[i]], tolower(scalarsFileName))){
            # remove those rows from scalar dataset that are represented as a slider or dropdown menu
            scalarDataset <<- data.tmp 
            modelInputData[[i]] <<- data.tmp[!(tolower(data.tmp[[1]]) %in% names(modelIn)), , drop = F]
          }else{
            modelInputData[[i]] <<- data.tmp
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
      col.id    <- scalarsFileHeaders[1]
      col.value <- scalarsFileHeaders[3]
      
      # check whether scalar dataset has already been imported
      if(is.null(scalarDataset)){
        if(load.mode == "xls"){
          # load from excel workbook
          tryCatch({
            # make read of excel sheets case insensitive by selecting sheet via ID
            sheet.id <- match(tolower(scalarsFileName), tolower(readxl::excel_sheets(isolate(input$localInput$datapath))))[1]
            data.tmp <- readxl::read_excel(input$localInput$datapath, sheet.id)
            #set names of scalar sheet to scalar headers
            names(data.tmp) <- scalarsFileHeaders
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
        if(!is.null(data.tmp)){
          scalarDataset <<- data.tmp
        }
      }
      if(!is.null(scalarDataset) && nrow(scalarDataset)){
        # double slider has two scalar values saved
        if((modelIn[[i]]$type == "slider" && length(modelIn[[i]]$slider$default) > 1) || (modelIn[[i]]$type == "daterange")){
          row.name <- paste0(row.name, c("_min", "_max"))
          data.tmp <- unlist(scalarDataset[tolower(scalarDataset[[col.id]]) %in% row.name, col.value, drop = F], use.names = F)
          if(!is.null(data.tmp) && length(data.tmp)){
            modelInputData[[i]] <<- data.tmp
            inputVerified <- TRUE
          }
        }else{
          data.tmp <- unlist(scalarDataset[tolower(scalarDataset[[col.id]]) == row.name, 
                                            col.value, drop = FALSE], use.names = FALSE)
          if(!is.null(data.tmp) && length(data.tmp)){
            modelInputData[[i]] <<- data.tmp
            inputVerified <- TRUE
          }
        }
      }
    }
    
    
    # check if input data is valid
    if(inputVerified){
      flog.debug("Dataset: %s loaded successfully (mode: %s, override: %s)", dataset, load.mode, overrideInput)
      newInputCount <<- newInputCount + 1
      # set identifier that data was overwritten 
      isEmptyInput[i] <<- TRUE
      if(!identical(load.mode, "scen")){
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
      input.initialized[dependentDatasets[[i]]] <<- FALSE
      if(!is.null(modelInWithDep[[tolower(names(modelIn)[[i]])]])){
        id <- match(tolower(names(modelIn)[[i]]), tolower(names(modelInWithDep)))[1]
        if(input.initialized[id]){
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

flog.trace("%d new input datasets loaded (load mode: %s, override: %s)", count.new.input, load.mode, overrideInput)
if(!is.null(isolate(rv$activeSname))){
  shinyjs::enable("btSave")
}
shinyjs::enable("btSaveAs")
# set initialisation flags for handsontables to FALSE
if(any(hotInit)){
  hotInit[]     <<- FALSE
}