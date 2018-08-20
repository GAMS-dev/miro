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
    if(tolower(dataset) %in% modelIn.tabular.data){
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
          if(identical(names(modelIn)[[i]], tolower(scalars.file.name))){
            # remove those rows from scalar dataset that are represented as a slider or dropdown menu
            scalar.dataset <<- data.tmp 
            model.input.data[[i]] <<- data.tmp[!(tolower(data.tmp[[1]]) %in% names(modelIn)), , drop = F]
          }else{
            model.input.data[[i]] <<- data.tmp
          }
          inputVerified <- TRUE
        }
      }else{
        # empty dataset
        model.input.data[[i]] <<- modelInTemplate[[i]]
        is.empty.input[[i]]   <<- TRUE
        inputVerified         <- TRUE
      }
      
    }else{
      # single dropdown, slider or date
      
      # get row names that need to be extracted from scalar table
      row.name <- tolower(names(modelIn)[[i]])
      # get column name of ID and value column
      col.id    <- scalars.file.headers[1]
      col.value <- scalars.file.headers[3]
      
      # check whether scalar dataset has already been imported
      if(is.null(scalar.dataset)){
        if(load.mode == "xls"){
          # load from excel workbook
          tryCatch({
            # make read of excel sheets case insensitive by selecting sheet via ID
            sheet.id <- match(tolower(scalars.file.name), tolower(readxl::excel_sheets(isolate(input$localInput$datapath))))[1]
            data.tmp <- readxl::read_excel(input$localInput$datapath, sheet.id)
            #set names of scalar sheet to scalar headers
            names(data.tmp) <- scalars.file.headers
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
          scalar.dataset <<- data.tmp
        }
      }
      if(!is.null(scalar.dataset) && nrow(scalar.dataset)){
        # double slider has two scalar values saved
        if((modelIn[[i]]$type == "slider" && length(modelIn[[i]]$slider$default) > 1) || (modelIn[[i]]$type == "daterange")){
          row.name <- paste0(row.name, c("_min", "_max"))
          data.tmp <- unlist(scalar.dataset[tolower(scalar.dataset[[col.id]]) %in% row.name, col.value, drop = F], use.names = F)
          if(!is.null(data.tmp) && length(data.tmp)){
            model.input.data[[i]] <<- data.tmp
            inputVerified <- TRUE
          }
        }else{
          data.tmp <- unlist(scalar.dataset[tolower(scalar.dataset[[col.id]]) == row.name, 
                                            col.value, drop = FALSE], use.names = FALSE)
          if(!is.null(data.tmp) && length(data.tmp)){
            model.input.data[[i]] <<- data.tmp
            inputVerified <- TRUE
          }
        }
      }
    }
    
    
    # check if input data is valid
    if(inputVerified){
      flog.debug("Dataset: %s loaded successfully (mode: %s, override: %s)", dataset, load.mode, overrideInput)
      count.new.input <<- count.new.input + 1
      # set identifier that data was overwritten 
      is.empty.input[i] <<- TRUE
      if(!identical(load.mode, "scen")){
        # set unsaved flag
        rv$unsavedFlag <<- TRUE
        # if scenario includes output data set dirty flag
        if(!no.output.data){
          dirty.flag <<- TRUE
          shinyjs::show("dirtyFlagIcon")
          shinyjs::show("dirtyFlagIconO")
        }
      }
      # reset dependent elements
      input.initialized[dependent.datasets[[i]]] <<- FALSE
      if(!is.null(modelIn.with.dep[[tolower(names(modelIn)[[i]])]])){
        id <- match(tolower(names(modelIn)[[i]]), tolower(names(modelIn.with.dep)))[1]
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
      if(tolower(dataset) %in% names(modelIn.must.import)){
        flog.info("The uploaded dataset: '%s' could not be verified.", modelIn.alias[i])
        errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$badInputData, modelIn.alias[i]), sep = "\n")
      }
    }
  }
})
showErrorMsg(lang$errMsg$GAMSInput$title, errMsg)

flog.trace("%d new input datasets loaded (load mode: %s, override: %s)", count.new.input, load.mode, overrideInput)
if(!is.null(isolate(rv$active.sname))){
  shinyjs::enable("btSave")
}
shinyjs::enable("btSaveAs")
# set initialisation flags for handsontables to FALSE
if(any(hot.init)){
  hot.init[]     <<- FALSE
}