# import datasets that should be loaded automatically on startup
if(length(externalInputConfig)){
  errMsg <- NULL
  lapply(seq_along(externalInputConfig), function(extIdx){
    item <- externalInputConfig[[extIdx]]
    if(!length(item)){
      return()
    }
    item$name <- names(externalInputConfig)[extIdx]
    i <- match(item$name, names(modelIn))
    
    # load from database
    tryCatch({
      externalInputData[[i]] <<- dataio$import(item)
    }, error = function(e) {
      flog.error("Problems fetching external data. Error message: %s.", e)
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$desc, item$name), sep = "\n")
    })
    if(!is.null(errMsg)){
      return(NULL)
    }
    if(!length(externalInputData[[i]])){
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$noData, modelInAlias[i]), sep = "\n")
      return(NULL)
    }
    switch(modelIn[[i]]$type,
           dropdown = {
             if(!is.null(item$colSubset)){
               subsetIdx <- match(tolower(item$colSubset), tolower(names(externalInputData[[i]])))
               if(any(is.na(subsetIdx))){
                 errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$badColName, 
                                                  paste(item$colSubset[is.na(subsetIdx)], collapse = ","), 
                                                  item$name), sep = "\n")
                 return(NULL)
               }
               choices <- externalInputData[[i]][item$colSubset]
             }else{
               choices <- externalInputData[[i]]
             }
             if(!length(choices)){
               errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$noData, modelInAlias[i]), sep = "\n")
               return(NULL)
             }else if(length(choices) > 1){
               # set aliases
               choices <- setNames(choices[[1]], choices[[2]])
             }else{
               # only 1 column so no aliases
               choices <- choices[[1]]
             }
             updateSelectInput(session, "dropdown_" %+% i, 
                               choices = choices, 
                               selected = modelIn[[i]]$dropdown$selected)
             
             observe({
               if(!identical(length(input[["dropdown_" %+% i]]), 1L) || 
                  identical(nchar(input[["dropdown_" %+% i]]), 0L)){
                 return()
               }
               externalInputData_filtered[[i]] <<- externalInputData[[i]][externalInputData[[i]][[item$colSubset[[1]]]] == input[["dropdown_" %+% i]],]
             }, priority = 1e6)
             
           },
           hot =,
           dt = {
             modelInputData[[i]] <<- externalInputData[[i]]
             
             if(length(isolate(rv[[paste0("in_", i)]]))){
               rv[[paste0("in_", i)]] <<- isolate(rv[[paste0("in_", i)]]) + 1L
             }else{
               rv[[paste0("in_", i)]] <<- 1L
             }
           },
           {
             flog.error("Input type: '%s' is not supported for share datesets.", modelIn[[i]]$type)
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$desc, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
    )
  })
  showErrorMsg(lang$errMsg$fetchDataset$title, errMsg)
}