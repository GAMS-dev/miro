# import datasets that should be loaded automatically on startup
datasetsToFetch <- names(modelIn)[vapply(seq_along(modelIn), function(i){return(identical(sharedData[i], TRUE))}, logical(1), USE.NAMES = FALSE)]

if(!is.null(datasetsToFetch)){
  errMsg <- NULL
  lapply(datasetsToFetch, function(dataset){
    i <- match(dataset, names(modelIn))
    table.name <- paste0(sharedTablePrefix, "_", dataset)
    # load from database
    tryCatch({
      sharedInputData[[i]] <<- auth$importShared(tableName = table.name)
    }, error = function(e) {
      flog.error("Problems fetching shared dataset from table: '%s'. Error message: %s.", table.name, e)
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$desc, dataset), sep = "\n")
    })
    if(!is.null(errMsg)){
      return(NULL)
    }
    if(!length(sharedInputData[[i]])){
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$noData, modelInAlias[i]), sep = "\n")
      return(NULL)
    }
    switch(modelIn[[i]]$type,
           dropdown = {
             if(!is.null(colSubset[[i]])){
               subset.idx <- match(tolower(colSubset[[i]]), tolower(names(sharedInputData[[i]])))
               if(any(is.na(subset.idx))){
                 errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$badColName, paste(colSubset[[i]][is.na(subset.idx)], collapse = ","), dataset), sep = "\n")
                 return(NULL)
               }
               choices <- sharedInputData[[i]][colSubset[[i]]]
             }else{
               choices <- sharedInputData[[i]]
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
             updateSelectInput(session, "dropdown_" %+% i, choices = choices, selected = modelIn[[i]]$dropdown$selected)
             
           },
           hot = {
             tryCatch({
               modelInputData[[i]] <<- sharedInputData[[i]][, names(modelIn[[i]]$headers), drop = FALSE]
             }, error = function(e){
               flog.error("The shared dataset: '%s' could not be loaded. Error message: %s.", modelInAlias[i], e)
               errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$desc, modelInAlias[i]), sep = "\n")
               return(NULL)
             })
             if(!is.null(errMsg)){
               return(NULL)
             }
             
             if(length(isolate(rv[[paste0("in_", i)]]))){
               rv[[paste0("in_", i)]] <<- isolate(rv[[paste0("in_", i)]]) + 1
             }else{
               rv[[paste0("in_", i)]] <<- 1
             }
           }
    )
  })
  showErrorMsg(lang$errMsg$fetchDataset$title, errMsg)
}