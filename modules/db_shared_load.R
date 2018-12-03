# import datasets that should be loaded automatically on startup
datasetsToFetch <- names(modelIn)[vapply(seq_along(modelIn), function(i){return(identical(sharedData[i], TRUE))}, logical(1L), USE.NAMES = FALSE)]

if(!is.null(datasetsToFetch)){
  errMsg <- NULL
  lapply(datasetsToFetch, function(dataset){
    i <- match(dataset, names(modelIn))
    tabNameShared <- paste0(sharedTablePrefix, "_", dataset)
    keyCol <- character(0L)
    if(length(colSubset[[i]])){
      keyCol <- colSubset[[i]][[1]]
    }
    # load from database
    tryCatch({
      sharedInputData[[i]] <<- auth$importShared(tableName = tabNameShared, keyCol = keyCol)
    }, error = function(e) {
      flog.error("Problems fetching shared dataset from table: '%s'. Error message: %s.", tabNameShared, e)
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
             
             observe({
               if(!identical(length(input[["dropdown_" %+% i]]), 1L) || identical(nchar(input[["dropdown_" %+% i]]), 0L)){
                 return()
               }
               sharedInputData_filtered[[i]] <<- sharedInputData[[i]][sharedInputData[[i]][[colSubset[[i]][1]]] == input[["dropdown_" %+% i]],]
             }, priority = 1e6)
             
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