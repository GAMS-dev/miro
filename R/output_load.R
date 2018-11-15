loadGAMSResults <- function(scalarsOutName, modelOut, workDir, modelName, method = "csv", csvDelim = ",", hiddenMarker = "###"){
  
  if(!(method %in% c("csv"))){
    stop(sprintf("Method ('%s') is not suported for loading output data.", method), call. = FALSE)
  }
  
  ret         <- list(tabular = NULL, scalar = NULL)
  scalarTmp  <- NULL
  # read scalar data in case it exists
  tryCatch({
    switch(method,
           csv = {
             if(file.exists(workDir %+% scalarsOutName %+% '.csv')){
               scalarTmp <- read_delim(workDir %+% scalarsOutName %+% '.csv', 
                                       csvDelim, col_types = modelOut[[scalarsOutName]]$colTypes)
             }
           })
    
  }, error = function(e) {
    stop(sprintf("Model output file: '%s' could not be read (model: '%s'). Error message: %s.", 
                 scalarsOutName %+% ".csv", modelName, e), call. = FALSE)
  })
  
  # fetch results from csv files
  lapply(seq_along(modelOut), function(i){
    if(identical(names(modelOut)[[i]], scalarsOutName)){
      if(!is.null(scalarTmp)){
        # scalars already imported
        tryCatch({
          # fetch only those scalar data that are not marked as hidden and remove the data fetched from scalar dataset
          removeRows       <- grepl(hiddenMarker, scalarTmp[[2]])
          ret$tabular[[i]] <<- scalarTmp[!removeRows, ]
          scalarTmp        <<- scalarTmp[removeRows, ]
        }, error = function(e){
          stop(sprintf("Problems removing hidden rows from scalar dataframe. Error message: %s.", e), call. = FALSE)
        })
      }else{
        # scalar output file does not exist, but is in modelOut vector
        stop(sprintf("Model output file: '%s' could not be read (model: '%s'). Error message: %s.", 
                     scalarsOutName %+% ".csv", modelName, e), call. = FALSE)
      }
    }else{
      tryCatch({
        switch(method,
               csv = {
                 if(file.exists(workDir %+% names(modelOut)[[i]] %+% '.csv')){
                   ret$tabular[[i]] <<- read_delim(workDir %+% names(modelOut)[[i]] %+% '.csv', 
                                                   csvDelim, col_types = modelOut[[i]]$colTypes)
                 }else{
                   stop(sprintf("Data for output dataset: '%s' could not be found.", names(modelOut)[[i]]), call. = FALSE)
                 }
               })
      }, error = function(e) {
        stop(sprintf("Model output file: '%s' could not be read (model: '%s'). Error message: %s.", 
                     names(modelOut)[[i]] %+% ".csv", modelName, e), call. = FALSE)
      })
    }
  })
  
  # if scalar dataset is nonempty, assign it to return value
  if(!is.null(scalarTmp) && nrow(scalarTmp)){
    ret$scalar <- scalarTmp
  }

  return(ret)
}