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
                                       csvDelim, col_types = cols())
               
             }
           })
    
  }, error = function(e) {
    stop(sprintf("Model output file: '%s' could not be read (model: '%s'). Error message: %s.", 
                 scalarsOutName %+% ".csv", modelName, e), call. = FALSE)
  })
  scalarTmp[is.na(scalarTmp)] <- 0L
  scalarTmp <- fixColTypes(scalarTmp,  "ccc")
  #set names of scalar sheet to scalar headers
  if(validateHeaders(names(scalarTmp), scalarsFileHeaders)){
    names(scalarTmp) <- scalarsFileHeaders
  }else{
    flog.warn("Dataset: '%s' has invalid headers ('%s'). Headers should be: '%s'.", 
              modelOutAlias[i], paste(names(scalarTmp), collapse = "', '"), 
              paste(scalarsFileHeaders, collapse = "', '"))
    stop(sprintf(lang$errMsg$GAMSOutput$badOutputData, modelOutAlias[i]), call. = FALSE)
  }
  
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
                                                   csvDelim, col_types = cols())
                 }else{
                   stop(sprintf("Data for output dataset: '%s' could not be found.", names(modelOut)[[i]]), call. = FALSE)
                 }
               })
      }, error = function(e) {
        stop(sprintf("Model output file: '%s' could not be read (model: '%s'). Error message: %s.", 
                     names(modelOut)[[i]] %+% ".csv", modelName, e), call. = FALSE)
      })
      ret$tabular[[i]][is.na(ret$tabular[[i]])] <<- 0L
      ret$tabular[[i]] <<- fixColTypes(ret$tabular[[i]],  modelOut[[i]]$colTypes)
      
      if(validateHeaders(names(ret$tabular[[i]]), names(modelOut[[i]]$headers))){
        names(ret$tabular[[i]]) <- names(modelOut[[i]]$headers)
      }else{
        flog.warn("Dataset: '%s' has invalid headers ('%s'). Headers should be: '%s'.", 
                  modelOutAlias[i], paste(names(ret$tabular[[i]]), collapse = "', '"), 
                  paste(names(modelOut[[i]]$headers), collapse = "', '"))
        stop(sprintf(lang$errMsg$GAMSOutput$badOutputData, modelOutAlias[i]), call. = FALSE)
      }
    }
  })
  
  # if scalar dataset is nonempty, assign it to return value
  if(!is.null(scalarTmp) && nrow(scalarTmp)){
    ret$scalar <- scalarTmp
  }

  return(ret)
}