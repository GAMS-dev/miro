loadGAMSResults <- function(scalarsOutName, modelOut, workDir, modelName, errMsg, scalarsFileHeaders, colTypes,
                            modelOutTemplate, method = "csv", csvDelim = ",", hiddenOutputScalars = character(0L)){
  
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
                                       csvDelim, col_types = cols(), 
                                       col_names = TRUE)
               
             }
           })
    
  }, error = function(e) {
    stop(sprintf("Model output file: '%s' could not be read (model: '%s'). Error message: %s.", 
                 scalarsOutName %+% ".csv", modelName, e), call. = FALSE)
  })
  if(length(scalarTmp)){
    if(!identical(length(scalarTmp), 3L)){
      flog.warn("Invalid scalar output data attempted to be read (number of headers of table does not match 3).")
      stop(sprintf(errMsg$badOutputData, scalarsOutName), call. = FALSE)
    }
    scalarTmp[is.na(scalarTmp)] <- 0L
    scalarTmp <- fixColTypes(scalarTmp,  "ccc")
    #set names of scalar sheet to scalar headers
    if(!hasValidHeaderTypes(scalarTmp, colTypes[[scalarsOutName]])){
      flog.warn("Dataset: '%s' has invalid header types ('%s'). Header types should be: '%s'.", 
                scalarsOutName, paste(vapply(scalarTmp, function(el) return(class(el)[[1L]]), 
                                                 character(1L), USE.NAMES = FALSE), collapse = "', '"), 
                colTypes[[scalarsOutName]])
      stop(sprintf(errMsg$badOutputData, scalarsOutName), call. = FALSE)
    }
    names(scalarTmp) <- scalarsFileHeaders
  }
  
  # fetch results from csv files
  lapply(seq_along(modelOut), function(i){
    if(identical(names(modelOut)[[i]], scalarsOutName)){
      if(!is.null(scalarTmp)){
        # scalars already imported
        tryCatch({
          # fetch only those scalar data that are not marked as hidden and remove the data fetched from scalar dataset
          removeRows       <- tolower(scalarTmp[[1]]) %in% hiddenOutputScalars
          ret$tabular[[i]] <<- scalarTmp[!removeRows, ]
          scalarTmp        <<- scalarTmp[removeRows, ]
        }, error = function(e){
          stop(sprintf("Problems removing hidden rows from scalar dataframe. Error message: %s.", e), call. = FALSE)
        })
      }else{
        # scalar output file does not exist, but is in modelOut vector
        stop(sprintf("Model output file: '%s' could not be read (model: '%s').", 
                     scalarsOutName %+% ".csv", modelName), call. = FALSE)
      }
    }else{
      tryCatch({
        switch(method,
               csv = {
                 if(file.exists(workDir %+% names(modelOut)[[i]] %+% '.csv')){
                   ret$tabular[[i]] <<- read_delim(workDir %+% names(modelOut)[[i]] %+% '.csv', 
                                                   csvDelim, col_types = cols(), 
                                                   col_names = TRUE)
                 }else{
                   ret$tabular[[i]] <<- modelOutTemplate[[i]]
                   return()
                 }
               })
      }, error = function(e) {
        stop(sprintf("Model output file: '%s' could not be read (model: '%s'). Error message: %s.", 
                     names(modelOut)[[i]] %+% ".csv", modelName, e), call. = FALSE)
      })
      if(!identical(length(ret$tabular[[i]]), length(modelOut[[i]]$headers))){
        flog.warn("Invalid output data attempted to be read (number of headers of table does not match GMSIO__config schema).")
        stop(sprintf(errMsg$badOutputData, names(modelOut)[i]), call. = FALSE)
      }
      ret$tabular[[i]] <<- fixColTypes(ret$tabular[[i]],  modelOut[[i]]$colTypes)
      ret$tabular[[i]][is.na(ret$tabular[[i]]) & vapply(ret$tabular[[i]], 
                                                        is.numeric, logical(1L), 
                                                        USE.NAMES = FALSE)] <<- 0L
      ret$tabular[[i]][is.na(ret$tabular[[i]])] <<- ""
      
      names(ret$tabular[[i]]) <<- names(modelOut[[i]]$headers)
      if(!hasValidHeaderTypes(ret$tabular[[i]], colTypes[[names(modelOut)[i]]])){
        flog.warn("Dataset: '%s' has invalid header types ('%s'). Header types should be: '%s'.", 
                  names(modelOut)[i], paste(vapply(ret$tabular[[i]], function(el) return(class(el)[[1L]]), 
                                                   character(1L), USE.NAMES = FALSE), collapse = "', '"), 
                  colTypes[[names(modelOut)[i]]])
        stop(sprintf(errMsg$badOutputData, names(modelOut)[i]), call. = FALSE)
      }
    }
    attr(ret$tabular[[i]], "aliases") <<- attr(modelOutTemplate[[i]], "aliases")
  })
  # if scalar dataset is nonempty, assign it to return value
  if(!is.null(scalarTmp) && nrow(scalarTmp)){
    ret$scalar <- scalarTmp
  }

  return(ret)
}