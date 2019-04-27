loadScenData <- function(scalarsName, metaData, workDir, modelName, errMsg, scalarsFileHeaders,
                         templates, method = "csv", csvDelim = ",", hiddenOutputScalars = character(0L),
                         fileName = character(0L)){
  if(identical(method, "xls")){
    xlsPath <- file.path(workDir, fileName)
    if(!file.exists(xlsPath)){
      stop(sprintf("File: '%s' could not be found."), xlsPath, call. = FALSE)
    }
    xlsSheetNames <- tolower(excel_sheets(xlsPath))
  }else if(identical(method, "gdx")){
    gdxPath <- file.path(workDir, fileName)
    if(!file.exists(gdxPath)){
      stop(sprintf("File: '%s' could not be found."), gdxPath, call. = FALSE)
    }
    xlsSheetNames <- tolower(excel_sheets(xlsPath))
  }else if(!identical(method, "csv")){
    stop(sprintf("Method ('%s') is not suported for loading data.", method), call. = FALSE)
  }
  ret         <- list(tabular = NULL, scalar = NULL)
  # read scalar data in case it exists
  tryCatch({
    switch(method,
           csv = {
             if(file.exists(workDir %+% scalarsName %+% '.csv')){
               ret$scalar <- read_delim(workDir %+% scalarsName %+% '.csv', 
                                       csvDelim, col_types = cols(), 
                                       col_names = TRUE)
               
             }
           },
           xls = {
             sheetID <- match(scalarsName, xlsSheetNames)[[1]]
             if(!is.na(sheetID)){
               ret$scalar <- read_excel(xlsPath, sheetID, 
                                       col_types = c("text", "text", "text"))
               
             }
           })
    
  }, error = function(e) {
    stop(sprintf("File: '%s' could not be read (model: '%s'). Error message: %s.", 
                 scalarsName %+% ".csv", modelName, e), call. = FALSE)
  })
  if(length(ret$scalar)){
    if(!identical(length(ret$scalar), 3L)){
      flog.warn("Invalid scalar data attempted to be read (number of headers of table does not match 3).")
      stop(sprintf(errMsg, scalarsName), call. = FALSE)
    }
    ret$scalar <- fixColTypes(ret$scalar,  "ccc")
    ret$scalar[is.na(ret$scalar)] <- ""
    #set names of scalar sheet to scalar headers
    if(!hasValidHeaderTypes(ret$scalar, "ccc")){
      flog.warn("Dataset: '%s' has invalid header types ('%s'). Header types should be: 'ccc'.", 
                scalarsName, paste(vapply(ret$scalar, function(el) return(class(el)[[1L]]), 
                                          character(1L), USE.NAMES = FALSE), collapse = "', '"))
      stop(sprintf(errMsg, scalarsName), call. = FALSE)
    }
    names(ret$scalar) <- scalarsFileHeaders
  }
  # fetch results from csv files
  lapply(seq_along(metaData), function(i){
    if(identical(names(metaData)[[i]], scalarsName)){
      if(!is.null(ret$scalar)){
        # scalars already imported
        tryCatch({
          # fetch only those scalar data that are not marked as hidden and remove the data fetched from scalar dataset
          removeRows       <- tolower(ret$scalar[[1]]) %in% hiddenOutputScalars
          ret$tabular[[i]] <<- ret$scalar[!removeRows, ]
        }, error = function(e){
          stop(sprintf("Problems removing hidden rows from scalar dataframe. Error message: %s.", e), call. = FALSE)
        })
      }else{
        # scalar output file does not exist, but is in metaData vector
        stop(sprintf("Model output file: '%s' could not be read (model: '%s').", 
                     scalarsName %+% ".csv", modelName), call. = FALSE)
      }
    }else{
      tryCatch({
        switch(method,
               csv = {
                 if(file.exists(workDir %+% names(metaData)[[i]] %+% '.csv')){
                   ret$tabular[[i]] <<- read_delim(workDir %+% names(metaData)[[i]] %+% '.csv', 
                                                   csvDelim, col_types = cols(), 
                                                   col_names = TRUE)
                 }else{
                   ret$tabular[[i]] <<- templates[[i]]
                   return()
                 }
               },
               xls = {
                 sheetID <- match(names(metaData)[[i]], xlsSheetNames)[[1]]
                 if(!is.na(sheetID)){
                   ret$tabular[[i]] <<- read_excel(xlsPath, sheetID,
                                                   col_names = TRUE)
                 }else{
                   ret$tabular[[i]] <<- templates[[i]]
                   return()
                 }
               })
      }, error = function(e) {
        stop(sprintf("Model file: '%s' could not be read (model: '%s'). Error message: %s", 
                     names(metaData)[[i]] %+% ".csv", modelName, e), call. = FALSE)
      })
      if(!identical(length(ret$tabular[[i]]), length(metaData[[i]]$headers))){
        flog.warn("Invalid data attempted to be read (number of headers of table does not match GMSIO__config schema).")
        stop(sprintf(errMsg, names(metaData)[i]), call. = FALSE)
      }
      ret$tabular[[i]] <<- fixColTypes(ret$tabular[[i]],  metaData[[i]]$colTypes)
      ret$tabular[[i]] <<- ret$tabular[[i]] %>% mutate_if(is.numeric , 
                                                          replace_na, replace = 0) %>% 
        replace(is.na(.), "")
      names(ret$tabular[[i]]) <<- names(metaData[[i]]$headers)
      if(!hasValidHeaderTypes(ret$tabular[[i]], metaData[[i]]$colTypes)){
        flog.warn("Dataset: '%s' has invalid header types ('%s'). Header types should be: '%s'.", 
                  names(metaData)[i], paste(vapply(ret$tabular[[i]], function(el) return(class(el)[[1L]]), 
                                                   character(1L), USE.NAMES = FALSE), collapse = "', '"), 
                  metaData[[i]]$colTypes)
        stop(sprintf(errMsg, names(metaData)[i]), call. = FALSE)
      }
    }
    attr(ret$tabular[[i]], "aliases") <<- attr(templates[[i]], "aliases")
  })
  
  return(ret)
}