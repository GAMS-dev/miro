loadScenData <- function(scalarsName, metaData, workDir, modelName, scalarsFileHeaders,
                         templates, errMsg = NULL, method = "csv", csvDelim = ",", hiddenOutputScalars = character(0L),
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
  }else if(!identical(method, "csv")){
    stop(sprintf("Method ('%s') is not suported for loading data.", method), call. = FALSE)
  }
  if(!length(errMsg)){
    errMsg <- "Dataset: '%s' is not valid. Please check the format of the data you wish to import."
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
           },
           gdx = {
             if(scalarsName %in% names(metaData)){
               ret$scalar <- tibble(metaData[[scalarsName]]$symnames, metaData[[scalarsName]]$symtext, 
                                    vapply(seq_along(metaData[[scalarsName]]$symnames), function(i){
                                      if(identical(metaData[[scalarsName]]$symtypes[[i]], "parameter")){
                                        scalar <- NA_character_
                                        tryCatch({
                                          scalar <- as.character(rgdx.scalar(gdxPath, 
                                                                             metaData[[scalarsName]]$symnames[[i]])[[1]])
                                        }, error = function(e){
                                          flog.warn("Scalar: '%s' could not be found in gdx container.", 
                                                    metaData[[scalarsName]]$symnames[[i]])
                                        })
                                        return(scalar)
                                      }else{
                                        scalar <- NA_character_
                                        tryCatch({
                                          scalar <- rgdx.set(gdxPath, metaData[[scalarsName]]$symnames[[i]])[[1]][1]
                                        }, error = function(e){
                                          flog.warn("Singleton set: '%s' could not be found in gdx container.", 
                                                    metaData[[scalarsName]]$symnames[[i]])
                                        })
                                        return(scalar)
                                      }
                                    }, character(1L), USE.NAMES = FALSE))
             }
           })
    
  }, error = function(e) {
    stop(sprintf("File: '%s' could not be read (model: '%s'). Error message: %s.", 
                 scalarsName, modelName, e), call. = FALSE)
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
        ret$tabular[[i]] <<- templates[[i]]
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
               },
               gdx = {
                 if(identical(attr(templates[[i]], "type"), "parameter")){
                   tryCatch({
                     ret$tabular[[i]] <<- as_tibble(rgdx.param(gdxPath, names(metaData)[[i]], 
                                                               check.names = FALSE)) %>% 
                       mutate_if(is.factor, as.character)
                     if(nchar(metaData[[i]]$colTypes) - nchar(gsub("d", "", metaData[[i]]$colTypes)) > 1L){
                       # pivot symbol
                       ret$tabular[[i]] <<- spread(ret$tabular[[i]], !!length(ret$tabular[[i]]) - 1, 
                                                   !!length(ret$tabular[[i]]))
                       newIdx <- match(names(metaData[[i]]$headers), tolower(names(ret$tabular[[i]])))
                       if(any(is.na(newIdx))){
                         flog.error("Dataset: '%s' has invalid headers. Headers are: '%s'. Headers should be: '%s'.", 
                                    names(metaData)[[i]], paste(names(ret$tabular[[i]]), collapse = "', '"), 
                         paste(names(metaData[[i]]$headers), collapse = "', '"))
                         stop(sprintf(errMsg, names(metaData)[i]), call. = FALSE)
                       }else{
                         ret$tabular[[i]] <<- ret$tabular[[i]][, newIdx]
                       }
                     }
                   }, error = function(e){
                     ret$tabular[[i]] <<- templates[[i]]
                   })
                 }else{
                   tryCatch({
                     ret$tabular[[i]] <<- as_tibble(rgdx.set(gdxPath, names(metaData)[[i]], 
                                                             check.names = FALSE)) %>%
                       mutate_if(is.factor, as.character)
                   }, error = function(e){
                     ret$tabular[[i]] <<- templates[[i]]
                   })
                 }
               })
      }, error = function(e) {
        stop(sprintf("Model file: '%s' could not be read (model: '%s'). Error message: %s", 
                     names(metaData)[[i]], modelName, e), call. = FALSE)
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