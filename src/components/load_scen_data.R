loadScenData <- function(scalarsName, metaData, workDir, modelName, scalarsFileHeaders,
                         templates, errMsg = NULL, method = "csv", csvDelim = ",", 
                         hiddenOutputScalars = character(0L),
                         fileName = character(0L), DDPar = character(0L), GMSOpt = character(0L),
                         dfClArgs = NULL, xlsio = NULL, csvio = NULL){
  ret <- list(tabular = NULL, scalar = NULL, noTabularData = TRUE, errors = character())
  loadDataErrors <- CharArray$new()
  if(length(method) == 1L && method %in% c("xls", "gdx", "scsv")){
    if(identical(method, "scsv") && is.null(csvio)){
      return(ret)
    }
    dataFilePath <- file.path(workDir, fileName)
    if(!file.exists(dataFilePath)){
      stop(sprintf("File: '%s' could not be found.", dataFilePath), call. = FALSE)
    }
  }else if(!identical(method, "csv")){
    stop("Method is not suported for loading data.", call. = FALSE)
  }
  if(!length(errMsg)){
    errMsg <- "Dataset: '%s' is not valid. Please check the format of the data you wish to import."
  }
  # read scalar data in case it exists
  isNewGdx <- TRUE
  tryCatch({
    switch(method,
           scsv = {
             tryCatch({
               ret$scalar <- csvio$read(dataFilePath, scalarsName)
             }, error_notfound = function(e){
               return(TRUE)
             })
           },
           csv = {
             if(file.exists(file.path(workDir, scalarsName %+% '.csv'))){
               ret$scalar <- read_delim(file.path(workDir, scalarsName %+% '.csv'), 
                                        csvDelim, col_types = "ccc", 
                                        col_names = TRUE)
             }
             if(length(c(DDPar, GMSOpt)) > 0 &&
                (length(ret$scalar) == 0L || length(ret$scalar) == 3L) &&
                file.exists(file.path(workDir, modelName %+% '.pf'))){
               pfFileContent <- loadPfFileContent(read_lines(
                 file.path(workDir, modelName %+% '.pf')), GMSOpt, DDPar)
               if(length(pfFileContent) == 3L){
                 if(length(ret$scalar)){
                   names(pfFileContent) <- names(ret$scalar)
                 }
                 ret$scalar <- bind_rows(ret$scalar, pfFileContent)
               }
             }
           },
           xls = {
             if(scalarsName %in% names(metaData)){
               ret$scalar <- xlsio$read(dataFilePath, scalarsName)
             }
           },
           gdx = {
             if(scalarsName %in% names(metaData)){
               ret$scalar <- gdxio$rgdx(dataFilePath, scalarsName, isNewGdx = isNewGdx)
               isNewGdx <- FALSE
             }
           })
    
  }, error = function(e) {
    stop(sprintf("File: '%s' could not be read (model: '%s'). Error message: %s.", 
                 scalarsName, modelName, e), call. = FALSE)
  })
  if(length(dfClArgs) == 3L){
    ret$scalar <- dplyr::bind_rows(ret$scalar, setNames(dfClArgs, names(ret$scalar)))
  }
  if(length(ret$scalar)){
    if(!identical(length(ret$scalar), 3L)){
      flog.warn("Invalid scalar data attempted to be read (number of headers of table does not match 3).")
      stop(sprintf(errMsg, scalarsName), call. = FALSE)
    }
    if(all(is.na(ret$scalar[[3]]))){
      ret$scalar <- NULL
    }else{
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
  }
  # fetch results from csv files
  lapply(seq_along(metaData), function(i){
    if(identical(names(metaData)[[i]], scalarsName)){
      if(length(ret$scalar)){
        # scalars already imported
        tryCatch({
          # fetch only those scalar data that are not marked as hidden and remove the data fetched from scalar dataset
          removeRows       <- tolower(ret$scalar[[1]]) %in% tolower(hiddenOutputScalars)
          ret$tabular[[i]] <<- ret$scalar[!removeRows, ]
          if(!length(ret$tabular[[i]])){
            ret$tabular[[i]] <<- templates[[i]]
          }else if(ret$noTabularData){
            ret$noTabularData <<- FALSE
          }
        }, error = function(e){
          stop(sprintf("Problems removing hidden rows from scalar dataframe. Error message: %s.", e), call. = FALSE)
        })
      }else{
        ret$tabular[[i]] <<- templates[[i]]
      }
    }else{
      tryCatch({
        switch(method,
               scsv = {
                 if(tryCatch({
                   ret$tabular[[i]] <<- csvio$read(dataFilePath, names(metaData)[[i]])
                   if(ret$noTabularData){
                     ret$noTabularData <<- FALSE
                   }
                   FALSE
                 }, error_notfound = function(e){
                   ret$tabular[[i]] <<- templates[[i]]
                   return(TRUE)
                 })){
                   return()
                 }
               },
               csv = {
                 if(file.exists(file.path(workDir, names(metaData)[[i]] %+% '.csv'))){
                   ret$tabular[[i]] <<- read_delim(file.path(workDir, names(metaData)[[i]] %+% '.csv'), 
                                                   csvDelim, col_types = metaData[[i]]$colTypes, 
                                                   col_names = TRUE)
                   if(ret$noTabularData){
                     ret$noTabularData <<- FALSE
                   }
                 }else{
                   ret$tabular[[i]] <<- templates[[i]]
                   return()
                 }
               },
               xls = {
                 if(tryCatch({
                   ret$tabular[[i]] <<- xlsio$read(dataFilePath, names(metaData)[[i]])
                   if(ret$noTabularData){
                     ret$noTabularData <<- FALSE
                   }
                   FALSE
                 }, error_notfound = function(e){
                   flog.debug("Symbol: %s not found in Excel spreadsheet", names(metaData)[[i]])
                   ret$tabular[[i]] <<- templates[[i]]
                   return(TRUE)
                 }, error_parse_config = function(e){
                   flog.info("Symbol: %s : Problems parsing index from Excel file. Error message: %s",
                              names(metaData)[[i]], conditionMessage(e))
                   loadDataErrors$push(conditionMessage(e))
                   ret$tabular[[i]] <<- templates[[i]]
                   return(TRUE)
                }, error_data = function(e){
                  flog.info("Symbol: %s : Problems reading data. Error message: %s",
                             names(metaData)[[i]], conditionMessage(e))
                  loadDataErrors$push(conditionMessage(e))
                  ret$tabular[[i]] <<- templates[[i]]
                  return(TRUE)
                })){
                   return()
                 }
               },
               gdx = {
                 tryCatch({
                   ret$tabular[[i]] <<- gdxio$rgdx(dataFilePath, names(metaData)[[i]], 
                                                   names = names(metaData[[i]]$headers),
                                                   isNewGdx = isNewGdx)
                   if(!inherits(ret$tabular[[i]], "data.frame")){
                     ret$tabular[[i]] <<- ddToTibble(ret$tabular[[i]], metaData[[i]])
                   }
                   if(!length(ret$tabular[[i]])){
                     ret$tabular[[i]] <<- templates[[i]]
                   }else if(ret$noTabularData){
                     ret$noTabularData <<- FALSE
                   }
                 }, error = function(e){
                   if(grepl("Compression library not found", 
                            conditionMessage(e), 
                            fixed = TRUE)){
                     stop("Compressed GDX is not supported. Please remove the GDXCOMPRESS environment variable.")
                   }
                   ret$tabular[[i]] <<- templates[[i]]
                 })
               })
      }, error = function(e) {
        stop(sprintf("Model file: '%s' could not be read. Error message: %s", 
                     names(metaData)[[i]], e), call. = FALSE)
      })
      if(!identical(length(ret$tabular[[i]]), length(metaData[[i]]$headers))){
        if(identical(metaData[[i]]$symtype, "set") &&
           length(metaData[[i]]$headers) == 1L &&
           length(ret$tabular[[i]]) == 2L){
          # multi-dropdown with single column
          ret$tabular[[i]] <<- ret$tabular[[i]][-2]
        }else{
          flog.warn("Invalid data attempted to be read (number of headers of table does not match io_config schema).")
          stop(sprintf(errMsg, names(metaData)[i]), call. = FALSE)
        }
      }
      ret$tabular[[i]] <<- fixColTypes(ret$tabular[[i]],  metaData[[i]]$colTypes)
      names(ret$tabular[[i]]) <<- names(metaData[[i]]$headers)
      ret$tabular[[i]] <<- ret$tabular[[i]] %>% mutate_if(is.character, 
                                                          replace_na, replace = "")
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
  ret$errors <- loadDataErrors$get()
  return(ret)
}