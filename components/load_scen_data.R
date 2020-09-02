loadScenData <- function(scalarsName, metaData, workDir, modelName, scalarsFileHeaders,
                         templates, errMsg = NULL, method = "csv", csvDelim = ",", 
                         hiddenOutputScalars = character(0L),
                         fileName = character(0L), DDPar = character(0L), GMSOpt = character(0L)){
  ret <- list(tabular = NULL, scalar = NULL)
  
  if(identical(method, "xls")){
    xlsPath <- nativeFileEnc(file.path(workDir, fileName))
    if(!file.exists(xlsPath)){
      stop(sprintf("File: '%s' could not be found.", xlsPath), call. = FALSE)
    }
    xlsSheetNames <- tolower(excel_sheets(xlsPath))
    xlsSheetNames <- vapply(strsplit(xlsSheetNames, " ", fixed = TRUE), 
                            "[[", character(1L), 1L)
    if(scalarsName %in% names(metaData)){
      scalarXlsSheetIds <- match(metaData[[scalarsName]]$symnames, xlsSheetNames)
      scalarXlsSheetIds <- scalarXlsSheetIds[!is.na(scalarXlsSheetIds)]
      if(length(scalarXlsSheetIds)){
        tryCatch({
          scalarDataTmp <- vapply(scalarXlsSheetIds, function(sheetID){
            data <- suppressMessages(read_excel(xlsPath, sheetID, col_names = FALSE))
            if(length(data) != 1L){
              stop(sprintf("Invalid length of scalar sheet: %s.", sheetID), call. = FALSE)
            }
            return(as.character(data[[1]][1]))
          }, character(1L), USE.NAMES = FALSE)
        }, error = function(e){
          flog.info("Could not read one or more scalar sheets of spreadsheet. Please make sure to only specify the scalar's value in A1.")
          stop("Could not read scalar sheets.", call. = FALSE)
        })
        scalarIds <- match(xlsSheetNames[scalarXlsSheetIds], metaData[[scalarsName]]$symnames)
        ret$scalar <- as_tibble(list(name  = metaData[[scalarsName]]$symnames[scalarIds],
                                     text  = metaData[[scalarsName]]$symtext[scalarIds],
                                     value = scalarDataTmp))
      }
    }
  }else if(identical(method, "gdx")){
    gdxPath <- file.path(workDir, fileName)
    if(!file.exists(gdxPath)){
      stop(sprintf("File: '%s' could not be found.", gdxPath), call. = FALSE)
    }
  }else if(!identical(method, "csv")){
    stop(sprintf("Method ('%s') is not suported for loading data.", method), call. = FALSE)
  }
  if(!length(errMsg)){
    errMsg <- "Dataset: '%s' is not valid. Please check the format of the data you wish to import."
  }
  # read scalar data in case it exists
  isNewGdx <- TRUE
  tryCatch({
    switch(method,
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
             sheetID <- match(scalarsName, xlsSheetNames)[[1]]
             if(!is.na(sheetID)){
               scalarTmp <- suppressMessages(
                 read_excel(xlsPath, sheetID, 
                            col_types = c("text", "text", "text"),
                            col_names = TRUE))
               if(length(ret$scalar)){
                 ret$scalar <- bind_rows(scalarTmp, ret$scalar)
               }else{
                 ret$scalar <- scalarTmp
               }
             }
           },
           gdx = {
             if(scalarsName %in% names(metaData)){
               ret$scalar <- gdxio$rgdx(gdxPath, scalarsName, isNewGdx = isNewGdx)
               isNewGdx <- FALSE
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
        }, error = function(e){
          stop(sprintf("Problems removing hidden rows from scalar dataframe. Error message: %s.", e), call. = FALSE)
        })
      }else{
        ret$tabular[[i]] <<- templates[[i]]
      }
      if(!length(ret$tabular[[i]])){
        ret$tabular[[i]] <<- templates[[i]]
      }
    }else{
      tryCatch({
        switch(method,
               csv = {
                 if(file.exists(file.path(workDir, names(metaData)[[i]] %+% '.csv'))){
                   ret$tabular[[i]] <<- read_delim(file.path(workDir, names(metaData)[[i]] %+% '.csv'), 
                                                   csvDelim, col_types = metaData[[i]]$colTypes, 
                                                   col_names = TRUE)
                 }else{
                   ret$tabular[[i]] <<- templates[[i]]
                   return()
                 }
               },
               xls = {
                 sheetID <- match(names(metaData)[[i]], xlsSheetNames)[[1]]
                 if(!is.na(sheetID)){
                   headerTypes <- metaData[[i]]$colTypes
                   ret$tabular[[i]] <<- suppressMessages(
                     read_excel(xlsPath, sheetID, 
                                col_types = 
                                  vapply(seq_along(metaData[[i]]$headers), 
                                         function(colId){
                                           if(identical(substr(headerTypes, colId, colId), "c"))
                                             return("text")
                                           return("numeric")
                                         }, character(1L), USE.NAMES = FALSE), 
                                col_names = TRUE))
                 }else{
                   ret$tabular[[i]] <<- templates[[i]]
                   return()
                 }
               },
               gdx = {
                 tryCatch({
                   ret$tabular[[i]] <<- gdxio$rgdx(gdxPath, names(metaData)[[i]], 
                                                   names = names(metaData[[i]]$headers),
                                                   isNewGdx = isNewGdx)
                   if(!inherits(ret$tabular[[i]], "data.frame")){
                     ret$tabular[[i]] <<- ddToTibble(ret$tabular[[i]], metaData[[i]])
                   }
                 }, error = function(e){
                   if(grepl("Compression library not found", 
                            conditionMessage(e), 
                            fixed = TRUE)){
                     stop("Compressed GDX is not supported. Please remove the GDXCOMPRESS environment variable.")
                   }
                   ret$tabular[[i]] <<- templates[[i]]
                 })
                 if(!length(ret$tabular[[i]])){
                   ret$tabular[[i]] <<- templates[[i]]
                 }
               })
      }, error = function(e) {
        stop(sprintf("Model file: '%s' could not be read. Error message: %s", 
                     names(metaData)[[i]], e), call. = FALSE)
      })
      if(!identical(length(ret$tabular[[i]]), length(metaData[[i]]$headers))){
        flog.warn("Invalid data attempted to be read (number of headers of table does not match io_config schema).")
        stop(sprintf(errMsg, names(metaData)[i]), call. = FALSE)
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
  
  return(ret)
}