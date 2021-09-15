XlsIO <- R6::R6Class("XlsIO", inherit = LocalFileIO, public = list(
  rInitFile = function(path){
    rFormat <- excel_format(nativeFileEnc(path))
    if(is.na(rFormat)){
      stop_custom("error_bad_format", lang$errMsg$xlsio$errors$badFormat, call. = FALSE)
    }
    private$warnings$reset()
    private$rpath <- nativeFileEnc(file.path(dirname(path), basename(path)))
    private$rIsXlsx <- identical(rFormat, "xlsx")
    private$rSheets <- excel_sheets(nativeFileEnc(path))
    private$rSheetsNoIndex <- vapply(strsplit(private$rSheets, " ", fixed = TRUE),
                                     "[[", character(1L), 1L, USE.NAMES = FALSE)
    private$rIndex <- list()
    private$sheetRefCount <- NULL
    private$cache <- NULL
    return(self)
  },
  readIndex = function(path, indexRange = NULL, forceInit = FALSE){
    path <- nativeFileEnc(path)
    if(forceInit || !identical(path, private$rpath)){
      self$rInitFile(path)
      private$initIndexFromFile(path, indexRange)
    }
    return(self)
  },
  getSymbolNames = function(){
    if(length(private$rIndex)){
      symNamesTmp <- names(private$rIndex)
      if(scalarsFileName %in% symNamesTmp){
        symNamesTmp <- c(symNamesTmp, private$scalars)
      }
      symNamesValid <- symNamesTmp %in% names(ioConfig$modelIn)
      if (any(!symNamesValid) && !scalarsFileName %in% symNamesTmp &&
          any(symNamesTmp[!symNamesValid] %in% private$scalars)) {
        symNamesTmp <- c(symNamesTmp[symNamesValid], scalarsFileName)
      } else {
        symNamesTmp <- symNamesTmp[symNamesValid]
      }
      return(symNamesTmp)
    }
    return(names(ioConfig$modelIn))
  },
  read = function(path, symName, indexRange = NULL, forceInit = FALSE){
    self$readIndex(path, indexRange, forceInit)
    if(length(private$rIndex) > 0L){
      if(symName %in% c(scalarsFileName, scalarsOutName)){
        return(private$readScalars(symName))
      }
      if(!symName %in% names(private$rIndex)){
        stop_custom("error_notfound", sprintf(lang$errMsg$xlsio$errors$symbolNotFound, symName), call. = FALSE)
      }
      if(symName %in% c(scalarEquationsName, scalarEquationsOutName)){
        return(private$readVEScalars(symName))
      }
      return(private$readFromIndex(symName))
    }
    if(symName %in% c(scalarsFileName, scalarsOutName)){
      return(private$readScalarsNoIndex(symName))
    }
    sheetId <- match(symName, tolower(private$rSheetsNoIndex))
    if(is.na(sheetId)){
      stop_custom("error_notfound", sprintf(lang$errMsg$xlsio$errors$symbolNotFound, symName), call. = FALSE)
    }
    return(private$readInternal(private$rpath, sheet = sheetId, col_names = TRUE))
  },
  getWarnings = function(){
    return(private$warnings$get())
  },
  getSheetNames = function(){
    return(private$rSheets)
  },
  write = function(path, dataToWrite, metaData = NULL, includeMetadataSheet = FALSE, includeEmptySheets = TRUE){
    if(!length(dataToWrite)){
      return(writexl::write_xlsx(tibble(), path))
    }

    wsNamesRaw <- names(dataToWrite)

    # remove empty datasets
    emptySheets <- vapply(dataToWrite, function(sheet) identical(nrow(sheet), 0L),
                          logical(1L), USE.NAMES = FALSE)
    if(isFALSE(includeEmptySheets)){
      dataToWrite[emptySheets] <- NULL
      wsNamesRaw <- wsNamesRaw[!emptySheets]
      emptySheets <- FALSE
    }
    if(!length(dataToWrite)){
      return(writexl::write_xlsx(tibble(), path))
    }

    wsNamesTmp <- wsNamesRaw

    isOutputSheet <- tolower(wsNamesTmp) %in% tolower(names(ioConfig$modelOut))
    if(any(isOutputSheet)){
      wsNamesTmp[isOutputSheet] <- paste0(wsNamesTmp[isOutputSheet],
                                          lang$nav$excelExport$outputSuffix)
    }
    if(any(!isOutputSheet)){
      wsNamesTmp[!isOutputSheet] <- paste0(wsNamesTmp[!isOutputSheet],
                                           lang$nav$excelExport$inputSuffix)
    }
    # need to make sure worksheet names do not exceed 31 characters as Excel does not support longer names
    if(any(nchar(wsNamesTmp) > 31)){
      wsNameExceedsLength <- nchar(wsNamesTmp) > 31
      wsNamesTmp[wsNameExceedsLength] <- paste0(substr(wsNamesTmp[wsNameExceedsLength], 1, 29), "..")
      if(any(duplicated(wsNamesTmp))){
        wsNameDuplicated <- duplicated(wsNamesTmp)
        wsNamesTmp <- lapply(seq_along(wsNamesTmp), function(wsID){
          if(wsNameDuplicated[wsID]){
            return(paste0(substr(wsNamesTmp[wsID], 1, 29), wsID))
          }
          return(wsNamesTmp[wsID])
        })
      }
    }
    names(dataToWrite) <- wsNamesTmp

    dataToWrite <- c(dataToWrite,
                     setNames(list(private$genIndexFromMetadata(wsNamesRaw[!emptySheets],
                                                                names(dataToWrite)[!emptySheets],
                                                                dataToWrite)),
                              "_index"))
    # include metadata sheet in Excel file
    if(isTRUE(includeMetadataSheet) && !is.null(metaData)){
      metaColIds <- match(c("_uid", "_sname", "_stime", "_stag",
                            "_accessr", "_accessw", "_accessx"), names(metaData))
      metaData <- metaData[, metaColIds[!is.na(metaColIds)], drop = FALSE]
      names(metaData) <- unlist(lang$nav$excelExport$metadataSheet[c("uid", "sname", "stime",
                                                                     "stag", "readPerm", "writePerm",
                                                                     "execPerm")[seq_along(metaData)]],
                                use.names = FALSE)
      metaData[[" "]] <- ""
      metaData[["  "]] <- ""
      symDesc <- bind_rows(lapply(seq_along(wsNamesRaw), function(idx){
        tibble(description = private$metadata[[wsNamesRaw[idx]]]$alias,
               `   ` = if(isOutputSheet[idx]) lang$nav$excelExport$outputSuffix else lang$nav$excelExport$inputSuffix)
      }))
      symDesc <- add_column(symDesc, symbol = xl_hyperlink(paste0("#'",
                                                                  names(dataToWrite)[seq_along(wsNamesRaw)], "'!A1"),
                                                           name = wsNamesRaw),
                            .before = 1L)
      names(symDesc)[1:2] <- c(lang$nav$excelExport$metadataSheet$symbol,
                               lang$nav$excelExport$metadataSheet$desc)
      metaData <- crossing(metaData, symDesc)
      metaData[seq(2, nrow(metaData)), seq_len(length(metaData) - 3L)] <- NA
      dataToWrite <- c(setNames(list(metaData),
                                paste0(" ", lang$nav$excelExport$metadataSheet$title)), dataToWrite)
    }
    return(writexl::write_xlsx(dataToWrite, path))
  },
  getValidExtensions = function(){
    return(c("xls", "xlsx", "xlsm"))
  }),
  private = list(
    rpath = character(0L),
    rSheets = character(0L),
    rSheetsNoIndex = character(0L),
    rIsXlsx = TRUE,
    rSymName = NULL,
    rIndex = list(),
    naList = c(""),
    sheetRefCount = NULL,
    cache = NULL,
    warnings = CharArray$new(),
    initIndexFromFile = function(path, indexRange = NULL){
      if(length(indexRange)){
        indexRange <- private$parseCellRange("_index", indexRange)
        indexName <- indexRange$sheet
      }else{
        indexName <- "_index"
        indexRange <- private$parseCellRange("_index", "_index!A1")
      }
      if(!indexName %in% private$rSheets){
        private$rIndex <- list()
        return(self)
      }
      indexTmp <- private$readInternal(path, range = indexRange, col_names = TRUE)

      private$rIndex <- private$parseIndex(indexTmp)
      return(self)
    },
    readInternal = function(...){
      if(private$rIsXlsx){
        return(suppressMessages(read_xlsx(..., .name_repair = "minimal", progress = FALSE)))
      }
      return(suppressMessages(read_xls(..., .name_repair = "minimal", progress = FALSE)))
    },
    getRangeSymbol = function(symName){
      index <- private$rIndex[[symName]]
      range <- private$parseCellRange(symName, index$range)

      if(symName %in% private$scalars){
        index$cdim <- 0L
        index$rdim <- 0L
      }

      if("ignorerows" %in% names(index) && !is.na(index$ignorerows)){
        rowsToIgnore <- tryCatch(private$rangeToIndex(index$ignorerows, allowLetters = FALSE),
                                 error_parse_config = function(e){
                                   stop_custom("error_parse_config",
                                               sprintf(lang$errMsg$xlsio$errors$badSymbolRange, symName), call. = FALSE)
                                 })
      }else{
        rowsToIgnore <- integer(0L)
      }
      if("ignorecols" %in% names(index) && !is.na(index$ignorecols)){
        colsToIgnore <- tryCatch(private$rangeToIndex(index$ignorecols, allowLetters = TRUE),
                                 error_parse_config = function(e){
                                   stop_custom("error_parse_config",
                                               sprintf(lang$errMsg$xlsio$errors$badSymbolRange, symName), call. = FALSE)
                                 })
      }else{
        colsToIgnore <- integer(0L)
      }
      isOpenRange <- FALSE
      if(is.na(range$lr[2])){
        isOpenRange <- TRUE
        if(identical(suppressWarnings(as.integer(index$cdim)), 0L)){
          # list view
          cols <- seq(range$ul[2], range$ul[2] + index$dim + length(colsToIgnore) + 1L)
          range$lr[2] <- cols[!cols %in% colsToIgnore][index$dim + 1L]
        }else if(identical(suppressWarnings(as.integer(index$rdim)), 0L)){
          # all pivoted
          rows <- seq(range$ul[1], range$ul[1] + index$dim + length(rowsToIgnore) + 1L)
          range$lr[1] <- rows[!rows %in% rowsToIgnore][index$dim + 1L]
        }
      }
      headerRange <- NULL
      if(index$cdim > 1L){
        headerRange <- range
        rows <- seq(headerRange$ul[1], headerRange$ul[1] + index$cdim + length(rowsToIgnore) + 1L)
        headerRange$lr[1] <- rows[!rows %in% rowsToIgnore][index$cdim]
        range$ul[1] <- headerRange$lr[1]

        if(index$rdim > 0L){
          cols <- seq(headerRange$ul[2], headerRange$ul[2] + index$rdim + length(colsToIgnore) + 1L)
          headerRange$ul[2] <- cols[!cols %in% colsToIgnore][index$rdim]
        }
      }
      return(list(range = range, rowsToIgnore = rowsToIgnore,
                  colsToIgnore = colsToIgnore, headerRange = headerRange, isOpenRange = isOpenRange))
    },
    readFromIndex = function(symName){
      index <- private$rIndex[[symName]]
      isSetType <- identical(private$metadata[[symName]]$symtype, "set")

      rangeInfo <- private$getRangeSymbol(symName)

      if(isSetType && index$dim > 0L && !is.na(rangeInfo$range$lr[2]) &&
         identical(rangeInfo$range$ul[1], rangeInfo$range$lr[1])){
        data <- suppressMessages(as_tibble(as.list(vector("character", rangeInfo$range$lr[2] - rangeInfo$range$ul[2] + 1L)),
                                           .name_repair = "unique"))
      }else{
        if(rangeInfo$range$sheet %in% names(private$cache)){
          if(!length(private$cache[[rangeInfo$range$sheet]])){
            rangeTmp <- suppressWarnings(cellranger::as.cell_limits(paste0(rangeInfo$range$sheet, "!A1")))
            rangeTmp$lr <- c(NA, NA)
            private$cache[[rangeInfo$range$sheet]] <- private$readInternal(private$rpath,
                                                                           range = rangeTmp,
                                                                           col_names = FALSE, na = private$naList,
                                                                           col_types = "text")
          }
          if(is.na(rangeInfo$range$lr[1])){
            rowRangeEndTmp <- nrow(private$cache[[rangeInfo$range$sheet]])
          }else{
            rowRangeEndTmp <- min(nrow(private$cache[[rangeInfo$range$sheet]]), rangeInfo$range$lr[1])
          }
          if(is.na(rangeInfo$range$lr[2])){
            colRangeTmp <- seq(rangeInfo$range$ul[2], length(private$cache[[rangeInfo$range$sheet]]))
          }else{
            colRangeTmp <- seq(rangeInfo$range$ul[2], min(length(private$cache[[rangeInfo$range$sheet]]), rangeInfo$range$lr[2]))
          }
          if(index$dim > 0L && index$cdim > 0L){
            colNamesTmp <- as.character(private$cache[[rangeInfo$range$sheet]][rangeInfo$range$ul[1], colRangeTmp])
            if(rangeInfo$range$ul[1] + 1L > rowRangeEndTmp){
              if(rangeInfo$range$ul[1] > rowRangeEndTmp){
                data <- private$cache[[rangeInfo$range$sheet]][0L, colRangeTmp]
              } else {
                data <- private$cache[[rangeInfo$range$sheet]][rowRangeEndTmp, colRangeTmp]
              }
            }else{
              data <- private$cache[[rangeInfo$range$sheet]][seq(rangeInfo$range$ul[1] + 1L, rowRangeEndTmp), colRangeTmp]
            }
            names(data)[!is.na(colNamesTmp)] <- colNamesTmp[!is.na(colNamesTmp)]
          }else{
            if (rangeInfo$range$ul[1] + 1L > rowRangeEndTmp) {
              if (rangeInfo$range$ul[1] > rowRangeEndTmp) {
                data <- private$cache[[rangeInfo$range$sheet]][0L, colRangeTmp]
              } else {
                data <- private$cache[[rangeInfo$range$sheet]][rowRangeEndTmp, colRangeTmp]
              }
            } else {
              data <- private$cache[[rangeInfo$range$sheet]][seq(rangeInfo$range$ul[1], rowRangeEndTmp), colRangeTmp]
            }
          }
          private$sheetRefCount[[rangeInfo$range$sheet]] <- private$sheetRefCount[[rangeInfo$range$sheet]] - 1L
          if(identical(private$sheetRefCount[[rangeInfo$range$sheet]], 0L)){
            private$cache[[rangeInfo$range$sheet]] <- NULL
          }
        }else{
          data <- private$readInternal(private$rpath, range = rangeInfo$range,
                                       col_names = index$dim > 0L && index$cdim > 0L,
                                       na = private$naList, col_types = "text")
        }
        if(isSetType && index$cdim > 0L && (!nrow(data) || rowSums(is.na(data))[[1]] == ncol(data))){
          # need to manually add (empty) set text
          data[1, ] <- ""
        }
      }
      colsToIgnore <- rangeInfo$colsToIgnore - rangeInfo$range$ul[2] + 1L

      if(length(colsToIgnore)){
        colsToIgnore <- colsToIgnore[colsToIgnore > 0 & colsToIgnore <= length(data)]
      }
      # not + 1 as first row is header
      rowsToIgnore <- rangeInfo$rowsToIgnore - rangeInfo$range$ul[1] + if(index$dim > 0L && index$cdim > 0L) 0L else 1L
      if(length(rowsToIgnore)){
        rowsToIgnore <- rowsToIgnore[rowsToIgnore > 0 & rowsToIgnore <= nrow(data)]
      }

      if(length(colsToIgnore)){
        if(length(rowsToIgnore)){
          data <- data[-rowsToIgnore, -colsToIgnore]
        }else{
          data <- data[, -colsToIgnore]
        }
      }else if(length(rowsToIgnore)){
        data <- data[-rowsToIgnore, ]
      }

      if(symName %in% private$scalars){
        if(!length(data)){
          return(NA_character_)
        }
        return(data[[1]])
      }

      if(index$rdim > 0L){
        if(length(data) < index$rdim + if(isSetType) 0L else 1L){
          if(nrow(data) > 0L){
            stop_custom("error_parse_config",
                        sprintf(lang$errMsg$xlsio$errors$badSymbolRange, symName), call. = FALSE)
          }else{
            data <- as_tibble(rep.int(list(character(0L)), index$rdim + if(isSetType) 0L else 1L),
                              .name_repair = "minimal")
          }
        }
        names(data)[seq_len(index$rdim)] <- names(private$metadata[[symName]]$headers)[seq_len(index$rdim)]
        if(index$cdim == 0L && length(data) > index$rdim){
          newColName <- names(private$metadata[[symName]]$headers)[length(data)]
          if(!is.na(newColName)){
            names(data)[length(data)] <- newColName
          }
        }
        emptyRows <- which(rowSums(is.na(data[seq_len(index$rdim)])) == index$rdim)
      }else{
        emptyRows <- integer(0L)
      }

      if(index$cdim > 1L){
        hdrTmp <- private$getColHeaders(symName, rangeInfo)
        if(length(data) > length(hdrTmp)){
          data <- data[seq_along(hdrTmp)]
        }
        names(data) <- hdrTmp
        emptyCols <- which(names(data) == rep.int("\U2024", index$cdim - 1L))
      }else{
        emptyCols <- which(names(data) == "")
      }

      # remove empty rows and columns
      if(length(emptyCols)){
        if(rangeInfo$isOpenRange && identical(index$se, "0")){
          emptyCols <- seq(emptyCols[1], length(data))
        }else if(rangeInfo$isOpenRange && identical(index$se, "1")){
          cutoffC <- match(1L, diff(emptyCols))
          if(!is.na(cutoffC)){
            emptyCols <- c(emptyCols, seq(emptyCols[cutoffC], length(data)))
          }
        }
        if(length(emptyRows)){
          if(rangeInfo$isOpenRange && identical(index$se, "0")){
            emptyRows <- seq(emptyRows[1], nrow(data))
          }else if(rangeInfo$isOpenRange && identical(index$se, "1")){
            cutoffR <- match(1L, diff(emptyRows))
            if(!is.na(cutoffR)){
              emptyRows <- c(emptyRows, seq(emptyRows[cutoffR], nrow(data)))
            }
          }
          data <- data[-emptyRows, -emptyCols]
        }else{
          data <- data[, -emptyCols]
        }
      }else if(length(emptyRows)){
        if(rangeInfo$isOpenRange && identical(index$se, "0")){
          emptyRows <- seq(emptyRows[1], nrow(data))
        }else if(rangeInfo$isOpenRange && identical(index$se, "1")){
          cutoffR <- match(1L, diff(emptyRows))
          if(!is.na(cutoffR)){
            emptyRows <- c(emptyRows, seq(emptyRows[cutoffR], nrow(data)))
          }
        }
        data <- data[-emptyRows, ]
      }

      if(index$cdim > 0L){
        if(length(data) < index$dim - index$cdim + 1L){
          stop_custom("error_parse_config",
                      sprintf(lang$errMsg$xlsio$errors$badSymbolRange, symName), call. = FALSE)
        }
        valColName <- names(private$metadata[[symName]]$headers)[length(private$metadata[[symName]]$headers)]
        if(index$cdim > 1L){
          if(private$isTable(symName)){
            data <- tidyr::pivot_longer(data, -seq_len(index$dim - index$cdim),
                                        names_to = c(names(private$metadata[[symName]]$headers)[seq(index$dim - index$cdim + 1L, index$dim - 1L)],
                                                     ".value"),
                                        names_sep = "\U2024", values_to = valColName, values_drop_na = TRUE)
          }else{
            data <- tidyr::pivot_longer(data, -seq_len(index$dim - index$cdim),
                                        names_to = names(private$metadata[[symName]]$headers)[seq(index$dim - index$cdim + 1L, index$dim)],
                                        names_sep = "\U2024", values_to = valColName, values_drop_na = TRUE)
          }
        }else if(!private$isTable(symName)){
          data <- tidyr::pivot_longer(data, -seq_len(index$dim - 1L),
                                      names_to = names(private$metadata[[symName]]$headers)[index$dim],
                                      values_to = valColName, values_drop_na = TRUE)
        }
      }else if(private$isTable(symName)){
        # need to pivot table that is loaded as list
        if(length(data) < index$dim){
          stop_custom("error_parse_config",
                      sprintf(lang$errMsg$xlsio$errors$badSymbolRange, symName), call. = FALSE)
        }
        data <- tidyr::pivot_wider(data,
                                   names_from = !!length(data) - 1L,
                                   values_from = !!length(data))
      }

      if(private$isTable(symName)){
        pivotedHeaders <- names(private$metadata[[symName]]$headers)[-seq_len(index$dim - 1L)]
        missingCols <- !tolower(pivotedHeaders) %in% tolower(names(data)[-seq_len(index$dim - 1L)])
        if(length(missingCols)){
          # add missing columns
          data[, pivotedHeaders[missingCols]] <- NA_real_
        }
        # need to potentially reorder columns
        dataHdr <- names(data)[-seq_len(index$dim - 1L)]
        if(any(!tolower(dataHdr) %in% tolower(pivotedHeaders))){
          stop_custom("error_data",
                      sprintf(lang$errMsg$xlsio$errors$invalidRecords, symName,
                              paste(dataHdr[!tolower(dataHdr) %in% tolower(pivotedHeaders)], collapse = "', '")), call. = FALSE)
        }
        colOrder <- match(tolower(pivotedHeaders), tolower(dataHdr))
        if(!identical(colOrder, seq_along(colOrder))){
          data <- data[, c(seq_len(index$dim - 1L), colOrder + (index$dim - 1L))]
        }
      }

      if(isSetType){
        return(private$readSet(data, symName))
      }
      if(private$isTable(symName)){
        numericalCols <- seq(stri_count_fixed(private$metadata[[symName]]$colTypes, "c") + 1L,
                             length(data))
        data <- suppressWarnings(
          mutate_at(data, numericalCols, as.numeric))
        if(tolower(index[["squeeze"]]) %in% c("1", "y")){
          data[data == 0] <- NA_real_
          data <- data[rowSums(is.na(data[numericalCols])) != length(numericalCols),]
        }
      }else{
        data <- suppressWarnings(mutate_at(data, length(data), as.numeric))
        if(tolower(index[["squeeze"]]) %in% c("1", "y")){
          data <- data[is.na(data[[length(data)]]) | data[[length(data)]] != 0,]
        }
      }
      return(data)
    },
    readScalars = function(symName){
      if(!symName %in% names(private$metadata)){
        stop_custom("error_notfound", sprintf(lang$errMsg$xlsio$errors$symbolNotFound, symName), call. = FALSE)
      }
      scalarsToProcess <- private$metadata[[symName]]$symnames
      scalarDesc <- private$metadata[[symName]]$symtext
      if(identical(symName, scalarsFileName) && length(private$clOptScalars)){
        scalarsToProcess <- c(scalarsToProcess, private$clOptScalars)
        scalarDesc <- c(scalarDesc, rep.int("", length(private$clOptScalars)))
      }
      scalarsProcessed <- character(0L)
      scalarsDf <- tibble(scalar = scalarsToProcess,
                          description = scalarDesc,
                          value = NA_character_)
      if(symName %in% names(private$rIndex)){
        scalarsDfTmp <- private$readFromIndex(symName)
        invalidScalars <- !scalarsDfTmp[[1]] %in% scalarsToProcess
        if(any(invalidScalars)){
          scalarsDfTmp <- scalarsDfTmp[!invalidScalars, ]
        }
        duplicatedSym <- duplicated(scalarsDfTmp[[1]])
        if(any(duplicatedSym)){
          stop_custom("error_data",
                      sprintf(lang$errMsg$xlsio$errors$duplicateScalars,
                              paste(scalarsDfTmp[[1]][duplicatedSym], collapse = "', '")), call. = FALSE)
        }
        noDescCol <- length(scalarsDfTmp) == 2L
        if(!noDescCol){
          openRange <- is.na(private$parseCellRange(symName, private$rIndex[[symName]]$range)$lr[1])
          noDescCol <- openRange && all(scalarsDfTmp[[3]] == "")
        }
        scalarsDf[match(scalarsDfTmp[[1]], scalarsToProcess), 3] <- scalarsDfTmp[[if(noDescCol) 2L else 3L]]
        scalarsProcessed <- scalarsDfTmp[[1]]
      }
      additionalScalarsToProcess <- names(private$rIndex)[names(private$rIndex) %in% setdiff(scalarsToProcess, scalarsProcessed)]
      if(length(additionalScalarsToProcess)){
        scalarsDf[match(additionalScalarsToProcess,
                        scalarsToProcess), 3] <- vapply(additionalScalarsToProcess,
                                                        function(scalarToProcess){
                                                          return(private$readFromIndex(scalarToProcess))
                                                        }, character(1L), USE.NAMES = FALSE)
      }
      return(scalarsDf)
    },
    readScalarsNoIndex = function(symName){
      if(!symName %in% names(private$metadata)){
        stop_custom("error_notfound", sprintf(lang$errMsg$xlsio$errors$symbolNotFound,
                                              symName), call. = FALSE)
      }
      scalarsToProcess <- private$metadata[[symName]]$symnames
      scalarDesc <- private$metadata[[symName]]$symtext
      if(identical(symName, scalarsFileName) && length(private$clOptScalars)){
        scalarsToProcess <- c(scalarsToProcess, private$clOptScalars)
        scalarDesc <- c(scalarDesc, rep.int("", length(private$clOptScalars)))
      }
      scalarsProcessed <- character(0L)
      scalarsDf <- tibble(scalar = scalarsToProcess,
                          description = scalarDesc,
                          value = NA_character_)

      sheetId <- match(symName, tolower(private$rSheetsNoIndex))
      if(!is.na(sheetId)){
        scalarsDfTmp <- private$readInternal(private$rpath, sheet = sheetId, col_names = TRUE)
        invalidScalars <- !scalarsDfTmp[[1]] %in% scalarsToProcess
        if(any(invalidScalars)){
          scalarsDfTmp <- scalarsDfTmp[!invalidScalars, ]
        }
        duplicatedSym <- duplicated(scalarsDfTmp[[1]])
        if(any(duplicatedSym)){
          stop_custom("error_data",
                      sprintf(lang$errMsg$xlsio$errors$duplicateScalars,
                              paste(scalarsDfTmp[[1]][duplicatedSym], collapse = "', '")), call. = FALSE)
        }
        scalarsDf[match(scalarsDfTmp[[1]], scalarsToProcess), 3] <- as.character(scalarsDfTmp[[3L]])
        scalarsProcessed <- scalarsDfTmp[[1]]
      }
      scalarXlsSheetIds <- match(setdiff(scalarsToProcess, scalarsProcessed), tolower(private$rSheetsNoIndex))
      scalarXlsSheetIds <- scalarXlsSheetIds[!is.na(scalarXlsSheetIds)]
      if(length(scalarXlsSheetIds)){
        scalarsDf[match(tolower(private$rSheetsNoIndex)[scalarXlsSheetIds],
                        scalarsToProcess), 3] <- vapply(scalarXlsSheetIds, function(sheetID){
                          return(as.character(private$readInternal(private$rpath,
                                                                   sheet = sheetID,
                                                                   col_names = FALSE)[[1]][1]))
                        }, character(1L), USE.NAMES = FALSE)
      }
      return(scalarsDf)
    },
    readVEScalars = function(symName){
      scalarsToProcess <- private$metadata[[symName]]$symnames
      if(symName %in% names(private$rIndex)){
        scalarsDf <- private$readFromIndex(symName)
        invalidScalars <- !scalarsDf[[1]] %in% scalarsToProcess
        if(any(invalidScalars)){
          scalarsDf <- scalarsDf[!invalidScalars, ]
        }
        duplicatedSym <- duplicated(scalarsDf[[1]])
        if(any(duplicatedSym)){
          stop_custom("error_data",
                      sprintf(lang$errMsg$xlsio$errors$duplicateScalarVE,
                              paste(scalarsDf[[1]][duplicatedSym], collapse = "', '")), call. = FALSE)
        }
        if(length(scalarsDf) == 6L){
          scalarsDf <- add_column(scalarsDf,
                                  description = scalarsDf[[2]][match(scalarsDf[[1]], scalarsToProcess)],
                                  .after = "scalar")
        }else{
          scalarsDf[[2]] <- private$metadata[[symName]]$symtext[match(scalarsToProcess, scalarsDf[[1]])]
        }
        return(scalarsDf)
      }
      return(tibble(scalar = scalarsToProcess,
                    description = private$metadata[[symName]]$symtext,
                    level = NA_real_,
                    marginal = NA_real_,
                    lower = NA_real_,
                    upper = NA_real_,
                    scale = NA_real_))
    },
    readSet = function(data, symName){
      index <- private$rIndex[[symName]]
      setVals <- tolower(index$values)
      if(identical(setVals, "auto")){
        if(identical(index$rdim, 0L) || identical(index$cdim, 0L)){
          if(length(data) < index$dim + 1L){
            setVals <- "nodata"
          }else{
            setVals <- "dense"
          }
        }else{
          setVals <- "yn"
        }
      }
      if(identical(setVals, "nodata")){
        if(length(data) < index$dim){
          stop_custom("error_data",
                      sprintf(lang$errMsg$xlsio$errors$badSymbolDim, symName), call. = FALSE)
        }else if(length(data) < index$dim + 1L){
          if(symName %in% c(scalarsFileName, scalarsOutName)){
            return(data)
          }
          data[["text"]] <- ""
        }else{
          data[[length(data)]] <- ""
        }
        return(data)
      }
      if(length(data) < index$dim + 1L){
        stop_custom("error_data",
                    sprintf(lang$errMsg$xlsio$errors$badSymbolDim, symName), call. = FALSE)
      }
      if(identical(setVals, "yn")){
        removeRows <- is.na(data[[length(data)]]) | tolower(as.character(data[[length(data)]])) %in% c("0", "n", "no")
        data <- data[!removeRows, ]
        data[[length(data)]] <- ""
      }else if(identical(setVals, "sparse")){
        data <- data[!is.na(data[[length(data)]]), ]
      }else{
        data[is.na(data[[length(data)]]), length(data)] <- ""
      }
      return(data)
    },
    getColHeaders = function(symName, rangeInfo){
      index <- private$rIndex[[symName]]
      headerData <- private$readInternal(private$rpath, range = rangeInfo$headerRange,
                                         col_names = FALSE, col_types = "text")
      # TODO: use na argument of readxl when  https://github.com/tidyverse/readxl/issues/572 is closed
      headerData[is.na(headerData)] <- ""
      colsToIgnore <- rangeInfo$colsToIgnore - rangeInfo$headerRange$ul[2] + 1L
      if(length(colsToIgnore)){
        colsToIgnore <- colsToIgnore[colsToIgnore < 1 | colsToIgnore > length(headerData)]
      }
      rowsToIgnore <- rangeInfo$rowsToIgnore - rangeInfo$headerRange$ul[1] + 1L
      if(length(rowsToIgnore)){
        rowsToIgnore <- rowsToIgnore[rowsToIgnore < 1 | rowsToIgnore > nrow(headerData)]
      }

      if(length(colsToIgnore)){
        if(length(rowsToIgnore)){
          headerData <- headerData[-rowsToIgnore, -colsToIgnore]
        }else{
          headerData <- headerData[, -colsToIgnore]
        }
      }else if(length(rowsToIgnore)){
        headerData <- headerData[-rowsToIgnore, ]
      }

      colHeaders <- character(0L)
      if(index$rdim > 0L){
        colHeaders <- names(private$metadata[[symName]]$headers)[seq_len(index$rdim)]
        headerData <- headerData[, -seq_len(index$rdim)]
      }

      colHeaders <- c(colHeaders, vapply(headerData, paste, character(1L),
                                         collapse = "\U2024", USE.NAMES = FALSE))
      return(colHeaders)
    },
    parseIndex = function(indexDf){
      colNames <- tolower(names(indexDf))
      symColId <- match(c("symbol", "sym", "name"), colNames)
      if(all(is.na(symColId))){
        if(!identical(colNames[2], "")){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexNoSymInfo, call. = FALSE)
        }
        symColId <- 2L
      }else{
        symColId <- symColId[!is.na(symColId)][1]
      }
      indexDf[[symColId]] <- tolower(indexDf[[symColId]])
      symbolsInExcel <- match(indexDf[[symColId]], c(names(private$metadata), private$scalars))
      invalidRowIds <- is.na(symbolsInExcel)
      if(any(invalidRowIds)){
        invalidSymbols <- indexDf[[symColId]][invalidRowIds & (!is.na(indexDf[[symColId]]))]
        if(length(invalidSymbols)){
          private$warnings$push(sprintf(lang$errMsg$xlsio$warnings$invalidSymbols,
                                        paste(invalidSymbols, collapse = "', '")))
        }
        nonEmptyInvalidRows <- is.na(indexDf[[symColId]]) & rowSums(is.na(indexDf)) != ncol(indexDf)
        if(length(nonEmptyInvalidRows)){
          private$warnings$push(lang$errMsg$xlsio$warnings$skipOptionsPersistence)
        }
        indexDf <- indexDf[!invalidRowIds, ]
      }
      symbolsInExcel <- indexDf[[symColId]]
      duplicateSymbols <- duplicated(symbolsInExcel)

      if(any(duplicateSymbols)){
        stop_custom("error_parse_config", sprintf(lang$errMsg$xlsio$errors$duplicateSymbols,
                                                  paste(symbolsInExcel[duplicateSymbols], collapse = "', '")), call. = FALSE)
      }

      if(!nrow(indexDf)){
        return(list())
      }
      if(symColId > 1L){
        indexDf <- indexDf[, -seq_len(symColId - 1L)]
      }
      if(!(tolower(names(indexDf)[2]) %in% c("range", "rng") || identical(names(indexDf)[2], ""))){
        stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexNoRangeInfo, call. = FALSE)
      }
      names(indexDf)[c(1L, 2L)] <- c("symbol", "range")
      colNames <- tolower(names(indexDf))

      # remove empty cols
      emptyCols <- vapply(seq_along(indexDf), function(idx){
        return(identical(colNames[idx], "") && is.logical(indexDf[[idx]]))
      }, logical(1L), USE.NAMES = FALSE)
      if(any(emptyCols)){
        indexDf <- indexDf[, !emptyCols]
        colNames <- tolower(names(indexDf))
      }
      kvArgCols <- colNames == ""
      if(any(kvArgCols)){
        private$warnings$push(lang$errMsg$xlsio$warnings$keyValueCols)
        indexDf <- indexDf[, !kvArgCols]
        colNames <- tolower(names(indexDf))
      }
      rdimColId <- match(c("rdim", "rowdim"), colNames)
      if(!all(is.na(rdimColId))){
        rdimColId <- rdimColId[!is.na(rdimColId)]
        if(length(rdimColId) > 1L){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexDuplicateRdim, call. = FALSE)
        }
        names(indexDf)[rdimColId] <- "rdim"
        tryCatch(indexDf[[rdimColId]] <- as.integer(indexDf[[rdimColId]]),
                 warning = function()
                   stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexInvalidRdimCol, call. = FALSE))
      }
      cdimColId <- match(c("cdim", "coldim"), colNames)
      if(!all(is.na(cdimColId))){
        cdimColId <- cdimColId[!is.na(cdimColId)]
        if(length(cdimColId) > 1L){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexDuplicateCdim, call. = FALSE)
        }
        names(indexDf)[cdimColId] <- "cdim"
        tryCatch(indexDf[[cdimColId]] <- as.integer(indexDf[[cdimColId]]),
                 warning = function()
                   stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexInvalidCdimCol, call. = FALSE))
      }
      ignoreRowId <- match(c("ignorerows"), colNames)
      if(!all(is.na(ignoreRowId))){
        indexDf[[ignoreRowId]] <- as.character(indexDf[[ignoreRowId]])
      }
      ignoreColId <- match(c("ignorecols", "ignorecolumns"), colNames)
      if(!all(is.na(ignoreColId))){
        ignoreColId <- ignoreColId[!is.na(ignoreColId)]
        if(length(ignoreColId) > 1L){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexDuplicateIgnoreCol, call. = FALSE)
        }
        names(indexDf)[ignoreColId] <- "ignorecols"
        indexDf[[ignoreColId]] <- as.character(indexDf[[ignoreColId]])
      }
      sEId <- match(c("skipempty", "se"), colNames)
      if(!all(is.na(sEId))){
        sEId <- sEId[!is.na(sEId)]
        if(length(sEId) > 1L){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexDuplicateSkipEmpty, call. = FALSE)
        }
        names(indexDf)[sEId] <- "se"
        indexDf[[sEId]] <- as.character(indexDf[[sEId]])
      }
      if("se" %in% names(indexDf)){
        valuesUndefined <- is.na(indexDf[["se"]])
        indexDf[valuesUndefined, "se"] <- "1"
        invalidValueOptions <- !tolower(indexDf[["se"]]) %in% c("0", "1")
        if(any(invalidValueOptions)){
          private$warnings$push(sprintf(lang$errMsg$xlsio$warnings$invalidSkipEmpty,
                                        paste(symbolsInExcel[invalidValueOptions], collapse = "', '")))
        }
        indexDf[invalidValueOptions, "se"] <- "1"
      }else{
        indexDf[["se"]] <- "1"
      }
      sqId <- match(c("squeeze", "sq"), colNames)
      if(!all(is.na(sqId))){
        sqId <- sqId[!is.na(sqId)]
        if(length(sqId) > 1L){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$indexDuplicateSqueeze, call. = FALSE)
        }
        names(indexDf)[sqId] <- "squeeze"
        indexDf[[sqId]] <- tolower(as.character(indexDf[[sqId]]))
      }
      if("squeeze" %in% names(indexDf)){
        valuesUndefined <- is.na(indexDf[["squeeze"]])
        indexDf[valuesUndefined, "squeeze"] <- "1"
        invalidValueOptions <- !tolower(indexDf[["squeeze"]]) %in% c("0", "1", "y", "n")
        if(any(invalidValueOptions)){
          private$warnings$push(sprintf(lang$errMsg$xlsio$warnings$invalidSqueeze,
                                        paste(symbolsInExcel[invalidValueOptions], collapse = "', '")))
        }
        indexDf[invalidValueOptions, "squeeze"] <- "1"
      }else{
        indexDf[["squeeze"]] <- "1"
      }
      invalidKeys <- !tolower(names(indexDf)) %in%
        c("symbol", "range", "cdim", "rdim", "dim", "values", "ignorerows", "ignorecols", "se", "squeeze")
      if(any(invalidKeys)){
        private$warnings$push(sprintf(lang$errMsg$xlsio$warnings$invalidOptions,
                                      paste(names(indexDf)[invalidKeys], collapse = "', '")))
        indexDf <- indexDf[, !invalidKeys]
      }
      names(indexDf) <- tolower(names(indexDf))
      indexDf[["dim"]] <- vapply(symbolsInExcel, function(symName){
        if(symName %in% private$scalars){
          return(0L)
        }
        dim <- stri_count_fixed(private$metadata[[symName]]$colTypes, "c")
        if(identical(private$metadata[[symName]]$symtype, "set")){
          if(dim == 1L){
            return(dim)
          }
          return(dim - 1L)
        }
        if(private$isTable(symName)){
          return(dim + 1L)
        }
        return(dim)
      }, integer(1L), USE.NAMES = FALSE)

      if("cdim" %in% names(indexDf)){
        if("rdim" %in% names(indexDf)){
          missingcDim <- is.na(indexDf[["cdim"]])
          missingrDim <- is.na(indexDf[["rdim"]])
          missingDim <- missingcDim & missingrDim
          indexDf[missingDim, "rdim"] <- indexDf[["dim"]][missingDim] - 1L
          indexDf[missingDim, "cdim"] <- 1L

          indexDf[missingcDim & !missingrDim, "cdim"] <- indexDf[["dim"]][missingcDim & !missingrDim] - indexDf[["rdim"]][missingcDim & !missingrDim]
          invalidRdim <- indexDf[["cdim"]] < 0L
          if(any(invalidRdim)){
            stop_custom("error_parse_config", sprintf(lang$errMsg$xlsio$errors$indexInvalidRdim,
                                                      paste(symbolsInExcel[invalidRdim], collapse = "', '")), call. = FALSE)
          }

          indexDf[!missingcDim & missingrDim, "rdim"] <- indexDf[["dim"]][!missingcDim & missingrDim] - indexDf[["cdim"]][!missingcDim & missingrDim]
          invalidCdim <- indexDf[["rdim"]] < 0L

          if(any(invalidCdim)){
            invalidCdimSym <- symbolsInExcel[invalidCdim]
            if(any(tolower(invalidCdimSym) %in% private$scalars)){
              invalidCdimSym <- invalidCdimSym[!tolower(invalidCdimSym) %in% private$scalars]
            }
            if(length(invalidCdimSym)){
              stop_custom("error_parse_config", sprintf(lang$errMsg$xlsio$errors$indexInvalidCdim,
                                                        paste(invalidCdimSym, collapse = "', '")), call. = FALSE)
            }
          }

          invalidDim <- (indexDf[["dim"]][!missingDim] -
                           indexDf[["cdim"]][!missingDim] -
                           indexDf[["rdim"]][!missingDim]) != 0L

          if(any(invalidDim)){
            stop_custom("error_parse_config", sprintf(lang$errMsg$xlsio$errors$indexInvalidDim,
                                                      paste(symbolsInExcel[!missingDim][invalidDim], collapse = "', '")), call. = FALSE)
          }
        }else{
          missingcDim <- is.na(indexDf[["cdim"]])
          indexDf[missingcDim, "rdim"]   <- indexDf[["dim"]][missingcDim] - 1L
          indexDf[missingcDim, "cdim"]   <- 1L

          indexDf[!missingcDim, "rdim"]  <- indexDf[["dim"]][!missingcDim] - indexDf[["cdim"]][!missingcDim]
          invalidCdim <- indexDf[["rdim"]] < 0L
          if(any(invalidCdim)){
            stop_custom("error_parse_config", sprintf(lang$errMsg$xlsio$errors$indexInvalidCdim,
                                                      paste(symbolsInExcel[invalidCdim], collapse = "', '")), call. = FALSE)
          }
        }
      }else if("rdim" %in% names(indexDf)){
        missingrDim <- is.na(indexDf[["rdim"]])

        indexDf[missingrDim, "rdim"]   <- indexDf[["dim"]][missingrDim] - 1L
        indexDf[missingrDim, "cdim"]   <- 1L

        indexDf[!missingrDim, "cdim"]  <- indexDf[["dim"]][!missingrDim] - indexDf[["rdim"]][!missingrDim]
        invalidRdim <- indexDf[["cdim"]] < 0L
        if(any(invalidRdim)){
          stop_custom("error_parse_config", sprintf(lang$errMsg$xlsio$errors$indexInvalidRdim,
                                                    paste(symbolsInExcel[invalidRdim], collapse = "', '")), call. = FALSE)
        }
      }else{
        indexDf[["cdim"]] <- indexDf[["dim"]] - 1L
        indexDf[["cdim"]] <- 1L
      }
      if("values" %in% names(indexDf)){
        valuesUndefined <- is.na(indexDf[["values"]])
        indexDf[valuesUndefined, "values"] <- "auto"
        invalidValueOptions <- !tolower(indexDf[["values"]]) %in% c("auto", "nodata", "yn", "sparse", "dense")
        if(any(invalidValueOptions)){
          stop_custom("error_parse_config", sprintf(lang$errMsg$xlsio$errors$indexInvalidValues,
                                                    paste(symbolsInExcel[invalidValueOptions], collapse = "', '")), call. = FALSE)
        }
      }else{
        indexDf[["values"]] <- "auto"
      }
      if(any(private$scalars %in% symbolsInExcel) &&
         any(c(scalarsFileName, scalarsOutName) %in% symbolsInExcel)){
        # need to throw warning if scalars both in table and declared individually
        private$warnings$push(lang$errMsg$xlsio$warnings$scalarDeclarations)
      }
      private$getSheetsToCache(indexDf[["range"]])
      return(setNames(split(indexDf,
                            seq_len(nrow(indexDf))),
                      indexDf[["symbol"]]))
    },
    rangeToIndex = function(rangeRaw, allowLetters = TRUE){
      rSplit <- strsplit(rangeRaw, ",", fixed = TRUE)[[1L]]
      return(sort(unique(unlist(lapply(rSplit, function(range){
        rangeSplit <- strsplit(range, ":", fixed = TRUE)[[1L]]
        if(length(rangeSplit) > 2L){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$invalidColRange, call. = FALSE)
        }
        if(allowLetters){
          index <- private$excelColToIndex(rangeSplit)
        }else{
          index <- suppressWarnings(as.integer(rangeSplit))
          if(any(is.na(index))){
            stop_custom("error_parse_config", lang$errMsg$xlsio$errors$invalidRange, call. = FALSE)
          }
        }
        if(length(index) == 2L && index[2] < index[1]){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$invalidRange, call. = FALSE)
        }
        return(index)
      }), use.names = FALSE))))
    },
    excelColToIndex = function(range){
      return(vapply(range, function(r){
        r <- trimws(r)
        rI <- suppressWarnings(as.integer(r))
        if(!any(is.na(rI))){
          if(rI < 1L){
            stop_custom("error_parse_config", lang$errMsg$xlsio$errors$invalidColRange, call. = FALSE)
          }
          return(rI)
        }
        rChar <- utf8ToInt(r) - 64L
        if(length(rChar) == 0L || length(rChar) > 3L || any(rChar < 1L | rChar > 26L)){
          stop_custom("error_parse_config", lang$errMsg$xlsio$errors$invalidColRange, call. = FALSE)
        }
        if(length(rChar) == 1L){
          return(rChar)
        }
        if(length(rChar) == 2L){
          return(rChar[1] * 26L + rChar[2])
        }
        return(rChar[1] * 676L + rChar[2] * 26L + rChar[3])
      }, integer(1L), USE.NAMES = FALSE))
    },
    getSheetsToCache = function(rangeCol){
      sheetNamesTmp <- trimws(rangeCol, whitespace = "\"")
      sheetNamesQuoted <- nchar(sheetNamesTmp) == nchar(rangeCol) - 2L
      sheetNames <- sheetNamesTmp
      sheetNames[!sheetNamesQuoted] <- trimws(sheetNames[!sheetNamesQuoted])
      sheetNames <- vapply(strsplit(sheetNames, "!", fixed = TRUE), function(el){
        paste(el[-length(el)], collapse = "!")
      }, character(1L), USE.NAMES = FALSE)
      sheetsToCache <- table(sheetNames)
      sheetsToCache <- sheetsToCache[sheetsToCache > 1]
      if(length(sheetsToCache)){
        private$sheetRefCount <- sheetsToCache
        private$cache <- setNames(vector("list", length(sheetsToCache)), names(sheetsToCache))
      }
      return(invisible(self))
    },
    parseCellRange = function(symName, range){
      rangeTmp <- trimws(range, whitespace = "\"")
      if(nchar(rangeTmp) == nchar(range) - 2L){
        # element was quoted
        range <- rangeTmp
      }else{
        range <- trimws(range)
      }
      if(range %in% c("", "!")){
        return(structure(list(ul = c(1L, 1L), lr = c(NA_integer_, NA_integer_),
                              sheet = private$rSheets[1]),
                         class = c("cell_limits", "list")))
      }
      if(startsWith(range, "!")){
        range <- paste0(private$rSheets[1], range)
      }else if(endsWith(range, "!")){
        range <- paste0(range, "A1")
      }else if(range %in% private$rSheets){
        range <- paste0(range, "!A1")
      }
      parsedRange <- suppressWarnings(cellranger::as.cell_limits(range))
      if(identical(parsedRange$ul, parsedRange$lr) &&
         !symName %in% private$scalars){
        parsedRange$lr <- c(NA_integer_, NA_integer_)
      }
      return(parsedRange)
    },
    genIndexFromMetadata = function(symbolNames, wsNames, data){
      return(bind_rows(lapply(seq_along(symbolNames), function(idx){
        symName <- symbolNames[idx]
        if(tolower(symName) %in% private$scalars){
          return(tibble(type = character(),
                        symbol = character(),
                        range = character(),
                        cDim = integer(),
                        dim = integer()))
        }
        symMeta <- private$metadata[[tolower(symName)]]
        if(tolower(symName) %in% c(scalarsFileName, scalarsOutName)){
          scalarNamesData <- tolower(data[[wsNames[idx]]][[1]])
          scalarNames <- symMeta$symnames
          scalarTypes <- symMeta$symtypes
          if(identical(tolower(symName), scalarsFileName) && length(private$clOptScalars)){
            scalarNames <- c(scalarNames, private$clOptScalars)
            scalarTypes <- c(scalarTypes, rep.int("set", length(private$clOptScalars)))
          }
          scalarIds <- match(scalarNamesData, scalarNames)
          scalarIds <- scalarIds[!is.na(scalarIds)]
          scalarNames <- scalarNames[scalarIds]
          return(tibble(type = vapply(scalarTypes[scalarIds], function(symType){
            if(symType %in% c("parameter", "equation", "variable")){
              return("par")
            }
            return("set")}, character(1L), USE.NAMES = FALSE),
            symbol = scalarNames,
            range = paste0('"', wsNames[idx], "!C", match(scalarNames, scalarNamesData) + 1L, '"'),
            cDim = 0L,
            dim = 0L))
        }
        symDim <- stri_count_fixed(symMeta$colTypes, "c")
        if(identical(symMeta$symtype, "set")){
          if(symDim > 1L){
            symDim <- symDim - 1L
          }
          isTable <- FALSE
        }else{
          isTable <- private$isTable(tolower(symName))
        }
        return(tibble(type = if(symMeta$symtype %in% c("parameter", "equation", "variable")) "par" else "set",
                      symbol = symName,
                      range = paste0('"', wsNames[idx], "!A", if(isTable) '1"' else '2"'),
                      cDim = if(isTable) 1L else 0L,
                      dim = symDim + if(isTable) 1L else 0L))
      })))
    }
  )
)
