CsvIO <- R6::R6Class("CsvIO", inherit = LocalFileIO, public = list(
  rInitFile = function(path){
    private$rpath <- path
    private$rHeaderRow <- TRUE
    private$rColsToRead <- NULL
    private$rColsToSkip <- NULL
    private$decimalSep <- "."
    private$rSample <- read_lines(path, n_max = 3L)
    private$rDelim <- private$guessDelim(private$rSample)
    if(length(private$rDelim) == 0L){
      stop_custom("error_bad_delim", "ERROR: Could not determine delimiter of CSV file.", call. = FALSE)
    }
    private$rHeaders <- strsplit(private$rSample[1], private$rDelim, fixed = TRUE)[[1L]]
    return(self)
  },
  read = function(path, symName, colsToRead = NULL, hasHeaderRow = TRUE, decimalSep = ".", forceInit = FALSE){
    if(forceInit || !identical(path, private$rpath)){
      self$rInitFile(path)
    }
    if(identical(hasHeaderRow, FALSE)){
      private$rHeaderRow <- hasHeaderRow
    }
    if(!is.null(colsToRead)){
      self$setColsToRead(colsToRead, symName)
    }
    if(!identical(decimalSep, ".")){
      private$decimalSep <- decimalSep
    }
    if(symName %in% c(scalarsFileName, scalarsOutName)){
      return(private$readScalars(symName))
    }
    return(private$readSymbol(symName))
  },
  setHasHeaderRow = function(hasHeaderRow){
    stopifnot(is.logical(hasHeaderRow), length(hasHeaderRow) == 1L)
    private$rHeaderRow <- hasHeaderRow
    return(self)
  },
  setColsToRead = function(colsToRead, symName){
    stopifnot(is.character(colsToRead), length(colsToRead) == length(private$metadata[[symName]]$headers))
    colsToSkip <- colsToRead == "-"
    colsToReadTmp <- match(colsToRead[!colsToSkip], private$rHeaders)
    if(any(is.na(colsToReadTmp))){
      stop(sprintf("Invalid columns: %s", paste(colsToRead[is.na(colsToReadTmp)], collapse = ", ")))
    }
    private$rColsToRead <- colsToReadTmp
    private$rColsToSkip <- which(colsToSkip)
    return(self)
  },
  getDelim = function(){
    return(private$rDelim)
  },
  getHeaders = function(){
    return(private$rHeaders)
  }), private = list(
    rpath = character(0L),
    supportedDelim = c(",", ";", "|", "\t"),
    rSample = character(0L),
    rDelim = ",",
    rHeaderRow = TRUE,
    rHeaders = character(0L),
    rColsToRead = NULL,
    rColsToSkip = NULL,
    decimalSep = ".",
    readSymbol = function(symName){
      needReorder <- FALSE
      if(is.null(private$rColsToRead)){
        colTypes <- private$metadata[[symName]]$colTypes
      }else{
        colTypes <- rep.int("-", length(private$rHeaders))
        symColTypes <- strsplit(private$metadata[[symName]]$colTypes, "", fixed = TRUE)[[1]]
        if(length(private$rColsToSkip)){
          symColTypes <- symColTypes[-private$rColsToSkip]
        }
        colTypes[private$rColsToRead] <- symColTypes
        colOrder <- order(private$rColsToRead)
        if(!identical(colOrder, seq_along(private$rColsToRead))){
          needReorder <- TRUE
        }
        colTypes <- paste(colTypes, collapse = "")
      }
      data <- suppressWarnings(
        read_delim(private$rpath, 
                   private$rDelim,
                   col_types = colTypes, 
                   col_names = private$rHeaderRow,
                   progress = FALSE,
                   skip_empty_rows = TRUE,
                   locale = locale(decimal_mark = private$decimalSep)))
      symHeaders <- names(private$metadata[[symName]]$headers)
      if(needReorder){
        data <- data[, colOrder]
      }
      if(!length(private$rColsToSkip)){
        names(data) <- symHeaders
        return(data)
      }
      names(data) <- symHeaders[-private$rColsToSkip]
      data[, symHeaders[private$rColsToSkip]] <- NA_character_
      colOrder <- match(names(data), symHeaders)
      if(!identical(colOrder, seq_along(symHeaders))){
        data <- data[, colOrder]
      }
      return(data)
    },
    readScalars = function(symName){
      scalarsToProcess <- private$metadata[[symName]]$symnames
      scalarDesc <- private$metadata[[symName]]$symtext
      if(identical(symName, scalarsFileName) && length(private$clOptScalars)){
        scalarsToProcess <- c(scalarsToProcess, private$clOptScalars)
        scalarDesc <- c(scalarDesc, rep.int("", length(private$clOptScalars)))
      }
      scalarsDf <- tibble(scalar = scalarsToProcess,
                          description = scalarDesc,
                          value = NA_character_)
      scalarsDfTmp <- private$readSymbol(symName)
      invalidScalars <- !scalarsDfTmp[[1]] %in% scalarsToProcess
      if(any(invalidScalars)){
        scalarsDfTmp <- scalarsDfTmp[!invalidScalars, ]
      }
      duplicatedSym <- duplicated(scalarsDfTmp[[1]])
      if(any(duplicatedSym)){
        stop_custom("error_data",
                    sprintf(lang$errMsg$xlsio$errors$duplicateScalars,
                            paste(scalarsDfTmp[[1]][duplicateSymbols], collapse = "', '")), call. = FALSE)
      }
      scalarsDf[match(scalarsDfTmp[[1]], scalarsToProcess), 3] <- scalarsDfTmp[[if(noDescCol) 2L else 3L]]
      return(scalarsDf)
    },
    guessDelim = function(sample){
      return(private$supportedDelim[vapply(private$supportedDelim, function(delim){
        nbItems <- vapply(strsplit(sample, delim, fixed = TRUE), length,
                          integer(1L), USE.NAMES = FALSE)
        return(nbItems[1] > 1L && var(nbItems) == 0L)
      }, logical(1L), USE.NAMES = FALSE)])
    })
)
