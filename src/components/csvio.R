CsvIO <- R6::R6Class("CsvIO", inherit = LocalFileIO, public = list(
  rInitFile = function(path, delim = NULL, needDelim = TRUE) {
    private$rpath <- file.path(dirname(path), basename(path))
    private$rSymName <- character(0L)
    private$rHeaderRow <- TRUE
    private$rColsToRead <- NULL
    private$rColsToSkip <- NULL
    private$decimalSep <- "."
    sampleTmp <- read_lines(path, n_max = 3L)
    if (identical(sampleTmp, character(0L)) || !all(validUTF8(sampleTmp))) {
      stop_custom("error_bad_encoding",
        lang$errMsg$csvio$errors$badEncoding,
        call. = FALSE
      )
    }
    private$rSample <- sampleTmp
    if (is.null(delim)) {
      private$rDelim <- private$guessDelim(private$rSample)
      if (length(private$rDelim) == 0L) {
        if (needDelim) {
          stop_custom("error_bad_delim",
            sprintf(
              lang$errMsg$csvio$errors$badDelim,
              paste(c(private$supportedDelim[private$supportedDelim != "\t"], "tab"),
                collapse = "' '"
              )
            ),
            call. = FALSE
          )
        } else {
          private$rDelim <- character(0L)
          private$rHeaders <- private$rSample[1]
          return(self)
        }
      }
      if (length(private$rDelim) > 1L) {
        stop_custom("error_ambiguous_delim",
          "delimiter could not be uniquely identified. Please use setRDelim method to set manually",
          call. = FALSE
        )
      }
    } else if (!delim %in% private$supportedDelim) {
      stop_custom("error_bad_delim",
        sprintf(
          lang$errMsg$csvio$errors$badDelim,
          paste(c(private$supportedDelim[private$supportedDelim != "\t"], "tab"),
            collapse = "' '"
          )
        ),
        call. = FALSE
      )
    } else {
      private$rDelim <- delim
    }

    private$rHeaders <- strsplit(private$rSample[1], private$rDelim, fixed = TRUE)[[1L]]
    return(self)
  },
  read = function(path, symName, colsToRead = NULL, hasHeaderRow = TRUE, delim = NULL, decimalSep = NULL, forceInit = FALSE) {
    if (forceInit || !identical(path, private$rpath)) {
      self$rInitFile(path, delim)
    } else if (!is.null(delim)) {
      self$setRDelim(delim)
    }
    if (length(private$rSymName) && !identical(private$rSymName, symName)) {
      stop_custom("error_notfound", "Invalid symbol name", call. = FALSE)
    }
    if (identical(hasHeaderRow, FALSE)) {
      private$rHeaderRow <- hasHeaderRow
    }
    if (!is.null(colsToRead)) {
      self$setColsToRead(colsToRead, symName)
    }
    if (!is.null(decimalSep)) {
      private$decimalSep <- decimalSep
    }
    if (length(private$rDelim) > 1L) {
      stop_custom("error_ambiguous_delim",
        "delimiter could not be uniquely identified. Please use setRDelim method to set manually",
        call. = FALSE
      )
    }
    if (symName %in% c(scalarsFileName, scalarsOutName)) {
      return(private$readScalars(symName))
    }
    return(private$readSymbol(symName))
  },
  setRSymName = function(symName) {
    private$rSymName <- symName
    return(self)
  },
  setRDelim = function(delim) {
    if (delim %in% private$supportedDelim) {
      private$rDelim <- delim
      private$rHeaders <- strsplit(private$rSample[1], private$rDelim, fixed = TRUE)[[1L]]
    } else {
      stop_custom("error_bad_delim",
        sprintf(
          lang$errMsg$csvio$errors$badDelim,
          paste(c(private$supportedDelim[private$supportedDelim != "\t"], "tab"),
            collapse = "' '"
          )
        ),
        call. = FALSE
      )
    }
    return(self)
  },
  setDecimalSep = function(decimalSep) {
    if (!decimalSep %in% c(".", ",")) {
      stop("Decimal separator not supported", call. = FALSE)
    }
    private$decimalSep <- decimalSep
    return(self)
  },
  setHasHeaderRow = function(hasHeaderRow) {
    stopifnot(is.logical(hasHeaderRow), length(hasHeaderRow) == 1L)
    private$rHeaderRow <- hasHeaderRow
    return(self)
  },
  setColsToRead = function(colsToRead, symName) {
    stopifnot(is.character(colsToRead), length(colsToRead) == length(private$metadata[[symName]]$headers))
    colsToSkip <- colsToRead == "-"
    colsToReadTmp <- match(colsToRead[!colsToSkip], private$rHeaders)
    if (any(is.na(colsToReadTmp))) {
      stop(sprintf("Invalid columns: %s", paste(colsToRead[is.na(colsToReadTmp)], collapse = ", ")))
    }
    private$rColsToRead <- colsToReadTmp
    private$rColsToSkip <- which(colsToSkip)
    return(self)
  },
  getRSymName = function() {
    return(private$rSymName)
  },
  getRDelim = function() {
    return(private$rDelim)
  },
  getHeaders = function() {
    return(private$rHeaders)
  },
  getValidExtensions = function() {
    return(c("csv", "tab", "tsv", "psv"))
  }
), private = list(
  rpath = character(0L),
  rSymName = character(0L),
  supportedDelim = c(",", ";", "|", "\t", ":"),
  rSample = character(0L),
  rDelim = ",",
  rHeaderRow = TRUE,
  rHeaders = character(0L),
  rColsToRead = NULL,
  rColsToSkip = NULL,
  decimalSep = ".",
  readSymbol = function(symName) {
    needReorder <- FALSE
    if (is.null(private$rColsToRead)) {
      colTypes <- private$metadata[[symName]]$colTypes
    } else {
      colTypes <- rep.int("-", length(private$rHeaders))
      symColTypes <- strsplit(private$metadata[[symName]]$colTypes, "", fixed = TRUE)[[1]]
      if (length(private$rColsToSkip)) {
        symColTypes <- symColTypes[-private$rColsToSkip]
      }
      colTypes[private$rColsToRead] <- symColTypes
      colOrder <- order(private$rColsToRead)
      if (!identical(colOrder, seq_along(private$rColsToRead))) {
        needReorder <- TRUE
      }
      colTypes <- paste(colTypes, collapse = "")
    }
    if (length(private$rDelim)) {
      data <- suppressWarnings(
        read_delim(private$rpath,
          private$rDelim,
          col_types = colTypes,
          col_names = private$rHeaderRow,
          progress = FALSE,
          skip_empty_rows = TRUE,
          locale = locale(decimal_mark = private$decimalSep)
        )
      )
    } else {
      data <- read_lines(private$rpath,
        progress = FALSE,
        skip_empty_rows = TRUE,
        locale = locale(decimal_mark = private$decimalSep)
      )
      if (private$rHeaderRow) {
        hdrTmp <- data[1]
        data <- as_tibble(data[-1])
        names(data) <- hdrTmp
      } else {
        data <- as_tibble(data)
      }
    }
    symHeaders <- names(private$metadata[[symName]]$headers)
    if (needReorder) {
      data <- data[, colOrder]
    }
    if (!length(private$rColsToSkip)) {
      names(data) <- symHeaders
      return(data)
    }
    names(data) <- symHeaders[-private$rColsToSkip]
    data[, symHeaders[private$rColsToSkip]] <- NA_character_
    colOrder <- match(names(data), symHeaders)
    if (!identical(colOrder, seq_along(symHeaders))) {
      data <- data[, colOrder]
    }
    return(data)
  },
  readScalars = function(symName) {
    scalarsDf <- self$getScalarTemplate(symName)
    scalarsToProcess <- scalarsDf[[1]]
    scalarsDfTmp <- private$readSymbol(symName)
    invalidScalars <- !scalarsDfTmp[[1]] %in% scalarsToProcess
    if (any(invalidScalars)) {
      scalarsDfTmp <- scalarsDfTmp[!invalidScalars, ]
    }
    duplicatedSym <- duplicated(scalarsDfTmp[[1]])
    if (any(duplicatedSym)) {
      stop_custom("error_data",
        sprintf(
          lang$errMsg$xlsio$errors$duplicateScalars,
          paste(scalarsDfTmp[[1]][duplicateSymbols], collapse = "', '")
        ),
        call. = FALSE
      )
    }
    scalarsDf[match(scalarsDfTmp[[1]], scalarsToProcess), 3] <- scalarsDfTmp[[3L]]
    return(scalarsDf)
  },
  guessDelim = function(lines) {
    # taken from vroom::guess_delim (https://github.com/tidyverse/vroom/blob/73c90c4fe490c0588b20ac527c40fcb1c683683e/R/vroom.R#L370)
    # licensed under MIT license (Copyright (c) 2023 vroom authors)
    # more info about license can be found in /src/LICENSE
    # modified slightly to return multiple delimiters if ambiguous
    if (length(lines) == 0) {
      return("")
    }

    # blank text within quotes
    lines <- gsub('"[^"]*"', "", lines)

    splits <- lapply(private$supportedDelim, stri_split_fixed, str = lines)

    counts <- lapply(splits, function(x) table(lengths(x)))

    num_fields <- vapply(counts, function(x) as.integer(names(x)[[1]]), integer(1))

    num_lines <- vapply(counts, function(x) (x)[[1]], integer(1))

    top_lines <- 0
    top_idx <- 0
    for (i in seq_along(private$supportedDelim)) {
      if (num_fields[[i]] >= 2 && num_lines[[i]] > top_lines ||
        (top_lines == num_lines[[i]] && (top_idx[1] <= 0 || num_fields[[top_idx[1]]] <= num_fields[[i]]))) {
        top_lines <- num_lines[[i]]
        if (top_idx[1] > 0) {
          top_idx <- c(top_idx, i)
        } else {
          top_idx <- i
        }
      }
    }
    if (top_idx[1] == 0) {
      character(0L)
    }
    private$supportedDelim[top_idx]
  }
))
