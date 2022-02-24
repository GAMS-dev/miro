GdxIO <- R6::R6Class("GdxIO", public = list(
  initialize = function(libDir, metaData,
                        scalarsFileName, scalarsOutName,
                        scalarEquationsName,
                        scalarEquationsOutName,
                        dropdownAliases) {
    stopifnot(
      is.list(metaData), length(metaData) > 0L,
      is.character(libDir), identical(length(libDir), 1L)
    )
    if (gdxrrwMIRO::igdx(libDir, silent = TRUE, returnStr = FALSE)) {
      private$gdxDllLoaded <- TRUE
    } else {
      stop(sprintf(
        "Could not find gdx library in: '%s'.",
        libDir
      ), call. = FALSE)
    }
    private$metaData <- metaData
    private$scalarsFileName <- scalarsFileName
    private$scalarsOutName <- scalarsOutName
    private$scalarEquationsName <- scalarEquationsName
    private$scalarEquationsOutName <- scalarEquationsOutName
    private$dropdownAliases <- dropdownAliases
    return(invisible(self))
  },
  getSymbols = function(gdxName = NULL) {
    if (length(gdxName)) {
      return(gdxrrwMIRO::gdxInfo(nativeFileEnc(gdxName), returnList = TRUE, dump = FALSE))
    }
    return(private$gdxSymbols)
  },
  rgdx = function(gdxName, symName, names = character(0L),
                  isNewGdx = FALSE) {
    stopifnot(
      is.character(gdxName), identical(length(gdxName), 1L),
      is.character(symName), identical(length(symName), 1L),
      is.logical(isNewGdx), identical(length(isNewGdx), 1L)
    )

    if (isNewGdx || !identical(gdxName, private$rgdxName)) {
      private$rgdxName <- nativeFileEnc(gdxName)
      private$gdxSymbols <- gdxrrwMIRO::gdxInfo(nativeFileEnc(gdxName), returnList = TRUE, dump = FALSE)
    }
    symName <- tolower(symName)
    if (symName %in% c(private$scalarsFileName, private$scalarsOutName)) {
      scalarSymbols <- private$metaData[[symName]]
      if (is.null(scalarSymbols)) {
        return(tibble::tibble())
      }
      return(tibble::tibble(
        scalarSymbols$symnames, scalarSymbols$symtext,
        vapply(seq_along(scalarSymbols$symnames), function(i) {
          scalar <- NA_character_
          if (scalarSymbols$symnames[[i]] %in% tolower(private$gdxSymbols$parameters)) {
            return(as.character(private$rgdxScalar(scalarSymbols$symnames[[i]])))
          } else if (scalarSymbols$symnames[[i]] %in% tolower(private$gdxSymbols$sets)) {
            scalarTmp <- private$rgdxSet(scalarSymbols$symnames[[i]])
            if (length(scalarTmp)) {
              if (length(scalarTmp) == 2 && !is.na(nchar(scalarTmp[[2]][1])) &&
                nchar(scalarTmp[[2]][1])) {
                return(paste0(
                  as.character(scalarTmp[[1]])[1], "||",
                  as.character(scalarTmp[[2]])[1]
                ))
              }
              return(as.character(scalarTmp[[1]])[1])
            }
          }
          return(scalar)
        }, character(1L), USE.NAMES = FALSE)
      ))
    } else if (identical(symName, scalarEquationsOutName)) {
      scalarSymbols <- private$metaData[[symName]]
      values <- lapply(seq_along(scalarSymbols$symnames), function(i) {
        if (scalarSymbols$symnames[[i]] %in% c(
          tolower(private$gdxSymbols$variables),
          tolower(private$gdxSymbols$equations)
        )) {
          return(private$rgdxScalarVe(scalarSymbols$symnames[[i]]))
        }
        return(NULL)
      })
      names(values) <- seq_along(values)
      salarVeFound <- !vapply(values, is.null, logical(1L), USE.NAMES = FALSE)
      values <- values[salarVeFound]
      if (!length(values)) {
        return(tibble())
      }
      values <- tibble::as_tibble(values)
      values <- t(values)
      colnames(values) <- seq_len(dim(values)[2])
      values <- tibble::as_tibble(values)
      return(dplyr::bind_cols(
        tibble::tibble(
          scalarSymbols$symnames[salarVeFound],
          scalarSymbols$symtext[salarVeFound]
        ),
        values
      ))
    }
    if (symName %in% tolower(private$gdxSymbols$sets)) {
      return(private$rgdxSet(symName, names = names))
    } else if (symName %in% tolower(private$gdxSymbols$parameters)) {
      return(private$rgdxParam(symName, names = names))
    } else if (symName %in% tolower(c(private$gdxSymbols$variables, private$gdxSymbols$equations))) {
      return(private$rgdxVe(symName, names = names))
    } else {
      stop(sprintf("Symbol: '%s' does not exist in gdx container: '%s'.", symName, gdxName),
        call. = FALSE
      )
    }
  },
  wgdx = function(gdxName, data, squeezeZeros = c("n", "y", "e")) {
    if (!length(names(data))) {
      gdxrrwMIRO::wgdx(nativeFileEnc(gdxName), list())
      return(invisible(self))
    }
    squeezeZeros <- match.arg(squeezeZeros)
    # tabular data
    scalarIdList <- NULL
    wgdxDotList <- lapply(seq_along(data), function(i) {
      if (!length(data[[i]])) {
        return(NA)
      }
      symName <- names(data)[i]
      if (symName %in% c(
        private$scalarsFileName, private$scalarsOutName,
        private$scalarEquationsName,
        private$scalarEquationsOutName
      )) {
        scalarIdList <<- c(scalarIdList, symName)
        return(NULL)
      }
      if (symName %in% names(private$metaData)) {
        symText <- private$metaData[[symName]]$alias
        symType <- private$metaData[[symName]]$symtype
      } else if (startsWith(symName, "_")) {
        return(NA)
      } else {
        stop(sprintf("Symbol: '%s' not found.", symName), call. = FALSE)
      }
      df <- data[[i]]
      names(df) <- names(private$metaData[[symName]]$headers)

      haveTe <- FALSE
      isSet <- TRUE
      nc <- length(df)
      isNumCol <- vapply(df, is.numeric, logical(1L), USE.NAMES = FALSE)
      noNumCols <- sum(isNumCol)

      if (symType == "set") {
        symDim <- nc - 1L
        haveTe <- TRUE
        domains <- names(df)[-length(df)]
      } else if (noNumCols == 1) {
        symDim <- nc - 1L
        isSet <- FALSE
        domains <- names(df)[-length(df)]
      } else {
        symDim <- min(which(isNumCol))
        isSet <- FALSE
        domains <- names(df)[seq_len(symDim)]
        df <- tidyr::pivot_longer(df, -seq_len(symDim - 1L),
          names_to = paste0(symName, "Hdr"),
          values_to = "value", values_drop_na = TRUE
        )
      }
      df[seq_len(length(df) - 1L)] <- dplyr::mutate(
        df[seq_len(length(df) - 1L)],
        across(where(function(el) {
          if (is.numeric(el)) {
            return(FALSE)
          }
          return(TRUE)
        }), ~ factor(.x, levels = unique(.x)))
      )
      ret <- list(
        name = symName, type = symType,
        ts = symText,
        domains = domains,
        form = "sparse"
      )
      if (haveTe) {
        ret$te <- df[[nc]]
      }
      if (isSet) {
        v <- matrix(0L,
          nrow = nrow(df),
          ncol = symDim
        )
      } else {
        v <- matrix(0L,
          nrow = nrow(df),
          ncol = symDim + 1L
        )
        v[, symDim + 1L] <- df[[symDim + 1L]]
      }
      ret$uels <- vector("list", symDim)
      ret$uels <- lapply(seq_len(symDim), function(j) {
        v[, j] <<- suppressWarnings(as.numeric(df[[j]]))
        return(levels(df[[j]]))
      })
      if (symType %in% c("variable", "equation")) {
        ret$dim <- symDim - 1L
        ret$typeCode <- 0L
        ret$domains[symDim] <- "_field"
        ret$uels[[symDim]] <- c("l", "m", "lo", "up", "s")
      } else {
        ret$dim <- symDim
      }
      ret$val <- v
      return(ret)
    })
    isScalarData <- FALSE
    if (length(scalarIdList)) {
      isScalarData <- vapply(wgdxDotList, is.null,
        logical(1L),
        USE.NAMES = FALSE
      )
    }
    isEmptySymbol <- is.na(wgdxDotList)

    # scalar data
    j <- 1L
    scalarSymList <- unlist(lapply(which(isScalarData), function(i) {
      if (identical(isEmptySymbol[[i]], TRUE)) {
        return(NA)
      }
      df <- data[[i]]
      nc <- length(df)

      symNames <- df[[1L]]
      inclSymNames <- !startsWith(symNames, "_")
      symNames <- symNames[inclSymNames]
      symIds <- match(symNames, private$metaData[[scalarIdList[[j]]]]$symnames)
      if (any(is.na(symIds))) {
        stop(sprintf(
          "Symbol(s): '%s' was not found in model data contract",
          paste(symNames[is.na(symIds)], collapse = "', '")
        ), call. = FALSE)
      }
      symTypes <- private$metaData[[scalarIdList[[j]]]]$symtypes[symIds]

      symValsRaw <- character()
      if (identical(nc, 3L)) {
        symTexts <- df[[2L]][inclSymNames]
        symValsRaw <- df[[3L]][inclSymNames]
        symVals <- strsplit(symValsRaw, "||", fixed = TRUE)
      } else {
        symTexts <- df[[2L]]
        df <- df[-seq_len(3)]
      }
      j <<- j + 1L
      lapply(seq_along(symNames), function(j) {
        symName <- symNames[j]
        symType <- symTypes[j]
        metaId <- match(
          symName,
          private$metaData[[private$scalarsFileName]]$symNames
        )

        form <- "sparse"
        dim <- 0L
        uels <- NULL
        te <- NULL
        typeCode <- NULL
        if (symType %in% c("variable", "equation")) {
          typeCode <- 0L
          df <- tidyr::gather(df[j, ],
            key = "field",
            "value", factor_key = TRUE,
            na.rm = TRUE
          )
          uels <- list(c("l", "m", "lo", "up", "s"))
          v <- data.matrix(df)
        } else if (symType == "parameter") {
          v <- suppressWarnings(as.numeric(symVals[[j]][1]))
          form <- "full"
        } else if (symType == "set") {
          # element is singleton set
          dim <- 1L
          v <- matrix(1L)
          symVal <- symVals[[j]]
          if (symName %in% names(private$dropdownAliases) &&
            !isTRUE(private$dropdownAliases[[symName]]$clearValue)) {
            aliasId <- match(symVal[1], private$dropdownAliases[[symName]]$choices)
            if (is.na(aliasId)) {
              uels <- list(symVal[1])
              te <- paste0(symVal[-1], collapse = "||")
            } else {
              uels <- list(symVal[1])
              te <- private$dropdownAliases[[symName]]$aliases[[aliasId]]
            }
          } else if (symName %in% ioConfig$textOnlySymbols) {
            uels <- list("")
            te <- symValsRaw[[j]]
          } else {
            uels <- list(symVal[1])
            te <- paste0(symVal[-1], collapse = "||")
          }
        } else {
          stop(sprintf("Unknown symbol type: '%s'.", symType),
            call. = FALSE
          )
        }
        ret <- list(
          name = symName, ts = symTexts[j],
          type = symType, dim = dim,
          form = form, val = v
        )
        if (!is.null(uels)) {
          ret$uels <- uels
        }
        if (!is.null(te)) {
          ret$te <- te
        }
        if (!is.null(typeCode)) {
          ret$typeCode <- typeCode
        }
        return(ret)
      })
    }), use.names = FALSE, recursive = FALSE)

    scalarSymList <- scalarSymList[!is.na(scalarSymList)]
    wgdxDotList <- wgdxDotList[!(isScalarData | isEmptySymbol)]
    tryCatch(
      do.call(gdxrrwMIRO::wgdx, c(nativeFileEnc(gdxName), wgdxDotList, scalarSymList, list(squeeze = squeezeZeros))),
      error = function(e) {
        errMsg <- conditionMessage(e)
        if (startsWith(errMsg, "GDXRRW:wgdx:GDXDupError:")) {
          errMsg <- stri_split_fixed(substr(errMsg, 25L, nchar(errMsg)), "\n")[[1L]]
          if (errMsg[[1]] %in% names(private$metaData)) {
            errMsg[[1]] <- sprintf(
              lang$errMsg$gdxio$errors$duplicateRecords,
              private$metaData[[errMsg[[1]]]]$alias
            )
          } else {
            flog.warn("Could not find symbol: '%s' that was reported to have duplicate records in data contract.", errMsg[[1]])
            errMsg[[1]] <- sprintf(
              lang$errMsg$gdxio$errors$duplicateRecords,
              errMsg[[1]]
            )
          }
          if (length(errMsg) > 11L) {
            errMsg <- paste0(
              paste(errMsg[1:11], collapse = "\n"),
              "\n\n",
              lang$errMsg$gdxio$errors$duplicateRecordsTruncated
            )
          } else {
            errMsg <- paste(errMsg, collapse = "\n")
          }
          stop_custom("error_duplicate_records", errMsg)
        }
        stop(errMsg)
      }
    )
    return(invisible(self))
  }
), private = list(
  rgdxName = character(1L),
  gdxDllLoaded = FALSE,
  gdxSymbols = list(),
  metaData = list(),
  scalarsFileName = character(1L),
  scalarsOutName = character(1L),
  scalarEquationsName = character(1L),
  scalarEquationsOutName = character(1L),
  dropdownAliases = list(),
  rgdxVe = function(symName, names = NULL) {
    sym <- gdxrrwMIRO::rgdx(private$rgdxName,
      list(
        name = symName,
        compress = FALSE,
        ts = FALSE,
        field = "all"
      ),
      squeeze = FALSE, useDomInfo = TRUE
    )
    symDim <- sym$dim
    if (identical(symDim, 0L)) {
      return(private$rgdxScalarVe(sym = sym))
    }
    private$rgdxTibble(sym, symDim + 1L, names)
  },
  rgdxScalarVe = function(symName = NULL, sym = NULL) {
    if (!length(sym)) {
      sym <- gdxrrwMIRO::rgdx(private$rgdxName, list(
        name = symName, compress = FALSE,
        ts = FALSE, field = "all"
      ),
      squeeze = FALSE, useDomInfo = TRUE
      )
    }
    return(sym$val[, 2L])
  },
  rgdxParam = function(symName, names = NULL) {
    sym <- gdxrrwMIRO::rgdx(private$rgdxName, list(name = symName, compress = FALSE, ts = FALSE),
      squeeze = FALSE, useDomInfo = TRUE
    )
    symDim <- sym$dim
    if (identical(symDim, 0L)) {
      return(private$rgdxScalar(sym = sym))
    }
    private$rgdxTibble(sym, symDim, names)
  },
  rgdxTibble = function(sym, symDim, names = NULL) {
    # check if symbol has to be pivoted
    symHeaders <- private$metaData[[tolower(sym$name)]]$headers
    pivotHeaders <- character(0L)

    if (sym$type %in% c("variable", "equation")) {
      pivotHeaders <- c("l", "m", "lo", "up", "s")
    } else {
      noNonNumCols <- sum(vapply(symHeaders, function(el) {
        identical(el$type, "string")
      }, logical(1L), USE.NAMES = FALSE))
      if (length(symHeaders) - noNonNumCols > 1L) {
        # symbol shall be pivoted
        pivotHeaders <- names(symHeaders)[-seq_len(noNonNumCols)]
      }
    }

    if (length(names) && !length(pivotHeaders)) {
      stopifnot(is.character(names), identical(length(names), symDim + 1L))
    }
    dflist <- vector("list", symDim + 1L)
    if (identical(dim(sym$val)[1], 0L)) { # empty symbol - no elements
      return(tibble())
    } else {
      dflist[seq_len(symDim)] <- lapply(seq_len(symDim), function(d) {
        # first arg to factor must be integer, not numeric: different as.character results
        factor(as.integer(sym$val[, d]), seq(to = length(sym$uels[[d]])),
          labels = sym$uels[[d]]
        )
      })
    }
    dflist[[symDim + 1L]] <- sym$val[, symDim + 1L]
    names(dflist) <- paste0("\U2009", seq_along(dflist))
    symDF <- tibble::as_tibble(dflist)
    symDF <- dplyr::mutate(symDF, across(where(is.factor), as.character))
    if (length(pivotHeaders)) {
      dfDim <- length(symDF)
      symDF <- tidyr::pivot_wider(symDF,
        names_from = !!length(symDF) - 1L,
        values_from = !!length(symDF)
      )
      nonPivotedColNames <- names(symDF)[seq_len(dfDim - 2L)]

      # append non pivot column names in case only pivot column names were specified
      pivotHeaders <- c(nonPivotedColNames, tolower(pivotHeaders))
      tableColNames <- c(nonPivotedColNames, tolower(names(symDF)[seq(
        dfDim - 1L,
        length(symDF)
      )]))
      if (any(!tableColNames %in% pivotHeaders)) {
        errMsg <- sprintf(
          "Dataset: '%s' has invalid headers. Headers are: '%s'. Headers should be: '%s'.",
          sym$name, paste(names(symDF), collapse = "', '"),
          paste(pivotHeaders, collapse = "', '")
        )
        flog.warn(errMsg)
        stop(errMsg, call. = FALSE)
      }
      newIdx <- match(
        pivotHeaders,
        tableColNames
      )

      if (length(tableColNames) != length(pivotHeaders)) {
        missingCols <- pivotHeaders[is.na(newIdx)]
        # add missing columns (columns with only zeros are squeezed out in gdx)
        missingCols <- setNames(
          rep.int(NA_real_, length(missingCols)),
          missingCols
        )
        symDF <- tibble::add_column(symDF, !!!missingCols)
        newIdx <- match(
          pivotHeaders,
          c(tableColNames, names(missingCols))
        )
      }
      symDF <- symDF[, newIdx]
    } else if (length(names)) {
      names(symDF) <- names
    }
    return(symDF)
  },
  rgdxScalar = function(symName = NULL, sym = NULL) {
    if (!length(sym)) {
      sym <- gdxrrwMIRO::rgdx(private$rgdxName, list(name = symName))
    }
    c <- 0
    if (identical(1L, dim(sym$val)[1])) {
      c <- sym$val[[1]][1]
    }
    return(c)
  },
  rgdxSet = function(symName, names = NULL) {
    sym <- gdxrrwMIRO::rgdx(private$rgdxName, list(
      name = symName,
      compress = FALSE,
      ts = FALSE, te = TRUE
    ),
    squeeze = FALSE, useDomInfo = TRUE
    )
    symDim <- sym$dim
    if (length(names)) {
      stopifnot(is.character(names), identical(length(names), symDim + 1L))
    }
    dflist <- vector("list", symDim + 1L)
    if (identical(dim(sym$val)[1], 0L)) { # empty symbol - no elements
      return(tibble())
    } else {
      dflist[seq_len(symDim)] <- lapply(seq_len(symDim), function(d) {
        # first arg to factor must be integer, not numeric: different as.character results
        factor(as.integer(sym$val[, d]),
          seq(to = length(sym$uels[[d]])),
          labels = sym$uels[[d]]
        )
      })
    }
    dflist[[symDim + 1L]] <- sym$te
    names(dflist) <- seq_along(dflist)
    symDF <- tibble::as_tibble(dflist)

    symDF <- dplyr::mutate(symDF, across(where(is.factor), as.character))
    if (symName %in% ioConfig$textOnlySymbols) {
      symDF <- symDF[, c(2, 1)]
    }
    if (length(names)) {
      names(symDF) <- names
    }
    if (symName %in% names(private$dropdownAliases) && length(symDF) == 2L) {
      choiceId <- match(symDF[[1L]], private$dropdownAliases[[symName]]$choices)
      hasChoices <- !is.na(choiceId)
      if (any(hasChoices)) {
        symAliases <- private$dropdownAliases[[symName]]$aliases[choiceId[hasChoices]]
        symDF[which(hasChoices), 2L] <- symAliases
      }
    }
    return(symDF)
  }
))
