GdxIO <- R6::R6Class("GdxIO", public = list(
  initialize = function(gamsSysDir, metaData){
    stopifnot(is.list(metaData), length(metaData) > 0L)
    stopifnot(is.character(gamsSysDir), identical(length(gamsSysDir), 1L))
    if(gdxrrw::igdx(gamsSysDir, silent = TRUE, returnStr = FALSE)){
      private$gdxDllLoaded <- TRUE
    }else{
      stop(sprintf("Could not find gdx library in GAMS system directory: '%s'.", 
                   gamsSysDir), call. = FALSE)
    }
    private$metaData <- metaData
    return(invisible(self))
  },
  rgdx = function(gdxName, symName, names = character(0L), 
                  pivotHeaders = character(0L), isNewGdx = FALSE){
    stopifnot(is.character(gdxName), identical(length(gdxName), 1L))
    stopifnot(is.character(symName), identical(length(symName), 1L))
    stopifnot(is.logical(isNewGdx), identical(length(isNewGdx), 1L))
    stopifnot(is.character(names))
    stopifnot(is.character(pivotHeaders))
    
    if(isNewGdx || !identical(gdxName, private$rgdxName)){
      private$rgdxName   <- gdxName
      private$gdxSymbols <- gdxrrw::gdxInfo(gdxName, returnList = TRUE, dump = FALSE)
    }
    symName <- tolower(symName)
    if(symName %in% c(scalarsFileName, scalarsOutName)){
      scalarSymbols <- private$metaData[[symName]]
      if(is.null(scalarSymbols)){
        return(tibble::tibble())
      }
      return(tibble::tibble(scalarSymbols$symnames, scalarSymbols$symtext, 
                            vapply(seq_along(scalarSymbols$symnames), function(i){
                              scalar <- NA_character_
                              if(scalarSymbols$symnames[[i]] %in% tolower(private$gdxSymbols$parameters)){
                                return(as.character(private$rgdxScalar(scalarSymbols$symnames[[i]])))
                              }else if(scalarSymbols$symnames[[i]] %in% tolower(private$gdxSymbols$sets)){
                                return(as.character(private$rgdxSet(scalarSymbols$symnames[[i]])[[1]][1]))
                              }
                              return(scalar)
                            }, character(1L), USE.NAMES = FALSE)))
    }else if(identical(symName, scalarEquationsOutName)){
      scalarSymbols <- private$metaData[[symName]]
      values <- lapply(seq_along(scalarSymbols$symnames), function(i){
        scalar <- rep.int(NA_real_, 5L)
        if(scalarSymbols$symnames[[i]] %in% c(tolower(private$gdxSymbols$variables),
                                              tolower(private$gdxSymbols$equations))){
          return(private$rgdxScalarVe(scalarSymbols$symnames[[i]]))
        }
        return(scalar)
      })
      names(values) <- seq_along(values)
      values <- tibble::as_tibble(values)
      values <- t(values)
      colnames(values) <- seq_len(dim(values)[2])
      values <- tibble::as_tibble(values)
      return(dplyr::bind_cols(tibble::tibble(scalarSymbols$symtypes, 
                                             scalarSymbols$symnames, 
                                             scalarSymbols$symtext), 
                              values))
    }
    if(symName %in% tolower(private$gdxSymbols$sets)){
      return(private$rgdxSet(symName, names = names))
    }else if(symName %in% tolower(private$gdxSymbols$parameters)){
      return(private$rgdxParam(symName, names = names, pivotHeaders = pivotHeaders))
    }else if(symName %in% tolower(c(private$gdxSymbols$variables, private$gdxSymbols$equations))){
      return(private$rgdxVe(symName, names = names))
    }else{
      stop(sprintf("Symbol: '%s' does not exist in gdx container: '%s'.", symName, gdxName), 
           call. = FALSE)
    }
  },
  wgdx = function(gdxName, data, squeezeZeros = c('y', 'n', 'e')){
    stopifnot(length(names(data)) > 0L)
    squeezeZeros <- match.arg(squeezeZeros)
    # tabular data
    wgdxDotList <- lapply(seq_along(data), function(i){
      if(!length(data[[i]]) || !nrow(data[[i]])){
        return(NA)
      }
      symName   <- names(data)[i]
      if(symName %in% c(scalarsFileName, scalarsOutName, scalarEquationsOutName)){
        return(NULL)
      }
      if(symName %in% names(private$metaData)){
        symText <- private$metaData[[symName]]$alias
      }else{
        symText <- symName
      }
      df        <- data[[i]]
      df[seq_len(length(df) - 1L)] <- dplyr::mutate_if(df[seq_len(length(df) - 1L)], 
                                                       isNonNumeric, as.factor)
      
      haveTe    <- FALSE
      isSet     <- TRUE
      nc        <- length(df)
      isNumCol  <- vapply(df, is.numeric, logical(1L), USE.NAMES = FALSE)
      noNumCols <- sum(isNumCol)
      
      if(identical(noNumCols, 0L)){
        symType <- "set"
        symDim  <- nc - 1L
        haveTe  <- TRUE
        domains <- names(df)[-length(df)]
      }else if(identical(noNumCols, 1L)){
        symType <- "parameter"
        symDim  <- nc - 1L
        isSet   <- FALSE
        domains <- names(df)[-length(df)]
      }else{
        symType <- "parameter"
        isSet   <- FALSE
        symDim  <- min(which(isNumCol))
        domains <- names(df)[seq_len(symDim)]
        df      <- tidyr::gather(df, key = !!paste0(symName, "Hdr"), 
                                 "value", -seq_len(symDim - 1L), 
                                 factor_key = TRUE)
      }
      ret <- list (name = symName, type = symType, 
                   dim = symDim, ts = symText, 
                   domains = domains, 
                   form = "sparse")
      if(haveTe){
        ret$te <- df[[nc]]
      }
      if(isSet){
        v <- matrix(0L, nrow = nrow(df), 
                    ncol = symDim)
      }else{
        v <- matrix(0L, nrow = nrow(df), 
                    ncol = symDim + 1L)
        v[, symDim + 1L] <- df[[symDim + 1L]]
      }
      ret$uels <- vector("list", symDim)
      ret$uels <- lapply(seq_len(symDim), function(j){
        v[, j] <<- as.numeric(df[[j]])
        return(levels(df[[j]]))
      })
      ret$val  <- v
      return(ret)
    })
    isScalarData <- vapply(wgdxDotList, is.null, 
                           logical(1L), USE.NAMES = FALSE)
    isEmptySymbol <- is.na(wgdxDotList)
    # scalar data
    scalarSymList <- unlist(lapply(which(isScalarData), function(i){
      if(identical(isEmptySymbol[[i]], TRUE)){
        return(NA)
      }
      df       <- data[[i]]
      nc       <- length(df)
      if(identical(nc, 3L)){
        symNames <- df[[1L]]
        symTexts <- df[[2L]]
        symVals  <- df[[3L]]
        symTypes <- NULL
      }else{
        symTypes <- df[[1L]]
        symNames <- df[[2L]]
        symTexts <- df[[3L]]
        df       <- df[-seq_len(3)]
      }
      lapply(seq_along(symNames), function(j){
        symName  <- symNames[j]
        form     <- "sparse"
        dim      <- 0L
        uels     <- NULL
        typeCode <- NULL
        if(is.null(symTypes)){
          v     <- suppressWarnings(as.numeric(symVals[j]))
          if(is.na(v)){
            # element is singleton set
            dim     <- 1L
            v       <- matrix(1L)
            uels    <- list(symVals[j])
            symType <- "set"
          }else{
            form    <- "full"
            symType <- "parameter"
          }
        }else{
          typeCode <- 0L
          symType  <- if(startsWith(symTypes[j], "va")) "variable" else "equation"
          df       <- tidyr::gather(df[j, ], key = "field", 
                                    "value", factor_key = TRUE)
          uels     <- list(c('l', 'm', 'lo', 'up', 's'))
          v        <- data.matrix(df)
        }
        ret <- list(name = symName, ts = symTexts[j], 
                    type = symType, dim = dim, 
                    form = form, val = v)
        if(!is.null(uels)){
          ret$uels <- uels
        }
        if(!is.null(typeCode)){
          ret$typeCode <- typeCode
        }
        return(ret)
      })
    }), use.names = FALSE, recursive = FALSE)
    
    scalarSymList <- scalarSymList[!is.na(scalarSymList)]
    wgdxDotList  <- wgdxDotList[!(isScalarData | isEmptySymbol)]
    do.call(gdxrrw::wgdx, c(gdxName, wgdxDotList, scalarSymList, list(squeeze = squeezeZeros)))
    return(invisible(self))
  }
), private = list(
  rgdxName = character(1L),
  gdxDllLoaded = FALSE,
  gdxSymbols = list(),
  metaData = list(),
  rgdxVe = function(symName, names = NULL){
    sym <- gdxrrw::rgdx(private$rgdxName, list(name = symName, compress = FALSE, ts = FALSE, field = "all"),
                        squeeze = FALSE, useDomInfo = TRUE)
    symDim <- sym$dim
    if(identical(symDim, 0L)){
      return(private$rgdxScalarVe(sym = sym))
    }
    private$rgdxTibble(sym, symDim + 1L, names, c("l", "m", "lo", "up", "s"))
  },
  rgdxScalarVe = function(symName = NULL, sym = NULL){
    if(!length(sym)){
      sym <- gdxrrw::rgdx(private$rgdxName, list(name = symName, compress = FALSE, 
                                                 ts = FALSE, field = "all"),
                          squeeze = FALSE, useDomInfo = TRUE)
    }
    return(sym$val[, 2L])
  },
  rgdxParam = function(symName, names = NULL, pivotHeaders = character(0L)){
    sym <- gdxrrw::rgdx(private$rgdxName, list(name = symName, compress = FALSE, ts = FALSE),
                        squeeze = FALSE, useDomInfo = TRUE)
    symDim <- sym$dim
    if(identical(symDim, 0L)){
      return(private$rgdxScalar(sym = sym))
    }
    private$rgdxTibble(sym, symDim, names, pivotHeaders)
  },
  rgdxTibble = function(sym, symDim, names = NULL, pivotHeaders = character(0L)){
    if(length(names) && !length(pivotHeaders)){
      stopifnot(is.character(names), identical(length(names), symDim + 1L))
    }
    dflist <- vector("list", symDim + 1L)
    if(identical(dim(sym$val)[1], 0L)){           # empty symbol - no elements
      return(tibble())
    }else{
      dflist[seq_len(symDim)] <- lapply(seq_len(symDim), function(d){
        # first arg to factor must be integer, not numeric: different as.character results
        factor(as.integer(sym$val[, d]), seq(to = length(sym$uels[[d]])), 
               labels = sym$uels[[d]])
      })
    }
    dflist[[symDim + 1L]] <- sym$val[, symDim + 1L]
    names(dflist) <- seq_along(dflist)
    symDF <- tibble::as_tibble(dflist)
    symDF <- dplyr::mutate_if(symDF, is.factor, as.character)
    if(length(pivotHeaders)){
      dfDim     <- length(symDF)
      symDF     <- tidyr::spread(symDF, !!length(symDF) - 1L, 
                                 !!length(symDF), fill = 0L)
      nonPivotedColNames <- names(symDF)[seq_len(dfDim - 2L)]
      
      # append non pivot column names in case only pivot column names were specified
      pivotHeaders <- c(nonPivotedColNames, tolower(pivotHeaders))
      newIdx    <- match(pivotHeaders, 
                         c(nonPivotedColNames, tolower(names(symDF)[seq(dfDim - 1L, length(symDF))])))
      if(any(is.na(newIdx))){
        errMsg <- sprintf("Dataset: '%s' has invalid headers. Headers are: '%s'. Headers should be: '%s'.", 
                          sym$name, paste(names(symDF), collapse = "', '"), 
                          paste(pivotHeaders, collapse = "', '"))
        flog.warn(errMsg)
        stop(errMsg, call. = FALSE)
      }else{
        symDF <- symDF[, newIdx]
      }
    }else if(length(names)){
      names(symDF) <- names
    }
    return(type_convert(symDF, cols()))
  },
  rgdxScalar = function(symName = NULL, sym = NULL){
    if(!length(sym)){
      sym <- gdxrrw::rgdx(private$rgdxName, list(name = symName))
    }
    c <- 0
    if(identical(1L, dim(sym$val)[1])){
      c <- sym$val[[1]][1]
    }
    return(c)
  },
  rgdxSet = function(symName, names = NULL){
    sym <- gdxrrw::rgdx(private$rgdxName, list(name = symName, 
                                               compress = FALSE, 
                                               ts = FALSE, te = TRUE),
                        squeeze = FALSE, useDomInfo = TRUE)
    symDim <- sym$dim
    
    if(length(names)){
      stopifnot(is.character(names), identical(length(names), symDim + 1L))
    }
    dflist <- vector("list", symDim + 1L)
    if(identical(dim(sym$val)[1], 0L)){           # empty symbol - no elements
      return(tibble())
    }else{
      dflist[seq_len(symDim)] <- lapply(seq_len(symDim), function(d){
        # first arg to factor must be integer, not numeric: different as.character results
        factor(as.integer(sym$val[,d]), 
               seq(to = length(sym$uels[[d]])), 
               labels=sym$uels[[d]])
      })
    }
    dflist[[symDim + 1L]] <- sym$te
    names(dflist) <- seq_along(dflist)
    symDF <- tibble::as_tibble(dflist)
    symDF <- dplyr::mutate_if(symDF, is.factor, as.character)
    if(length(names)){
      names(symDF) <- names
    }
    return(type_convert(symDF, cols()))
  }
))
