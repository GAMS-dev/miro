# TODO: scalars, scalars_out and scalarsve_out!!!

GdxIO <- R6::R6Class("GdxIO", public = list(
  initialize = function(gamsSysDir){
    stopifnot(is.character(gamsSysDir), identical(length(gamsSysDir), 1L))
    if(gdxrrw::igdx(gamsSysDir, silent = TRUE, returnStr = FALSE)){
      private$gdxDllLoaded <- TRUE
    }else{
      stop(sprintf("Could not find gdx library in GAMS system directory: '%s'.", 
                   gamsSysDir), call. = FALSE)
    }
    return(invisible(self))
  },
  rgdx = function(gdxName, symName, names = character(0L), pivotHeaders = character(0L)){
    stopifnot(is.character(gdxName), identical(length(gdxName), 1L))
    stopifnot(is.character(symName), identical(length(symName), 1L))
    stopifnot(is.character(names))
    stopifnot(is.character(pivotHeaders))
    
    if(!identical(gdxName, private$rgdxName)){
      private$rgdxName   <- gdxName
      private$gdxSymbols <- gdxInfo(gdxName, returnList = TRUE, dump = FALSE)
    }
    symName <- tolower(symName)
    if(symName %in% tolower(private$gdxSymbols$sets)){
      return(private$rgdxSet(symName, names = names))
    }else if(symName %in% tolower(private$gdxSymbols$parameters)){
      return(private$rgdxParam(symName, names = names, pivotHeaders = pivotHeaders))
    }else if(symName %in% tolower(c(private$gdxSymbols$variables, private$gdxSymbols$equations))){
      return(private$rgdxParam(symName, names = names, pivotHeaders = c("l", "m", "lo", "up", "s")))
    }else{
      stop(sprintf("Symbol: '%s' does not exist in gdx container: '%s'.", symName, gdxName), 
           call. = FALSE)
    }
  },
  wgdx = function(gdxName, data, aliases = character(0L)){
    stopifnot(length(names(data)) > 0L)
    if(!length(aliases)){
      aliases <- names(data)
    }
    do.call(gdxrrw::wgdx, c(gdxName, lapply(seq_along(data), function(i){
      df        <- data[[i]]
      df[seq_len(length(df) - 1L)] <- dplyr::mutate_if(df[seq_len(length(df) - 1L)], 
                                                       is.character, as.factor)
      symName   <- names(data)[i]
      symText   <- aliases[i]
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
    })))
    return(invisible(self))
  }
), private = list(
  rgdxName = character(1L),
  gdxDllLoaded = FALSE,
  gdxSymbols = list(),
  rgdxParam = function(symName, names = NULL, pivotHeaders = character(0L)){
    sym <- gdxrrw::rgdx(private$rgdxName, list(name = symName, compress = FALSE, ts = FALSE),
                        squeeze = FALSE, useDomInfo = TRUE)
    symDim <- sym$dim
    if(identical(symDim, 0L)){
      return(private$rgdxScalar(symName))
    }
    if(length(names) && !length(pivotHeaders)){
      stopifnot(is.character(names), identical(length(names), symDim + 1L))
    }
    
    dflist <- vector("list", symDim + 1L)
    if(identical(dim(sym$val)[1], 0L)){           # empty symbol - no elements
      return(NULL)
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
    if(length(pivotHeaders)){
      dfDim     <- length(symDF)
      pivotUELS <- sym$uels[[dfDim - 1L]]
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
    return(symDF)
  },
  rgdxScalar = function(symName){
    sym <- gdxrrw::rgdx(private$rgdxName, list(name = symName))
    c <- 0
    if(identical(1L, dim(sym$val)[1])){
      c <- sym$val[[1]][1]
    }
    return(c)
  },
  rgdxSet = function(symName, names=NULL){
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
      return(NULL)
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
    
    symDF <- as_tibble(dflist)
    if(length(names)){
      names(symDF) <- names
    }
    return(symDF)
  }
))