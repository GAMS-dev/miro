LocalFileIO <- R6::R6Class("LocalFileIO",
  public = list(
    initialize = function() {
      private$metadata <- c(ioConfig$modelOut, ioConfig$modelIn)
      isClOptScalar <- startsWith(names(ioConfig$modelIn), prefixDDPar) | startsWith(names(ioConfig$modelIn), prefixGMSOpt)
      if (any(isClOptScalar)) {
        private$clOptScalars <- private$getSpecialScalars(names(ioConfig$modelIn)[isClOptScalar])
      }

      if (scalarsFileName %in% names(ioConfig$modelInRaw)) {
        scalarMeta <- ioConfig$modelInRaw[scalarsFileName]
        scalarMeta[[1]]$symtype <- "set"
        scalarMeta[[1]]$colTypes <- "ccc"
        scalarsTmp <- scalarMeta[[1]]$symnames
        scalarMetaId <- match(scalarsFileName, names(private$metadata))
        if (is.na(scalarMetaId)) {
          # no scalars sheet in metadata
          private$metadata <- c(private$metadata, scalarMeta)
        } else {
          private$metadata <- c(private$metadata[-scalarMetaId], scalarMeta)
        }
        private$scalars <- c(scalarsTmp, private$clOptScalars)
      } else {
        private$scalars <- private$clOptScalars
      }
      if (scalarsOutName %in% names(private$metadata)) {
        private$scalars <- c(private$scalars, private$metadata[[scalarsOutName]]$symnames)
        private$metadata[[scalarsOutName]]$symtype <- "set"
      }
      return(self)
    },
    getScalarTemplate = function(symName) {
      scalarsToProcess <- private$metadata[[symName]]$symnames
      scalarDesc <- private$metadata[[symName]]$symtext
      if (identical(symName, scalarsFileName) && length(private$clOptScalars)) {
        scalarsToProcess <- c(scalarsToProcess, private$clOptScalars)
        scalarDesc <- c(scalarDesc, rep.int("", length(private$clOptScalars)))
      }
      return(tibble(
        scalar = scalarsToProcess,
        description = scalarDesc,
        value = NA_character_
      ))
    },
    fixScalarDf = function(data, symName, ignoreInvalidScalars = FALSE) {
      scalarColumnName <- names(private$metadata[[symName]]$headers)[1]
      valueColumnName <- names(private$metadata[[symName]]$headers)[3]
      requiredColumnsFound <- c(scalarColumnName, valueColumnName) %in% names(data)
      if (!all(requiredColumnsFound)) {
        if (ignoreInvalidScalars) {
          return(self$getScalarTemplate(symName))
        } else {
          stop_custom("error_validation", sprintf(
            "Invalid scalar dataframe. Required column(s): %s not found.",
            paste(c(scalarColumnName, valueColumnName)[!requiredColumnsFound], collapse = ", ")
          ))
        }
      }
      scalarDfTmp <- self$getScalarTemplate(symName)
      scalarIds <- match(data[[scalarColumnName]], scalarDfTmp[[1L]])
      isInvalidScalar <- is.na(scalarIds)
      scalarIds <- scalarIds[!isInvalidScalar]
      if (any(isInvalidScalar)) {
        invalidScalars <- paste(data[[scalarColumnName]][isInvalidScalar],
          collapse = ", "
        )
        if (ignoreInvalidScalars) {
          flog.info("Invalid scalar(s) found in scalar dataframe: %s", invalidScalars)
        } else {
          stop_custom("error_validation", sprintf(
            "Invalid scalar(s) found in scalar dataframe: %s.",
            invalidScalars
          ))
        }
      }
      scalarDfTmp[scalarIds, 3] <- as.character(data[[valueColumnName]][!isInvalidScalar])
      return(scalarDfTmp)
    }
  ),
  private = list(
    metadata = list(),
    scalars = character(0L),
    clOptScalars = character(0L),
    isTable = function(symName) {
      meta <- private$metadata[[symName]]
      numDim <- stri_count_fixed(meta$colTypes, "d")
      return(numDim > 0L &&
        (!identical(names(meta$headers)[length(meta$headers)], "value") ||
          numDim > 1L))
    },
    getSpecialScalars = function(specialScalars) {
      return(unlist(lapply(specialScalars, function(specialScalar) {
        if (identical(ioConfig$modelIn[[specialScalar]]$type, "daterange")) {
          return(paste0(specialScalar, c("_lo", "_up")))
        }
        if (!identical(ioConfig$modelIn[[specialScalar]]$type, "slider") ||
          !length(ioConfig$modelIn[[specialScalar]]$slider$default) > 1) {
          if (specialScalar %in% ioConfig$hcubeScalars) {
            return(NULL)
          }
          return(specialScalar)
        }
        if (isTRUE(ioConfig$modelIn[[specialScalar]]$slider$single)) {
          return(paste0(specialScalar, c("$lo", "$up", "$step")))
        }
        if (isTRUE(ioConfig$modelIn[[specialScalar]]$slider$double)) {
          return(paste0(specialScalar, c("_lo", "_up", "$step", "$mode")))
        }
        return(paste0(specialScalar, c("_lo", "_up")))
      }), use.names = FALSE))
    }
  )
)
