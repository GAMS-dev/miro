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
        if (LAUNCHHCUBEMODE) {
          scalarsTmp <- private$getSpecialScalars(scalarMeta[[1]]$symnames)
          scalarId <- match(scalarsTmp, scalarMeta[[1]]$symnames)
          scalarMeta[[1]]$symnames <- scalarsTmp
          scalarMeta[[1]]$symtypes <- scalarMeta[[1]]$symtypes[scalarId]
          scalarMeta[[1]]$symtypes[is.na(scalarMeta[[1]]$symtypes)] <- "parameter"
          scalarMeta[[1]]$symtext <- scalarMeta[[1]]$symtext[scalarId]
          scalarMeta[[1]]$symtext[is.na(scalarMeta[[1]]$symtext)] <- ""
        } else {
          scalarsTmp <- scalarMeta[[1]]$symnames
        }
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
