LocalFileIO <- R6::R6Class("LocalFileIO", public = list(
  initialize = function(){
    private$metadata <- c(ioConfig$modelOut, ioConfig$modelIn)
    isClOptScalar <- startsWith(names(ioConfig$modelIn), prefixDDPar) | startsWith(names(ioConfig$modelIn), prefixGMSOpt)
    if(any(isClOptScalar)){
      private$clOptScalars <- names(ioConfig$modelIn)[isClOptScalar]
    }
    if(scalarsFileName %in% names(ioConfig$modelInRaw)){
      private$metadata <- c(private$metadata, ioConfig$modelInRaw[scalarsFileName])
      private$scalars <- c(private$metadata[[scalarsFileName]]$symnames, private$clOptScalars)
      private$metadata[[scalarsFileName]]$symtype <- "set"
      private$metadata[[scalarsFileName]]$colTypes <- "ccc"
    }else{
      private$scalars <- private$clOptScalars
    }
    if(scalarsOutName %in% names(private$metadata)){
      private$scalars <- c(private$scalars, private$metadata[[scalarsOutName]]$symnames)
      private$metadata[[scalarsOutName]]$symtype <- "set"
    }
    return(self)
  }),
  private = list(
    metadata = list(),
    scalars = character(0L),
    clOptScalars = character(0L),
    isTable = function(symName){
      meta <- private$metadata[[symName]]
      numDim <- stri_count_fixed(meta$colTypes, "d")
      return(numDim > 0L &&
               (!identical(names(meta$headers)[length(meta$headers)], "value") ||
                  numDim > 1L))
    }
  )
)
