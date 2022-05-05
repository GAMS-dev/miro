CustomAnalysisData <- R6Class("CustomAnalysisData", public = list(
  initialize = function(scenData, refId) {
    private$scenData <- scenData
    private$refId <- refId
    return(invisible(self))
  },
  getAllSymbols = function() {
    return(private$scenData$getDbSymbols())
  },
  get = function(symName) {
    allSymbols <- self$getAllSymbols()
    if (!symName %in% allSymbols) {
      stop_custom("error_invalid_symbol",
        sprintf("Symbol: %s not found", symName),
        call. = FALSE
      )
    }
    return(private$scenData$getAll(private$refId, symName = symName))
  },
  getMetadata = function() {
    return(unname(private$scenData$getById("meta", refId = private$refId)))
  }
), private = list(
  scenData = NULL,
  refId = NULL
))
