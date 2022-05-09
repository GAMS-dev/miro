CustomComparisonData <- R6Class("CustomComparisonData", public = list(
  initialize = function(scenData, refId) {
    private$scenData <- scenData
    private$refId <- refId
    return(invisible(self))
  },
  getAllSymbols = function() {
    return(private$scenData$getDbSymbols())
  },
  get = function(symbolName) {
    allSymbols <- self$getAllSymbols()
    if (!symbolName %in% allSymbols) {
      stop_custom("error_invalid_symbol",
        sprintf("Symbol: %s not found", symbolName),
        call. = FALSE
      )
    }
    return(private$scenData$getAll(private$refId, symName = symbolName))
  },
  getMetadata = function() {
    return(unname(private$scenData$getById("meta", refId = private$refId)))
  }
), private = list(
  scenData = NULL,
  refId = NULL
))
