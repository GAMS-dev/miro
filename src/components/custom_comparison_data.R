CustomComparisonData <- R6Class("CustomComparisonData", public = list(
  initialize = function(scenData, refId, attachments) {
    private$scenData <- scenData
    private$refId <- refId
    private$attachments <- attachments
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
  getAttachmentData = function(scenIds = NULL, fileNames = NULL, includeContent = FALSE, includeSandboxScen = TRUE) {
    if (is.null(scenIds)) {
      scenIds <- vapply(self$getMetadata(), "[[", integer(1L), "_sid")
    }
    return(private$attachments$getData(scenIds, fileNames, includeContent, includeSandboxScen))
  },
  getMetadata = function() {
    return(unname(private$scenData$getById("meta", refId = private$refId)))
  }
), private = list(
  scenData = NULL,
  refId = NULL,
  attachments = NULL
))
