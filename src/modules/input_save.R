getInputDataFromSandbox <- function() {
  # define temporary list to save input data to
  dataTmp <- lapply(modelInFileNames, function(symName) {
    if (symName %in% names(modelIn)) {
      datasetTmp <- isolate(sandboxInputData$getData(symName)())
      if (!identical(length(datasetTmp), length(modelIn[[symName]]$headers)) ||
        !hasValidHeaderTypes(datasetTmp, modelIn[[symName]]$colTypes)) {
        stop(sprintf("No valid input data found for symbol: %s.", symName), call. = FALSE)
      }
      names(datasetTmp) <- names(modelIn[[symName]]$headers)
      return(datasetTmp)
    }
  })
  names(dataTmp) <- modelInFileNames
  scalarData <- bind_rows(lapply(names(modelIn)[!names(modelIn) %in% modelInFileNames], function(symName) {
    valTmp <- isolate(sandboxInputData$getData(symName)())
    if (is.logical(valTmp)) {
      valTmp <- as.integer(valTmp)
    }
    scalarNames <- symName
    scalarAliases <- tryCatch(modelIn[[symName]][["alias"]], error = function(e) {
      return(symName)
    })
    if (identical(modelIn[[symName]]$type, "daterange") || length(modelIn[[symName]]$slider$default) > 1L) {
      scalarNames <- paste0(scalarNames, c("_lo", "_up"))
      scalarAliases <- paste0(scalarAliases, c(" (lower)", " (upper)"))
    }
    tibble(
      scalar = scalarNames, description = scalarAliases,
      value = if (length(valTmp)) as.character(valTmp) else NA_character_
    )
  }))
  if (nrow(scalarData) > 0L) {
    names(scalarData) <- scalarsFileHeaders
    if (is.null(dataTmp[[length(modelInFileNames)]])) {
      dataTmp[[length(modelInFileNames)]] <- scalarData
    } else {
      dataTmp[[length(modelInFileNames)]] <- bind_rows(
        dataTmp[[length(modelInFileNames)]],
        scalarData
      )
    }
  }
  return(dataTmp)
}
