renderDTable <- function(data, options, roundPrecision = 2, render = TRUE, metadata = NULL) {
  # Renders the datatable for a dataframe using the provided configuration.
  #
  # Args:
  #   data:                     dataframe that is to be rendered
  #   options:                  list with options for datatable
  #   roundPrecision:           number of decimal places used for rounding
  #   render:                   boolean that specifies whether to return shiny render object
  #
  # Returns:
  #   DT object or renderDT object with data and options specified
  if (length(metadata)) {
    colHeaders <- vapply(metadata[["headers"]], "[[", character(1L), "alias")
  } else if (length(attr(data, "aliases"))) {
    colHeaders <- attr(data, "aliases")
  } else {
    colHeaders <- names(data)
  }
  if (length(options$pivotCols)) {
    pivotIdx <- match(options$pivotCols[[1]], names(data))[1]
    aliasesTmp <- colHeaders[-c(pivotIdx, length(data))]
    data <- pivot_wider(data,
      names_from = !!pivotIdx,
      values_from = !!length(data),
      names_sort = isTRUE(options$sortPivotCols)
    )
    options[["pivotCols"]] <- NULL
    if (length(aliasesTmp)) {
      names(data)[seq_along(aliasesTmp)] <- aliasesTmp
    }
  } else {
    names(data) <- colHeaders
  }

  if ("DT" %in% (.packages())) {
    dt <- do.call(datatable, c(list(data), options))

    isNumericCol <- vapply(data, is.numeric, logical(1L), USE.NAMES = FALSE)
    if (any(isNumericCol)) {
      dt <- formatRound(dt, seq_along(data)[isNumericCol],
        digits = if (length(options$options$decimals)) options$options$decimals else roundPrecision
      )
    }
    if (render) {
      return(renderDT(dt))
    }
    return(dt)
  }
  data <- roundDf(data, roundPrecision)

  if (render) {
    return(renderDataTable(data, options = options))
  }
  return(datatable(data, options = options))
}
