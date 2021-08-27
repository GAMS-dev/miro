renderPivot <- function(data, options, height = NULL, roundPrecision = 2, static = FALSE) {
  # Renders the pivottable for a dataframe using the provided configuration.
  #
  # Args:
  #   data:                     dataframe that is to be rendered
  #   options:                  options for pivottable
  #   height:                   height of pivottable
  #   roundPrecision:           number of decimal places used for rounding
  #   static:                   boolean which specifies whether return value is static DT object or renderDT object used
  #                             for reactive programming
  #
  # Returns:
  #   rpivotTable object or renderRpivotTable object used for reactive programming

  # set default height
  if (is.null(height)) {
    height <- pivotDefaultHeight
  }
  if (is.null(options$locale)) {
    "en"
  }
  p <- do.call(rpivotTable, c(list(roundDf(data, roundPrecision)), options))
  if (static) {
    return(p)
  } else {
    return(renderRpivotTable(p))
  }
}
