renderDTable <- function(data, options, roundPrecision = 2){
  # Renders the datatable for a dataframe using the provided configuration.
  #
  # Args:
  #   data:                     dataframe that is to be rendered
  #   options:                  options for datatable
  #   roundPrecision:           number of decimal places used for rounding
  #
  # Returns:
  #   DT object or renderDT object with data and options specified
  
  dt <- do.call(datatable, c(list(data), options)) %>%
    formatRound(seq_along(data), roundPrecision)
  
  return(renderDT(dt))
}