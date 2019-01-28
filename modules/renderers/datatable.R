renderDTable <- function(data, options, roundPrecision = 2){
  # Renders the datatable for a dataframe using the provided configuration.
  #
  # Args:
  #   data:                     dataframe that is to be rendered
  #   options:                  list with options for datatable
  #   roundPrecision:           number of decimal places used for rounding
  #
  # Returns:
  #   DT object or renderDT object with data and options specified
  names(data) <- attr(data, "aliases")
  if("DT" %in% (.packages())){
    dt <- do.call(datatable, c(list(data), options)) %>%
      formatRound(seq_along(data)[vapply(data, is.numeric, logical(1L), USE.NAMES = FALSE)], 
                  digits = roundPrecision)
    
    return(renderDT(dt))
  }else{
    data <- roundDf(data, roundPrecision)
    return(renderDataTable(data, options = options))
  }
}