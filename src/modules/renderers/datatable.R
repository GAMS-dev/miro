renderDTable <- function(data, options, roundPrecision = 2, render = TRUE){
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
  if(length(attr(data, "aliases"))){
    names(data) <- attr(data, "aliases")
  }
  
  if("DT" %in% (.packages())){
    dt <- do.call(datatable, c(list(data), options))
    
    isNumericCol <- vapply(data, is.numeric, logical(1L), USE.NAMES = FALSE)
    if(any(isNumericCol)){
      dt <- formatRound(dt, seq_along(data)[isNumericCol], 
                        digits = if(length(options$options$decimals)) options$options$decimals else roundPrecision)
    }
    if(render)
      return(renderDT(dt))
    return(dt)
  }
  data <- roundDf(data, roundPrecision)
  
  if(render)
    return(renderDataTable(data, options = options))
  return(datatable(data, options = options))
}