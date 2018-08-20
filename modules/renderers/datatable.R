renderDTable <- function(data, options, height = NULL, roundPrecision = 2){
  # Renders the datatable for a dataframe using the provided configuration.
  #
  # Args:
  #   data:                     dataframe that is to be rendered
  #   options:                  options for datatable
  #   height:                   height of datatable  
  #   roundPrecision:           number of decimal places used for rounding
  #
  # Returns:
  #   DT object or renderDT object with data and options specified
  
  if(is.null(options$extensions)){
    options$extensions <- list()
  }
  data <- roundDf(data,roundPrecision)
  dt <- DT::datatable(data, options = options$options, extensions = options$extensions, class = options$class, filter = options$filter, rownames = options$rownames, 
                      height = height)
  
  return(DT::renderDataTable(dt))
}