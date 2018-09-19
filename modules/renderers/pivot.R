renderPivot <- function(data, options, height = NULL, roundPrecision = 2, static = FALSE){
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
  if(is.null(height)){
    height <- pivotDefaultHeight
  }
  
  p <- rpivotTable::rpivotTable(roundDf(data, roundPrecision), rows = options$rows, cols = options$cols, aggregatorName = options$aggregatorName,
                                vals = options$vals, rendererName = options$rendererName, inclusions = options$inclusions,
                                exclusions = options$exclusions, sorter = options$sorter, subtotals = options$subtotals, 
                                locale = if(is.null(options$locale)){
                                  "en"
                                }else{
                                  options$locale
                                }, height = height)
  if(static){
    return(p)
  }else{
    return(rpivotTable::renderRpivotTable(p))
  }
}