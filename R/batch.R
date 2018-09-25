genGmsString <- function(par, val, modelName){
  allParValCombinations <- do.call("expand.grid", 
                                   c(val, 
                                     stringsAsFactors = FALSE))
  allParValCombinations <- vapply(
    seq_len(nrow(allParValCombinations)), function(row){
      paste(par, allParValCombinations[row, ], sep = "", collapse = " ")
  }, character(1L), USE.NAMES = FALSE)
  
  return(trimws(paste0("gams ", modelName, ".gms ", allParValCombinations)))
}
getCombinationsSlider <- function(lowerVal, upperVal, stepSize = 1){
  # BEGIN error checks
  stopifnot(is.numeric(lowerVal), length(lowerVal) == 1)
  stopifnot(is.numeric(upperVal), length(upperVal) == 1)
  stopifnot(is.numeric(stepSize), length(stepSize) == 1, stepSize > 0)
  # END error checks
  
  ret <- list()
  repeat{
    lowTmp  <- seq(lowerVal, upperVal, stepSize)
    ret$min <- c(ret$min, lowTmp)
    ret$max  <- c(ret$max, rep.int(upperVal, length(lowTmp)))
    upperVal <- upperVal - stepSize
    if(upperVal < lowerVal){
      if((upperVal + 1e-10) < lowerVal){
        break 
      }else{
        upperVal <- lowerVal
      }
    }
  }
  return(ret)
}