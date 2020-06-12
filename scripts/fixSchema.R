schema <- suppressWarnings(jsonlite::fromJSON("conf/language_schema.json", 
                                              simplifyDataFrame = FALSE, 
                                              simplifyMatrix = FALSE))
fixRequired <- function(schema){
  if("properties" %in% names(schema)){
    schema[["minProperties"]] <- length(schema[["properties"]])
  }
  recurseFixRequired <- function(list){
    lapply(list, function(el){
      if(is.list(el)){
        outList <- el
        if("properties" %in% names(el)){
          outList[["minProperties"]] <- length(el[["properties"]])
        }
        if("required" %in% names(el)){
          outList[["required"]] <- NULL
        }
        return(recurseFixRequired(outList))
      }
      return(el)
    })
  }
  return(recurseFixRequired(schema))
}
write_json(fixRequired(schema), "conf/language_schema.json", pretty = TRUE, auto_unbox = TRUE, null = "null")
