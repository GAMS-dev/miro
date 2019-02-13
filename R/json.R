validateJson <- function(jsonFileLocation, jsonSchemaLocation, verbose = TRUE, greedy = TRUE, addDefaults = TRUE){
  # Validates specified JSON file with JSON schema file and returns boolean as well as possible error messages in attributes.
  #
  # Args:
  #   jsonFileLocation:       location of JSON file to be validated
  #   jsonSchemaLocation:     location of JSON schema file used for validation
  #   verbose:                  Be verbose? If TRUE, then an attribute "errors" will list validation failures as a data.frame 
  #   greedy:                   Continue after the first error?
  #   addDefaults:             boolean that specifies whether default values defined in the schema file should be automatically added 
  #
  # Returns:
  #   boolean specifying whether JSON file is valid or not. If boolean is FALSE, an additional attribute "errors" is returned.
  
  if(!file.exists(jsonFileLocation)){
    stop("JSON file could not be found. Check if the path you specified is valid.", call. = F)
  }
  if(!file.exists(jsonSchemaLocation)){
    stop("JSON Schema file could not be found. Check if the path you specified is valid.")
  }
  
  
  tryCatch({
    jsonValidator <- suppressWarnings(jsonvalidate::json_validator(jsonSchemaLocation))
  }, error = function(e) {
    stop(paste0("Error reading'", jsonSchemaLocation,"'. Check for valid JSON syntax and make sure file is accessible."), 
         call. = F)
  })
  
  tryCatch({
    validatedJson <- suppressWarnings(jsonValidator(jsonFileLocation, verbose = verbose, greedy = greedy, error = FALSE))
  }, error = function(e) {
    stop(paste0("Error reading'",jsonFileLocation,"'. Check for valid JSON syntax and make sure file is accessible."), 
         call. = FALSE)
  })
  json   <- NULL
  errors <- NULL
  if(validatedJson){
    if(addDefaults){
      return(list(jsonAddDefaults(jsonFileLocation = jsonFileLocation, jsonSchemaLocation = jsonSchemaLocation), NULL))
    }else{
      json <- suppressWarnings(jsonlite::fromJSON(jsonFileLocation, simplifyDataFrame = F, simplifyMatrix = F))
    }
  }else{
    errors <- attr(validatedJson, 'errors')
  }
  return(list(json, errors))
  
}

jsonAddDefaults <- function(jsonFileLocation, jsonSchemaLocation){
  #  Adds default values to a JSON file. 
  #
  # Args:
  #   jsonFileLocation:       location of JSON file used for validation
  #   jsonSchemaLocation:     location of JSON schema file used for validation
  #
  # Returns:
  #   list with default values added where missing
  
  
  ## error checks
  if(file.exists(jsonFileLocation)){
    tryCatch({
      listWithoutDefaults <- suppressWarnings(jsonlite::fromJSON(jsonFileLocation, simplifyDataFrame = F, simplifyMatrix = F))
    }, error = function(e){
      stop(paste0("There seems to be a syntax error in your JSON file. Error message: ", e), call. = F)
    })
  }else{
    stop("The JSON file you selected could not be located.", call. = F)
  }
  if(file.exists(jsonSchemaLocation)){
    tryCatch({
      suppressWarnings(jsonlite::fromJSON(jsonSchemaLocation, simplifyDataFrame = F, simplifyMatrix = F))
    }, error = function(e){
      stop(paste0("There seems to be a syntax error in your JSON schema file. Error message: ", e), call. = F)
    })
  }else{
    stop("The JSON schema file you selected could not be located.", call. = F)
  }
  # import JSON schema file as string
  tryCatch({
    json <- paste(readLines(jsonSchemaLocation), collapse = "")
  }, error = function(e){
    stop(paste0("Error loading JSON schema file. Error message ", e), call. = F)
  })
  # generate a new V8 context
  ct <- V8::v8(global = "window")
  if(!file.exists("JS/json-schema-fill-defaults.js")){
    stop("File: 'json-schema-fill-defaults.js' could not be located.")
  }
  ct$source("JS/json-schema-fill-defaults.js")
  # convert json string to JSON object
  ct$eval(sprintf("var json = JSON.parse('%s')", json))
  # extract default values from JSON object
  ct$eval("var defaults = jsonSchemaDefaults(json)")
  # bring JSON object with defaults back to R
  defaults <- ct$get("defaults", simplifyDataFrame = F, simplifyMatrix = F)
  
  # add default values
  listWithDefaults <- modifyList(defaults, listWithoutDefaults)
  
  return(listWithDefaults)
}