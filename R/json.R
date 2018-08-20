get.json.file.schema.pairs <- function(fileDir, schemaDir = fileDir, schema.pattern = "_schema"){
  # Get pairs of json and schema files from specified directory.
  #
  # Args:
  #   fileDir:                  location of JSON files
  #   schemaDir:                location of JSON schema files
  #   schema.pattern:           Pattern used to identify schema files (e.g. 'xxx_schema.json'). 
  #                             Only patterns of the form ('xxxPATTERN.json') are supported
  #
  # Returns:
  #   List of json-schema pairs
  
  json.files <- grep(list.files(fileDir), pattern = paste0(schema.pattern,"\\.json$"), inv=TRUE, value=TRUE)
  json.schema.files <- list.files(schemaDir, pattern = paste0(schema.pattern,"\\.json$"))
  
  json.schema.map <- list()
  for(json.file in json.files){
    name <- sub("\\.json$", "", json.file)
    if(paste0(name, schema.pattern, ".json") %in% json.schema.files){
      json.schema.map[[name]] <-c(paste0(fileDir,name,".json"), paste0(schemaDir,name,schema.pattern,".json"))
    }
  }
  return(json.schema.map)
}

validate.json <- function(json.file.location, json.schema.location, verbose = TRUE, greedy = TRUE, add.defaults = TRUE){
  # Validates specified JSON file with JSON schema file and returns boolean as well as possible error messages in attributes.
  #
  # Args:
  #   json.file.location:       location of JSON file to be validated
  #   json.schema.location:     location of JSON schema file used for validation
  #   verbose:                  Be verbose? If TRUE, then an attribute "errors" will list validation failures as a data.frame 
  #   greedy:                   Continue after the first error?
  #   add.defaults:             boolean that specifies whether default values defined in the schema file should be automatically added 
  #
  # Returns:
  #   boolean specifying whether JSON file is valid or not. If boolean is FALSE, an additional attribute "errors" is returned.
  
  if(!file.exists(json.file.location)){
    stop("JSON file could not be found. Check if the path you specified is valid.", call. = F)
  }
  if(!file.exists(json.schema.location)){
    stop("JSON Schema file could not be found. Check if the path you specified is valid.")
  }
  
  
  tryCatch({
    json.validator <- suppressWarnings(jsonvalidate::json_validator(json.schema.location))
  }, error = function(e) {
    stop(paste0("Error reading'",json.schema.location,"'. Check for valid JSON syntax and make sure file is accessible."), 
         call. = F)
  })
  
  tryCatch({
    validated.json <- suppressWarnings(json.validator(json.file.location, verbose = verbose, greedy = greedy, error = FALSE))
  }, error = function(e) {
    stop(paste0("Error reading'",json.file.location,"'. Check for valid JSON syntax and make sure file is accessible."), 
         call. = FALSE)
  })
  json   <- NULL
  errors <- NULL
  if(validated.json){
    if(add.defaults){
      return(list(json.add.defaults(json.file.location = json.file.location, json.schema.location = json.schema.location), NULL))
    }else{
      json <- suppressWarnings(jsonlite::fromJSON(json.file.location, simplifyDataFrame = F, simplifyMatrix = F))
    }
  }else{
    errors <- attr(validated.json, 'errors')
  }
  return(list(json, errors))
  
}

json.add.defaults <- function(json.file.location, json.schema.location){
  #  Adds default values to a JSON file. 
  #
  # Args:
  #   json.file.location:       location of JSON file used for validation
  #   json.schema.location:     location of JSON schema file used for validation
  #
  # Returns:
  #   list with default values added where missing
  
  
  ## error checks
  if(file.exists(json.file.location)){
    tryCatch({
      list.without.defaults <- suppressWarnings(jsonlite::fromJSON(json.file.location, simplifyDataFrame = F, simplifyMatrix = F))
    }, error = function(e){
      stop(paste0("There seems to be a syntax error in your JSON file. Error message: ", e), call. = F)
    })
  }else{
    stop("The JSON file you selected could not be located.", call. = F)
  }
  if(file.exists(json.schema.location)){
    tryCatch({
      suppressWarnings(jsonlite::fromJSON(json.schema.location, simplifyDataFrame = F, simplifyMatrix = F))
    }, error = function(e){
      stop(paste0("There seems to be a syntax error in your JSON schema file. Error message: ", e), call. = F)
    })
  }else{
    stop("The JSON schema file you selected could not be located.", call. = F)
  }
  # import JSON schema file as string
  tryCatch({
    json <- paste(readLines(json.schema.location), collapse = "")
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
  list.with.defaults <- modifyList(defaults, list.without.defaults)
  
  return(list.with.defaults)
}