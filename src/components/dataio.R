DataIO <- R6Class("DataIO", public = list(
  initialize = function(config, db = NULL){
    stopifnot(is.list(config))
    if(!is.null(db))
      stopifnot(is.R6(db))
    private$config <- config[!vapply(names(config), is.null,
                                     logical(1L), USE.NAMES = FALSE)]
    private$db <- db
  },
  import = function(item, dsName){
    stopifnot(length(item) > 0L, is.list(item))
    data <- NULL
    if(identical(item$source, "customFunction")){
      if(!length(item$functionName)){
        stop("No function name specified for remote import", call. = FALSE)
      }
      tryCatch({
        fetchFunction <- match.fun(item$functionName)
      }, error = function(e){
        stop(sprintf("A custom data import function: '%s' was not found. Please make sure you first define such a function.",
                     item$functionName), call. = FALSE)
      })
      tryCatch({
        remoteData <- fetchFunction(dsName)
      }, error = function(e){
        stop(sprintf("Problems importing data via custom import function: '%s'. Error message: %s",
                     item$functionName, conditionMessage(e)), call. = FALSE)
      })
      return(private$validateData(dsName, remoteData))
    }else{
      if(!length(item$method)){
        stop("No HTTP method specified for REST API call", call. = FALSE)
      }
      if(!length(item$url)){
        stop("No url specified to send HTTP requests to", call. = FALSE)
      }
      item$headers <- private$buildHTTPHeader(item)
      bodyValues <- lapply(item$httpBody, "[[",
                           "value")
      bodyValues <- c(bodyValues, private$config$modelName, dsName)
      bodyKeys <- vapply(item$httpBody, "[[", character(1L),
                         "key", USE.NAMES = FALSE)
      bodyKeys <- c(bodyKeys, "modelname", "dataset")
      if(any(duplicated(bodyKeys))){
        stop("Duplicated body keys found", call. = FALSE)
      }
      names(bodyValues) <- bodyKeys
      item$body <- bodyValues
      data <- as_tibble(safeFromJSON(private$sendHTTPRequest(item)))
    }
    return(private$validateData(dsName, data))
  },
  export = function(data, item, dsName){
    stopifnot(inherits(data, "data.frame"), length(item) > 0L, is.list(item))

    switch(tolower(item$source),
           customfunction = {
             if(!length(item$functionName)){
               stop("No function name specified for remote export", call. = FALSE)
             }
             tryCatch({
               exportFunction <- match.fun(item$functionName)
             }, error = function(e){
               stop(sprintf("A custom data export function: '%s' was not found. Please make sure you first define such a function.",
                            item$functionName), call. = FALSE)
             })
             tryCatch({
               exportFunction(dsName, data)
             }, error = function(e){
               stop(sprintf("Problems exporting data via custom export function: '%s'. Error message: %s",
                            item$functionName, conditionMessage(e)), call. = FALSE)
             })
             return(self)
           },
           rest = {
             if(!length(item$method)){
               stop("No HTTP method specified for REST API call", call. = FALSE)
             }
             if(identical(item$method, "GET") || identical(item$method, "HEAD")){
               stop(sprintf("HTTP method '%s' not supported for exporting data", item$method), call. = FALSE)
             }
             if(!length(item$url)){
               stop("No url specified to send HTTP requests to", call. = FALSE)
             }
             bodyValues <- lapply(item$httpBody, "[[",
                                  "value")
             names(bodyValues) <- vapply(item$httpBody, "[[", character(1L),
                                         "key", USE.NAMES = FALSE)
             item$body <- list(data = data,
                               modelname = private$config$modelName,
                               dataset = dsName,
                               options = bodyValues)
             item$headers <- private$buildHTTPHeader(item)
             private$sendHTTPRequest(item)
             return(self)
           },
           {
             stop(sprintf("Export method: '%s' not supported",
                          item$source), call. = FALSE)
           })
  }
), private = list(
  config = NULL,
  db = NULL,
  sendHTTPRequest = function(item){
    switch(item$method,
           GET = {
             req <- GET(url = item$url, query = item$body, item$headers, timeout(20))
           },
           HEAD = {
             req <- HEAD(url = item$url, query = item$body, item$headers, timeout(20))
           },
           POST = {
             req <- POST(url = item$url, body = item$body, item$headers, timeout(20),
                         encode = if(length(item$encode)) item$encode else "json")

           },
           PUT = {
             req <- PUT(url = item$url, body = item$body, item$headers, timeout(20),
                        encode = if(length(item$encode)) item$encode else "json")
           },
           PATCH = {
             req <- PATCH(url = item$url, body = item$body, item$headers, timeout(20),
                        encode = if(length(item$encode)) item$encode else "json")
           },
           DELETE = {
             req <- DELETE(url = item$url, body = item$body, item$headers, timeout(20),
                           encode = if(length(item$encode)) item$encode else "json")
           },
           {
             stop(sprintf("Unsupported HTTP method: '%s'", item$method), call. = FALSE)
           })
    if(status_code(req) >= 300){
      stop(sprintf("HTTP request resulted in status code: '%s'. Aborting",
                   status_code(req)), call. = FALSE)
    }
    return(content(req, as = 'text', encoding = 'UTF-8'))
  },
  retrieveVal = function(val){
    if(startsWith(val, "@env:")){
      return(Sys.getenv(substring(val, 6L)))
    }
    return(val)
  },
  buildHTTPHeader = function(item){
    headerValues <- vapply(item$httpHeaders, "[[", character(1L),
                           "value", USE.NAMES = FALSE)
    headerKeys <- vapply(item$httpHeaders, "[[", character(1L),
                         "key", USE.NAMES = FALSE)
    if(length(item$authentication)){
      headerKeys <- c(headerKeys, "Authorization")
      headerValues <- c(headerValues,
                        paste0("Basic ",
                               base64_encode(charToRaw(
                                 paste0(private$retrieveVal(item$authentication$username), ":",
                                        private$retrieveVal(item$authentication$password))))))
    }
    if(any(duplicated(headerKeys))){
      stop("Duplicated header keys found", call. = FALSE)
    }
    names(headerValues) <- headerKeys
    if(!"Timestamp" %in% headerKeys){
      headerValues <- c(headerValues, "Timestamp" = as.character(Sys.time(), usetz = TRUE))
    }
    return(add_headers(.headers = headerValues))
  },
  validateData = function(name, data){
    dataId <- match(name, names(private$config$modelIn))
    metaData <- private$config$modelIn[[dataId]]
    if(is.na(dataId)){
      dataId <- match(name, names(private$config$modelOut))
      if(is.na(dataId)){
        stop(sprintf("Dataset: '%s' not found", name), call. = FALSE)
      }
      metaData <- private$config$modelOut[[dataId]]
    }
    if(identical(length(data), length(metaData$headers)) &&
       hasValidHeaderTypes(data, metaData$colTypes)){
      names(data) <- names(metaData$headers)
      return(data)
    }
    stop(sprintf("Dataset: '%s' is not valid", name), call. = FALSE)
  }
))
