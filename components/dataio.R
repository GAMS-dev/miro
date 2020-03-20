DataIO <- R6Class("DataIO", public = list(
  initialize = function(config, auth = NULL, db = NULL){
    stopifnot(is.list(config))
    if(!is.null(auth))
      stopifnot(is.R6(auth))
    if(!is.null(db))
      stopifnot(is.R6(db))
    private$config <- config[!vapply(names(config), is.null, 
                                     logical(1L), USE.NAMES = FALSE)]
    private$auth <- auth
    private$db <- db
  },
  import = function(item, dsName){
    stopifnot(length(item) > 0L, is.list(item))
    data <- NULL
    if(identical(item$source, "database")){
      if(is.null(auth)){
        stop("No auth object provided. Cannot connect to internal database.", 
             call. = FALSE)
      }
      data <- auth$importShared(tableName = paste0(sharedTablePrefix, "_", dsName), 
                                keyCol = if(length(item$colSubset)) item$colSubset else character(0L))
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
      data <- as_tibble(fromJSON(private$sendHTTPRequest(item)))
    }
    return(private$validateData(dsName, data))
  },
  export = function(data, item, dsName){
    stopifnot(inherits(data, "data.frame"), length(item) > 0L, is.list(item))
    
    # at the moment support only rest endpoint
    type <- "rest"
    
    switch(tolower(type),
           database = {
             if(is.null(auth)){
               stop("No auth object provided. Cannot connect to internal database.", 
                    call. = FALSE)
             }
             db$exportDataset(tableName = paste0(sharedTablePrefix, "_", dataset), 
                              data, checkColNames = TRUE)
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
             stop(sprintf("Source type: '%s' not supported", 
                          type), call. = FALSE)
           })
  }
), private = list(
  config = NULL,
  auth = NULL,
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