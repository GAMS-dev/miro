CustomDataIO <- R6Class("CustomDataIO", inherit = LocalFileIO, public = list(
  initialize = function(customRendererDir) {
    private$customRendererDir <- customRendererDir
    return(super$initialize())
  },
  getLabel = function() {
    return(private$config$label)
  },
  setLocalFile = function(localFile) {
    private$localFile <- localFile
    return(invisible(self))
  },
  setConfig = function(config) {
    private$config <- config
    private$remoteData <- NULL
    return(invisible(self))
  },
  read = function(dsName, sandboxScenario) {
    data <- NULL
    if (length(private$config$functionName)) {
      # custom
      if (length(private$config$symNames) && !dsName %in% private$config$symNames) {
        stop_custom(
          "error_notfound",
          sprintf(
            "Custom importer not defined for dataset: %s.",
            dsName
          )
        )
      }
      if (is.null(private$remoteData)) {
        tryCatch(
          {
            fetchFunction <- match.fun(private$config$functionName)
          },
          error = function(e) {
            stop(sprintf(
              "A custom data import function: '%s' was not found. Please make sure you first define such a function.",
              private$config$functionName
            ), call. = FALSE)
          }
        )
        private$remoteData <- list()
        private$remoteData <- fetchFunction(private$config$datasetsToFetch,
          localFile = private$localFile,
          views = sandboxScenario$getViews(),
          attachments = sandboxScenario$getAttachments(),
          metadata = sandboxScenario$getMetadata(),
          customRendererDir = customRendererDir
        )
        if (is.null(private$remoteData)) {
          flog.warn("The custom importer function returned NULL. Please avoid this and output an error instead (e.g. with abortSafe(message)).")
          private$remoteData <- list()
        }
      }
      if (identical(dsName, scalarsFileName)) {
        if (scalarsFileName %in% private$config$datasetsToFetch) {
          return(self$fixScalarDf(private$remoteData[[dsName]], dsName,
            ignoreInvalidScalars = TRUE
          ))
        }
        return(self$fixScalarDf(tibble(
          scalar = private$config$scalarSymNames,
          description = "",
          value = vapply(private$remoteData[private$config$scalarSymNames], function(scalarVal) {
            if (is.null(scalarVal)) {
              return(NA_character_)
            }
            return(as.character(scalarVal))
          }, character(1L), USE.NAMES = FALSE)
        ), dsName, ignoreInvalidScalars = TRUE))
      }
      return(private$remoteData[[dsName]])
    }
    if (!dsName %in% names(private$config)) {
      stop_custom(
        "error_notfound",
        sprintf(
          "Custom importer not defined for dataset: %s.",
          dsName
        )
      )
    }
    item <- private$config[[dsName]]
    # FIXME: Remove deprecated REST calls when removing remoteImport feature
    if (!length(item$method)) {
      stop("No HTTP method specified for REST API call", call. = FALSE)
    }
    if (!length(item$url)) {
      stop("No url specified to send HTTP requests to", call. = FALSE)
    }
    item$headers <- private$buildHTTPHeader(item)
    bodyValues <- lapply(
      item$httpBody, "[[",
      "value"
    )
    bodyValues <- c(bodyValues, modelName, dsName)
    bodyKeys <- vapply(item$httpBody, "[[", character(1L),
      "key",
      USE.NAMES = FALSE
    )
    bodyKeys <- c(bodyKeys, "modelname", "dataset")
    if (any(duplicated(bodyKeys))) {
      stop("Duplicated body keys found", call. = FALSE)
    }
    names(bodyValues) <- bodyKeys
    item$body <- bodyValues
    data <- as_tibble(safeFromJSON(private$sendHTTPRequest(item)))
    return(data)
  },
  write = function(data, path = NULL, sandboxScenario = NULL) {
    if (length(private$config$functionName)) {
      tryCatch(
        {
          exportFunction <- match.fun(private$config$functionName)
        },
        error = function(e) {
          stop(sprintf(
            "A custom data export function: '%s' was not found. Please make sure you first define such a function.",
            private$config$functionName
          ), call. = FALSE)
        }
      )
      if (is.null(sandboxScenario)) {
        # this should not happen since custom exporters are currently
        # only supported for the sandbox scenario
        stop_custom(
          "error_internal",
          "Custom exporters not supported for non-sandbox scenario."
        )
      } else {
        exportFunction(data,
          path = path,
          views = sandboxScenario$getViews(),
          attachments = sandboxScenario$getAttachments(),
          metadata = sandboxScenario$getMetadata(),
          customRendererDir = customRendererDir
        )
      }
      return(invisible(self))
    }
    for (dsName in names(data)) {
      # FIXME: Remove deprecated REST calls when removing remoteExport feature
      item <- private$config[[dsName]]
      if (!length(item$method)) {
        stop("No HTTP method specified for REST API call", call. = FALSE)
      }
      if (identical(item$method, "GET") || identical(item$method, "HEAD")) {
        stop(sprintf("HTTP method '%s' not supported for exporting data", item$method), call. = FALSE)
      }
      if (!length(item$url)) {
        stop("No url specified to send HTTP requests to", call. = FALSE)
      }
      bodyValues <- lapply(
        item$httpBody, "[[",
        "value"
      )
      names(bodyValues) <- vapply(item$httpBody, "[[", character(1L),
        "key",
        USE.NAMES = FALSE
      )
      item$body <- list(
        data = data,
        modelname = modelName,
        dataset = dsName,
        options = bodyValues
      )
      item$headers <- private$buildHTTPHeader(item)
      private$sendHTTPRequest(item)
    }
    return(invisible(self))
  }
), private = list(
  config = NULL,
  localFile = NULL,
  remoteData = NULL,
  customRendererDir = NULL,
  sendHTTPRequest = function(item) {
    switch(item$method,
      GET = {
        req <- GET(url = item$url, query = item$body, item$headers, timeout(20))
      },
      HEAD = {
        req <- HEAD(url = item$url, query = item$body, item$headers, timeout(20))
      },
      POST = {
        req <- POST(
          url = item$url, body = item$body, item$headers, timeout(20),
          encode = if (length(item$encode)) item$encode else "json"
        )
      },
      PUT = {
        req <- PUT(
          url = item$url, body = item$body, item$headers, timeout(20),
          encode = if (length(item$encode)) item$encode else "json"
        )
      },
      PATCH = {
        req <- PATCH(
          url = item$url, body = item$body, item$headers, timeout(20),
          encode = if (length(item$encode)) item$encode else "json"
        )
      },
      DELETE = {
        req <- DELETE(
          url = item$url, body = item$body, item$headers, timeout(20),
          encode = if (length(item$encode)) item$encode else "json"
        )
      },
      {
        stop(sprintf("Unsupported HTTP method: '%s'", item$method), call. = FALSE)
      }
    )
    if (status_code(req) >= 300) {
      stop(sprintf(
        "HTTP request resulted in status code: '%s'. Aborting",
        status_code(req)
      ), call. = FALSE)
    }
    return(content(req, as = "text", encoding = "UTF-8"))
  },
  retrieveVal = function(val) {
    if (startsWith(val, "@env:")) {
      return(Sys.getenv(substring(val, 6L)))
    }
    return(val)
  },
  buildHTTPHeader = function(item) {
    headerValues <- vapply(item$httpHeaders, "[[", character(1L),
      "value",
      USE.NAMES = FALSE
    )
    headerKeys <- vapply(item$httpHeaders, "[[", character(1L),
      "key",
      USE.NAMES = FALSE
    )
    if (length(item$authentication)) {
      headerKeys <- c(headerKeys, "Authorization")
      headerValues <- c(
        headerValues,
        paste0(
          "Basic ",
          base64_encode(charToRaw(
            paste0(
              private$retrieveVal(item$authentication$username), ":",
              private$retrieveVal(item$authentication$password)
            )
          ))
        )
      )
    }
    if (any(duplicated(headerKeys))) {
      stop("Duplicated header keys found", call. = FALSE)
    }
    names(headerValues) <- headerKeys
    if (!"Timestamp" %in% headerKeys) {
      headerValues <- c(headerValues, "Timestamp" = as.character(Sys.time(), usetz = TRUE))
    }
    return(add_headers(.headers = headerValues))
  }
))
