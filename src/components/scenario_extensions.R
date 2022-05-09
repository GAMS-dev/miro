ScenarioExtensions <- R6Class("ScenarioExtensions",
  public = list(
    initialize = function(inputSymbols, outputSymbols, tabularInputSymbols,
                          rv = NULL, customCompareModeConfig = NULL) {
      private$inputSymbols <- inputSymbols
      private$outputSymbols <- outputSymbols
      private$tabularInputSymbols <- tabularInputSymbols
      private$rv <- rv
      private$customCompareModeConfig <- customCompareModeConfig
      return(invisible(self))
    },
    registerUpdateCallback = function(session, callback) {
      symName <- private$getSymbolName(session)
      if (length(symName) == 2) {
        if (!identical(symName[[2]], "1")) {
          stop("Cannot register callbacks in comparison mode.", call. = FALSE)
        }
      } else {
        symName <- c(symName, "1")
      }
      private$updateCallbacks[[symName[[2]]]][[symName[[1]]]] <- callback
    },
    isReadonly = function(session) {
      symName <- private$getSymbolName(session)
      if (length(symName) == 2) {
        if (identical(symName[[2]], "1")) {
          return(FALSE)
        }
        return(TRUE)
      }
      return(FALSE)
    }
  ),
  private = list(
    inputSymbols = NULL,
    tabularInputSymbols = NULL,
    outputSymbols = NULL,
    customCompareModeConfig = NULL,
    rv = NULL,
    updateCallbacks = list("1" = list()),
    getSymbolName = function(session) {
      if (is.character(session) && identical(length(session), 1L)) {
        if (!tolower(session) %in% c(private$inputSymbols, private$outputSymbols)) {
          stop(sprintf("Invalid symbol name: %s", session), call. = FALSE)
        }
        return(tolower(session))
      }
      id <- strsplit(session$ns(""), "-", fixed = TRUE)[[1]]
      if (identical(id[1], "data")) {
        # editable input table
        id <- strsplit(id[2], "_", fixed = TRUE)[[1]]
        symId <- suppressWarnings(as.integer(id[2]))
        if (is.na(symId) || symId > length(private$inputSymbols)) {
          stop(sprintf("Invalid symbol id: %s", symId), call. = FALSE)
        }
        return(private$inputSymbols[[symId]])
      }
      if (startsWith(id[1], "cmpCustom_")) {
        customCompModeId <- substring(id[1], 11L)
        if (!customCompModeId %in% names(private$customCompareModeConfig)) {
          stop(sprintf("Invalid custom compare mode id: %s", customCompModeId), call. = FALSE)
        }
        return(c(
          paste0(
            "_customcomp_",
            customCompModeId
          ),
          "1"
        ))
      }
      id <- strsplit(id[1], "_", fixed = TRUE)[[1]]
      if (identical(id[1], "in")) {
        # input symbol
        symId <- suppressWarnings(as.integer(id[2]))
        if (is.na(symId) || symId > length(private$inputSymbols)) {
          stop(sprintf("Invalid symbol id: %s", symId), call. = FALSE)
        }
        return(private$inputSymbols[[symId]])
      } else if (identical(id[1], "tab")) {
        if (identical(id[2], "1")) {
          # output symbol
          symId <- suppressWarnings(as.integer(id[3]))
          if (is.na(symId) || symId > length(private$outputSymbols)) {
            stop(sprintf("Invalid symbol id: %s", symId), call. = FALSE)
          }
          return(private$outputSymbols[[symId]])
        } else {
          # compare mode
          scenId <- suppressWarnings(as.integer(id[2]))
          if (is.na(scenId)) {
            stop(sprintf("Invalid scen Id: %s", id[2]), call. = FALSE)
          }
          symId <- suppressWarnings(as.integer(id[3]))
          if (is.na(symId)) {
            stop(sprintf("Invalid symbol id: %s", id[3]), call. = FALSE)
          }
          prefix <- ""
          if (identical(scenId, 0L)) {
            scenId <- "1"
            prefix <- "_pivotcomp_"
          }
          return(c(
            paste0(
              prefix,
              ioConfig$scenTableNamesToDisplay[[symId]]
            ),
            scenId
          ))
        }
      } else if (startsWith(id[1], "preview")) {
        # config mode
        return(session$input[["symbol_name"]])
      } else {
        stop(sprintf("Invalid id: %s", paste(id, collapse = "_")), call. = FALSE)
      }
    },
    markUnsaved = function() {
      if (length(private$rv)) {
        isolate(private$rv$unsavedFlag <- TRUE)
      }
      return(invisible(self))
    }
  )
)
