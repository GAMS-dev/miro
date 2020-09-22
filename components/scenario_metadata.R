ScenarioMetadata <- R6Class("ScenarioMetadata", 
                            public = list(
                              initialize = function(inputSymbols, outputSymbols, tabularInputSymbols, rv = NULL){
                                private$inputSymbols <- inputSymbols
                                private$outputSymbols <- outputSymbols
                                private$tabularInputSymbols <- tabularInputSymbols
                                private$rv <- rv
                                return(invisible(self))
                              },
                              registerUpdateCallback = function(session, callback){
                                symName <- private$getSymbolName(session)
                                if(length(symName) == 2){
                                  stop("Cannot register callbacks in comparison mode.", call. = FALSE)
                                }
                                private$updateCallbacks[[symName]] <- callback
                              },
                              isReadonly = function(session){
                                if(length(private$getSymbolName(session)) == 2){
                                  return(TRUE)
                                }
                                return(FALSE)
                              }),
                            private = list(
                              inputSymbols = NULL,
                              tabularInputSymbols = NULL,
                              outputSymbols = NULL,
                              rv = NULL,
                              updateCallbacks = list(),
                              getSymbolName = function(session){
                                id <- strsplit(session$ns(""), "-", fixed = TRUE)[[1]]
                                if(identical(id[1], "data")){
                                  # editable input table
                                  id <- strsplit(id[2], "_", fixed = TRUE)[[1]]
                                  symId <- suppressWarnings(as.integer(id[2]))
                                  if(is.na(symId) ||symId > length(private$inputSymbols)){
                                    stop(sprintf("Invalid symbol id: %s", symId), call. = FALSE)
                                  }
                                  return(private$inputSymbols[[symId]])
                                }
                                id <- strsplit(id[1], "_", fixed = TRUE)[[1]]
                                if(identical(id[1], "in")){
                                  # input symbol
                                  symId <- suppressWarnings(as.integer(id[2]))
                                  if(is.na(symId) ||symId > length(private$inputSymbols)){
                                    stop(sprintf("Invalid symbol id: %s", symId), call. = FALSE)
                                  }
                                  return(private$inputSymbols[[symId]])
                                }else if(identical(id[1], "tab")){
                                  if(length(id) == 2){
                                    # output symbol
                                    symId <- suppressWarnings(as.integer(id[2]))
                                    if(is.na(symId) ||symId > length(private$outputSymbols)){
                                      stop(sprintf("Invalid symbol id: %s", symId), call. = FALSE)
                                    }
                                    return(private$outputSymbols[[symId]])
                                  }else{
                                    # compare mode
                                    scenId <- suppressWarnings(as.integer(id[2]))
                                    if(is.na(scenId)){
                                      stop(sprintf("Invalid scen Id: %s", id[2]), call. = FALSE)
                                    }
                                    symId <- suppressWarnings(as.integer(id[3]))
                                    if(is.na(symId)){
                                      stop(sprintf("Invalid symbol id: %s", id[3]), call. = FALSE)
                                    }
                                    if(symId <= length(private$outputSymbols)){
                                      return(c(private$outputSymbols[[symId]], scenId))
                                    }
                                    return(c(private$tabularInputSymbols[[symId - length(private$outputSymbols)]],
                                             scenId))
                                  }
                                }else{
                                  stop(sprintf("Invalid id: %s", paste(id, collapse = "_")), call. = FALSE)
                                }
                              },
                              markUnsaved = function(){
                                if(length(private$rv)){
                                  isolate(private$rv$unsavedFlag <- TRUE)
                                }
                                return(invisible(self))
                              }
                            )
)
