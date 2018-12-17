# render input data for input sheets with forward dependency on other input sheets
getScalarValue <- function(data, operator){
  stopifnot(length(operator) >= 0L)
  if(!length(data)){
    return(NULL)
  }
  errMsg    <- NULL
  scalarVal <- NULL
  switch(operator,
         count = {
           scalarVal <- length(data)
         },
         max = {
           try(scalarVal <- max(as.numeric(data)))
         },
         min = {
           try(scalarVal <- min(as.numeric(data)))
         },
         mean = {
           try(scalarVal <- mean(as.numeric(data)))
         },
         median = {
           try(scalarVal <- median(as.numeric(data)))
         },
         var = {
           try(scalarVal <- var(as.numeric(data)))
         },
         sd = {
           try(scalarVal <- sd(as.numeric(data)))
         },
         {
           flog.error("Unknown operator: '%s'.", operator)
           stop()
         }
  )
  if(!is.numeric(scalarVal) || is.na(scalarVal)){
    stop("Input data is not numeric.", call. = FALSE)
  }else{
    return(scalarVal)
  }
}

getData     <- vector(mode = "list", length = length(modelInWithDep))
getSelected <- vector(mode = "list", length = length(modelIn))
inputInitialized <- vector(mode = "logical", length = length(modelInWithDep))

lapply(seq_along(modelIn), function(id){
  i    <- match(names(modelIn)[[id]], names(modelInWithDep))[1]
  if(!is.na(i)){
    name <- names(modelInWithDep)[[i]]
  }
  switch(modelIn[[id]]$type,
         checkbox = {
           if(is.na(i)){
             # no dependency
             getSelected[[id]] <<- reactive({
               if(is.null(rv[["in_" %+% id]])){
                 return(NULL)
               }
               if(!length(modelInputData[[id]][[1]])){
                 return(isolate(input[["cb_" %+% id]]))
               }else{
                 if(identical(as.integer(modelInputData[[id]]), 1L)){
                   value <- TRUE
                 }else{
                   value <- FALSE
                 }
                 modelInputData[[id]] <<- list(NULL)
                 return(value)
               }
             })
             
             observe({
               noCheck[id] <<- TRUE
               updateCheckboxInput(session, "cb_" %+% id, value = getSelected[[id]]())
             })
           }else{
             # has dependency
             observe({
               k <- modelIn[[id]]$checkbox$sheetId
               value  <- NULL
               errMsg <- NULL
               noShared <- FALSE
               rv[["in_" %+% k]]
               input[["in_" %+% k]]
               rv[["in_" %+% id]]
               
               if(sharedData[k]){
                 switch(modelIn[[k]]$type,
                        dropdown = {
                          input[["dropdown_" %+% k]]
                          if(length(sharedInputData_filtered[[k]]) && nrow(sharedInputData_filtered[[k]])){
                            tryCatch(
                                value <- filterDf(sharedInputData_filtered[[k]], modelIn[[id]]$checkbox$max)
                            , error = function(e){
                              flog.error("Some problem occurred attempting to fetch values for checkbox: '%s' " %+%
                                           "(forward dependency on dataset: '%s'). Error message: %s.", 
                                         modelInAlias[id], modelInAlias[k], e)
                              errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                            })
                          }
                        },
                        {
                          flog.debug("Widgets other than dropdown menus are currently not supported for shared datasets of checkboxes.")
                          return()
                        })
               }else{
                 tryCatch({
                   value <- getInputDataset(k)[[modelIn[[id]]$checkbox$max]]
                 }, error = function(e){
                   flog.error("Some problem occurred attempting to fetch values for checkbox: '%s' " %+%
                                "(forward dependency on dataset: '%s'). Error message: %s.", 
                              modelInAlias[id], modelInAlias[k], e)
                   errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                 })
                 if(is.null(showErrorMsg(lang$errMsg$dataError$title, errMsg))){
                   return()
                 }
               }
               tryCatch({
                 value <- getScalarValue(unlist(value, use.names = FALSE), modelIn[[id]]$checkbox$operator)
               }, error = function(e){
                 flog.warn("Input type for checkbox: '%s' is not numeric. (Operator: '%s')", 
                           name, modelIn[[id]]$checkbox$operator)
                 errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
               })
               if(is.null(showErrorMsg(lang$errMsg$dataError$title, errMsg))){
                 return()
               }
               if(!inputInitialized[i]){
                 if(is.numeric(value) && !identical(value, -Inf)){
                   inputInitialized[i] <<- TRUE
                   showEl(session, "#cbDiv_" %+% id)
                   hideEl(session, "#no_data_dep_" %+% id)
                 }else{
                   return()
                 }
               }
               
               if(length(modelInputData[[id]][[1]])){
                 selected <- suppressWarnings(as.integer(modelInputData[[id]]))
                 if(value < selected){
                   flog.warn("A checkbox value of 1 was fetched from database, but maximum allowed value is 0. Selected value was set to 0.")
                   selected <- 0L
                 }
                 noShared <- TRUE
                 if(is.na(value)){
                   flog.warn("Bad input value for checkbox returned from database.")
                   return()
                 }
               }else{
                 selected <- isolate(input[["cb_" %+% id]])
               }
               noCheck[id] <<- TRUE
               updateCheckboxInput(session, "cb_" %+% id, value = selected)
               if(value <= 0.5){
                 disableEl(session, "#cb_" %+% id)
               }else{
                 enableEl(session, "#cb_" %+% id)
               }
             })
           }
         },
         date = {
           getSelected[[id]] <<- shiny::reactive({
             if(is.null(rv[["in_" %+% id]])){
               return(NULL)
             }
             if(!length(modelInputData[[id]][[1]])){
               return(isolate(input[["date_" %+% id]]))
             }else{
               value <- modelInputData[[id]]
               modelInputData[[id]] <<- list(NULL)
               return(value)
             }
           })
           # TODO: support dependency
           observe({
             noCheck[id] <<- TRUE
             shiny::updateDateInput(session, "date_" %+% id, value = getSelected[[id]]())
           })
         },
         daterange = {
           getSelected[[id]] <<- shiny::reactive({
             if(is.null(rv[["in_" %+% id]])){
               return(NULL)
             }
             if(!length(modelInputData[[id]])){
               return(isolate(input[["daterange_" %+% id]]))
             }else{
               value <- modelInputData[[id]]
               modelInputData[[id]] <<- list(NULL)
               return(value)
             }
           })
           # TODO: support dependency
           
           observe({
             noCheck[id] <<- TRUE
             shiny::updateDateRangeInput(session, "daterange_" %+% id, 
                                         start = getSelected[[id]]()[[1]], end = getSelected[[id]]()[[2]])
           })
         },
         dropdown = {
           # retrieve selected value for dropdown menu
           getSelected[[id]] <<- reactive({
             if(is.null(rv[["in_" %+% id]])){
               return(NULL)
             }
             if(!length(modelInputData[[id]][[1]])){
               return(isolate(input[["dropdown_" %+% id]]))
             }else{
               value <- modelInputData[[id]]
               modelInputData[[id]] <<- list(NULL)
               return(value)
             }
           })
           if(is.na(i)){
             # does not have any dependencies on other datasets
             
             # observe changes of dropdown menu data
             observe({
               noCheck[id] <<- TRUE
               updateSelectInput(session, "dropdown_" %+% id, selected = getSelected[[id]]())
             })
           }else{
             # has dependencies on other datasets
             
             # retrieve choices for dropdown menu
             getData[[i]] <<- reactive({
               choices <- vector(mode = "list", length = length(ddownDep[[name]]$fw) + 1)
               aliases <- vector(mode = "list", length = length(ddownDep[[name]]$aliases) + 1)
               # retrieve single value data
               if(!is.null(choicesNoDep[[name]])){
                 choices[[1]] <- choicesNoDep[[name]]
               }
               if(!is.null(aliasesNoDep[[name]])){
                 aliases[[1]] <- aliasesNoDep[[name]]
               }
               
               if(length(ddownDep[[name]]$fw)){
                 errMsg <- NULL
                 # reset counter
                 j <- 2
                 for(dataSheet in unique(tolower(names(ddownDep[[name]]$fw)))){
                   k <- match(dataSheet, names(modelIn))
                   if(sharedData[k] && modelIn[[k]]$type == "dropdown"){
                     # dependent sheet is a dataset that uses shared data
                     input[["dropdown_" %+% k]]
                     rv[["in_" %+% k]]
                     if(length(sharedInputData_filtered[[k]]) && nrow(sharedInputData_filtered[[k]])){
                       tryCatch(
                         choices[[j]] <- filterDf(sharedInputData_filtered[[k]], ddownDep[[name]]$fw[[dataSheet]])
                         , error = function(e){
                           flog.error("Some problem occurred attempting to fetch values for dropdown menu: '%s' " %+%
                                        "(forward dependency on dataset: '%s'). Error message: %s.", 
                                      modelInAlias[id], modelInAlias[k], e)
                           errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                       })
                       if(!is.null(ddownDep[[name]]$aliases[[dataSheet]])){
                         tryCatch(
                           aliases[[j]] <- filterDf(sharedInputData_filtered[[k]], ddownDep[[name]]$aliases[[dataSheet]])
                           , error = function(e){
                             flog.error("Some problem occurred attempting to fetch values for dropdown menu: '%s' " %+%
                                          "(forward dependency on dataset: '%s'). Error message: %s.", 
                                        modelInAlias[id], modelInAlias[k], e)
                             errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                           })
                       }
                       if(!is.null(modelIn[[id]]$dropdown$operator)){
                         # element is checkbox with shared dependency that was transformed to dropdown in batch mode
                         tryCatch({
                           value <- getScalarValue(unlist(choices[[j]], use.names = FALSE), modelIn[[id]]$dropdown$operator)
                         }, error = function(e){
                           flog.warn("Input type for checkbox: '%s' is not numeric.", modelInAlias[id])
                           errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                         })
                         if(!is.null(errMsg)){
                           next
                         }
                         if(value <= 0.5){
                           choices[[j]] <- c(0L)
                           aliases[[j]] <- lang$nav$batchMode$checkboxAliases[[1]]
                         }else{
                           choices[[j]] <- c(0L, 1L)
                           aliases[[j]] <- lang$nav$batchMode$checkboxAliases
                         }
                       }
                     }
                   }else{
                     rv[["in_" %+% k]]
                     input[["in_" %+% k]]
                     tryCatch({
                       dataTmp <- getInputDataset(k)
                     }, error = function(e){
                       flog.error("Some problem occurred attempting to fetch values for dropdown menu: '%s' " %+%
                                    "(forward dependency on dataset: '%s'). Error message: %s.", 
                                  modelInAlias[id], modelInAlias[k], e)
                       errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                     })
                     if(!is.null(errMsg)){
                       next
                     }
                     choices[[j]] <- dataTmp[[ddownDep[[name]]$fw[[dataSheet]]]]
                     if(!length(choices[[j]]) || choices[[j]][[1]] == ""){
                       return(NULL)
                     }
                     if(!is.null(ddownDep[[name]]$aliases[[dataSheet]])){
                       aliases[[j]] <- dataTmp[[ddownDep[[name]]$aliases[[dataSheet]]]]
                     }
                   }
                   j <- j + 1
                 }
                 showErrorMsg(lang$errMsg$dataError$title, errMsg)
               }
               aliases <- unique(unlist(aliases, use.names = FALSE))
               choices <- unique(unlist(choices, use.names = FALSE))
               if(length(aliases)){
                 if(length(aliases) == length(choices)){
                   names(choices) <- aliases
                   return(sort(choices))
                 }else{
                   # length of aliases and choices does not match
                   flog.error(lang$errMsg$ddLenMismatch$desc, modelInAlias[k])
                   errMsg <- sprintf(lang$errMsg$ddLenMismatch$desc, modelInAlias[k])
                   showErrorMsg(lang$errMsg$ddLenMismatch$title, errMsg)
                 }
               }else{
                 return(sort(choices))
               }
             })
             
             # observe changes of dropdown menu data
             observe({
               # update choices
               if(!inputInitialized[i]){
                 choices <- getData[[i]]()
                 if(length(choices)){
                   selectedEl <- modelIn[[id]]$dropdown$selected[[1]]
                   if((!length(selectedEl) && (!identical(modelIn[[id]]$dropdown$multiple, TRUE) || 
                       identical(modelIn[[id]]$dropdown$single, TRUE))) || 
                      (length(selectedEl) && !selectedEl %in% choices)){
                     selectedEl <- choices[[1]]
                   }
                   noCheck[id] <<- TRUE
                   updateSelectInput(session, paste0("dropdown_", id), choices = choices, 
                                            selected = selectedEl)
                   inputInitialized[i] <<- TRUE
                   showEl(session, paste0("#dropdown_", id))
                   hideEl(session, paste0("#no_data_dep_", id))
                   # refresh selected item in case it was uploaded (e.g. via Excel or database)
                   if(length(isolate(rv[[paste0("in_", id)]]))){
                     rv[[paste0("in_", id)]] <<- isolate(rv[[paste0("in_", id)]]) + 1
                   }else if(length(modelInputData[[id]][[1]])){
                     noCheck[[id]] <<- TRUE
                     rv[[paste0("in_", id)]] <<- 1
                   }
                 }
               }else{
                 noCheck[id] <<- TRUE
                 updateSelectInput(session, paste0("dropdown_", id), choices = getData[[i]](), 
                                          selected = isolate(input[[paste0("dropdown_", id)]]))
               }
             })
             # observe changes of dropdown default value
             observe({
               noCheck[id] <<- TRUE
               # update default
               updateSelectInput(session, paste0("dropdown_", id), selected = getSelected[[id]]())
             }, priority = -1)
           }
         },
         slider = {
           # retrieve selected value for slider
           getSelected[[id]] <<- reactive({
             if(is.null(rv[[paste0("in_", id)]])){
               return(NULL)
             }
             if(!length(modelInputData[[id]][[1]])){
               value <- isolate(input[[paste0("slider_", id)]])
               modelInputData[[id]] <<- list(NULL, value, FALSE)
             }else{
               value <- suppressWarnings(as.numeric(modelInputData[[id]]))
               if(any(is.na(value))){
                 return(NULL)
               }
               value <- sort(value)
               modelInputData[[id]] <<- list(NULL, value, TRUE)
             }
             return(value)
           })
           if(is.na(i)){
             # does not have any dependencies on other datasets
             # observe changes of slider data
             observe({
               # update slider with default value
               value <- getSelected[[id]]()
               if(!is.null(value)){
                 noCheck[id] <<- TRUE
                 updateSliderInput(session, paste0("slider_", id), value = value)
               }
             })
           }else{
             # has dependencies on other datasets
             
             # retrieve choices for slider
             getData[[i]] <<- reactive({
               errMsg <- NULL
               dataTmp <- NULL
               sliderData <- lapply(seq_along(sliderValues[[name]]), function(valId){
                 el <- sliderValues[[name]][[valId]]
                 # return numeric data (no external dependency)
                 if(is.numeric(el)){
                   return(el)
                 }else if(names(sliderValues[[name]])[valId] %in% c("def", "def1", "def2") &&
                          is.list(modelInputData[[id]]) && length(modelInputData[[id]]) == 3L){
                   if(modelInputData[[id]][[3L]]){
                     selected <- modelInputData[[id]][[2L]]
                     modelInputData[[id]][[3L]] <<- FALSE
                   }else{
                     selected <- isolate(input[[paste0("slider_", id)]])
                   }
                   if(names(sliderValues[[name]])[valId] == "def2"){
                     return(max(selected))
                   }
                   return(min(selected))
                 }else{
                   # retrieve externally dependent data
                   k <- match(names(el)[1], tolower(names(modelIn)))[1]
                   if(modelIn[[k]]$type == "daterange"){
                     switch(el[["$operator"]],
                            count = {
                              dateRange <- input[[paste0("daterange_", k)]]
                              return(as.numeric(difftime(dateRange[[2]], dateRange[[1]])))
                            },
                            {
                              errMsg <<- paste(errMsg, "Bad operator for input type: daterange", sep = "\n")     # todo: language file!
                            }
                     )
                     return(NULL)
                   }
                   if(length(rv[["in_" %+% k]]) && (modelIn[[k]]$type == "hot" && 
                                                         !is.null(input[["in_" %+% k]]) || 
                                                         (!is.null(tableContent[[i]]) && nrow(tableContent[[i]]))) && !isEmptyInput[k]){
                     if(modelIn[[k]]$type == "hot"){
                       dataTmp <- unique(hot_to_r(isolate(input[["in_" %+% k]]))[[el[[1]][1]]])
                     }else{
                       dataTmp <- unique(tableContent[[i]][[el[[1]][1]]])
                     } 
                   }else if(length(modelInputData[[k]][[1]]) && isEmptyInput[k]){
                     # no input is shown in UI, so get hidden data
                     try(dataTmp <- unique(modelInputData[[k]][[el[[1]][1]]]))
                   }else if(sharedData[k] && modelIn[[k]]$type == "dropdown"){
                     # dependent sheet is a dataset that uses shared data
                     input[["dropdown_" %+% k]]
                     if(length(sharedInputData_filtered[[k]]) && nrow(sharedInputData_filtered[[k]])){
                       tryCatch(
                         dataTmp <- unique(filterDf(sharedInputData_filtered[[k]], el[[1]]))
                         , error = function(e){
                           flog.error("Some problem occurred attempting to fetch values for dropdown menu: '%s' " %+%
                                        "(forward dependency on dataset: '%s'). Error message: %s.", 
                                      modelInAlias[id], modelInAlias[k], e)
                           errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                         })
                     }
                   }else{
                     return(NULL)
                   }
                   if(!length(dataTmp) || identical(dataTmp, "")){
                     return(NULL)
                   }
                   tryCatch({
                     scalarVal <- getScalarValue(dataTmp, el[["$operator"]])
                     return(scalarVal)
                   }, error = function(e){
                     flog.warn("Input type for slider: '%s' is not numeric.", name)
                     errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el[[1]][1], name), sep = "\n")
                   })
                 }
               })
               names(sliderData) <- names(sliderValues[[name]])
               showErrorMsg(lang$errMsg$renderSlider$title, errMsg)
               
               if(is.numeric(sliderData$def1) && is.numeric(sliderData$def2)){
                 sliderData$def <- c(sliderData$def1, sliderData$def2)
               }
               return(sliderData)
             })
             
             # observe changes of slider data
             observe({
               value <- getData[[i]]()$def
               if(inputInitialized[i] && is.numeric(modelIn[[id]]$slider$default)){
                 # in case slider has only numeric values as default (no dependencies), keep currently selected value(s)
                 value <- isolate(input[[paste0("slider_", id)]])
               }
               noCheck[id] <<- TRUE
               updateSliderInput(session, inputId = paste0("slider_", id), value = value, min = getData[[i]]()$min, 
                                        max = getData[[i]]()$max, step = getData[[i]]()$step)
               if(!inputInitialized[i]){
                 if(!is.null(isolate(getData[[i]]()$min)) && !is.null(isolate(getData[[i]]()$max))){
                   inputInitialized[i] <<- TRUE
                   showEl(session, paste0("#slider_", id))
                   hideEl(session, paste0("#no_data_dep_", id))
                   # refresh selected item in case it was uploaded (e.g. via Excel or database)
                   if(length(isolate(rv[[paste0("in_", id)]]))){
                     rv[[paste0("in_", id)]] <<- isolate(rv[[paste0("in_", id)]]) + 1
                   }else if(length(modelInputData[[id]][[1]])){
                     rv[[paste0("in_", id)]] <<- 1
                   }
                 }
               }
             })
             # update slider default value
             observe({
               value <- getSelected[[id]]()
               if(!is.null(value)){
                 noCheck[id] <<- TRUE
                 updateSliderInput(session, inputId = paste0("slider_", id), value = value)
               }
             }, priority = -1)
           }
         }
  )
  
})