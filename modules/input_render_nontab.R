# render input data for input sheets with forward dependency on other input sheets
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
                 if(identical(modelInputData[[id]], TRUE) || 
                    identical(as.integer(modelInputData[[id]]), 1L)){
                   value <- TRUE
                 }else{
                   value <- FALSE
                 }
                 modelInputData[[id]] <<- list(NULL)
                 noCheck[id]            <<- TRUE
                 return(value)
               }
             })
             
             observe({
               shiny::updateCheckboxInput(session, "cb_" %+% id, value = getSelected[[id]]())
             })
           }else{
             # has dependency
             observe({
               k <- modelIn[[id]]$checkbox$sheetId
               value  <- NULL
               errMsg <- NULL
               rv[["in_" %+% k]]
               input[["in_" %+% k]]
               if(sharedData[k]){
                 switch(modelIn[[k]]$type,
                        dropdown = {
                          try(
                            value <- sharedInputData[[k]][sharedInputData[[k]][[colSubset[[k]][1]]] == input[["dropdown_" %+% k]], 
                                                            modelIn[[id]]$checkbox$value, drop = FALSE]
                          )
                        },
                        {
                          flog.debug("Widgets other than dropdown menus are currently not supported for shared datasets.")
                          return()
                        })
               }else{
                 tryCatch({
                   value <- getInputDataset(k)[[modelIn[[id]]$checkbox$value]]
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
               
               value <- suppressWarnings(max(unlist(value, use.names = FALSE)))
               
               if(!inputInitialized[i]){
                 if(is.numeric(value) && !identical(value, -Inf)){
                   inputInitialized[i] <<- TRUE
                   shinyjs::show("cbDiv_" %+% id)
                   hide("no_data_dep_" %+% id)
                 }
               }
              
               shiny::updateCheckboxInput(session, "cb_" %+% id, value = value)
               if(identical(modelIn[[id]]$checkbox$disable, TRUE)){
                 if(value <= 0){
                   disable("cb_" %+% id)
                 }else{
                   enable("cb_" %+% id)
                 }
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
               noCheck[id]          <<- TRUE
               return(value)
             }
           })
           # TODO: support dependency
           observe({
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
               noCheck[id]           <<- TRUE
               return(value)
             }
           })
           # TODO: support dependency
           
           observe({
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
               noCheck[id]           <<- TRUE
               return(value)
             }
           })
           if(is.na(i)){
             # does not have any dependencies on other datasets
             
             # observe changes of dropdown menu data
             observe({
               shiny::updateSelectInput(session, "dropdown_" %+% id, selected = getSelected[[id]]())
             })
           }else{
             # has dependencies on other datasets
             
             # retrieve choices for dropdown menu
             getData[[i]] <<- shiny::reactive({
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
                     try(
                       choices[[j]] <- sharedInputData[[k]][sharedInputData[[k]][[colSubset[[k]][1]]] == input[["dropdown_" %+% k]], , drop = FALSE][[ddownDep[[name]]$fw[[dataSheet]]]]
                     )
                     if(!is.null(ddownDep[[name]]$aliases[[dataSheet]])){
                       try(
                         aliases[[j]] <- sharedInputData[[k]][sharedInputData[[k]][[colSubset[[k]][1]]] == input[["dropdown_" %+% k]], , drop = FALSE][[ddownDep[[name]]$aliases[[dataSheet]]]]
                       )
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
                 if(!is.null(choices)){
                   shiny::updateSelectInput(session, paste0("dropdown_", id), choices = choices, 
                                            selected = modelIn[[id]]$dropdown$selected)
                   inputInitialized[i] <<- TRUE
                   shinyjs::show(paste0("dropdown_", id))
                   shinyjs::hide(paste0("no_data_dep_", id))
                   # refresh selected item in case it was uploaded (e.g. via Excel or database)
                   if(length(isolate(rv[[paste0("in_", id)]]))){
                     rv[[paste0("in_", id)]] <<- isolate(rv[[paste0("in_", id)]]) + 1
                   }else if(length(modelInputData[[id]][[1]])){
                     noCheck[[id]] <<- TRUE
                     rv[[paste0("in_", id)]] <<- 1
                   }
                 }
               }else{
                 updateSelectInput(session, paste0("dropdown_", id), choices = getData[[i]](), 
                                          selected = isolate(input[[paste0("dropdown_", id)]]))
               }
             })
             # observe changes of dropdown default value
             observe({
               # update default
               updateSelectInput(session, paste0("dropdown_", id), selected = getSelected[[id]]())
             })
           }
         },
         slider = {
           # retrieve selected value for slider
           getSelected[[id]] <<- reactive({
             if(is.null(rv[[paste0("in_", id)]])){
               return(NULL)
             }
             if(!length(modelInputData[[id]][[1]])){
               return(isolate(input[[paste0("slider_", id)]]))
             }else{
               value <- modelInputData[[id]]
               modelInputData[[id]] <<- list(NULL)
               noCheck[id]           <<- TRUE
               return(value)
             }
           })
           if(is.na(i)){
             # does not have any dependencies on other datasets
             # observe changes of slider data
             observe({
               # update slider with default value
               shiny::updateSliderInput(session, paste0("slider_", id), value = getSelected[[id]]())
               
             })
           }else{
             # has dependencies on other datasets
             
             # retrieve choices for slider
             getData[[i]] <<- shiny::reactive({
               errMsg <- NULL
               sliderData <- lapply(sliderValues[[name]], function(el){
                 # return numeric data (no external dependency)
                 if(is.numeric(el)){
                   return(el)
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
                                                         nrow(tableContent[[i]])) && !isEmptyInput[k]){
                     if(modelIn[[k]]$type == "hot"){
                       data <- unique(hot_to_r(isolate(input[["in_" %+% k]]))[[el[[1]]]])
                     }else{
                       data <- unique(tableContent[[i]][[el[[1]]]])
                     } 
                   }else if(length(modelInputData[[k]][[1]]) && isEmptyInput[k]){
                     # no input is shown in UI, so get hidden data
                     data <- unique(modelInputData[[k]][[el[[1]]]])
                   }else if(sharedData[k] && modelIn[[k]]$type == "dropdown"){
                     # dependent sheet is a dataset that uses shared data
                     try(
                      data <- unique(sharedInputData[[k]][sharedInputData[[k]][[colSubset[[k]][1]]] == input[[paste0("dropdown_", k)]], 
                                                            , drop = FALSE][[el[[1]]]])
                     )
                   }else{
                     return(NULL)
                   }
                   if(!length(data) || identical(data, "")){
                     return(NULL)
                   }
                   
                   switch(el[["$operator"]],
                          count = {
                            return(length(data))
                          },
                          max = {
                            maxData <- max(data)
                            if((!is.numeric(maxData) || is.na(maxData)) && strictMode){
                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el, name), sep = "\n")
                            }else{
                              return(maxData)
                            }
                          },
                          min = {
                            minData <- min(data)
                            if((!is.numeric(min(data)) || is.na(minData)) && strictMode){
                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el, name), sep = "\n")
                            }else{
                              return(minData)
                            }
                          },
                          mean = {
                            meanData = mean(data)
                            if((!is.numeric(meanData) || is.na(meanData)) && strictMode){
                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el, name), sep = "\n")
                            }else{
                              return(meanData)
                            }
                          },
                          median = {
                            medianData = median(data)
                            if((!is.numeric(medianData) || is.na(medianData)) && strictMode){
                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el, name), sep = "\n")
                            }else{
                              return(medianData)
                            }
                          },
                          var = {
                            varData = var(data)
                            if((!is.numeric(varData) || is.na(varData)) && strictMode){
                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el, name), sep = "\n")
                            }else{
                              return(varData)
                            }
                          },
                          sd = {
                            sdData = sd(data)
                            if((!is.numeric(sdData) || is.na(sdData)) && strictMode){
                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el, name), sep = "\n")
                            }else{
                              return(sdData)
                            }
                          }
                   )
                 }
               })
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
               updateSliderInput(session, inputId = paste0("slider_", id), value = value, min = getData[[i]]()$min, 
                                        max = getData[[i]]()$max, step = getData[[i]]()$step)
               
               if(!inputInitialized[i]){
                 if(!is.null(isolate(getData[[i]]()$min)) && !is.null(isolate(getData[[i]]()$max))){
                   inputInitialized[i] <<- TRUE
                   shinyjs::show(paste0("slider_", id))
                   shinyjs::hide(paste0("no_data_dep_", id))
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
               shiny::updateSliderInput(session, inputId = paste0("slider_", id), value = getSelected[[id]]())
             })
           }
         }
  )
  
})