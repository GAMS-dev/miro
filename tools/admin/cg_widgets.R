latest_widget_symbol_type  <- NULL
currentWidgetSymbolName <- character(0L)
modelInWithPrefix <- names(modelIn)

local({
  isDDPar  <- modelInWithPrefix %in% DDPar
  isGMSOpt <- modelInWithPrefix %in% GMSOpt
  modelInWithPrefix[isDDPar]  <<- prefixDDPar %+% modelInWithPrefix[isDDPar]
  modelInWithPrefix[isGMSOpt] <<- prefixGMSOpt %+% modelInWithPrefix[isGMSOpt]
})
if(length(modelInRaw[[scalarsFileName]])){
  scalarInputSymWithAliases <- setNames(modelInRaw[[scalarsFileName]]$symnames, 
                                        modelInRaw[[scalarsFileName]]$symtext)
}else{
  scalarInputSymWithAliases <- c()
}
widgetSymbols <- setNames(c(modelInWithPrefix, 
                          if(length(modelIn[[scalarsFileName]])) 
                            modelIn[[scalarsFileName]]$symnames), 
                          c(modelInAlias, 
                            if(length(modelIn[[scalarsFileName]]))
                              modelIn[[scalarsFileName]]$symtext))

updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
validateWidgetConfig <- function(widgetJSON){
  if(!length(widgetJSON$alias) || identical(nchar(trimws(widgetJSON$alias)), 0L)){
    return("The alias of the element must not be empty!")
  }
  if(startsWith(currentWidgetSymbolName, prefixDDPar) && 
     identical(nchar(trimws(currentWidgetSymbolName)), nchar(prefixDDPar))){
    return("The name of the double-dash parameter must not be empty!")
  }
  if(startsWith(currentWidgetSymbolName, prefixGMSOpt) && 
     identical(nchar(trimws(currentWidgetSymbolName)), nchar(prefixGMSOpt))){
    return("The name of the GAMS option must not be empty!")
  }
  if(startsWith(currentWidgetSymbolName, prefixDDPar)){ 
    symbolNameTmp <- substr(currentWidgetSymbolName, nchar(prefixDDPar)+1L, nchar(currentWidgetSymbolName))
    if(any(symbolNameTmp == names(modelInRaw)) || any(symbolNameTmp == scalarInputSym)){
      return("The name of the double-dash parameter must not be the same as the name of another used GAMS symbol or command line parameter! Please choose another name.")
    }
    rm(symbolNameTmp)
  } 
  if(startsWith(currentWidgetSymbolName, prefixGMSOpt)){ 
    symbolNameTmp <- substr(currentWidgetSymbolName, nchar(prefixGMSOpt)+1L, nchar(currentWidgetSymbolName))
    if(any(symbolNameTmp == names(modelInRaw)) || any(symbolNameTmp == scalarInputSym)){
      return("The name of the GAMS option must not be the same as the name of another used GAMS symbol or command line parameter!")
    }
    rm(symbolNameTmp)
  } 

    
  switch(widgetJSON$widgetType, 
         slider = ,
         sliderrange = {
           if(length(widgetJSON$default) < 1L || 
              length(widgetJSON$default) > 2L){
             return("The default value is falsy!")
           }
           if(any(widgetJSON$default < widgetJSON$min) || 
              any(widgetJSON$max < widgetJSON$default)){
             return("The default value must be a number in the range [min, max]!")
           }
           if(!is.logical(widgetJSON$tick)){
             return("The tick identifier must be a logical!")
           }
           if(!is.numeric(widgetJSON$step)){
             return("The step size must be a number!")
           }
         },
         dropdown = {
           if(!identical(length(widgetJSON$aliases), 0L) && 
              !identical(length(widgetJSON$choices), length(widgetJSON$aliases))){
             return("Your dropdown menu must have the same number of aliases as choices or no aliases at all!")
           }
           if(length(widgetJSON$selected) && (!widgetJSON$selected %in% widgetJSON$choices)){
             return("The default value must be one of the choices you specified!")
           }
         },
         textinput = {
           
         },
         checkbox = {
           if(!is.logical(widgetJSON$value)){
             return("The value must be a logical!")
           }
           if(identical(widgetJSON$value, TRUE)){
             rv$widgetConfig$value <<- 1L
           }else{
             rv$widgetConfig$value <<- 0L
           }
         },
         date = {
           defDate <- NULL
           minDate <- NULL
           maxDate <- NULL
           errMsg  <- NULL
           if(!is.null(widgetJSON$value)){
             eTxt <- "Default date is invalid!"
             tryCatch(defDate <- as.Date(widgetJSON$value), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(defDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$min)){
             eTxt <- paste(errMsg, "Minimum date is invalid!", collapse = "\n")
             tryCatch(minDate <- as.Date(widgetJSON$min), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(minDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$max)){
             eTxt <- paste(errMsg, "Maximum date is invalid!", collapse = "\n")
             tryCatch(maxDate <- as.Date(widgetJSON$max), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(maxDate)){
               errMsg <- eTxt
             }
           }
           if(length(errMsg)){
             return(errMsg)
           }
           if(!is.null(defDate) && !is.null(minDate)){
             if(defDate < minDate){
               return("The default date must be after the start date!")
             }
           }
           if(!is.null(defDate) && !is.null(maxDate)){
             if(defDate > maxDate){
               return("The default date must be before the end date!")
             }
           }
           if(!is.null(minDate) && !is.null(maxDate)){
             if(minDate > maxDate){
               return("The minimum date must be before the maximum date!")
             }
           }
           if(identical(nchar(widgetJSON$format), 0L)){
             return("Minimum length for custom date format is 1!")
           }
         },
         daterange = {
           startDate <- NULL
           endDate   <- NULL
           minDate <- NULL
           maxDate <- NULL
           errMsg  <- NULL
           if(!is.null(widgetJSON$start)){
             eTxt <- "Start date is invalid!"
             tryCatch(startDate <- as.Date(widgetJSON$start), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(startDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$end)){
             eTxt <- "End date is invalid!"
             tryCatch(endDate <- as.Date(widgetJSON$end), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(endDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$min)){
             eTxt <- paste(errMsg, "Minimum date is invalid!", collapse = "\n")
             tryCatch(minDate <- as.Date(widgetJSON$min), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(minDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$max)){
             eTxt <- paste(errMsg, "Maximum date is invalid!", collapse = "\n")
             tryCatch(maxDate <- as.Date(widgetJSON$max), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(maxDate)){
               errMsg <- eTxt
             }
           }
           if(length(errMsg)){
             return(errMsg)
           }
           if(!is.null(startDate) && !is.null(minDate)){
             if(startDate < minDate){
               return("The start date must be after the minimum date!")
             }
           }
           if(!is.null(endDate) && !is.null(maxDate)){
             if(endDate > maxDate){
               return("The end date must be before the maximum date!")
             }
           }
           
           if(!is.null(startDate) && !is.null(minDate)){
             if(startDate < minDate){
               return("The start date must be after the minimum date!")
             }
           }
           if(!is.null(endDate) && !is.null(maxDate)){
             if(endDate > maxDate){
               return("The end date must be before the maximum date!")
             }
           }
           if(!is.null(minDate) && !is.null(maxDate)){
             if(minDate > maxDate){
               return("The minimum date must be before the maximum date!")
             }
           }
           if(identical(nchar(widgetJSON$format), 0L)){
             return("Minimum length for custom date format is 1!")
           }
           if(!is.null(startDate) && !is.null(endDate)){
             if(startDate > endDate){
               return("The start date must be before the end date!")
             }
           }
           if(!is.null(minDate) && !is.null(maxDate)){
             if(minDate > maxDate){
               return("The minimum date must be before the maximum date!")
             }
           }
         },
         table = {
           if(!is.logical(widgetJSON$readonly)){
             return("The readonly identifier must be a logical!")
           }
           if(!is.logical(widgetJSON$heatmap)){
             return("The heatmap identifier must be a logical!")
           }
           if(any(!widgetJSON$readonlyCols %in% inputSymHeaders[[currentWidgetSymbolName]])){
             return("Some columns you selected to be readonly do not exist in the symbol currently selected!")
           }
         },
         {
           return("no valid widget type")
         })
  return("")
}

observeEvent({input$widget_symbol
  rv$widget_symbol}, {
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L, 
      identical(input$widget_symbol_type, "gams"))
  
  hideEl(session, "#noWidgetConfigMsg")
  
  if(input$widget_symbol %in% scalarInputSym){
    currentWidgetSymbolName <<- input$widget_symbol
    if(!currentWidgetSymbolName %in% names(configJSON$inputWidgets)){
      showEl(session, "#noWidgetConfigMsg")
    }
    symID <- match(input$widget_symbol, modelInRaw[[scalarsFileName]]$symnames)
    widgetOptions <- c("Slider" = "slider", "Dropdown menu" = "dropdown", "Checkbox" = "checkbox")
    
    if(!identical(modelInRaw[[scalarsFileName]]$symtypes[[symID]], "parameter")){
      widgetOptions <- c(widgetOptions, "Date" = "date")
    }
  }else if(any(startsWith(input$widget_symbol, c(prefixDDPar, prefixGMSOpt)))){
    widgetOptions <- c("Slider" = "slider", "Slider range" = "sliderrange", "Dropdown menu" = "dropdown", 
                       "Checkbox" = "checkbox", "Date" = "date", "Date range" = "daterange", 
                       "Text box" = "textinput")
  }else if(input$widget_symbol %in% names(modelInRaw)){
    currentWidgetSymbolName <<- input$widget_symbol
    widgetOptions <- c("Table" = "table")
    if(!input$widget_symbol %in% names(modelInRaw)){
      flog.warn("Symbol: '%s' is not an input symbol.", input$widget_symbol)
      return()
    }
  }else{
    flog.error("Unknown input symbol: '%s'.", input$widget_symbol)
    showHideEl(session, "#unknownErrorWidgets", 4000L)
    return()
  }
  selectedType <- NULL
  if(currentWidgetSymbolName %in% names(configJSON$inputWidgets)){
    selectedType <- configJSON$inputWidgets[[currentWidgetSymbolName]]$widgetType
  }
  updateSelectInput(session, "widget_type", choices = widgetOptions, selected = selectedType)
  rv$widget_type <- rv$widget_type + 1L
})
observeEvent(input$widget_go, {
  currentWidgetSymbolName <<- prefixGMSOpt %+% tolower(input$widget_go)
})
observeEvent(input$widget_dd, {
  currentWidgetSymbolName <<- prefixDDPar %+% tolower(input$widget_dd)
})
observeEvent(input$widget_symbol_type, {
  if(input$widget_symbol_type %in% c("dd", "go")){
    updateSelectInput(session, "widget_type", choices = c("Slider" = "slider", "Slider range" = "sliderrange", "Dropdown menu" = "dropdown", 
                                                          "Checkbox" = "checkbox", "Date" = "date", "Date range" = "daterange", 
                                                          "Text box" = "textinput"))
    if(!length(latest_widget_symbol_type)){
      latest_widget_symbol_type <<- input$widget_symbol_type
      return()
    }
    if(latest_widget_symbol_type %in% c("dd", "go")){
      if(identical(input$widget_symbol_type, "go") &&
         identical(latest_widget_symbol_type, "dd") && 
         startsWith(prefixDDPar, currentWidgetSymbolName)){
        currentWidgetSymbolName <<- prefixGMSOpt %+% substr(currentWidgetSymbolName, 
                                                            nchar(prefixDDPar) + 1L, 
                                                            nchar(currentWidgetSymbolName))
      }
      if(identical(input$widget_symbol_type, "dd") &&
         identical(latest_widget_symbol_type, "go") && 
         startsWith(prefixGMSOpt, currentWidgetSymbolName)){
        currentWidgetSymbolName <<- prefixDDPar %+% substr(currentWidgetSymbolName, 
                                                           nchar(prefixGMSOpt) + 1L, 
                                                           nchar(currentWidgetSymbolName))
      }
    }
    if(identical(input$widget_symbol_type, "go")){
      currentWidgetSymbolName <<- prefixGMSOpt %+% tolower(input$widget_go)
    }else if(identical(input$widget_symbol_type, "dd")){
      currentWidgetSymbolName <<- prefixDDPar %+% tolower(input$widget_dd)
    }
    latest_widget_symbol_type <<- input$widget_symbol_type
    return()
  }
  latest_widget_symbol_type <<- input$widget_symbol_type
  rv$widget_symbol <- rv$widget_symbol + 1L
})
output$table_preview <- renderRHandsontable({
  req(input$widget_symbol %in% names(inputSymHeaders))
  headers_tmp <- inputSymHeaders[[input$widget_symbol]]
  data        <- data.frame(matrix(c(letters[1:10], 
                                     replicate(length(headers_tmp) - 1L,
                                               1:10)), 10))
  
  ht <- rhandsontable(data = data,
                      colHeaders = names(headers_tmp),
                      readOnly = input$table_readonly)
  if(length(input$table_readonlyCols)){
    ht <- hot_col(ht, names(headers_tmp)[match(input$table_readonlyCols, 
                                               headers_tmp)], 
                  readOnly = TRUE)
  }
  if(identical(input$table_heatmap, TRUE)){
    return(hot_heatmap(ht))
  }else{
    return(ht)
  }
})
observeEvent({input$widget_type
  rv$widget_type}, {
    req(length(input$widget_type) > 0L, nchar(currentWidgetSymbolName) > 0L)
    removeUI(selector = "#widget_options .shiny-input-container", multiple = TRUE)
  rv$widgetConfig <- list()
  currentConfig <- NULL
  if(currentWidgetSymbolName %in% names(configJSON$inputWidgets)){
    currentConfig <- configJSON$inputWidgets[[currentWidgetSymbolName]]
  }
  if(identical(input$widget_type, "table")){
    rv$widgetConfig <- list(widgetType = "table",
                            alias = currentConfig$alias,
                            readonly = identical(currentConfig$readonly, TRUE),
                            readonlyCols = currentConfig$readonlyCols,
                            heatmap = identical(currentConfig$heatmap, TRUE))
    insertUI(selector = "#widget_options",
             tagList(
               textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO 
                         (note that changing this setting does currently not reflect in the live preview)", value = rv$widgetConfig$alias),
               tags$div(class = "shiny-input-container",
                        tags$label(class = "cb-label", "for" = "table_readonly", "Should table be readonly?"),
                        tags$div(
                          tags$label(class = "checkbox-material",
                                     checkboxInput("table_readonly", 
                                                   value = rv$widgetConfig$readonly, label = NULL)
                          ))
                        ),
               selectInput("table_readonlyCols", "Select certain columns to be readonly", 
                           choices = inputSymHeaders[[input$widget_symbol]], 
                           selected = rv$widgetConfig$readonlyCols, multiple = TRUE),
               tags$div(class = "shiny-input-container",
                        tags$label(class = "cb-label", "for" = "table_heatmap", "Turn table into a heatmap?"),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("table_heatmap", value = rv$widgetConfig$heatmap, label = NULL)
                          ))
                        )), 
             where = "beforeEnd")
    output$widget_preview <- renderUI("")
    showEl(session, "#table_preview")
    return()
  }
  switch(input$widget_type, 
         slider = {
           rv$widgetConfig <- list(widgetType = "slider",
                                   alias = currentConfig$alias,
                                   label = currentConfig$label,
                                   min = if(length(currentConfig$min)) currentConfig$min else 0L,
                                   max = if(length(currentConfig$max)) currentConfig$max else 10L,
                                   default = if(length(currentConfig$default)) currentConfig$default else 2L,
                                   step = if(length(currentConfig$step)) currentConfig$step else 1L,
                                   ticks = identical(currentConfig$ticks, TRUE),
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           dynamicMin <- getWidgetDependencies("slider", rv$widgetConfig$min)
           dynamicMax <- getWidgetDependencies("slider", rv$widgetConfig$max)
           dynamicDef <- getWidgetDependencies("slider", rv$widgetConfig$def)
           
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO 
                                (note that changing this setting does currently not reflect in the live preview)", value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container",
                        tags$div(class = "col-sm-8",
                                 conditionalPanel(condition = "input.slider_min_dep_selector===true",
                                                  numericInput("slider_min", "Minimum value", 
                                                               value = if(is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L)
                                 ),
                                 conditionalPanel(condition = "input.slider_min_dep_selector!==true",
                                                  selectInput("slider_min_dep", "Select symbol and header to depend upon", 
                                                              choices = inputSymMultiDim, selected = dynamicMin[2]),
                                                  selectInput("slider_min_dep_header", NULL, 
                                                              choices = if(length(dynamicMin)) inputSymHeaders[[dynamicMin[2]]] else
                                                                inputSymHeaders[[1]],
                                                              selected = dynamicMin[3]),
                                                  selectInput("slider_min_dep_op", "Select operator", 
                                                              choices = c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                                                          "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                                                          "Standard Deviation" = "sd"),
                                                              selected = dynamicMin[1])
                                 )),
                        tags$div(class = "col-sm-4",
                                 tags$div(class = "shiny-input-container",
                                          tags$label(class = "cb-label", "for" = "slider_min_dep_selector", "Static minimum?"),
                                          tags$div(
                                            tags$label(class = "checkbox-material", 
                                                       checkboxInput("slider_min_dep_selector", 
                                                                     value = is.numeric(rv$widgetConfig$min), label = NULL)
                                            ))
                                 ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.slider_max_dep_selector===true",
                                                         numericInput("slider_max", "Maximum value", 
                                                                      value = if(is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L)
                                        ),
                                        conditionalPanel(condition = "input.slider_max_dep_selector!==true",
                                                         selectInput("slider_max_dep", "Select symbol and header to depend upon", 
                                                                     choices = inputSymMultiDim, selected = dynamicMax[2]),
                                                         selectInput("slider_max_dep_header", NULL, 
                                                                     choices = if(length(dynamicMax)) inputSymHeaders[[dynamicMax[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicMax[3]),
                                                         selectInput("slider_max_dep_op", "Select operator", 
                                                                     choices = c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                                                                 "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                                                                 "Standard Deviation" = "sd"),
                                                                     selected = dynamicMax[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", "Static maximum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_max_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$max), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.slider_def_dep_selector===true",
                                                         numericInput("slider_def", "Default value", 
                                                                      value = if(is.numeric(rv$widgetConfig$default)) rv$widgetConfig$default else 2L)
                                        ),
                                        conditionalPanel(condition = "input.slider_def_dep_selector!==true",
                                                         selectInput("slider_def_dep", "Select symbol and header to depend upon", 
                                                                     choices = inputSymMultiDim, selected = dynamicDef[2]),
                                                         selectInput("slider_def_dep_header", NULL, 
                                                                     choices = if(length(dynamicDef)) inputSymHeaders[[dynamicDef[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicDef[3]),
                                                         selectInput("slider_def_dep_op", "Select operator", 
                                                                     choices = c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                                                                 "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                                                                 "Standard Deviation" = "sd"),
                                                                     selected = dynamicDef[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_def_dep_selector", "Static default?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_def_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$default), label = NULL)
                                                   ))
                                        ))
                      ),
                      numericInput("slider_step", "Step size", value = rv$widgetConfig$step, min = 0L),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "slider_ticks", "Show tick marks?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("slider_ticks", value = rv$widgetConfig$ticks, label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          "Should element be expanded automatically in Hypercube mode?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_hcube", value = !rv$widgetConfig$noHcube, 
                                                          label = NULL)
                                 ))
                      )
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             sliderInput("slider_preview", rv$widgetConfig$label, min = if(is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L,
                         max = if(is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L, 
                         value = if(is.numeric(rv$widgetConfig$default)) rv$widgetConfig$default else 2L,
                         step = if(is.numeric(rv$widgetConfig$step)) rv$widgetConfig$step else 1L,
                         ticks = rv$widgetConfig$ticks)
           })
         },
         sliderrange = {
           rv$widgetConfig <- list(widgetType = "slider",
                                   alias = currentConfig$widget_alias,
                                   label = currentConfig$widget_label,
                                   min = if(length(currentConfig$min)) currentConfig$min else 0L,
                                   max = if(length(currentConfig$max)) currentConfig$max else 10L,
                                   default = if(length(currentConfig$default) > 1L) currentConfig$default else c(2L, 5L),
                                   step = if(length(currentConfig$step)) currentConfig$step else 1L,
                                   ticks = identical(currentConfig$ticks, TRUE),
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           dynamicMin <- getWidgetDependencies("slider", rv$widgetConfig$min)
           dynamicMax <- getWidgetDependencies("slider", rv$widgetConfig$max)
           
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO 
                                (note that changing this setting does currently not reflect in the live preview)", 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.slider_min_dep_selector===true",
                                                         numericInput("slider_min", "Minimum value", 
                                                                      value = if(is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L)
                                        ),
                                        conditionalPanel(condition = "input.slider_min_dep_selector!==true",
                                                         selectInput("slider_min_dep", "Select symbol and header to depend upon", 
                                                                     choices = inputSymMultiDim, selected = dynamicMin[2]),
                                                         selectInput("slider_min_dep_header", NULL, 
                                                                     choices = if(length(dynamicMin)) inputSymHeaders[[dynamicMin[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicMin[3]),
                                                         selectInput("slider_min_dep_op", "Select operator", 
                                                                     choices = c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                                                                 "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                                                                 "Standard Deviation" = "sd"),
                                                                     selected = dynamicMin[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", "Static minimum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_min_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$min), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.slider_max_dep_selector===true",
                                                         numericInput("slider_max", "Maximum value", 
                                                                      value = if(is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L)
                                        ),
                                        conditionalPanel(condition = "input.slider_max_dep_selector!==true",
                                                         selectInput("slider_max_dep", "Select symbol and header to depend upon", 
                                                                     choices = inputSymMultiDim, selected = dynamicMax[2]),
                                                         selectInput("slider_max_dep_header", NULL, 
                                                                     choices = if(length(dynamicMax)) inputSymHeaders[[dynamicMax[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicMax[3]),
                                                         selectInput("slider_max_dep_op", "Select operator", 
                                                                     choices = c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                                                                 "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                                                                 "Standard Deviation" = "sd"),
                                                                     selected = dynamicMax[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", "Static maximum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_max_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$max), label = NULL)
                                                   ))
                                        ))
                      ),
                      numericInput("slider_def1", "Lower default value", 
                                   value = rv$widgetConfig$default[1L]),
                      numericInput("slider_def2", "Upper default value", 
                                   value = rv$widgetConfig$default[2L]),
                      numericInput("slider_step", "Step size", value = rv$widgetConfig$step, min = 0L),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "slider_ticks", "Show tick marks?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("slider_ticks", value = rv$widgetConfig$ticks, label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          "Should element be expanded automatically in Hypercube mode?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_hcube", value = !rv$widgetConfig$noHcube, 
                                                          label = NULL)
                                 ))
                      )
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             sliderInput("slider_preview", rv$widgetConfig$label, min = rv$widgetConfig$min,
                         max = rv$widgetConfig$max, 
                         value = rv$widgetConfig$default,
                         step = rv$widgetConfig$step,
                         ticks = rv$widgetConfig$ticks)
           })
         },
         dropdown = {
           rv$widgetConfig <- list(widgetType = "dropdown",
                                   alias = currentConfig$alias,
                                   label = currentConfig$label,
                                   choices = currentConfig$choices,
                                   aliases = currentConfig$aliases,
                                   selected = currentConfig$selected,
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           dynamicChoices <- getWidgetDependencies("dropdown", rv$widgetConfig$choices)
           
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO 
                                (note that changing this setting does currently not reflect in the live preview)", 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.dd_choice_dep_selector===true",
                                                         selectizeInput("dd_choices", "Choices to select from", 
                                                                        if(!length(dynamicChoices)) currentConfig$choices else c(), 
                                                                        selected = if(!length(dynamicChoices)) currentConfig$choices else "",
                                                                        multiple = TRUE, options = list(
                                                                          'create' = TRUE,
                                                                          'persist' = FALSE)),
                                                         selectizeInput("dd_aliases", "Aliases for choices (optional)", 
                                                                        if(!length(dynamicChoices)) currentConfig$aliases else c(), 
                                                                        selected = if(!length(dynamicChoices)) rv$widgetConfig$aliases else "",
                                                                        multiple = TRUE, options = list(
                                                                          'create' = TRUE,
                                                                          'persist' = FALSE))
                                        ),
                                        conditionalPanel(condition = "input.dd_choice_dep_selector!==true",
                                                         selectInput("dd_choice_dep", "Select symbol and header to depend upon", 
                                                                     choices = c(c("All" = ""), inputSymMultiDim), 
                                                                                 selected = dynamicChoices[2]),
                                                         selectInput("dd_choice_dep_header", NULL, 
                                                                     choices = if(length(dynamicChoices)) inputSymHeaders[[dynamicChoices[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicChoices[3]),
                                                         selectInput("dd_choice_dep_type", "Select type of dependency", 
                                                                     choices = c("Forward" = "0", "Backward" = "1", 
                                                                                 "Forward and Backward" = "2"),
                                                                     selected = dynamicChoices[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "dd_choice_dep_selector", "Static choices?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("dd_choice_dep_selector", 
                                                                            value = identical(length(dynamicChoices), 0L), label = NULL)
                                                   ))
                                        ))
                      ),
                      selectInput("dd_default", "Default value", choices = rv$widgetConfig$choices, 
                                  selected = rv$widgetConfig$selected),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          "Should element be expanded automatically in Hypercube mode?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_hcube", value = !rv$widgetConfig$noHcube, 
                                                          label = NULL)
                                 ))
                      )
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             if(identical(length(rv$widgetConfig$aliases), 0L)){
               choices <- rv$widgetConfig$choices
             }else{
               req(identical(length(rv$widgetConfig$choices), length(rv$widgetConfig$aliases)))
               choices <- setNames(rv$widgetConfig$choices, rv$widgetConfig$aliases)
             }
             selectInput("dropdown_preview", rv$widgetConfig$label, choices = choices, 
                         selected = rv$widgetConfig$selected)
           })
         },
         checkbox = {
           rv$widgetConfig <- list(widgetType = "checkbox",
                                   alias = currentConfig$alias,
                                   label = currentConfig$label,
                                   value = identical(currentConfig$value, TRUE),
                                   noHcube = identical(currentConfig$noHcube, TRUE),
                                   class =  "checkbox-material")
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO 
                                (note that changing this setting does currently not reflect in the live preview)", 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_value", 
                                          "Default value"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_value", value = rv$widgetConfig$value, 
                                                          label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          "Should element be expanded automatically in Hypercube mode?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_hcube", value = !rv$widgetConfig$noHcube, 
                                                          label = NULL)
                                 ))
                      )
                    ), 
                    where = "beforeEnd")
           
           output$widget_preview <- renderUI({
             tagList(
               tags$label(class = "cb-label", "for" = "checkbox_preview", 
                          rv$widgetConfig$label), 
               tags$div(
                 tags$label(class = "checkbox-material", "for" = "checkbox_preview", 
                            checkboxInput("checkbox_preview", label = NULL,
                                          value = rv$widgetConfig$value))
               )
             )
           })
         },
         date = {
           rv$widgetConfig <- list(widgetType = "date",
                                   alias = currentConfig$alias,
                                   label = currentConfig$label,
                                   value = currentConfig$value,
                                   min   = currentConfig$min,
                                   max   = currentConfig$max,
                                   format = if(length(currentConfig$format)) currentConfig$format else "yyyy-mm-dd",
                                   startview = if(length(currentConfig$startview)) currentConfig$startview else "month",
                                   weekstart = if(length(currentConfig$weekstart)) currentConfig$weekstart else 0L,
                                   daysofweekdisabled = currentConfig$daysofweekdisabled,
                                   autoclose = identical(currentConfig$autoclose, FALSE),
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO 
                                (note that changing this setting does currently not reflect in the live preview)", value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_def_off!==true",
                                                         dateInput("date_default", "Choose the default date", value = rv$widgetConfig$value)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_def_off", "Use current date"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_def_off", 
                                                                            value = is.null(rv$widgetConfig$value), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_min_off!==true",
                                                         dateInput("date_min", "Choose the minimum allowed date", value = rv$widgetConfig$min)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_min_off", "No minimum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_min_off", 
                                                                            value = is.null(rv$widgetConfig$min), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_max_off!==true",
                                                         dateInput("date_max", "Choose the maximum allowed date", 
                                                                   value = rv$widgetConfig$max)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_max_off", "No maximum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_max_off", 
                                                                            value = is.null(rv$widgetConfig$max), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_format_custom_selector!==true",
                                                         selectInput("date_format", "Choose how you want the date to be formatted.",
                                                                     choices = dateFormatChoices, 
                                                                     selected = if(rv$widgetConfig$format %in% dateFormatChoices) rv$widgetConfig$format
                                                                     else NULL)
                                        ),
                                        conditionalPanel(condition = "input.date_format_custom_selector===true",
                                                         textInput("date_format_custom", "Enter a custom date format",
                                                                   value = rv$widgetConfig$format)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_format_custom_selector", "Custom date format?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_format_custom_selector", 
                                                                            value = !rv$widgetConfig$format %in% dateFormatChoices, 
                                                                            label = NULL)
                                                   ))
                                        ))
                      ),
                      selectInput("date_startview", "What date range is shown per default?", 
                                  choices = c("Month" = "month", "Year" = "year", "Decade" = "decade")),
                      selectInput("date_weekstart", "What day shall be the start of the week?", 
                                  choices = c("Sunday" = 0L, "Monday" = 1L, "Tuesday" = 2L, 
                                              "Wednesday" = 3L, "Thursday" = 4L, "Friday" = 5L, "Saturday" = 6L),
                                  selected = 0L),
                      selectInput("date_daysdisabled", "Select weekdays that you want to disable", 
                                  choices = c("Sunday" = 0L, "Monday" = 1L, "Tuesday" = 2L, 
                                              "Wednesday" = 3L, "Thursday" = 4L, "Friday" = 5L, "Saturday" = 6L),
                                  selected = rv$widgetConfig$daysofweekdisabled, multiple = TRUE),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "date_autoclose", 
                                          "Should datepicker be closed immediately when a date is selected?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("date_autoclose", value = rv$widgetConfig$autoclose, 
                                                          label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          "Should element be expanded automatically in Hypercube mode?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_hcube", value = !rv$widgetConfig$noHcube, 
                                                          label = NULL)
                                 ))
                      )
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             dateInput("date_preview", label = rv$widgetConfig$label,
                       value = rv$widgetConfig$value, min = rv$widgetConfig$min, max = rv$widgetConfig$max,
                       format = rv$widgetConfig$format, startview = rv$widgetConfig$startview, 
                       weekstart = rv$widgetConfig$weekstart, daysofweekdisabled = rv$widgetConfig$daysofweekdisabled,
                       autoclose = rv$widgetConfig$autoclose)
           })
         },
         daterange = {
           rv$widgetConfig <- list(widgetType = "daterange",
                                   alias = currentConfig$alias,
                                   label = currentConfig$label,
                                   start = currentConfig$start,
                                   end   = currentConfig$end,
                                   min   = currentConfig$min,
                                   max   = currentConfig$max,
                                   format = if(length(currentConfig$format)) currentConfig$format else "yyyy-mm-dd",
                                   startview = if(length(currentConfig$startview)) currentConfig$startview else "month",
                                   weekstart = if(length(currentConfig$weekstart)) currentConfig$weekstart else 0L,
                                   separator = if(length(currentConfig$separator)) currentConfig$separator else " to ",
                                   autoclose = identical(currentConfig$autoclose, FALSE),
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO
                                (note that changing this setting does currently not reflect in the live preview)", value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_start_off!==true",
                                                         dateInput("date_start", "Choose the default start date", value = rv$widgetConfig$start)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_start_off", "Use current date"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_start_off", 
                                                                            value = is.null(rv$widgetConfig$start), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_end_off!==true",
                                                         dateInput("date_start", "Choose the default end date", 
                                                                   value = rv$widgetConfig$end)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_end_off", "Use current date"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_end_off", 
                                                                            value = is.null(rv$widgetConfig$end), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_min_off!==true",
                                                         dateInput("date_min", "Choose the minimum allowed date", value = rv$widgetConfig$min)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_min_off", "No minimum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_min_off", 
                                                                            value = is.null(rv$widgetConfig$min), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_max_off!==true",
                                                         dateInput("date_max", "Choose the maximum allowed date", 
                                                                   value = rv$widgetConfig$max)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_max_off", "No maximum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_max_off", 
                                                                            value = is.null(rv$widgetConfig$max), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_format_custom_selector!==true",
                                                         selectInput("date_format", "Choose how you want the date to be formatted.",
                                                                     choices = dateFormatChoices, 
                                                                     selected = if(rv$widgetConfig$format %in% dateFormatChoices) rv$widgetConfig$format
                                                                     else NULL)
                                        ),
                                        conditionalPanel(condition = "input.date_format_custom_selector===true",
                                                         textInput("date_format_custom", "Enter a custom date format",
                                                                   value = rv$widgetConfig$format)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_format_custom_selector", "Custom date format?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_format_custom_selector", 
                                                                            value = !rv$widgetConfig$format %in% dateFormatChoices, 
                                                                            label = NULL)
                                                   ))
                                        ))
                      ),
                      selectInput("date_startview", "What date range is shown per default?", 
                                  choices = c("Month" = "month", "Year" = "year", "Decade" = "decade")),
                      selectInput("date_weekstart", "What day shall be the start of the week?", 
                                  choices = c("Sunday" = 0L, "Monday" = 1L, "Tuesday" = 2L, 
                                              "Wednesday" = 3L, "Thursday" = 4L, "Friday" = 5L, "Saturday" = 6L),
                                  selected = 0L),
                      textInput("date_separator", "Select the separator between start and end date selector", 
                                  value = rv$widgetConfig$separator),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "date_autoclose", 
                                          "Should datepicker be closed immediately when a date is selected?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("date_autoclose", value = rv$widgetConfig$autoclose, 
                                                          label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          "Should element be expanded automatically in Hypercube mode?"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_hcube", value = !rv$widgetConfig$noHcube, 
                                                          label = NULL)
                                 ))
                      )
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             dateRangeInput("daterange_preview", label = rv$widgetConfig$label,
                            start = rv$widgetConfig$start, end = rv$widgetConfig$end, 
                            min = rv$widgetConfig$min, max = rv$widgetConfig$max,
                            format = rv$widgetConfig$format, startview = rv$widgetConfig$startview, 
                            weekstart = rv$widgetConfig$weekstart, separator = rv$widgetConfig$separator,
                            autoclose = rv$widgetConfig$autoclose)
           })
         },
         textinput = {
           rv$widgetConfig <- list(widgetType = "textinput",
                                   alias = currentConfig$alias,
                                   label = currentConfig$label,
                                   value = if(length(currentConfig$value)) currentConfig$value else "",
                                   placeholder = if(length(currentConfig$placeholder)) currentConfig$placeholder else "")
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO
                                (note that changing this setting does currently not reflect in the live preview)", 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      textInput("widget_value", "Choose a default value", value = rv$widgetConfig$value),
                      textInput("text_placeholder", "Choose a placeholder", value = rv$widgetConfig$placeholder)
                    ), 
                    where = "beforeEnd")
           
           output$widget_preview <- renderUI({
             textInput("textinput_preview", rv$widgetConfig$label, value = rv$widgetConfig$value,
                       placeholder = rv$widgetConfig$placeholder)
           })
         }
  )
  hideEl(session, "#table_preview")
})

observeEvent(input$widget_alias, {
  rv$widgetConfig$alias <<- input$widget_alias
})
observeEvent(input$widget_label, {
  rv$widgetConfig$label <<- input$widget_label
})
observeEvent(input$widget_hcube, {
  rv$widgetConfig$noHcube <<- !input$widget_hcube
})

observeEvent(input$table_readonly, {
  rv$widgetConfig$readonly <<- input$table_readonly
})
observe({
  if(!length(input$table_readonlyCols)){
    isolate(rv$widgetConfig$readonlyCols <<- NULL)
  }else{
    isolate(rv$widgetConfig$readonlyCols <<- input$table_readonlyCols)
  }
})
observeEvent(input$table_heatmap, {
  rv$widgetConfig$heatmap <<- input$table_heatmap
})

observeEvent(input$slider_min, {
  if(!is.numeric(input$slider_min))
    return()
  rv$widgetConfig$min <<- input$slider_min
})
observeEvent(input$slider_min_dep, {
  updateSelectInput(session, "slider_min_dep_header", choices = inputSymHeaders[[input$slider_min_dep]])
})
observeEvent(input$slider_max, {
  if(!is.numeric(input$slider_max))
    return()
  rv$widgetConfig$max <<- input$slider_max
})
observeEvent(input$slider_max_dep, {
  updateSelectInput(session, "slider_max_dep_header", choices = inputSymHeaders[[input$slider_max_dep]])
})
observeEvent(input$slider_def, {
  if(!is.numeric(input$slider_def))
    return()
  rv$widgetConfig$default <<- input$slider_def
})
observeEvent(input$slider_def_dep, {
  updateSelectInput(session, "slider_def_dep_header", choices = inputSymHeaders[[input$slider_def_dep]])
})
observeEvent(input$slider_def1, {
  if(!is.numeric(input$slider_def1))
    return()
  rv$widgetConfig$default[1] <<- input$slider_def1
})
observeEvent(input$slider_def2, {
  if(!is.numeric(input$slider_def2))
    return()
  rv$widgetConfig$default[2] <<- input$slider_def2
})
observeEvent(input$slider_step, {
  if(!is.numeric(input$slider_step))
    return()
  rv$widgetConfig$step <<- input$slider_step
})
observeEvent(input$slider_ticks, {
  if(!is.logical(input$slider_ticks))
    return()
  rv$widgetConfig$ticks <<- input$slider_ticks
})

observeEvent(input$dd_choices, {
  rv$widgetConfig$choices <<- input$dd_choices
  updateSelectInput(session, "dd_default", choices = input$dd_choices)
})
observeEvent(input$dd_choice_dep, {
  if(identical(input$dd_choice_dep, "")){
    updateSelectInput(session, "dd_choice_dep_header", 
                      choices = allInputSymHeaders)
  }else{
    updateSelectInput(session, "dd_choice_dep_header", choices = inputSymHeaders[[input$dd_choice_dep]])
  }
})
observe({
  if(!length(input$dd_aliases)){
    isolate(rv$widgetConfig$aliases <<- NULL)
  }else{
    isolate(rv$widgetConfig$aliases <<- input$dd_aliases)
  }
})
observeEvent(input$dd_default, {
  rv$widgetConfig$selected <<- input$dd_default
})
observeEvent(input$widget_value, {
  rv$widgetConfig$value <<- input$widget_value
})

observeEvent(input$date_default, {
  rv$widgetConfig$value <<- input$date_default
})
observeEvent(input$date_def_off, {
  if(input$date_def_off){
    rv$widgetConfig$value <<- NULL
  }else{
    rv$widgetConfig$value <<- input$date_default
  }
})
observeEvent(input$date_start, {
  rv$widgetConfig$start <<- input$date_start
})
observeEvent(input$date_start_off, {
  if(input$date_start_off){
    rv$widgetConfig$start <<- NULL
  }else{
    rv$widgetConfig$start <<- input$date_start
  }
})
observeEvent(input$date_end, {
  rv$widgetConfig$end <<- input$date_end
})
observeEvent(input$date_end_off, {
  if(input$date_end_off){
    rv$widgetConfig$end <<- NULL
  }else{
    rv$widgetConfig$end <<- input$date_end
  }
})
observeEvent(input$date_min, {
  rv$widgetConfig$min <<- input$date_min
})
observeEvent(input$date_min_off, {
  if(identical(input$date_min_off, TRUE)){
    rv$widgetConfig$min <<- NULL
  }else{
    rv$widgetConfig$min <<- input$date_min
  }
})
observeEvent(input$date_max, {
  rv$widgetConfig$max <<- input$date_max
})
observeEvent(input$date_max_off, {
  if(input$date_max_off){
    rv$widgetConfig$max <<- NULL
  }else{
    rv$widgetConfig$max <<- input$date_max
  }
})
observeEvent(input$date_format, {
  rv$widgetConfig$format <<- input$date_format
})
observeEvent(input$date_format_custom, {
  rv$widgetConfig$format <<- input$date_format_custom
})
observeEvent(input$date_format_custom_selector, {
  if(input$date_format_custom_selector){
    rv$widgetConfig$format <<- input$date_format_custom
  }else{
    rv$widgetConfig$format <<- input$date_format
  }
})
observeEvent(input$date_startview, {
  rv$widgetConfig$startview <<- input$date_startview
})
observeEvent(input$date_weekstart, {
  rv$widgetConfig$weekstart <<- as.integer(input$date_weekstart)
})
observe({
  if(!length(input$date_daysdisabled)){
    isolate(rv$widgetConfig$daysofweekdisabled <<- NULL)
  }else{
    isolate(rv$widgetConfig$daysofweekdisabled <<- as.integer(input$date_daysdisabled))
  }
})


observeEvent(input$date_autoclose, {
  rv$widgetConfig$autoclose <<- input$date_autoclose
})
observeEvent(input$date_separator, {
  rv$widgetConfig$separator <<- input$date_separator
})

observeEvent(input$text_placeholder, {
  rv$widgetConfig$placeholder <<- input$text_placeholder
})

#  ==============================
#          SAVE JSON
#  ==============================
observeEvent(input$saveWidget, {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  
  errMsg <- validateWidgetConfig(rv$widgetConfig)
  if(nchar(errMsg)){
    showHideEl(session, "#widgetValidationErr", 5000L, errMsg)
    return()
  }
  rv$saveWidgetConfirm <- rv$saveWidgetConfirm + 1L
})
observeEvent(virtualActionButton(input$saveWidgetConfirm, rv$saveWidgetConfirm), {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  
  if(rv$widgetConfig$widgetType %in% c("slider", "sliderrange")){
    if(identical(input$slider_min_dep_selector, FALSE)){
      rv$widgetConfig$min <<- paste0(input$slider_min_dep_op, "(", input$slider_min_dep, 
                                     "$", input$slider_min_dep_header, ")")
    }
    if(identical(input$slider_max_dep_selector, FALSE)){
      rv$widgetConfig$max <<- paste0(input$slider_max_dep_op, "(", input$slider_max_dep, 
                                     "$", input$slider_max_dep_header, ")")
    }
    if(identical(rv$widgetConfig$widgetType, "slider") && identical(input$slider_def_dep_selector, FALSE)){
      rv$widgetConfig$default <<- paste0(input$slider_def_dep_op, "(", input$slider_def_dep, 
                                         "$", input$slider_def_dep_header, ")")
    }
  }else if(identical(rv$widgetConfig$widgetType, "dropdown")){
    if(identical(input$dd_choice_dep_selector, FALSE)){
      switch(input$dd_choice_dep_type,
             "0" = {
               rv$widgetConfig$choices <<- paste0("$", input$dd_choice_dep, 
                                                  input$dd_choice_dep_header)
             },
             "1" = {
               rv$widgetConfig$choices <<- paste0(input$dd_choice_dep, 
                                                  input$dd_choice_dep_header, "$")
             }, 
             "2" = {
               rv$widgetConfig$choices <<- paste0("$", input$dd_choice_dep, 
                                                  input$dd_choice_dep_header, "$")
             })
    }
  }
  
  configJSON$inputWidgets[[currentWidgetSymbolName]] <<- rv$widgetConfig
  
  if(any(startsWith(currentWidgetSymbolName, c(prefixDDPar, prefixGMSOpt)))){
    widgetSymbols <<- c(widgetSymbols, setNames(currentWidgetSymbolName, rv$widgetConfig$alias))
    updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
  }else if(currentWidgetSymbolName %in% scalarInputSymWithAliases){
    if(all(scalarInputSymWithAliases %in% names(configJSON$inputWidgets))){
      widgetSymbols <<- widgetSymbols[widgetSymbols != scalarsFileName]
      if(scalarsFileName %in% names(configJSON$inputWidgets)){
        configJSON$inputWidgets[[scalarsFileName]] <<- NULL
      }
      updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
    }else{
      hideEl(session, "#noWidgetConfigMsg")
    }
  }
  write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
  removeModal()
  showHideEl(session, "#widgetUpdateSuccess", 4000L)
})
observeEvent(input$deleteWidget, {
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L)
  
  showModal(modalDialog(title = "Please confirm", sprintf("Are you sure that you want to delete the existing configuration for this symbol? This cannot be undone!"), 
                          footer = tagList(modalButton("Cancel"), 
                                           actionButton("deleteWidgetConfirm", "Confirm"))))
})
observeEvent(input$deleteWidgetConfirm, {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  
  configJSON$inputWidgets[[currentWidgetSymbolName]] <<- NULL
  write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
  if(any(startsWith(currentWidgetSymbolName, c(prefixDDPar, prefixGMSOpt)))){
    widgetSymbols <<- widgetSymbols[widgetSymbols != currentWidgetSymbolName]
    updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
  }else if(currentWidgetSymbolName %in% scalarInputSymWithAliases){
    if(!scalarsFileName %in% widgetSymbols){
      widgetSymbols <<- c(widgetSymbols, setNames(scalarsFileName, modelInRaw[[scalarsFileName]]$alias))
      updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
    }else{
      showEl(session, "#noWidgetConfigMsg")
    }
  }
  removeModal()
  showHideEl(session, "#widgetUpdateSuccess", 4000L)
})