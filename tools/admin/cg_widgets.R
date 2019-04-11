latest_widget_symbol_type  <- NULL
currentWidgetSymbolName <- character(0L)
modelInWithPrefix <- names(modelIn)

local({
  isDDPar  <- modelInWithPrefix %in% DDPar
  isGMSOpt <- modelInWithPrefix %in% GMSOpt
  modelInWithPrefix[isDDPar]  <<- prefixDDPar %+% modelInWithPrefix[isDDPar]
  modelInWithPrefix[isGMSOpt] <<- prefixGMSOpt %+% modelInWithPrefix[isGMSOpt]
})
widgetSymbols <- setNames(c(modelInWithPrefix, 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symnames),  
                          c(modelInAlias, 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symtext))

updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
validateWidgetConfig <- function(widgetJSON){
  if(!length(widgetJSON$alias) || nchar(widgetJSON$alias) < 1L){
    return("The element's alias must not be empty.")
  }
  # TODO: VALIDATE WIDGETS
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
           if(!is.numeric(widgetJSON$width)){
             return("The width must be a number!")
           }
         },
         dropdown = {
           if(!identical(length(widgetJSON$aliases), 0L) && 
              !identical(length(widgetJSON$choices), length(widgetJSON$aliases))){
             return("Your dropdown menu must have the same number of aliases as choices or no choices at all!")
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
         },
         date = {
           
         },
         daterange = {
           
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
  
  currentWidgetSymbolName <<- input$widget_symbol
  if(input$widget_symbol %in% scalarInputSym){
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
observeEvent(input$widget_clPar, {
  if(identical(input$widget_symbol_type, "dd")){
    currentWidgetSymbolName <<- prefixDDPar %+% tolower(input$widget_clPar)
  }else if(identical(input$widget_symbol_type, "go")){
    currentWidgetSymbolName <<- prefixGMSOpt %+% tolower(input$widget_clPar)
  }
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
      latest_widget_symbol_type <<- input$widget_symbol_type
      return()
    }
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
               textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO", value = rv$widgetConfig$alias),
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
                                   step = if(length(currentConfig$step)) currentConfig$step else 2L,
                                   ticks = identical(currentConfig$ticks, TRUE),
                                   width = if(length(currentConfig$width)) currentConfig$width else 100L,
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           dynamicMin <- getWidgetDependencies("slider", rv$widgetConfig$min)
           dynamicMax <- getWidgetDependencies("slider", rv$widgetConfig$max)
           dynamicDef <- getWidgetDependencies("slider", rv$widgetConfig$def)
           
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO", value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "column-wrapper shiny-input-container",
                        tags$div(class = "col-sm-10",
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
                        tags$div(class = "col-sm-2",
                                 tags$div(class = "shiny-input-container",
                                          tags$label(class = "cb-label", "for" = "slider_min_dep_selector", "Static minimum?"),
                                          tags$div(
                                            tags$label(class = "checkbox-material", 
                                                       checkboxInput("slider_min_dep_selector", 
                                                                     value = is.numeric(rv$widgetConfig$min), label = NULL)
                                            ))
                                 ))
                      ),
                      tags$div(class = "column-wrapper shiny-input-container",
                               tags$div(class = "col-sm-10",
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
                               tags$div(class = "col-sm-2",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", "Static maximum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_max_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$max), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "column-wrapper shiny-input-container",
                               tags$div(class = "col-sm-10",
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
                               tags$div(class = "col-sm-2",
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
                      sliderInput("slider_width", "Width of slider (%)", 0, 100, value = rv$widgetConfig$width,
                                  ticks = FALSE),
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
                                   step = if(length(currentConfig$step)) currentConfig$step else 2L,
                                   ticks = identical(currentConfig$ticks, TRUE),
                                   width = if(length(currentConfig$width)) currentConfig$width else 100L,
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           dynamicMin <- getWidgetDependencies("slider", rv$widgetConfig$min)
           dynamicMax <- getWidgetDependencies("slider", rv$widgetConfig$max)
           
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO", 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "column-wrapper shiny-input-container",
                               tags$div(class = "col-sm-10",
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
                               tags$div(class = "col-sm-2",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", "Static minimum?"),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_min_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$min), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(class = "column-wrapper shiny-input-container",
                               tags$div(class = "col-sm-10",
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
                               tags$div(class = "col-sm-2",
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
                      sliderInput("slider_width", "Width of slider (%)", 0, 100, value = rv$widgetConfig$width,
                                  ticks = FALSE),
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
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO", 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "column-wrapper shiny-input-container",
                               tags$div(class = "col-sm-10",
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
                                                                     choices = c(c("All" = "$"), inputSymMultiDim), 
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
                               tags$div(class = "col-sm-2",
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
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", "Enter the element name as it should be displayed in GAMS MIRO", value = rv$widgetConfig$alias),
                      textInput("widget_label", "Choose a label", value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "cb_default", 
                                          "Default value"),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("cb_default", value = rv$widgetConfig$value, 
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
                            checkboxInput("checkbox_preview", label = NULL, value = rv$widgetConfig$value))
               )
             )
           })
         },
         date = {
           output$widget_preview <- renderUI({
             dateInput("date_preview", "I am a date selector")
           })
           rv$widgetConfig$widgetType <<- "date"
         },
         daterange = {
           output$widget_preview <- renderUI({
             dateRangeInput("daterange_preview", "I am a date range selector")
           })
         },
         textinput = {
           output$widget_preview <- renderUI({
             textInput("text_preview", "I am a text input")
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
  switch(input$widget_type, 
         slider = ,
         sliderrange = {
           updateSliderInput(session, "slider_preview", value = input$slider_preview, 
                             label = input$widget_label)
         },
         dropdown = {
           updateSelectInput(session, "dropdown_preview", label = input$widget_label)
         },
         checkbox = {
           updateCheckboxInput(session, "checkbox_preview", label = input$widget_label)
         })
  
})
observeEvent(input$widget_hcube, {
  rv$widgetConfig$noHcube <<- !input$widget_hcube
})

observeEvent(input$table_readonly, {
  rv$widgetConfig$readonly <<- input$table_readonly
})
observeEvent(input$table_readonlyCols, {
  if(!length(input$table_readonlyCols)){
    rv$widgetConfig$readonlyCols <<- NULL
  }else{
    rv$widgetConfig$readonlyCols <<- input$table_readonlyCols
  }
})
observeEvent(input$table_heatmap, {
  rv$widgetConfig$heatmap <<- input$table_heatmap
})

observeEvent(input$slider_min, {
  if(!is.numeric(input$slider_min))
    return()
  updateSliderInput(session, "slider_preview", min = input$slider_min)
  rv$widgetConfig$min <<- input$slider_min
})
observeEvent(input$slider_min_dep, {
  updateSelectInput(session, "slider_min_dep_header", choices = inputSymHeaders[[input$slider_min_dep]])
})
observeEvent(input$slider_max, {
  if(!is.numeric(input$slider_max))
    return()
  updateSliderInput(session, "slider_preview", max = input$slider_max)
  rv$widgetConfig$max <<- input$slider_max
})
observeEvent(input$slider_max_dep, {
  updateSelectInput(session, "slider_max_dep_header", choices = inputSymHeaders[[input$slider_max_dep]])
})
observeEvent(input$slider_def, {
  if(!is.numeric(input$slider_def))
    return()
  updateSliderInput(session, "slider_preview", value = input$slider_def)
  rv$widgetConfig$default <<- input$slider_def
})
observeEvent(input$slider_def_dep, {
  updateSelectInput(session, "slider_def_dep_header", choices = inputSymHeaders[[input$slider_def_dep]])
})
observeEvent(input$slider_def1, {
  if(!is.numeric(input$slider_def1))
    return()
  rv$widgetConfig$default[1] <<- input$slider_def1
  updateSliderInput(session, "slider_preview", value = rv$widgetConfig$default)
})
observeEvent(input$slider_def2, {
  if(!is.numeric(input$slider_def2))
    return()
  rv$widgetConfig$default[2] <<- input$slider_def2
  updateSliderInput(session, "slider_preview", value = rv$widgetConfig$default)
})
observeEvent(input$slider_step, {
  if(!is.numeric(input$slider_step))
    return()
  updateSliderInput(session, "slider_preview", value = input$slider_preview, 
                    step = input$slider_step)
  rv$widgetConfig$step <<- input$slider_step
})
observeEvent(input$slider_width, {
  if(!is.numeric(input$slider_width))
    return()
  rv$widgetConfig$width <<- input$slider_width
})
observeEvent(input$slider_ticks, {
  if(!is.logical(input$slider_ticks))
    return()
  rv$widgetConfig$ticks <<- input$slider_ticks
})

observeEvent(input$dd_choices, {
  rv$widgetConfig$choices <<- input$dd_choices
  if(identical(length(input$dd_aliases), 0L)){
    updateSelectInput(session, "dropdown_preview", choices = input$dd_choices)
  }else if(identical(length(input$dd_choices), length(input$dd_aliases))){
    updateSelectInput(session, "dropdown_preview", choices = setNames(input$dd_choices, input$dd_aliases))
  }
  updateSelectInput(session, "dd_default", choices = input$dd_choices)
})
observeEvent(input$dd_choice_dep, {
  if(identical(input$dd_choice_dep, "$")){
    updateSelectInput(session, "dd_choice_dep_header", 
                      choices = allInputSymHeaders)
  }else{
    updateSelectInput(session, "dd_choice_dep_header", choices = inputSymHeaders[[input$dd_choice_dep]])
  }
})
observeEvent(input$dd_aliases, {
  rv$widgetConfig$aliases <<- input$dd_aliases
  if(identical(length(input$dd_aliases), 0L)){
    updateSelectInput(session, "dropdown_preview", choices = input$dd_choices)
  }else if(identical(length(input$dd_choices), length(input$dd_aliases))){
    updateSelectInput(session, "dropdown_preview", choices = setNames(input$dd_choices, input$dd_aliases))
  }
})
observeEvent(input$dd_default, {
  rv$widgetConfig$selected <<- input$dd_default
  updateSelectInput(session, "dropdown_preview", selected = rv$widgetConfig$selected)
})

observeEvent(input$cb_default, {
  rv$widgetConfig$value <<- input$cb_default
  updateCheckboxInput(session, "checkbox_preview", value = rv$widgetConfig$value)
})

#  ==============================
#          SAVE JSON
#  ==============================
observeEvent(input$saveWidget, {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  
  errMsg <- validateWidgetConfig(rv$widgetConfig)
  if(nchar(errMsg)){
    showHideEl(session, "#widgetValidationErr", 5000L, errMsg)
  }
  if(tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))){
    showModal(modalDialog(title = "Data exists", sprintf("A widget configuration already exists for symbol: '%s'. Do you want to overwrite this configuration? This cannot be undone!", activeSymbol$name), 
                          footer = tagList(modalButton("Cancel"), 
                                           actionButton("saveWidgetConfirm", "Overwrite"))))
    return()
  }
  rv$saveWidgetConfirm <- rv$saveWidgetConfirm + 1L
})
observeEvent(virtualActionButton(input$saveWidgetConfirm, rv$saveWidgetConfirm), {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  
  if(rv$widgetConfig$widgetType %in% c("slider", "sliderrange")){
    if(identical(input$slider_min_dep_selector, TRUE)){
      rv$widgetConfig$min <<- paste0(input$slider_min_dep_op, "(", input$slider_min_dep, 
                                     "$", input$slider_min_dep_header)
    }
    if(identical(input$slider_max_dep_selector, TRUE)){
      rv$widgetConfig$max <<- paste0(input$slider_max_dep_op, "(", input$slider_max_dep, 
                                     "$", input$slider_max_dep_header)
    }
    if(identical(rv$widgetConfig$widgetType, "slider") && identical(input$slider_def_dep_selector, TRUE)){
      rv$widgetConfig$default <<- paste0(input$slider_def_dep_op, "(", input$slider_def_dep, 
                                         "$", input$slider_def_dep_header)
    }
  }else if(identical(rv$widgetConfig$widgetType, "dropdown")){
    if(identical(input$dd_choice_dep_selector, TRUE)){
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
  
  write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
  if(any(startsWith(currentWidgetSymbolName, c(prefixDDPar, prefixGMSOpt)))){
    widgetSymbols <<- c(widgetSymbols, setNames(currentWidgetSymbolName, rv$widgetConfig$alias))
    updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
  }
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
    widgetSymbols <<- widgetSymbols[!widgetSymbols == currentWidgetSymbolName]
    updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
  }
  removeModal()
  showHideEl(session, "#widgetUpdateSuccess", 4000L)
})