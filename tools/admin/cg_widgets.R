latest_widget_symbol_type  <- NULL
currentWidgetSymbolName <- character(0L)
modelInWithPrefix <- names(modelIn)

langSpecificWidget <- list()
langSpecificWidget$widgetOptionsInput <- c("Slider" = "slider", "Dropdown menu" = "dropdown", "Checkbox" = "checkbox")
names(langSpecificWidget$widgetOptionsInput) <- lang$adminMode$widgets$widgetOptions$input
langSpecificWidget$widgetOptionsAll <- c("Slider" = "slider", "Slider range" = "sliderrange", "Dropdown menu" = "dropdown", 
                                           "Checkbox" = "checkbox", "Date" = "date", "Date range" = "daterange", 
                                           "Text box" = "textinput")
names(langSpecificWidget$widgetOptionsAll) <- lang$adminMode$widgets$widgetOptions$all
langSpecificWidget$widgetOptionsTable <- c("Table" = "table")
names(langSpecificWidget$widgetOptionsTable) <- lang$adminMode$widgets$widgetOptions$table
langSpecificWidget$widgetOptionsDate <- c("Date" = "date")
names(langSpecificWidget$widgetOptionsDate) <- lang$adminMode$widgets$widgetOptions$date
langSpecificWidget$minDepOp <- c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                          "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                          "Standard Deviation" = "sd")
names(langSpecificWidget$minDepOp) <- lang$adminMode$widgets$slider$minDepOp$choices
langSpecificWidget$maxDepOp <- c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                 "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                 "Standard Deviation" = "sd")
names(langSpecificWidget$maxDepOp) <- lang$adminMode$widgets$slider$maxDepOp$choices
langSpecificWidget$defDepOp <- c("Minimum" = "min", "Maximum" = "max", "Count" = "card",
                                 "Mean" = "mean", "Median" = "median", "Variance" = "var", 
                                 "Standard Deviation" = "sd")
names(langSpecificWidget$defDepOp) <- lang$adminMode$widgets$slider$defDepOp$choices
langSpecificWidget$depChoices <- c("All" = "")
names(langSpecificWidget$depChoices) <- lang$adminMode$widgets$dropdown$choiceDep$depChoices
langSpecificWidget$typeChoices <- c("Forward" = "0", "Backward" = "1", 
                                 "Forward and Backward" = "2")
names(langSpecificWidget$typeChoices) <- lang$adminMode$widgets$dropdown$choiceDep$typeChoices
langSpecificWidget$startview <- c("Month" = "month", "Year" = "year", "Decade" = "decade")
names(langSpecificWidget$startview) <- lang$adminMode$widgets$date$startview$choices
langSpecificWidget$weekdays <- c("Sunday" = 0L, "Monday" = 1L, "Tuesday" = 2L, 
                                 "Wednesday" = 3L, "Thursday" = 4L, "Friday" = 5L, "Saturday" = 6L)
names(langSpecificWidget$weekdays) <- lang$adminMode$widgets$date$weekstart$choices

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
    return(lang$adminMode$widgets$validate$val1)
  }
  if(startsWith(currentWidgetSymbolName, prefixDDPar) && 
     identical(nchar(trimws(currentWidgetSymbolName)), nchar(prefixDDPar))){
    return(lang$adminMode$widgets$validate$val2)
  }
  if(startsWith(currentWidgetSymbolName, prefixGMSOpt) && 
     identical(nchar(trimws(currentWidgetSymbolName)), nchar(prefixGMSOpt))){
    return(lang$adminMode$widgets$validate$val3)
  }
  if(startsWith(currentWidgetSymbolName, prefixDDPar)){ 
    symbolNameTmp <- substr(currentWidgetSymbolName, nchar(prefixDDPar)+1L, nchar(currentWidgetSymbolName))
    if(!identical(input$widget_symbol_type, "gams") && (any(symbolNameTmp == names(modelInRaw)) || any(symbolNameTmp == scalarInputSym))){
      return(lang$adminMode$widgets$validate$val4)
    }
    rm(symbolNameTmp)
  } 
  if(startsWith(currentWidgetSymbolName, prefixGMSOpt)){ 
    symbolNameTmp <- substr(currentWidgetSymbolName, nchar(prefixGMSOpt)+1L, nchar(currentWidgetSymbolName))
    if(!identical(input$widget_symbol_type, "gams") && (any(symbolNameTmp == names(modelInRaw)) || any(symbolNameTmp == scalarInputSym))){
      return(lang$adminMode$widgets$validate$val5)
    }
    rm(symbolNameTmp)
  }

    
  switch(widgetJSON$widgetType, 
         slider = ,
         sliderrange = {
           if(length(widgetJSON$default) < 1L || 
              length(widgetJSON$default) > 2L){
             return(lang$adminMode$widgets$validate$val6)
           }
           if(widgetJSON$default < widgetJSON$min && 
              identical(input$slider_min_dep_selector, TRUE) &&
              identical(input$slider_def_dep_selector, TRUE)){
             return(lang$adminMode$widgets$validate$val7)
           }
           if(widgetJSON$max < widgetJSON$default && 
              identical(input$slider_max_dep_selector, TRUE) && 
              identical(input$slider_def_dep_selector, TRUE)){
             return(lang$adminMode$widgets$validate$val7)
           }
           if(!is.logical(widgetJSON$tick)){
             return(lang$adminMode$widgets$validate$val8)
           }
           if(!is.numeric(widgetJSON$step)){
             return(lang$adminMode$widgets$validate$val9)
           }
         },
         dropdown = {
           if(!length(widgetJSON$choices)){
             return(lang$adminMode$widgets$validate$val38)
           }
           if(!identical(length(widgetJSON$aliases), 0L) && 
              !identical(length(widgetJSON$choices), length(widgetJSON$aliases))){
             return(lang$adminMode$widgets$validate$val10)
           }
           if(length(widgetJSON$selected) && (!widgetJSON$selected %in% widgetJSON$choices)){
             return(lang$adminMode$widgets$validate$val11)
           }
         },
         textinput = {
           
         },
         checkbox = {
           if(!is.logical(widgetJSON$value)){
             return(lang$adminMode$widgets$validate$val12)
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
             eTxt <- lang$adminMode$widgets$validate$val13
             tryCatch(defDate <- as.Date(widgetJSON$value), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(defDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$min)){
             eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val14, collapse = "\n")
             tryCatch(minDate <- as.Date(widgetJSON$min), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(minDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$max)){
             eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val15, collapse = "\n")
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
               return(lang$adminMode$widgets$validate$val16)
             }
           }
           if(!is.null(defDate) && !is.null(maxDate)){
             if(defDate > maxDate){
               return(lang$adminMode$widgets$validate$val17)
             }
           }
           if(!is.null(minDate) && !is.null(maxDate)){
             if(minDate > maxDate){
               return(lang$adminMode$widgets$validate$val18)
             }
           }
           if(identical(nchar(widgetJSON$format), 0L)){
             return(lang$adminMode$widgets$validate$val19)
           }
         },
         daterange = {
           startDate <- NULL
           endDate   <- NULL
           minDate <- NULL
           maxDate <- NULL
           errMsg  <- NULL
           if(!is.null(widgetJSON$start)){
             eTxt <- lang$adminMode$widgets$validate$val20
             tryCatch(startDate <- as.Date(widgetJSON$start), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(startDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$end)){
             eTxt <- lang$adminMode$widgets$validate$val21
             tryCatch(endDate <- as.Date(widgetJSON$end), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(endDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$min)){
             eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val22, collapse = "\n")
             tryCatch(minDate <- as.Date(widgetJSON$min), error = function(e){
               errMsg <<- eTxt
             })
             if(!length(minDate)){
               errMsg <- eTxt
             }
           }
           if(!is.null(widgetJSON$max)){
             eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val23, collapse = "\n")
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
               return(lang$adminMode$widgets$validate$val24)
             }
           }
           if(!is.null(endDate) && !is.null(maxDate)){
             if(endDate > maxDate){
               return(lang$adminMode$widgets$validate$val25)
             }
           }
           
           if(!is.null(startDate) && !is.null(minDate)){
             if(startDate < minDate){
               return(lang$adminMode$widgets$validate$val26)
             }
           }
           if(!is.null(endDate) && !is.null(maxDate)){
             if(endDate > maxDate){
               return(lang$adminMode$widgets$validate$val27)
             }
           }
           if(!is.null(minDate) && !is.null(maxDate)){
             if(minDate > maxDate){
               return(lang$adminMode$widgets$validate$val28)
             }
           }
           if(identical(nchar(widgetJSON$format), 0L)){
             return(lang$adminMode$widgets$validate$val29)
           }
           if(!is.null(startDate) && !is.null(endDate)){
             if(startDate > endDate){
               return(lang$adminMode$widgets$validate$val30)
             }
           }
           if(!is.null(minDate) && !is.null(maxDate)){
             if(minDate > maxDate){
               return(lang$adminMode$widgets$validate$val31)
             }
           }
         },
         table = {
           if(!is.logical(widgetJSON$readonly)){
             return(lang$adminMode$widgets$validate$val32)
           }
           if(!is.logical(widgetJSON$heatmap)){
             return(lang$adminMode$widgets$validate$val33)
           }
           if(any(!widgetJSON$readonlyCols %in% inputSymHeaders[[currentWidgetSymbolName]])){
             return(lang$adminMode$widgets$validate$val34)
           }
         },
         {
           return(lang$adminMode$widgets$validate$val35)
         })
  return("")
}

observeEvent({input$widget_symbol
  rv$widget_symbol}, {
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L, 
      identical(input$widget_symbol_type, "gams"))
  
  hideEl(session, "#noWidgetConfigMsg")
  hideEl(session, "#optionConfigMsg")
  hideEl(session, "#doubledashConfigMsg")
  
  if(input$widget_symbol %in% scalarInputSym){
    currentWidgetSymbolName <<- input$widget_symbol
    if(!currentWidgetSymbolName %in% names(configJSON$inputWidgets)){
      showEl(session, "#noWidgetConfigMsg")
    }
    symID <- match(input$widget_symbol, modelInRaw[[scalarsFileName]]$symnames)
    widgetOptions <- langSpecificWidget$widgetOptionsInput
    
    if(!identical(modelInRaw[[scalarsFileName]]$symtypes[[symID]], "parameter")){
      #lang$adminMode$widgets$widgetOptionsDate$choices
      widgetOptions <- c(widgetOptions, langSpecificWidget$widgetOptionsDate)
    }
  }else if(any(startsWith(input$widget_symbol, c(prefixDDPar, prefixGMSOpt)))){
    currentWidgetSymbolName <<- input$widget_symbol
    if(startsWith(currentWidgetSymbolName,prefixGMSOpt)){
      showEl(session, "#optionConfigMsg")
    }
    if(startsWith(currentWidgetSymbolName, prefixDDPar)){
      showEl(session, "#doubledashConfigMsg")
    }
    widgetOptions <- langSpecificWidget$widgetOptionsAll
  }else if(input$widget_symbol %in% names(modelInRaw)){
    currentWidgetSymbolName <<- input$widget_symbol
    widgetOptions <- langSpecificWidget$widgetOptionsTable
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
    updateSelectInput(session, "widget_type", choices = langSpecificWidget$widgetOptionsAll)
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
                            alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
                            readonly = identical(currentConfig$readonly, TRUE),
                            readonlyCols = currentConfig$readonlyCols,
                            heatmap = identical(currentConfig$heatmap, TRUE))
    insertUI(selector = "#widget_options",
             tagList(
               tags$div(style = "max-width:400px;",
                        textInput("widget_alias", lang$adminMode$widgets$table$alias, value = rv$widgetConfig$alias)),
               tags$div(class = "shiny-input-container",
                        tags$label(class = "cb-label", "for" = "table_readonly", lang$adminMode$widgets$table$readonly),
                        tags$div(
                          tags$label(class = "checkbox-material",
                                     checkboxInput("table_readonly", 
                                                   value = rv$widgetConfig$readonly, label = NULL)
                          ))
               ),
               tags$div(style = "max-width:400px;",
                        selectInput("table_readonlyCols", lang$adminMode$widgets$table$readonlyCols, 
                                    choices = inputSymHeaders[[input$widget_symbol]], 
                                    selected = rv$widgetConfig$readonlyCols, multiple = TRUE)),
               tags$div(class = "shiny-input-container",
                        tags$label(class = "cb-label", "for" = "table_heatmap", lang$adminMode$widgets$table$heatmap),
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
                                   alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
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
                      tags$div(style = "max-width:400px;",
                               textInput("widget_alias", lang$adminMode$widgets$slider$alias, value = rv$widgetConfig$alias)),
                      tags$div(style = "max-width:400px;",
                               textInput("widget_label", lang$adminMode$widgets$slider$label, value = rv$widgetConfig$label)),
                      tags$div(class = "shiny-input-container",
                        tags$div(class = "col-sm-8",
                                 conditionalPanel(condition = "input.slider_min_dep_selector===true",
                                                  tags$div(style = "max-width:400px;",
                                                  numericInput("slider_min", lang$adminMode$widgets$slider$min, 
                                                               value = if(is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L))
                                 ),
                                 conditionalPanel(condition = "input.slider_min_dep_selector!==true",
                                                  selectInput("slider_min_dep", lang$adminMode$widgets$slider$minDep, 
                                                              choices = inputSymMultiDim, selected = dynamicMin[2]),
                                                  selectInput("slider_min_dep_header", NULL, 
                                                              choices = if(length(dynamicMin)) inputSymHeaders[[dynamicMin[2]]] else
                                                                inputSymHeaders[[1]],
                                                              selected = dynamicMin[3]),
                                                  selectInput("slider_min_dep_op", lang$adminMode$widgets$slider$minDepOp$label, 
                                                              choices = langSpecificWidget$minDepOp,
                                                              selected = dynamicMin[1])
                                 )),
                        tags$div(class = "col-sm-4",
                                 tags$div(class = "shiny-input-container",
                                          tags$label(class = "cb-label", "for" = "slider_min_dep_selector", 
                                                     lang$adminMode$widgets$slider$minDepSelector),
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
                                                         tags$div(style = "max-width:400px;",
                                                                  numericInput("slider_max", lang$adminMode$widgets$slider$max, 
                                                                               value = if(is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L))
                                        ),
                                        conditionalPanel(condition = "input.slider_max_dep_selector!==true",
                                                         selectInput("slider_max_dep", lang$adminMode$widgets$slider$maxDep, 
                                                                     choices = inputSymMultiDim, selected = dynamicMax[2]),
                                                         selectInput("slider_max_dep_header", NULL, 
                                                                     choices = if(length(dynamicMax)) inputSymHeaders[[dynamicMax[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicMax[3]),
                                                         selectInput("slider_max_dep_op", lang$adminMode$widgets$slider$maxDepOp$label, 
                                                                     choices = langSpecificWidget$maxDepOp,
                                                                     selected = dynamicMax[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", lang$adminMode$widgets$slider$maxDepSelector),
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
                                                         tags$div(style = "max-width:400px;",
                                                                  numericInput("slider_def", lang$adminMode$widgets$slider$default, 
                                                                               value = if(is.numeric(rv$widgetConfig$default)) rv$widgetConfig$default else 2L))
                                        ),
                                        conditionalPanel(condition = "input.slider_def_dep_selector!==true",
                                                         selectInput("slider_def_dep", lang$adminMode$widgets$slider$defDep, 
                                                                     choices = inputSymMultiDim, selected = dynamicDef[2]),
                                                         selectInput("slider_def_dep_header", NULL, 
                                                                     choices = if(length(dynamicDef)) inputSymHeaders[[dynamicDef[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicDef[3]),
                                                         selectInput("slider_def_dep_op", lang$adminMode$widgets$slider$defDepOp$label, 
                                                                     choices = langSpecificWidget$defDepOp,
                                                                     selected = dynamicDef[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_def_dep_selector", lang$adminMode$widgets$slider$defDepSelector),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_def_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$default), label = NULL)
                                                   ))
                                        ))
                      ),
                      tags$div(style = "max-width:400px;",
                               numericInput("slider_step", lang$adminMode$widgets$slider$step, value = rv$widgetConfig$step, min = 0L)),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "slider_ticks", lang$adminMode$widgets$slider$ticks),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("slider_ticks", value = rv$widgetConfig$ticks, label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", lang$adminMode$widgets$slider$hcube),
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
                                   alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
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
                      textInput("widget_alias", lang$adminMode$widgets$sliderrange$alias, 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", lang$adminMode$widgets$sliderrange$label, value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.slider_min_dep_selector===true",
                                                         numericInput("slider_min", lang$adminMode$widgets$sliderrange$min, 
                                                                      value = if(is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L)
                                        ),
                                        conditionalPanel(condition = "input.slider_min_dep_selector!==true",
                                                         selectInput("slider_min_dep", lang$adminMode$widgets$sliderrange$minDep, 
                                                                     choices = inputSymMultiDim, selected = dynamicMin[2]),
                                                         selectInput("slider_min_dep_header", NULL, 
                                                                     choices = if(length(dynamicMin)) inputSymHeaders[[dynamicMin[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicMin[3]),
                                                         selectInput("slider_min_dep_op", lang$adminMode$widgets$sliderrange$minDepOp$label, 
                                                                     choices = langSpecificWidget$minDepOp,
                                                                     selected = dynamicMin[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", lang$adminMode$widgets$sliderrange$minDepSelector),
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
                                                         numericInput("slider_max", lang$adminMode$widgets$sliderrange$max, 
                                                                      value = if(is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L)
                                        ),
                                        conditionalPanel(condition = "input.slider_max_dep_selector!==true",
                                                         selectInput("slider_max_dep", lang$adminMode$widgets$sliderrange$maxDep, 
                                                                     choices = inputSymMultiDim, selected = dynamicMax[2]),
                                                         selectInput("slider_max_dep_header", NULL, 
                                                                     choices = if(length(dynamicMax)) inputSymHeaders[[dynamicMax[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicMax[3]),
                                                         selectInput("slider_max_dep_op", lang$adminMode$widgets$sliderrange$maxDepOp$label, 
                                                                     choices = langSpecificWidget$maxDepOp,
                                                                     selected = dynamicMax[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "slider_min_dep_selector", lang$adminMode$widgets$sliderrange$maxDepSelector),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("slider_max_dep_selector", 
                                                                            value = is.numeric(rv$widgetConfig$max), label = NULL)
                                                   ))
                                        ))
                      ),
                      numericInput("slider_def1", lang$adminMode$widgets$sliderrange$default1, 
                                   value = rv$widgetConfig$default[1L]),
                      numericInput("slider_def2", lang$adminMode$widgets$sliderrange$default2, 
                                   value = rv$widgetConfig$default[2L]),
                      numericInput("slider_step", "Step size", value = rv$widgetConfig$step, min = 0L),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "slider_ticks", lang$adminMode$widgets$sliderrange$ticks),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("slider_ticks", value = rv$widgetConfig$ticks, label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          lang$adminMode$widgets$sliderrange$hcube),
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
                                   alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
                                   label = currentConfig$label,
                                   choices = currentConfig$choices,
                                   aliases = currentConfig$aliases,
                                   selected = currentConfig$selected,
                                   noHcube = identical(currentConfig$noHcube, TRUE))
           dynamicChoices <- getWidgetDependencies("dropdown", rv$widgetConfig$choices)
           
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", lang$adminMode$widgets$dropdown$alias, 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", lang$adminMode$widgets$dropdown$label, value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.dd_choice_dep_selector===true",
                                                         selectizeInput("dd_choices", lang$adminMode$widgets$dropdown$choices, 
                                                                        if(!length(dynamicChoices)) currentConfig$choices else c(), 
                                                                        selected = if(!length(dynamicChoices)) currentConfig$choices else "",
                                                                        multiple = TRUE, options = list(
                                                                          'create' = TRUE,
                                                                          'persist' = FALSE)),
                                                         selectizeInput("dd_aliases", lang$adminMode$widgets$dropdown$choicesAlias, 
                                                                        if(!length(dynamicChoices)) currentConfig$aliases else c(), 
                                                                        selected = if(!length(dynamicChoices)) rv$widgetConfig$aliases else "",
                                                                        multiple = TRUE, options = list(
                                                                          'create' = TRUE,
                                                                          'persist' = FALSE))
                                        ),
                                        conditionalPanel(condition = "input.dd_choice_dep_selector!==true",
                                                         selectInput("dd_choice_dep", lang$adminMode$widgets$dropdown$choiceDep$label, 
                                                                     choices = c(langSpecificWidget$depChoices, inputSymMultiDim), 
                                                                                 selected = dynamicChoices[2]),
                                                         selectInput("dd_choice_dep_header", NULL, 
                                                                     choices = if(length(dynamicChoices)) inputSymHeaders[[dynamicChoices[2]]] else
                                                                       inputSymHeaders[[1]],
                                                                     selected = dynamicChoices[3]),
                                                         selectInput("dd_choice_dep_type", lang$adminMode$widgets$dropdown$choiceDep$type, 
                                                                     choices = langSpecificWidget$typeChoices,
                                                                     selected = dynamicChoices[1])
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "dd_choice_dep_selector", lang$adminMode$widgets$dropdown$choiceDep$selector),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("dd_choice_dep_selector", 
                                                                            value = identical(length(dynamicChoices), 0L), label = NULL)
                                                   ))
                                        ))
                      ),
                      selectInput("dd_default", lang$adminMode$widgets$dropdown$default, choices = rv$widgetConfig$choices, 
                                  selected = rv$widgetConfig$selected),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          lang$adminMode$widgets$dropdown$hcube),
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
                                   alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
                                   label = currentConfig$label,
                                   value = identical(currentConfig$value, TRUE),
                                   noHcube = identical(currentConfig$noHcube, TRUE),
                                   class =  "checkbox-material")
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", lang$adminMode$widgets$checkbox$alias, 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", lang$adminMode$widgets$checkbox$label, value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_value", 
                                          lang$adminMode$widgets$checkbox$default),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("widget_value", value = rv$widgetConfig$value, 
                                                          label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          lang$adminMode$widgets$checkbox$hcube),
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
                                   alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
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
                      textInput("widget_alias", lang$adminMode$widgets$date$alias, value = rv$widgetConfig$alias),
                      textInput("widget_label", lang$adminMode$widgets$date$label, value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_def_off!==true",
                                                         dateInput("date_default", lang$adminMode$widgets$date$default, value = rv$widgetConfig$value)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_def_off", lang$adminMode$widgets$date$defOff),
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
                                                         dateInput("date_min", lang$adminMode$widgets$date$min, value = rv$widgetConfig$min)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_min_off", lang$adminMode$widgets$date$minOff),
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
                                                         dateInput("date_max", lang$adminMode$widgets$date$max, 
                                                                   value = rv$widgetConfig$max)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_max_off", lang$adminMode$widgets$date$maxOff),
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
                                                         selectInput("date_format", lang$adminMode$widgets$date$format,
                                                                     choices = dateFormatChoices, 
                                                                     selected = if(rv$widgetConfig$format %in% dateFormatChoices) rv$widgetConfig$format
                                                                     else NULL)
                                        ),
                                        conditionalPanel(condition = "input.date_format_custom_selector===true",
                                                         textInput("date_format_custom", lang$adminMode$widgets$date$formatCustom,
                                                                   value = rv$widgetConfig$format)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_format_custom_selector", lang$adminMode$widgets$date$formatCustomSelector),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_format_custom_selector", 
                                                                            value = !rv$widgetConfig$format %in% dateFormatChoices, 
                                                                            label = NULL)
                                                   ))
                                        ))
                      ),
                      selectInput("date_startview", lang$adminMode$widgets$date$startview$label, 
                                  choices = langSpecificWidget$startview),
                      selectInput("date_weekstart", lang$adminMode$widgets$date$weekstart$label, 
                                  choices = langSpecificWidget$weekdays,
                                  selected = 0L),
                      selectInput("date_daysdisabled", lang$adminMode$widgets$date$daysDisabled, 
                                  choices = langSpecificWidget$weekdays,
                                  selected = rv$widgetConfig$daysofweekdisabled, multiple = TRUE),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "date_autoclose", 
                                          lang$adminMode$widgets$date$autoclose),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("date_autoclose", value = rv$widgetConfig$autoclose, 
                                                          label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          lang$adminMode$widgets$date$hcube),
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
                                   alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
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
                      textInput("widget_alias", lang$adminMode$widgets$daterange$alias, value = rv$widgetConfig$alias),
                      textInput("widget_label", lang$adminMode$widgets$daterange$label, value = rv$widgetConfig$label),
                      tags$div(class = "shiny-input-container", style = "width:100%;display:inline-block;",
                               tags$div(class = "col-sm-8",
                                        conditionalPanel(condition = "input.date_start_off!==true",
                                                         dateInput("date_start", lang$adminMode$widgets$daterange$defaultStart, 
                                                                   value = rv$widgetConfig$start)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_start_off", lang$adminMode$widgets$daterange$startOff),
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
                                                         dateInput("date_start", lang$adminMode$widgets$daterange$defaultEnd, 
                                                                   value = rv$widgetConfig$end)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_end_off", lang$adminMode$widgets$daterange$endOff),
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
                                                         dateInput("date_min", lang$adminMode$widgets$daterange$min, value = rv$widgetConfig$min)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_min_off", lang$adminMode$widgets$daterange$minOff),
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
                                                         dateInput("date_max", lang$adminMode$widgets$daterange$max, 
                                                                   value = rv$widgetConfig$max)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_max_off", lang$adminMode$widgets$daterange$maxOff),
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
                                                         selectInput("date_format", lang$adminMode$widgets$daterange$format,
                                                                     choices = dateFormatChoices, 
                                                                     selected = if(rv$widgetConfig$format %in% dateFormatChoices) rv$widgetConfig$format
                                                                     else NULL)
                                        ),
                                        conditionalPanel(condition = "input.date_format_custom_selector===true",
                                                         textInput("date_format_custom", lang$adminMode$widgets$daterange$formatCustom,
                                                                   value = rv$widgetConfig$format)
                                        )),
                               tags$div(class = "col-sm-4",
                                        tags$div(class = "shiny-input-container",
                                                 tags$label(class = "cb-label", "for" = "date_format_custom_selector", lang$adminMode$widgets$daterange$formatCustomSelector),
                                                 tags$div(
                                                   tags$label(class = "checkbox-material", 
                                                              checkboxInput("date_format_custom_selector", 
                                                                            value = !rv$widgetConfig$format %in% dateFormatChoices, 
                                                                            label = NULL)
                                                   ))
                                        ))
                      ),
                      selectInput("date_startview", lang$adminMode$widgets$daterange$startview$label, 
                                  choices = langSpecificWidget$startview),
                      selectInput("date_weekstart", lang$adminMode$widgets$daterange$weekstart$label, 
                                  choices = langSpecificWidget$weekdays,
                                  selected = 0L),
                      textInput("date_separator", lang$adminMode$widgets$daterange$separator, 
                                  value = rv$widgetConfig$separator),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "date_autoclose", 
                                          lang$adminMode$widgets$daterange$autoclose),
                               tags$div(
                                 tags$label(class = "checkbox-material", 
                                            checkboxInput("date_autoclose", value = rv$widgetConfig$autoclose, 
                                                          label = NULL)
                                 ))
                      ),
                      tags$div(class = "shiny-input-container",
                               tags$label(class = "cb-label", "for" = "widget_hcube", 
                                          lang$adminMode$widgets$daterange$hcube),
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
                                   alias = if(length(currentConfig$alias)) currentConfig$alias else isolate(input$widget_symbol),
                                   label = currentConfig$label,
                                   value = if(length(currentConfig$value)) currentConfig$value else "",
                                   placeholder = if(length(currentConfig$placeholder)) currentConfig$placeholder else "")
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_alias", lang$adminMode$widgets$textinput$alias, 
                                value = rv$widgetConfig$alias),
                      textInput("widget_label", lang$adminMode$widgets$textinput$label, value = rv$widgetConfig$label),
                      textInput("widget_value", lang$adminMode$widgets$textinput$value, value = rv$widgetConfig$value),
                      textInput("text_placeholder", lang$adminMode$widgets$textinput$placeholder, value = rv$widgetConfig$placeholder)
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
  if(nchar(input$widget_label))
    rv$widgetConfig$label <<- input$widget_label
  else
    rv$widgetConfig$label <<- NULL
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
  if(rv$widgetConfig$widgetType %in% c("slider", "sliderrange")){
    if(identical(input$slider_min_dep_selector, FALSE)){
      rv$widgetConfig$min <<- paste0(input$slider_min_dep_op, "(", input$slider_min_dep, 
                                     "$", input$slider_min_dep_header, ")")
    }else{
      rv$widgetConfig$min <<- input$slider_min
    }
    if(identical(input$slider_max_dep_selector, FALSE)){
      rv$widgetConfig$max <<- paste0(input$slider_max_dep_op, "(", input$slider_max_dep, 
                                     "$", input$slider_max_dep_header, ")")
    }else{
      rv$widgetConfig$max <<- input$slider_max
    }
    if(identical(rv$widgetConfig$widgetType, "slider") && identical(input$slider_def_dep_selector, FALSE)){
      rv$widgetConfig$default <<- paste0(input$slider_def_dep_op, "(", input$slider_def_dep, 
                                         "$", input$slider_def_dep_header, ")")
    }else{
      rv$widgetConfig$def <<- input$slider_def
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
  errMsg <- validateWidgetConfig(rv$widgetConfig)
  if(nchar(errMsg)){
    showHideEl(session, "#widgetValidationErr", 5000L, errMsg)
    return()
  }
  rv$saveWidgetConfirm <- rv$saveWidgetConfirm + 1L
})
observeEvent(virtualActionButton(input$saveWidgetConfirm, rv$saveWidgetConfirm), {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  configJSON$inputWidgets[[currentWidgetSymbolName]] <<- rv$widgetConfig
  print("++++++++++++++++++++++++++++++++++++")
  print(configJSON$inputWidgets[[currentWidgetSymbolName]])
  
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
      hideEl(session, "#optionConfigMsg")
      hideEl(session, "#doubledashConfigMsg")
    }
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  removeModal()
  showHideEl(session, "#widgetUpdateSuccess", 4000L)
})
observeEvent(input$deleteWidget, {
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L)
  
  showModal(modalDialog(title = lang$adminMode$widgets$modalDialog$title, sprintf(lang$adminMode$widgets$modalDialog$message), 
                          footer = tagList(modalButton(lang$adminMode$widgets$modalDialog$cancel), 
                                           actionButton("deleteWidgetConfirm", lang$adminMode$widgets$modalDialog$confirm))))
})
observeEvent(input$deleteWidgetConfirm, {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  
  configJSON$inputWidgets[[currentWidgetSymbolName]] <<- NULL
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE)
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