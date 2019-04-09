latest_widget_symbol_type <- NULL
widgetSymbols <- setNames(c(names(modelIn), 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symnames),  
                          c(modelInAlias, 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symtext))
CLParameter <- setNames(c(DDPar, GMSOpt), c(rep.int("dd", length(DDPar)), rep.int("go", length(GMSOpt))))
updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
validateWidgetConfig <- function(widgetJSON){
  # TODO: VALIDATE WIDGETS
  switch(widgetJSON$widgetType, 
         slider = {
           
         },
         dropdown = {
           
         },
         textinput = {
           
         },
         checkbox = {
           
         },
         date = {
           
         },
         daterange = {
           
         },
         {
           return("no valid widget type")
         })
  return("")
}

observeEvent(input$widget_symbol, {
  req(length(input$widget_symbol))
  if(length(latest_widget_symbol_type) && 
     latest_widget_symbol_type %in% c("dd", "go")){
    widgetOptions <- c("Slider" = "slider", "Dropdown menu" = "dropdown", 
                       "Checkbox" = "checkbox", "Date" = "date", "Date range" = "daterange", 
                       "Text box" = "textinput")
  }
  if(input$widget_symbol %in% scalarInputSym){
    symID <- match(input$widget_symbol, modelInRaw[[scalarsFileName]]$symnames)
    if(is.na(symID)){
      symID <- match(input$widget_symbol, CLParameter)
      if(is.na(symID)){
        showHideEl(session, "#unknownErrorWidgets", 4000L)
        return()
      }
      widgetOptions <- c("Slider" = "slider", "Dropdown menu" = "dropdown", 
                         "Checkbox" = "checkbox", "Date" = "date", "Date range" = "daterange", 
                         "Text box" = "textinput")
    }else{
      widgetOptions <- c("Slider" = "slider", "Dropdown menu" = "dropdown", "Checkbox" = "checkbox")
    }
    if(!identical(modelInRaw[[scalarsFileName]]$symtypes[[symID]], "parameter")){
      widgetOptions <- c(widgetOptions, "Date" = "date")
    }
  }else{
    widgetOptions <- c("Table" = "table")
  }
  removeUI(selector = "#widget_wrapper .shiny-input-container", multiple = TRUE)
  insertUI(selector = "#widget_wrapper",
           tagList(
             selectInput("widget_type", "Select the type of widget you want to use", widgetOptions),
             tags$div(id = "widget_options", class = "shiny-input-container")
           ), 
           where = "beforeEnd")
})
observeEvent(input$widget_symbol_type, {
  if(!length(latest_widget_symbol_type)){
    latest_widget_symbol_type <<- input$widget_symbol_type
    return()
  }
  if(latest_widget_symbol_type %in% c("dd", "go") && 
     input$widget_symbol_type %in% c("dd", "go")){
    return()
  }
  
  rv$widget_type <- rv$widget_type + 1L
})
observeEvent({input$widget_type
  rv$widget_type}, {
    req(length(input$widget_type) > 0L)
  if(identical(input$widget_type, "table")){
    insertUI(selector = "#widget_options",
             tagList(
               tags$label(class = "cb-label", "for" = "table_readonly", "Should table be readonly?"),
               tags$div(
                 tags$label(class = "checkbox-material",
                            checkboxInput("table_readonly", value = FALSE, label = NULL)
                 )),
               selectInput("table_readonlyCols", "Select certain columns to be readonly", choices = c()),
               tags$label(class = "cb-label", "for" = "table_heatmap", "Turn table into a heatmap?"),
               tags$div(
                 tags$label(class = "checkbox-material", 
                            checkboxInput("table_heatmap", value = FALSE, label = NULL)
                 ))
             ), 
             where = "beforeEnd")
    output$table_preview <- renderRHandsontable(rhandsontable(tibble("Column 1" = 1:10, "Column 2" = letters[1:10])))
    output$widget_preview <- renderUI("")
    showEl(session, "#table_preview")
    return()
  }
  removeUI(selector = "#widget_options .shiny-input-container", multiple = TRUE)
  rv$widgetConfig$widgetType <<- input$widget_type
  switch(input$widget_type, 
         slider = {
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_label", "Choose a label"),
                      numericInput("slider_min", "Minimum value", 0L),
                      numericInput("slider_max", "Maximum value", 10L),
                      numericInput("slider_def", "Default value", 2L),
                      numericInput("slider_step", "Step size", 1L, 0L),
                      checkboxInput("slider_ticks", "Show tick marks?"),
                      sliderInput("slider_width", "Width of slider (%)", 0, 100, 100, 
                                  ticks = FALSE)
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             sliderInput("slider_test", "I am a slider", 0L, 100L, 20L)
           })
         },
         dropdown = {
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_label", "Choose a label"),
                      selectizeInput("dd_choices", "Choices to select from", 
                                     c(), selected = NULL,
                                     multiple = TRUE, options = list(
                                       'create' = TRUE,
                                       'persist' = FALSE)),
                      selectizeInput("dd_choices", "Aliases for choices (optional)", 
                                     c(), selected = NULL,
                                     multiple = TRUE, options = list(
                                       'create' = TRUE,
                                       'persist' = FALSE)),
                      textInput("dd_default", "Default value")
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             selectInput("dropdown_test", "I am a dropdown menu", c("option 1", "option 2", "option 3"))
           })
         },
         checkbox = {
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_label", "Choose a label"),
                      checkboxInput("cb_default", "Default value")
                    ), 
                    where = "beforeEnd")
           output$widget_preview <- renderUI({
             tagList(
               tags$label(class = "cb-label", "for" = "checkbox_test", 
                          "I am a checkbox"), 
               tags$div(
                 tags$label(class = "checkbox-material", "for" = "checkbox_test", 
                            checkboxInput("checkbox_test", label = NULL))
               )
             )
           })
         },
         date = {
           output$widget_preview <- renderUI({
             dateInput("date_test", "I am a date selector")
           })
         },
         daterange = {
           output$widget_preview <- renderUI({
             dateRangeInput("daterange_test", "I am a date range selector")
           })
         },
         textinput = {
           output$widget_preview <- renderUI({
             textInput("text_test", "I am a text input")
           })
         }
  )
  hideEl(session, "#table_preview")
})
observeEvent(input$widget_label, {
  rv$widgetConfig$label <<- input$widget_label
  updateSliderInput(session, "slider_test", value = input$slider_test, 
                    label = input$widget_label)
})
observeEvent(input$slider_min, {
  if(!is.numeric(input$slider_min))
    return()
  updateSliderInput(session, "slider_test", min = input$slider_min)
  rv$widgetConfig$min <<- input$slider_min
})
observeEvent(input$slider_max, {
  if(!is.numeric(input$slider_max))
    return()
  updateSliderInput(session, "slider_test", max = input$slider_max)
  rv$widgetConfig$max <<- input$slider_max
})
observeEvent(input$slider_def, {
  if(!is.numeric(input$slider_def))
    return()
  updateSliderInput(session, "slider_test", value = input$slider_def)
  rv$widgetConfig$default <<- input$slider_def
})
observeEvent(input$slider_step, {
  if(!is.numeric(input$slider_step))
    return()
  updateSliderInput(session, "slider_test", value = input$slider_test, 
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

#  ==============================
#          SAVE JSON
#  ==============================
observeEvent(input$saveWidget, {
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L)
  
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
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L)
  
  configJSON$dataRendering[[tolower(input$widget_symbol)]] <<- rv$widgetConfig
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
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L)
  widgetName <- input$widget_symbol
  if(widgetName %in% CLParameter){
    clID <- match(widgetName, CLParameter)
    widgetSymbols <<- widgetSymbols[-match(widgetName, widgetSymbols)]
    if(identical(names(CLParameter)[clID], "dd")){
      widgetName <- prefixDDPar %+% tolower(widgetName)
    }else{
      widgetName <- prefixGMSOpt %+% tolower(widgetName)
    }
    CLParameter <<- CLParameter[-clID]
    updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
  }else{
    widgetName <- tolower(widgetName)
  }
  configJSON$inputWidgets[[widgetName]] <<- NULL
  write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
  removeModal()
  showHideEl(session, "#widgetUpdateSuccess", 4000L)
})