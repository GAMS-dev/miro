observeEvent(input$widget_symbol, {
  req(length(input$widget_symbol))
  if(input$widget_symbol %in% scalarInputSym){
    symID <- match(input$widget_symbol, scalarInputSym)
    if(is.na(symID)){
      showHideEl(session, "#unknownErrorWidgets", 4000L)
      return()
    }
    widgetOptions <- c("Slider" = "slider", "Dropdown menu" = "dropdown", "Checkbox" = "checkbox")
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
observeEvent(input$widget_type, {
  if(identical(input$widget_type, "table")){
    output$table_preview <- renderRHandsontable(rhandsontable(tibble("Column 1" = 1:10, "Column 2" = letters[1:10])))
    output$widget_preview <- renderUI("")
    showEl(session, "#table_preview")
    return()
  }
  removeUI(selector = "#widget_options .shiny-input-container", multiple = TRUE)
  switch(input$widget_type, 
         slider = {
           insertUI(selector = "#widget_options",
                    tagList(
                      textInput("widget_label", "Choose a label"),
                      numericInput("slider_min", "Minimum value", 0L),
                      numericInput("slider_max", "Maximum value", 10L),
                      numericInput("slider_def", "Default value", 2L)
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
         text = {
           output$widget_preview <- renderUI({
             textInput("text_test", "I am a text input")
           })
         }
  )
  hideEl(session, "#table_preview")
})