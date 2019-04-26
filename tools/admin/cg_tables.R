data <- tibble("Column 1" = 1:10, "Column 2" = 11:20, "Column 3" = letters[1:10], "Column 4" = letters[11:20])
attr(data, "aliases") <- c("Column 1", "Column 2", "Column 3", "Column 4")

observeEvent(input$table_type, {
  req(length(input$table_type) > 0L)
  
  rv$tableConfig$tableType <<- input$table_type
  
  removeUI(selector = "#table_wrapper .shiny-input-container", multiple = TRUE)
  
  if(identical(input$table_type, "hot")){
    insertUI(selector = "#table_wrapper", getHotOptions(), where = "beforeEnd")
    hideEl(session, "#preview-output-dt")
    showEl(session, "#preview-output-hot")
  }else if(identical(input$table_type, "dt")){
    insertUI(selector = "#table_wrapper", getDtOptions(), where = "beforeEnd")
    hideEl(session, "#preview-output-hot")
    showEl(session, "#preview-output-dt")
    
  }
})

getHotOptions <- reactive({
  tagList(
    numericInput("hot_height", "Height of input tables (in px)", min = 0L,
                 value = if(length(configJSON$handsontable$height)) configJSON$handsontable$height else 700L),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_readonly",
                        "Restrict editing of tables? If activated, tables can not be modified by the user."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_readonly", value = configJSON$handsontable$readonly, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "Highlight the column of current active cell (note that changing this setting does currently not reflect in the live preview)."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_highcol", value = if(identical(configJSON$handsontable$highlightCol, FALSE)) FALSE else TRUE, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_highrow",
                        "Highlight the row of current active cell (note that changing this setting does currently not reflect in the live preview)."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_highrow", value = if(identical(configJSON$handsontable$highlightRow, FALSE)) FALSE else TRUE, label = NULL)
               ))
    ),
    selectInput("hot_strech", "Default scenario comparison mode.", 
                choices = c("No strech" = "none", "Strech last column" = "last", 
                            "Strech all columns" = "all"),
                selected = if(length(configJSON$handsontable$stretchH)) configJSON$handsontable$stretchH else "all"
    ),
    conditionalPanel(
      condition = "input.hot_strech !== 'all'",
      numericInput("hot_colwidth", "Width of a single column", min = 0L, 
                   value = if(length(configJSON$handsontable$colWidths)) configJSON$handsontable$colWidths else 10L)),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_sort",
                        "Enable column sorting in ascending/descending order 
                        (by clicking on column name in header row)"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_sort", value = if(identical(configJSON$handsontable$columnSorting, FALSE)) FALSE else TRUE, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_move",
                        "Enable manual column moving. When active, the 
                        column positions can be manually changed."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_move", value = configJSON$handsontable$manualColumnMove, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_resize",
                        "Enable manual column resizing."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_resize", value = configJSON$handsontable$manualColumnResize, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_context_enable",
                        "Enable table context menu (accessible via right mouse click)."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_context_enable", 
                                        value = if(identical(configJSON$handsontable$contextMenu$enabled, FALSE)) FALSE else TRUE, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
             conditionalPanel(
               condition = "input.hot_context_enable == true",
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_rowedit",
                                   "Enable row editing"),
                        tags$div(
                          tags$label(class = "checkbox-material",
                                     checkboxInput("hot_context_rowedit", 
                                                   value = if(identical(configJSON$handsontable$contextMenu$allowRowEdit, FALSE)) FALSE else TRUE, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_coledit",
                                   "Enable column editing"),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_coledit", 
                                                   value = if(identical(configJSON$handsontable$contextMenu$allowColEdit, FALSE)) FALSE else TRUE, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_readonly",
                                   "Enable read-only toggle"),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_readonly", 
                                                   value = configJSON$handsontable$contextMenu$allowReadOnly, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_comments",
                                   "Enable comments in table. Will be ignored when solving the model and are not saved in database."),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_comments", 
                                                   value = configJSON$handsontable$contextMenu$allowComments, label = NULL)
                          ))
               )
             )
    )
  )
})

getDtOptions <- reactive({
  tagList(
    selectInput("dt_class", "Table style", 
               choices = c("display", "cell-border", "compact", "hover", "nowrap", "order-column", "row-border", "stripe"), 
               selected = if(length(configJSON$datatable$class)) configJSON$datatable$class else "display"),
    selectInput("dt_filter", "Include column filters", 
                choices = c("No column filters" = "none", "Position: bottom" = "bottom", 
                            "Position: top" = "top"),
                selected = if(length(configJSON$datatable$filter)) configJSON$datatable$filter else "bottom"
    ),
    numericInput("dt_pagelength", "Number of items to display per page", 
                 value = if(length(configJSON$datatable$options$pageLength)) configJSON$datatable$options$pageLength else 15L),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "dt_rownames", "Show row numbers?"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_rownames", value = configJSON$datatable$rownames, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "dt_buttons", "Add export buttons?"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_buttons", value = "Buttons" %in% configJSON$datatable$extensions, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "dt_colReorder", "Allow reordering of columns?"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_colReorder", value = "ColReorder" %in% configJSON$datatable$extensions, label = NULL)
               ))
    )
  )
})


observeEvent(input$hot_height, {
  rv$tableConfig$handsontable$height <<- input$hot_height
})
observeEvent(input$hot_readonly, {
  rv$tableConfig$handsontable$readonly <<- input$hot_readonly
})
observeEvent(input$hot_highcol, {
  rv$tableConfig$handsontable$highlightCol <<- input$hot_highcol
})
observeEvent(input$hot_highrow, {
  rv$tableConfig$handsontable$highlightRow <<- input$hot_highrow
})
observeEvent(input$hot_strech, {
  rv$tableConfig$handsontable$stretchH <<- input$hot_strech
})
observeEvent(input$hot_sort, {
  rv$tableConfig$handsontable$columnSorting <<- input$hot_sort
})
observeEvent(input$hot_move, {
  rv$tableConfig$handsontable$manualColumnMove <<- input$hot_move
})
observeEvent(input$hot_resize, {
  rv$tableConfig$handsontable$manualColumnResize <<- input$hot_resize
})
observeEvent(input$hot_colwidth, {
  rv$tableConfig$handsontable$colWidths <<- input$hot_colwidth
})
observeEvent(input$hot_context_enable, {
  rv$tableConfig$handsontable$contextMenu$enabled <<- input$hot_context_enable
})
observeEvent(input$hot_context_rowedit, {
  rv$tableConfig$handsontable$contextMenu$allowRowEdit <<- input$hot_context_rowedit
})
observeEvent(input$hot_context_coledit, {
  rv$tableConfig$handsontable$contextMenu$allowColEdit <<- input$hot_context_coledit
})
observeEvent(input$hot_context_readonly, {
  rv$tableConfig$handsontable$contextMenu$allowReadOnly <<- input$hot_context_readonly
})
observeEvent(input$hot_context_comments, {
  rv$tableConfig$handsontable$contextMenu$allowComments <<- input$hot_context_comments
})

observeEvent(input$dt_class, {
  rv$tableConfig$datatable$class <<- input$dt_class
})
observeEvent(input$dt_filter, {
  rv$tableConfig$datatable$filter <<- input$dt_filter
})
observeEvent(input$dt_rownames, {
  rv$tableConfig$datatable$rownames <<- input$dt_rownames
})
observeEvent(input$dt_extension, {
  rv$tableConfig$datatable$extensions <<- input$dt_extension
})
observeEvent(input$dt_dom, {
  if(nchar(input$dt_dom))
    rv$tableConfig$datatable$options$dom <<- input$dt_dom
  else
    rv$tableConfig$datatable$options$dom <<- NULL
})
observeEvent(input$dt_buttons, {
  if(nchar(input$dt_buttons))
    rv$tableConfig$datatable$options$buttons <<- input$dt_buttons
  else
    rv$tableConfig$datatable$options$buttons <<- NULL
})
observeEvent(input$dt_pagelength, {
  rv$tableConfig$datatable$options$pageLength <<- input$dt_pagelength
})
observeEvent(input$dt_buttons, {
  if(input$dt_buttons){
    rv$tableConfig$datatable$options$dom <<- 'Bfrtip'
    rv$tableConfig$datatable$options$buttons <<- c('copy', 'csv', 'excel', 'pdf', 'print')
    rv$tableConfig$datatable$extensions <<- c(rv$tableConfig$datatable$extensions, "Buttons")
  }else{
    rv$tableConfig$datatable$options$dom <<- NULL
    rv$tableConfig$datatable$options$buttons <<- NULL
    newExtensionsVector <- rv$tableConfig$datatable$extensions[!rv$tableConfig$datatable$extensions %in% "Buttons"]
    if(!length(newExtensionsVector))
      newExtensionsVector <- NULL
    rv$tableConfig$datatable$extensions <<- newExtensionsVector
  }
})
observeEvent(input$dt_colReorder, {
  if(input$dt_colReorder){
    rv$tableConfig$datatable$options$colReorder <<- TRUE
    rv$tableConfig$datatable$extensions <<- c(rv$tableConfig$datatable$extensions, "ColReorder")
  }else{
    rv$tableConfig$datatable$options$colReorder <<- NULL
    newExtensionsVector <- rv$tableConfig$datatable$extensions[!rv$tableConfig$datatable$extensions %in% "ColReorder"]
    if(!length(newExtensionsVector))
      newExtensionsVector <- NULL
    rv$tableConfig$datatable$extensions <<- newExtensionsVector
  }
})


observe({
  req(rv$tableConfig$tableType)
  tryCatch({
    if(isolate(rv$tableConfig$tableType) == "hot"){
      hotOptions        <- rv$tableConfig$handsontable
      print(hotOptions)
      output[["table_preview_hot"]] <- renderRHandsontable({
        ht <- rhandsontable(data = data, height = hotOptions$height, 
                            colHeaders = letters[1:4],
                            search = hotOptions$search, 
                            readOnly = hotOptions$readonly, selectCallback = TRUE)
        ht <- hot_table(ht, contextMenu = hotOptions$contextMenu$enabled, 
                        highlightCol = hotOptions$highlightCol, 
                        highlightRow = hotOptions$highlightRow,
                        rowHeaderWidth = hotOptions$rowHeaderWidth,
                        enableComments = TRUE, 
                        stretchH = hotOptions$stretchH,
                        overflow = hotOptions$overflow)
        ht <- hot_context_menu(ht, allowRowEdit = hotOptions$contextMenu$allowRowEdit, 
                               allowColEdit = hotOptions$contextMenu$allowColEdit, 
                               allowReadOnly = hotOptions$contextMenu$allowReadOnly, 
                               allowComments = hotOptions$contextMenu$allowComments)
        ht <- hot_cols(ht, columnSorting = hotOptions$columnSorting, 
                       manualColumnMove = hotOptions$manualColumnMove, 
                       manualColumnResize = hotOptions$manualColumnResize, 
                       colWidths = hotOptions$colWidths, 
                       fixedColumnsLeft = hotOptions$fixedColumnsLeft)
      })
    }else if(isolate(rv$tableConfig$tableType) == "dt"){
      dtOptions <- rv$tableConfig$datatable
      print(dtOptions)
      print("asdasdadasdasd")
      callModule(renderData, "table_preview_dt", type = "datatable", 
                 data = data, dtOptions = dtOptions,
                 roundPrecision = 2, modelDir = modelDir)
    }else if(isolate(rv$tableConfig$tableType) == "piv"){
      return()
    }
    hideEl(session, "#preview-error")
  }, error = function(e) {
    hideEl(session, "#preview-content-hot")
    hideEl(session, "#preview-content-dt")
  })
}, priority = -1000)

#  =======================================
#          SAVE JSON (automatically)
#  =======================================
observeEvent(rv$tableConfig, {
  req(length(rv$tableConfig$tableType))
  switch(rv$tableConfig$tableType,
         "hot" = {
           configJSON$handsontable <<- rv$tableConfig$handsontable
         },
         "dt" = {
           configJSON$datatable <<- rv$tableConfig$datatable
         },
         "piv" = {
           configJSON$pivottable <<- rv$tableConfig$pivottable
         }, 
         {
           return()
         })
  write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
})
