data <- tibble("Column 1" = 1:10, "Column 2" = 11:20, "Column 3" = letters[1:10], "Column 4" = letters[11:20])

observeEvent(input$table_type, {
  req(length(input$table_type) > 0L)
  rv$tableConfig$tableType <<- input$table_type
  removeUI(selector = "#table_wrapper .shiny-input-container", multiple = TRUE)
  #removeUI(selector = "#table_dt_options .shiny-input-container", multiple = FALSE)
  #removeUI(selector = "#table_hot_options .shiny-input-container", multiple = FALSE)
  if(identical(input$table_type, "hot")){
    insertUI(selector = "#table_wrapper", getHotOptions(), where = "beforeEnd")
    #insertUI(selector = "#table_hot_options",
    #         tags$div(id = "hot_options", getHotOptions()), where = "beforeEnd")
    output$table_preview_hot <- renderRHandsontable(rhandsontable(data = data))
    #hideEl(session, "#dt_options")
    #showEl(session, "#hot_options")
    
    hideEl(session, "#preview-output-dt")
    showEl(session, "#preview-output-hot")
  }else if(identical(input$table_type, "dt")){
    insertUI(selector = "#table_wrapper", getDtOptions(), where = "beforeEnd")
    #insertUI(selector = "#table_dt_options",
    #         tags$div(id = "dt_options", getDtOptions()), where = "beforeEnd")
    
    output$table_preview_dt <- renderDataTable(datatable(data = data))
    #hideEl(session, "#hot_options")
    #showEl(session, "#dt_options")
    
    hideEl(session, "#preview-output-hot")
    showEl(session, "#preview-output-dt")
    
  }else if(identical(input$table_type, "piv")){
    return()
  }
})

getHotOptions <- reactive({
  tagList(
    tags$div("Note: None of these options must be set! 
             If an option is not set, the default option applies."),
    numericInput("hot_height", "Height of input tables (in px)", min = 0L, value = 100L),
    numericInput("hot_width", "Width of input tables (in px)", min = 0L, value = 200L),
    tags$div(tags$label(class = "cb-label",
                        "Restrict editing of tables? If activated, tables can not be modified by the user."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_readonly", value = FALSE, label = NULL)
               ))
    ),
    tags$div(tags$label(class = "cb-label", "Allow browser search function for input table"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_search", value = TRUE, label = NULL)
               ))
    ),
    tags$div(tags$label(class = "cb-label", "Highlight the column of current active cell."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_highcol", value = TRUE, label = NULL)
               ))
    ),
    tags$div(tags$label(class = "cb-label", "Highlight the row of current active cell."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_highrow", value = TRUE, label = NULL)
               ))
    ),
    numericInput("hot_widthhead", "Width of the row number/row header column", min = 0L, value = 10L),
    tags$div(tags$label(class = "cb-label",
                        "Enable comments in table. Will be ignored when solving the model."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_comment", value = TRUE, label = NULL)
               ))
    ),
    selectInput("hot_strech", "Default scenario comparison mode.", 
                choices = c("No strech" = "none", "Strech last column" = "last", 
                            "Strech all columns" = "all"),
                selected = "all"
    ),
    tags$div(tags$label(class = "cb-label",
                        "Enable column sorting in ascending/descending order 
                        (by clicking on column name in header row)"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_sort", value = TRUE, label = NULL)
               ))
    ),
    tags$div(tags$label(class = "cb-label",
                        "Enable manual column moving. When active, the 
                        column positions can be manually changed."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_move", value = FALSE, label = NULL)
               ))
    ),
    tags$div(tags$label(class = "cb-label",
                        "Enable manual column resizing."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_resize", value = FALSE, label = NULL)
               ))
    ),
    numericInput("hot_colwidth", "Width of a single column", min = 0L, value = 10L),
    numericInput("hot_fixcol", "Number of columns fixed on the left", min = 0L, value = 0L),
    tags$div(tags$label(class = "cb-label",
                        "Enable manual column resizing."),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_context_enable", value = TRUE, label = NULL)
               ))
    ),
    tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
             conditionalPanel(
               condition = "input.hot_context_enable == true",
               tags$div(tags$label(class = "cb-label",
                                   "Enable row editing"),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_rowedit", value = TRUE, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label",
                                   "Enable column editing"),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_coledit", value = TRUE, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label",
                                   "Enable read-only toggle"),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_readonly", value = FALSE, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label",
                                   "Enable comments in table. Will be ignored when solving the model."),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_comments", value = TRUE, label = NULL)
                          ))
               )
             )
    )
  )
})

getDtOptions <- reactive({
  tagList(
    tags$div("Note: None of these options must be set! 
             If an option is not set, the default option applies."),
    textInput("dt_class", "###ARRAY MISSING###Css classes used for styling the output tables"),
    selectInput("dt_filter", "Include column filters", 
                choices = c("No column filters" = "none", "Position: bottom" = "bottom", 
                            "Position: top" = "top"),
                selected = "bottom"
    ),
    tags$div(tags$label(class = "cb-label", "Show row names?"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_rownames", value = FALSE, label = NULL)
               ))
    ),
    tags$div(tags$label(class = "cb-label","Use extensions for the output tables. See"),
             tags$a(class = "cb-label", style="color: #f90;", target="_blank", href="https://rstudio.github.io/DT/extensions.html", "here"),
             tags$label(class = "cb-label"," for examples.")),
    selectInput("dt_extension", label = NULL, 
                choices = c("AutoFill", "Buttons", "ColReorder", "ColVis", "FixedColumns",
                            "FixedHeader", "KeyTable", "Responsive", "RowReorder", "Scroller",
                            "Select"),
                multiple = TRUE
    ),
    textInput("dt_dom", "Position and order of table control elements"),
    textInput("dt_buttons", "###wrong###Buttons or groups of buttons to insert"),
    numericInput("dt_pagelength", "Number of items to display per page", 1L),
    tags$div("###column specific options - Why in general datatable options? 
             In handsontable, this is done different!"),
    selectizeInput("test", "test", c(),
                   multiple = TRUE, options = list(
                     'create' = TRUE,
                     'persist' = FALSE)),
    textInput("dt_classname", "Class name"),
    tags$div(tags$label(class = "cb-label", "Show row names?"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_rownames", value = FALSE, label = NULL)
               ))
    )
    
  )
})


observeEvent(input$hot_height, {
  rv$tableConfig$handsontable$height <<- input$hot_height
})
observeEvent(input$hot_width, {
  rv$tableConfig$handsontable$width <<- input$hot_width
})
observeEvent(input$hot_readonly, {
  rv$tableConfig$handsontable$readonly <<- input$hot_readonly
})
observeEvent(input$hot_search, {
  rv$tableConfig$handsontable$search <<- input$hot_search
})
observeEvent(input$hot_highcol, {
  rv$tableConfig$handsontable$highlightCol <<- input$hot_highcol
})
observeEvent(input$hot_highrow, {
  rv$tableConfig$handsontable$highlightRow <<- input$hot_highrow
})
observeEvent(input$hot_widthhead, {
  rv$tableConfig$handsontable$rowHeaderWidth <<- input$hot_widthhead
})
observeEvent(input$hot_comment, {
  rv$tableConfig$handsontable$enableComments <<- input$hot_comment
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
observeEvent(input$hot_fixcol, {
  rv$tableConfig$handsontable$fixedColumnsLeft <<- input$hot_fixcol
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
  rv$tableConfig$datatable$options$dom <<- input$dt_dom
})
observeEvent(input$dt_buttons, {
  rv$tableConfig$datatable$options$buttons <<- input$dt_buttons
})
observeEvent(input$dt_pagelength, {
  rv$tableConfig$datatable$options$pageLength <<- input$dt_pagelength
})
#observeEvent(input$dt_coldef, {
#  rv$tableConfig$datatable$options$columnDefs <<- input$dt_coldef
#})
#observeEvent(input$dt_targets, {
#  rv$tableConfig$datatable$options$columnDefs$targets <<- input$dt_targets
#})
#observeEvent(input$dt_classname, {
#  rv$tableConfig$datatable$options$columnDefs$className <<- input$dt_classname
#})
#observeEvent(input$dt_visible, {
#  rv$tableConfig$datatable$options$columnDefs$visible <<- input$dt_visible
#})
#observeEvent(input$dt_searchable, {
#  rv$tableConfig$datatable$options$columnDefs$searchable <<- input$dt_searchable
#})


observe({
  req(rv$tableConfig$tableType)
  tryCatch({
    if(isolate(rv$tableConfig$tableType) == "hot"){
      hotOptions        <<- rv$tableConfig$handsontable
      print(hotOptions)
      output[["table_preview_hot"]] <- renderRHandsontable({
        ht <<- rhandsontable(data = data, height = hotOptions$height, 
                             colHeaders = NULL,
                             width = hotOptions$width, search = hotOptions$search, 
                             readOnly = hotOptions$readonly, selectCallback = TRUE)
        ht <<- hot_table(ht, contextMenu = hotOptions$contextMenu$enabled, 
                         highlightCol = hotOptions$highlightCol, 
                         highlightRow = hotOptions$highlightRow,
                         rowHeaderWidth = hotOptions$rowHeaderWidth, 
                         enableComments = hotOptions$enableComments, 
                         stretchH = hotOptions$stretchH,
                         overflow = hotOptions$overflow)
        ht <<- hot_context_menu(ht, allowRowEdit = hotOptions$contextMenu$allowRowEdit, 
                                allowColEdit = hotOptions$contextMenu$allowColEdit, 
                                allowReadOnly = hotOptions$contextMenu$allowReadOnly, 
                                allowComments = hotOptions$contextMenu$allowComments)
        ht <<- hot_cols(ht, columnSorting = hotOptions$columnSorting, 
                        manualColumnMove = hotOptions$manualColumnMove, 
                        manualColumnResize = hotOptions$manualColumnResize, 
                        colWidths = hotOptions$colWidths, 
                        fixedColumnsLeft = hotOptions$fixedColumnsLeft)
      })
      #showEl(session, "#preview-output-hot")
      #hideEl(session, "#preview-output-dt")
    }else if(isolate(rv$tableConfig$tableType) == "dt"){
      dtOptions <- rv$tableConfig$datatable
      print(dtOptions)
      callModule(renderData, "table_preview_dt", type = "datatable", 
                 data = data, dtOptions = dtOptions,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      #hideEl(session, "#preview-content-hot")
      #showEl(session, "#preview-content-dt")
    }else if(isolate(rv$tableConfig$tableType) == "piv"){
      return()
    }
    hideEl(session, "#preview-error")
  }, error = function(e) {
    hideEl(session, "#preview-content-hot")
    hideEl(session, "#preview-content-dt")
    #showEl(session, "#preview-error")
    #output[["preview-errmsg"]] <- renderText(toString(e))
  })
}, priority = -1000)




#  ==============================
#          SAVE JSON
#  ==============================
observeEvent(input$saveTable, {
  print(rv$tableConfig)
})
