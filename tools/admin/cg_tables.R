data <- tibble("Column 1" = 1:10, "Column 2" = 11:20, "Column 3" = letters[1:10], "Column 4" = letters[11:20])
attr(data, "aliases") <- c("Column 1", "Column 2", "Column 3", "Column 4")
dtExtensions <- Set$new()

langSpecificTable <- list()
langSpecificTable$stretch <- c("No stretch" = "none", "Stretch last column" = "last", "Stretch all columns" = "all")
names(langSpecificTable$stretch) <- lang$adminMode$tables$hot$stretchChoices
langSpecificTable$class <- c("display" = "display", "cell-border" = "cell-border", "compact" = "compact", 
                               "hover" = "hover", "nowrap" = "nowrap", "order-column" = "order-column", 
                               "row-border" = "row-border", "stripe" = "stripe")
names(langSpecificTable$class) <- lang$adminMode$tables$dt$class$choices
langSpecificTable$filter <- c("No column filters" = "none", "Position: bottom" = "bottom", 
                             "Position: top" = "top")
names(langSpecificTable$filter) <- lang$adminMode$tables$dt$filter$choices
langSpecificTable$buttons <- c("copy" = "copy", "csv" = "csv", "excel" = "excel", "pdf" = "pdf", "print" = "print")
names(langSpecificTable$buttons) <- lang$adminMode$tables$dt$buttons$choices

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
    tags$div(style = "max-width:400px;",
      numericInput("hot_height", lang$adminMode$tables$hot$height, min = 0L,
                 value = if(length(configJSON$handsontable$height)) configJSON$handsontable$height else config$handsontable$height)),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_readonly",
                        lang$adminMode$tables$hot$readonly),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_readonly", value = if(length(configJSON$handsontable$readonly)) configJSON$handsontable$readonly 
                                        else config$handsontable$readonly, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", lang$adminMode$tables$hot$highcol),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_highcol", value = if(length(configJSON$handsontable$highlightCol)) configJSON$handsontable$highlightCol 
                                        else config$handsontable$highlightCol, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_highrow",
                        lang$adminMode$tables$hot$highrow),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_highrow", value = if(length(configJSON$handsontable$highlightRow)) configJSON$handsontable$highlightRow 
                                        else config$handsontable$highlightRow, label = NULL)
               ))
    ),
    tags$div(style = "max-width:400px;",
             selectInput("hot_stretch", lang$adminMode$tables$hot$stretch, 
                         choices = langSpecificTable$stretch,
                         selected = if(length(configJSON$handsontable$stretchH)) configJSON$handsontable$stretchH else config$handsontable$stretchH
             )),
    conditionalPanel(
      condition = "input.hot_stretch !== 'all'",
      tags$div(class = "shiny-input-container",
               tags$label(class = "cb-label", "for" = "hot_customWidth",
                          lang$adminMode$tables$hot$stretchCustom),
               tags$div(
                 tags$label(class = "checkbox-material", 
                            checkboxInput("hot_customWidth", label = NULL, value = if(length(configJSON$handsontable$colWidths)) TRUE else FALSE)
                 ))
      )),
    conditionalPanel(
      condition = "input.hot_customWidth===true && input.hot_stretch !== 'all'",
      tags$div(style = "padding-left:40px;max-width:440px;",
               numericInput("hot_colwidth", lang$adminMode$tables$hot$colwidth, min = 0L, 
                            value = if(length(configJSON$handsontable$colWidths) && !identical(configJSON$handsontable$colWidths, "[]")) configJSON$handsontable$colWidths else 0L))),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_sort",
                        lang$adminMode$tables$hot$sort),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_sort", value = if(length(configJSON$handsontable$columnSorting)) configJSON$handsontable$columnSorting 
                                        else config$handsontable$columnSorting, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_resize",
                        lang$adminMode$tables$hot$resize),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_resize", value = if(length(configJSON$handsontable$manualColumnResize)) configJSON$handsontable$manualColumnResize 
                                        else config$handsontable$manualColumnResize, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "hot_context_enable",
                        lang$adminMode$tables$hot$enable),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("hot_context_enable", 
                                        value = if(length(configJSON$handsontable$contextMenu$enabled)) configJSON$handsontable$contextMenu$enabled 
                                        else config$handsontable$contextMenu$enabled, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;padding-left:40px;",
             conditionalPanel(
               condition = "input.hot_context_enable == true",
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_rowedit",
                                   lang$adminMode$tables$hot$contextRowedit),
                        tags$div(
                          tags$label(class = "checkbox-material",
                                     checkboxInput("hot_context_rowedit", 
                                                   value = if(length(configJSON$handsontable$contextMenu$allowRowEdit)) configJSON$handsontable$contextMenu$allowRowEdit 
                                                   else config$handsontable$contextMenu$allowRowEdit, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_coledit",
                                   lang$adminMode$tables$hot$contextColedit),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_coledit", 
                                                   value = if(length(configJSON$handsontable$contextMenu$allowColEdit)) configJSON$handsontable$contextMenu$allowColEdit 
                                                   else config$handsontable$contextMenu$allowColEdit, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_readonly",
                                   lang$adminMode$tables$hot$contextReadonly),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_readonly", 
                                                   value = if(length(configJSON$handsontable$contextMenu$allowReadOnly)) configJSON$handsontable$contextMenu$allowReadOnly 
                                                   else config$handsontable$contextMenu$allowReadOnly, label = NULL)
                          ))
               ),
               tags$div(tags$label(class = "cb-label", "for" = "hot_context_comments",
                                   lang$adminMode$tables$hot$contextComments),
                        tags$div(
                          tags$label(class = "checkbox-material", 
                                     checkboxInput("hot_context_comments", 
                                                   value = if(length(configJSON$handsontable$contextMenu$allowComments)) configJSON$handsontable$contextMenu$allowComments 
                                                   else config$handsontable$contextMenu$allowComments, label = NULL)
                          ))
               )
             )
    )
  )
})

getDtOptions <- reactive({
  tagList(
    tags$div(style = "max-width:400px;",
             selectInput("dt_class", lang$adminMode$tables$dt$class$label, 
                         choices = langSpecificTable$class, 
                         selected = if(length(configJSON$datatable$class)) configJSON$datatable$class else config$datatable$class)),
    tags$div(style = "max-width:400px;",
             selectInput("dt_filter", lang$adminMode$tables$dt$filter$label, 
                         choices = langSpecificTable$filter,
                         selected = if(length(configJSON$datatable$filter)) configJSON$datatable$filter else configJSON$datatable$filter
             )),
    tags$div(style = "max-width:400px;",
             numericInput("dt_pagelength", lang$adminMode$tables$dt$pagelength$label, 
                          value = if(length(configJSON$datatable$options$pageLength)) configJSON$datatable$options$pageLength 
                          else config$datatable$options$pageLength)),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "dt_rownames", lang$adminMode$tables$dt$rownames$label),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_rownames", value = if(length(configJSON$datatable$rownames)) configJSON$datatable$rownames 
                                        else config$datatable$rownames, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "dt_colReorder", lang$adminMode$tables$dt$colReorder$label),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_colReorder", value = "ColReorder" %in% configJSON$datatable$extensions, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             tags$label(class = "cb-label", "for" = "dt_buttons", lang$adminMode$tables$dt$buttons$label),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("dt_buttons", value = "Buttons" %in% configJSON$datatable$extensions, label = NULL)
               ))
    ),
    tags$div(class = "shiny-input-container",
             style = "max-height:800px;max-height: 80vh;padding-right:30px;padding-left:40px;",
             conditionalPanel(
               condition = "input.dt_buttons == true",
               selectInput("dt_buttons_select", 
                           choices = langSpecificTable$buttons, 
                           selected = configJSON$datatable$options$buttons,
                           multiple = TRUE, label = lang$adminMode$tables$dt$buttons$select)
             )
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
observeEvent(input$hot_stretch, {
  rv$tableConfig$handsontable$stretchH <<- input$hot_stretch
  if(identical(input$hot_stretch, "all")){
    updateCheckboxInput(session, "hot_customWidth", value=FALSE)
  }
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
observeEvent(input$hot_customWidth, {
  if(identical(input$hot_customWidth, FALSE))
    updateNumericInput(session, "hot_colwidth", value = 0L)
})

observeEvent(input$hot_colwidth, {
  if(!is.na(input$hot_colwidth) && input$hot_colwidth != 0)
    rv$tableConfig$handsontable$colWidths <<- input$hot_colwidth
  else
    rv$tableConfig$handsontable$colWidths <<- "[]"
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
observeEvent(input$dt_pagelength, {
  rv$tableConfig$datatable$options$pageLength <<- input$dt_pagelength
})
observeEvent(input$dt_buttons, {
  if(identical(isolate({input$dt_buttons}), TRUE)){
    isolate({
      rv$tableConfig$datatable$options$buttons <<- input$dt_buttons_select
    })
    if(length(input$dt_buttons_select)){
      dtExtensions$push("Buttons")
      rv$tableConfig$datatable$options$dom <<- 'Bfrtip'
    }
  }else{
    dtExtensions$delete("Buttons")
    rv$tableConfig$datatable$options$dom <<- NULL
    rv$tableConfig$datatable$options$buttons <<- NULL
    newExtensionsVector <- rv$tableConfig$datatable$extensions[!rv$tableConfig$datatable$extensions %in% "Buttons"]
    if(!length(newExtensionsVector))
      newExtensionsVector <- NULL
    rv$tableConfig$datatable$extensions <<- newExtensionsVector
  }
  rv$tableConfig$datatable$extensions <<- dtExtensions$get()
})
observe({
  input$dt_buttons_select
  isolate({
    rv$tableConfig$datatable$options$buttons <<- input$dt_buttons_select
    if(length(rv$tableConfig$datatable$options$buttons) > 0){
      rv$tableConfig$datatable$options$dom <<- 'Bfrtip'
      dtExtensions$push("Buttons")
    }else{
      rv$tableConfig$datatable$options$dom <<- NULL
      rv$tableConfig$datatable$options$buttons <<- NULL
      dtExtensions$delete("Buttons")
      newExtensionsVector <- rv$tableConfig$datatable$extensions[!rv$tableConfig$datatable$extensions %in% "Buttons"]
      if(!length(newExtensionsVector))
        newExtensionsVector <- NULL
      rv$tableConfig$datatable$extensions <<- newExtensionsVector
    }
    rv$tableConfig$datatable$extensions <<- dtExtensions$get()
  })
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
      print("++++++++++++++++++++++++++")
      #print(hotOptions)
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
      print("++++++++++++++++++++++++++")
      #print(dtOptions)
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
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE)
})
