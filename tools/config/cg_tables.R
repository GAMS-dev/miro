tableSymbols <- inputSymMultiDimChoices
if(length(tableSymbols)){
  updateSelectInput(session, "table_symbol", choices = tableSymbols)
  noTableSymbols <- FALSE
}


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
langSpecificTable$buttons <- c("copy" = "copy", "CSV" = "csv", "Excel" = "excel", "PDF" = "pdf", "print" = "print")
names(langSpecificTable$buttons) <- lang$adminMode$tables$dt$buttons$choices

observeEvent(input$table_type, {
  req(length(input$table_type) > 0L)
  
  rv$tableConfig$tableType <<- input$table_type
  
  removeUI(selector = "#table_wrapper .shiny-input-container", multiple = TRUE)
  
  if(identical(input$table_type, "hot")){
    insertUI(selector = "#table_wrapper", getHotOptions(), where = "beforeEnd")
    hideEl(session, "#preview-output-dt")
    hideEl(session, "#preview-output-tableWidget")
    showEl(session, "#preview-output-hot")
  }else if(identical(input$table_type, "dt")){
    insertUI(selector = "#table_wrapper", getDtOptions(), where = "beforeEnd")
    hideEl(session, "#preview-output-hot")
    hideEl(session, "#preview-output-tableWidget")
    showEl(session, "#preview-output-dt")
  }else if(identical(input$table_type, "symbol")){
    hideEl(session, "#preview-output-hot")
    hideEl(session, "#preview-output-dt")
    showEl(session, "#preview-output-tableWidget")
    
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
                            value = if(length(configJSON$handsontable$colWidths)) configJSON$handsontable$colWidths else 150L))),
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
             tags$label(class = "cb-label info-position", "for" = "dt_buttons", 
                        tags$div(lang$adminMode$tables$dt$buttons$label, 
                                 tags$a("", title = paste0(lang$adminMode$tables$dt$buttons$title, " - ",
                                                           tolower(lang$adminMode$general$ui$tooltipDocs)), class="info-wrapper", href="https://gams.com/miro/customize.html#export-buttons", 
                                        tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
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

observeEvent({input$table_symbol
  input$table_type
  rv$reset_table_input}, {
    req(identical(input$table_type, "symbol"), length(input$table_symbol) > 0L, nchar(input$table_symbol) > 0L)
    currentTableSymbolName <<- input$table_symbol
    pivotCols <- NULL
    if(length(inputSymHeaders[[input$table_symbol]]) > 2L){
      numericHeaders <- vapply(modelIn[[input$table_symbol]]$headers, 
                               function(header) identical(header$type, "numeric"), 
                               logical(1L), USE.NAMES = FALSE)
      if(sum(numericHeaders) <= 1L){
        pivotCols <- inputSymHeaders[[input$table_symbol]][!numericHeaders]
      }
    }
    rv$tableWidgetConfig <- list() 
    currentConfig <- NULL
    if(currentTableSymbolName %in% names(configJSON$inputWidgets)){
      currentConfig <- configJSON$inputWidgets[[currentTableSymbolName]]
    }
    tableAlias <- ''
    if(length(currentConfig$alias) && nchar(currentConfig$alias)){
      tableAlias <- currentConfig$alias
    }else{
      tablesymbolID <- match(isolate(input$table_symbol), tableSymbols)
      if(!is.na(tablesymbolID)){
        tableAlias <- names(inputSymMultiDim)[[tablesymbolID]]
      }
    }
    
    rv$tableWidgetConfig <- list(widgetType = "table",
                                 alias = tableAlias,
                                 readonly = isTRUE(currentConfig$readonly),
                                 readonlyCols = currentConfig$readonlyCols,
                                 heatmap = isTRUE(currentConfig$heatmap),
                                 bigData = isTRUE(currentConfig$bigData))
    if(length(currentConfig$pivotCols)){
      rv$tableWidgetConfig$pivotCols <- currentConfig$pivotCols
    }
    removeUI(selector = "#table_wrapper .shiny-input-container", multiple = TRUE)
    insertUI(selector = "#table_wrapper",
             tagList(
               tags$div(class="option-wrapper",
                        textInput("table_alias", lang$adminMode$widgets$ui$alias, value = rv$tableWidgetConfig$alias)),
               checkboxInput_MIRO("table_bigdata", lang$adminMode$widgets$table$bigData, value = isTRUE(rv$tableWidgetConfig$bigData)),
               checkboxInput_MIRO("table_readonly", lang$adminMode$widgets$table$readonly, value = rv$tableWidgetConfig$readonly),
               if(length(pivotCols)){
                 tags$div(class="option-wrapper",
                          selectInput("table_pivotCols", lang$adminMode$widgets$table$pivotCols, 
                                      choices = c(`_` = "_", pivotCols), 
                                      selected = if(length(rv$tableWidgetConfig$pivotCols)) rv$tableWidgetConfig$pivotCols else "_"))
               },
               conditionalPanel(condition = "input.table_bigdata===false && input.table_pivotCols === '_'",
                                tags$div(class="option-wrapper",
                                         selectInput("table_readonlyCols", lang$adminMode$widgets$table$readonlyCols, 
                                                     choices = inputSymHeaders[[input$table_symbol]], 
                                                     selected = rv$tableWidgetConfig$readonlyCols, multiple = TRUE))
               ),
               conditionalPanel(condition = "input.table_bigdata===false",
                                tags$div(class="option-wrapper",
                                         checkboxInput_MIRO("table_heatmap", 
                                                            lang$adminMode$widgets$table$heatmap, 
                                                            value = rv$tableWidgetConfig$heatmap))
               )), 
             where = "beforeEnd")
    if(isTRUE(rv$tableWidgetConfig$bigData)){
      #hideEl(session, "#hot_preview")
    }else{
      showEl(session, "#hot_preview")
    }
})

createTableData <- function(symbol, pivotCol){
  headersRaw <- inputSymHeaders[[symbol]]
  headersTmp <- names(headersRaw)
  data        <- data.frame(matrix(c(replicate(length(headersTmp) - 1L,
                                               letters[1:10]), 1:10),
                                   10))
  isPivotTable <- FALSE
  if(length(pivotCol) && pivotCol != "_"){
    isPivotTable <- TRUE
    pivotIdx <- match(pivotCol, inputSymHeaders[[input$table_symbol]])[[1L]]
    data[length(data)] <- as.integer(data[[length(data)]])
    data <- pivot_wider(data, names_from = !!pivotIdx, 
                        values_from = !!length(data))
    attrTmp <- headersTmp[-c(pivotIdx, length(headersTmp))]
    attrTmp <- c(attrTmp, 
                 names(data)[seq(length(attrTmp) + 1L, 
                                 length(data))])
    headersTmp  <- attrTmp
  }
  return(list(data = data, headers = headersTmp, headersRaw = headersRaw,
              isPivotTable = isPivotTable))
}
output$hot_preview <- renderRHandsontable({
  req(input$table_symbol %in% names(inputSymHeaders))
  if(isTRUE(rv$tableWidgetConfig$bigData)){
    return()
  }
  data <- createTableData(input$table_symbol, input$table_pivotCols)
  
  headersTmp <- data$headersRaw
  headersUnnamed <- unname(headersTmp)
  pivotTable <- data$isPivotTable
  data <- data$data
  
  ht <- rhandsontable(data = data,
                      colHeaders = headersUnnamed,
                      readOnly = input$table_readonly,
                      digits = NA)
  if(!pivotTable && length(input$table_readonlyCols) && 
     input$table_readonlyCols %in% headersTmp){
    ht <- hot_col(ht, headersUnnamed[match(input$table_readonlyCols, 
                                           headersTmp)], 
                  readOnly = TRUE)
  }
  if(isTRUE(input$table_heatmap)){
    return(hot_heatmap(ht))
  }else{
    return(ht)
  }
})

output$dt_preview <- renderDT({
  req(input$table_symbol %in% names(inputSymHeaders))

  data <- createTableData(input$table_symbol, input$table_pivotCols)
  
  headersTmp <- unname(data$headers)
  data <- data$data
  
  dtOptions <- list(editable = !isTRUE(input$table_readonly),
                    colnames = headersTmp)
  
  if(!isTRUE(rv$tableWidgetConfig$bigData)){
    showEl(session, "#hot_preview")
    return()
  }
  #hideEl(session, "#hot_preview")
  return(renderDTable(data, dtOptions, render = FALSE))
})

observeEvent(input$hot_height, {
  if(is.na(input$hot_height))
    configJSON$tableConfig$handsontable$height <<- NULL
  if(!is.na(input$hot_height))
    rv$tableConfig$handsontable$height <<- input$hot_height
  else
    rv$tableConfig$handsontable$height <<- NULL
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

observeEvent(input$hot_colwidth, {
  if(!is.na(input$hot_colwidth) && input$hot_colwidth != 0)
    rv$tableConfig$handsontable$colWidths <<- input$hot_colwidth
  else
    rv$tableConfig$handsontable$colWidths <<- 300L
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
  if(is.na(input$dt_pagelength))
    configJSON$tableConfig$datatable$options$pageLength <<- NULL
  if(!is.na(input$dt_pagelength))
    rv$tableConfig$datatable$options$pageLength <<- input$dt_pagelength
  else
    rv$tableConfig$datatable$options$pageLength <<- NULL
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

observe({
  req(rv$tableConfig$tableType)
  tryCatch({
    if(isolate(rv$tableConfig$tableType) == "hot"){
      hotOptions        <- rv$tableConfig$handsontable
      if(is.null(hotOptions)){
        return()
      }
      output[["table_preview_hot"]] <- renderRHandsontable({
        ht <- rhandsontable(data = data, height = hotOptions$height, 
                            colHeaders = letters[1:4],
                            search = hotOptions$search, 
                            readOnly = hotOptions$readonly, selectCallback = TRUE,
                            digits = NA)
        ht <- hot_table(ht, contextMenu = hotOptions$contextMenu$enabled, 
                        highlightCol = hotOptions$highlightCol, 
                        highlightRow = hotOptions$highlightRow,
                        rowHeaderWidth = hotOptions$rowHeaderWidth,
                        stretchH = hotOptions$stretchH,
                        overflow = hotOptions$overflow)
        if(isTRUE(hotOptions$contextMenu$enabled)){
          ht <- hot_context_menu(ht, allowRowEdit = hotOptions$contextMenu$allowRowEdit, 
                                 allowColEdit = hotOptions$contextMenu$allowColEdit, 
                                 allowReadOnly = hotOptions$contextMenu$allowReadOnly)
        }
        ht <- hot_cols(ht, columnSorting = hotOptions$columnSorting, 
                       manualColumnMove = hotOptions$manualColumnMove, 
                       manualColumnResize = hotOptions$manualColumnResize, 
                       colWidths = hotOptions$colWidths, 
                       fixedColumnsLeft = hotOptions$fixedColumnsLeft)
      })
    }else if(isolate(rv$tableConfig$tableType) == "dt"){
      dtOptions <- rv$tableConfig$datatable

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


observeEvent(input$table_alias, {
  if(length(input$table_alias))
    rv$tableWidgetConfig$alias <<- input$table_alias
  else
    rv$tableWidgetConfig$alias <<- NULL
})
observeEvent(input$table_bigdata, {
  if(input$table_bigdata == TRUE){
    rv$tableWidgetConfig$bigData <<- TRUE
    rv$tableWidgetConfig$heatmap <<- FALSE
  }else{
    rv$tableWidgetConfig$bigData <<- FALSE
    rv$tableWidgetConfig$heatmap <<- input$table_heatmap
  }
})
observeEvent(input$table_readonly, {
  rv$tableWidgetConfig$readonly <<- input$table_readonly
})
observeEvent(input$table_readonlyCols, ignoreNULL = FALSE, {
  if(!length(input$table_readonlyCols)){
    configJSON$tableWidgetConfig$readonlyCols <<- NULL
  }
  rv$tableWidgetConfig$readonlyCols <<- input$table_readonlyCols
})
observeEvent(input$table_pivotCols, {
  rv$tableWidgetConfig$pivotCols <<- input$table_pivotCols
})
observeEvent(input$table_heatmap, {
  rv$tableWidgetConfig$heatmap <<- input$table_heatmap
})
observeEvent(c(input$table_pivotCols, input$table_readonly, input$table_heatmap), {
  if(!identical(input$table_pivotCols, "_")){
    if(isTRUE(input$table_readonly) || isTRUE(input$table_heatmap))
      showEl(session, "#pivotColsRestriction")
    else
      hideEl(session, "#pivotColsRestriction")
  }else{
    hideEl(session, "#pivotColsRestriction")
  }
})


validateTableConfig <- function(configJSON){
  if(!length(configJSON$alias) || identical(nchar(trimws(configJSON$alias)), 0L)){
    return(lang$adminMode$widgets$validate[["val1"]])
  }
  if(identical(grepl("\\s", currentTableSymbolName), TRUE)){
    return(lang$adminMode$widgets$validate$val39)
  }
  
  if(!is.logical(configJSON$readonly)){
    return(lang$adminMode$widgets$validate$val32)
  }
  if(!is.logical(configJSON$heatmap)){
    return(lang$adminMode$widgets$validate$val33)
  }
  if(any(!configJSON$readonlyCols %in% inputSymHeaders[[currentTableSymbolName]])){
    return(lang$adminMode$widgets$validate$val34)
  }
  return("")
}
  
  
#  =====================================================================
#          SAVE JSON (global settings are saved automatically)
#  =====================================================================
observeEvent(rv$tableConfig, {
  req(length(rv$tableConfig$tableType))
  switch(rv$tableConfig$tableType,
         "hot" = {
           configJSON$handsontable <<- rv$tableConfig$handsontable
           if(identical(configJSON$handsontable$colWidths, "[]"))
             configJSON$handsontable$colWidths <<- NULL
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
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
})

observeEvent(input$saveTableWidget, {
  req(length(currentTableSymbolName) > 0L, nchar(currentTableSymbolName) > 0L)
  errMsg <- validateTableConfig(rv$tableWidgetConfig)
  if(nchar(errMsg)){
    showHideEl(session, "#tableValidationErr", 5000L, errMsg)
    return()
  }
  rv$saveTableConfirm <- rv$saveTableConfirm + 1L
})
observeEvent(virtualActionButton(input$saveTableConfirm, rv$saveTableConfirm), {
  req(length(currentTableSymbolName) > 0L, nchar(currentTableSymbolName) > 0L)

  configJSON$inputWidgets[[currentTableSymbolName]] <<- rv$tableWidgetConfig
  if(!length(configJSON$inputWidgets[[currentTableSymbolName]]$readonlyCols)){
    configJSON$inputWidgets[[currentTableSymbolName]]$readonlyCols <<- NULL
  }
  if(identical(configJSON$inputWidgets[[currentTableSymbolName]]$pivotCols, "_")){
    configJSON$inputWidgets[[currentTableSymbolName]]$pivotCols <<- NULL
  }
  
  currentTableSymbolID <- match(currentTableSymbolName, tableSymbols)
  if(!is.na(currentTableSymbolID) && 
     !identical(names(tableSymbols)[[currentTableSymbolID]], paste(currentTableSymbolName, ": ",
                                                                   rv$tableWidgetConfig$alias))){
    names(tableSymbols)[[currentTableSymbolID]] <<- paste0(currentTableSymbolName, ": ", 
                                                           rv$tableWidgetConfig$alias)
    updateSelectInput(session, "table_symbol", choices = tableSymbols)
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  
  if(noTableSymbols){
    hideEl(session, "#noTableSymbolMsg")
    noTableSymbols <<- FALSE
  }
  removeModal()
  showHideEl(session, "#tableWidgetUpdateSuccess", 4000L)
})
observeEvent(input$deleteTableWidget, {
  req(length(input$table_symbol) > 0L, nchar(input$table_symbol) > 0L)
  
  showModal(modalDialog(title = lang$adminMode$tables$symbol$removeDialog$title, lang$adminMode$tables$symbol$removeDialog$message, 
                        footer = tagList(modalButton(lang$adminMode$tables$symbol$removeDialog$cancel), 
                                         actionButton("deleteTableWidgetConfirm", lang$adminMode$tables$symbol$removeDialog$confirm))))
})
observeEvent(input$deleteTableWidgetConfirm, {
  req(length(currentTableSymbolName) > 0L, nchar(currentTableSymbolName) > 0L)
  
  configJSON$inputWidgets[[currentTableSymbolName]] <<- NULL
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  removeModal()
  showHideEl(session, "#tableWidgetUpdateSuccess", 4000L)
  hideEl(session, "#noTableSymbolMsg")
  if(!length(tableSymbols)){
    if(!noTableSymbols){
      showEl(session, "#noTableSymbolMsg")
      noTableSymbols <<- TRUE
    }
    currentTableSymbolName <<- character(0L)
  }else{
    currentTableSymbolID <- match(currentTableSymbolName, tableSymbols)
    names(tableSymbols)[[currentTableSymbolID]] <<- inputSymMultiDimChoices[[currentTableSymbolID]]
    updateSelectInput(session, "table_symbol", choices = tableSymbols)
    rv$reset_table_input <- rv$reset_table_input + 1L
  }
})
