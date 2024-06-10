tableSymbols <- outputSymMultiDimChoices
currentTableSymbolName <- character(0L)
if (length(tableSymbols)) {
  updateSelectInput(session, "table_symbol", choices = tableSymbols)
  noTableSymbols <- FALSE
}
configuredTable <- FALSE
currentColFormatConfig <- NULL

data <- tibble("Column 1" = 1:10, "Column 2" = 11:20, "Column 3" = letters[1:10], "Column 4" = letters[11:20])
attr(data, "aliases") <- c("Column 1", "Column 2", "Column 3", "Column 4")
dtExtensions <- Set$new()
outputTableExtensions <- Set$new()

langSpecificTable <- list()
langSpecificTable$stretch <- c("No stretch" = "none", "Stretch last column" = "last", "Stretch all columns" = "all")
names(langSpecificTable$stretch) <- lang$adminMode$tables$hot$stretchChoices
langSpecificTable$class <- c(
  "display" = "display", "cell-border" = "cell-border", "compact" = "compact",
  "hover" = "hover", "nowrap" = "nowrap", "order-column" = "order-column",
  "row-border" = "row-border", "stripe" = "stripe"
)
names(langSpecificTable$class) <- lang$adminMode$tables$dt$class$choices
langSpecificTable$filter <- c(
  "No column filters" = "none", "Position: bottom" = "bottom",
  "Position: top" = "top"
)
names(langSpecificTable$filter) <- lang$adminMode$tables$dt$filter$choices
langSpecificTable$buttons <- c("copy" = "copy", "CSV" = "csv", "Excel" = "excel", "PDF" = "pdf", "print" = "print", "colvis" = "colvis")
names(langSpecificTable$buttons) <- lang$adminMode$tables$dt$buttons$choices

createTableData <- function(symbol, pivotCol = NULL, createColNames = FALSE) {
  if (symbol %in% inputSymMultiDimChoices) {
    headersRaw <- inputSymHeaders[[symbol]]
    headersTmp <- names(headersRaw)
    noNumericCols <- sum(vapply(modelIn[[symbol]]$headers, function(hdr) identical(hdr$type, "numeric"), logical(1), USE.NAMES = FALSE))
  } else if (symbol %in% outputSymMultiDimChoices) {
    headersRaw <- outputSymHeaders[[symbol]]
    headersTmp <- headersRaw
    noNumericCols <- sum(vapply(modelOut[[symbol]]$headers, function(hdr) identical(hdr$type, "numeric"), logical(1), USE.NAMES = FALSE))
  } else {
    noNumericCols <- sum(vapply(modelIn[[symbol]]$headers, function(hdr) identical(hdr$type, "numeric"), logical(1), USE.NAMES = FALSE))
  }
  numericColValues <- c(
    1.123456789, 2.123456789, 3.123456789, 4.123456789,
    5.123456789, 6.123456789, 7.123456789, 8.123456789,
    9.123456789, 10.123456789
  )
  data <- data.frame(matrix(
    c(
      replicate(
        length(headersTmp) - noNumericCols,
        paste0(letters[1:10], " ")
      ),
      if (noNumericCols > 0L) replicate(noNumericCols, numericColValues)
    ),
    10
  ))
  if (noNumericCols > 0L) {
    data <- dplyr::mutate(data, dplyr::across(!!seq(length(data) - noNumericCols + 1L, length(data)), as.numeric))
  }
  isPivotTable <- FALSE
  if (length(pivotCol) && pivotCol != "_") {
    isPivotTable <- TRUE
    if (symbol %in% outputSymMultiDimChoices) {
      pivotIdx <- match(pivotCol, outputSymHeadersFull[[input$table_symbol]])[[1L]]
      return(list(
        data = data, headers = headersTmp, headersRaw = headersRaw,
        isPivotTable = isPivotTable
      ))
    }
    pivotIdx <- match(pivotCol, inputSymHeaders[[input$table_symbol]])[[1L]]
    if (is.na(pivotIdx)) {
      return(list(
        data = data, headers = headersTmp, headersRaw = headersRaw,
        isPivotTable = isPivotTable
      ))
    }
    data <- pivot_wider(data,
      names_from = !!pivotIdx,
      values_from = !!length(data)
    )
    attrTmp <- headersTmp[-c(pivotIdx, length(headersTmp))]
    attrTmp <- c(
      attrTmp,
      names(data)[seq(
        length(attrTmp) + 1L,
        length(data)
      )]
    )
    headersTmp <- attrTmp
  } else if (createColNames) {
    names(data) <- headersRaw
  }
  return(list(
    data = data, headers = headersTmp, headersRaw = headersRaw,
    isPivotTable = isPivotTable
  ))
}

observeEvent(input$table_type, {
  req(length(input$table_type) > 0L)

  rv$tableConfig$tableType <<- input$table_type

  removeUI(selector = "#table_wrapper div", multiple = TRUE)

  if (identical(input$table_type, "hot")) {
    insertUI(selector = "#table_wrapper", getHotOptions(), where = "beforeEnd")
    hideEl(session, "#preview-output-dt")
    hideEl(session, "#preview-output-tableWidget")
    showEl(session, "#preview-output-hot")
  } else if (identical(input$table_type, "dt")) {
    insertUI(selector = "#table_wrapper", getDtOptions(), where = "beforeEnd")
    hideEl(session, "#preview-output-hot")
    hideEl(session, "#preview-output-tableWidget")
    showEl(session, "#preview-output-dt")
  } else if (identical(input$table_type, "symbol")) {
    hideEl(session, "#preview-output-hot")
    hideEl(session, "#preview-output-dt")
    showEl(session, "#preview-output-tableWidget")
  }
})

output$tableLabelWrapper <- renderUI({
  if (length(rv$tableWidgetConfig$label) && !identical(trimws(rv$tableWidgetConfig$label), "")) {
    tagList(
      tags$div(
        id = "inputTableLabel",
        class = "readme-wrapper label-wrapper",
        markdownKatex(rv$tableWidgetConfig$label)
      ),
      tags$script("setTimeout(function(){Miro.parseKatex(document.getElementById('inputTableLabel'))},500)")
    )
  }
})

getHotOptions <- reactive({
  input$table_type
  tagList(
    tags$div(
      style = "max-width:400px;",
      numericInput("hot_height", lang$adminMode$tables$hot$height,
        min = 0L,
        value = if (length(configJSON$handsontable$height)) configJSON$handsontable$height else config$handsontable$height
      )
    ),
    tags$div(
      class = "shiny-input-container",
      checkboxInput_SIMPLE("hot_readonly", lang$adminMode$tables$hot$readonly,
        value = if (length(configJSON$handsontable$readonly)) {
          configJSON$handsontable$readonly
        } else {
          config$handsontable$readonly
        }
      )
    ),
    tags$div(
      style = "max-width:400px;",
      selectInput("hot_stretch", lang$adminMode$tables$hot$stretch,
        choices = langSpecificTable$stretch,
        selected = if (length(configJSON$handsontable$stretchH)) configJSON$handsontable$stretchH else config$handsontable$stretchH
      )
    ),
    conditionalPanel(
      condition = "input.hot_stretch !== 'all'",
      tags$div(
        style = "max-width:400px;",
        numericInput("hot_colwidth", lang$adminMode$tables$hot$colwidth,
          min = 0L,
          value = if (length(configJSON$handsontable$colWidths)) configJSON$handsontable$colWidths else 200L
        )
      )
    ),
    tags$div(
      class = "shiny-input-container",
      checkboxInput_SIMPLE("hot_context_enable", lang$adminMode$tables$hot$enable,
        value = if (length(configJSON$handsontable$contextMenu$enabled)) {
          configJSON$handsontable$contextMenu$enabled
        } else {
          config$handsontable$contextMenu$enabled
        }
      )
    ),
    tags$div(
      class = "shiny-input-container",
      style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;padding-left:40px;",
      conditionalPanel(
        condition = "input.hot_context_enable == true",
        checkboxInput_SIMPLE("hot_context_rowedit", lang$adminMode$tables$hot$contextRowedit,
          value = if (length(configJSON$handsontable$contextMenu$allowRowEdit)) {
            configJSON$handsontable$contextMenu$allowRowEdit
          } else {
            config$handsontable$contextMenu$allowRowEdit
          }
        ),
        checkboxInput_SIMPLE("hot_context_coledit", lang$adminMode$tables$hot$contextColedit,
          value = if (length(configJSON$handsontable$contextMenu$allowColEdit)) {
            configJSON$handsontable$contextMenu$allowColEdit
          } else {
            config$handsontable$contextMenu$allowColEdit
          }
        )
      )
    )
  )
})

getDtOptions <- reactive({
  input$table_type
  tagList(
    tags$div(
      style = "max-width:400px;",
      selectInput("dt_class", lang$adminMode$tables$dt$class$label,
        choices = langSpecificTable$class,
        selected = if (length(configJSON$datatable$class)) configJSON$datatable$class else config$datatable$class
      )
    ),
    tags$div(
      style = "max-width:400px;",
      selectInput("dt_filter", lang$adminMode$tables$dt$filter$label,
        choices = langSpecificTable$filter,
        selected = if (length(configJSON$datatable$filter)) configJSON$datatable$filter else config$datatable$filter
      )
    ),
    tags$div(
      style = "max-width:400px;",
      numericInput("dt_pagelength", lang$adminMode$tables$dt$pagelength$label,
        min = 1L,
        value = if (length(configJSON$datatable$options$pageLength)) {
          configJSON$datatable$options$pageLength
        } else {
          config$datatable$options$pageLength
        }
      )
    ),
    tags$div(
      class = "shiny-input-container",
      checkboxInput_SIMPLE("dt_rownames", lang$adminMode$tables$dt$rownames$label,
        value = if (length(configJSON$datatable$rownames)) {
          configJSON$datatable$rownames
        } else {
          config$datatable$rownames
        }
      )
    ),
    tags$div(
      style = "max-width:400px;",
      selectInput("dt_buttons",
        labelTooltip(
          label = lang$adminMode$tables$dt$buttons$select,
          tooltip = paste0(
            lang$adminMode$tables$dt$buttons$title, " - ",
            tolower(lang$adminMode$general$ui$tooltipDocs)
          ),
          href = "https://gams.com/miro/configuration_tables.html#export-buttons"
        ),
        choices = langSpecificTable$buttons,
        selected = configJSON$datatable$options$buttons,
        multiple = TRUE
      )
    )
  )
})

observeEvent(
  {
    input$table_symbol
    input$table_type
    rv$reset_table_input
  },
  {
    req(
      identical(input$table_type, "symbol"),
      length(input$table_symbol) > 0L,
      nchar(input$table_symbol) > 0L
    )

    currentTableSymbolName <<- input$table_symbol
    rv$tableWidgetConfig <- list()
    currentConfig <- NULL
    removeUI(selector = "#table_wrapper div", multiple = TRUE)
    isGamsTable <<- FALSE

    if (currentTableSymbolName %in% outputSymMultiDimChoices) {
      pivotCols <<- NULL
      if (length(outputSymHeaders[[currentTableSymbolName]]) > 2L) {
        numericHeaders <- vapply(modelOut[[currentTableSymbolName]]$headers,
          function(header) identical(header$type, "numeric"),
          logical(1L),
          USE.NAMES = FALSE
        )
        if (sum(numericHeaders) <= 1L) {
          pivotCols <<- setNames(
            names(modelOut[[currentTableSymbolName]]$headers)[!numericHeaders],
            outputSymHeaders[[currentTableSymbolName]][!numericHeaders]
          )
        }
      }
      if (currentTableSymbolName %in% names(configJSON$outputTables)) {
        currentConfig <- configJSON$outputTables[[currentTableSymbolName]]
        configuredTable <<- TRUE
      } else {
        configuredTable <<- FALSE
      }
      rv$tableWidgetConfig$pivotCols <<- checkLength(configuredTable, currentConfig$pivotCols, "_")
      rv$tableWidgetConfig$class <<- checkLength(configuredTable, currentConfig$class, "display")
      rv$tableWidgetConfig$filter <<- checkLength(configuredTable, currentConfig$filter, "bottom")
      rv$tableWidgetConfig$options$pageLength <<- checkLength(configuredTable, currentConfig$options$pageLength, 15L)
      rv$tableWidgetConfig$options$decimals <<- checkLength(configuredTable, currentConfig$options$decimals, 3L)
      rv$tableWidgetConfig$rownames <<- checkLength(configuredTable, currentConfig$rownames, FALSE)
      rv$tableWidgetConfig$options$buttons <<- checkLength(configuredTable, currentConfig$options$buttons, NULL)
      outputTableExtensions <<- Set$new(if (length(rv$tableWidgetConfig$options$buttons)) "Buttons")
      insertUI(selector = "#table_wrapper", getOutputTableOptions(), where = "beforeEnd")
      showEl(session, "#outputTable_preview")
    }
    if (length(currentConfig)) {
      enableEl(session, "#deleteTableWidget")
    } else {
      disableEl(session, "#deleteTableWidget")
    }
  }
)

getOutputTableOptions <- reactive({
  tagList(
    tags$div(
      class = "shiny-input-container option-wrapper", style = "max-width:400px;",
      tags$div(
        style = if (!length(pivotCols)) "display:none",
        selectInput("outputTable_pivotCols", lang$adminMode$widgets$table$pivotCols,
          choices = c(`_` = "_", pivotCols),
          selected = if (length(rv$tableWidgetConfig$pivotCols)) rv$tableWidgetConfig$pivotCols else "_"
        )
      ),
      selectInput("outputTable_class", lang$adminMode$tables$dt$class$label,
        choices = langSpecificTable$class,
        selected = rv$tableWidgetConfig$class
      ),
      selectInput("outputTable_filter", lang$adminMode$tables$dt$filter$label,
        choices = langSpecificTable$filter,
        selected = rv$tableWidgetConfig$filter
      ),
      numericInput("outputTable_pagelength", lang$adminMode$tables$dt$pagelength$label,
        min = 1L,
        value = rv$tableWidgetConfig$options$pageLength
      ),
      numericInput("outputTable_decimals", lang$adminMode$tables$dt$decimals$label,
        min = 0L,
        value = rv$tableWidgetConfig$options$decimals
      ),
      checkboxInput_SIMPLE("outputTable_rownames", lang$adminMode$tables$dt$rownames$label,
        value = rv$tableWidgetConfig$rownames
      ),
      selectInput("outputTable_buttons",
        labelTooltip(
          label = lang$adminMode$tables$dt$buttons$select,
          tooltip = paste0(
            lang$adminMode$tables$dt$buttons$title, " - ",
            tolower(lang$adminMode$general$ui$tooltipDocs)
          ),
          href = "https://gams.com/miro/configuration_tables.html#export-buttons"
        ),
        choices = langSpecificTable$buttons,
        selected = rv$tableWidgetConfig$options$buttons,
        multiple = TRUE
      ),
      checkboxInput_SIMPLE("outputTable_noGraph", lang$adminMode$tables$symbol$noGraph,
        value = if (tolower(currentTableSymbolName) %in% tolower(names(configJSON$dataRendering)) &&
          identical(configJSON$dataRendering[[currentTableSymbolName]]$outType, "datatable")) {
          TRUE
        } else {
          FALSE
        }
      )
    )
  )
})

observeEvent(input$outputTable_pivotCols, {
  rv$tableWidgetConfig$pivotCols <<- input$outputTable_pivotCols
})
observeEvent(input$outputTable_class, {
  rv$tableWidgetConfig$class <<- input$outputTable_class
})
observeEvent(input$outputTable_filter, {
  rv$tableWidgetConfig$filter <<- input$outputTable_filter
})
observeEvent(input$outputTable_pagelength, {
  if (is.na(input$outputTable_pagelength)) {
    rv$tableWidgetConfig$options$pageLength <<- NULL
    return()
  }
  rv$tableWidgetConfig$options$pageLength <<- input$outputTable_pagelength
})
observeEvent(input$outputTable_decimals, {
  if (is.na(input$outputTable_decimals)) {
    rv$tableWidgetConfig$options$decimals <<- NULL
    return()
  }
  rv$tableWidgetConfig$options$decimals <<- input$outputTable_decimals
})
observeEvent(input$outputTable_rownames, {
  rv$tableWidgetConfig$rownames <<- input$outputTable_rownames
})
observeEvent(input$outputTable_buttons, ignoreNULL = FALSE, {
  if (length(input$outputTable_buttons)) {
    rv$tableWidgetConfig$options$buttons <<- input$outputTable_buttons
    outputTableExtensions$push("Buttons")
    rv$tableWidgetConfig$options$dom <<- "Bfrtip"
  } else {
    outputTableExtensions$delete("Buttons")
    rv$tableWidgetConfig$options$dom <<- NULL
    rv$tableWidgetConfig$options$buttons <<- NULL
  }
  rv$tableWidgetConfig$extensions <<- outputTableExtensions$get()
})

output$dt_preview <- renderDT({
  req(input$table_symbol %in% names(inputSymHeaders))

  data <- createTableData(input$table_symbol, input$table_pivotCols)

  headersTmp <- unname(data$headers)
  data <- data$data

  dtOptions <- list(
    editable = !isTRUE(input$table_readonly),
    colnames = headersTmp
  )
  if (!is.null(configJSON$datatable)) {
    dtOptions <- modifyList(
      configJSON$datatable,
      dtOptions
    )
  }

  if (!isTRUE(currentTableSymbolName %in% names(configJSON$outputTables)) &&
    !identical(rv$tableWidgetConfig$tableType, "bigdata")) {
    hideEl(session, "#outputTable_preview")
    return()
  }
  # hideEl(session, "#hot_preview")
  return(renderDTable(data, dtOptions, render = FALSE))
})

output$outputTable_preview <- renderDT({
  req(input$table_symbol %in% outputSymMultiDimChoices)

  data <- createTableData(input$table_symbol, input$outputTable_pivotCols)
  headersTmp <- unname(data$headers)
  data <- as_tibble(data$data)
  showEl(session, "#outputTable_preview")
  dtOptions <- rv$tableWidgetConfig
  dtOptions$editable <- FALSE
  attr(data, "aliases") <- headersTmp
  if (identical(dtOptions$pivotCols, "_")) {
    dtOptions$pivotCols <- NULL
  } else {
    names(data) <- names(modelOut[[input$table_symbol]]$headers)
  }
  return(renderDTable(data, dtOptions, render = FALSE))
})

observeEvent(input$hot_height, {
  if (is.na(input$hot_height)) {
    configJSON$tableConfig$handsontable$height <<- NULL
  }
  if (!is.na(input$hot_height)) {
    rv$tableConfig$handsontable$height <<- input$hot_height
  } else {
    rv$tableConfig$handsontable$height <<- NULL
  }
})
observeEvent(input$hot_readonly, {
  rv$tableConfig$handsontable$readonly <<- input$hot_readonly
})
observeEvent(input$hot_stretch, {
  rv$tableConfig$handsontable$stretchH <<- input$hot_stretch
})
observeEvent(input$hot_colwidth, {
  if (!is.na(input$hot_colwidth) && input$hot_colwidth != 0) {
    rv$tableConfig$handsontable$colWidths <<- input$hot_colwidth
  } else {
    rv$tableConfig$handsontable$colWidths <<- 200L
  }
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
  if (nchar(input$dt_dom)) {
    rv$tableConfig$datatable$options$dom <<- input$dt_dom
  } else {
    rv$tableConfig$datatable$options$dom <<- NULL
  }
})
observeEvent(input$dt_pagelength, {
  if (is.na(input$dt_pagelength)) {
    configJSON$tableConfig$datatable$options$pageLength <<- NULL
  }
  if (!is.na(input$dt_pagelength)) {
    rv$tableConfig$datatable$options$pageLength <<- input$dt_pagelength
  } else {
    rv$tableConfig$datatable$options$pageLength <<- NULL
  }
})
observeEvent(input$dt_buttons, ignoreNULL = FALSE, {
  if (length(input$dt_buttons)) {
    rv$tableConfig$datatable$options$buttons <<- input$dt_buttons
    dtExtensions$push("Buttons")
    rv$tableConfig$datatable$options$dom <<- "Bfrtip"
  } else {
    dtExtensions$delete("Buttons")
    rv$tableConfig$datatable$options$dom <<- NULL
    rv$tableConfig$datatable$options$buttons <<- NULL
  }
  rv$tableConfig$datatable$extensions <<- dtExtensions$get()
})

observe(
  {
    req(rv$tableConfig$tableType)
    tryCatch(
      {
        if (isolate(rv$tableConfig$tableType) == "hot") {
          hotOptions <- rv$tableConfig$handsontable
          if (is.null(hotOptions)) {
            return()
          }
          output[["table_preview_hot"]] <- renderRHandsontable({
            ht <- rhandsontable(
              data = data, height = hotOptions$height,
              colHeaders = letters[1:4],
              search = hotOptions$search,
              readOnly = hotOptions$readonly, selectCallback = TRUE,
              digits = NA
            )
            ht <- hot_table(ht,
              contextMenu = hotOptions$contextMenu$enabled,
              highlightCol = config$handsontable$highlightCol,
              highlightRow = config$handsontable$highlightRow,
              rowHeaderWidth = hotOptions$rowHeaderWidth,
              stretchH = hotOptions$stretchH,
              overflow = hotOptions$overflow
            )
            if (isTRUE(hotOptions$contextMenu$enabled)) {
              ht <- hot_context_menu(ht,
                allowRowEdit = hotOptions$contextMenu$allowRowEdit,
                allowColEdit = hotOptions$contextMenu$allowColEdit,
                allowReadOnly = config$handsontable$contextMenu$allowReadOnly
              )
            }
            ht <- hot_cols(ht,
              columnSorting = config$handsontable$columnSorting,
              manualColumnMove = hotOptions$manualColumnMove,
              manualColumnResize = config$handsontable$manualColumnResize,
              colWidths = hotOptions$colWidths,
              fixedColumnsLeft = hotOptions$fixedColumnsLeft
            )
          })
        } else if (isolate(rv$tableConfig$tableType) == "dt") {
          dtOptions <- rv$tableConfig$datatable
          callModule(renderData, "table_preview_dt",
            type = "datatable",
            data = data, dtOptions = dtOptions,
            roundPrecision = 2, modelDir = modelDir
          )
        } else if (isolate(rv$tableConfig$tableType) == "piv") {
          return()
        }
        hideEl(session, "#preview-error")
      },
      error = function(e) {
        hideEl(session, "#preview-content-hot")
        hideEl(session, "#preview-content-dt")
      }
    )
  },
  priority = -1000
)

#  =====================================================================
#          SAVE JSON (global settings are saved automatically)
#  =====================================================================
observeEvent(rv$tableConfig, {
  req(length(rv$tableConfig$tableType))
  switch(rv$tableConfig$tableType,
    "hot" = {
      configJSON$handsontable <<- rv$tableConfig$handsontable
      if (identical(configJSON$handsontable$colWidths, "[]")) {
        configJSON$handsontable$colWidths <<- NULL
      }
    },
    "dt" = {
      configJSON$datatable <<- rv$tableConfig$datatable
    },
    "piv" = {
      configJSON$pivottable <<- rv$tableConfig$pivottable
    },
    {
      return()
    }
  )
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
})

observeEvent(input$saveTableWidget, {
  req(length(currentTableSymbolName) > 0L, nchar(currentTableSymbolName) > 0L)
  if (isTRUE(input$outputTable_noGraph) && tolower(currentTableSymbolName) %in% tolower(names(configJSON$dataRendering)) &&
    !identical(configJSON$dataRendering[[currentTableSymbolName]]$outType, "datatable")) {
    showModal(modalDialog(
      title = lang$adminMode$tables$symbol$saveJson$warnTitle, sprintf(lang$adminMode$tables$symbol$saveJson$warnContent, currentTableSymbolName),
      footer = tagList(
        modalButton(lang$adminMode$tables$symbol$saveJson$cancel),
        actionButton("saveTableConfirm", lang$adminMode$tables$symbol$saveJson$overwrite)
      )
    ))
    return()
  }
  rv$saveTableConfirm <- rv$saveTableConfirm + 1L
})
observeEvent(virtualActionButton(input$saveTableConfirm, rv$saveTableConfirm), {
  req(length(currentTableSymbolName) > 0L, nchar(currentTableSymbolName) > 0L)

  if (currentTableSymbolName %in% outputSymMultiDimChoices) {
    configJSON$outputTables[[currentTableSymbolName]] <<- rv$tableWidgetConfig
    if (is.null(configJSON$outputTables[[currentTableSymbolName]]$pivotCols) ||
      identical(configJSON$outputTables[[currentTableSymbolName]]$pivotCols, "_")) {
      configJSON$outputTables[[currentTableSymbolName]]$pivotCols <<- NULL
    }
    if (isTRUE(input$outputTable_noGraph)) {
      configJSON$dataRendering[[currentTableSymbolName]] <<- configJSON$outputTables[[currentTableSymbolName]]
      configJSON$dataRendering[[currentTableSymbolName]]$outType <<- "datatable"
      if (identical(tolower(input$table_symbol), tolower(activeSymbol$name))) {
        newChartTool <<- "datatable"
        updateSelectInput(session, "chart_tool", selected = newChartTool)
        tableSymbol <<- TRUE
        rv$refreshOptions <- rv$refreshOptions + 1L
        disableEl(session, "#saveGraph")
        showEl(session, "#deleteGraph")
      }
    } else if (identical(configJSON$dataRendering[[currentTableSymbolName]]$outType, "datatable")) {
      configJSON$dataRendering[[currentTableSymbolName]] <<- NULL
      if (identical(tolower(input$table_symbol), tolower(activeSymbol$name))) {
        newChartTool <<- "pie"
        updateSelectInput(session, "chart_tool", selected = newChartTool)
        tableSymbol <<- FALSE
        rv$refreshOptions <- rv$refreshOptions + 1L
        enableEl(session, "#saveGraph")
        showEl(session, "#deleteGraph")
      }
    }
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

  if (noTableSymbols) {
    hideEl(session, "#noTableSymbolMsg")
    noTableSymbols <<- FALSE
  }
  removeModal()
  showHideEl(session, "#tableWidgetUpdateSuccess", 4000L)
  enableEl(session, "#deleteTableWidget")
})
observeEvent(input$deleteTableWidget, {
  req(length(input$table_symbol) > 0L, nchar(input$table_symbol) > 0L)

  showModal(modalDialog(
    title = lang$adminMode$tables$symbol$removeDialog$title, lang$adminMode$tables$symbol$removeDialog$message,
    footer = tagList(
      modalButton(lang$adminMode$tables$symbol$removeDialog$cancel),
      actionButton("deleteTableWidgetConfirm", lang$adminMode$tables$symbol$removeDialog$confirm)
    )
  ))
})
observeEvent(input$deleteTableWidgetConfirm, {
  req(length(currentTableSymbolName) > 0L, nchar(currentTableSymbolName) > 0L)

  if (currentTableSymbolName %in% inputSymMultiDimChoices) {
    configJSON$inputWidgets[[currentTableSymbolName]] <<- NULL
  } else if (currentTableSymbolName %in% outputSymMultiDimChoices) {
    configJSON$outputTables[[currentTableSymbolName]] <<- NULL
    if (!length(configJSON$outputTables)) {
      configJSON$outputTables <<- NULL
    }
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  disableEl(session, "#deleteTableWidget")
  removeModal()
  showHideEl(session, "#tableWidgetUpdateSuccess", 4000L)
  hideEl(session, "#noTableSymbolMsg")
  if (!length(tableSymbols)) {
    if (!noTableSymbols) {
      showEl(session, "#noTableSymbolMsg")
      noTableSymbols <<- TRUE
    }
    currentTableSymbolName <<- character(0L)
  } else {
    rv$reset_table_input <- rv$reset_table_input + 1L
  }
})
