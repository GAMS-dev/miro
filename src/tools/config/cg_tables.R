tableSymbols <- setNames(
  list(
    c(inputSymMultiDimChoices),
    c(outputSymMultiDimChoices)
  ),
  c(
    lang$adminMode$tables$ui$inputSymbols,
    lang$adminMode$tables$ui$outputSymbols
  )
)
inputPivotRendererEnv <- new.env(parent = emptyenv())
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
    tags$div(
      id = "inputTableLabel",
      class = "readme-wrapper label-wrapper",
      markdown(rv$tableWidgetConfig$label)
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
      checkboxInput_MIRO("hot_readonly", lang$adminMode$tables$hot$readonly,
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
      checkboxInput_MIRO("hot_resize", lang$adminMode$tables$hot$resize,
        value = if (length(configJSON$handsontable$manualColumnResize)) {
          configJSON$handsontable$manualColumnResize
        } else {
          config$handsontable$manualColumnResize
        }
      )
    ),
    tags$div(
      class = "shiny-input-container",
      checkboxInput_MIRO("hot_context_enable", lang$adminMode$tables$hot$enable,
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
        checkboxInput_MIRO("hot_context_rowedit", lang$adminMode$tables$hot$contextRowedit,
          value = if (length(configJSON$handsontable$contextMenu$allowRowEdit)) {
            configJSON$handsontable$contextMenu$allowRowEdit
          } else {
            config$handsontable$contextMenu$allowRowEdit
          }
        ),
        checkboxInput_MIRO("hot_context_coledit", lang$adminMode$tables$hot$contextColedit,
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
      checkboxInput_MIRO("dt_rownames", lang$adminMode$tables$dt$rownames$label,
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
          href = "https://gams.com/miro/customize.html#export-buttons"
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

    if (currentTableSymbolName %in% inputSymMultiDimChoices) {
      if (currentTableSymbolName %in% names(configJSON$inputWidgets)) {
        currentConfig <- configJSON$inputWidgets[[currentTableSymbolName]]
        configuredTable <<- TRUE
      } else {
        configuredTable <<- FALSE
      }
      pivotCols <<- NULL
      if (length(inputSymHeaders[[input$table_symbol]]) > 2L) {
        numericHeaders <- vapply(modelIn[[input$table_symbol]]$headers,
          function(header) identical(header$type, "numeric"),
          logical(1L),
          USE.NAMES = FALSE
        )
        if (sum(numericHeaders) <= 1L) {
          pivotCols <<- inputSymHeaders[[input$table_symbol]][!numericHeaders]
        } else {
          isGamsTable <<- TRUE
        }
      }

      # tableAlias <- ''
      # if(length(currentConfig$alias) && nchar(currentConfig$alias)){
      #   tableAlias <- currentConfig$alias
      # }else{
      #   #TODO: check
      #   tablesymbolID <- match(currentTableSymbolName, tableSymbols[[names(tableSymbols)[[1]]]])
      #
      #   if(!is.na(tablesymbolID)){
      #     tableAlias <- names(inputSymMultiDim)[[tablesymbolID]]
      #   }
      # }

      if (identical(currentConfig$tableType, "pivot")) {
        rv$tableWidgetConfig <- list(
          widgetType = "table",
          tableType = "pivot",
          label = currentConfig$label,
          options = checkLength(configuredTable, currentConfig[["options"]], list())
        )
        rv$tableWidgetConfig$options$input <- TRUE
      } else if (identical(currentConfig$tableType, "bigdata") || isTRUE(currentConfig$bigData)) {
        rv$tableWidgetConfig <- list(
          widgetType = "table",
          tableType = "bigdata",
          label = currentConfig$label,
          readonly = checkLength(configuredTable, currentConfig[["readonly"]], FALSE),
          pivotCols = checkLength(configuredTable, currentConfig[["pivotCols"]], "_")
        )
      } else {
        rv$tableWidgetConfig <- list(
          widgetType = "table",
          tableType = "default",
          label = currentConfig$label,
          readonly = checkLength(configuredTable, currentConfig[["readonly"]], FALSE),
          readonlyCols = checkLength(configuredTable, currentConfig[["readonlyCols"]], NULL),
          colWidths = if (configuredTable && !is.null(currentConfig[["colWidths"]]) && length(currentConfig[["colWidths"]]) > 1) {
            "custom"
          } else {
            checkLength(configuredTable, currentConfig[["colWidths"]], NULL)
          },
          hideIndexCol = checkLength(configuredTable, currentConfig$hideIndexCol, FALSE),
          heatmap = checkLength(configuredTable, currentConfig$heatmap, FALSE),
          pivotCols = checkLength(configuredTable, currentConfig$pivotCols, "_"),
          colFormat = checkLength(configuredTable, currentConfig$colFormat, NULL),
          fixedColumnsLeft = checkLength(configuredTable, currentConfig$fixedColumnsLeft, NULL)
        )
      }
      currentColFormatConfig <<- rv$tableWidgetConfig$colFormat
      insertUI(selector = "#table_wrapper", getSymbolHotOptions(), where = "beforeEnd")
      hideEl(session, "#outputTable_preview")
      if (identical(rv$tableWidgetConfig$tableType, input$inputTable_type)) {
        # need to trigger observer as it is lazy..
        refreshTableType(refreshSameSymbol = identical(currentTableSymbolName, input$table_symbol))
      }
    } else if (currentTableSymbolName %in% outputSymMultiDimChoices) {
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
      # hideEl(session, "#hot_preview")
      hideEl(session, "#inputTable_pivot-data")
      hideEl(session, "#pivotColsRestriction")
      showEl(session, "#outputTable_preview")
    }
    if (length(currentConfig)) {
      enableEl(session, "#deleteTableWidget")
    } else {
      disableEl(session, "#deleteTableWidget")
    }
  }
)

getSymbolHotOptions <- function() {
  tagList(
    # tags$div(class="option-wrapper",
    #          textInput("table_alias", lang$adminMode$widgets$ui$alias, value = rv$tableWidgetConfig$alias)),
    tags$div(
      class = "option-wrapper",
      textAreaInput("table_label", lang$adminMode$widgets$table$label,
        value = rv$tableWidgetConfig$label
      ),
      selectInput("inputTable_type", lang$adminMode$widgets$table$type,
        choices = setNames(
          c("default", "bigdata", "pivot"),
          lang$adminMode$widgets$table$typeChoices
        ),
        selected = if (length(rv$tableWidgetConfig$tableType)) {
          rv$tableWidgetConfig$tableType
        } else if (isTRUE(rv$tableWidgetConfig$bigData)) {
          "bigdata"
        } else {
          "default"
        }
      )
    ),
    conditionalPanel(
      condition = "input.inputTable_type==='pivot'",
      getMIROPivotOptions(rv$tableWidgetConfig$options, prefix = "inputpivot_"),
      tags$div(
        class = "config-message",
        style = "display:block;",
        lang$adminMode$graphs$miroPivotOptions$infoMsgDummyData
      )
    ),
    conditionalPanel(
      condition = "input.inputTable_type!=='pivot'",
      checkboxInput_MIRO("table_readonly", lang$adminMode$widgets$table$readonly, value = rv$tableWidgetConfig$readonly),
      tags$div(
        class = "option-wrapper",
        style = if (!length(pivotCols)) "display:none",
        selectInput("table_pivotCols", lang$adminMode$widgets$table$pivotCols,
          choices = c(`_` = "_", pivotCols),
          selected = if (length(rv$tableWidgetConfig$pivotCols)) rv$tableWidgetConfig$pivotCols else "_"
        )
      )
    ),
    conditionalPanel(
      condition = paste0("input.inputTable_type==='default' && ((input.table_pivotCols!=null && input.table_pivotCols!=='_')
                                        || ", tolower(isGamsTable), ")"),
      tags$div(
        class = "option-wrapper",
        checkboxInput_MIRO("table_fixedColumnsLeft", lang$adminMode$widgets$table$fixedColumnsLeft,
          value = if (length(rv$tableWidgetConfig$fixedColumnsLeft)) TRUE else FALSE
        )
      )
    ),
    conditionalPanel(
      condition = paste0("input.inputTable_type==='default' && ((input.table_pivotCols==null || input.table_pivotCols==='_')
                                        ||", tolower(isGamsTable), ")"),
      tags$div(
        class = "option-wrapper",
        selectInput("table_readonlyCols", lang$adminMode$widgets$table$readonlyCols,
          choices = inputSymHeaders[[input$table_symbol]],
          selected = rv$tableWidgetConfig$readonlyCols, multiple = TRUE
        )
      ),
      tags$div(
        class = "option-wrapper",
        lapply(names(modelIn[[input$table_symbol]]$headers), function(headerName) {
          if (identical(modelIn[[input$table_symbol]]$headers[[headerName]]$type, "numeric")) {
            defaultVal <- "2"
            if (length(rv$tableWidgetConfig$colFormat) &&
              headerName %in% names(rv$tableWidgetConfig$colFormat)) {
              if (identical(
                rv$tableWidgetConfig$colFormat[[headerName]]$format,
                "0,0a"
              )) {
                defaultVal <- "0"
              } else {
                defaultVal <- as.character(nchar(strsplit(rv$tableWidgetConfig$colFormat[[headerName]]$format, ".",
                  fixed = TRUE
                )[[1]][2]))
                if (is.na(defaultVal)) {
                  defaultVal <- "2"
                }
              }
            }
            tags$div(
              class = "form-group shiny-input-container",
              tags$label(class = "control-label", sprintf(
                lang$adminMode$widgets$table$colDecimals,
                modelIn[[input$table_symbol]]$headers[[headerName]]$alias
              )),
              tags$input(
                type = "number", class = "form-control miro-dynamic-input-id", `data-binding-id` = "table_colDecimals",
                `data-input-id` = headerName, value = defaultVal, min = "0"
              )
            )
          }
        })
      )
    ),
    conditionalPanel(
      condition = "input.inputTable_type==='default'",
      tags$div(
        class = "option-wrapper",
        numericInput("table_colWidths",
          tags$div(
            lang$adminMode$widgets$table$colWidths,
            tags$a("",
              title = lang$adminMode$widgets$table$colWidthsTooltip, class = "info-wrapper",
              href = "https://gams.com/miro/customize.html#table-colwidths",
              tags$span(
                class = "fas fa-info-circle", class = "info-icon",
                role = "presentation",
                `aria-label` = "More information"
              ), target = "_blank"
            )
          ),
          value = if (!identical(rv$tableWidgetConfig$colWidths, "custom")) {
            rv$tableWidgetConfig$colWidths
          } else {
            NULL
          }, min = 0, step = 1
        ),
        tags$div(
          id = "customColWidths", class = "config-message", style = if (identical(rv$tableWidgetConfig$colWidths, "custom")) "display:block;",
          lang$adminMode$widgets$table$customColWidths
        )
      ),
      tags$div(
        class = "option-wrapper",
        checkboxInput_MIRO("table_hideIndexCol",
          lang$adminMode$widgets$table$hideIndexCol,
          value = rv$tableWidgetConfig$hideIndexCol
        ),
        checkboxInput_MIRO("table_heatmap",
          lang$adminMode$widgets$table$heatmap,
          value = rv$tableWidgetConfig$heatmap
        )
      ),
      tags$div(
        tags$h4(
          style = "font-weight: 600;",
          lang$adminMode$widgets$table$dropdownColsTitle
        ),
        tags$div(lang$adminMode$widgets$table$dropdownColsDesc, tags$a("https://gams.com/miro/customize.html#table-dropdown",
          title = lang$adminMode$general$ui$tooltipDocs,
          href = "https://gams.com/miro/customize.html#table-dropdown", target = "_blank"
        )),
      )
    )
  )
}

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
      checkboxInput_MIRO("outputTable_rownames", lang$adminMode$tables$dt$rownames$label,
        value = rv$tableWidgetConfig$rownames
      ),
      selectInput("outputTable_buttons",
        labelTooltip(
          label = lang$adminMode$tables$dt$buttons$select,
          tooltip = paste0(
            lang$adminMode$tables$dt$buttons$title, " - ",
            tolower(lang$adminMode$general$ui$tooltipDocs)
          ),
          href = "https://gams.com/miro/customize.html#export-buttons"
        ),
        choices = langSpecificTable$buttons,
        selected = rv$tableWidgetConfig$options$buttons,
        multiple = TRUE
      ),
      checkboxInput_MIRO("outputTable_noGraph", lang$adminMode$tables$symbol$noGraph,
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

createTableData <- function(symbol, pivotCol = NULL, createColNames = FALSE) {
  if (symbol %in% inputSymMultiDimChoices) {
    headersRaw <- inputSymHeaders[[symbol]]
    headersTmp <- names(headersRaw)
  }
  if (symbol %in% outputSymMultiDimChoices) {
    headersRaw <- outputSymHeaders[[symbol]]
    headersTmp <- headersRaw
  }
  noNumericCols <- sum(vapply(modelIn[[symbol]]$headers, function(hdr) identical(hdr$type, "numeric"), logical(1), USE.NAMES = FALSE))
  numericColValues <- c(
    1.123456789, 2.123456789, 3.123456789, 4.123456789,
    5.123456789, 6.123456789, 7.123456789, 8.123456789,
    9.123456789, 10.123456789
  )
  data <- data.frame(matrix(
    c(
      replicate(
        length(headersTmp) - noNumericCols,
        letters[1:10]
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
output$hot_preview <- renderRHandsontable({
  req(input$table_symbol %in% names(inputSymHeaders))
  if (!identical(rv$tableWidgetConfig$tableType, "default")) {
    return()
  }
  data <- createTableData(input$table_symbol, rv$tableWidgetConfig$pivotCols)

  headersTmp <- data$headersRaw
  headersUnnamed <- unname(headersTmp)
  colHeaders <- data$headers
  pivotTable <- data$isPivotTable
  data <- data$data
  colsReadonly <- match(rv$tableWidgetConfig$readonlyCols, headersUnnamed)
  colsReadonly <- colsReadonly[!is.na(colsReadonly)]
  colWidths <- if (!is.null(rv$tableWidgetConfig$colWidths) && !identical(rv$tableWidgetConfig$colWidths, "custom")) {
    rv$tableWidgetConfig$colWidths
  } else {
    200
  }
  fixedColumnsLeft <- isolate(rv$tableWidgetConfig$fixedColumnsLeft)
  if (is.null(fixedColumnsLeft)) fixedColumnsLeft <- 0

  readOnlyTable <- identical(input$table_readonly, TRUE)
  if (any(duplicated(colHeaders))) {
    if (readOnlyTable) {
      stop("readOnly is currently not supported for identical column headers. Please make sure that the column headers are unique by changing the column aliases in the General section.", call. = FALSE)
    }
    readOnlyTable <- NULL
  }
  ht <- rhandsontable(
    data = data,
    rowHeaders = if (isTRUE(input$table_hideIndexCol)) NULL else rownames(data),
    colHeaders = colHeaders,
    readOnly = readOnlyTable,
    digits = NA,
    naAsNull = pivotTable
  )

  if (!pivotTable) {
    for (colName in names(rv$tableWidgetConfig$colFormat)) {
      ht <- hot_col(ht, match(colName, headersUnnamed),
        format = rv$tableWidgetConfig$colFormat[[colName]]$format
      )
    }
  }
  if (!pivotTable && length(colsReadonly)) {
    ht <- hot_col(ht, colsReadonly, readOnly = TRUE)
  }
  ht <- hot_cols(ht,
    fixedColumnsLeft = fixedColumnsLeft,
    colWidths = colWidths
  )
  if (isTRUE(input$table_heatmap)) {
    return(hot_heatmap(ht))
  } else {
    return(ht)
  }
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

  data <- createTableData(input$table_symbol, "_")

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
observeEvent(input$hot_resize, {
  rv$tableConfig$handsontable$manualColumnResize <<- input$hot_resize
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
              manualColumnResize = hotOptions$manualColumnResize,
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


# observeEvent(input$table_alias, {
#   if(length(input$table_alias))
#     rv$tableWidgetConfig$alias <<- input$table_alias
#   else
#     rv$tableWidgetConfig$alias <<- NULL
# })
refreshTableType <- function(refreshSameSymbol = FALSE) {
  labelTmp <- rv$tableWidgetConfig$label
  if (identical(input$inputTable_type, "bigdata")) {
    rv$tableWidgetConfig$tableType <- "bigdata"
    hideEl(session, "#pivotColsRestriction")
    hideEl(session, "#inputTable_pivot-data")
    if (refreshSameSymbol) {
      rv$tableWidgetConfig <<- list(
        widgetType   = "table",
        tableType    = "bigdata",
        readonly     = input$table_readonly,
        pivotCols    = input$table_pivotCols
      )
    } else {
      rv$tableWidgetConfig <- list(
        widgetType = "table",
        tableType = "bigdata",
        readonly = checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]][["readonly"]], FALSE),
        pivotCols = checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]][["pivotCols"]], "_")
      )
    }
    if (length(labelTmp)) {
      rv$tableWidgetConfig$label <- labelTmp
    }
  } else if (identical(input$inputTable_type, "pivot")) {
    rv$tableWidgetConfig$tableType <- "pivot"
    hideEl(session, "#pivotColsRestriction")
    showEl(session, "#inputTable_pivot-data")
    for (el in ls(envir = inputPivotRendererEnv)) {
      if ("Observer" %in% class(inputPivotRendererEnv[[el]])) {
        inputPivotRendererEnv[[el]]$destroy()
      }
    }
    pivotOptions <- list()

    if (currentTableSymbolName %in% inputSymMultiDimChoices &&
      currentTableSymbolName %in% names(configJSON$inputWidgets) &&
      length(configJSON$inputWidgets[[currentTableSymbolName]][["options"]])) {
      pivotOptions <- configJSON$inputWidgets[[currentTableSymbolName]][["options"]]
    }
    pivotOptions$input <- TRUE
    pivotOptions$enableHideEmptyCols <- TRUE
    pivotOptions$emptyUEL <- rv$tableWidgetConfig$options$emptyUEL

    metadata <- list(
      headers = modelIn[[currentTableSymbolName]]$headers,
      symtype = modelIn[[currentTableSymbolName]]$symtype,
      symname = currentTableSymbolName
    )
    aggregationFunctions <- if (identical(metadata$symtype, "set")) {
      setNames(
        c("count", "min"),
        c(
          lang$renderers$miroPivot$aggregationFunctions$count,
          lang$renderers$miroPivot$aggregationFunctions$min
        )
      )
    } else {
      setNames(
        c("sum", "count", "mean", "median", "min", "max"),
        c(
          lang$renderers$miroPivot$aggregationFunctions$sum,
          lang$renderers$miroPivot$aggregationFunctions$count,
          lang$renderers$miroPivot$aggregationFunctions$mean,
          lang$renderers$miroPivot$aggregationFunctions$median,
          lang$renderers$miroPivot$aggregationFunctions$min,
          lang$renderers$miroPivot$aggregationFunctions$max
        )
      )
    }
    selectedAggregationFuction <- pivotOptions[["aggregationFunction"]]
    if (!length(selectedAggregationFuction) ||
      !selectedAggregationFuction %in% aggregationFunctions) {
      selectedAggregationFuction <- aggregationFunctions[1]
    }
    updateSelectInput(session, "inputTable_pivot-miroPivot-aggregationFunction",
      choices = aggregationFunctions,
      selected = selectedAggregationFuction
    )
    callModule(renderData, "inputTable_pivot",
      type = "miropivot",
      data = createTableData(currentTableSymbolName, createColNames = TRUE)$data, rendererEnv = inputPivotRendererEnv,
      customOptions = c(
        list(
          "_metadata_" = metadata,
          resetOnInit = TRUE
        ),
        pivotOptions
      ),
      roundPrecision = 2, modelDir = modelDir
    )
  } else {
    rv$tableWidgetConfig$tableType <- "default"
    hideEl(session, "#inputTable_pivot-data")
    if (refreshSameSymbol) {
      rv$tableWidgetConfig <<- list(
        widgetType = "table",
        tableType = "default",
        readonly = input$table_readonly,
        pivotCols = input$table_pivotCols,
        readonlyCols = input$table_readonlyCols,
        colWidths = if (!is.na(input$table_colWidths) && input$table_colWidths != 0) {
          input$table_colWidths
        } else {
          NULL
        },
        hideIndexCol = input$table_hideIndexCol,
        heatmap = input$table_heatmap
      )
      numericColsTmp <- sum(vapply(modelIn[[input$table_symbol]]$headers,
        function(header) identical(header$type, "numeric"),
        logical(1L),
        USE.NAMES = FALSE
      ))
      if (isTRUE(input$table_fixedColumnsLeft)) {
        if (isGamsTable) {
          fixedColumnsLeftTmp <- length(inputSymHeaders[[input$table_symbol]]) - numericColsTmp
        } else {
          fixedColumnsLeftTmp <- length(inputSymHeaders[[input$table_symbol]]) - numericColsTmp - 1L
        }
        rv$tableWidgetConfig$fixedColumnsLeft <<- fixedColumnsLeftTmp
      }
      if (length(currentColFormatConfig)) {
        rv$tableWidgetConfig$colFormat <- currentColFormatConfig
      }
    } else {
      rv$tableWidgetConfig <- list(
        widgetType = "table",
        tableType = "default",
        readonly = checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]][["readonly"]], FALSE),
        readonlyCols = checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]][["readonlyCols"]], NULL),
        colWidths = if (configuredTable && length(configJSON$inputWidgets[[currentTableSymbolName]][["colWidths"]]) &&
          length(configJSON$inputWidgets[[currentTableSymbolName]][["colWidths"]]) > 1) {
          "custom"
        } else {
          checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]][["colWidths"]], NULL)
        },
        hideIndexCol = checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]]$hideIndexCol, FALSE),
        heatmap = checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]]$heatmap, FALSE),
        pivotCols = checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]]$pivotCols, "_")
      )
      rv$tableWidgetConfig$fixedColumnsLeft <- checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]]$fixedColumnsLeft, NULL)
      rv$tableWidgetConfig$colFormat <- checkLength(configuredTable, configJSON$inputWidgets[[currentTableSymbolName]]$colFormat, NULL)
      currentColFormatConfig <<- NULL
    }
    if (length(labelTmp)) {
      rv$tableWidgetConfig$label <- labelTmp
    }
    if (!identical(rv$tableWidgetConfig$pivotCols, "_") &&
      (isTRUE(rv$tableWidgetConfig$readonly) || isTRUE(rv$tableWidgetConfig$heatmap))) {
      showEl(session, "#pivotColsRestriction")
    } else {
      hideEl(session, "#pivotColsRestriction")
    }
  }
}
observeEvent(input$inputTable_type, {
  refreshTableType(refreshSameSymbol = TRUE)
})
observeEvent(input$table_hideIndexCol, {
  rv$tableWidgetConfig$hideIndexCol <<- input$table_hideIndexCol
})
observeEvent(input$table_readonly, {
  rv$tableWidgetConfig$readonly <<- input$table_readonly
})
observeEvent(input$table_label, {
  if (nchar(input$table_label)) {
    rv$tableWidgetConfig$label <<- input$table_label
  } else {
    rv$tableWidgetConfig$label <<- NULL
  }
})
observeEvent(input$table_readonlyCols, ignoreNULL = FALSE, {
  if (!length(input$table_readonlyCols)) {
    configJSON$tableWidgetConfig$readonlyCols <<- NULL
    return()
  }
  rv$tableWidgetConfig$readonlyCols <<- input$table_readonlyCols
})
observeEvent(input$table_colWidths, ignoreNULL = FALSE, {
  if (!identical(rv$tableWidgetConfig$colWidths, "custom") &&
    (is.null(input$table_colWidths) || is.na(input$table_colWidths) || input$table_colWidths == 0)) {
    configJSON$tableWidgetConfig$colWidths <<- NULL
    rv$tableWidgetConfig$colWidths <<- NULL
    return()
  }
  rv$tableWidgetConfig$colWidths <<- input$table_colWidths
  hideEl(session, "#customColWidths")
})
observeEvent(input$table_pivotCols, {
  rv$tableWidgetConfig$pivotCols <<- input$table_pivotCols
  if (!identical(input$table_pivotCols, "_")) {
    rv$tableWidgetConfig$readonlyCols <<- NULL
    numericColsTmp <- sum(vapply(modelIn[[input$table_symbol]]$headers,
      function(header) identical(header$type, "numeric"),
      logical(1L),
      USE.NAMES = FALSE
    ))
    if (isGamsTable) {
      fixedColumnsLeftTmp <- length(inputSymHeaders[[input$table_symbol]]) - numericColsTmp
    } else {
      fixedColumnsLeftTmp <- length(inputSymHeaders[[input$table_symbol]]) - numericColsTmp - 1L
    }
    rv$tableWidgetConfig$fixedColumnsLeft <<- fixedColumnsLeftTmp
  } else {
    rv$tableWidgetConfig$readonlyCols <<- input$table_readonlyCols
    rv$tableWidgetConfig$fixedColumnsLeft <<- NULL
  }
})
observeEvent(input$table_fixedColumnsLeft, {
  if (isFALSE(input$table_fixedColumnsLeft)) {
    rv$tableWidgetConfig$fixedColumnsLeft <<- NULL
    return()
  }
  numericColsTmp <- sum(vapply(modelIn[[input$table_symbol]]$headers,
    function(header) identical(header$type, "numeric"),
    logical(1L),
    USE.NAMES = FALSE
  ))
  if (isGamsTable) {
    fixedColumnsLeftTmp <- length(inputSymHeaders[[input$table_symbol]]) - numericColsTmp
  } else {
    fixedColumnsLeftTmp <- length(inputSymHeaders[[input$table_symbol]]) - numericColsTmp - 1L
  }
  rv$tableWidgetConfig$fixedColumnsLeft <<- fixedColumnsLeftTmp
})
observeEvent(input$table_heatmap, {
  rv$tableWidgetConfig$heatmap <<- input$table_heatmap
})
observeEvent(input$table_colDecimals, {
  noDecimals <- suppressWarnings(as.integer(input$table_colDecimals$val))
  if (is.na(noDecimals)) {
    noDecimals <- 2L
  }
  if (identical(noDecimals, 2L)) {
    if (length(rv$tableWidgetConfig$colFormat)) {
      rv$tableWidgetConfig$colFormat[[input$table_colDecimals$id]] <<- NULL
    }
    if (!length(rv$tableWidgetConfig$colFormat)) {
      rv$tableWidgetConfig$colFormat <- NULL
    }
  } else {
    if (!length(rv$tableWidgetConfig$colFormat)) {
      rv$tableWidgetConfig$colFormat <<- list()
    }
    rv$tableWidgetConfig$colFormat[[input$table_colDecimals$id]] <- list(format = if (noDecimals > 0) paste0("0,0.", strrep("0", noDecimals)) else "0,0a")
  }
  currentColFormatConfig <<- rv$tableWidgetConfig$colFormat
})
observeEvent(c(input$table_pivotCols, input$table_readonly, input$table_heatmap), {
  if (!identical(input$table_pivotCols, "_") &&
    (isTRUE(input$table_readonly) || isTRUE(input$table_heatmap))) {
    showEl(session, "#pivotColsRestriction")
  } else {
    hideEl(session, "#pivotColsRestriction")
  }
})


validateTableConfig <- function(configJSON) {
  if (currentTableSymbolName %in% inputSymMultiDimChoices) {
    # if(!length(configJSON$alias) || identical(nchar(trimws(configJSON$alias)), 0L)){
    #   return(lang$adminMode$widgets$validate[["val1"]])
    # }
    if (identical(grepl("\\s", currentTableSymbolName), TRUE)) {
      return(lang$adminMode$widgets$validate$val39)
    }
    if (identical(configJSON$tableType, "pivot") && sum(vapply(modelIn[[currentTableSymbolName]]$headers, function(header) {
      return(identical(header$type, "numeric"))
    }, logical(1L))) > 1L) {
      return(sprintf(lang$adminMode$widgets$validate$val59, currentTableSymbolName))
    }
    if (any(!configJSON$readonlyCols %in% inputSymHeaders[[currentTableSymbolName]])) {
      return(lang$adminMode$widgets$validate$val34)
    }
  } else if (currentTableSymbolName %in% outputSymMultiDimChoices) {

  }
  return("")
}
observeEvent(input$inputpivot_enableHideEmptyCols, {
  if (isTRUE(input$inputpivot_enableHideEmptyCols)) {
    showEl(session, "#inputTable_pivot-miroPivot-hideEmptyCols")
  } else {
    updateCheckboxInput(session, "inputTable_pivot-miroPivot-hideEmptyCols", value = FALSE)
    hideEl(session, "#inputTable_pivot-miroPivot-hideEmptyCols")
  }
})
observeEvent(input$inputpivot_emptyUEL, {
  if (!identical(input$inputTable_type, "pivot")) {
    return()
  }
  if (identical(input$inputpivot_emptyUEL, "")) {
    rv$tableWidgetConfig$options$emptyUEL <- NULL
  } else {
    rv$tableWidgetConfig$options$emptyUEL <- input$inputpivot_emptyUEL
  }
  refreshTableType(refreshSameSymbol = FALSE)
})

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
  errMsg <- validateTableConfig(rv$tableWidgetConfig)
  if (nchar(errMsg)) {
    showHideEl(session, "#tableValidationErr", 5000L, errMsg)
    return()
  }
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

  if (currentTableSymbolName %in% inputSymMultiDimChoices) {
    if (identical(input$inputTable_type, "pivot")) {
      newConfig <- list(
        widgetType = "table",
        tableType = "pivot",
        options = list(
          aggregationFunction = input[["inputTable_pivot-miroPivot-aggregationFunction"]],
          pivotRenderer = input[["inputTable_pivot-miroPivot-pivotRenderer"]],
          enableHideEmptyCols = isTRUE(input$inputpivot_enableHideEmptyCols),
          hideEmptyCols = input[["inputTable_pivot-miroPivot-hideEmptyCols"]]
        )
      )
      if (length(rv$tableWidgetConfig$label)) {
        newConfig$label <- rv$tableWidgetConfig$label
      }
      if (length(rv$tableWidgetConfig$options$emptyUEL)) {
        newConfig$options$emptyUEL <- rv$tableWidgetConfig$options$emptyUEL
      }
      for (indexEl in list(c("rows", "rowIndexList"))) {
        indexVal <- input[[paste0("inputTable_pivot-miroPivot-", indexEl[[2]])]]
        if (length(indexVal)) {
          newConfig$options[[indexEl[[1]]]] <- indexVal
        }
      }
      for (indexEl in list(
        c("aggregations", "aggregationIndexList"),
        c("filter", "filterIndexList"),
        c("cols", "colIndexList")
      )) {
        indexVal <- input[[paste0("inputTable_pivot-miroPivot-", indexEl[[2]])]]
        if (length(indexVal)) {
          filterElList <- lapply(indexVal, function(el) {
            return(input[[paste0("inputTable_pivot-miroPivot-filter_", el)]])
          })
          names(filterElList) <- indexVal
          newConfig$options[[indexEl[[1]]]] <- filterElList
        }
      }
      configJSON$inputWidgets[[currentTableSymbolName]] <<- newConfig
    } else {
      if (identical(rv$tableWidgetConfig$tableType, configJSON$inputWidgets[[currentTableSymbolName]]$tableType)) {
        tableConfigTmp <- configJSON$inputWidgets[[currentTableSymbolName]]
        for (key in names(rv$tableWidgetConfig)) {
          tableConfigTmp[[key]] <- rv$tableWidgetConfig[[key]]
        }
      } else {
        tableConfigTmp <- rv$tableWidgetConfig
      }
      configJSON$inputWidgets[[currentTableSymbolName]] <<- tableConfigTmp
      if (!length(configJSON$inputWidgets[[currentTableSymbolName]]$readonlyCols)) {
        configJSON$inputWidgets[[currentTableSymbolName]]$readonlyCols <<- NULL
      }
      if (!length(configJSON$inputWidgets[[currentTableSymbolName]]$colWidths)) {
        configJSON$inputWidgets[[currentTableSymbolName]]$colWidths <<- NULL
      }
      if (is.null(configJSON$inputWidgets[[currentTableSymbolName]]$pivotCols) ||
        identical(configJSON$inputWidgets[[currentTableSymbolName]]$pivotCols, "_")) {
        configJSON$inputWidgets[[currentTableSymbolName]]$pivotCols <<- NULL
      } else {
        configJSON$inputWidgets[[currentTableSymbolName]]$colFormat <<- NULL
      }
    }
  } else if (currentTableSymbolName %in% outputSymMultiDimChoices) {
    configJSON$outputTables[[currentTableSymbolName]] <<- rv$tableWidgetConfig
    if (is.null(configJSON$outputTables[[currentTableSymbolName]]$pivotCols) ||
      identical(configJSON$outputTables[[currentTableSymbolName]]$pivotCols, "_")) {
      configJSON$outputTables[[currentTableSymbolName]]$pivotCols <<- NULL
    }
    if (isTRUE(input$outputTable_noGraph)) {
      configJSON$dataRendering[[currentTableSymbolName]] <<- rv$tableWidgetConfig
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
  # currentTableSymbolID <- match(currentTableSymbolName, tableSymbols)
  # if(!is.na(currentTableSymbolID) &&
  #    !identical(names(tableSymbols)[[currentTableSymbolID]], paste(currentTableSymbolName, ": ",
  #                                                                  rv$tableWidgetConfig$alias))){
  #   names(tableSymbols)[[currentTableSymbolID]] <<- paste0(currentTableSymbolName, ": ",
  #                                                          rv$tableWidgetConfig$alias)
  #   updateSelectInput(session, "table_symbol", choices = tableSymbols)
  # }
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
