latest_widget_symbol_type <- NULL
currentWidgetSymbolName <- character(0L)
inputPivotRendererEnv <- new.env(parent = emptyenv())
symbolsDefinedExternally <- inputSymInForeignRenderers[inputSymInForeignRenderers %in% widgetSymbols]
gamsOptionWidgets <- widgetSymbols[startsWith(widgetSymbols, prefixGMSOpt)]
doubleDashWidgets <- widgetSymbols[startsWith(widgetSymbols, prefixDDPar)]
widgetSymbols <- widgetSymbols[!widgetSymbols %in% c(gamsOptionWidgets, doubleDashWidgets)]

allInputSymbols <- setNames(
  list(
    c(widgetSymbols),
    c(inputSymMultiDimChoices),
    c(gamsOptionWidgets),
    c(doubleDashWidgets)
  ),
  c(
    lang$adminMode$widgets$ui$scalarSymbols,
    lang$adminMode$widgets$ui$multiDimSymbols,
    lang$adminMode$widgets$ui$widgetCategorieOpt,
    lang$adminMode$widgets$ui$widgetCategorieDoubleDash
  )
)
updateSelectInputNoClear <- function(session, id, choices) {
  selected <- input[[id]]
  if (length(selected) && !selected %in% choices) {
    selected <- NULL
  }
  updateSelectInput(session, id,
    choices = choices,
    selected = selected
  )
}
isNonSingletonSet <- function(symName) {
  return(length(modelInRaw[[symName]]$headers) == 2L)
}
isParameter <- function(symName) {
  return(identical(modelInRaw[[symName]]$symtype, "parameter"))
}

langSpecificWidget <- list()
langSpecificWidget$widgetOptionsInput <- setNames(
  c("slider", "dropdown", "checkbox", "numericinput"),
  c(
    lang$adminMode$widgets$widgetOptions$slider,
    lang$adminMode$widgets$widgetOptions$dropdown,
    lang$adminMode$widgets$widgetOptions$checkbox,
    lang$adminMode$widgets$widgetOptions$numericinput
  )
)
langSpecificWidget$widgetOptionsAll <- setNames(
  c(
    "slider", "sliderrange", "dropdown",
    "checkbox", "date", "daterange",
    "textinput", "numericinput"
  ),
  c(
    lang$adminMode$widgets$widgetOptions$slider,
    lang$adminMode$widgets$widgetOptions$sliderrange,
    lang$adminMode$widgets$widgetOptions$dropdown,
    lang$adminMode$widgets$widgetOptions$checkbox,
    lang$adminMode$widgets$widgetOptions$date,
    lang$adminMode$widgets$widgetOptions$daterange,
    lang$adminMode$widgets$widgetOptions$text,
    lang$adminMode$widgets$widgetOptions$numericinput
  )
)
langSpecificWidget$widgetOptionsGo <- setNames(
  c("slider", "dropdown", "checkbox", "textinput", "numericinput"),
  c(
    lang$adminMode$widgets$widgetOptions$slider,
    lang$adminMode$widgets$widgetOptions$dropdown,
    lang$adminMode$widgets$widgetOptions$checkbox,
    lang$adminMode$widgets$widgetOptions$text,
    lang$adminMode$widgets$widgetOptions$numericinput
  )
)
langSpecificWidget$widgetOptionsSet <- setNames(c("multidropdown", "table"), c(
  lang$adminMode$widgets$widgetOptions$dropdown,
  lang$adminMode$widgets$widgetOptions$table
))
langSpecificWidget$widgetOptionsParameter <- setNames(c("table"), c(
  lang$adminMode$widgets$widgetOptions$table
))
langSpecificWidget$widgetOptionsDate <- setNames("date", lang$adminMode$widgets$widgetOptions$date)
langSpecificWidget$widgetOptionsTextinput <- setNames("textinput", lang$adminMode$widgets$widgetOptions$text)
langSpecificWidget$minDepOp <- c(
  "Minimum" = "min", "Maximum" = "max", "Count" = "card",
  "Mean" = "mean", "Median" = "median", "Variance" = "var",
  "Standard Deviation" = "sd"
)
names(langSpecificWidget$minDepOp) <- lang$adminMode$widgets$slider$depOp$choices
langSpecificWidget$maxDepOp <- c(
  "Minimum" = "min", "Maximum" = "max", "Count" = "card",
  "Mean" = "mean", "Median" = "median", "Variance" = "var",
  "Standard Deviation" = "sd"
)
names(langSpecificWidget$maxDepOp) <- lang$adminMode$widgets$slider$depOp$choices
langSpecificWidget$defDepOp <- c(
  "Minimum" = "min", "Maximum" = "max", "Count" = "card",
  "Mean" = "mean", "Median" = "median", "Variance" = "var",
  "Standard Deviation" = "sd"
)
names(langSpecificWidget$defDepOp) <- lang$adminMode$widgets$slider$depOp$choices
langSpecificWidget$depChoices <- c("All" = "")
names(langSpecificWidget$depChoices) <- lang$adminMode$widgets$dropdown$choiceDep$depChoices
langSpecificWidget$startview <- c("Month" = "month", "Year" = "year", "Decade" = "decade")
names(langSpecificWidget$startview) <- lang$adminMode$widgets$date$startview$choices
langSpecificWidget$weekdays <- c(
  "Sunday" = 0L, "Monday" = 1L, "Tuesday" = 2L,
  "Wednesday" = 3L, "Thursday" = 4L, "Friday" = 5L, "Saturday" = 6L
)
names(langSpecificWidget$weekdays) <- lang$adminMode$widgets$date$weekstart$choices
langSpecificWidget$decimalCharacterChoices <- c(",", ".", "&middot;", "&#1643;", "&#9110;")
names(langSpecificWidget$decimalCharacterChoices) <- lang$adminMode$widgets$numericinput$decimalCharacterChoices
langSpecificWidget$digitGroupSeparatorChoices <- c(",", ".", " ", "empty", "'", "&#1644;", "&#729;")
names(langSpecificWidget$digitGroupSeparatorChoices) <- lang$adminMode$widgets$numericinput$digitGroupSeparatorChoices

if (length(modelInRaw[[scalarsFileName]])) {
  scalarInputSymWithAliases <- modelInRaw[[scalarsFileName]]$symnames
} else {
  scalarInputSymWithAliases <- c()
}

if (length(allInputSymbols)) {
  updateSelectInput(session, "widget_symbol", choices = allInputSymbols)
  noWidgetSymbols <- FALSE
} else {
  showEl(session, "#noSymbolMsg")
  showEl(session, "#noWidgetMsg")
  hideEl(session, "#noWidgetConfigMsg")
  hideEl(session, "#externalConfigMsg")
  hideEl(session, "#pivotColsRestriction")
  noWidgetSymbols <- TRUE
}

validateWidgetConfig <- function(widgetJSON) {
  if (startsWith(currentWidgetSymbolName, prefixDDPar) &&
    identical(nchar(trimws(currentWidgetSymbolName)), nchar(prefixDDPar))) {
    return(lang$adminMode$widgets$validate[["val2"]])
  }
  if (startsWith(currentWidgetSymbolName, prefixGMSOpt)) {
    if (identical(nchar(trimws(currentWidgetSymbolName)), nchar(prefixGMSOpt))) {
      return(lang$adminMode$widgets$validate[["val3"]])
    } else if (trimws(tolower(currentWidgetSymbolName)) %in% paste0(prefixGMSOpt, reservedGMSOpt)) {
      return(sprintf(
        lang$adminMode$widgets$validate[["val61"]],
        paste(reservedGMSOpt, collapse = "', '")
      ))
    }
  }
  if (identical(grepl("\\s", currentWidgetSymbolName), TRUE)) {
    return(lang$adminMode$widgets$validate$val39)
  }



  switch(widgetJSON$widgetType,
    slider = ,
    sliderrange = {
      if (!is.null(widgetJSON$default) && (length(widgetJSON$default) < 1L ||
        length(widgetJSON$default) > 2L)) {
        return(lang$adminMode$widgets$validate[["val6"]])
      }
      if (is.na(widgetJSON$min)) {
        return(lang$adminMode$widgets$validate$val40)
      }
      if (is.na(widgetJSON$max)) {
        return(lang$adminMode$widgets$validate$val41)
      }
      if (any(widgetJSON$max < widgetJSON$min) &&
        isTRUE(input$slider_min_dep_selector) &&
        isTRUE(input$slider_max_dep_selector)) {
        return(lang$adminMode$widgets$validate$val42)
      }
      if (!is.null(widgetJSON$default) && (any(widgetJSON$default < widgetJSON$min) &&
        isTRUE(input$slider_min_dep_selector) &&
        isTRUE(input$slider_def_dep_selector))) {
        return(lang$adminMode$widgets$validate[["val7"]])
      }
      if (!is.null(widgetJSON$default) && (any(widgetJSON$max < widgetJSON$default) &&
        isTRUE(input$slider_max_dep_selector) &&
        isTRUE(input$slider_def_dep_selector))) {
        return(lang$adminMode$widgets$validate[["val7"]])
      }
      if (!is.logical(widgetJSON$tick)) {
        return(lang$adminMode$widgets$validate[["val8"]])
      }
      if (!is.numeric(widgetJSON$step) || widgetJSON$step <= 0L) {
        return(lang$adminMode$widgets$validate[["val9"]])
      }
      if (length(widgetJSON$minStep)) {
        if (!is.numeric(widgetJSON$minStep) ||
          widgetJSON$minStep < 0L) {
          return(lang$adminMode$widgets$validate[["val48"]])
        }
      }
    },
    dropdown = ,
    multidropdown = {
      if (!length(widgetJSON$choices)) {
        return(lang$adminMode$widgets$validate$val38)
      }
      if (!identical(length(widgetJSON$aliases), 0L) &&
        !identical(length(widgetJSON$choices), length(widgetJSON$aliases))) {
        return(lang$adminMode$widgets$validate$val10)
      }
      if (length(widgetJSON$selected) && !widgetJSON$selected %in% widgetJSON$choices &&
        isTRUE(input$dd_choice_dep_selector)) {
        return(lang$adminMode$widgets$validate$val11)
      }
      if (isTRUE(widgetJSON$clearValue) && isTRUE(widgetJSON$multiple)) {
        return(lang$adminMode$widgets$validate$val55)
      }
    },
    textinput = {

    },
    checkbox = {
      if (isTRUE(widgetJSON$value)) {
        rv$widgetConfig$value <<- 1L
      } else if (isFALSE(widgetJSON$value)) {
        rv$widgetConfig$value <<- 0L
      } else if (!(identical(widgetJSON$value, 1L) || identical(widgetJSON$value, 0L))) {
        return(lang$adminMode$widgets$validate$val12)
      }
    },
    date = {
      defDate <- NULL
      minDate <- NULL
      maxDate <- NULL
      errMsg <- NULL
      if (!is.null(widgetJSON$value)) {
        eTxt <- lang$adminMode$widgets$validate$val13
        tryCatch(defDate <- as.Date(widgetJSON$value), error = function(e) {
          errMsg <<- eTxt
        })
        if (!length(defDate)) {
          errMsg <- eTxt
        }
      }
      if (!is.null(widgetJSON$min)) {
        eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val14, collapse = "\n")
        tryCatch(minDate <- as.Date(widgetJSON$min), error = function(e) {
          errMsg <<- eTxt
        })
        if (!length(minDate)) {
          errMsg <- eTxt
        }
      }
      if (!is.null(widgetJSON$max)) {
        eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val15, collapse = "\n")
        tryCatch(maxDate <- as.Date(widgetJSON$max), error = function(e) {
          errMsg <<- eTxt
        })
        if (!length(maxDate)) {
          errMsg <- eTxt
        }
      }
      if (length(errMsg)) {
        return(errMsg)
      }
      if (!is.null(defDate) && !is.null(minDate)) {
        if (defDate < minDate) {
          return(lang$adminMode$widgets$validate$val16)
        }
      }
      if (!is.null(defDate) && !is.null(maxDate)) {
        if (defDate > maxDate) {
          return(lang$adminMode$widgets$validate$val17)
        }
      }
      if (!is.null(minDate) && !is.null(maxDate)) {
        if (minDate > maxDate) {
          return(lang$adminMode$widgets$validate$val18)
        }
      }
      if (identical(nchar(widgetJSON$format), 0L)) {
        return(lang$adminMode$widgets$validate$val19)
      }
    },
    daterange = {
      startDate <- NULL
      endDate <- NULL
      minDate <- NULL
      maxDate <- NULL
      errMsg <- NULL
      if (!is.null(widgetJSON[["start"]])) {
        eTxt <- lang$adminMode$widgets$validate$val20
        tryCatch(startDate <- as.Date(widgetJSON$start), error = function(e) {
          errMsg <<- eTxt
        })
        if (!length(startDate)) {
          errMsg <- eTxt
        }
      }
      if (!is.null(widgetJSON$end)) {
        eTxt <- lang$adminMode$widgets$validate$val21
        tryCatch(endDate <- as.Date(widgetJSON$end), error = function(e) {
          errMsg <<- eTxt
        })
        if (!length(endDate)) {
          errMsg <- eTxt
        }
      }
      if (!is.null(widgetJSON$min)) {
        eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val22, collapse = "\n")
        tryCatch(minDate <- as.Date(widgetJSON$min), error = function(e) {
          errMsg <<- eTxt
        })
        if (!length(minDate)) {
          errMsg <- eTxt
        }
      }
      if (!is.null(widgetJSON$max)) {
        eTxt <- paste(errMsg, lang$adminMode$widgets$validate$val23, collapse = "\n")
        tryCatch(maxDate <- as.Date(widgetJSON$max), error = function(e) {
          errMsg <<- eTxt
        })
        if (!length(maxDate)) {
          errMsg <- eTxt
        }
      }
      if (length(errMsg)) {
        return(errMsg)
      }
      if (!is.null(startDate) && !is.null(minDate)) {
        if (startDate < minDate) {
          return(lang$adminMode$widgets$validate$val24)
        }
      }
      if (!is.null(endDate) && !is.null(maxDate)) {
        if (endDate > maxDate) {
          return(lang$adminMode$widgets$validate$val25)
        }
      }

      if (!is.null(startDate) && !is.null(minDate)) {
        if (startDate < minDate) {
          return(lang$adminMode$widgets$validate$val26)
        }
      }
      if (!is.null(endDate) && !is.null(maxDate)) {
        if (endDate > maxDate) {
          return(lang$adminMode$widgets$validate$val27)
        }
      }
      if (!is.null(minDate) && !is.null(maxDate)) {
        if (minDate > maxDate) {
          return(lang$adminMode$widgets$validate$val28)
        }
      }
      if (identical(nchar(widgetJSON$format), 0L)) {
        return(lang$adminMode$widgets$validate$val29)
      }
      if (!is.null(startDate) && !is.null(endDate)) {
        if (startDate > endDate) {
          return(lang$adminMode$widgets$validate$val30)
        }
      }
      if (!is.null(minDate) && !is.null(maxDate)) {
        if (minDate > maxDate) {
          return(lang$adminMode$widgets$validate$val31)
        }
      }
      if (!nchar(widgetJSON$separator)) {
        return(lang$adminMode$widgets$validate$val44)
      }
    },
    numericinput = {
      if (any(widgetJSON$max < widgetJSON$min)) {
        return(lang$adminMode$widgets$validate$val42)
      }
      if (!is.null(widgetJSON$value) && (any(widgetJSON$value < widgetJSON$min))) {
        return(lang$adminMode$widgets$validate[["val7"]])
      }
      if (!is.null(widgetJSON$value) && (any(widgetJSON$max < widgetJSON$value))) {
        return(lang$adminMode$widgets$validate[["val7"]])
      }
      if (identical(widgetJSON[["decimalCharacter"]], widgetJSON[["digitGroupSeparator"]])) {
        return(lang$adminMode$widgets$validate[["val58"]])
      }
    },
    table = {
      if (identical(configJSON$tableType, "pivot") && sum(vapply(modelIn[[currentWidgetSymbolName]]$headers, function(header) {
        return(identical(header$type, "numeric"))
      }, logical(1L))) > 1L) {
        return(sprintf(lang$adminMode$widgets$validate$val59, currentWidgetSymbolName))
      }
      if (any(!configJSON$readonlyCols %in% inputSymHeaders[[currentWidgetSymbolName]])) {
        return(lang$adminMode$widgets$validate$val34)
      }
    },
    {
      return(lang$adminMode$widgets$validate$val35)
    }
  )
  return("")
}
output$widgetTableLabelWrapper <- renderUI({
  if (!identical(rv$widgetConfig$widgetType, "table")) {
    return()
  }
  if (length(rv$widgetConfig$label) && !identical(trimws(rv$widgetConfig$label), "")) {
    tags$div(
      id = "widgetTableLabel",
      class = "readme-wrapper label-wrapper",
      markdown(rv$widgetConfig$label)
    )
  }
})
output$hot_preview <- renderRHandsontable({
  req(input$widget_symbol %in% names(inputSymHeaders))
  if (!identical(rv$widgetConfig$tableType, "default")) {
    return()
  }
  data <- createTableData(input$widget_symbol, rv$widgetConfig$pivotCols)

  headersTmp <- data$headersRaw
  headersUnnamed <- unname(headersTmp)
  colHeaders <- data$headers
  pivotTable <- data$isPivotTable
  data <- data$data
  colsReadonly <- match(rv$widgetConfig$readonlyCols, headersUnnamed)
  colsReadonly <- colsReadonly[!is.na(colsReadonly)]
  colWidths <- if (!is.null(rv$widgetConfig$colWidths) && !identical(rv$widgetConfig$colWidths, "custom")) {
    rv$widgetConfig$colWidths
  } else {
    200
  }
  fixedColumnsLeft <- isolate(rv$widgetConfig$fixedColumnsLeft)
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
    for (colName in names(rv$widgetConfig$colFormat)) {
      ht <- hot_col(ht, match(colName, headersUnnamed),
        format = rv$widgetConfig$colFormat[[colName]]$format
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

output$bigdata_preview <- renderDT({
  req(input$widget_symbol %in% names(inputSymHeaders))

  data <- createTableData(input$widget_symbol, input$table_pivotCols)

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

  if (!identical(rv$widgetConfig$tableType, "bigdata")) {
    # showEl(session, "#hot_preview")
    return()
  }
  # hideEl(session, "#hot_preview")
  return(renderDTable(data, dtOptions, render = FALSE))
})

observeEvent(
  {
    input$widget_symbol
    rv$widget_symbol
  },
  {
    req(
      length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L,
      identical(input$widget_symbol_type, "gams")
    )
    hideEl(session, "#noSymbolMsg")
    hideEl(session, "#noWidgetMsg")
    hideEl(session, "#noWidgetConfigMsg")
    hideEl(session, "#externalConfigMsg")
    hideEl(session, "#pivotColsRestriction")
    if (input$widget_symbol %in% symbolsDefinedExternally) {
      showElReplaceTxt(
        session, "#externalConfigMsg",
        sprintf(
          lang$adminMode$widgets$ui$widgetExternallyDefinedMsg,
          names(symbolsDefinedExternally)[symbolsDefinedExternally == input$widget_symbol][[1]]
        )
      )
    }
    if (input$widget_symbol %in% modelInRaw[[scalarsFileName]]$symnames) {
      currentWidgetSymbolName <<- input$widget_symbol
      if (!currentWidgetSymbolName %in% names(configJSON$inputWidgets)) {
        showEl(session, "#noWidgetConfigMsg")
      }
      symID <- match(input$widget_symbol, modelInRaw[[scalarsFileName]]$symnames)
      widgetOptions <- langSpecificWidget$widgetOptionsInput

      if (!identical(modelInRaw[[scalarsFileName]]$symtypes[[symID]], "parameter")) {
        # lang$adminMode$widgets$widgetOptionsDate$choices
        widgetOptions <- c(
          widgetOptions, langSpecificWidget$widgetOptionsDate,
          langSpecificWidget$widgetOptionsTextinput
        )
      }
    } else if (any(startsWith(input$widget_symbol, c(prefixDDPar, prefixGMSOpt)))) {
      currentWidgetSymbolName <<- input$widget_symbol
      if (startsWith(currentWidgetSymbolName, prefixGMSOpt)) {
        widgetOptions <- langSpecificWidget$widgetOptionsGo
      }
      if (startsWith(currentWidgetSymbolName, prefixDDPar)) {
        widgetOptions <- langSpecificWidget$widgetOptionsAll
      }
    } else if (input$widget_symbol %in% names(modelInRaw)) {
      currentWidgetSymbolName <<- input$widget_symbol
      if (!currentWidgetSymbolName %in% names(configJSON$inputWidgets)) {
        showEl(session, "#noWidgetConfigMsg")
      }
      if (isParameter(input$widget_symbol)) {
        widgetOptions <- langSpecificWidget$widgetOptionsParameter
      } else if (isNonSingletonSet(input$widget_symbol)) {
        widgetOptions <- langSpecificWidget$widgetOptionsSet
      } else {
        flog.error("Unknown input symbol: '%s'.", input$widget_symbol)
        showHideEl(session, "#unknownErrorWidgets", 4000L)
        return()
      }
    } else {
      flog.error("Unknown input symbol: '%s'.", input$widget_symbol)
      showHideEl(session, "#unknownErrorWidgets", 4000L)
      return()
    }
    selectedType <- widgetOptions[[1L]]
    if (currentWidgetSymbolName %in% names(configJSON$inputWidgets)) {
      selectedType <- configJSON$inputWidgets[[currentWidgetSymbolName]]$widgetType
      if (identical(selectedType, "slider") &&
        identical(length(configJSON$inputWidgets[[currentWidgetSymbolName]]$default), 2L)) {
        selectedType <- "sliderrange"
      } else if (identical(selectedType, "dropdown") &&
        (isTRUE(configJSON$inputWidgets[[currentWidgetSymbolName]]$multiple) ||
          isNonSingletonSet(currentWidgetSymbolName))) {
        selectedType <- "multidropdown"
      }
      if (!selectedType %in% widgetOptions) {
        widgetAlias <- names(langSpecificWidget$widgetOptionsAll[langSpecificWidget$widgetOptionsAll == selectedType[1]])
        removeUI(selector = "#widget_options .shiny-input-container", multiple = TRUE)
        insertUI(
          selector = "#widget_options",
          tags$div(
            class = "shiny-input-container config-no-hide",
            sprintf(
              lang$adminMode$widgets$ui$notConfigurable, currentWidgetSymbolName,
              if (length(widgetAlias)) widgetAlias else selectedType
            )
          ),
          where = "beforeEnd"
        )
        disableEl(session, "#saveWidget")
        disableEl(session, "#deleteWidget")
        output$widget_preview <- renderUI({
        })
      }
    }

    updateSelectInput(session, "widget_type",
      choices = widgetOptions,
      selected = selectedType
    )
    if (identical(input$widget_type, selectedType)) {
      rv$widget_type <- rv$widget_type + 1L
    }
  }
)
observeEvent(input$widget_go, {
  currentWidgetSymbolName <<- prefixGMSOpt %+% tolower(input$widget_go)
})
observeEvent(input$widget_dd, {
  ddName <- gsub("^[-]*", "", tolower(input$widget_dd))
  currentWidgetSymbolName <<- prefixDDPar %+% ddName
})
observeEvent(input$widget_symbol_type, {
  if (input$widget_symbol_type %in% c("dd", "go")) {
    hideEl(session, "#noSymbolMsg")
    hideEl(session, "#noWidgetMsg")
    hideEl(session, "#noWidgetConfigMsg")
    hideEl(session, "#deleteWidget")
    hideEl(session, "#externalConfigMsg")
    hideEl(session, "#pivotColsRestriction")

    updateTextInput(session, "widget_label", value = "")
    updateTextInput(session, "widget_tooltip", value = "")
    updateTextInput(session, paste0("widget_", input$widget_symbol_type), value = "")

    widgetOptionsTmp <- if (identical(input$widget_symbol_type, "dd")) {
      langSpecificWidget$widgetOptionsAll
    } else {
      langSpecificWidget$widgetOptionsGo
    }
    updateSelectInput(session, "widget_type", choices = widgetOptionsTmp)
    if (identical(input$widget_type, widgetOptionsTmp[[1]])) {
      rv$widget_type <- rv$widget_type + 1L
    }

    if (!length(latest_widget_symbol_type)) {
      latest_widget_symbol_type <<- input$widget_symbol_type
      return()
    }
    if (identical(input$widget_symbol_type, "go")) {
      currentWidgetSymbolName <<- prefixGMSOpt %+% tolower(input$widget_go)
    } else if (identical(input$widget_symbol_type, "dd")) {
      currentWidgetSymbolName <<- prefixDDPar %+% tolower(input$widget_dd)
    }
    latest_widget_symbol_type <<- input$widget_symbol_type
    return()
  } else if (!length(allInputSymbols)) {
    showEl(session, "#noSymbolMsg")
    showEl(session, "#noWidgetMsg")
    showEl(session, "#deleteWidget")
    currentWidgetSymbolName <<- character(0L)
    latest_widget_symbol_type <<- input$widget_symbol_type
    return()
  }
  showEl(session, "#deleteWidget")
  latest_widget_symbol_type <<- input$widget_symbol_type
  rv$widget_symbol <- rv$widget_symbol + 1L
})
observeEvent(
  {
    input$widget_type
    rv$widget_type
  },
  {
    req(
      length(input$widget_type) > 0L, length(currentWidgetSymbolName) > 0L,
      nchar(currentWidgetSymbolName) > 0L
    )
    removeUI(selector = "#widget_options .shiny-input-container", multiple = TRUE)
    enableEl(session, "#saveWidget")
    enableEl(session, "#deleteWidget")
    rv$widgetConfig <- list()
    currentConfig <- NULL
    isGamsTable <<- FALSE
    if (currentWidgetSymbolName %in% names(configJSON$inputWidgets)) {
      currentConfig <- configJSON$inputWidgets[[currentWidgetSymbolName]]
    }
    setAlias <- FALSE
    widgetAlias <- ""
    if (!currentWidgetSymbolName %in% inputSymMultiDimChoices) {
      if (length(currentConfig[["alias"]]) && nchar(currentConfig[["alias"]])) {
        widgetAlias <- currentConfig[["alias"]]
        setAlias <- TRUE
      } else {
        setAlias <- FALSE
        widgetSymbolID <- match(input$widget_symbol, allInputSymbols)
        if (!is.na(widgetSymbolID)) {
          widgetAlias <- names(allInputSymbols)[[widgetSymbolID]]
        }
      }
    }
    switch(input$widget_type,
      slider = {
        rv$widgetConfig <- list(
          widgetType = "slider",
          min = if (length(currentConfig$min)) currentConfig$min else 0L,
          max = if (length(currentConfig$max)) currentConfig$max else 10L,
          default =
            if (length(currentConfig$default) && length(currentConfig$default) < 2L) {
              currentConfig$default
            } else if (length(currentConfig$default) && length(currentConfig$default) > 1L) {
              currentConfig$default[1]
            } else if (is.numeric(currentConfig$min)) {
              currentConfig$min
            } else {
              2L
            },
          step = if (length(currentConfig$step)) currentConfig$step else 1L,
          ticks = isTRUE(currentConfig$ticks),
          noHcube = isTRUE(currentConfig$noHcube)
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig$minStep <- currentConfig$minStep
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        dynamicMin <- getWidgetDependencies("slider", rv$widgetConfig$min)
        dynamicMax <- getWidgetDependencies("slider", rv$widgetConfig$max)
        dynamicDef <- getWidgetDependencies("slider", rv$widgetConfig$default)

        staticMinInput <- tags$div(numericInput("slider_min", lang$adminMode$widgets$slider$min,
          value = if (is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L
        ))
        staticMaxInput <- tags$div(numericInput("slider_max", lang$adminMode$widgets$slider$max,
          value = if (is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L
        ))
        staticDefInput <- tags$div(numericInput("slider_def", lang$adminMode$widgets$slider$default,
          value =
            if (is.numeric(rv$widgetConfig$default)) {
              rv$widgetConfig$default
            } else if (is.numeric(rv$widgetConfig$min)) {
              rv$widgetConfig$min
            } else {
              NULL
            }
        ))
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$slider$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$slider$minlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  if (length(inputSymMultiDim)) {
                    tagList(
                      conditionalPanel(
                        condition = "input.slider_min_dep_selector===true",
                        staticMinInput
                      ),
                      conditionalPanel(
                        condition = "input.slider_min_dep_selector!==true",
                        tags$label(
                          class = "cb-label", "for" = "slider_min_dep_wrapper",
                          style = "display:block;", lang$adminMode$widgets$slider$dep
                        ),
                        tags$div(
                          id = "slider_min_dep_wrapper",
                          class = "shiny-input-container two-col-wrapper",
                          tags$div(
                            class = "two-col-left",
                            selectInput("slider_min_dep", NULL,
                              choices = inputSymMultiDim, selected = dynamicMin[2]
                            )
                          ),
                          tags$div(
                            class = "two-col-right",
                            selectInput("slider_min_dep_header", NULL,
                              choices = if (length(dynamicMin)) {
                                inputSymHeaders[[dynamicMin[2]]]
                              } else {
                                inputSymHeaders[[1]]
                              },
                              selected = dynamicMin[3]
                            )
                          )
                        ),
                        selectInput("slider_min_dep_op", lang$adminMode$widgets$slider$depOp$label,
                          choices = langSpecificWidget$minDepOp,
                          selected = dynamicMin[1]
                        )
                      )
                    )
                  } else {
                    staticMinInput
                  }
                ),
                if (length(inputSymMultiDim)) {
                  tags$div(
                    class = "col-sm-4",
                    tags$div(
                      class = "shiny-input-container",
                      checkboxInput_MIRO(
                        "slider_min_dep_selector",
                        lang$adminMode$widgets$slider$depSelector,
                        is.numeric(rv$widgetConfig$min)
                      )
                    )
                  )
                }
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$slider$maxlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  if (length(inputSymMultiDim)) {
                    tagList(
                      conditionalPanel(
                        condition = "input.slider_max_dep_selector===true",
                        staticMaxInput
                      ),
                      conditionalPanel(
                        condition = "input.slider_max_dep_selector!==true",
                        tags$label(
                          class = "cb-label", "for" = "slider_max_dep_wrapper",
                          style = "display:block;", lang$adminMode$widgets$slider$dep
                        ),
                        tags$div(
                          id = "slider_max_dep_wrapper",
                          class = "shiny-input-container two-col-wrapper",
                          tags$div(
                            class = "two-col-left",
                            selectInput("slider_max_dep", NULL,
                              choices = inputSymMultiDim, selected = dynamicMax[2]
                            )
                          ),
                          tags$div(
                            class = "two-col-right",
                            selectInput("slider_max_dep_header", NULL,
                              choices = if (length(dynamicMax)) {
                                inputSymHeaders[[dynamicMax[2]]]
                              } else {
                                inputSymHeaders[[1]]
                              },
                              selected = dynamicMax[3]
                            )
                          )
                        ),
                        selectInput("slider_max_dep_op", lang$adminMode$widgets$slider$depOp$label,
                          choices = langSpecificWidget$maxDepOp,
                          selected = dynamicMax[1]
                        )
                      )
                    )
                  } else {
                    staticMaxInput
                  }
                ),
                if (length(inputSymMultiDim)) {
                  tags$div(
                    class = "col-sm-4",
                    tags$div(
                      class = "shiny-input-container",
                      checkboxInput_MIRO(
                        "slider_max_dep_selector",
                        lang$adminMode$widgets$slider$depSelector,
                        is.numeric(rv$widgetConfig$max)
                      )
                    )
                  )
                }
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$slider$defaultlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  if (length(inputSymMultiDim)) {
                    tagList(
                      conditionalPanel(
                        condition = "input.slider_def_dep_selector===true",
                        staticDefInput
                      ),
                      conditionalPanel(
                        condition = "input.slider_def_dep_selector!==true",
                        tags$label(
                          class = "cb-label", "for" = "slider_max_dep_wrapper",
                          style = "display:block;", lang$adminMode$widgets$slider$dep
                        ),
                        tags$div(
                          id = "slider_max_dep_wrapper",
                          class = "shiny-input-container two-col-wrapper",
                          tags$div(
                            class = "two-col-left",
                            selectInput("slider_def_dep", NULL,
                              choices = inputSymMultiDim, selected = dynamicDef[2]
                            )
                          ),
                          tags$div(
                            class = "two-col-right",
                            selectInput("slider_def_dep_header", NULL,
                              choices = if (length(dynamicDef)) {
                                inputSymHeaders[[dynamicDef[2]]]
                              } else {
                                inputSymHeaders[[1]]
                              },
                              selected = dynamicDef[3]
                            )
                          )
                        ),
                        selectInput("slider_def_dep_op", lang$adminMode$widgets$slider$depOp$label,
                          choices = langSpecificWidget$defDepOp,
                          selected = dynamicDef[1]
                        )
                      )
                    )
                  } else {
                    staticDefInput
                  }
                ),
                if (length(inputSymMultiDim)) {
                  tags$div(
                    class = "col-sm-4",
                    tags$div(
                      class = "shiny-input-container",
                      checkboxInput_MIRO(
                        "slider_def_dep_selector",
                        lang$adminMode$widgets$slider$depSelector,
                        is.numeric(rv$widgetConfig$default)
                      )
                    )
                  )
                }
              )
            ),
            tags$div(
              class = "option-wrapper",
              numericInput("slider_step", lang$adminMode$widgets$slider$step, value = rv$widgetConfig$step, min = 0L),
              tags$div(
                class = "info-position",
                numericInput("slider_minStep", tags$div(
                  lang$adminMode$widgets$slider$minStep,
                  tags$a("",
                    title = lang$adminMode$widgets$slider$minStepTooltip,
                    class = "info-wrapper",
                    href = "https://gams.com/miro/widgets.html#slider-option-minstep",
                    tags$span(
                      class = "fas fa-info-circle", class = "info-icon",
                      role = "presentation",
                      `aria-label` = "More information"
                    ), target = "_blank"
                  )
                ),
                value = rv$widgetConfig$minStep, min = 0L
                )
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "slider_ticks", lang$adminMode$widgets$slider$ticks,
                rv$widgetConfig$ticks
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "widget_hcube", lang$adminMode$widgets$slider$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          sliderInput("slider_preview", widgetlabel,
            min = if (is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L,
            max = if (is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L,
            value = if (is.numeric(rv$widgetConfig$default)) rv$widgetConfig$default else 2L,
            step = if (is.numeric(rv$widgetConfig$step)) rv$widgetConfig$step else 1L,
            ticks = rv$widgetConfig$ticks
          )
        })
      },
      sliderrange = {
        rv$widgetConfig <- list(
          widgetType = "slider",
          min = if (length(currentConfig$min)) currentConfig$min else 0L,
          max = if (length(currentConfig$max)) currentConfig$max else 10L,
          default =
            if (length(currentConfig$default) > 1L) {
              currentConfig$default
            } else if (is.numeric(currentConfig$min)) {
              c(currentConfig$min, currentConfig$min)
            } else {
              c(2L, 5L)
            },
          step = if (length(currentConfig$step)) currentConfig$step else 1L,
          ticks = isTRUE(currentConfig$ticks),
          noHcube = isTRUE(currentConfig$noHcube)
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig$minStep <- currentConfig$minStep
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        dynamicMin <- getWidgetDependencies("slider", rv$widgetConfig$min)
        dynamicMax <- getWidgetDependencies("slider", rv$widgetConfig$max)
        staticMinInput <- numericInput("slider_min", lang$adminMode$widgets$sliderrange$min,
          value = if (is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L
        )
        staticMaxInput <- numericInput("slider_max", lang$adminMode$widgets$sliderrange$max,
          value = if (is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L
        )

        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$sliderrange$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$sliderrange$minlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  if (length(inputSymMultiDim)) {
                    tagList(
                      conditionalPanel(
                        condition = "input.slider_min_dep_selector===true",
                        staticMinInput
                      ),
                      conditionalPanel(
                        condition = "input.slider_min_dep_selector!==true",
                        tags$label(
                          class = "cb-label", "for" = "slider_min_dep_wrapper",
                          style = "display:block;", lang$adminMode$widgets$sliderrange$dep
                        ),
                        tags$div(
                          id = "slider_min_dep_wrapper",
                          class = "shiny-input-container two-col-wrapper",
                          tags$div(
                            class = "two-col-left",
                            selectInput("slider_min_dep", NULL,
                              choices = inputSymMultiDim, selected = dynamicMin[2]
                            )
                          ),
                          tags$div(
                            class = "two-col-right",
                            selectInput("slider_min_dep_header", NULL,
                              choices = if (length(dynamicMin)) {
                                inputSymHeaders[[dynamicMin[2]]]
                              } else {
                                inputSymHeaders[[1]]
                              },
                              selected = dynamicMin[3]
                            )
                          )
                        ),
                        selectInput("slider_min_dep_op", lang$adminMode$widgets$sliderrange$depOp$label,
                          choices = langSpecificWidget$minDepOp,
                          selected = dynamicMin[1]
                        )
                      )
                    )
                  } else {
                    staticMinInput
                  }
                ),
                if (length(inputSymMultiDim)) {
                  tags$div(
                    class = "col-sm-4",
                    tags$div(
                      class = "shiny-input-container",
                      checkboxInput_MIRO(
                        "slider_min_dep_selector",
                        lang$adminMode$widgets$sliderrange$depSelector,
                        is.numeric(rv$widgetConfig$min)
                      )
                    )
                  )
                }
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$slider$maxlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  if (length(inputSymMultiDim)) {
                    tagList(
                      conditionalPanel(
                        condition = "input.slider_max_dep_selector===true",
                        staticMaxInput
                      ),
                      conditionalPanel(
                        condition = "input.slider_max_dep_selector!==true",
                        tags$label(
                          class = "cb-label", "for" = "slider_max_dep_wrapper",
                          style = "display:block;", lang$adminMode$widgets$sliderrange$dep
                        ),
                        tags$div(
                          id = "slider_max_dep_wrapper",
                          class = "shiny-input-container two-col-wrapper",
                          tags$div(
                            class = "two-col-left",
                            selectInput("slider_max_dep", NULL,
                              choices = inputSymMultiDim, selected = dynamicMax[2]
                            )
                          ),
                          tags$div(
                            class = "two-col-right",
                            selectInput("slider_max_dep_header", NULL,
                              choices = if (length(dynamicMax)) {
                                inputSymHeaders[[dynamicMax[2]]]
                              } else {
                                inputSymHeaders[[1]]
                              },
                              selected = dynamicMax[3]
                            )
                          )
                        ),
                        selectInput("slider_max_dep_op", lang$adminMode$widgets$sliderrange$depOp$label,
                          choices = langSpecificWidget$maxDepOp,
                          selected = dynamicMax[1]
                        )
                      )
                    )
                  } else {
                    staticMaxInput
                  }
                ),
                if (length(inputSymMultiDim)) {
                  tags$div(
                    class = "col-sm-4",
                    tags$div(
                      class = "shiny-input-container",
                      checkboxInput_MIRO(
                        "slider_max_dep_selector",
                        lang$adminMode$widgets$sliderrange$depSelector,
                        is.numeric(rv$widgetConfig$max)
                      )
                    )
                  )
                }
              )
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                numericInput("slider_def1", lang$adminMode$widgets$sliderrange$default1,
                  value = rv$widgetConfig$default[1]
                )
              ),
              tags$div(
                class = "two-col-right",
                numericInput("slider_def2", lang$adminMode$widgets$sliderrange$default2,
                  value = rv$widgetConfig$default[2]
                )
              )
            ),
            tags$div(
              class = "two-col-left",
              numericInput("slider_step", lang$adminMode$widgets$sliderrange$step,
                value = rv$widgetConfig$step, min = 0L
              ),
              numericInput("slider_minStep", lang$adminMode$widgets$slider$minStep,
                value = rv$widgetConfig$minStep, min = 0L
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "slider_ticks", lang$adminMode$widgets$sliderrange$ticks,
                rv$widgetConfig$ticks
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "widget_hcube",
                lang$adminMode$widgets$sliderrange$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          sliderInput("slider_preview", widgetlabel,
            min = if (is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min else 0L,
            max = if (is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max else 10L,
            value = rv$widgetConfig$default,
            step = if (is.numeric(rv$widgetConfig$step)) rv$widgetConfig$step else 1L,
            ticks = rv$widgetConfig$ticks
          )
        })
      },
      dropdown = ,
      multidropdown = {
        rv$widgetConfig <- list(
          widgetType = "dropdown",
          choices = currentConfig$choices,
          selected = currentConfig$selected,
          noHcube = isTRUE(currentConfig$noHcube),
          clearValue = isTRUE(currentConfig$clearValue),
          multiple = isTRUE(currentConfig$multiple)
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        rv$widgetConfig$aliases <- currentConfig$aliases
        dynamicChoices <- getWidgetDependencies("dropdown", rv$widgetConfig$choices)
        singletonSetId <- NA_integer_
        if (scalarsFileName %in% names(modelInRaw)) {
          singletonSetId <- match(currentWidgetSymbolName, modelInRaw[[scalarsFileName]]$symnames)[1L]
        }
        ddChoicesDynamic <- inputSymMultiDim[!inputSymMultiDim %in% currentWidgetSymbolName]
        staticChoiceInput <- tagList(
          selectizeInput("dd_choices", lang$adminMode$widgets$dropdown$choices,
            if (!length(dynamicChoices)) currentConfig$choices else c(),
            selected = if (!length(dynamicChoices)) currentConfig$choices else "",
            multiple = TRUE, options = list(
              "create" = TRUE,
              "persist" = FALSE
            )
          ),
          selectizeInput("dd_aliases", lang$adminMode$widgets$dropdown$choicesAlias,
            if (!length(dynamicChoices)) currentConfig$aliases else c(),
            selected = if (!length(dynamicChoices)) rv$widgetConfig$aliases else "",
            multiple = TRUE, options = list(
              "create" = TRUE,
              "persist" = FALSE
            )
          ),
          selectInput("dd_default", lang$adminMode$widgets$dropdown$default,
            choices = if (length(dynamicChoices)) "" else rv$widgetConfig$choices,
            selected = if (length(dynamicChoices)) "" else rv$widgetConfig$selected
          )
        )
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$dropdown$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container conditional highlight-block",
              tags$div(
                class = "col-sm-8",
                if (length(ddChoicesDynamic)) {
                  if (length(dynamicChoices)) {
                    if (identical(dynamicChoices[[2]], "")) {
                      depHeader <- allInputSymHeaders
                    } else {
                      depHeader <- inputSymHeaders[[dynamicChoices[2]]]
                    }
                  } else {
                    depHeader <- inputSymHeaders[[1]]
                  }
                  tagList(
                    conditionalPanel(
                      condition = "input.dd_choice_dep_selector===true",
                      staticChoiceInput
                    ),
                    conditionalPanel(
                      condition = "input.dd_choice_dep_selector!==true",
                      tags$label(
                        class = "cb-label", "for" = "dd_choice_dep_wrapper",
                        style = "display:block;", lang$adminMode$widgets$dropdown$choiceDep$label
                      ),
                      tags$div(
                        id = "dd_choice_dep_wrapper",
                        class = "shiny-input-container two-col-wrapper",
                        tags$div(
                          class = "two-col-left",
                          selectInput("dd_choice_dep", NULL,
                            choices = c(
                              langSpecificWidget$depChoices,
                              ddChoicesDynamic
                            ),
                            selected = dynamicChoices[2]
                          )
                        ),
                        tags$div(
                          class = "two-col-right",
                          selectInput("dd_choice_dep_header", NULL,
                            choices = depHeader,
                            selected = dynamicChoices[3]
                          )
                        )
                      ),
                      checkboxInput_MIRO(
                        "dd_choice_dep_type",
                        lang$adminMode$widgets$dropdown$choiceDep$type,
                        identical(dynamicChoices[1], "2")
                      )
                    )
                  )
                } else {
                  staticChoiceInput
                }
              ),
              if (length(ddChoicesDynamic)) {
                tags$div(
                  class = "col-sm-4",
                  checkboxInput_MIRO(
                    "dd_choice_dep_selector",
                    lang$adminMode$widgets$dropdown$choiceDep$selector,
                    identical(length(dynamicChoices), 0L)
                  )
                )
              }
            ),
            if (input$widget_type == "multidropdown") {
              tags$div(
                class = "shiny-input-container",
                checkboxInput_SIMPLE(
                  "widget_multiple",
                  lang$adminMode$widgets$dropdown$multiple,
                  rv$widgetConfig$multiple
                )
              )
            },
            if (!is.na(singletonSetId) &&
              identical(modelInRaw[[scalarsFileName]]$symtypes[singletonSetId], "set")) {
              tags$div(
                class = "shiny-input-container info-position",
                checkboxInput_SIMPLE(
                  "widget_clearValue",
                  tags$div(
                    lang$adminMode$widgets$dropdown$clearValue,
                    tags$a("",
                      title = lang$adminMode$widgets$dropdown$clearValueTooltip,
                      class = "info-wrapper", href = "https://gams.com/miro/widgets.html#dropdown-option-clearvalue",
                      tags$span(
                        class = "fas fa-info-circle", class = "info-icon",
                        role = "presentation",
                        `aria-label` = "More information"
                      ), target = "_blank"
                    )
                  ),
                  rv$widgetConfig$clearValue
                )
              )
            },
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "widget_hcube",
                lang$adminMode$widgets$dropdown$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          if (identical(length(rv$widgetConfig$aliases), 0L)) {
            choices <- rv$widgetConfig$choices
          } else {
            req(identical(length(rv$widgetConfig$choices), length(rv$widgetConfig$aliases)))
            choices <- setNames(rv$widgetConfig$choices, rv$widgetConfig$aliases)
          }
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          selectInput("dropdown_preview", widgetlabel,
            choices = choices,
            selected = rv$widgetConfig$selected, multiple = rv$widgetConfig$multiple
          )
        })
      },
      checkbox = {
        rv$widgetConfig <- list(
          widgetType = "checkbox",
          value = identical(currentConfig$value, 1L),
          noHcube = isTRUE(currentConfig$noHcube),
          class = "checkbox-material"
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$checkbox$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "widget_value",
                lang$adminMode$widgets$checkbox$default,
                rv$widgetConfig$value
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "widget_hcube",
                lang$adminMode$widgets$checkbox$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )

        output$widget_preview <- renderUI({
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          tagList(
            checkboxInput_MIRO(
              "checkbox_preview",
              widgetlabel,
              rv$widgetConfig$value
            )
          )
        })
      },
      date = {
        rv$widgetConfig <- list(
          widgetType = "date",
          format = if (length(currentConfig$format)) currentConfig$format else "yyyy-mm-dd",
          startview = if (length(currentConfig$startview)) currentConfig$startview else "month",
          weekstart = if (length(currentConfig$weekstart)) currentConfig$weekstart else 0L,
          autoclose = if (identical(currentConfig$autoclose, FALSE)) FALSE else TRUE,
          noHcube = isTRUE(currentConfig$noHcube)
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig$value <- currentConfig$value
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        rv$widgetConfig$min <- currentConfig$min
        rv$widgetConfig$max <- currentConfig$max
        rv$widgetConfig$daysofweekdisabled <- currentConfig$daysofweekdisabled
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$date$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$date$defaultlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  conditionalPanel(
                    condition = "input.date_def_off!==true",
                    dateInput("date_default", lang$adminMode$widgets$date$default, value = rv$widgetConfig$value)
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$div(
                    class = "shiny-input-container",
                    checkboxInput_MIRO(
                      "date_def_off",
                      lang$adminMode$widgets$date$defOff,
                      is.null(rv$widgetConfig$value)
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$date$minlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  conditionalPanel(
                    condition = "input.date_min_off!==true",
                    dateInput("date_min", lang$adminMode$widgets$date$min, value = rv$widgetConfig$min)
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$div(
                    class = "shiny-input-container",
                    checkboxInput_MIRO(
                      "date_min_off", lang$adminMode$widgets$date$minOff,
                      is.null(rv$widgetConfig$min)
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$date$maxlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  conditionalPanel(
                    condition = "input.date_max_off!==true",
                    dateInput("date_max", lang$adminMode$widgets$date$max,
                      value = rv$widgetConfig$max
                    )
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$div(
                    class = "shiny-input-container",
                    checkboxInput_MIRO(
                      "date_max_off", lang$adminMode$widgets$date$maxOff,
                      is.null(rv$widgetConfig$max)
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block conditional",
              tags$div(
                class = "col-sm-8 no-padding-left",
                conditionalPanel(
                  condition = "input.date_format_custom_selector!==true",
                  selectInput("date_format", lang$adminMode$widgets$date$format,
                    choices = dateFormatChoices,
                    selected = if (rv$widgetConfig$format %in% dateFormatChoices) {
                      rv$widgetConfig$format
                    } else {
                      NULL
                    }
                  )
                ),
                conditionalPanel(
                  condition = "input.date_format_custom_selector===true",
                  textInput("date_format_custom", lang$adminMode$widgets$date$formatCustom,
                    value = rv$widgetConfig$format
                  )
                )
              ),
              tags$div(
                class = "col-sm-4",
                tags$div(
                  class = "shiny-input-container",
                  checkboxInput_MIRO(
                    "date_format_custom_selector",
                    lang$adminMode$widgets$date$formatCustomSelector,
                    !rv$widgetConfig$format %in% dateFormatChoices
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                selectInput("date_startview", lang$adminMode$widgets$date$startview$label,
                  choices = langSpecificWidget$startview,
                  selected = rv$widgetConfig$startview
                )
              ),
              tags$div(
                class = "two-col-right",
                selectInput("date_weekstart", lang$adminMode$widgets$date$weekstart$label,
                  choices = langSpecificWidget$weekdays,
                  selected = rv$widgetConfig$weekstart
                )
              )
            ),
            selectInput("date_daysdisabled", lang$adminMode$widgets$date$daysDisabled,
              choices = langSpecificWidget$weekdays,
              selected = rv$widgetConfig$daysofweekdisabled, multiple = TRUE
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "date_autoclose",
                lang$adminMode$widgets$date$autoclose,
                rv$widgetConfig$autoclose
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "widget_hcube",
                lang$adminMode$widgets$date$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          dateInput("date_preview",
            label = widgetlabel,
            value = rv$widgetConfig$value, min = rv$widgetConfig$min, max = rv$widgetConfig$max,
            format = rv$widgetConfig$format, startview = rv$widgetConfig$startview,
            weekstart = rv$widgetConfig$weekstart, daysofweekdisabled = rv$widgetConfig$daysofweekdisabled,
            autoclose = rv$widgetConfig$autoclose
          )
        })
      },
      daterange = {
        rv$widgetConfig <- list(
          widgetType = "daterange",
          format = if (length(currentConfig$format)) currentConfig$format else "yyyy-mm-dd",
          startview = if (length(currentConfig$startview)) currentConfig$startview else "month",
          weekstart = if (length(currentConfig$weekstart)) currentConfig$weekstart else 0L,
          separator = if (length(currentConfig$separator)) currentConfig$separator else " to ",
          autoclose = if (identical(currentConfig$autoclose, FALSE)) FALSE else TRUE,
          noHcube = isTRUE(currentConfig$noHcube)
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig[["start"]] <- currentConfig[["start"]]
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        rv$widgetConfig$end <- currentConfig$end
        rv$widgetConfig$min <- currentConfig$min
        rv$widgetConfig$max <- currentConfig$max

        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$daterange$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$daterange$defaultStartlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  conditionalPanel(
                    condition = "input.date_start_off!==true",
                    dateInput("date_start", lang$adminMode$widgets$daterange$defaultStart,
                      value = rv$widgetConfig$start
                    )
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$div(
                    class = "shiny-input-container",
                    checkboxInput_MIRO(
                      "date_start_off",
                      lang$adminMode$widgets$daterange$startOff,
                      is.null(rv$widgetConfig$start)
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$daterange$defaultEndlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  conditionalPanel(
                    condition = "input.date_end_off!==true",
                    dateInput("date_end", lang$adminMode$widgets$daterange$defaultEnd,
                      value = rv$widgetConfig$end
                    )
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$div(
                    class = "shiny-input-container",
                    checkboxInput_MIRO(
                      "date_end_off",
                      lang$adminMode$widgets$daterange$endOff,
                      is.null(rv$widgetConfig$end)
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$daterange$minlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  conditionalPanel(
                    condition = "input.date_min_off!==true",
                    dateInput("date_min", lang$adminMode$widgets$daterange$min, value = rv$widgetConfig$min)
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$div(
                    class = "shiny-input-container",
                    checkboxInput_MIRO(
                      "date_min_off",
                      lang$adminMode$widgets$daterange$minOff,
                      is.null(rv$widgetConfig$min)
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block",
              tags$label(lang$adminMode$widgets$daterange$maxlabel),
              tags$div(
                class = "conditional",
                tags$div(
                  class = "col-sm-8",
                  conditionalPanel(
                    condition = "input.date_max_off!==true",
                    dateInput("date_max", lang$adminMode$widgets$daterange$max,
                      value = rv$widgetConfig$max
                    )
                  )
                ),
                tags$div(
                  class = "col-sm-4",
                  tags$div(
                    class = "shiny-input-container",
                    checkboxInput_MIRO(
                      "date_max_off",
                      lang$adminMode$widgets$daterange$maxOff,
                      is.null(rv$widgetConfig$max)
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container highlight-block conditional",
              tags$div(
                class = "col-sm-8 no-padding-left",
                conditionalPanel(
                  condition = "input.date_format_custom_selector!==true",
                  selectInput("date_format", lang$adminMode$widgets$daterange$format,
                    choices = dateFormatChoices,
                    selected = if (rv$widgetConfig$format %in% dateFormatChoices) {
                      rv$widgetConfig$format
                    } else {
                      NULL
                    }
                  )
                ),
                conditionalPanel(
                  condition = "input.date_format_custom_selector===true",
                  textInput("date_format_custom", lang$adminMode$widgets$daterange$formatCustom,
                    value = rv$widgetConfig$format
                  )
                )
              ),
              tags$div(
                class = "col-sm-4",
                tags$div(
                  class = "shiny-input-container",
                  checkboxInput_MIRO(
                    "date_format_custom_selector",
                    lang$adminMode$widgets$daterange$formatCustomSelector,
                    !rv$widgetConfig$format %in% dateFormatChoices
                  )
                )
              )
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                selectInput("date_startview", lang$adminMode$widgets$daterange$startview$label,
                  choices = langSpecificWidget$startview,
                  selected = rv$widgetConfig$startview
                )
              ),
              tags$div(
                class = "two-col-right",
                selectInput("date_weekstart", lang$adminMode$widgets$daterange$weekstart$label,
                  choices = langSpecificWidget$weekdays,
                  selected = rv$widgetConfig$weekstart
                )
              )
            ),
            tags$div(
              class = "option-wrapper",
              textInput("date_separator", lang$adminMode$widgets$daterange$separator,
                value = rv$widgetConfig$separator
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "date_autoclose",
                lang$adminMode$widgets$daterange$autoclose,
                rv$widgetConfig$autoclose
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_SIMPLE(
                "widget_hcube",
                lang$adminMode$widgets$daterange$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          dateRangeInput("daterange_preview",
            label = widgetlabel,
            start = rv$widgetConfig$start, end = rv$widgetConfig$end,
            min = rv$widgetConfig$min, max = rv$widgetConfig$max,
            format = rv$widgetConfig$format, startview = rv$widgetConfig$startview,
            weekstart = rv$widgetConfig$weekstart, separator = rv$widgetConfig$separator,
            autoclose = rv$widgetConfig$autoclose
          )
        })
      },
      textinput = {
        rv$widgetConfig <- list(
          widgetType = "textinput",
          value = if (length(currentConfig$value)) currentConfig$value else "",
          placeholder = if (length(currentConfig$placeholder)) currentConfig$placeholder else "",
          clearValue = isTRUE(currentConfig$clearValue)
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        singletonSetId <- NA_integer_
        if (scalarsFileName %in% names(modelInRaw)) {
          singletonSetId <- match(currentWidgetSymbolName, modelInRaw[[scalarsFileName]]$symnames)[1L]
        }
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$textinput$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                textInput("widget_value", lang$adminMode$widgets$textinput$value, value = rv$widgetConfig$value)
              ),
              tags$div(
                class = "two-col-right",
                textInput("text_placeholder", lang$adminMode$widgets$textinput$placeholder, value = rv$widgetConfig$placeholder)
              )
            ),
            if (!is.na(singletonSetId) &&
              identical(modelInRaw[[scalarsFileName]]$symtypes[singletonSetId], "set")) {
              tags$div(
                class = "shiny-input-container two-col-wrapper",
                tags$div(
                  class = "two-col-left",
                  checkboxInput_SIMPLE("widget_clearValue", labelTooltip(
                    lang$adminMode$widgets$textinput$clearValue,
                    lang$adminMode$widgets$textinput$clearValueTooltip,
                    "https://gams.com/miro/widgets.html#textbox-option-clearvalue"
                  ),
                  value = rv$widgetConfig$clearValue
                  )
                )
              )
            }
          ),
          where = "beforeEnd"
        )

        output$widget_preview <- renderUI({
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          textInput("textinput_preview", widgetlabel,
            value = rv$widgetConfig$value,
            placeholder = rv$widgetConfig$placeholder
          )
        })
      },
      numericinput = {
        rv$widgetConfig <- list(
          widgetType = "numericinput",
          value = if (length(currentConfig$value) && !is.character(currentConfig$value)) currentConfig$value,
          min = if (length(currentConfig$min) && !is.character(currentConfig$min)) currentConfig$min,
          max = if (length(currentConfig$max) && !is.character(currentConfig$max)) currentConfig$max,
          decimal = if (length(currentConfig[["decimal"]])) currentConfig[["decimal"]] else 0L,
          decimalCharacter = if (length(currentConfig[["decimalCharacter"]])) currentConfig[["decimalCharacter"]] else ".",
          digitGroupSeparator = if (length(currentConfig[["digitGroupSeparator"]])) currentConfig[["digitGroupSeparator"]] else ",",
          sign = if (length(currentConfig$sign)) currentConfig$sign else NULL
        )
        if (setAlias) {
          rv$widgetConfig$alias <- widgetAlias
        }
        rv$widgetConfig$label <- checkLength(TRUE, currentConfig$label, widgetAlias)
        rv$widgetConfig$tooltip <- checkLength(TRUE, currentConfig$tooltip, NULL)
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$numericinput$label, value = rv$widgetConfig$label),
              textInput("widget_tooltip", lang$adminMode$widgets$ui$tooltip, value = rv$widgetConfig$tooltip)
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                numericInput("numericinput_min", lang$adminMode$widgets$numericinput$min,
                  value = if (is.numeric(rv$widgetConfig$min)) rv$widgetConfig$min
                )
              ),
              tags$div(
                class = "two-col-right",
                numericInput("numericinput_max", lang$adminMode$widgets$numericinput$max,
                  value = if (is.numeric(rv$widgetConfig$max)) rv$widgetConfig$max
                )
              )
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                numericInput("numericinput_value", lang$adminMode$widgets$numericinput$value,
                  value = if (is.numeric(rv$widgetConfig$value)) rv$widgetConfig$value
                )
              ),
              tags$div(
                class = "two-col-right",
                numericInput("numericinput_decimal", lang$adminMode$widgets$numericinput$decimal,
                  min = 0,
                  value = if (is.numeric(rv$widgetConfig$decimal)) rv$widgetConfig$decimal else 0L
                )
              )
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                selectInput("numericinput_decimalCharacter", lang$adminMode$widgets$numericinput$decimalCharacter,
                  choices = langSpecificWidget$decimalCharacterChoices,
                  selected = rv$widgetConfig$decimalCharacter
                )
              ),
              tags$div(
                class = "two-col-right",
                selectInput("numericinput_digitGroupSeparator", lang$adminMode$widgets$numericinput$digitGroupSeparator,
                  choices = langSpecificWidget$digitGroupSeparatorChoices,
                  selected = if (identical(rv$widgetConfig$digitGroupSeparator, "")) "empty" else rv$widgetConfig$digitGroupSeparator
                )
              )
            ),
            tags$div(
              class = "shiny-input-container two-col-wrapper",
              tags$div(
                class = "two-col-left",
                textInput("numericinput_sign", lang$adminMode$widgets$numericinput$sign, value = rv$widgetConfig$sign)
              )
            )
          ),
          where = "beforeEnd"
        )

        output$widget_preview <- renderUI({
          widgetlabel <- rv$widgetConfig$label
          if (!is.null(rv$widgetConfig$tooltip)) {
            widgetlabel <- labelTooltip(widgetlabel, rv$widgetConfig$tooltip)
          }
          autoNumericInput("numericinput_preview",
            label = widgetlabel,
            value = rv$widgetConfig$value,
            min = rv$widgetConfig$min,
            max = rv$widgetConfig$max,
            sign = rv$widgetConfig$sign,
            decimal = rv$widgetConfig[["decimal"]],
            decimalCharacter = rv$widgetConfig[["decimalCharacter"]],
            digitGroupSeparator = rv$widgetConfig$digitGroupSeparator
          )
        })
      },
      table = {
        if (currentWidgetSymbolName %in% names(configJSON$inputWidgets)) {
          configuredTable <<- TRUE
        } else {
          configuredTable <<- FALSE
        }
        pivotCols <<- NULL
        if (length(inputSymHeaders[[input$widget_symbol]]) > 2L) {
          numericHeaders <- vapply(modelIn[[input$widget_symbol]]$headers,
            function(header) identical(header$type, "numeric"),
            logical(1L),
            USE.NAMES = FALSE
          )
          if (sum(numericHeaders) <= 1L) {
            pivotCols <<- inputSymHeaders[[input$widget_symbol]][!numericHeaders]
          } else {
            isGamsTable <<- TRUE
          }
        }
        if (identical(currentConfig$tableType, "pivot")) {
          rv$widgetConfig <- list(
            widgetType = "table",
            tableType = "pivot",
            label = currentConfig$label,
            options = checkLength(configuredTable, currentConfig[["options"]], list())
          )
          rv$widgetConfig$options$input <- TRUE
        } else if (identical(currentConfig$tableType, "bigdata") || isTRUE(currentConfig$bigData)) {
          rv$widgetConfig <- list(
            widgetType = "table",
            tableType = "bigdata",
            label = currentConfig$label,
            readonly = checkLength(configuredTable, currentConfig[["readonly"]], FALSE),
            pivotCols = checkLength(configuredTable, currentConfig[["pivotCols"]], "_")
          )
        } else {
          rv$widgetConfig <- list(
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
        currentColFormatConfig <<- rv$widgetConfig$colFormat
        output$widget_preview <- renderUI("")
        insertUI(selector = "#widget_options", getSymbolHotOptions(), where = "beforeEnd")
        if (identical(rv$widgetConfig$tableType, input$inputTable_type)) {
          # need to trigger observer as it is lazy..
          refreshTableType(refreshSameSymbol = identical(currentWidgetSymbolName, input$widget_symbol))
        }
      }
    )
  }
)

getSymbolHotOptions <- function() {
  tagList(
    tags$div(
      class = "option-wrapper",
      textAreaInput("table_label", lang$adminMode$widgets$table$label,
        value = rv$widgetConfig$label
      ),
      selectInput("inputTable_type", lang$adminMode$widgets$table$type,
        choices = setNames(
          c("default", "bigdata", "pivot"),
          lang$adminMode$widgets$table$typeChoices
        ),
        selected = if (length(rv$widgetConfig$tableType)) {
          rv$widgetConfig$tableType
        } else if (isTRUE(rv$widgetConfig$bigData)) {
          "bigdata"
        } else {
          "default"
        }
      )
    ),
    conditionalPanel(
      condition = "input.inputTable_type==='pivot'",
      getMIROPivotOptions(rv$widgetConfig$options, prefix = "inputpivot_"),
      tags$div(
        class = "config-message shiny-input-container",
        style = "display:block;",
        lang$adminMode$graphs$miroPivotOptions$infoMsgDummyData
      )
    ),
    conditionalPanel(
      condition = "input.inputTable_type!=='pivot'",
      tags$div(
        class = "shiny-input-container",
        checkboxInput_SIMPLE("table_readonly", lang$adminMode$widgets$table$readonly, value = rv$widgetConfig$readonly)
      ),
      tags$div(
        class = "option-wrapper shiny-input-container",
        style = if (!length(pivotCols)) "display:none",
        selectInput("table_pivotCols", lang$adminMode$widgets$table$pivotCols,
          choices = c(`_` = "_", pivotCols),
          selected = if (length(rv$widgetConfig$pivotCols)) rv$widgetConfig$pivotCols else "_"
        )
      )
    ),
    conditionalPanel(
      condition = paste0("input.inputTable_type==='default' && ((input.table_pivotCols!=null && input.table_pivotCols!=='_')
                                        || ", tolower(isGamsTable), ")"),
      tags$div(
        class = "option-wrapper shiny-input-container",
        checkboxInput_SIMPLE("table_fixedColumnsLeft", lang$adminMode$widgets$table$fixedColumnsLeft,
          value = if (length(rv$widgetConfig$fixedColumnsLeft)) TRUE else FALSE
        )
      )
    ),
    conditionalPanel(
      condition = paste0("input.inputTable_type==='default' && ((input.table_pivotCols==null || input.table_pivotCols==='_')
                                        ||", tolower(isGamsTable), ")"),
      tags$div(
        class = "option-wrapper shiny-input-container",
        selectInput("table_readonlyCols", lang$adminMode$widgets$table$readonlyCols,
          choices = inputSymHeaders[[input$widget_symbol]],
          selected = rv$widgetConfig$readonlyCols, multiple = TRUE
        )
      ),
      tags$div(
        class = "option-wrapper shiny-input-container",
        lapply(names(modelIn[[input$widget_symbol]]$headers), function(headerName) {
          if (identical(modelIn[[input$widget_symbol]]$headers[[headerName]]$type, "numeric")) {
            defaultVal <- "2"
            if (length(rv$widgetConfig$colFormat) &&
              headerName %in% names(rv$widgetConfig$colFormat)) {
              if (identical(
                rv$widgetConfig$colFormat[[headerName]]$format,
                "0,0a"
              )) {
                defaultVal <- "0"
              } else {
                defaultVal <- as.character(nchar(strsplit(rv$widgetConfig$colFormat[[headerName]]$format, ".",
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
                modelIn[[input$widget_symbol]]$headers[[headerName]]$alias
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
        class = "option-wrapper shiny-input-container",
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
          value = if (!identical(rv$widgetConfig$colWidths, "custom")) {
            rv$widgetConfig$colWidths
          } else {
            NULL
          }, min = 0, step = 1
        ),
        tags$div(
          id = "customColWidths", class = "config-message", style = if (identical(rv$widgetConfig$colWidths, "custom")) "display:block;",
          lang$adminMode$widgets$table$customColWidths
        )
      ),
      tags$div(
        class = "option-wrapper shiny-input-container",
        checkboxInput_SIMPLE("table_hideIndexCol",
          lang$adminMode$widgets$table$hideIndexCol,
          value = rv$widgetConfig$hideIndexCol
        ),
        checkboxInput_SIMPLE("table_heatmap",
          lang$adminMode$widgets$table$heatmap,
          value = rv$widgetConfig$heatmap
        )
      ),
      tags$div(
        class = "shiny-input-container",
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
refreshTableType <- function(refreshSameSymbol = FALSE) {
  labelTmp <- rv$widgetConfig$label
  if (identical(input$inputTable_type, "bigdata")) {
    rv$widgetConfig$tableType <- "bigdata"
    hideEl(session, "#pivotColsRestriction")
    hideEl(session, "#inputTable_pivot-data")
    if (refreshSameSymbol) {
      rv$widgetConfig <<- list(
        widgetType   = "table",
        tableType    = "bigdata",
        readonly     = input$table_readonly,
        pivotCols    = input$table_pivotCols
      )
    } else {
      rv$widgetConfig <- list(
        widgetType = "table",
        tableType = "bigdata",
        readonly = checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]][["readonly"]], FALSE),
        pivotCols = checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]][["pivotCols"]], "_")
      )
    }
    if (length(labelTmp)) {
      rv$widgetConfig$label <- labelTmp
    }
  } else if (identical(input$inputTable_type, "pivot")) {
    rv$widgetConfig$tableType <- "pivot"
    hideEl(session, "#pivotColsRestriction")
    showEl(session, "#inputTable_pivot-data")
    for (el in ls(envir = inputPivotRendererEnv)) {
      if ("Observer" %in% class(inputPivotRendererEnv[[el]])) {
        inputPivotRendererEnv[[el]]$destroy()
      }
    }
    pivotOptions <- list()

    if (currentWidgetSymbolName %in% inputSymMultiDimChoices &&
      currentWidgetSymbolName %in% names(configJSON$inputWidgets) &&
      length(configJSON$inputWidgets[[currentWidgetSymbolName]][["options"]])) {
      pivotOptions <- configJSON$inputWidgets[[currentWidgetSymbolName]][["options"]]
    }
    pivotOptions$input <- TRUE
    pivotOptions$enableHideEmptyCols <- TRUE
    pivotOptions$emptyUEL <- rv$widgetConfig$options$emptyUEL

    metadata <- list(
      headers = modelIn[[currentWidgetSymbolName]]$headers,
      symtype = modelIn[[currentWidgetSymbolName]]$symtype,
      symname = currentWidgetSymbolName
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
      data = createTableData(currentWidgetSymbolName, createColNames = TRUE)$data, rendererEnv = inputPivotRendererEnv,
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
    rv$widgetConfig$tableType <- "default"
    hideEl(session, "#inputTable_pivot-data")
    if (refreshSameSymbol) {
      rv$widgetConfig <<- list(
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
      numericColsTmp <- sum(vapply(modelIn[[input$widget_symbol]]$headers,
        function(header) identical(header$type, "numeric"),
        logical(1L),
        USE.NAMES = FALSE
      ))
      if (isTRUE(input$table_fixedColumnsLeft)) {
        if (isGamsTable) {
          fixedColumnsLeftTmp <- length(inputSymHeaders[[input$widget_symbol]]) - numericColsTmp
        } else {
          fixedColumnsLeftTmp <- length(inputSymHeaders[[input$widget_symbol]]) - numericColsTmp - 1L
        }
        rv$widgetConfig$fixedColumnsLeft <<- fixedColumnsLeftTmp
      }
      if (length(currentColFormatConfig)) {
        rv$widgetConfig$colFormat <- currentColFormatConfig
      }
    } else {
      rv$widgetConfig <- list(
        widgetType = "table",
        tableType = "default",
        readonly = checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]][["readonly"]], FALSE),
        readonlyCols = checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]][["readonlyCols"]], NULL),
        colWidths = if (configuredTable && length(configJSON$inputWidgets[[currentWidgetSymbolName]][["colWidths"]]) &&
          length(configJSON$inputWidgets[[currentWidgetSymbolName]][["colWidths"]]) > 1) {
          "custom"
        } else {
          checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]][["colWidths"]], NULL)
        },
        hideIndexCol = checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]]$hideIndexCol, FALSE),
        heatmap = checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]]$heatmap, FALSE),
        pivotCols = checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]]$pivotCols, "_")
      )
      rv$widgetConfig$fixedColumnsLeft <- checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]]$fixedColumnsLeft, NULL)
      rv$widgetConfig$colFormat <- checkLength(configuredTable, configJSON$inputWidgets[[currentWidgetSymbolName]]$colFormat, NULL)
      currentColFormatConfig <<- NULL
    }
    if (length(labelTmp)) {
      rv$widgetConfig$label <- labelTmp
    }
    if (!identical(rv$widgetConfig$pivotCols, "_") &&
      (isTRUE(rv$widgetConfig$readonly) || isTRUE(rv$widgetConfig$heatmap))) {
      showEl(session, "#pivotColsRestriction")
    } else {
      hideEl(session, "#pivotColsRestriction")
    }
  }
}
observeEvent(input$inputTable_type, {
  refreshTableType(refreshSameSymbol = TRUE)
})
observeEvent(input$inputpivot_emptyUEL, {
  if (!identical(input$inputTable_type, "pivot")) {
    return()
  }
  if (identical(input$inputpivot_emptyUEL, "")) {
    rv$widgetConfig$options$emptyUEL <- NULL
  } else {
    rv$widgetConfig$options$emptyUEL <- input$inputpivot_emptyUEL
  }
  refreshTableType(refreshSameSymbol = FALSE)
})
observeEvent(c(input$table_pivotCols, input$table_readonly, input$table_heatmap), {
  if (!identical(input$table_pivotCols, "_") &&
    (isTRUE(input$table_readonly) || isTRUE(input$table_heatmap))) {
    showEl(session, "#pivotColsRestriction")
  } else {
    hideEl(session, "#pivotColsRestriction")
  }
})
observeEvent(input$inputpivot_enableHideEmptyCols, {
  if (isTRUE(input$inputpivot_enableHideEmptyCols)) {
    showEl(session, "#inputTable_pivot-miroPivot-hideEmptyCols")
  } else {
    updateCheckboxInput(session, "inputTable_pivot-miroPivot-hideEmptyCols", value = FALSE)
    hideEl(session, "#inputTable_pivot-miroPivot-hideEmptyCols")
  }
})

observeEvent(input$table_hideIndexCol, {
  rv$widgetConfig$hideIndexCol <<- input$table_hideIndexCol
})
observeEvent(input$table_readonly, {
  rv$widgetConfig$readonly <<- input$table_readonly
})
observeEvent(input$table_label, {
  if (nchar(input$table_label)) {
    rv$widgetConfig$label <<- input$table_label
  } else {
    rv$widgetConfig$label <<- NULL
  }
})
observeEvent(input$table_readonlyCols, ignoreNULL = FALSE, {
  if (!length(input$table_readonlyCols)) {
    configJSON$widgetConfig$readonlyCols <<- NULL
    return()
  }
  rv$widgetConfig$readonlyCols <<- input$table_readonlyCols
})
observeEvent(input$table_colWidths, ignoreNULL = FALSE, {
  if (!identical(rv$widgetConfig$colWidths, "custom") &&
    (is.null(input$table_colWidths) || is.na(input$table_colWidths) || input$table_colWidths == 0)) {
    configJSON$widgetConfig$colWidths <<- NULL
    rv$widgetConfig$colWidths <<- NULL
    return()
  }
  rv$widgetConfig$colWidths <<- input$table_colWidths
  hideEl(session, "#customColWidths")
})
observeEvent(input$table_pivotCols, {
  rv$widgetConfig$pivotCols <<- input$table_pivotCols
  if (!identical(input$table_pivotCols, "_")) {
    rv$widgetConfig$readonlyCols <<- NULL
    numericColsTmp <- sum(vapply(modelIn[[input$widget_symbol]]$headers,
      function(header) identical(header$type, "numeric"),
      logical(1L),
      USE.NAMES = FALSE
    ))
    if (isGamsTable) {
      fixedColumnsLeftTmp <- length(inputSymHeaders[[input$widget_symbol]]) - numericColsTmp
    } else {
      fixedColumnsLeftTmp <- length(inputSymHeaders[[input$widget_symbol]]) - numericColsTmp - 1L
    }
    rv$widgetConfig$fixedColumnsLeft <<- fixedColumnsLeftTmp
  } else {
    rv$widgetConfig$readonlyCols <<- input$table_readonlyCols
    rv$widgetConfig$fixedColumnsLeft <<- NULL
  }
})
observeEvent(input$table_fixedColumnsLeft, {
  if (isFALSE(input$table_fixedColumnsLeft)) {
    rv$widgetConfig$fixedColumnsLeft <<- NULL
    return()
  }
  numericColsTmp <- sum(vapply(modelIn[[input$widget_symbol]]$headers,
    function(header) identical(header$type, "numeric"),
    logical(1L),
    USE.NAMES = FALSE
  ))
  if (isGamsTable) {
    fixedColumnsLeftTmp <- length(inputSymHeaders[[input$widget_symbol]]) - numericColsTmp
  } else {
    fixedColumnsLeftTmp <- length(inputSymHeaders[[input$widget_symbol]]) - numericColsTmp - 1L
  }
  rv$widgetConfig$fixedColumnsLeft <<- fixedColumnsLeftTmp
})
observeEvent(input$table_heatmap, {
  rv$widgetConfig$heatmap <<- input$table_heatmap
})
observeEvent(input$table_colDecimals, {
  noDecimals <- suppressWarnings(as.integer(input$table_colDecimals$val))
  if (is.na(noDecimals)) {
    noDecimals <- 2L
  }
  if (identical(noDecimals, 2L)) {
    if (length(rv$widgetConfig$colFormat)) {
      rv$widgetConfig$colFormat[[input$table_colDecimals$id]] <<- NULL
    }
    if (!length(rv$widgetConfig$colFormat)) {
      rv$widgetConfig$colFormat <- NULL
    }
  } else {
    if (!length(rv$widgetConfig$colFormat)) {
      rv$widgetConfig$colFormat <<- list()
    }
    rv$widgetConfig$colFormat[[input$table_colDecimals$id]] <- list(format = if (noDecimals > 0) paste0("0,0.", strrep("0", noDecimals)) else "0,0a")
  }
  currentColFormatConfig <<- rv$widgetConfig$colFormat
})

#######
observeEvent(input$widget_label, {
  if (nchar(input$widget_label)) {
    rv$widgetConfig$label <<- input$widget_label
  } else {
    rv$widgetConfig$label <<- NULL
  }
})
observeEvent(input$widget_tooltip, {
  if (nchar(input$widget_tooltip)) {
    rv$widgetConfig$tooltip <<- input$widget_tooltip
  } else {
    rv$widgetConfig$tooltip <<- NULL
  }
})
observeEvent(input$widget_multiple, {
  rv$widgetConfig$multiple <<- input$widget_multiple
})
observeEvent(input$widget_clearValue, {
  rv$widgetConfig$clearValue <<- input$widget_clearValue
})
observeEvent(input$widget_hcube, {
  rv$widgetConfig$noHcube <<- !input$widget_hcube
})
observeEvent(input$slider_min, {
  if (!is.numeric(input$slider_min)) {
    return()
  }
  rv$widgetConfig$min <<- input$slider_min
})
observeEvent(input$slider_min_dep, {
  updateSelectInputNoClear(session, "slider_min_dep_header", choices = inputSymHeaders[[input$slider_min_dep]])
})
observeEvent(input$slider_max, {
  if (!is.numeric(input$slider_max)) {
    return()
  }
  rv$widgetConfig$max <<- input$slider_max
})
observeEvent(input$slider_max_dep, {
  updateSelectInputNoClear(session, "slider_max_dep_header", choices = inputSymHeaders[[input$slider_max_dep]])
})
observeEvent(input$slider_def, {
  if (is.na(input$slider_def)) {
    configJSON$widgetConfig$default <<- NULL
  }
  if (is.numeric(input$slider_def)) {
    rv$widgetConfig$default <<- input$slider_def
  } else {
    rv$widgetConfig$default <<- NULL
  }
})
observeEvent(input$slider_def_dep, {
  updateSelectInputNoClear(session, "slider_def_dep_header", choices = inputSymHeaders[[input$slider_def_dep]])
})
observeEvent(input$slider_def1, {
  if (!is.numeric(input$slider_def1)) {
    return()
  }
  rv$widgetConfig$default[1] <<- input$slider_def1
})
observeEvent(input$slider_def2, {
  if (!is.numeric(input$slider_def2)) {
    return()
  }
  rv$widgetConfig$default[2] <<- input$slider_def2
})
observeEvent(input$slider_step, {
  if (!is.numeric(input$slider_step)) {
    return()
  }
  rv$widgetConfig$step <<- input$slider_step
})
observeEvent(input$slider_minStep, {
  if (!is.numeric(input$slider_minStep)) {
    rv$widgetConfig$minStep <<- NULL
    return()
  }
  rv$widgetConfig$minStep <<- input$slider_minStep
})
observeEvent(input$slider_ticks, {
  if (!is.logical(input$slider_ticks)) {
    return()
  }
  rv$widgetConfig$ticks <<- input$slider_ticks
})
observeEvent(input$dd_choices, ignoreNULL = FALSE, {
  rv$widgetConfig$choices <<- input$dd_choices
  if (!length(input$dd_choices)) {
    choicestmp <- character(0)
  } else {
    choicestmp <- input$dd_choices
  }
  updateSelectInputNoClear(session, "dd_default", choices = choicestmp)
})
observeEvent(input$dd_choice_dep, {
  if (identical(input$dd_choice_dep, "")) {
    updateSelectInputNoClear(session, "dd_choice_dep_header",
      choices = allInputSymHeaders
    )
  } else {
    updateSelectInputNoClear(session, "dd_choice_dep_header",
      choices = inputSymHeaders[[input$dd_choice_dep]]
    )
  }
})
observe({
  if (!length(input$dd_aliases)) {
    isolate(rv$widgetConfig$aliases <<- NULL)
  } else {
    isolate(rv$widgetConfig$aliases <<- input$dd_aliases)
  }
})
observeEvent(input$dd_default, {
  if (!nchar(input$dd_default)) {
    rv$widgetConfig$selected <<- NULL
    return()
  }
  rv$widgetConfig$selected <<- input$dd_default
})
observeEvent(input$dd_choice_dep_selector, {
  if (isTRUE(input$dd_choice_dep_selector)) {
    rv$widgetConfig$choices <<- input$dd_choices
    rv$widgetConfig$aliases <<- input$dd_aliases
    rv$widgetConfig$selected <<- input$dd_default
  } else {
    rv$widgetConfig$choices <<- paste0(
      "$", input$dd_choice_dep,
      if (nchar(input$dd_choice_dep)) {
        "$"
      },
      input$dd_choice_dep_header,
      if (isTRUE(input$dd_choice_dep_type)) {
        "$"
      }
    )
    rv$widgetConfig$aliases <<- NULL
    rv$widgetConfig$selected <<- NULL
  }
})
observeEvent(input$widget_value, {
  rv$widgetConfig$value <<- input$widget_value
})

observeEvent(input$date_default, {
  rv$widgetConfig$value <<- input$date_default
})
observeEvent(input$date_def_off, {
  if (input$date_def_off) {
    rv$widgetConfig$value <<- NULL
  } else {
    rv$widgetConfig$value <<- input$date_default
  }
})
observeEvent(input$date_start, {
  rv$widgetConfig[["start"]] <<- input$date_start
})
observeEvent(input$date_start_off, {
  if (input$date_start_off) {
    rv$widgetConfig[["start"]] <<- NULL
  } else {
    rv$widgetConfig[["start"]] <<- input$date_start
  }
})
observeEvent(input$date_end, {
  rv$widgetConfig$end <<- input$date_end
})
observeEvent(input$date_end_off, {
  if (input$date_end_off) {
    rv$widgetConfig$end <<- NULL
  } else {
    rv$widgetConfig$end <<- input$date_end
  }
})
observeEvent(input$date_min, {
  rv$widgetConfig$min <<- input$date_min
})
observeEvent(input$date_min_off, {
  if (isTRUE(input$date_min_off)) {
    rv$widgetConfig$min <<- NULL
  } else {
    rv$widgetConfig$min <<- input$date_min
  }
})
observeEvent(input$date_max, {
  rv$widgetConfig$max <<- input$date_max
})
observeEvent(input$date_max_off, {
  if (input$date_max_off) {
    rv$widgetConfig$max <<- NULL
  } else {
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
  if (input$date_format_custom_selector) {
    rv$widgetConfig$format <<- input$date_format_custom
  } else {
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
  if (!length(input$date_daysdisabled)) {
    isolate(rv$widgetConfig$daysofweekdisabled <<- NULL)
  } else {
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
observeEvent(input$numericinput_value, ignoreNULL = FALSE, {
  if (!is.numeric(input$numericinput_value)) {
    rv$widgetConfig$value <<- NULL
    return()
  }
  rv$widgetConfig$value <<- input$numericinput_value
})
observeEvent(input$numericinput_min, ignoreNULL = FALSE, {
  if (!is.numeric(input$numericinput_min)) {
    rv$widgetConfig$min <<- NULL
    return()
  }
  rv$widgetConfig$min <<- input$numericinput_min
})
observeEvent(input$numericinput_max, ignoreNULL = FALSE, {
  if (!is.numeric(input$numericinput_max)) {
    rv$widgetConfig$max <<- NULL
    return()
  }
  rv$widgetConfig$max <<- input$numericinput_max
})
observeEvent(input$numericinput_decimal, {
  if (!is.numeric(input$numericinput_decimal)) {
    rv$widgetConfig$decimal <<- NULL
    return()
  }
  rv$widgetConfig$decimal <<- input$numericinput_decimal
})
observeEvent(input$numericinput_sign, {
  if (!nchar(input$numericinput_sign)) {
    rv$widgetConfig$sign <<- NULL
    return()
  }
  rv$widgetConfig$sign <<- input$numericinput_sign
})
observeEvent(input$numericinput_decimalCharacter, {
  rv$widgetConfig$decimalCharacter <<- input$numericinput_decimalCharacter
})
observeEvent(input$numericinput_digitGroupSeparator, {
  digitGroupSeparatorTmp <<- input$numericinput_digitGroupSeparator
  if (identical(input$numericinput_digitGroupSeparator, "empty")) {
    digitGroupSeparatorTmp <<- ""
  }
  rv$widgetConfig$digitGroupSeparator <<- digitGroupSeparatorTmp
})

#  ==============================
#          SAVE JSON
#  ==============================
observeEvent(input$saveWidget, {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)
  if (rv$widgetConfig$widgetType %in% c("slider", "sliderrange")) {
    if (isFALSE(input$slider_min_dep_selector)) {
      rv$widgetConfig$min <<- paste0(
        input$slider_min_dep_op, "(", input$slider_min_dep,
        "$", input$slider_min_dep_header, ")"
      )
    } else {
      rv$widgetConfig$min <<- input$slider_min
    }
    if (isFALSE(input$slider_max_dep_selector)) {
      rv$widgetConfig$max <<- paste0(
        input$slider_max_dep_op, "(", input$slider_max_dep,
        "$", input$slider_max_dep_header, ")"
      )
    } else {
      rv$widgetConfig$max <<- input$slider_max
    }
    if (identical(rv$widgetConfig$widgetType, "slider") &&
      identical(length(rv$widgetConfig$default), 1L)) {
      if (isFALSE(input$slider_def_dep_selector)) {
        rv$widgetConfig$default <<- paste0(
          input$slider_def_dep_op, "(", input$slider_def_dep,
          "$", input$slider_def_dep_header, ")"
        )
      } else {
        rv$widgetConfig$default <<- input$slider_def
      }
    }
  } else if (rv$widgetConfig$widgetType %in% c("dropdown", "multidropdown")) {
    if (isFALSE(input$dd_choice_dep_selector)) {
      if (isTRUE(input$dd_choice_dep_type)) {
        rv$widgetConfig$choices <<- paste0(
          "$", input$dd_choice_dep,
          if (nchar(input$dd_choice_dep)) {
            "$"
          },
          input$dd_choice_dep_header, "$"
        )
      } else {
        rv$widgetConfig$choices <<- paste0(
          "$", input$dd_choice_dep,
          if (nchar(input$dd_choice_dep)) {
            "$"
          },
          input$dd_choice_dep_header
        )
      }
    }
  }
  errMsg <- validateWidgetConfig(rv$widgetConfig)
  if (nchar(errMsg)) {
    showHideEl(session, "#widgetValidationErr", 5000L, errMsg)
    return()
  }
  rv$saveWidgetConfirm <- rv$saveWidgetConfirm + 1L
})
observeEvent(virtualActionButton(input$saveWidgetConfirm, rv$saveWidgetConfirm), {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)

  configJSON$inputWidgets[[currentWidgetSymbolName]] <<- rv$widgetConfig
  if (!length(configJSON$inputWidgets[[currentWidgetSymbolName]]$readonlyCols)) {
    configJSON$inputWidgets[[currentWidgetSymbolName]]$readonlyCols <<- NULL
  }
  if (identical(configJSON$inputWidgets[[currentWidgetSymbolName]]$pivotCols, "_")) {
    configJSON$inputWidgets[[currentWidgetSymbolName]]$pivotCols <<- NULL
  }
  if (configJSON$inputWidgets[[currentWidgetSymbolName]]$widgetType %in% c("dropdown", "multidropdown") &&
    isTRUE(input$dd_choice_dep_selector)) {
    configJSON$inputWidgets[[currentWidgetSymbolName]]$
      choices <- gsub("$", "$$",
      configJSON$inputWidgets[[currentWidgetSymbolName]]$choices,
      fixed = TRUE
    )
    if (length(configJSON$inputWidgets[[currentWidgetSymbolName]]$aliases)) {
      configJSON$inputWidgets[[currentWidgetSymbolName]]$
        aliases <- gsub("$", "$$",
        configJSON$inputWidgets[[currentWidgetSymbolName]]$aliases,
        fixed = TRUE
      )
    }
  }
  # table
  if (currentWidgetSymbolName %in% inputSymMultiDimChoices) {
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
      if (length(rv$widgetConfig$label)) {
        newConfig$label <- rv$widgetConfig$label
      }
      if (length(rv$widgetConfig$options$emptyUEL)) {
        newConfig$options$emptyUEL <- rv$widgetConfig$options$emptyUEL
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
      configJSON$inputWidgets[[currentWidgetSymbolName]] <<- newConfig
    } else {
      if (identical(rv$widgetConfig$tableType, configJSON$inputWidgets[[currentWidgetSymbolName]]$tableType)) {
        widgetconfigTmp <- configJSON$inputWidgets[[currentWidgetSymbolName]]
        for (key in names(rv$widgetConfig)) {
          widgetconfigTmp[[key]] <- rv$widgetConfig[[key]]
        }
      } else {
        widgetconfigTmp <- rv$widgetConfig
      }
      configJSON$inputWidgets[[currentWidgetSymbolName]] <<- widgetconfigTmp
      if (!length(configJSON$inputWidgets[[currentWidgetSymbolName]]$readonlyCols)) {
        configJSON$inputWidgets[[currentWidgetSymbolName]]$readonlyCols <<- NULL
      }
      if (!length(configJSON$inputWidgets[[currentWidgetSymbolName]]$colWidths)) {
        configJSON$inputWidgets[[currentWidgetSymbolName]]$colWidths <<- NULL
      }
      if (is.null(configJSON$inputWidgets[[currentWidgetSymbolName]]$pivotCols) ||
        identical(configJSON$inputWidgets[[currentWidgetSymbolName]]$pivotCols, "_")) {
        configJSON$inputWidgets[[currentWidgetSymbolName]]$pivotCols <<- NULL
      } else {
        configJSON$inputWidgets[[currentWidgetSymbolName]]$colFormat <<- NULL
      }
    }
  }
  symbolDDNeedsUpdate <- FALSE
  if (any(startsWith(currentWidgetSymbolName, prefixDDPar))) {
    allInputSymbols[[lang$adminMode$widgets$ui$widgetCategorieDoubleDash]] <<- c(
      allInputSymbols[[lang$adminMode$widgets$ui$widgetCategorieDoubleDash]],
      setNames(
        currentWidgetSymbolName,
        paste0("--", substring(currentWidgetSymbolName, nchar(prefixDDPar) + 1L))
      )
    )
    symbolDDNeedsUpdate <- TRUE
  } else if (any(startsWith(currentWidgetSymbolName, prefixGMSOpt))) {
    allInputSymbols[[lang$adminMode$widgets$ui$widgetCategorieOpt]] <<- c(
      allInputSymbols[[lang$adminMode$widgets$ui$widgetCategorieOpt]],
      setNames(
        currentWidgetSymbolName,
        substring(currentWidgetSymbolName, nchar(prefixGMSOpt) + 1L)
      )
    )
    symbolDDNeedsUpdate <- TRUE
  } else if (currentWidgetSymbolName %in% scalarInputSymWithAliases) {
    if (all(scalarInputSymWithAliases %in% names(configJSON$inputWidgets))) {
      allInputSymbols <<- allInputSymbols[!allInputSymbols %in% scalarsFileName]
      if (scalarsFileName %in% names(configJSON$inputWidgets)) {
        configJSON$inputWidgets[[scalarsFileName]] <<- NULL
      }
      symbolDDNeedsUpdate <- TRUE
    } else {
      hideEl(session, "#noSymbolMsg")
      hideEl(session, "#noWidgetMsg")
      hideEl(session, "#noWidgetConfigMsg")
      hideEl(session, "#externalConfigMsg")
      hideEl(session, "#pivotColsRestriction")
    }
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

  if (symbolDDNeedsUpdate) {
    updateSelectInput(session, "widget_symbol", choices = allInputSymbols)
  }
  if (any(startsWith(currentWidgetSymbolName, c(prefixDDPar, prefixGMSOpt)))) {
    updateTextInput(session, paste0("widget_", input$widget_symbol_type), value = "")
  }
  if (noWidgetSymbols) {
    hideEl(session, "#noSymbolMsg")
    hideEl(session, "#noWidgetMsg")
    noWidgetSymbols <<- FALSE
  }
  if (isNonSingletonSet(currentWidgetSymbolName)) {
    # need to remove set from overwrite sheet order as it is no longer a widget
    tabId <- match(currentWidgetSymbolName, inputTabs)
    if (!is.na(tabId)) {
      inputTabs <<- inputTabs[-tabId]
      newSheetOrder <- inputTabs
      if (length(input$general_overwriteSheetOrderInput)) {
        newSheetOrder <- inputTabs[order(match(inputTabs, input$general_overwriteSheetOrderInput))]
      }
      updateSelectInput(session, "general_overwriteSheetOrderInput",
        choices = newSheetOrder, selected = newSheetOrder
      )
    }
  }
  removeModal()
  hideEl(session, "#noWidgetConfigMsg")
  showHideEl(session, "#widgetUpdateSuccess", 4000L)
  updateTabsetPanel(session, "widget_symbol_type", "gams")
})
observeEvent(input$deleteWidget, {
  req(length(input$widget_symbol) > 0L, nchar(input$widget_symbol) > 0L)

  showModal(modalDialog(
    title = lang$adminMode$widgets$removeDialog$title, lang$adminMode$widgets$removeDialog$message,
    footer = tagList(
      modalButton(lang$adminMode$widgets$removeDialog$cancel),
      actionButton("deleteWidgetConfirm", lang$adminMode$widgets$removeDialog$confirm)
    )
  ))
})
observeEvent(input$deleteWidgetConfirm, {
  req(length(currentWidgetSymbolName) > 0L, nchar(currentWidgetSymbolName) > 0L)

  configJSON$inputWidgets[[currentWidgetSymbolName]] <<- NULL
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  if (any(startsWith(currentWidgetSymbolName, c(prefixDDPar, prefixGMSOpt)))) {
    allInputSymbols <<- allInputSymbols[!allInputSymbols %in% currentWidgetSymbolName]
    updateSelectInput(session, "widget_symbol", choices = allInputSymbols)
  } else if (currentWidgetSymbolName %in% scalarInputSymWithAliases) {
    showEl(session, "#noWidgetConfigMsg")
  }
  if (isNonSingletonSet(currentWidgetSymbolName)) {
    # need to add set to overwrite sheet order as it is no longer a widget
    tabId <- match(currentWidgetSymbolName, inputTabs)
    if (is.na(tabId)) {
      inputTabs <<- c(
        inputTabs,
        inputSymMultiDim[match(currentWidgetSymbolName, inputSymMultiDim)]
      )
      newSheetOrder <- inputTabs
      if (length(input$general_overwriteSheetOrderInput)) {
        newSheetOrder <- inputTabs[order(match(inputTabs, input$general_overwriteSheetOrderInput))]
      }
      updateSelectInput(session, "general_overwriteSheetOrderInput",
        choices = newSheetOrder, selected = newSheetOrder
      )
    }
  }
  removeModal()
  showHideEl(session, "#widgetUpdateSuccess", 4000L)
  hideEl(session, "#noSymbolMsg")
  hideEl(session, "#noWidgetMsg")
  showEl(session, "#noWidgetConfigMsg")
  hideEl(session, "#externalConfigMsg")
  hideEl(session, "#pivotColsRestriction")
  if (!length(allInputSymbols)) {
    if (!noWidgetSymbols) {
      showEl(session, "#noSymbolMsg")
      showEl(session, "#noWidgetMsg")
      noWidgetSymbols <<- TRUE
    }
    currentWidgetSymbolName <<- character(0L)
  }
})
