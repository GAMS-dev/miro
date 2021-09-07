latest_widget_symbol_type <- NULL
currentWidgetSymbolName <- character(0L)

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

if (length(widgetSymbols)) {
  updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
  noWidgetSymbols <- FALSE
} else {
  showEl(session, "#noSymbolMsg")
  showEl(session, "#noWidgetMsg")
  hideEl(session, "#noWidgetConfigMsg")
  hideEl(session, "#optionConfigMsg")
  hideEl(session, "#doubledashConfigMsg")
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
    {
      return(lang$adminMode$widgets$validate$val35)
    }
  )
  return("")
}

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
    hideEl(session, "#optionConfigMsg")
    hideEl(session, "#doubledashConfigMsg")
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
        showElReplaceTxt(
          session, "#optionConfigMsg",
          sprintf(
            lang$adminMode$widgets$ui$optionConfigMsg,
            substring(currentWidgetSymbolName, nchar(prefixGMSOpt) + 1L)
          )
        )
        widgetOptions <- langSpecificWidget$widgetOptionsGo
      }
      if (startsWith(currentWidgetSymbolName, prefixDDPar)) {
        showElReplaceTxt(
          session, "#doubledashConfigMsg",
          sprintf(
            lang$adminMode$widgets$ui$doubledashConfigMsg,
            substring(currentWidgetSymbolName, nchar(prefixDDPar) + 1L)
          )
        )
        widgetOptions <- langSpecificWidget$widgetOptionsAll
      }
    } else if (input$widget_symbol %in% names(modelInRaw)) {
      currentWidgetSymbolName <<- input$widget_symbol
      if (!currentWidgetSymbolName %in% names(configJSON$inputWidgets)) {
        showEl(session, "#noWidgetConfigMsg")
      }
      if (isNonSingletonSet(input$widget_symbol)) {
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
    hideEl(session, "#optionConfigMsg")
    hideEl(session, "#noSymbolMsg")
    hideEl(session, "#noWidgetMsg")
    hideEl(session, "#doubledashConfigMsg")
    hideEl(session, "#noWidgetConfigMsg")
    hideEl(session, "#deleteWidget")
    updateTextInput(session, "widget_label", value = "")
    if (identical(input$widget_symbol_type, "dd")) {
      updateTextInput(session, "widget_dd", value = "")
      updateSelectInput(session, "widget_type",
        choices =
          langSpecificWidget$widgetOptionsAll
      )
    } else if (identical(input$widget_symbol_type, "go")) {
      updateTextInput(session, "widget_go", value = "")
      updateSelectInput(session, "widget_type",
        choices =
          langSpecificWidget$widgetOptionsGo
      )
    }
    if (!length(latest_widget_symbol_type)) {
      latest_widget_symbol_type <<- input$widget_symbol_type
      return()
    }
    if (latest_widget_symbol_type %in% c("dd", "go")) {
      if (identical(input$widget_symbol_type, "go") &&
        identical(latest_widget_symbol_type, "dd") &&
        startsWith(prefixDDPar, currentWidgetSymbolName)) {
        currentWidgetSymbolName <<- prefixGMSOpt %+% substr(
          currentWidgetSymbolName,
          nchar(prefixDDPar) + 1L,
          nchar(currentWidgetSymbolName)
        )
      }
      if (identical(input$widget_symbol_type, "dd") &&
        identical(latest_widget_symbol_type, "go") &&
        startsWith(prefixGMSOpt, currentWidgetSymbolName)) {
        currentWidgetSymbolName <<- prefixDDPar %+% substr(
          currentWidgetSymbolName,
          nchar(prefixGMSOpt) + 1L,
          nchar(currentWidgetSymbolName)
        )
      }
    }
    if (identical(input$widget_symbol_type, "go")) {
      currentWidgetSymbolName <<- prefixGMSOpt %+% tolower(input$widget_go)
    } else if (identical(input$widget_symbol_type, "dd")) {
      currentWidgetSymbolName <<- prefixDDPar %+% tolower(input$widget_dd)
    }
    latest_widget_symbol_type <<- input$widget_symbol_type
    return()
  } else if (!length(widgetSymbols)) {
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
    if (currentWidgetSymbolName %in% names(configJSON$inputWidgets)) {
      currentConfig <- configJSON$inputWidgets[[currentWidgetSymbolName]]
    }
    widgetAlias <- ""
    if (length(currentConfig$alias) && nchar(currentConfig$alias)) {
      widgetAlias <- currentConfig$alias
      setAlias <- TRUE
    } else {
      setAlias <- FALSE
      widgetSymbolID <- match(input$widget_symbol, widgetSymbols)
      if (!is.na(widgetSymbolID)) {
        widgetAlias <- names(widgetSymbols)[[widgetSymbolID]]
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
              textInput("widget_label", lang$adminMode$widgets$slider$label, value = rv$widgetConfig$label)
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
              checkboxInput_MIRO(
                "slider_ticks", lang$adminMode$widgets$slider$ticks,
                rv$widgetConfig$ticks
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_MIRO(
                "widget_hcube", lang$adminMode$widgets$slider$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          sliderInput("slider_preview", rv$widgetConfig$label,
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
              textInput("widget_label", lang$adminMode$widgets$sliderrange$label, value = rv$widgetConfig$label)
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
              checkboxInput_MIRO(
                "slider_ticks", lang$adminMode$widgets$sliderrange$ticks,
                rv$widgetConfig$ticks
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_MIRO(
                "widget_hcube",
                lang$adminMode$widgets$sliderrange$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          sliderInput("slider_preview", rv$widgetConfig$label,
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
              textInput("widget_label", lang$adminMode$widgets$dropdown$label, value = rv$widgetConfig$label)
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
                checkboxInput_MIRO(
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
                checkboxInput_MIRO(
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
              checkboxInput_MIRO(
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
          selectInput("dropdown_preview", rv$widgetConfig$label,
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
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$checkbox$label, value = rv$widgetConfig$label)
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_MIRO(
                "widget_value",
                lang$adminMode$widgets$checkbox$default,
                rv$widgetConfig$value
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_MIRO(
                "widget_hcube",
                lang$adminMode$widgets$checkbox$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )

        output$widget_preview <- renderUI({
          tagList(
            checkboxInput_MIRO(
              "checkbox_preview",
              rv$widgetConfig$label,
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
        rv$widgetConfig$min <- currentConfig$min
        rv$widgetConfig$max <- currentConfig$max
        rv$widgetConfig$daysofweekdisabled <- currentConfig$daysofweekdisabled
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$date$label, value = rv$widgetConfig$label)
            ),
            tags$div(
              class = "shiny-input-container",
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
              class = "shiny-input-container",
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
              class = "shiny-input-container",
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
              class = "shiny-input-container conditional",
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
              checkboxInput_MIRO(
                "date_autoclose",
                lang$adminMode$widgets$date$autoclose,
                rv$widgetConfig$autoclose
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_MIRO(
                "widget_hcube",
                lang$adminMode$widgets$date$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          dateInput("date_preview",
            label = rv$widgetConfig$label,
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
        rv$widgetConfig$end <- currentConfig$end
        rv$widgetConfig$min <- currentConfig$min
        rv$widgetConfig$max <- currentConfig$max

        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$daterange$label, value = rv$widgetConfig$label)
            ),
            tags$div(
              class = "shiny-input-container",
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
              class = "shiny-input-container",
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
              class = "shiny-input-container",
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
              class = "shiny-input-container",
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
              class = "shiny-input-container conditional",
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
              checkboxInput_MIRO(
                "date_autoclose",
                lang$adminMode$widgets$daterange$autoclose,
                rv$widgetConfig$autoclose
              )
            ),
            tags$div(
              class = "shiny-input-container",
              checkboxInput_MIRO(
                "widget_hcube",
                lang$adminMode$widgets$daterange$hcube,
                !rv$widgetConfig$noHcube
              )
            )
          ),
          where = "beforeEnd"
        )
        output$widget_preview <- renderUI({
          dateRangeInput("daterange_preview",
            label = rv$widgetConfig$label,
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
        singletonSetId <- NA_integer_
        if (scalarsFileName %in% names(modelInRaw)) {
          singletonSetId <- match(currentWidgetSymbolName, modelInRaw[[scalarsFileName]]$symnames)[1L]
        }
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$textinput$label, value = rv$widgetConfig$label)
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
                  checkboxInput_MIRO("widget_clearValue", labelTooltip(
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
          textInput("textinput_preview", rv$widgetConfig$label,
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
        insertUI(
          selector = "#widget_options",
          tagList(
            tags$div(
              class = "shiny-input-container",
              textInput("widget_label", lang$adminMode$widgets$numericinput$label, value = rv$widgetConfig$label)
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
          autoNumericInput("numericinput_preview",
            label = rv$widgetConfig$label,
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
      # in case a table was configured for a set
      table = {
        insertUI(
          selector = "#widget_options",
          tags$div(
            class = "shiny-input-container config-no-hide",
            paste0(lang$adminMode$widgets$ui$tableWidget, lang$adminMode$uiR$table, "'!")
          ),
          where = "beforeEnd"
        )
        disableEl(session, "#saveWidget")
        disableEl(session, "#deleteWidget")
        output$widget_preview <- renderUI({
        })
      }
    )
    # hideEl(session, "#hot_preview")
  }
)
observeEvent(input$widget_label, {
  if (nchar(input$widget_label)) {
    rv$widgetConfig$label <<- input$widget_label
  } else {
    rv$widgetConfig$label <<- NULL
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

  symbolDDNeedsUpdate <- FALSE

  if (any(startsWith(currentWidgetSymbolName, c(prefixDDPar, prefixGMSOpt)))) {
    widgetSymbols <<- c(widgetSymbols, setNames(currentWidgetSymbolName, currentWidgetSymbolName))
    symbolDDNeedsUpdate <- TRUE
  } else if (currentWidgetSymbolName %in% scalarInputSymWithAliases) {
    if (all(scalarInputSymWithAliases %in% names(configJSON$inputWidgets))) {
      widgetSymbols <<- widgetSymbols[widgetSymbols != scalarsFileName]
      if (scalarsFileName %in% names(configJSON$inputWidgets)) {
        configJSON$inputWidgets[[scalarsFileName]] <<- NULL
      }
      symbolDDNeedsUpdate <- TRUE
    } else {
      hideEl(session, "#noSymbolMsg")
      hideEl(session, "#noWidgetMsg")
      hideEl(session, "#noWidgetConfigMsg")
      hideEl(session, "#optionConfigMsg")
      hideEl(session, "#doubledashConfigMsg")
    }
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

  if (symbolDDNeedsUpdate) {
    updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
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
    widgetSymbols <<- widgetSymbols[widgetSymbols != currentWidgetSymbolName]
    updateSelectInput(session, "widget_symbol", choices = widgetSymbols)
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
  hideEl(session, "#optionConfigMsg")
  hideEl(session, "#doubledashConfigMsg")
  if (!length(widgetSymbols)) {
    if (!noWidgetSymbols) {
      showEl(session, "#noSymbolMsg")
      showEl(session, "#noWidgetMsg")
      noWidgetSymbols <<- TRUE
    }
    currentWidgetSymbolName <<- character(0L)
  }
})
