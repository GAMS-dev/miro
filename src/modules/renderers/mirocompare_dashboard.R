dashboardCompareOutput <- function(id, height = NULL, options = NULL, path = NULL, ...) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "dashboard-css custom-compare",
      fluidRow(
        class = "outer-row",
        column(12,
          class = "custom-grid-right",
          fluidRow(
            class = "display-flex valueboxes",
            uiOutput(ns("valueboxesTitle")),
            uiOutput(ns("valueboxes"),
              class = "miro-dashboard-comparison-valueboxes-wrapper"
            )
          )
        ),
        column(12,
          id = ns("dataView"), class = "custom-grid-left padding-custom",
          fluidRow(
            uiOutput(ns("dataViews")),
          )
        )
      )
    )
  )
}

renderDashboardCompare <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, ...) {
  ns <- session$ns

  removeTableHeader <- function(viewData) {
    numericColumnNames <- names(Filter(is.numeric, viewData))

    if (length(numericColumnNames) > 1) {
      viewData <- viewData %>%
        pivot_longer(
          cols = all_of(numericColumnNames),
          names_to = "Hdr",
          values_to = "value"
        )
    }

    return(viewData)
  }

  combineData <- function(data, scenarioNames) {
    nonEmptyData <- Filter(function(df) nrow(df) > 0, data)

    if (length(nonEmptyData) == 0) {
      return(NULL)
    }

    numericColumnNames <- names(Filter(is.numeric, data[[1]]))

    # GAMS Tables need to be lengthened to only have one value column
    # Remove sandbox scenario
    dataTmp <- data[-1]
    names(dataTmp) <- scenarioNames
    combinedData <- bind_rows(dataTmp, .id = "_scenName")

    if (length(numericColumnNames) > 1) {
      combinedData <- removeTableHeader(combinedData)
    }



    return(combinedData)
  }

  scenarioNames <- bind_rows(data$getMetadata()[-1])[["_sname"]]

  dataViewsConfig <- options$dataViewsConfig

  prepareData <- function(config, viewData) {
    dataTmp <- viewData

    filterIndexList <- names(config$filter)
    aggFilterIndexList <- names(config$aggregations)
    colFilterIndexList <- names(config$cols)
    filterIndexList <- c(filterIndexList, aggFilterIndexList, colFilterIndexList)

    filterElements <- vector("list", length(filterIndexList))
    names(filterElements) <- filterIndexList
    multiFilterIndices <- c()

    for (filterIndex in filterIndexList) {
      filterElements[[filterIndex]] <- sort(unique(dataTmp[[filterIndex]]))
      optionId <- "filter"
      if (filterIndex %in% aggFilterIndexList) {
        optionId <- "aggregations"
      } else if (filterIndex %in% colFilterIndexList) {
        optionId <- "cols"
      }
      filterVal <- config[[optionId]][[filterIndex]]

      if (!any(filterVal %in% filterElements[[filterIndex]])) {
        if (filterIndex %in% c(aggFilterIndexList, colFilterIndexList)) {
          # nothing selected = no filter for aggregations/cols
          next
        }
        filterVal <- filterElements[[filterIndex]][1]
      }
      if (any(is.na(match(filterIndex, names(dataTmp))))) {
        flog.warn(
          "Attempt to tamper with the app detected! User entered: '%s' as filter index",
          filterIndex
        )
        stop("Attempt to tamper with the app detected!", call. = FALSE)
      }
      if (length(filterVal) > 1L) {
        multiFilterIndices <- c(multiFilterIndices, filterIndex)
        filterExpression <- paste0(
          ".[[\"", filterIndex, "\"]]%in%c('",
          paste(gsub("'", "\\'", filterVal, fixed = TRUE), collapse = "','"),
          "')"
        )
      } else {
        filterExpression <- paste0(
          ".[[\"", filterIndex, "\"]]=='",
          gsub("'", "\\'", filterVal, fixed = TRUE),
          "'"
        )
      }
      dataTmp <- dataTmp %>% filter(
        !!rlang::parse_expr(filterExpression)
      )
    }


    rowIndexList <- config$rows
    aggregationFunction <- config$aggregationFunction
    if (is.null(rowIndexList)) {
      rowIndexList <- character(0)
    }
    rowIndexList <- c(
      rowIndexList,
      multiFilterIndices[!multiFilterIndices %in% c(aggFilterIndexList, colFilterIndexList)]
    )
    valueColName <- names(dataTmp)[length(dataTmp)]
    if (length(aggFilterIndexList)) {
      if (identical(aggregationFunction, "")) {
        aggregationFunction <- "count"
      } else if (length(aggregationFunction) != 1L ||
        !aggregationFunction %in% c("sum", "count", "min", "max", "mean", "median", "sd")) {
        flog.warn(
          "Attempt to tamper with the app detected! User entered: '%s' as aggregation function.",
          aggregationFunction
        )
        stop("Attempt to tamper with the app detected!", call. = FALSE)
      }
      valueColName <- names(dataTmp)[length(dataTmp)]
      if (!identical(valueColName, "value")) {
        names(dataTmp)[length(dataTmp)] <- "value"
      }
      dataTmp <- dataTmp %>%
        group_by(!!!rlang::syms(c(rowIndexList, colFilterIndexList))) %>%
        summarise(value = !!rlang::parse_expr(
          if (identical(aggregationFunction, "count")) {
            "sum(!is.na(value))"
          } else {
            paste0(aggregationFunction, "(value, na.rm = TRUE)")
          }
        ), .groups = "drop_last")
      if (!identical(valueColName, "value")) {
        names(dataTmp)[length(dataTmp)] <- valueColName
      }
    }
    if (length(rowIndexList)) {
      dataTmp <- dataTmp %>%
        select(!!!c(rowIndexList, colFilterIndexList, valueColName)) %>%
        arrange(!!!rlang::syms(rowIndexList))
    } else {
      dataTmp <- dataTmp %>% select(!!!c(colFilterIndexList, valueColName))
    }

    # apply custom labels
    if (length(config$chartOptions$customLabels)) {
      labelCols <- dataTmp[, vapply(dataTmp, class, character(1L), USE.NAMES = FALSE) == "character"]
      for (col in seq_len(length(labelCols))) {
        dataTmp[[col]] <- sapply(dataTmp[[col]], function(x) {
          if (x %in% names(config$chartOptions$customLabels)) {
            config$chartOptions$customLabels[[x]]
          } else {
            x
          }
        })
      }
    }

    userFilterData <- list()

    if (length(config$userFilter) &&
      !(length(config$userFilter) == 1 && config$userFilter %in% names(dataViewsConfig))) {
      for (filter in config$userFilter) {
        userFilterData[[filter]] <- unique(dataTmp[[filter]])
      }
    }

    if (length(colFilterIndexList)) {
      # note that names_sep is not an ASCII full stop, but UNICODE U+2024
      tryCatch(
        {
          dataTmp <- dataTmp %>%
            pivot_wider(
              names_from = !!colFilterIndexList, values_from = !!valueColName,
              names_sep = "\U2024",
              names_sort = TRUE, names_repair = "unique"
            )
        },
        warning = function(w) {
          if (grepl("list-cols", conditionMessage(w), fixed = TRUE)) {
            flog.trace("Dashboard configuration: Data contains duplicated keys and can therefore not be pivoted.")
            showErrorMsg(
              lang$renderers$miroPivot$errorTitle,
              lang$renderers$miroPivot$errPivotDuplicate
            )
          } else {
            flog.info(
              "Dashboard configuration: Unexpected warning while pivoting data. Error message: %s",
              conditionMessage(e)
            )
            showErrorMsg(
              lang$renderers$miroPivot$errorTitle,
              lang$renderers$miroPivot$errPivot
            )
          }
        },
        error = function(e) {
          flog.info(
            "Dashboard configuration: Unexpected error while pivoting data. Error message: %s",
            conditionMessage(e)
          )
          showErrorMsg(
            lang$renderers$miroPivot$errorTitle,
            lang$renderers$miroPivot$errPivot
          )
        }
      )
    }

    attr(dataTmp, "noRowHeaders") <- length(rowIndexList)
    for (filterName in names(userFilterData)) {
      attr(dataTmp, paste0("userFilterData_", filterName)) <- userFilterData[[filterName]]
    }
    return(dataTmp)
  }

  hasMultipleNumeric <- function(df) {
    numericCols <- vapply(df, is.numeric, logical(1L), USE.NAMES = FALSE)
    sum(numericCols) > 1
  }

  getData <- function(indicator) {
    noRowHeaders <- attr(dashboardChartData[[indicator]], "noRowHeaders")
    dataTmp <- dashboardChartData[[indicator]]
    if (length(dataViewsConfig[[indicator]]$decimals)) {
      dataTmp <- dataTmp %>%
        mutate(across(where(is.numeric), ~ round(., as.numeric(dataViewsConfig[[indicator]]$decimals))))
    }

    # filter user selection
    if (length(dataViewsConfig[[indicator]]$userFilter)) {
      indicatorTmp <- indicator
      if (length(dataViewsConfig[[indicator]]$userFilter) == 1 &&
        dataViewsConfig[[indicator]]$userFilter %in% names(dataViewsConfig)) {
        indicatorTmp <- dataViewsConfig[[indicator]]$userFilter
      }

      for (filterName in dataViewsConfig[[indicatorTmp]]$userFilter) {
        if (length(input[[paste0(indicatorTmp, "userFilter_", filterName)]])) {
          filterEl <- input[[paste0(indicatorTmp, "userFilter_", filterName)]]
          if (filterName %in% names(dataViewsConfig[[indicator]]$cols)) {
            dataTmp <- dataTmp %>%
              select(
                seq_len(noRowHeaders),
                (any_of(filterEl) |
                  contains(paste0("\U2024", filterEl, "\U2024")) |
                  starts_with(paste0(filterEl, "\U2024")) |
                  ends_with(paste0("\U2024", filterEl)))
              )
          } else {
            dataTmp <- dataTmp %>%
              filter(!!rlang::sym(filterName) %in% filterEl)
          }
        }
      }
    }
    return(dataTmp)
  }

  heatmapColors <- function(symbolData, noRowHeaders, heatmaptype = 1L) {
    if (heatmaptype == 1L) {
      brks <- quantile(symbolData[-seq_len(as.numeric(noRowHeaders))],
        probs = seq(.05, .95, .05), na.rm = TRUE
      )
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        {
          paste0("rgb(255,", ., ",", ., ")")
        }
      return(list(brks = brks, clrs = clrs))
    }

    # Exclude row headers
    relevantData <- symbolData[-seq_len(as.numeric(noRowHeaders))]

    # Separate positive and negative values
    positiveValues <- unlist(lapply(relevantData, function(col) col[col >= 0]), use.names = FALSE)
    negativeValues <- unlist(lapply(relevantData, function(col) col[col < 0]), use.names = FALSE)

    # Remove NA and get unique values
    positiveValues <- unique(positiveValues[!is.na(positiveValues)])
    negativeValues <- unique(negativeValues[!is.na(negativeValues)])

    # Determine extremes
    symbolLowestNegative <- if (length(negativeValues)) min(negativeValues) else if (length(positiveValues)) min(positiveValues) else 0
    symbolHighestPositive <- if (length(positiveValues)) max(positiveValues) else if (length(negativeValues)) max(negativeValues) else 0


    symbolAbsMax <- max(abs(symbolLowestNegative), abs(symbolHighestPositive))

    # Generate quantiles for positive and negative values separately
    symbolQuantilesPositive <- seq(0, symbolAbsMax, length.out = 10)
    symbolQuantilesNegative <- seq(-symbolAbsMax, 0, length.out = 10)

    brks <- unique(c(symbolQuantilesNegative, symbolQuantilesPositive))

    # Generate color values for positive values (shades of green)
    symbolPositiveColors <- round(seq(90, 50,
      length.out = length(symbolQuantilesPositive)
    ), 0) %>%
      {
        paste0("hsl(202,52%,", ., "%)")
      }

    # Generate color values for negative values (shades of red)
    symbolNegativeColors <- round(seq(50, 90,
      length.out = length(symbolQuantilesNegative)
    ), 0) %>%
      {
        paste0("hsl(34,90%,", ., "%)")
      }

    # Combine the positive and negative color sets
    clrs <- c(symbolNegativeColors, symbolPositiveColors)

    return(list(brks = brks, clrs = clrs))
  }

  applyCustomLabelsOrder <- function(data, noRowHeaders, customLabelsOrder) {
    mergedCols <- paste0("\U2024", "mergedCols")
    orderCol <- paste0(mergedCols, "\U2024")
    orderTmpCol <- paste0(orderCol, "\U2024")
    orderTibble <- tibble(
      !!mergedCols := customLabelsOrder,
      !!orderTmpCol := seq_along(customLabelsOrder)
    )

    data <- data %>%
      unite(!!mergedCols, 1:noRowHeaders, sep = "\U2024", remove = FALSE) %>%
      left_join(orderTibble, by = mergedCols) %>%
      mutate(!!orderCol := ifelse(is.na(!!sym(orderTmpCol)),
        max(!!sym(orderTmpCol), na.rm = TRUE) + row_number(),
        !!sym(orderTmpCol)
      )) %>%
      arrange(!!sym(orderCol)) %>%
      select(-all_of(c(mergedCols, orderTmpCol, orderCol)))

    return(data)
  }

  # change the column order. Will change the order of table columns/plotted series
  applyCustomSeriesOrder <- function(data, noRowHeaders, customSeriesOrder) {
    fixedCols <- colnames(data)[1:noRowHeaders]
    valueCols <- colnames(data)[(noRowHeaders + 1):ncol(data)]
    validLabels <- customSeriesOrder[customSeriesOrder %in% valueCols]
    remainingCols <- setdiff(valueCols, validLabels)
    orderedValueCols <- c(validLabels, remainingCols)
    orderedData <- data %>%
      select(all_of(fixedCols), all_of(orderedValueCols))
    return(orderedData)
  }

  dashboardChartData <- list()
  currentConfig <- c()
  for (view in names(dataViewsConfig)) {
    if (!is.list(dataViewsConfig[[view]])) {
      # custom user output
      next
    }
    # Check whether last view uses same data as current one
    if (length(currentConfig) &&
      identical(
        currentConfig[setdiff(names(currentConfig), c("pivotRenderer", "decimals"))],
        dataViewsConfig[[view]][setdiff(names(dataViewsConfig[[view]]), c("pivotRenderer", "decimals"))]
      )) {
      dashboardChartData[[view]] <- preparedData
      next
    }

    currentConfig <- dataViewsConfig[[view]]

    viewData <- combineData(data$get(currentConfig$data), scenarioNames)
    preparedData <- prepareData(currentConfig, viewData)
    dashboardChartData[[view]] <- preparedData
  }

  # Boxes for  KPIs (custom infobox)
  infoBoxCustom <-
    function(id = NULL,
             value = NULL,
             prefix = "+",
             postfix = "%",
             noColor = FALSE,
             invert = FALSE,
             title = "",
             subtitle = NULL,
             icon = shiny::icon("bar-chart"),
             color = "aqua",
             width = 12,
             href = NULL,
             fill = FALSE,
             customColor = NULL,
             noView = FALSE) {
      shinydashboard:::validateColor(color)
      shinydashboard:::tagAssert(icon, type = "i")

      colorClass <- paste0("bg-", color)
      boxContent <- div(
        class = "info-box custom-info-box",
        class = if (fill) {
          colorClass
        },
        class = if (noView) {
          "no-view"
        },
        span(
          class = "info-box-icon",
          class = if (!fill) {
            colorClass
          },
          style = if (!is.null(customColor)) {
            paste0("background-color:", customColor, "!important;")
          } else {
            ""
          },
          icon
        ),
        div(
          class = "info-box-content",
          span(
            class = "info-box-text",
            title
          ),
          if (!is.null(value)) {
            span(
              class = "info-box-number",
              style = if (is.na(suppressWarnings(as.numeric(value))) || noColor) {
                "color:''"
              } else if (!is.na(suppressWarnings(as.numeric(value))) &&
                as.numeric(value) == 0) {
                "color:''"
              } else if (!is.na(suppressWarnings(as.numeric(value))) &&
                as.numeric(value) > 0) {
                if (invert) {
                  "color:#dd4b39!important"
                } else {
                  "color:#3d9970!important"
                }
              } else if (invert) {
                "color:#3d9970!important"
              } else {
                "color:#dd4b39!important"
              },
              if (!is.na(suppressWarnings(as.numeric(value)))) {
                if (as.numeric(value) < 0 && identical(prefix, "+")) {
                  prefix <- ""
                }
                paste0(prefix, format(value, big.mark = ","), postfix)
              } else {
                "NA"
              }
            )
          },
          if (!is.null(subtitle)) {
            p(subtitle)
          }
        )
      )

      if (!is.null(href)) {
        boxContent <- a(href = href, boxContent)
      }

      div(
        class = if (!is.null(width)) {
          paste0("col-sm-", width)
        },
        `data-namespace` = ns(""),
        id = id,
        boxContent
      )
    }

  chartChoices <- setNames(
    c(
      "table", "heatmap", "pie", "doughnut", "bar", "horizontalbar",
      "stackedbar", "horizontalstackedbar", "line", "scatter", "area",
      "stackedarea", "radar", "timeseries"
    ),
    c(
      lang$renderers$miroPivot$renderer$table,
      lang$renderers$miroPivot$renderer$heatmap,
      lang$renderers$miroPivot$renderer$pie,
      lang$renderers$miroPivot$renderer$doughnut,
      lang$renderers$miroPivot$renderer$bar,
      lang$renderers$miroPivot$renderer$horizontalbar,
      lang$renderers$miroPivot$renderer$stackedbar,
      lang$renderers$miroPivot$renderer$horizontalstackedbar,
      lang$renderers$miroPivot$renderer$line,
      lang$renderers$miroPivot$renderer$scatter,
      lang$renderers$miroPivot$renderer$area,
      lang$renderers$miroPivot$renderer$stackedarea,
      lang$renderers$miroPivot$renderer$radar,
      lang$renderers$miroPivot$renderer$timeseries
    )
  )

  # Get scalar output data in case valueboxes should show a value
  if (length(options$valueBoxes$valueScalar) && any(!is.na(options$valueBoxes$valueScalar))) {
    scalarData <- NULL

    if ("_scalars_out" %in% data$getAllSymbols()) {
      scalarData <- combineData(data$get("_scalars_out"), scenarioNames) %>%
        mutate(value = suppressWarnings(as.numeric(value)))
    }
    if ("_scalarsve_out" %in% data$getAllSymbols()) {
      scalarVeData <- combineData(data$get("_scalarsve_out"), scenarioNames) %>%
        filter(Hdr == "level") %>%
        select(-Hdr)
      if (is.null(scalarData)) {
        scalarData <- scalarVeData
      } else {
        scalarData <- bind_rows(scalarData, scalarVeData)
      }
    }

    if (is.null(scalarData)) {
      abortSafe("No scalar output symbols found for valueBoxes")
    }
  }

  # Value boxes title and scenario select (if value boxes show values)
  output$valueboxesTitle <- renderUI({
    tagList(
      if (length(options$valueBoxesTitle)) {
        column(12,
          class = "col-xs-12 col-sm-12 custom-highlight-block custom-padding",
          tags$h4(options$valueBoxesTitle, class = "highlight-block")
        )
      },
      if (any(!is.na(options$valueBoxes$valueScalar))) {
        column(12,
          class = "col-xs-12 col-sm-12 custom-highlight-block custom-padding",
          tags$div(
            tags$div(
              class = "scenario-dropdown", style = "height: 30px;",
              selectizeInput(
                ns("scenarioSelect"),
                label = NULL,
                choices = setNames(
                  c("_", scenarioNames),
                  c(lang$renderers$dashboardComparison$selectScen, scenarioNames)
                ),
                options = list(onInitialize = I(paste0("function(value) {
  document.querySelector('.selectize-input input[id^=\"", ns("scenarioSelect"), "\"]').setAttribute('readonly', 'readonly');
}")))
              ),
            )
          )
        )
      }
    )
  })

  # Valueboxes output
  output$valueboxes <- renderUI({
    box_columns <- lapply(1:length(options$valueBoxes$id), function(i) {
      # Note: Modify in case (optional) valueBox values should be calculated differently
      if (is.null(options$valueBoxes$valueScalar[i]) ||
        is.na(options$valueBoxes$valueScalar[i]) ||
        is.null(input$scenarioSelect) ||
        !nzchar(input$scenarioSelect) ||
        identical(input$scenarioSelect, "_")) {
        valueTmp <- NULL
      } else {
        valueTmp <- scalarData %>%
          filter(
            `_scenName` == input$scenarioSelect,
            scalar == tolower(options$valueBoxes$valueScalar[i])
          )
        valueTmp <- as.numeric(valueTmp[[length(valueTmp)]][1])

        if (!is.na(options$valueBoxes$decimals[i])) {
          valueTmp <- round(valueTmp, digits = as.numeric(options$valueBoxes$decimals[i]))
        }
      }

      valBoxName <- options$valueBoxes$id[i]

      column(12,
        class = "box-styles col-xs-6 col-sm-6",
        infoBoxCustom(
          id = ns(valBoxName),
          value = valueTmp,
          prefix = options$valueBoxes$prefix[i],
          postfix = options$valueBoxes$postfix[i],
          noColor = options$valueBoxes$noColor[i],
          invert = options$valueBoxes$redPositive[i],
          title = options$valueBoxes$title[i],
          color = if (startsWith(options$valueBoxes$color[i], "#")) "aqua" else options$valueBoxes$color[i],
          icon = icon(options$valueBoxes$icon[i]),
          customColor = if (startsWith(options$valueBoxes$color[i], "#")) options$valueBoxes$color[i] else NULL,
          noView = if (!valBoxName %in% names(options$dataViews)) TRUE else FALSE
        )
      )
    })
    do.call(tagList, box_columns)
  })

  # Data View switch
  observeEvent(input$showChart, {
    views <- names(options$dataViews)
    boxWithoutView <- options$valueBoxes$id[!options$valueBoxes$id %in% views]

    reportToRender <- substr(input$showChart, nchar(session$ns("")) + 1L, nchar(input$showChart))
    if (reportToRender %in% boxWithoutView) {
      return()
    }
    reportToRender <- if (reportToRender %in% views) reportToRender else options$valueBoxes$id[[1]]

    for (view in views) {
      if (identical(reportToRender, view)) {
        showEl(session, paste0("#", session$ns(paste0(view, "View"))))
      } else {
        hideEl(session, paste0("#", session$ns(paste0(view, "View"))))
      }
    }
  })

  # Data views
  # names(options$dataViews) must match options$valueBoxes$id entries
  output$dataViews <- renderUI({
    sections <- lapply(names(options$dataViews), function(viewList) {
      view <- options$dataViews[[viewList]]
      if (is.null(names(view))) {
        view <- unlist(view, recursive = FALSE)
      }
      idList <- as.list(names(view))
      titleList <- view

      tags$div(
        id = ns(paste0(viewList, "View")),
        style = ifelse(viewList == options$valueBoxes$id[[1]], "", "display:none;"),
        lapply(seq_along(idList), function(i) {
          id <- idList[[i]]
          title <- titleList[[i]]

          if (is.list(dataViewsConfig[[id]])) {
            userFilter <- NULL
            if (length(dataViewsConfig[[id]]$userFilter)) {
              userFilter <- dataViewsConfig[[id]]$userFilter
            }

            column(
              width = if (length(dataViewsConfig[[id]]$colWidth)) as.numeric(dataViewsConfig[[id]]$colWidth) else 12,
              class = if (!nchar(title)) "add-margin",
              if (nchar(title)) {
                tags$h4(title, class = "highlight-block")
              },
              tags$div(
                style = "overflow:auto;",
                tags$div(
                  class = "row table-chart-wide-widgets",
                  tags$div(
                    class = "charttype-and-btn-wrapper",
                    class = if (length(userFilter) %% 2 == 0) "even-inline" else if (length(userFilter) == 1) "one-inline" else "odd-inline",
                    tags$div(
                      class = "custom-dropdown",
                      selectizeInput(ns(paste0(id, "ChartType")),
                        label = NULL,
                        choices = chartChoices,
                        selected = dataViewsConfig[[id]]$pivotRenderer,
                        options = list(onInitialize = I(paste0("function(value) {
                  document.querySelector('.selectize-input input[id^=\"", ns(paste0(id, "ChartType")), "\"]').setAttribute('readonly', 'readonly');
                }")))
                      )
                    ),
                    uiOutput(ns(paste0(id, "DownloadButtons")))
                  ),
                  if (length(userFilter) && !(length(userFilter) == 1 && userFilter %in% names(dataViewsConfig))) {
                    singleDropdownFilters <- if (!is.null(dataViewsConfig[[id]]$singleDropdown)) {
                      dataViewsConfig[[id]]$singleDropdown
                    } else {
                      character(0)
                    }

                    filterInputs <- lapply(userFilter, function(filterName) {
                      userFilterChoices <- attr(dashboardChartData[[id]], paste0("userFilterData_", filterName))
                      multiple <- if (filterName %in% singleDropdownFilters) {
                        FALSE
                      } else {
                        TRUE
                      }

                      if (multiple) {
                        userFilterChoices <- c("All" = "", userFilterChoices)
                      }

                      tags$div(
                        class = "custom-dropdown-wide user-filter",
                        class = if (length(userFilter) %% 2 == 0) "even-inline" else if (length(userFilter) == 1) "one-inline" else "odd-inline",
                        selectizeInput(ns(paste0(id, "userFilter_", filterName)),
                          label = NULL,
                          choices = userFilterChoices,
                          multiple = multiple, width = "100%",
                          options = list(onInitialize = I(paste0("function(value) {
                                         document.querySelector('.selectize-input input[id^=\"", ns(paste0(id, "userFilter_", filterName)), "\"]').setAttribute('readonly', 'readonly');
                                       }")))
                        )
                      )
                    })

                    do.call(tagList, filterInputs)
                  }
                ),
                tags$div(
                  class = "table-chart-wide-wrapper",
                  DT::DTOutput(ns(paste0(id, "Table"))),
                  tags$div(
                    id = ns(paste0(id, "ChartWrapper")), class = "dashboard-chart-wrapper",
                    style = paste0("height: ", if (length(dataViewsConfig[[id]]$height)) dataViewsConfig[[id]]$height else "33vh"),
                    chartjs::chartjsOutput(ns(paste0(id, "Chart")),
                      height = if (length(dataViewsConfig[[id]]$height)) dataViewsConfig[[id]]$height else "33vh"
                    )
                  )
                )
              )
            )
          } else {
            uiOutput(ns(id))
          }
        })
      )
    })

    do.call(tagList, sections)
  })

  # show/hide Chart for view with multi-chart layout. Show/hide table is done in renderDT() directly
  toggleChartType <- function(indicator) {
    if (input[[paste0(indicator, "ChartType")]] %in% c("table", "heatmap")) {
      hideEl(session, paste0("#", session$ns(paste0(indicator, "ChartWrapper"))))
    } else {
      showEl(session, paste0("#", session$ns(paste0(indicator, "ChartWrapper"))))
    }
  }

  lapply(names(dashboardChartData), function(indicator) {
    observeEvent(input[[paste0(indicator, "ChartType")]], {
      toggleChartType(indicator)
    })

    # table for each view
    output[[paste0(indicator, "Table")]] <- renderDT({
      if (!nrow(dashboardChartData[[indicator]]) ||
        (!input[[paste0(indicator, "ChartType")]] %in% c("table", "heatmap"))) {
        return()
      }

      dataTmp <- getData(indicator)
      noRowHeaders <- attr(dashboardChartData[[indicator]], "noRowHeaders")

      # apply custom labels order
      if (length(dataViewsConfig[[indicator]]$chartOptions$customLabelsOrder)) {
        dataTmp <- applyCustomLabelsOrder(
          dataTmp,
          noRowHeaders,
          dataViewsConfig[[indicator]]$chartOptions$customLabelsOrder
        )
      }

      # apply custom series order
      if (length(dataViewsConfig[[indicator]]$chartOptions$customSeriesOrder)) {
        dataTmp <- applyCustomSeriesOrder(
          dataTmp,
          noRowHeaders,
          dataViewsConfig[[indicator]]$chartOptions$customSeriesOrder
        )
      }

      # heatmap
      if (input[[paste0(indicator, "ChartType")]] == "heatmap") {
        if (identical(dataViewsConfig[[indicator]]$chartOptions$heatmapType, 2L)) {
          dataColors <- heatmapColors(dataTmp, noRowHeaders, 2L)
        } else {
          dataColors <- heatmapColors(dataTmp, noRowHeaders, 1L)
        }
      }

      # Table Summary
      colSummarySettings <- NULL

      nonNumericCols <- dataTmp %>%
        select(where(~ !is.numeric(.))) %>%
        names()

      fullSummaryEnabled <- identical(dataViewsConfig[[indicator]]$tableSummarySettings[["enabled"]], TRUE)
      if (fullSummaryEnabled || identical(dataViewsConfig[[indicator]]$tableSummarySettings$rowEnabled, TRUE)) {
        tablesummarySettings <- dataViewsConfig[[indicator]]$tableSummarySettings
        if (identical(tablesummarySettings$rowSummaryFunction, "sum")) {
          dataTmp <- mutate(
            ungroup(dataTmp),
            !!paste0(
              lang$renderers$miroPivot$aggregationFunctions$sum,
              "\U2003\U2003"
            ) := rowSums(dataTmp[vapply(dataTmp, is.numeric,
              logical(1L),
              USE.NAMES = FALSE
            )], na.rm = TRUE)
          )
        } else if (identical(tablesummarySettings$rowSummaryFunction, "mean")) {
          dataTmp <- mutate(
            ungroup(dataTmp),
            !!paste0(
              lang$renderers$miroPivot$aggregationFunctions$mean,
              "\U2003\U2003"
            ) := rowMeans(dataTmp[vapply(dataTmp, is.numeric,
              logical(1L),
              USE.NAMES = FALSE
            )], na.rm = TRUE)
          )
        } else {
          # count
          dataTmp <- mutate(
            ungroup(dataTmp),
            !!paste0(
              lang$renderers$miroPivot$aggregationFunctions$count,
              "\U2003\U2003"
            ) := rowSums(!is.na(dataTmp[vapply(dataTmp, is.numeric,
              logical(1L),
              USE.NAMES = FALSE
            )]))
          )
        }
      }
      if (fullSummaryEnabled || identical(dataViewsConfig[[indicator]]$tableSummarySettings$colEnabled, TRUE)) {
        tablesummarySettings <- dataViewsConfig[[indicator]]$tableSummarySettings
        colSummarySettings <- list(caption = lang$renderers$miroPivot$aggregationFunctions[[tablesummarySettings$colSummaryFunction]])
        roundPrecision <- if (length(dataViewsConfig[[indicator]]$decimals)) as.numeric(dataViewsConfig[[indicator]]$decimals) else 2L
        if (identical(tablesummarySettings$colSummaryFunction, "count")) {
          colSummarySettings$data <- round(colSums(!is.na(dataTmp[vapply(dataTmp, is.numeric,
            logical(1L),
            USE.NAMES = FALSE
          )])), digits = roundPrecision)
        } else {
          colSummarySettings$data <- round(as.numeric(slice(summarise(dataTmp, across(
            where(is.numeric),
            \(x) match.fun(tablesummarySettings$colSummaryFunction)(x, na.rm = TRUE)
          )), 1L)), digits = roundPrecision)
        }
      }

      tableObj <- DT::datatable(
        dataTmp,
        rownames = FALSE,
        container = DTbuildColHeaderContainer(
          names(dataTmp),
          noRowHeaders,
          nonNumericCols,
          colSummary = colSummarySettings
        ),
        options = list(
          paging = FALSE, dom = "t",
          scrollX = TRUE,
          scrollY = if (length(dataViewsConfig[[indicator]]$height)) dataViewsConfig[[indicator]]$height else "33vh",
          scrollCollapse = TRUE,
          columnDefs = list(list(
            className = "dt-left", targets = "_all"
          )),
          drawCallback = if (identical(input[[paste0(indicator, "ChartType")]], "heatmap")) JS('function() {$(this.api().table().body()).addClass("heatmap") }')
        )
      )

      if (!identical(input[[paste0(indicator, "ChartType")]], "heatmap")) {
        return(tableObj)
      }
      return(
        formatStyle(tableObj, seq(noRowHeaders + 1, length(dataTmp)),
          color = "#333",
          backgroundColor = styleInterval(
            dataColors$brks,
            dataColors$clrs
          )
        )
      )
    })

    customColors <- c(
      "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
      "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
      "#cab2d6", "#6a3d9a", "#ffff99", "#b15928",
      "#FCDCDB", "#f9b9b7", "#D5D3DA", "#ada9b7",
      "#e2546b", "#66101F", "#E0ABD7", "#c45ab3",
      "#8bf2fe", "#1BE7FF", "#a1d1b6", "#4C9F70",
      "#F6FAA9", "#f0f757", "#d3b499", "#9E6D42",
      "#50caf3", "#086788", "#eee49d", "#E0CA3C",
      "#dbcac7", "#BA9790", "#f69e84", "#EB4511",
      "#ccadf1", "#9B5DE5", "#A1FB8B", "#47fa1a",
      "#8dadd0", "#38618c", "#fcebea", "#fad8d6",
      "#a6b474", "#373d20", "#a248ce", "#210b2c",
      "#f37ea9", "#d81159", "#68f7f7", "#08bdbd",
      "#98feb1", "#35ff69", "#d27193", "#6d213c",
      "#edfab1", "#dcf763", "#feb46f", "#e06c00",
      "#f3ebaa", "#e9d758", "#c0c7c7", "#829191",
      "#f3cac5", "#E8998D", "#c7dac9", "#91b696",
      "#BE99A4", "#714955", "#7c7ccf", "#2a2a72",
      "#7efee0", "#00ffc5", "#c28eb1", "#6c3a5c",
      "#df7192", "#8b1e3f", "#95D86B", "#3E721D"
    )

    # charts output
    output[[paste0(indicator, "Chart")]] <- chartjs::renderChartjs({
      dataTmp <- getData(indicator)

      chartType <- tolower(input[[paste0(indicator, "ChartType")]])
      currentView <- dataViewsConfig[[indicator]]

      if (!nrow(dataTmp) ||
        !chartType %in%
          c(
            "line", "scatter", "area", "stackedarea", "bar",
            "stackedbar", "radar", "timeseries", "pie", "doughnut",
            "horizontalbar", "horizontalstackedbar"
          )) {
        return()
      }

      rowHeaderLen <- attr(dashboardChartData[[indicator]], "noRowHeaders")
      noSeries <- length(dataTmp) - rowHeaderLen

      # apply custom labels order
      if (length(currentView$chartOptions$customLabelsOrder)) {
        dataTmp <- applyCustomLabelsOrder(
          dataTmp,
          rowHeaderLen,
          currentView$chartOptions$customLabelsOrder
        )
      }

      # apply custom series order
      if (length(dataViewsConfig[[indicator]]$chartOptions$customSeriesOrder)) {
        dataTmp <- applyCustomSeriesOrder(
          dataTmp,
          rowHeaderLen,
          dataViewsConfig[[indicator]]$chartOptions$customSeriesOrder
        )
      }

      labels <- do.call(paste, c(dataTmp[seq_len(rowHeaderLen)], list(sep = ".")))
      if (!length(labels)) {
        labels <- "value"
      }

      currentSeriesLabels <- names(dataTmp)[seq(rowHeaderLen + 1L, noSeries + rowHeaderLen)]

      if (length(currentView$chartOptions$customChartColors) &&
        length(names(currentView$chartOptions$customChartColors))) {
        # custom chart colors specified

        colorLabels <- names(currentView$chartOptions$customChartColors)
        colorLabelsNew <- colorLabels
        if (length(currentView$chartOptions$customLabels)) {
          colorLabelsNew <- c()
          for (colorLabel in colorLabels) {
            label <- colorLabel
            if (colorLabel %in% names(currentView$chartOptions$customLabels)) {
              label <- currentView$chartOptions$customLabels[[colorLabel]]
            } else {
              labelsTmp <- strsplit(colorLabel, "\U2024")[[1]]
              labelMatch <- which(labelsTmp %in% names(currentView$chartOptions$customLabels))
              labelsTmp[labelMatch] <- unlist(currentView$chartOptions$customLabels[labelsTmp[labelMatch]])
              label <- paste(labelsTmp, collapse = "\U2024")
            }
            colorLabelsNew <- c(colorLabelsNew, label)
          }
        }

        chartColorIdx <- match(
          currentSeriesLabels,
          colorLabelsNew
        )
        chartColorsToUse <- currentView$chartOptions$customChartColors[chartColorIdx]
        chartColorsToUse[is.na(chartColorIdx)] <- list(c("#666", "#666"))
        chartColorsToUse <- unlist(chartColorsToUse, use.names = FALSE)
      } else {
        chartColorsToUse <- customColors
      }

      if (chartType %in% c(
        "line", "scatter", "area", "stackedarea",
        "timeseries"
      )) {
        chartJsObj <- chartjs(
          customColors = chartColorsToUse
        ) %>%
          cjsLine(
            labels = labels,
            xTitle = currentView$chartOptions$xTitle,
            yTitle = currentView$chartOptions$yTitle
          )
        if (identical(chartType, "scatter")) {
          chartJsObj$x$options$showLine <- FALSE
        } else if (identical(chartType, "stackedarea")) {
          chartJsObj$x$scales$y$stacked <- if (identical(currentView$chartOptions$singleStack, TRUE)) "single" else TRUE
          chartJsObj$x$options$plugins$tooltip <- list(
            mode = "index",
            position = "nearest"
          )
        } else if (identical(chartType, "timeseries")) {
          chartJsObj$x$options$normalized <- TRUE
          chartJsObj$x$options$animation <- FALSE
          chartJsObj$x$options$elements <- list(
            point = list(
              radius = 0L,
              hitRadius = 4L
            )
          )
          chartJsObj$x$options$plugins$tooltip <- list(
            mode = "index",
            position = "nearest"
          )
        }
      } else if (chartType %in% c("stackedbar", "horizontalstackedbar")) {
        chartJsObj <- chartjs(
          customColors = chartColorsToUse
        ) %>%
          cjsBar(
            labels = labels, stacked = TRUE,
            xTitle = currentView$chartOptions$xTitle,
            yTitle = currentView$chartOptions$yTitle
          )
        chartJsObj$x$scales$y$stacked <- if (identical(currentView$chartOptions$singleStack, TRUE)) "single" else TRUE
        chartJsObj$x$options$plugins$tooltip <- list(
          mode = "index",
          position = "nearest"
        )
      } else if (identical(chartType, "radar")) {
        chartJsObj <- chartjs(
          customColors = chartColorsToUse
        ) %>%
          cjsRadar(labels = labels)
      } else if (identical(chartType, "pie")) {
        chartJsObj <- chartjs(
          customColors = chartColorsToUse,
          title = currentView$chartOptions$title
        ) %>%
          cjsPie(labels = labels)
      } else if (identical(chartType, "doughnut")) {
        chartJsObj <- chartjs(
          customColors = chartColorsToUse,
          title = currentView$chartOptions$title
        ) %>%
          cjsDoughnut(labels = labels, cutout = 80)
      } else {
        chartJsObj <- chartjs(
          customColors = chartColorsToUse
        ) %>%
          cjsBar(
            labels = labels,
            xTitle = currentView$chartOptions$xTitle,
            yTitle = currentView$chartOptions$yTitle
          )
      }
      if (length(currentView$chartOptions$y2axis$series)) {
        axisType <- if (chartType %in% c("horizontalbar", "horizontalstackedbar")) "x2" else "y2"
        position <- if (axisType == "x2") "top" else "right"

        chartJsObj <- cjsAddScale(chartJsObj,
          axis = axisType,
          position = position,
          type = if (identical(currentView$chartOptions$y2axis$logScale, TRUE)) "logarithmic" else "linear",
          display = "auto",
          title = list(
            text = if (length(currentView$chartOptions$y2axis$title)) currentView$chartOptions$y2axis$title else NULL,
            display = if (length(currentView$chartOptions$y2axis$title)) TRUE else FALSE
          )
        )

        if (identical(currentView$chartOptions$y2axis$showGrid, FALSE)) {
          chartJsObj$x$scales[[axisType]]$grid$display <- FALSE
        }
        if (length(currentView$chartOptions$y2axis$min)) {
          chartJsObj$x$scales[[axisType]]$min <- currentView$chartOptions$y2axis$min
        }
        if (length(currentView$chartOptions$y2axis$max)) {
          chartJsObj$x$scales[[axisType]]$max <- currentView$chartOptions$y2axis$max
        }
      }

      if (identical(currentView$chartOptions$yLogScale, TRUE)) {
        if (chartType %in% c("horizontalbar", "horizontalstackedbar")) {
          chartJsObj$x$scales$x$type <- "logarithmic"
          chartJsObj$x$options$indexAxis <- "y"
          chartJsObj$x$scales$y$type <- "category"
        } else if (identical(chartJsObj$x$scales$y$type, "linear")) {
          chartJsObj$x$scales$y$type <- "logarithmic"
        }
      } else if (chartType %in% c("horizontalbar", "horizontalstackedbar")) {
        chartJsObj$x$scales$x$type <- "linear"
        chartJsObj$x$options$indexAxis <- "y"
        chartJsObj$x$scales$y$type <- "category"
      }

      axisType <- if (chartType %in% c("horizontalbar", "horizontalstackedbar")) "x" else "y"
      xGrid <- if (chartType %in% c("horizontalbar", "horizontalstackedbar")) "y" else "x"
      if (length(currentView$chartOptions$yMin)) {
        chartJsObj$x$scales[[axisType]]$min <- currentView$chartOptions$yMin
      }
      if (length(currentView$chartOptions$yMax)) {
        chartJsObj$x$scales[[axisType]]$max <- currentView$chartOptions$yMax
      }
      if (identical(currentView$chartOptions$showXGrid, FALSE)) {
        chartJsObj$x$scales[[xGrid]]$grid$display <- FALSE
      }
      if (identical(currentView$chartOptions$showYGrid, FALSE)) {
        chartJsObj$x$scales[[axisType]]$grid$display <- FALSE
      }
      if (identical(currentView$chartOptions$showDataMarkers, FALSE) &&
        !identical(chartType, "scatter")) {
        chartJsObj$x$options$normalized <- TRUE
        chartJsObj$x$options$animation <- FALSE
        chartJsObj$x$options$elements <- list(
          point = list(
            radius = 0L,
            hitRadius = 4L
          )
        )
        chartJsObj$x$options$plugins$tooltip <- list(
          mode = "index",
          position = "nearest"
        )
      }

      chartJsObj <- chartJsObj %>% cjsLegend()

      multiChartRenderer <- character(0)
      if (length(currentView$chartOptions$multiChartOptions$multiChartRenderer)) {
        multiChartRenderer <- currentView$chartOptions$multiChartOptions$multiChartRenderer
      }
      for (i in seq_len(noSeries)) {
        label <- names(dataTmp)[rowHeaderLen + i]
        originalLabel <- label

        if (label %in% currentView$chartOptions$customLabels) {
          originalLabel <- names(currentView$chartOptions$customLabels)[
            which(currentView$chartOptions$customLabels == label)
          ]
        }

        scaleID <- NULL
        if (length(currentView$chartOptions$y2axis$series) && originalLabel %in% currentView$chartOptions$y2axis$series) {
          if (chartType %in% c("horizontalbar", "horizontalstackedbar")) {
            scaleID <- "x2"
          } else {
            scaleID <- "y2"
          }
        }

        lineDash <- NULL
        if (length(currentView$chartOptions$customLineDashPatterns) && length(currentView$chartOptions$customLineDashPatterns[[label]])) {
          lineDash <- currentView$chartOptions$customLineDashPatterns[[label]]
        }
        borderWidth <- NULL
        if (length(currentView$chartOptions$customBorderWidths) && length(currentView$chartOptions$customBorderWidths[[label]])) {
          borderWidth <- currentView$chartOptions$customBorderWidths[[label]]
        }

        if (originalLabel %in% currentView$chartOptions$multiChartSeries) {
          if (chartType %in% c("line", "area", "stackedarea", "timeseries")) {
            multiChartRenderer <- if (length(multiChartRenderer)) multiChartRenderer else "bar"
          } else {
            multiChartRenderer <- if (length(multiChartRenderer)) multiChartRenderer else "line"
          }
          if ((multiChartRenderer %in% c("line", "scatter") || identical(chartType, "stackedarea")) &&
            !identical(currentView$chartOptions$multiChartOptions$stackMultiChartSeries, "regularStack")) {
            order <- 0
          } else {
            order <- 2
          }

          args <- list(
            chartJsObj,
            dataTmp[[rowHeaderLen + i]],
            label = label,
            type = multiChartRenderer,
            showLine = multiChartRenderer %in% c("line", "area", "stackedarea", "timeseries"),
            order = order,
            scaleID = scaleID,
            pointHitRadius = if (identical(currentView$chartOptions$multiChartOptions$showMultiChartDataMarkers, TRUE)) 1L else 0L,
            pointRadius = if (identical(currentView$chartOptions$multiChartOptions$showMultiChartDataMarkers, TRUE)) 3L else 0L,
            stack = if (identical(currentView$chartOptions$multiChartOptions$stackMultiChartSeries, "regularStack")) {
              "stack1"
            } else if (identical(currentView$chartOptions$multiChartOptions$stackMultiChartSeries, "individualStack")) {
              "stack0"
            } else {
              as.character(i)
            },
            stepped = identical(currentView$chartOptions$multiChartOptions$multiChartStepPlot, TRUE)
          )

          if (!is.null(lineDash)) {
            args$borderDash <- lineDash
          }
          if (!is.null(borderWidth)) {
            args$borderWidth <- borderWidth
          }

          chartJsObj <- do.call(cjsSeries, args)
        } else {
          fillOpacity <- if (identical(chartType, "stackedarea")) {
            if (length(currentView$chartOptions$fillOpacity)) currentView$chartOptions$fillOpacity else 1
          } else if (identical(chartType, "area")) {
            if (length(currentView$chartOptions$fillOpacity)) currentView$chartOptions$fillOpacity else 0.15
          } else {
            0.15
          }

          args <- list(
            chartJsObj,
            dataTmp[[rowHeaderLen + i]],
            label = label,
            fill = chartType %in% c("area", "stackedarea"),
            fillOpacity = fillOpacity,
            order = 1,
            scaleID = scaleID,
            stack = if (chartType %in% c("stackedarea", "stackedbar", "horizontalstackedbar")) "stack1" else NULL,
            stepped = identical(currentView$chartOptions$stepPlot, TRUE)
          )

          if (!is.null(lineDash)) {
            args$borderDash <- lineDash
          }
          if (!is.null(borderWidth)) {
            args$borderWidth <- borderWidth
          }

          chartJsObj <- do.call(cjsSeries, args)
        }
      }

      if (length(currentView$chartFontSize)) {
        chartFontSize <- isolate(as.numeric(currentView$chartFontSize))
        chartJsObj$x$options$plugins$tooltip$bodyFont$size <- chartFontSize
        chartJsObj$x$options$plugins$tooltip$titleFont$size <- chartFontSize
        chartJsObj$x$options$plugins$legend$labels$font$size <- chartFontSize

        for (scale in names(chartJsObj$x$scales)) {
          chartJsObj$x$scales[[scale]]$ticks$font$size <- chartFontSize
          chartJsObj$x$scales[[scale]]$title$font$size <- chartFontSize
        }
      }

      if (identical(currentView$chartOptions$hideLegend, TRUE)) {
        chartJsObj$x$options$plugins$legend$display <- FALSE
      }

      if (chartType %in% c("stackedbar", "stackedarea")) {
        chartJsObj$x$options$plugins$tooltip$callbacks$afterBody <- htmlwidgets::JS("function(tooltipItem, chart) {
                                                                    const rawValues = this.dataPoints.map(point => point.formattedValue);
                                                                    const allValues = rawValues.map(str => parseFloat(str.replace(/,/g, '')));
                                                                    let rowSums = allValues.reduce((acc, row) => acc + parseFloat(row), 0);
                                                                    return 'Total: ' + parseFloat(rowSums).toFixed(2);
                                                                    }")
      }

      # Fit chart to screen
      chartJsObj$x$options$maintainAspectRatio <- FALSE

      # set locale for '.' as decimal sign
      chartJsObj$x$options$locale <- "en-US"

      # enable zoom
      chartJsObj$x$options$plugins$zoom <- list(
        zoom = list(
          wheel = list(
            enabled = TRUE
          ),
          pinch = list(
            enabled = TRUE
          ),
          mode = "xy",
          scaleMode = "y"
        ),
        pan = list(
          enabled = TRUE,
          mode = "xy"
        )
      )

      return(chartJsObj)
    })

    # download buttons: png & csv
    output[[paste0(indicator, "DownloadButtons")]] <- renderUI({
      canvasId <- paste0(indicator, "Chart")

      tagList(
        tags$div(
          class = " dashboard-btn-wrapper",
          tags$a(
            id = ns(paste0(indicator, "DownloadCsv")),
            class = "btn btn-default btn-custom pivot-btn-custom shiny-download-link dashboard-btn dashboard-btn-csv",
            href = "",
            target = "_blank",
            download = NA,
            tags$div(
              tags$i(class = "fa fa-file-csv")
            ),
            title = lang$renderers$miroPivot$btDownloadCsv
          ),
          tags$a(
            id = ns(paste0(indicator, "DownloadPng")),
            class = "btn btn-default bt-export-canvas btn-custom pivot-btn-custom dashboard-btn dashboard-btn-png",
            style = if (input[[paste0(indicator, "ChartType")]] %in% c("table", "heatmap")) "display:none;",
            download = paste0(canvasId, ".png"),
            href = "#",
            `data-canvasid` = ns(canvasId),
            tags$div(
              tags$i(class = "fa fa-file-image")
            ),
            title = lang$renderers$miroPivot$btDownloadPng
          )
        )
      )
    })

    # download csv data
    output[[paste0(indicator, "DownloadCsv")]] <- downloadHandler(
      filename = paste0(indicator, ".csv"),
      content = function(file) {
        dataTmp <- getData(indicator)
        return(write_csv(dataTmp, file, na = ""))
      }
    )
  })
}
