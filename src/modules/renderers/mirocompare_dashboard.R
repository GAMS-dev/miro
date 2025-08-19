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
            # names(options$dataViews) must match options$valueBoxes$id entries
            lapply(options$valueBoxes$id, function(sectionId) {
              tags$div(id = ns(paste0("sectionContent_", sectionId)))
            })
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

  dataViewsConfig <- dashboardPreprocessDataViewsConfig(options$dataViewsConfig)

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

  activeView <- reactive({
    if (is.null(input$showChart)) {
      av <- options$valueBoxes$id[[1]]
    } else {
      av <- substr(
        input$showChart,
        nchar(session$ns("")) + 1L,
        nchar(input$showChart)
      )

      if (av %in% names(options$dataViews)) av else options$valueBoxes$id[[1]]
    }
  })

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
  rendererEnv[[ns("showChart")]] <- observeEvent(input$showChart, {
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

  # show/hide Chart for view with multi-chart layout. Show/hide table is done in renderDT() directly
  toggleChartType <- function(indicator) {
    if (input[[paste0(indicator, "ChartType")]] %in% c("table", "heatmap")) {
      hideEl(session, paste0("#", session$ns(paste0(indicator, "DownloadPng"))))
      hideEl(session, paste0("#", session$ns(paste0(indicator, "ChartWrapper"))))
    } else {
      showEl(session, paste0("#", session$ns(paste0(indicator, "DownloadPng"))))
      showEl(session, paste0("#", session$ns(paste0(indicator, "ChartWrapper"))))
    }
  }

  dashboardChartData <- setNames(
    vector("list", length(dataViewsConfig)),
    names(dataViewsConfig)
  )

  rawData <- dashboardChartData

  renderedSections <- list()

  rendererEnv[[ns("viewsToRender")]] <- observe({
    # Lazily build the entire section/data-view that corresponds to the
    # currently active dashboard view (clicked value box; activeView()):
    #   1. Leave if this section was already rendered.
    #   2. Iterate over every view in the section,
    #      fetch / transform its data and cache it (rawData, dashboardChartData),
    #      gather corresponding userFilter choices
    #   3. Insert the finished UI for the section via `dashboardRenderDataView()`.
    #   4. For every view just inserted, register:
    #        - an `observeEvent()` that toggles chart-type controls,
    #        - a `renderDT()` table,
    #        - a `renderChartjs()` chart,
    #        - CSV download handler.

    av <- req(activeView())

    if (av %in% names(renderedSections)) {
      return()
    }

    showEl(session, "#loading-screen")
    on.exit(hideEl(session, "#loading-screen"))

    currentConfig <- list()
    preparedData <- NULL

    isolate({
      views <- names(unlist(options$dataViews[[av]]))
      allUserFilterChoices <- setNames(vector("list", length(views)), views)

      for (view in views) {
        if (!view %in% names(options$dataViewsConfig) ||
          !is.list(dataViewsConfig[[view]])) {
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

        rawData[[view]] <- viewData
        preparedData <- dashboardPrepareData(currentConfig, viewData)
        dashboardChartData[[view]] <- preparedData

        # Retrieve user-defined filters to pass to the renderer
        userFilter <- NULL
        if (length(dataViewsConfig[[view]]$userFilter)) {
          userFilter <- dataViewsConfig[[view]]$userFilter
          if (!(length(userFilter) == 1 && userFilter %in% names(dataViewsConfig))) {
            singleDropdown <- if (!is.null(dataViewsConfig[[view]]$singleDropdown)) {
              dataViewsConfig[[view]]$singleDropdown
            } else {
              character(0)
            }
            allUserFilterChoices[[view]] <- setNames(
              lapply(userFilter, function(filterName) {
                choices <- attr(dashboardChartData[[view]], paste0("userFilterData_", filterName))
                if (!filterName %in% singleDropdown) {
                  choices <- c("All" = "", choices)
                }
                choices
              }),
              userFilter
            )
          }
        }
      }
    })

    removeUI(
      selector = paste0("#", ns(paste0("sectionContent_", av)), " .dashboard-section-wrapper"),
      multiple = TRUE
    )

    insertUI(
      selector = paste0("#", ns(paste0("sectionContent_", av))), where = "afterBegin",
      ui = dashboardRenderDataView(av, options, allUserFilterChoices, ns)
    )

    lapply(names(unlist(options$dataViews[[av]])), function(indicator) {
      observeEvent(input[[paste0(indicator, "ChartType")]], {
        toggleChartType(indicator)
      })

      # table for each view
      output[[paste0(indicator, "Table")]] <- renderDT({
        tableData <- dashboardChartData[[indicator]]
        if (is.null(tableData)) {
          return()
        }

        if (!nrow(tableData) ||
          (!input[[paste0(indicator, "ChartType")]] %in% c("table", "heatmap"))) {
          return()
        }

        selectedUserFilters <- NULL
        if (length(dataViewsConfig[[indicator]]$userFilter)) {
          userFilterIndicator <- dataViewsConfig[[indicator]]$.userFilterExternalSymbol
          if (is.null(userFilterIndicator)) {
            userFilterIndicator <- indicator
          }
          selectedUserFilters <- lapply(dataViewsConfig[[indicator]]$userFilter, function(fn) {
            input[[paste0(userFilterIndicator, "userFilter_", fn)]]
          })
          names(selectedUserFilters) <- dataViewsConfig[[indicator]]$userFilter
        }

        dataTmp <- dashboardGetData(indicator, dashboardChartData, dataViewsConfig, selectedUserFilters)
        noRowHeaders <- attr(tableData, "noRowHeaders")

        # heatmap
        if (input[[paste0(indicator, "ChartType")]] == "heatmap") {
          if (identical(dataViewsConfig[[indicator]]$chartOptions$heatmapType, 2L)) {
            dataColors <- dashboardHeatmapColors(dataTmp, noRowHeaders, 2L)
          } else {
            dataColors <- dashboardHeatmapColors(dataTmp, noRowHeaders, 1L)
          }
        }

        # baseline comparison
        roundPrecision <- if (length(dataViewsConfig[[indicator]]$decimals)) as.numeric(dataViewsConfig[[indicator]]$decimals) else 2L
        if (length(attr(dataTmp, "baselineComp"))) {
          tableSessionId <- stringi::stri_rand_strings(1, 10)[[1]]
          baselineCompRenderFn <- list(list(
            targets = seq(noRowHeaders + 1, length(dataTmp)) - 1L,
            render = JS(paste0(
              "function(data, type, row, meta) {
  if (type !== 'display') {
    return data;
  }
  const pm=", if (length(roundPrecision)) paste0("DTWidget.formatRound(data,", roundPrecision, ",3,',','.','0')") else "data;",
              if (length(attr(dataTmp, "baselineComp")$metricSuffix) > 1L) {
                paste0(
                  "
  const offset=(meta.row+meta.settings._iDisplayStart)+(meta.col-", noRowHeaders, ")*", nrow(dataTmp), ";
  const secondaryMetric=DTWidget.formatRound(", toJSON(attr(dataTmp, "baselineComp")$secondaryData[[".secondary"]]), "[offset],", roundPrecision, ",3,',','.','0');
  const refData=", toJSON(attr(dataTmp, "baselineComp")$secondaryData[[".primary"]]), "[offset];
  if (Math.abs(refData - data) > 1e-4 && window.alertPushed !== '", tableSessionId, "') {
    window.alertPushed = '", tableSessionId, "';
    Miro.modal('Something went wrong. Please dont trust the data! Also, please contact GAMS about this issue (id: 981273) via support@gams.com', 'OK');
  }
  return '<span class=\"miro-pivot-primary-data\">'+pm+(pm===''?'':'", attr(dataTmp, "baselineComp")$metricSuffix[[1]], "')+'</span>'+(secondaryMetric===''?'':' ('+secondaryMetric+'",
                  attr(dataTmp, "baselineComp")$metricSuffix[[2]], "'+')');}"
                )
              } else {
                paste0("return pm+(pm===''?'':'", attr(dataTmp, "baselineComp")$metricSuffix[[1]], "');}")
              }
            ))
          ))
        } else {
          baselineCompRenderFn <- NULL
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
            columnDefs = c(list(list(
              className = "dt-left", targets = "_all"
            )), baselineCompRenderFn),
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
        chartData <- dashboardChartData[[indicator]]
        if (is.null(chartData)) {
          return()
        }

        if (!nrow(chartData) ||
          (input[[paste0(indicator, "ChartType")]] %in% c("table", "heatmap"))) {
          return()
        }

        selectedUserFilters <- NULL
        if (length(dataViewsConfig[[indicator]]$userFilter)) {
          userFilterIndicator <- dataViewsConfig[[indicator]]$.userFilterExternalSymbol
          if (is.null(userFilterIndicator)) {
            userFilterIndicator <- indicator
          }
          selectedUserFilters <- lapply(dataViewsConfig[[indicator]]$userFilter, function(fn) {
            input[[paste0(userFilterIndicator, "userFilter_", fn)]]
          })
          names(selectedUserFilters) <- dataViewsConfig[[indicator]]$userFilter
        }

        dataTmp <- dashboardGetData(indicator, dashboardChartData, dataViewsConfig, selectedUserFilters)

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

        labels <- do.call(paste, c(dataTmp[seq_len(rowHeaderLen)], list(sep = ".")))
        if (!length(labels)) {
          labels <- "value"
        }

        currentSeriesLabels <- names(dataTmp)[seq(rowHeaderLen + 1L, noSeries + rowHeaderLen)]

        if (length(currentView$chartOptions$customChartColors) &&
          length(names(currentView$chartOptions$customChartColors))) {
          colorLabelsNew <- dashboardTransformLabels(
            originalLabels = names(currentView$chartOptions$customChartColors),
            customLabels = currentView$chartOptions$customLabels
          )

          numSeries <- length(currentSeriesLabels)
          colorList <- vector("list", numSeries)
          for (i in seq_len(numSeries)) {
            colorList[[i]] <- dashboardDefaultColorPair(i, customColors)
          }

          for (i in seq_along(currentSeriesLabels)) {
            seriesLab <- currentSeriesLabels[i]

            exactIdx <- which(colorLabelsNew == seriesLab)
            if (length(exactIdx) == 1) {
              colorList[[i]] <- currentView$chartOptions$customChartColors[[exactIdx]]
            } else {
              patternMatches <- which(vapply(colorLabelsNew, dashboardMatchSeriesLabel, logical(1L), label = seriesLab, USE.NAMES = FALSE))

              if (length(patternMatches) > 0) {
                chosenIdx <- patternMatches[1]
                colorList[[i]] <- currentView$chartOptions$customChartColors[[chosenIdx]]
              }
            }
          }

          chartColorsToUse <- unlist(colorList, use.names = FALSE)
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
        groupElements <- NULL
        if (length(currentView$chartOptions$groupDimension) &&
          chartType %in% c("stackedbar", "horizontalstackedbar")) {
          groupElements <- unique(rawData[[indicator]][[currentView$chartOptions$groupDimension]])
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
          if (length(currentView$chartOptions$customLineDashPatterns) &&
            length(names(currentView$chartOptions$customLineDashPatterns))) {
            transformedLineDashNames <- dashboardTransformLabels(
              originalLabels = names(currentView$chartOptions$customLineDashPatterns),
              customLabels = currentView$chartOptions$customLabels
            )

            exactIdx <- which(transformedLineDashNames == label)
            if (length(exactIdx) == 1) {
              lineDash <- currentView$chartOptions$customLineDashPatterns[[exactIdx]]
            } else {
              patternMatches <- which(vapply(transformedLineDashNames, dashboardMatchSeriesLabel, logical(1L), label = label, USE.NAMES = FALSE))
              if (length(patternMatches) > 0) {
                chosenIdx <- patternMatches[1]
                lineDash <- currentView$chartOptions$customLineDashPatterns[[chosenIdx]]
              } else {
                lineDash <- NULL
              }
            }
          }

          borderWidth <- NULL
          if (length(currentView$chartOptions$customBorderWidths) &&
            length(names(currentView$chartOptions$customBorderWidths)) > 0) {
            transformedBorderWidthNames <- dashboardTransformLabels(
              originalLabels = names(currentView$chartOptions$customBorderWidths),
              customLabels = currentView$chartOptions$customLabels
            )

            exactIdx <- which(transformedBorderWidthNames == label)
            if (length(exactIdx) == 1) {
              borderWidthCandidate <- currentView$chartOptions$customBorderWidths[[exactIdx]]
              borderWidthCandidate <- round(as.numeric(borderWidthCandidate))
              if (!is.na(borderWidthCandidate)) {
                borderWidth <- borderWidthCandidate
              }
            } else {
              patternMatches <- which(vapply(transformedBorderWidthNames, dashboardMatchSeriesLabel, logical(1L), label = label, USE.NAMES = FALSE))
              if (length(patternMatches) > 0) {
                chosenIdx <- patternMatches[1]
                borderWidthCandidate <- currentView$chartOptions$customBorderWidths[[chosenIdx]]
                borderWidthCandidate <- round(as.numeric(borderWidthCandidate))
                if (!is.na(borderWidthCandidate)) {
                  borderWidth <- borderWidthCandidate
                }
              }
            }
          }

          stack <- NULL
          if (length(groupElements) && chartType %in% c("stackedbar", "horizontalstackedbar")) {
            patternMatches <- which(vapply(groupElements, dashboardMatchSeriesLabel, logical(1L), label = originalLabel, exact = TRUE, USE.NAMES = FALSE))
            if (length(patternMatches) == 0) {
              stack <- NULL
            } else {
              stack <- paste0("stack", patternMatches[1])
            }
          } else {
            stack <- if (chartType %in% c("stackedarea", "stackedbar", "horizontalstackedbar")) "stack1" else NULL
          }

          multiChartSeries <- FALSE
          if (length(currentView$chartOptions$multiChartSeries)) {
            series <- currentView$chartOptions$multiChartSeries
            if (any(vapply(series, dashboardMatchSeriesLabel, logical(1L), label = label, exact = TRUE, USE.NAMES = FALSE))) {
              multiChartSeries <- TRUE
            }
          }
          if (multiChartSeries) {
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
                stack
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
              stack = stack,
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

      # download csv data
      output[[paste0(indicator, "DownloadCsv")]] <- downloadHandler(
        filename = paste0(indicator, ".csv"),
        content = function(file) {
          selectedUserFilters <- NULL
          if (length(dataViewsConfig[[indicator]]$userFilter)) {
            userFilterIndicator <- dataViewsConfig[[indicator]]$.userFilterExternalSymbol
            if (is.null(userFilterIndicator)) {
              userFilterIndicator <- indicator
            }
            selectedUserFilters <- lapply(dataViewsConfig[[indicator]]$userFilter, function(fn) {
              input[[paste0(userFilterIndicator, "userFilter_", fn)]]
            })
            names(selectedUserFilters) <- dataViewsConfig[[indicator]]$userFilter
          }

          dataTmp <- dashboardGetData(indicator, dashboardChartData, dataViewsConfig, selectedUserFilters)
          return(write_csv(dataTmp, file, na = ""))
        }
      )
    })

    renderedSections[[av]] <<- TRUE
  })
}
