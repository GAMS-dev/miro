dashboardOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$script(JS(paste0("$(document).on('click', '#", ns("valueboxes"), " .shiny-html-output',
              function(){
                  Shiny.setInputValue('", ns("showChart"), "',this.id,{ priority:'event'})

              })")))
    ),
    tags$div(
      class = "dashboard-css",
      fluidRow(
        class = "outer-row", style = "margin:0px;",
        column(12,
          class = "custom-grid-right",
          fluidRow(
            class = "display-flex valueboxes",
            uiOutput(ns("valueboxes"))
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

renderDashboard <- function(id, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

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
        setIndices <- names(dataTmp)[-length(dataTmp)]
        if (is.null(rowIndexList)) {
          rowIndexList <- setIndices
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
          labelCols <- dataTmp[, sapply(dataTmp, class) == "character"]
          for (i in seq_len(nrow(dataTmp))) {
            for (j in seq_len(length(labelCols))) {
              if (is.na(dataTmp[[i, j]])) {
                next
              }
              for (key in names(config$chartOptions$customLabels)) {
                if (dataTmp[[i, j]] == key) {
                  dataTmp[[i, j]] <- config$chartOptions$customLabels[[key]]
                  break
                }
              }
            }
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
        numericCols <- sapply(df, is.numeric)
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
              if (filterName %in% names(dataViewsConfig[[indicatorTmp]]$cols)) {
                dataTmp <- dataTmp %>%
                  select(
                    1:as.numeric(noRowHeaders),
                    (matches(paste0("^", filterEl, "$")) |
                      contains(paste0(filterEl, "\U2024")) |
                      contains(paste0("\U2024", filterEl)))
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

        # If no data symbol is provided, the renderer's base symbol is used.
        if (is.list(data)) {
          if (!is.null(currentConfig$data)) {
            viewData <- data[[tolower(currentConfig$data)]]
          } else {
            viewData <- data[[options[["_metadata_"]]$symname]]
          }
        } else {
          viewData <- data
        }

        # GAMS Tables need to be lengthened to only have one value column
        # as this is how a view is stored
        numericColumnNames <- viewData[sapply(viewData, is.numeric)] %>% names()

        if (length(numericColumnNames) > 1) {
          viewData <- viewData %>%
            pivot_longer(
              cols = numericColumnNames,
              names_to = "Hdr",
              values_to = "value"
            )
        }

        preparedData <- prepareData(currentConfig, viewData)
        dashboardChartData[[view]] <- preparedData
      }


      # Boxes for  KPIs (custom infobox)
      infoBoxCustom <-
        function(value = NULL,
                 prefix = "+",
                 postfix = "%",
                 noColor = FALSE,
                 invert = FALSE,
                 title,
                 subtitle = NULL,
                 icon = shiny::icon("bar-chart"),
                 color = "aqua",
                 width = 4,
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
                  if (!is.na(suppressWarnings(as.numeric(value))) &&
                    as.numeric(value) > 0) {
                    paste0(prefix, value, postfix)
                  } else if (!is.na(suppressWarnings(as.numeric(value))) &&
                    as.numeric(value) == 0) {
                    paste0("0", postfix)
                  } else if (!is.na(suppressWarnings(as.numeric(value)))) {
                    paste0(value, postfix)
                  } else {
                    value
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
            boxContent
          )
        }

      chartChoices <- c(
        "Line" = "line", "Scatter" = "scatter", "Area" = "area",
        "Stackedarea" = "stackedarea", "Bar" = "bar",
        "Stackedbar" = "stackedbar", "Radar" = "radar",
        "Timeseries" = "timeseries", "Table" = "table",
        "Heatmap" = "heatmap"
      )
      # Valueboxes output
      output$valueboxes <- renderUI({
        box_columns <- lapply(options$valueBoxes$Id, function(box_name) {
          column(12, class = "box-styles col-xs-6 col-sm-6", shinydashboard::valueBoxOutput(ns(box_name), width = 12))
        })
        if (length(options$valueBoxesTitle)) {
          tagList(
            column(12,
              class = "col-xs-12 col-sm-12 custom-highlight-block custom-padding",
              tags$h4(options$valueBoxesTitle, class = "highlight-block")
            ),
            do.call(tagList, box_columns)
          )
        } else {
          do.call(tagList, box_columns)
        }
      })

      lapply(1:length(options$valueBoxes$Id), function(i) {
        valBoxName <- options$valueBoxes$Id[i]

        if (is.na(options$valueBoxes$ValueScalar[i])) {
          valueTmp <- NULL
        } else {
          if (is.null(outputScalarsFull)) {
            abortSafe("No scalar symbols found for valueBoxes")
          }
          valueTmp <- outputScalarsFull %>%
            filter(scalar == tolower(options$valueBoxes$ValueScalar[i]))
          if (!nrow(valueTmp)) {
            abortSafe(sprintf("No scalar symbol '%s' found for valueBox '%s'", options$valueBoxes$ValueScalar[i], options$valueBoxes$Id[i]))
          }
        }

        output[[valBoxName]] <- renderValueBox({

          # Note: Modify in case (optional) valueBox values should be calculated differently
          if (!is.null(valueTmp)) {
            valueTmp <- valueTmp %>%
              pull(value) %>%
              as.numeric()
            if (!is.na(options$valueBoxes$Decimals[i])) {
              valueTmp <- valueTmp %>% round(as.numeric(options$valueBoxes$Decimals[i]))
            }
          }

          infoBoxCustom(
            value = valueTmp,
            prefix = options$valueBoxes$Prefix[i],
            postfix = options$valueBoxes$Postfix[i],
            noColor = options$valueBoxes$NoColor[i],
            invert = options$valueBoxes$redPositive[i],
            title = options$valueBoxes$Title[i],
            color = if (startsWith(options$valueBoxes$Color[i], "#")) "aqua" else options$valueBoxes$Color[i],
            icon = icon(options$valueBoxes$Icon[i]),
            customColor = if (startsWith(options$valueBoxes$Color[i], "#")) options$valueBoxes$Color[i] else NULL,
            noView = if (!valBoxName %in% names(options$dataViews)) TRUE else FALSE
          )
        })
      })

      # Data View switch
      observeEvent(input$showChart, {
        views <- names(options$dataViews)
        boxWithoutView <- options$valueBoxes$Id[!options$valueBoxes$Id %in% views]

        reportToRender <- substr(input$showChart, nchar(session$ns("")) + 1L, nchar(input$showChart))
        if (reportToRender %in% boxWithoutView) {
          return()
        }
        reportToRender <- if (reportToRender %in% views) reportToRender else options$valueBoxes$Id[[1]]

        for (view in views) {
          if (identical(reportToRender, view)) {
            showEl(session, paste0("#", session$ns(paste0(view, "View"))))
          } else {
            hideEl(session, paste0("#", session$ns(paste0(view, "View"))))
          }
        }
      })

      # Data views
      # names(options$dataViews) must match options$valueBoxes$Id entries
      output$dataViews <- renderUI({
        sections <- lapply(names(options$dataViews), function(viewList) {
          view <- options$dataViews[[viewList]]
          idList <- list()
          titleList <- list()

          for (i in seq_along(view)) {
            idList[[i]] <- names(view)[i]
            titleList[[i]] <- view[[i]]
          }

          tags$div(
            id = ns(paste0(viewList, "View")),
            style = ifelse(viewList == options$valueBoxes$Id[[1]], "", "display:none;"),
            lapply(seq_along(idList), function(i) {
              id <- idList[[i]]
              title <- titleList[[i]]

              # allow custom renderer here?
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
                        filterInputs <- lapply(userFilter, function(filterName) {
                          tags$div(
                            class = "custom-dropdown-wide user-filter",
                            class = if (length(userFilter) %% 2 == 0) "even-inline" else if (length(userFilter) == 1) "one-inline" else "odd-inline",
                            selectizeInput(ns(paste0(id, "userFilter_", filterName)),
                              label = NULL,
                              choices = c("All" = "", attr(dashboardChartData[[id]], paste0("userFilterData_", filterName))),
                              multiple = TRUE, width = "100%",
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
                      dataTableOutput(ns(paste0(id, "Table"))),
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

          # heatmap
          if (input[[paste0(indicator, "ChartType")]] == "heatmap") {
            brks <- quantile(dataTmp[-seq_len(as.numeric(noRowHeaders))],
              probs = seq(.05, .95, .05), na.rm = TRUE
            )
            clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
              {
                paste0("rgb(255,", ., ",", ., ")")
              }
          }

          # Table Summary
          colSummarySettings <- NULL

          nonNumericCols <- dataTmp %>%
            select(where(~ !is.numeric(.))) %>%
            names()

          if (dataViewsConfig[[indicator]]$tableSummarySettings$enabled) {
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
              unlist(nonNumericCols[names(dataTmp)[seq_len(noRowHeaders)]],
                use.names = FALSE
              ),
              colSummary = colSummarySettings
            ),
            options = list(
              paging = FALSE, dom = "t",
              scrollX = TRUE,
              scrollY = if (length(dataViewsConfig[[indicator]]$height)) dataViewsConfig[[indicator]]$height else "33vh",
              scrollCollapse = TRUE,
              columnDefs = list(list(
                className = "dt-left", targets = "_all"
              ))
            )
          )

          if (!identical(input[[paste0(indicator, "ChartType")]], "heatmap")) {
            return(tableObj)
          }
          return(formatStyle(tableObj, seq(noRowHeaders + 1, length(dataTmp)),
            color = "#000",
            backgroundColor = styleInterval(brks, clrs)
          ))
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
                "stackedbar", "radar", "timeseries"
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
              chartJsObj$x$scales$y$stacked <- TRUE
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
          } else if (identical(chartType, "stackedbar")) {
            chartJsObj <- chartjs(
              customColors = chartColorsToUse
            ) %>%
              cjsBar(
                labels = labels, stacked = TRUE,
                xTitle = currentView$chartOptions$xTitle,
                yTitle = currentView$chartOptions$yTitle
              )
            chartJsObj$x$options$plugins$tooltip <- list(
              mode = "index",
              position = "nearest"
            )
          } else if (identical(chartType, "radar")) {
            chartJsObj <- chartjs(
              customColors = chartColorsToUse
            ) %>%
              cjsRadar(labels = labels)
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
          if (identical(currentView$chartOptions$yLogScale, TRUE) &&
            identical(chartJsObj$x$scales$y$type, "linear")) {
            chartJsObj$x$scales$y$type <- "logarithmic"
          }

          for (i in seq_len(noSeries)) {
            chartJsObj <- cjsSeries(chartJsObj, dataTmp[[rowHeaderLen + i]],
              label = names(dataTmp)[rowHeaderLen + i],
              fill = chartType %in% c("area", "stackedarea"),
              fillOpacity = if (identical(chartType, "stackedarea")) 1 else 0.15
            )
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
              overScaleMode = "y"
            ),
            pan = list(
              enabled = TRUE,
              mode = "xy"
            )
          )

          return(chartJsObj %>% cjsLegend())
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
                id = ns(paste0(indicator, "DownloadPng")), # ns("downloadPng"),
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
  )
}
