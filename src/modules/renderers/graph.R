isColor <- function(x) {
  tryCatch(is.matrix(col2rgb(x)),
    error = function(e) FALSE
  )
}
renderGraph <- function(data, configData, options, height = NULL, input = NULL, filterCol = NULL) {
  # Renders the graph for a dataframe using the provided configuration.
  #
  # Args:
  #   data:                     dataframe that is to be plotted
  #   configData:              data used for configuration of graphs
  #   options:                  options for customizing graph
  #   height:                   height of graph object (default: automatic sizing)
  #
  # Returns:
  #   rendered graph for the provided dataframe
  if (inherits(data, "data.frame")) {
    data <- type_convert(data, cols())
  }
  if (options$tool == "plotly") {
    pieGrid <- NULL
    rendery2axis <- FALSE
    return(renderPlotly({
      if (length(filterCol) && length(input$data_filter)) {
        if (isTRUE(options$filter$date)) {
          filterTmp <- as.POSIXct(input$data_filter)
          data <- filter(data, between(
            !!filterCol, filterTmp[1],
            max(filterTmp[1], filterTmp[2])
          ))
        } else {
          data <- filter(data, !!filterCol %in% input$data_filter)
        }
      }

      if (options$type == "pie") {
        # pie chart
        p <- NULL
        chartsPerRow <- if (is.numeric(options$gridRows)) options$gridRows else 3L
        pieGrid <- list(
          rows = (length(options$traces) - 1L) %/% chartsPerRow + 1L,
          columns = if (length(options$traces) < chartsPerRow) {
            length(options$traces)
          } else {
            chartsPerRow
          }
        )
        lapply(seq_along(options$traces), function(j) {
          if (j == 1) {
            p <<- plot_ly(
              height = if (!is.null(options$fixedHeight)) options$fixedHeight else height,
              width = options$fixedWidth
            ) %>%
              add_pie(p,
                data = data, labels = try(data[[options$traces[[1]]$labels]]),
                values = try(data[[options$traces[[1]]$values]]),
                hole = options$traces[[1]]$hole,
                name = options$traces[[1]]$name,
                domain = list(row = 0L, column = 0L)
              )
          } else {
            p <<- add_pie(p,
              labels = try(data[[options$traces[[j]]$labels]]),
              values = try(data[[options$traces[[j]]$values]]),
              hole = options$traces[[j]]$hole,
              name = options$traces[[j]]$name,
              domain = list(
                row = (j - 1L) %/% chartsPerRow,
                column = (j - 1L) %% chartsPerRow
              )
            )
          }
        })
      } else if (options$type == "bar") {
        # bar plot
        p <- NULL
        lapply(seq_along(options$ydata), function(j) {
          if (identical(options$ydata[[j]]$yaxis, "y2")) {
            rendery2axis <<- TRUE
            yaxis <- "y2"
          } else {
            yaxis <- "y"
          }
          yData <- options$ydata[[j]]
          markerStyle <- list(line = list(
            color = yData$marker$line$color,
            width = yData$marker$line$width
          ))
          markerColor <- yData$marker$color
          if (length(markerColor)) {
            markerStyle$color <- markerColor
          }
          if (j == 1) {
            p <<- plot_ly(data,
              height = if (!is.null(options$fixedHeight)) options$fixedHeight else height,
              width = options$fixedWidth
            ) %>%
              add_trace(
                x = try(data[[options$xdata]]), type = "bar",
                y = try(data[[names(options$ydata)[1]]]),
                name = yData$label,
                yaxis = yaxis,
                showlegend = options$ydata[[j]]$showlegend,
                color = if (!is.null(options$color)) {
                  try(data[[options$color]])
                },
                marker = markerStyle,
                width = if (!is.null(options$width)) {
                  try(data[[options$width]])
                },
                orientation = options$orientation
              )
          } else {
            p <<- add_trace(p,
              x = try(data[[options$xdata]]), type = "bar",
              y = try(data[[names(options$ydata)[j]]]), name = yData$label,
              marker = markerStyle,
              showlegend = options$ydata[[j]]$showlegend,
              yaxis = yaxis,
              orientation = options$orientation
            )
          }
        })
      } else if (options$type == "scatter") {
        # scatter plot
        p <- NULL
        lapply(seq_along(options$ydata), function(j) {
          if (identical(options$ydata[[j]]$yaxis, "y2")) {
            rendery2axis <<- TRUE
            yaxis <- "y2"
          } else {
            yaxis <- "y"
          }
          if (j == 1) {
            p <<- plot_ly(data,
              x = try(data[[options$xdata]]), y = try(data[[names(options$ydata)[[1]]]]),
              name = options$ydata[[1]]$label,
              mode = options$ydata[[1]]$mode,
              yaxis = yaxis,
              marker = getMarkerInfo(options$ydata[[1]]$marker),
              line = list(
                color = options$ydata[[1]]$line$color,
                width = options$ydata[[1]]$line$width,
                shape = options$ydata[[1]]$line$shape,
                dash = options$ydata[[1]]$line$dash
              ),
              fill = options$ydata[[1]]$fill,
              showlegend = options$ydata[[j]]$showlegend,
              color = if (!is.null(options$color)) {
                try(data[[options$color]])
              },
              symbol = if (!is.null(options$symbol)) {
                try(data[[options$symbol]])
              },
              colors = options$colors, symbols = options$symbols,
              size = options$ydata[[1]]$size, type = "scatter",
              height = if (!is.null(options$fixedHeight)) options$fixedHeight else height,
              width = options$fixedWidth,
              frame = if (!is.null(options$ydata[[1]]$frame)) {
                try(data[[options$ydata[[1]]$frame]])
              }
            )
          } else {
            p <<- add_trace(p,
              y = try(data[[names(options$ydata)[[j]]]]), name = options$ydata[[j]]$label,
              yaxis = yaxis,
              mode = options$ydata[[j]]$mode,
              marker = getMarkerInfo(options$ydata[[j]]$marker),
              line = list(
                color = options$ydata[[j]]$line$color,
                width = options$ydata[[j]]$line$width,
                shape = options$ydata[[j]]$line$shape,
                dash = options$ydata[[j]]$line$dash
              ),
              fill = options$ydata[[j]]$fill,
              showlegend = options$ydata[[j]]$showlegend,
              color = if (!is.null(options$ydata[[j]]$color)) {
                try(data[[options$ydata[[j]]$color]])
              },
              symbol = if (!is.null(options$ydata[[j]]$symbol)) {
                try(data[[options$ydata[[j]]$symbol]])
              },
              colors = options$ydata[[j]]$colors,
              symbols = options$ydata[[j]]$symbols, size = options$ydata[[j]]$size,
              frame = if (!is.null(options$ydata[[j]]$frame)) {
                try(data[[options$ydata[[j]]$frame]])
              }
            )
          }
        })
        if (!is.null(options$animation)) {
          p <- animation_opts(p,
            frame = options$animation$frame,
            transition = options$animation$transition,
            easing = options$animation$easing,
            redraw = options$animation$redraw,
            mode = options$animation$mode
          )
        }
        if (!is.null(options$animation$slider)) {
          p <- animation_slider(p,
            hide = if (!is.null(options$animation$slider$hide)) {
              try(options$animation$slider$hide)
            },
            label = if (!is.null(options$animation$slider$label)) {
              try(options$animation$slider$label)
            },
            currentvalue = list(
              prefix = if (!is.null(options$animation$slider$prefix)) {
                try(options$animation$slider$prefix)
              },
              font = list(color = if (!is.null(options$animation$slider$fontcolor)) {
                try(options$animation$slider$fontcolor)
              })
            )
          )
        }
      } else if (options$type == "bubble") {
        # bubble chart
        p <- NULL
        lapply(seq_along(options$ydata), function(j) {
          maxsize <- NULL
          sizeref <- 1L
          if (!is.null(options$ydata[[j]]$marker$maxsize)) {
            maxsize <- options$ydata[[j]]$marker$maxsize
          }
          if (!is.null(options$ydata[[j]]$marker$size)) {
            sizevalues <- data[[options$ydata[[j]]$marker$size]]
          }
          if (!is.null(maxsize) && !is.null(sizevalues)) {
            sizeref <- 2.0 * max(sizevalues) / (maxsize**2)
          }
          if (j == 1) {
            p <<- plot_ly(data,
              x = try(data[[options$xdata]]), y = try(data[[names(options$ydata)[[1]]]]),
              name = options$ydata[[1]]$label,
              mode = options$ydata[[1]]$mode,
              marker = list(
                symbol = options$ydata[[1]]$marker$symbol,
                opacity = options$ydata[[1]]$marker$opacity,
                size = if (!is.null(options$ydata[[1]]$marker$size)) {
                  try(data[[options$ydata[[1]]$marker$size]])
                },
                sizemode = options$ydata[[1]]$marker$sizemode, sizeref = sizeref,
                color = if (!is.null(options$ydata[[1]]$marker$color)) {
                  if (isColor(options$ydata[[1]]$marker$color) ||
                    startsWith(options$ydata[[1]]$marker$color, "rgba(")) {
                    options$ydata[[1]]$marker$color
                  } else {
                    try(data[[options$ydata[[1]]$marker$color]])
                  }
                },
                # color = options$ydata[[1]]$marker$colorDep,
                line = list(
                  color = options$ydata[[1]]$marker$line$color,
                  width = options$ydata[[1]]$marker$line$width
                )
              ),
              line = list(
                color = options$ydata[[1]]$line$color,
                width = options$ydata[[1]]$line$width,
                shape = options$ydata[[1]]$line$shape,
                dash = options$ydata[[1]]$line$dash
              ),
              showlegend = options$ydata[[1]]$showlegend,
              color = if (!is.null(options$color)) {
                try(data[[options$color]])
              },
              symbol = if (!is.null(options$symbol)) {
                try(data[[options$symbol]])
              },
              colors = options$colors, symbols = options$symbols,
              size = options$ydata[[1]]$size, type = "scatter",
              height = if (!is.null(options$fixedHeight)) options$fixedHeight else height,
              width = options$fixedWidth,
              frame = if (!is.null(options$ydata[[1]]$frame)) {
                try(data[[options$ydata[[1]]$frame]])
              }
            )
          } else {
            p <<- add_trace(p,
              y = try(data[[names(options$ydata)[[j]]]]), name = options$ydata[[j]]$label,
              mode = options$ydata[[j]]$mode,
              marker = list(
                symbol = options$ydata[[j]]$marker$symbol,
                opacity = options$ydata[[j]]$marker$opacity,
                size = if (!is.null(options$ydata[[j]]$marker$size)) {
                  try(data[[options$ydata[[j]]$marker$size]])
                },
                sizemode = options$ydata[[1]]$marker$sizemode,
                sizeref = sizeref,
                color = if (!is.null(options$ydata[[j]]$marker$color)) {
                  try(data[[options$ydata[[j]]$marker$color]])
                },
                # color = options$ydata[[1]]$marker$colorDep,
                line = list(
                  color = options$ydata[[j]]$marker$line$color,
                  width = options$ydata[[j]]$marker$line$width
                )
              ),
              line = list(
                color = options$ydata[[j]]$line$color,
                width = options$ydata[[j]]$line$width,
                shape = options$ydata[[j]]$line$shape,
                dash = options$ydata[[j]]$line$dash
              ),
              showlegend = options$ydata[[j]]$showlegend,
              color = if (!is.null(options$ydata[[j]]$color)) {
                try(data[[options$ydata[[j]]$color]])
              },
              symbol = if (!is.null(options$ydata[[j]]$symbol)) {
                try(data[[options$ydata[[j]]$symbol]])
              },
              colors = options$ydata[[j]]$colors,
              symbols = options$ydata[[j]]$symbols, size = options$ydata[[j]]$size,
              frame = if (!is.null(options$ydata[[j]]$frame)) {
                try(data[[options$ydata[[j]]$frame]])
              }
            )
          }
        })
        if (!is.null(options$animation)) {
          p <- animation_opts(p,
            frame = options$animation$frame,
            transition = options$animation$transition,
            easing = options$animation$easing,
            redraw = options$animation$redraw,
            mode = options$animation$mode
          )
        }
        if (!is.null(options$animation$slider)) {
          p <- animation_slider(p,
            hide = if (!is.null(options$animation$slider$hide)) {
              try(options$animation$slider$hide)
            },
            label = if (!is.null(options$animation$slider$label)) {
              try(options$animation$slider$label)
            },
            currentvalue = list(
              prefix = if (!is.null(options$animation$slider$prefix)) {
                try(options$animation$slider$prefix)
              },
              font = list(color = if (!is.null(options$animation$slider$fontcolor)) {
                try(options$animation$slider$fontcolor)
              })
            )
          )
        }
      } else if (options$type == "hist") {
        # histogram
        p <- NULL
        if (identical(options$horizontal, TRUE)) {
          lapply(seq_along(options$xdata), function(j) {
            xData <- options$xdata[[j]]
            markerStyle <- list()
            markerColor <- xData$color
            if (length(markerColor)) {
              markerStyle$color <- markerColor
            }
            if (j == 1) {
              p <<- plot_ly(data,
                type = "histogram", histnorm = options$histnorm,
                height = if (!is.null(options$fixedHeight)) options$fixedHeight else height,
                width = options$fixedWidth,
                nbinsy = options$nbins,
                color = if (!is.null(options$color)) {
                  try(data[[options$color]])
                },
                alpha = options$alpha,
                cumulative = list(enabled = identical(options$cumulative, TRUE))
              ) %>%
                add_histogram(
                  y = try(data[[names(options$xdata)[[j]]]]),
                  name = options$xdata[[j]]$labels,
                  marker = markerStyle
                )
            } else {
              p <<- add_histogram(p,
                y = try(data[[names(options$xdata)[[j]]]]),
                name = options$xdata[[j]]$labels,
                marker = markerStyle
              )
            }
          })
        } else {
          lapply(seq_along(options$xdata), function(j) {
            xData <- options$xdata[[j]]
            markerStyle <- list()
            markerColor <- xData$color

            if (length(markerColor)) {
              markerStyle$color <- markerColor
            }
            if (j == 1) {
              p <<- plot_ly(data,
                type = "histogram", histnorm = options$histnorm,
                height = if (!is.null(options$fixedHeight)) options$fixedHeight else height,
                width = options$fixedWidth,
                nbinsx = options$nbins,
                color = if (!is.null(options$color)) {
                  try(data[[options$color]])
                },
                alpha = options$alpha,
                cumulative = list(enabled = identical(options$cumulative, TRUE))
              ) %>%
                add_histogram(
                  x = try(data[[names(options$xdata)[[j]]]]),
                  name = options$xdata[[j]]$labels,
                  marker = markerStyle
                )
            } else {
              p <<- add_histogram(p,
                x = try(data[[names(options$xdata)[[j]]]]),
                name = options$xdata[[j]]$labels,
                marker = markerStyle
              )
            }
          })
        }
      } else {
        stop("The plot type you selected is currently not supported for tool plotly.", call. = FALSE)
      }
      if (length(p)) {
        layout(p,
          title = options$title, barmode = options$barmode, margin = options$margins,
          xaxis = list(
            title = options$xaxis$title, showgrid = options$xaxis$showgrid,
            zeroline = options$xaxis$zeroline, showticklabels = options$xaxis$showticklabels,
            range = c(options$xaxis$rangefrom, options$xaxis$rangeto),
            categoryorder = options$xaxis$categoryorder
          ),
          yaxis = list(
            title = options$yaxis$title, showgrid = options$yaxis$showgrid,
            zeroline = options$yaxis$zeroline, showticklabels = options$yaxis$showticklabels,
            range = c(options$yaxis$rangefrom, options$yaxis$rangeto),
            categoryorder = options$yaxis$categoryorder,
            scaleanchor = options$yaxis$scaleanchor,
            scaleratio = options$yaxis$scaleratio
          ),
          yaxis2 = if (isTRUE(rendery2axis)) {
            list(
              title = options$y2axis$title, showgrid = options$y2axis$showgrid,
              zeroline = options$y2axis$zeroline, showticklabels = options$y2axis$showticklabels,
              range = c(options$y2axis$rangefrom, options$y2axis$rangeto),
              categoryorder = options$y2axis$categoryorder,
              scaleanchor = options$y2axis$scaleanchor,
              scaleratio = options$y2axis$scaleratio,
              overlaying = if (isTRUE(rendery2axis)) "y",
              side = if (isTRUE(rendery2axis)) "right"
            )
          },
          paper_bgcolor = if (length(options$paper_bgcolor)) options$paper_bgcolor else "rgba(0,0,0,0)",
          plot_bgcolor = if (length(options$plot_bgcolor)) options$plot_bgcolor else "rgba(0,0,0,0)",
          showlegend = options$showlegend, grid = pieGrid,
          legend = options$legend, bargap = options$bargap, bargroupgap = options$bargroupgap
        ) %>%
          config(p,
            staticPlot = isTRUE(options$staticPlot),
            toImageButtonOptions = list(width = NULL, height = NULL)
          )
      }
    }))
  } else if (options$tool == "dygraphs") {
    return(renderDygraph({
      if (length(filterCol) && length(input$data_filter)) {
        if (isTRUE(options$filter$date)) {
          filterTmp <- as.POSIXct(input$data_filter)
          data <- filter(data, between(
            !!filterCol, filterTmp[1],
            max(filterTmp[1], filterTmp[2])
          ))
        } else {
          data <- filter(data, !!filterCol %in% input$data_filter)
        }
      }
      # time series chart
      p <- NULL
      lapply(seq_along(options$ydata), function(j) {
        if (j == 1) {
          # check whether data is already correctly formatted and if y variables are labeled in config.json
          if (!is.null(options$color)) {
            key <- match(tolower(options$color), tolower(colnames(data)))
            value <- match(tolower(names(options$ydata)[1]), tolower(colnames(data)))
            if (is.na(value)) {
              value <- length(data)
            }
            # bring data into right matrix format
            if (length(unique(data[[key]])) > 50L) {
              stop("The column you selected to pivot on contains too many (unique) elements: maximum of 50 elements allowed.",
                call. = FALSE
              )
            }

            xts_data <- pivot_wider(data,
              names_from = !!key,
              values_from = !!value
            )

            if (length(options$xdata)) {
              xtsIdx <- match(tolower(options$xdata), tolower(colnames(data)))[[1]]
              if (is.na(xtsIdx)) {
                stop(sprintf("Could not find x data column: '%s'.", options$xdata), call. = FALSE)
              }
              xts_idx <- NULL
              tryCatch(
                {
                  xts_idx <- as.POSIXct(xts_data[[xtsIdx]])
                  xts_data <- xts_data[, -c(xtsIdx)]
                  xts_data <- xts(xts_data, order.by = xts_idx)
                },
                error = function(e) {
                  xts_data <<- xts_data %>% select(!!sym(colnames(data)[xtsIdx]), everything())
                }
              )
            } else {
              xtsIdx <- seq_along(xts_data)[vapply(xts_data, isDate, logical(1L), USE.NAMES = FALSE)][1]
              if (length(xtsIdx)) {
              } else {
                xts_idx <- as.POSIXct(xts_data[[xtsIdx]])
                xts_data <- xts_data[, -c(xtsIdx)]
                xts_data <- xts(xts_data, order.by = xts_idx)
              }
            }
            p <<- dygraph(xts_data,
              main = options$title, periodicity = NULL, group = NULL,
              elementId = NULL
            )
          } else {
            idxVector <- match(tolower(names(options$ydata)), tolower(names(data)))
            dataColId <- 1L
            if (length(options$xdata)) {
              dataColId <- match(tolower(options$xdata[1]), tolower(names(data)))
              if (is.na(dataColId)) {
                dataColId <- 1L
              }
            }
            dateCol <- data[[dataColId]]
            if (!inherits(dateCol, "POSIXct")) {
              dateCol <- tryCatch(as.POSIXct(dateCol, tz = "GMT"),
                error = function(e) {
                  stop("X axis data could not be identified as dates. Try: yyyy-mm-dd format.", call. = FALSE)
                }
              )
            }

            xts_data <- xts(data[, idxVector], order.by = dateCol)

            p <<- dygraph(xts_data, main = options$title, periodicity = NULL, group = NULL, elementId = NULL)
            p <<- dySeries(p,
              name = names(options$ydata)[[1]], label = options$ydata[[1]]$label,
              color = options$ydata[[1]]$color, axis = options$ydata[[1]]$yaxis,
              stepPlot = options$ydata[[1]]$stepPlot, stemPlot = options$ydata[[1]]$stemPlot,
              fillGraph = options$ydata[[1]]$fillGraph, drawPoints = options$ydata[[1]]$drawPoints,
              pointSize = options$ydata[[1]]$pointSize, pointShape = options$ydata[[1]]$pointShape,
              strokeWidth = options$ydata[[1]]$strokeWidth,
              strokePattern = options$ydata[[1]]$strokePattern,
              strokeBorderWidth = options$ydata[[1]]$strokeBorderWidth,
              strokeBorderColor = options$ydata[[1]]$strokeBorderColor
            )
          }
        } else {
          p <<- dySeries(p,
            name = names(options$ydata)[[j]], label = options$ydata[[j]]$label, color = options$ydata[[j]]$color, axis = options$ydata[[j]]$yaxis,
            stepPlot = options$ydata[[j]]$stepPlot, stemPlot = options$ydata[[j]]$stemPlot, fillGraph = options$ydata[[j]]$fillGraph, drawPoints = options$ydata[[j]]$drawPoints,
            pointSize = options$ydata[[j]]$pointSize, pointShape = options$ydata[[j]]$pointShape, strokeWidth = options$ydata[[j]]$strokeWidth,
            strokePattern = options$ydata[[j]]$strokePattern,
            strokeBorderWidth = options$ydata[[j]]$strokeBorderWidth, strokeBorderColor = options$ydata[[j]]$strokeBorderColor
          )
        }
      })
      # add graph options specified in config.json
      if (!is.null(options$dyOptions)) {
        p <- do.call(dyOptions, c(list(dygraph = p), options$dyOptions))
      }
      # lenged options
      if (!is.null(options$dyLegend)) {
        p <- do.call(dyLegend, c(list(dygraph = p), options$dyLegend))
      }
      # highlighting options - highlight hovered series
      if (!is.null(options$dyHighlight)) {
        p <- do.call(dyHighlight, c(list(dygraph = p), options$dyHighlight))
      }
      # use a selector for panning and zooming
      if (!is.null(options$dyRangeSelector)) {
        p <- do.call(dyRangeSelector, c(list(dygraph = p), options$dyRangeSelector))
      }
      # Candlestick charts: use the first four data series to plot, the rest of the data series (if any) are rendered with line plotter.
      if (!is.null(options$dyCandlestick)) {
        p <- do.call(dyCandlestick, c(list(dygraph = p), options$dyCandlestick))
      }
      if (!is.null(options$xaxis)) {
        p <- do.call(dyAxis, c(list(dygraph = p), options$xaxis))
      }
      if (!is.null(options$yaxis)) {
        p <- do.call(dyAxis, c(list(dygraph = p), options$yaxis))
      }
      if (!is.null(options$yaxis2)) {
        p <- do.call(dyAxis, c(list(dygraph = p), options$yaxis2))
      }
      # Event lines to note points within a time series.
      if (!is.null(options$dyEvent)) {
        lapply(seq_along(options$dyEvent), function(j) {
          event <- getEvent(configData, names(options$dyEvent)[[j]])
          p <<- do.call(dyEvent, c(list(dygraph = p, x = event), options$dyEvent[[j]]))
        })
      }
      # Limit lines to highlight data levels.
      if (!is.null(options$dyLimit)) {
        lapply(seq_along(options$dyLimit), function(j) {
          options$dyLimit[[j]]$limit <- getEvent(configData, options$dyLimit[[j]]$limit)
          p <<- do.call(dyLimit, c(list(dygraph = p), options$dyLimit[[j]]))
        })
      }
      # Annotations to note points within a time series.
      if (!is.null(options$dyAnnotation)) {
        lapply(seq_along(options$dyAnnotation), function(j) {
          event <- getEvent(configData, names(options$dyAnnotation)[[j]])
          p <<- do.call(dyAnnotation, c(list(dygraph = p, x = event), options$dyAnnotation[[j]]))
        })
      }
      # Add a shading effect to the graph background for one or more time ranges.
      if (length(options$dyShading)) {
        lapply(seq_along(options$dyShading), function(j) {
          options$dyShading[[j]]$from <- getEvent(configData, options$dyShading[[j]]$from)
          options$dyShading[[j]]$to <- getEvent(configData, options$dyShading[[j]]$to)
          p <<- do.call(dyShading, c(list(dygraph = p), options$dyShading[[j]]))
        })
      }
      p
    }))
  } else if (options$tool == "leaflet") {
    return(renderLeaflet({
      if (length(filterCol) && length(input$data_filter)) {
        if (isTRUE(options$filter$date)) {
          filterTmp <- as.POSIXct(input$data_filter)
          data <- filter(data, between(
            !!filterCol, filterTmp[1],
            max(filterTmp[1], filterTmp[2])
          ))
        } else {
          data <- filter(data, !!filterCol %in% input$data_filter)
        }
      }
      p <- leaflet(data) %>% addTiles()

      lapply(seq_along(options$markers), function(j) {
        icons <- awesomeIcons(
          icon = if (length(options$markers[[j]]$iconOptions$icon)) {
            options$markers[[j]]$iconOptions$icon
          } else {
            "circle"
          },
          iconColor = if (length(options$markers[[j]]$iconOptions$iconColor)) {
            options$markers[[j]]$iconOptions$iconColor
          } else {
            "#000000"
          },
          markerColor = if (length(options$markers[[j]]$iconOptions$markerColor)) {
            options$markers[[j]]$iconOptions$markerColor
          } else {
            "blue"
          },
          library = "fa"
        )
        p <<- addAwesomeMarkers(p,
          lng = data[[options$markers[[j]]$lng]],
          lat = data[[options$markers[[j]]$lat]], layerId = j,
          icon = icons,
          group = options$markers[[j]]$group,
          label = if (length(options$markers[[j]][["label"]])) {
            eval(parseLabel(options$markers[[j]][["label"]], names(data)))
          },
          labelOptions = options$markers[[j]]$labelOptions
        )
      })
      if (length(options$hideGroups)) {
        p <- leaflet::hideGroup(p, options$hideGroups)
      }
      # leaflet minicharts will always overwrite previous flow
      # with identical coordinates, so we hack it by adding eps
      eps <- 1e-14

      lapply(seq_along(options$flows), function(j) {
        lng0 <- data[[options$flows[[j]]$lng0]]
        if (any(is.na(lng0))) {
          stop("Missing lng0 data for flow", call. = FALSE)
        }
        lat0 <- data[[options$flows[[j]]$lat0]]
        if (any(is.na(lat0))) {
          stop("Missing lat0 data for flow", call. = FALSE)
        }
        lng1 <- data[[options$flows[[j]]$lng1]]
        if (any(is.na(lng1))) {
          stop("Missing lng1 data for flow", call. = FALSE)
        }
        lat1 <- data[[options$flows[[j]]$lat1]]
        if (any(is.na(lat1))) {
          stop("Missing lat1 data for flow", call. = FALSE)
        }
        p <<- addFlows(p,
          lng0 = lng0 + (j - 1) * eps,
          lat0 = lat0 + (j - 1) * eps,
          lng1 = lng1 + (j - 1) * eps,
          lat1 = lat1 + (j - 1) * eps,
          color = options$flows[[j]]$color,
          flow = coalesce(data[[options$flows[[j]]$flow]], 0), opacity = options$flows[[j]]$opacity,
          minThickness = options$flows[[j]]$minThickness,
          layerId = if (length(options$flows[[j]]$layerId)) {
            eval(parseLabel(options$flows[[j]]$layerId, names(data)))
          },
          time = if (length(options$flows[[j]]$time)) data[[options$flows[[j]]$time]],
          maxThickness = options$flows[[j]]$maxThickness,
          initialTime = options$flows[[j]]$initialTime,
          dir = options$flows[[j]]$dir
        )
      })
      lapply(seq_along(options$minicharts), function(j) {
        chartDataTmp <- data[, options$minicharts[[j]]$chartdata]
        if (!nrow(chartDataTmp)) {
          return() # to avoid ugly errors in log
        }
        if (identical(options$minicharts[[j]]$variableSize, TRUE)) {
          rowSumsTmp <- rowSums(chartDataTmp, na.rm = TRUE)
          multiplier <- rowSumsTmp / max(rowSumsTmp)
        } else {
          multiplier <- 1
        }
        p <<- addMinicharts(p,
          lng = data[[options$minicharts[[j]]$lng]],
          lat = data[[options$minicharts[[j]]$lat]],
          chartdata = chartDataTmp,
          time = if (length(options$minicharts[[j]]$time)) data[[options$minicharts[[j]]$time]],
          # maxValues = options$minicharts[[j]]$maxValues,
          type = options$minicharts[[j]]$type,
          fillColor = d3.schemeCategory10[1],
          colorPalette = d3.schemeCategory10,
          width = as.numeric(options$minicharts[[j]]$width) * multiplier,
          height = as.numeric(options$minicharts[[j]]$height) * multiplier,
          opacity = options$minicharts[[j]]$opacity,
          showLabels = options$minicharts[[j]]$showLabels,
          labelText = NULL, labelMinSize = 8,
          labelMaxSize = 24, labelStyle = NULL,
          transitionTime = if (length(options$minicharts[[j]]$transitionTime)) options$minicharts[[j]]$transitionTime,
          popup = popupArgs(),
          layerId = if (length(options$minicharts[[j]]$layerId)) data[[options$minicharts[[j]]$layerId]],
          legend = options$minicharts[[j]]$legend,
          legendPosition = options$minicharts[[j]]$legendPosition,
          timeFormat = NULL, initialTime = NULL,
          onChange = NULL
        )
      })
      if (length(options$layersControl$baseGroups) + length(options$layersControl$overlayGroups) > 0L) {
        p <- addLayersControl(p,
          baseGroups = if (length(options$layersControl$baseGroups)) options$layersControl$baseGroups else character(0L),
          overlayGroups = if (length(options$layersControl$overlayGroups)) options$layersControl$overlayGroups else character(0L),
          position = options$layersControl$position,
          options = if (length(options$layersControl$options)) {
            do.call(layersControlOptions, options$layersControl$options)
          } else {
            layersControlOptions()
          }
        )
      }
      p
    }))
  } else if (options$tool == "timevis") {
    return(renderTimevis({
      if (length(filterCol) && length(input$data_filter)) {
        if (isTRUE(options$filter$date)) {
          filterTmp <- as.POSIXct(input$data_filter)
          data <- filter(data, between(
            !!filterCol, filterTmp[1],
            max(filterTmp[1], filterTmp[2])
          ))
        } else {
          data <- filter(data, !!filterCol %in% input$data_filter)
        }
      }
      p <- timevis()

      # GANTT chart

      # timevis items
      if (!length(options$series[[1]]$start)) {
        stop("No start data found!", call. = FALSE)
      }
      id <- seq_along(data[[options$series[[1]]$start]])
      content <- data[[options$series[[1]]$content]]
      start <- data[[options$series[[1]]$start]]
      end <- NULL
      type <- NULL
      title <- NULL
      group <- NULL
      subgroup <- NULL

      if (length(options$series[[1]]$end)) {
        end <- data[[options$series[[1]]$end]]
      }
      if (!identical(length(end), length(start))) {
        end <- vector("numeric", length(id))
        end[] <- NA_real_
      }
      if (length(options$series[[1]]$title)) {
        title <- data[[options$series[[1]]$title]]
      }
      if (!identical(length(title), length(start))) {
        title <- vector("numeric", length(id))
        title[] <- NA_real_
      }
      if (length(options$series[[1]]$group)) {
        group <- data[[options$series[[1]]$group]]
      }
      if (!identical(length(group), length(start))) {
        group <- vector("numeric", length(id))
        group[] <- NA_real_
      }
      if (length(options$series[[1]]$subgroup)) {
        subgroup <- data[[options$series[[1]]$subgroup]]
      }
      if (!identical(length(subgroup), length(start))) {
        subgroup <- vector("numeric", length(id))
        subgroup[] <- NA_real_
      }
      if (length(options$series[[1]]$type)) {
        type <- vector("character", length(id))
        type[] <- options$series[[1]]$type
      }
      data <- tibble(
        id = id, content = content, start = start, end = end, type = type,
        title = title, group = group, subgroup = subgroup
      )

      # timevis groups
      groups <- NULL
      gId <- NULL
      gContent <- NULL
      gTitle <- NULL
      gSubgroupOrder <- NULL

      if (!is.null(group) && all(!is.na(group))) {
        gId <- unique(group)
      }
      if (length(options$series[[1]]$gContent)) {
        gContent <- data[[options$series[[1]]$gContent]]
        if (!identical(length(gContent), length(gId))) {
          gContent <- vector("numeric", length(gId))
          gContent[] <- "group"
        }
      } else {
        gContent <- gId
      }
      if (length(options$series[[1]]$groupTitle)) {
        gTitle <- unique(data[[options$series[[1]]$groupTitle]])
      }
      if (!identical(length(gTitle), length(gId))) {
        gTitle <- vector("numeric", length(gId))
        gTitle[] <- NA_real_
      }
      if (length(options$series[[1]]$subgroupOrder)) {
        gSubgroupOrder <- unique(data[[options$series[[1]]$subgroupOrder]])
      }
      if (!identical(length(gSubgroupOrder), length(gId))) {
        gSubgroupOrder <- vector("numeric", length(gId))
        gSubgroupOrder[] <- NA_real_
      }
      if (!is.null(gId) && all(!is.na(gId))) {
        groups <- tibble(id = gId, content = gContent, title = gTitle, subgroupOrder = gSubgroupOrder)
      }

      p <- timevis(data,
        groups = groups,
        showZoom = options$showZoom,
        zoomFactor = options$zoomFactor,
        fit = options$fit,
        width = options$width,
        height = options$height,
        elementId = options$elementId,
        options = list(
          selectable = options$editable,
          editable = options$editable,
          multiselect = options$multiselect,
          showCurrentTime = options$showCurrentTime
        )
      )
      for (j in seq_along(options$custom)) {
        p <- addCustomTime(p,
          time = options$custom[[j]]$time,
          itemId = paste0("timeline_", j)
        )
      }

      p
    }))
  } else {
    stop("The tool you selected for plotting graphs is not currently supported.", call. = FALSE)
  }
}
getEvent <- function(configData, eventId) {
  # extracts event from configData
  #
  # args:
  # configData :     configuration dataframe
  # eventId    :     id of the event
  #
  # returns:
  # string with event information extracted from configData
  if (length(configData)) {
    idx <- match(tolower(eventId), tolower(configData[[1]]))
    if (is.na(idx)) {
      # index could not be found so return the string (fixed value)
      return(eventId)
    } else {
      return(configData[[3]][[idx]])
    }
  } else {
    # config data does not exist, so return string
    return(eventId)
  }
}
getMarkerInfo <- function(data) {
  marker <- list(
    opacity = data$opacity,
    size = data$size,
    line = list(
      color = data$line$color,
      width = data$line$width
    )
  )
  if (length(data$color)) {
    marker$color <- data$color
  }
  if (length(data$symbol)) {
    marker$symbol <- data$symbol
  }
  return(marker)
}
parseLabel <- function(label, colNames) {
  if (!nchar(label)) {
    return(NULL)
  }
  label <- gsub("\\", "\\\\", label, fixed = TRUE)
  label <- gsub('"', '\\"', label, fixed = TRUE)
  for (colName in colNames) {
    label <- gsub(paste0("[", colName, "]"), paste0('",data[[\'', colName, '\']],"'),
      label,
      fixed = TRUE
    )
  }
  return(parse(text = paste0('paste0("', label, '")')))
}
