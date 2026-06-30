renderPlotlyGraph <- function(data, configData = NULL, options, height = NULL,
                              input = NULL) {
  pieGrid <- NULL
  rendery2axis <- FALSE

  renderPlotly({
    data <- resolveRendererData(data)

    if (is.null(data)) {
      return(NULL)
    }

    if (!is.null(input)) {
      data <- filterRendererData(data, input, options)
    }

    if (is.null(data)) {
      return(NULL)
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
        config(
          staticPlot = isTRUE(options$staticPlot),
          toImageButtonOptions = list(width = NULL, height = NULL)
        )
    }
  })
}

miroPlotlyOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  dataGraph <- plotlyOutput(ns("graph"), height = if (length(height)) height else "100%")

  if (length(options$filter$col)) {
    return(tags$div(
      class = "data-filter-wrapper",
      if (isTRUE(options$filter$date)) {
        dateRangeInput(
          ns("data_filter"),
          options$filter$label
        )
      } else {
        selectInput(
          ns("data_filter"),
          options$filter$label,
          choices = c(),
          multiple = isTRUE(options$filter$multiple)
        )
      },
      dataGraph
    ))
  }

  dataGraph
}

renderMiroPlotly <- function(id, data, options = NULL, path = NULL,
                             roundPrecision = 2L, rendererEnv = NULL,
                             views = NULL, outputScalarsFull = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      if (length(options$filter$col)) {
        observe({
          updateRendererFilterInput(session, input, data, options)
        })
      }

      output$graph <- renderPlotlyGraph(data,
        configData = outputScalarsFull,
        options = options,
        input = input
      )
    }
  )
}
