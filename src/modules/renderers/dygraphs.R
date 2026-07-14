miroDygraphsOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  dataGraph <- tags$div(
    class = "dyGraphs-wrapper",
    dygraphOutput(ns("graph"), height = if (length(height)) height else "70vh")
  )

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

renderMiroDygraphs <- function(id, data, options = NULL, path = NULL,
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

      output$graph <- renderDygraph({
        data <- resolveRendererData(data)

        if (is.null(data)) {
          return(NULL)
        }

        data <- filterRendererData(data, input, options)

        if (is.null(data) || !nrow(data)) {
          return(NULL)
        }

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
                xts_data <- tryCatch(
                  {
                    xts_idx <- as.POSIXct(xts_data[[xtsIdx]])
                    xts_values <- xts_data[, -c(xtsIdx)]
                    xts(xts_values, order.by = xts_idx)
                  },
                  error = function(e) {
                    xts_data %>%
                      select(!!sym(colnames(data)[xtsIdx]), everything())
                  }
                )
              } else {
                xtsIdx <- seq_along(xts_data)[vapply(xts_data, isDate, logical(1L), USE.NAMES = FALSE)][1]
                if (!length(xtsIdx) || is.na(xtsIdx)) {
                  stop(
                    "X axis data could not be identified as dates. Try: yyyy-mm-dd format.",
                    call. = FALSE
                  )
                }
                xts_idx <- as.POSIXct(xts_data[[xtsIdx]])
                xts_data <- xts_data[, -c(xtsIdx)]
                xts_data <- xts(xts_data, order.by = xts_idx)
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
        # legend options
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
            event <- getEvent(outputScalarsFull, names(options$dyEvent)[[j]])
            p <<- do.call(dyEvent, c(list(dygraph = p, x = event), options$dyEvent[[j]]))
          })
        }
        # Limit lines to highlight data levels.
        if (!is.null(options$dyLimit)) {
          lapply(seq_along(options$dyLimit), function(j) {
            options$dyLimit[[j]]$limit <- getEvent(outputScalarsFull, options$dyLimit[[j]]$limit)
            p <<- do.call(dyLimit, c(list(dygraph = p), options$dyLimit[[j]]))
          })
        }
        # Annotations to note points within a time series.
        if (!is.null(options$dyAnnotation)) {
          lapply(seq_along(options$dyAnnotation), function(j) {
            event <- getEvent(outputScalarsFull, names(options$dyAnnotation)[[j]])
            p <<- do.call(dyAnnotation, c(list(dygraph = p, x = event), options$dyAnnotation[[j]]))
          })
        }
        # Add a shading effect to the graph background for one or more time ranges.
        if (length(options$dyShading)) {
          lapply(seq_along(options$dyShading), function(j) {
            options$dyShading[[j]]$from <- getEvent(outputScalarsFull, options$dyShading[[j]]$from)
            options$dyShading[[j]]$to <- getEvent(outputScalarsFull, options$dyShading[[j]]$to)
            p <<- do.call(dyShading, c(list(dygraph = p), options$dyShading[[j]]))
          })
        }
        p
      })
    }
  )
}
