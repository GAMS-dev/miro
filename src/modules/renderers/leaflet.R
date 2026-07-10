miroLeafletOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  dataGraph <- leafletOutput(ns("graph"), height = if (length(height)) height else "70vh")

  if (length(options$filter$col)) {
    filterInput <- if (isTRUE(options$filter$date)) {
      dateRangeInput(
        ns("data_filter"),
        label = options$filter$label,
        start = options$filter$start,
        end = options$filter$end
      )
    } else {
      selectInput(
        ns("data_filter"),
        label = options$filter$label,
        choices = NULL,
        multiple = isTRUE(options$filter$multiple)
      )
    }

    return(tags$div(
      class = "data-filter-wrapper",
      tags$div(class = "data-filter", filterInput),
      dataGraph
    ))
  }

  dataGraph
}

renderMiroLeaflet <- function(id, data, options = NULL, path = NULL,
                              rendererEnv = NULL, views = NULL,
                              outputScalarsFull = NULL, roundPrecision = 2L,
                              ...) {
  moduleServer(
    id,
    function(input, output, session) {
      if (length(options$filter$col)) {
        observe({
          updateRendererFilterInput(session, input, data, options)
        })
      }

      output$graph <- renderLeaflet({
        dataTmp <- resolveRendererData(data)

        if (is.null(dataTmp)) {
          return(NULL)
        }

        if (!nrow(dataTmp)) {
          return(leaflet() %>% addTiles())
        }

        dataTmp <- filterRendererData(dataTmp, input, options)

        if (is.null(dataTmp) || !nrow(dataTmp)) {
          return(leaflet() %>% addTiles())
        }

        p <- leaflet(dataTmp) %>% addTiles()

        lapply(seq_along(options$markers), function(markerIdx) {
          markerOptions <- options$markers[[markerIdx]]

          icons <- awesomeIcons(
            icon = if (length(markerOptions$iconOptions$icon)) {
              markerOptions$iconOptions$icon
            } else {
              "circle"
            },
            iconColor = if (length(markerOptions$iconOptions$iconColor)) {
              markerOptions$iconOptions$iconColor
            } else {
              "#000000"
            },
            markerColor = if (length(markerOptions$iconOptions$markerColor)) {
              markerOptions$iconOptions$markerColor
            } else {
              "blue"
            },
            library = "fa"
          )

          p <<- addAwesomeMarkers(p,
            lng = dataTmp[[markerOptions$lng]],
            lat = dataTmp[[markerOptions$lat]],
            layerId = paste0("marker_", markerIdx, "_", seq_len(nrow(dataTmp))),
            icon = icons,
            group = markerOptions$group,
            label = if (length(markerOptions[["label"]])) {
              eval(
                parseLabel(markerOptions[["label"]], names(dataTmp)),
                envir = list(data = dataTmp)
              )
            },
            labelOptions = markerOptions$labelOptions
          )
        })

        if (length(options$hideGroups)) {
          p <- leaflet::hideGroup(p, options$hideGroups)
        }

        eps <- 1e-14

        lapply(seq_along(options$flows), function(flowIdx) {
          flowOptions <- options$flows[[flowIdx]]

          lng0 <- dataTmp[[flowOptions$lng0]]
          lat0 <- dataTmp[[flowOptions$lat0]]
          lng1 <- dataTmp[[flowOptions$lng1]]
          lat1 <- dataTmp[[flowOptions$lat1]]

          if (any(is.na(lng0))) stop("Missing lng0 data for flow", call. = FALSE)
          if (any(is.na(lat0))) stop("Missing lat0 data for flow", call. = FALSE)
          if (any(is.na(lng1))) stop("Missing lng1 data for flow", call. = FALSE)
          if (any(is.na(lat1))) stop("Missing lat1 data for flow", call. = FALSE)

          p <<- addFlows(p,
            lng0 = lng0 + (flowIdx - 1) * eps,
            lat0 = lat0 + (flowIdx - 1) * eps,
            lng1 = lng1 + (flowIdx - 1) * eps,
            lat1 = lat1 + (flowIdx - 1) * eps,
            color = flowOptions$color,
            flow = coalesce(dataTmp[[flowOptions$flow]], 0),
            opacity = flowOptions$opacity,
            minThickness = flowOptions$minThickness,
            layerId = if (length(flowOptions$layerId)) {
              eval(
                parseLabel(flowOptions$layerId, names(dataTmp)),
                envir = list(data = dataTmp)
              )
            },
            time = if (length(flowOptions$time)) dataTmp[[flowOptions$time]],
            maxThickness = flowOptions$maxThickness,
            initialTime = flowOptions$initialTime,
            dir = flowOptions$dir
          )
        })

        lapply(seq_along(options$minicharts), function(minichartIdx) {
          minichartOptions <- options$minicharts[[minichartIdx]]
          chartDataTmp <- dataTmp[, minichartOptions$chartdata]

          if (!nrow(chartDataTmp)) {
            return()
          }

          multiplier <- if (identical(minichartOptions$variableSize, TRUE)) {
            rowSumsTmp <- rowSums(chartDataTmp, na.rm = TRUE)
            maxRowSum <- max(rowSumsTmp)
            if (is.na(maxRowSum) || maxRowSum == 0) {
              rep(1, length(rowSumsTmp))
            } else {
              rowSumsTmp / maxRowSum
            }
          } else {
            1
          }

          p <<- addMinicharts(p,
            lng = dataTmp[[minichartOptions$lng]],
            lat = dataTmp[[minichartOptions$lat]],
            chartdata = chartDataTmp,
            time = if (length(minichartOptions$time)) dataTmp[[minichartOptions$time]],
            type = minichartOptions$type,
            fillColor = d3.schemeCategory10[1],
            colorPalette = d3.schemeCategory10,
            width = as.numeric(minichartOptions$width) * multiplier,
            height = as.numeric(minichartOptions$height) * multiplier,
            opacity = minichartOptions$opacity,
            showLabels = minichartOptions$showLabels,
            labelText = NULL,
            labelMinSize = 8,
            labelMaxSize = 24,
            labelStyle = NULL,
            transitionTime = if (length(minichartOptions$transitionTime)) {
              minichartOptions$transitionTime
            },
            popup = popupArgs(),
            layerId = if (length(minichartOptions$layerId)) {
              dataTmp[[minichartOptions$layerId]]
            },
            legend = minichartOptions$legend,
            legendPosition = minichartOptions$legendPosition,
            timeFormat = NULL,
            initialTime = NULL,
            onChange = NULL
          )
        })

        if (length(options$layersControl$baseGroups) +
          length(options$layersControl$overlayGroups) > 0L) {
          p <- addLayersControl(p,
            baseGroups = if (length(options$layersControl$baseGroups)) {
              options$layersControl$baseGroups
            } else {
              character(0L)
            },
            overlayGroups = if (length(options$layersControl$overlayGroups)) {
              options$layersControl$overlayGroups
            } else {
              character(0L)
            },
            position = options$layersControl$position,
            options = if (length(options$layersControl$options)) {
              do.call(layersControlOptions, options$layersControl$options)
            } else {
              layersControlOptions()
            }
          )
        }

        p
      })
    }
  )
}
