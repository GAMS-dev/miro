miroLeafletOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  
  dataGraph <- leafletOutput(ns("graph"), height = if (length(height)) height else "70vh")
  
  if (length(options$filter$col)) {
    filterInput <- if (isTRUE(options$filter$date)) {
      dateRangeInput(ns("data_filter"),
                     label = NULL,
                     start = options$filter$start,
                     end = options$filter$end
      )
    } else {
      selectizeInput(ns("data_filter"),
                     label = NULL,
                     choices = NULL,
                     multiple = TRUE,
                     options = list(plugins = list("remove_button"))
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

renderMiroLeaflet <- function(id, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, roundPrecision = 2L, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      if (inherits(data, "data.frame")) {
        data <- type_convert(data, cols())
      }
      
      filterCol <- NULL
      if (length(options$filter$col)) {
        filterCol <- sym(options$filter$col)
        
        if (!isTRUE(options$filter$date)) {
          choices <- unique(data[[options$filter$col]])
          updateSelectizeInput(session, "data_filter",
                               choices = choices,
                               selected = choices[1],
                               server = TRUE
          )
        }
      }
      
      output$graph <- renderLeaflet({
        dataTmp <- data
        
        if (length(filterCol) && length(input$data_filter)) {
          if (isTRUE(options$filter$date)) {
            filterTmp <- as.POSIXct(input$data_filter)
            dataTmp <- filter(dataTmp, between(
              !!filterCol,
              filterTmp[1],
              max(filterTmp[1], filterTmp[2])
            ))
          } else {
            dataTmp <- filter(dataTmp, !!filterCol %in% input$data_filter)
          }
        }
        
        p <- leaflet(dataTmp) %>% addTiles()
        
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
                                  lng = dataTmp[[options$markers[[j]]$lng]],
                                  lat = dataTmp[[options$markers[[j]]$lat]],
                                  layerId = j,
                                  icon = icons,
                                  group = options$markers[[j]]$group,
                                  label = if (length(options$markers[[j]][["label"]])) {
                                    eval(parseLabel(options$markers[[j]][["label"]], names(dataTmp)))
                                  },
                                  labelOptions = options$markers[[j]]$labelOptions
          )
        })
        
        if (length(options$hideGroups)) {
          p <- leaflet::hideGroup(p, options$hideGroups)
        }
        
        eps <- 1e-14
        
        lapply(seq_along(options$flows), function(j) {
          lng0 <- dataTmp[[options$flows[[j]]$lng0]]
          lat0 <- dataTmp[[options$flows[[j]]$lat0]]
          lng1 <- dataTmp[[options$flows[[j]]$lng1]]
          lat1 <- dataTmp[[options$flows[[j]]$lat1]]
          
          if (any(is.na(lng0))) stop("Missing lng0 data for flow", call. = FALSE)
          if (any(is.na(lat0))) stop("Missing lat0 data for flow", call. = FALSE)
          if (any(is.na(lng1))) stop("Missing lng1 data for flow", call. = FALSE)
          if (any(is.na(lat1))) stop("Missing lat1 data for flow", call. = FALSE)
          
          p <<- addFlows(p,
                         lng0 = lng0 + (j - 1) * eps,
                         lat0 = lat0 + (j - 1) * eps,
                         lng1 = lng1 + (j - 1) * eps,
                         lat1 = lat1 + (j - 1) * eps,
                         color = options$flows[[j]]$color,
                         flow = coalesce(dataTmp[[options$flows[[j]]$flow]], 0),
                         opacity = options$flows[[j]]$opacity,
                         minThickness = options$flows[[j]]$minThickness,
                         layerId = if (length(options$flows[[j]]$layerId)) {
                           eval(parseLabel(options$flows[[j]]$layerId, names(dataTmp)))
                         },
                         time = if (length(options$flows[[j]]$time)) dataTmp[[options$flows[[j]]$time]],
                         maxThickness = options$flows[[j]]$maxThickness,
                         initialTime = options$flows[[j]]$initialTime,
                         dir = options$flows[[j]]$dir
          )
        })
        
        lapply(seq_along(options$minicharts), function(j) {
          chartDataTmp <- dataTmp[, options$minicharts[[j]]$chartdata]
          if (!nrow(chartDataTmp)) return()
          
          multiplier <- if (identical(options$minicharts[[j]]$variableSize, TRUE)) {
            rowSumsTmp <- rowSums(chartDataTmp, na.rm = TRUE)
            rowSumsTmp / max(rowSumsTmp)
          } else {
            1
          }
          
          p <<- addMinicharts(p,
                              lng = dataTmp[[options$minicharts[[j]]$lng]],
                              lat = dataTmp[[options$minicharts[[j]]$lat]],
                              chartdata = chartDataTmp,
                              time = if (length(options$minicharts[[j]]$time)) dataTmp[[options$minicharts[[j]]$time]],
                              type = options$minicharts[[j]]$type,
                              fillColor = d3.schemeCategory10[1],
                              colorPalette = d3.schemeCategory10,
                              width = as.numeric(options$minicharts[[j]]$width) * multiplier,
                              height = as.numeric(options$minicharts[[j]]$height) * multiplier,
                              opacity = options$minicharts[[j]]$opacity,
                              showLabels = options$minicharts[[j]]$showLabels,
                              labelText = NULL,
                              labelMinSize = 8,
                              labelMaxSize = 24,
                              labelStyle = NULL,
                              transitionTime = if (length(options$minicharts[[j]]$transitionTime)) {
                                options$minicharts[[j]]$transitionTime
                              },
                              popup = popupArgs(),
                              layerId = if (length(options$minicharts[[j]]$layerId)) {
                                dataTmp[[options$minicharts[[j]]$layerId]]
                              },
                              legend = options$minicharts[[j]]$legend,
                              legendPosition = options$minicharts[[j]]$legendPosition,
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