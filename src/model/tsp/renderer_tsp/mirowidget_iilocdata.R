mirowidget_iilocdataOutput <- function(id, height, options, path){
  ns <- NS(id)
  leaflet::leafletOutput(ns("tsp_input"), width = "100%", height = 700)
}

renderMirowidget_iilocdata <- function(input, output, session, data, options = NULL, path = NULL, ...){
  markerCnt <- 1L

  rv <- reactiveValues(markerPositions = list)
  observe({
    if(length(data()) != 3L && !nrow(data())){
      return()
    }
    isolate({
      rv$markerPositions <- lapply(seq_len(nrow(data())), function(i){
        list(lat = data()[[2]][i], lng = data()[[3]][i])
      })
      names(rv$markerPositions) <- data()[[1]]
    })
  })
  output$tsp_input <- leaflet::renderLeaflet({
    if(length(data()) == 3L && nrow(data())){
      return(leaflet::leaflet() %>% leaflet::addTiles() %>%
               leaflet::addMarkers(data()[[3L]], data()[[2L]], label = data()[[1L]],
                                   group = "markers", options = leaflet::markerOptions(draggable = TRUE),
                                   layerId = data()[[1L]]))
    }else{
      return(leaflet::leaflet() %>% leaflet::addTiles())
    }
  })
  init <- FALSE

  observe({
    input$tsp_input_marker_click
    if(!init){
      return()
    }
    isolate({
      markerId <- input$tsp_input_marker_click$id
      rv$markerPositions[[markerId]] <<- NULL
      leaflet::leafletProxy("tsp_input") %>%
        leaflet::removeMarker(markerId)
    })
  })
  observe({
    input$tsp_input_marker_dragend
    if(!init){
      return()
    }
    isolate({
      markerId <- input$tsp_input_marker_dragend$id
      rv$markerPositions[[markerId]] <<- list(lat = input$tsp_input_marker_dragend$lat,
                                              lng = input$tsp_input_marker_dragend$lng)
    })
  })

  observe({
    input$tsp_input_click
    if(!init){
      init <<- TRUE
      return()
    }
    isolate({
      markerId <- paste0("Location", markerCnt)
      newMarker <- list(lat = input$tsp_input_click$lat,
                        lng = input$tsp_input_click$lng)
      rv$markerPositions[[markerId]] <<- newMarker
      markerCnt <<- markerCnt + 1L

      leaflet::leafletProxy("tsp_input") %>%
        leaflet::addMarkers(lat = newMarker$lat, lng = newMarker$lng, group = "markers",
                            options = leaflet::markerOptions(draggable = TRUE), layerId = markerId)
    })
  })
  return(reactive({
    if(!length(rv$markerPositions)){
      return(tibble(u = character(0L), lat= numeric(0L), lng = numeric(0L)))
    }
    return(tibble(u = names(rv$markerPositions),
                  lat = vapply(rv$markerPositions, "[[", numeric(1L), "lat"),
                  lng = vapply(rv$markerPositions, "[[", numeric(1L), "lng")))
  }))
}
