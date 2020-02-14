tspOutput <- function(id, height, options, path){
  ns <- NS(id)
  leafletOutput(ns("tsp_input"), width = "100%", height = 700)
}

renderTsp <- function(input, output, session, data, options = NULL, path = NULL){
  force(data)
  markerCnt <- 1L
  
  dataTmp <- list()
  if(length(data) == 3L && nrow(data)){
    dataTmp <- lapply(seq_len(nrow(data)), function(i){
      list(lat = data[[2]][i], lng = data[[3]][i])
    })
    names(dataTmp) <- data[[1]]
  }
  output$tsp_input <- renderLeaflet({
    if(length(dataTmp)){
      return(leaflet() %>% addTiles() %>%
               addMarkers(data[[3L]], data[[2L]], label = data[[1L]],
                          group = "markers", options = markerOptions(draggable = TRUE), 
                          layerId = data[[1L]]))
    }else{
      return(leaflet() %>% addTiles())
    }
  })
  
  rv <- reactiveValues(markerPositions = dataTmp)
  init <- FALSE
  
  observe({
    input$tsp_input_marker_click
    if(!init){
      return()
    }
    isolate({
      markerId <- input$tsp_input_marker_click$id
      rv$markerPositions[[markerId]] <<- NULL
      leafletProxy("tsp_input") %>%
        removeMarker(markerId)
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
      
      leafletProxy("tsp_input") %>%
        addMarkers(lat = newMarker$lat, lng = newMarker$lng, group = "markers",
                   options = markerOptions(draggable = TRUE), layerId = markerId) 
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

