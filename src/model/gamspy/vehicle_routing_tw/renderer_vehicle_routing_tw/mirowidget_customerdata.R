mirowidget_customerdataOutput <- function(id, height, options, path) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      leaflet::leafletOutput(ns("vrptw_input"), width = "100%", height = 400)
    ),
    column(
      width = 12,
      DT::DTOutput(ns("table"))
    ),
    column(
      width = 6,
      textOutput(ns("title"))
    )
  )
}

renderMirowidget_customerdata <- function(input, output, session, data, options = NULL, path = NULL, ...) {
  markerCnt <- 0L
  depotName <- "Depot"
  output$title <- renderText({
    input$table_cell_edit
    depotName
  })

  rv <- reactiveValues(
    markerPositions = list(),
    customerData = tibble(
      u = character(), lat = numeric(), lng = numeric(),
      demand = numeric(), readyTime = numeric(), dueDate = numeric(), serviceTime = numeric()
    )
  )

  output$table <- DT::renderDT({
    customerDataTmp <- tibble(
      u = names(rv$markerPositions),
      lat = vapply(rv$markerPositions, "[[", numeric(1L), "lat"),
      lng = vapply(rv$markerPositions, "[[", numeric(1L), "lng"),
      demand = vapply(rv$markerPositions, "[[", numeric(1L), "demand"),
      readyTime = vapply(rv$markerPositions, "[[", numeric(1L), "readyTime"),
      dueDate = vapply(rv$markerPositions, "[[", numeric(1L), "dueDate"),
      serviceTime = vapply(rv$markerPositions, "[[", numeric(1L), "serviceTime")
    )
    isolate(rv$customerData <- customerDataTmp)
    DT::datatable(customerDataTmp, editable = TRUE) %>% DT::formatRound(c("lat", "lng"), digits = 3L)
  })

  # TODO no duplicate names after edit
  observe({
    input$table_cell_edit
    if (!init) {
      return()
    }
    {
      row <- input$table_cell_edit$row
      clmn <- input$table_cell_edit$col

      isolate({
        oldMarkerName <- rv$customerData[["u"]][row]
        if (clmn == 1) {
          idx <- match(oldMarkerName, names(rv$markerPositions))
          names(rv$markerPositions)[idx] <- input$table_cell_edit$value
          markerName <- input$table_cell_edit$value
          if (oldMarkerName == depotName) {
            depotName <<- markerName
          }
        }
        rv$markerPositions[[oldMarkerName]][[names(rv$customerData)[clmn]]] <- input$table_cell_edit$value
        if (clmn > 3L) {
          return()
        }
        markerName <- oldMarkerName
        leaflet::leafletProxy("vrptw_input") %>%
          leaflet::removeMarker(oldMarkerName) %>%
          leaflet::addMarkers(
            lat = rv$markerPositions[[markerName]]$lat, lng = rv$markerPositions[[markerName]]$lng, group = "markers",
            options = leaflet::markerOptions(draggable = TRUE), layerId = markerName, label = markerName
          )
      })
    }
  })


  observe({
    if (length(data()) != 7L && !nrow(data())) {
      return()
    }
    isolate({
      rv$markerPositions <- lapply(seq_len(nrow(data())), function(i) {
        list(
          lat = data()[[2]][i], lng = data()[[3]][i], demand = data()[[4]][i],
          readyTime = data()[[5]][i], dueDate = data()[[6]][i], serviceTime = data()[[7]][i]
        )
      })
      names(rv$markerPositions) <- data()[[1]]
      depotName <<- data()[[1]][1]
    })
  })

  output$vrptw_input <- leaflet::renderLeaflet({
    icons <- leaflet::awesomeIcons(
      icon = "fa-solid fa-warehouse",
      iconColor = "white",
      markerColor = "green",
      library = "fa"
    )
    
    if (length(data()) == 7L && nrow(data())) {
      return(leaflet::leaflet() %>% leaflet::addTiles() %>%
        leaflet::addMarkers(data()[[3L]][-(1)], data()[[2L]][-(1)],
          label = data()[[1L]][-(1)],
          group = "markers", options = leaflet::markerOptions(draggable = TRUE),
          layerId = data()[[1L]][-(1)]
        ) %>%
        leaflet::addAwesomeMarkers(data()[[3L]][1], data()[[2L]][1],
          icon = icons,
          group = "markers",
          label = data()[[1L]][1],
          options  = leaflet::markerOptions(draggable = TRUE),
          layerId = data()[[1L]][1],
        ))
    } else {
      return(leaflet::leaflet() %>% leaflet::addTiles())
    }
  })
  init <- FALSE

  observe({
    input$vrptw_input_marker_click
    if (!init) {
      return()
    }
    isolate({
      markerId <- input$vrptw_input_marker_click$id
      if (markerId != depotName) {
        rv$markerPositions[[markerId]] <<- NULL
        leaflet::leafletProxy("vrptw_input") %>%
          leaflet::removeMarker(markerId)
      }
    })
  })
  observe({
    input$vrptw_input_marker_dragend
    if (!init) {
      return()
    }
    isolate({
      markerId <- input$vrptw_input_marker_dragend$id
      rv$markerPositions[[markerId]]$lat <- input$vrptw_input_marker_dragend$lat
      rv$markerPositions[[markerId]]$lng <- input$vrptw_input_marker_dragend$lng
    })
  })

  observe({
    input$vrptw_input_click
    if (!init) {
      init <<- TRUE
      return()
    }
    isolate({
      markerId <- paste0("Location", markerCnt)
      if (!length(rv$markerPositions)) {
        markerId <<- depotName
      }
      newMarker <- list(
        lat = input$vrptw_input_click$lat,
        lng = input$vrptw_input_click$lng,
        demand = 0L,
        serviceTime = 0L,
        dueDate = 0L,
        readyTime = 0L
      )
      rv$markerPositions[[markerId]] <<- newMarker
      markerCnt <<- markerCnt + 1L

      leaflet::leafletProxy("vrptw_input") %>%
        leaflet::addMarkers(
          lat = newMarker$lat, lng = newMarker$lng, group = "markers",
          options = leaflet::markerOptions(draggable = TRUE), layerId = markerId, label = markerId
        )
    })
  })
  return(reactive({
    rv$customerData
  }))
}
