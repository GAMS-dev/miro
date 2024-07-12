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
    )
  )
}

renderMirowidget_customerdata <- function(input, output, session, data, options = NULL, path = NULL, ...) {
  markerCnt <- 0L
  depotName <- "Depot"
  init <- FALSE

  depotIcon <- leaflet::awesomeIcons(
    icon = "fa-solid fa-warehouse",
    iconColor = "white",
    markerColor = "green",
    library = "fa"
  )

  rv <- reactiveValues(
    markerPositions = list(),
    customerData = tibble(
      i = character(), lat = numeric(), lng = numeric(),
      demand = numeric(), readyTime = numeric(), dueDate = numeric(), serviceTime = numeric()
    )
  )

  tableProxy <- DT::dataTableProxy("table")

  resetTable <- function() {
    DT::replaceData(tableProxy, isolate(rv$customerData), resetPaging = FALSE, rownames = FALSE)
  }

  updateCustomerData <- function() {
    customerDataTmp <- tibble(
      i = names(rv$markerPositions),
      lat = vapply(rv$markerPositions, "[[", numeric(1L), "lat"),
      lng = vapply(rv$markerPositions, "[[", numeric(1L), "lng"),
      demand = vapply(rv$markerPositions, "[[", numeric(1L), "demand"),
      readyTime = vapply(rv$markerPositions, "[[", numeric(1L), "readyTime"),
      dueDate = vapply(rv$markerPositions, "[[", numeric(1L), "dueDate"),
      serviceTime = vapply(rv$markerPositions, "[[", numeric(1L), "serviceTime")
    )
    rv$customerData <- customerDataTmp
    resetTable()
  }

  output$table <- DT::renderDT({
    DT::datatable(data(), editable = TRUE, rownames = FALSE) %>% DT::formatRound(c("lat", "lng"), digits = 3L)
  })

  observe({
    input$table_cell_edit
    if (!init) {
      return()
    }
    {
      row <- input$table_cell_edit$row
      clmn <- input$table_cell_edit$col + 1

      if (input$table_cell_edit$value == "") {
        resetTable()
        return()
      }

      isolate({
        oldMarkerName <- rv$customerData[["i"]][row]
        markerName <- oldMarkerName
        if (clmn == 1) {
          idx <- match(oldMarkerName, names(rv$markerPositions))
          markerName <- input$table_cell_edit$value
          if (markerName %in% names(rv$markerPositions)) {
            resetTable()
            return()
          }
          names(rv$markerPositions)[idx] <- markerName
          if (oldMarkerName == depotName) {
            depotName <<- markerName
          }
        } else {
          rv$markerPositions[[oldMarkerName]][[names(rv$customerData)[clmn]]] <- input$table_cell_edit$value
        }
        updateCustomerData()
        if (clmn > 3L) {
          return()
        }

        if (markerName == depotName) {
          leaflet::leafletProxy("vrptw_input") %>%
            leaflet::removeMarker(oldMarkerName) %>%
            leaflet::addAwesomeMarkers(
              lat = rv$markerPositions[[markerName]]$lat, lng = rv$markerPositions[[markerName]]$lng,
              icon = depotIcon,
              group = "markers",
              label = markerName,
              options = leaflet::markerOptions(draggable = TRUE),
              layerId = markerName,
            )
        } else {
          leaflet::leafletProxy("vrptw_input") %>%
            leaflet::removeMarker(oldMarkerName) %>%
            leaflet::addMarkers(
              lat = rv$markerPositions[[markerName]]$lat, lng = rv$markerPositions[[markerName]]$lng, group = "markers",
              options = leaflet::markerOptions(draggable = TRUE), layerId = markerName, label = markerName
            )
        }
      })
    }
  })


  observe({
    dataTmp <- data()
    isolate({
      rv$markerPositions <- lapply(seq_len(nrow(dataTmp)), function(i) {
        list(
          lat = dataTmp[[2]][i], lng = dataTmp[[3]][i], demand = dataTmp[[4]][i],
          readyTime = dataTmp[[5]][i], dueDate = dataTmp[[6]][i], serviceTime = dataTmp[[7]][i]
        )
      })
      if (length(rv$markerPositions)) {
        names(rv$markerPositions) <- dataTmp[[1]]
        depotName <<- dataTmp[[1]][1]
      } else {
        depotName <<- "Depot"
      }
      updateCustomerData()
    })
  })

  output$vrptw_input <- leaflet::renderLeaflet({
    if (length(data()) == 7L && nrow(data())) {
      return(leaflet::leaflet() %>% leaflet::addTiles() %>%
        leaflet::addMarkers(data()[[3L]][-(1)], data()[[2L]][-(1)],
          label = data()[[1L]][-(1)],
          group = "markers", options = leaflet::markerOptions(draggable = TRUE),
          layerId = data()[[1L]][-(1)]
        ) %>%
        leaflet::addAwesomeMarkers(data()[[3L]][1], data()[[2L]][1],
          icon = depotIcon,
          group = "markers",
          label = data()[[1L]][1],
          options = leaflet::markerOptions(draggable = TRUE),
          layerId = data()[[1L]][1],
        ))
    } else {
      return(leaflet::leaflet() %>% leaflet::addTiles())
    }
  })

  observe({
    input$vrptw_input_marker_click
    if (!init) {
      return()
    }
    isolate({
      markerId <- input$vrptw_input_marker_click$id
      if (markerId != depotName) {
        rv$markerPositions[[markerId]] <<- NULL
        updateCustomerData()
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
      updateCustomerData()
    })
  })

  observe({
    input$vrptw_input_click
    if (!init) {
      init <<- TRUE
      return()
    }
    isolate({
      # If an existing marker was renamed to e.g Location6 this would overwrite the marker
      # In this demo app this will not be handeled, for a production ready app this should be taken into account
      markerId <- paste0("Location", markerCnt)
      newMarker <- list(
        lat = input$vrptw_input_click$lat,
        lng = input$vrptw_input_click$lng,
        demand = 0L,
        serviceTime = 0L,
        dueDate = 0L,
        readyTime = 0L
      )
      if (!length(rv$markerPositions)) {
        markerId <- depotName
      }

      rv$markerPositions[[markerId]] <<- newMarker

      updateCustomerData()
      markerCnt <<- markerCnt + 1L

      if (markerId == depotName) {
        leaflet::leafletProxy("vrptw_input") %>%
          leaflet::addAwesomeMarkers(
            lat = newMarker$lat, lng = newMarker$lng,
            icon = depotIcon,
            group = "markers",
            label = markerId,
            options = leaflet::markerOptions(draggable = TRUE),
            layerId = markerId,
          )
      } else {
        leaflet::leafletProxy("vrptw_input") %>%
          leaflet::addMarkers(
            lat = newMarker$lat, lng = newMarker$lng, group = "markers",
            options = leaflet::markerOptions(draggable = TRUE), layerId = markerId, label = markerId
          )
      }
    })
  })
  return(reactive({
    rv$customerData
  }))
}
