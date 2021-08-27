trnsport1Output <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  # set default height
  if (is.null(height)) {
    height <- 800
  }
  tagList(
    textOutput(ns("path")),
    leaflet::leafletOutput(ns("trnsport"))
  )
}

renderTrnsport1 <- function(input, output, session, data, options = NULL, path = NULL, ...) {
  data <- data$schedule %>%
    left_join(data$ilocdata, by = names(data$ilocdata)[1]) %>%
    left_join(data$jlocdata, by = names(data$jlocdata)[1])
  # generate map
  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>%
    leaflet::addMarkers(
      lng = data[["lng.x"]], lat = data[["lat.x"]],
      label = paste0(data$i, " (capacity: ", data$cap, ")"),
      labelOptions = leaflet::labelOptions(
        noHide = TRUE, textsize = "22px",
        style = list("background-color" = "rgb(243, 150, 25)")
      )
    ) %>%
    leaflet::addMarkers(
      lng = data[["lng.y"]], lat = data[["lat.y"]],
      label = paste(sep = "\n", data$j, " (demand: ", data$demand, ")"),
      labelOptions = leaflet::labelOptions(
        closeButton = FALSE, noHide = TRUE,
        textsize = "22px", style = list("color" = "rgb(243, 150, 25)")
      )
    ) %>%
    leaflet.minicharts::addFlows(
      lng0 = data[["lng.x"]], lat0 = data[["lat.x"]],
      lng1 = data[["lng.y"]], lat1 = data[["lat.y"]],
      color = "indianred",
      flow = coalesce(data$quantities, 0), opacity = 1, minThickness = 0,
      maxThickness = 12,
      layerId = paste0("From ", data$i, " to ", data$j),
      popup = leaflet.minicharts::popupArgs()
    )
  output$path <- renderText({
    path
  })
  output$trnsport <- leaflet::renderLeaflet(map)
}
