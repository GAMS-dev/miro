mirorenderer_xOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  leaflet::leafletOutput(ns("map"), height = height)
}

renderMirorenderer_x <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, attachments = NULL, outputScalarsFull = NULL, ...) {
  depotIcon <- leaflet::awesomeIcons(
    icon = "fa-solid fa-warehouse",
    iconColor = "white",
    markerColor = "green",
    library = "fa"
  )

  xData <- data$x
  customerdataData <- data$customerdata
  tData <- data$t
  wData <- data$w
  activeConnections <- xData[xData$level > 0.5, c("i", "j", "k")]

  flows <- left_join(activeConnections, customerdataData[c("i", "lat", "lng")], by = join_by("i")) %>%
    rename(lat_i = "lat", lng_i = "lng") %>%
    left_join(customerdataData[c("i", "lat", "lng")], by = c(j = "i")) %>%
    rename(lat_j = "lat", lng_j = "lng") %>%
    select(-c("i", "j"))


  names(tData)[names(tData) == "level"] <- "arrivalTime"
  tData["arrivalTime"] <- round(tData["arrivalTime"], 2)
  customerdataData <- left_join(customerdataData, tData[c("i", "arrivalTime")])
  names(wData)[names(wData) == "level"] <- "waitingTime"
  wData["waitingTime"] <- round(wData["waitingTime"], 2)
  customerdataData <- left_join(customerdataData, wData[c("i", "waitingTime")])
  customerdataData["demand"] <- paste("Demand:", customerdataData$demand)
  customerdataData["readyTime"] <- paste("Ready time:", customerdataData$readyTime)
  customerdataData["dueDate"] <- paste("Due date:", customerdataData$dueDate)
  customerdataData["serviceTime"] <- paste("Servicetime:", customerdataData$serviceTime)
  customerdataData["arrivalTime"] <- paste("Arrival time:", customerdataData$arrivalTime)
  customerdataData["waitingTime"] <- paste("Waiting time:", customerdataData$waitingTime)

  markerLabels <- paste(paste0("<b>", htmltools::htmlEscape(customerdataData$i), "</b>"),
    htmltools::htmlEscape(customerdataData$arrivalTime),
    htmltools::htmlEscape(customerdataData$waitingTime),
    htmltools::htmlEscape(customerdataData$dueDate),
    htmltools::htmlEscape(customerdataData$serviceTime),
    htmltools::htmlEscape(customerdataData$demand),
    htmltools::htmlEscape(customerdataData$readyTime),
    sep = "<br>"
  )

  mapData <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(customerdataData$lng[-(1)], customerdataData$lat[-(1)], popup = markerLabels[-(1)], label = customerdataData[["i"]][-(1)]) %>%
    leaflet::addAwesomeMarkers(customerdataData$lng[1], customerdataData$lat[1],
      icon = depotIcon,
      label = customerdataData[["i"]][1],
    )

  flowColors <- c(
    "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928",
    "#f9b9b7", "#ada9b7", "#66101F", "#c45ab3", "#1BE7FF", "#4C9F70",
    "#f0f757", "#9E6D42", "#086788", "#E0CA3C", "#BA9790", "#EB4511",
    "#9B5DE5", "#47fa1a", "#38618c", "#fad8d6", "#373d20", "#210b2c",
    "#d81159", "#08bdbd", "#35ff69", "#6d213c", "#dcf763", "#e06c00",
    "#e9d758", "#829191", "#E8998D", "#91b696", "#714955", "#2a2a72",
    "#00ffc5", "#6c3a5c", "#8b1e3f", "#3E721D"
  )
  i <- 1L
  for (tour_id in unique(flows[["k"]])) {
    flowDataTmp <- flows[flows$k == tour_id, ]
    mapData <- leaflet.minicharts::addFlows(
      mapData,
      flowDataTmp$lng_i, flowDataTmp$lat_i, flowDataTmp$lng_j, flowDataTmp$lat_j,
      flow = 0.5, maxThickness = 4, color = flowColors[i]
    )
    i <- i + 1L
  }

  output$map <- leaflet::renderLeaflet(mapData)
}
