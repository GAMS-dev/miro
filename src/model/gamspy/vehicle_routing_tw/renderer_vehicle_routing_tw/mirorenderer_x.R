mirorenderer_xOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  tagList(
    textOutput(ns("title")),
    leaflet::leafletOutput(ns("map"))
  )
}

renderMirorenderer_x <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, attachments = NULL, outputScalarsFull = NULL, ...) {
  output$title <- renderText(options$title)

  xData <- data$x
  customerdataData <- data$customerdata
  tData <- data$t
  wData <- data$w
  activeConnections <- xData[xData$level == 1, ][c("i", "j", "k")]

  flows <- left_join(activeConnections, customerdataData[c("i", "lat", "lng")], "i") %>%
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
  customerdataData["demand"] <- paste(rep("Demand:", nrow(customerdataData)), customerdataData$demand, sep = " ")
  customerdataData["readyTime"] <- paste(rep("Ready time:", nrow(customerdataData)), customerdataData$readyTime, sep = " ")
  customerdataData["dueDate"] <- paste(rep("Due date:", nrow(customerdataData)), customerdataData$dueDate, sep = " ")
  customerdataData["serviceTime"] <- paste(rep("Servicetime:", nrow(customerdataData)), customerdataData$serviceTime, sep = " ")
  customerdataData["arrivalTime"] <- paste(rep("Arrival time", nrow(customerdataData)), customerdataData$arrivalTime, sep = " ")
  customerdataData["waitingTime"] <- paste(rep("Waiting time", nrow(customerdataData)), customerdataData$waitingTime, sep = " ")

  markerLabels <- paste(customerdataData$arrivalTime, customerdataData$waitingTime, sep = "<br>") %>%
    paste(customerdataData$dueDate, customerdataData$serviceTime, sep = "<br>") %>%
    paste(customerdataData$demand, customerdataData$readyTime, sep = "<br>")

  mapData <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers(customerdataData$lng, customerdataData$lat, popup = markerLabels)

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
