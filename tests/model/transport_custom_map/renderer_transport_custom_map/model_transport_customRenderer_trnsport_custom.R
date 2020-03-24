trnsport1Output <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  # set default height
  if(is.null(height)){
    height <- 800
  }
  
  leaflet::leafletOutput(ns("trnsport"), height = height)
}

renderTrnsport1 <- function(input, output, session, data, options = NULL, path = NULL, ...){
  data <- data$schedule %>% left_join(data$ilocdata, by = names(data$ilocdata)[1]) %>% 
    left_join(data$jlocdata, by = names(data$jlocdata)[1])
  #generate map
  map <- leaflet() %>%
         addTiles() %>%
         setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>%
      addMarkers(
        lng = data[["lng.x"]], lat = data[["lat.x"]],
        label = paste0(data$i, " (capacity: ", data$cap, ")"),
        labelOptions = labelOptions(noHide = T, textsize = "22px", style = list("background-color" = "rgb(243, 150, 25)"))) %>%
      addMarkers(
        lng = data[["lng.y"]], lat = data[["lat.y"]],
        label = paste(sep = "\n", data$j, " (demand: ", data$demand, ")"),
        labelOptions = labelOptions(closeButton = F, noHide = T, textsize = "22px", style= list("color" = "rgb(243, 150, 25)"))) %>%
    
         addFlows(lng0 = data[["lng.x"]], lat0 = data[["lat.x"]], lng1 = data[["lng.y"]], lat1 = data[["lat.y"]], 
                  color = "indianred", flow = data$quantities, opacity = 1, minThickness = 0, 
                  maxThickness = 12, 
                  layerId = paste0("From ", data$i, " to ", data$j), popup = popupArgs())
  output$trnsport <- leaflet::renderLeaflet(map)
}