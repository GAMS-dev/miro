simple1Output <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  # set default height
  if(is.null(height)){
    height <- 800
  }
  
  leaflet::leafletOutput(ns("simple"), height = height)
}

renderSimple1 <- function(input, output, session, data, options = NULL, path = NULL){

  #generate map
  map <- leaflet() %>%
         addTiles() %>%
         addMinicharts(lng = data$lng, lat = data$lat, time = data$tt, 
                       type = "pie", chartdata = data[, c("renewable", "fossil")], 
                       width = 100 * data$total / max(data$total), transitionTime = 0, 
                       popup = popupArgs())
  output$simple <- leaflet::renderLeaflet(map)
}

simple2Output <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  if(is.null(height)){
    height <- 800
  }
  leaflet::leafletOutput(ns("simple"), height = height)
}

renderSimple2 <- function(input, output, session, data, options = NULL, path = NULL){

  map <- leaflet() %>%
         addTiles() %>%
         addFlows(lng0 = data$lng0, lat0 = data$lat0, lng1 = data$lng1, lat1 = data$lat1, 
                  color = "indianred", flow = data$flow, 
                  time = data$tt, opacity = 1, minThickness = 1, 
                  maxThickness = 12, 
                  #Popup for flows - label0 and label1 need to be filled before
                  popup = popupArgs())
  
  
  output$simple <- leaflet::renderLeaflet(map)
}