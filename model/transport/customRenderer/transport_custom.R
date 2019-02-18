trnsport1Output <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  # set default height
  if(is.null(height)){
    height <- 800
  }
  
  leaflet::leafletOutput(ns("trnsport"), height = height)
}

renderTrnsport1 <- function(input, output, session, data, options = NULL, path = NULL){

  tryCatch({
    mapData <- read_sf(file.path(path, options$geojsonloc))
  }, error = function(e){
    stop(paste0("Problems reading GEOJSON file. Error message: ", e), call. = FALSE)
  })
  
  labels <- mapData$name
  
  # generate a color palette
  getPalette <- colorRampPalette(brewer.pal(12, "Set3"))
  pal        <- colorFactor(palette = getPalette(length(unique(labels))), mapData$id)

  #generate map
  map <- leaflet(mapData) %>%
         addTiles() %>%
         setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>%
         addPolygons(smoothFactor = 0.5, fillOpacity = 0.4, 
                     highlightOptions = highlightOptions(color = "white", weight = 1), 
                     color = ~pal(id), label = labels,
                     labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                                     textsize = "15px", direction = "auto")) %>%
      addMarkers(
        lng = data$lngp, lat = data$latp,
        label = paste0(data$i, " (capacity: ", data$cap, ")"),
        labelOptions = labelOptions(noHide = T, textsize = "22px", style = list("background-color" = "rgb(243, 150, 25)"))) %>%
      addMarkers(
        lng = data$lngm, lat = data$latm,
        label = paste(sep = "\n", data$j, " (demand: ", data$demand, ")"),
        labelOptions = labelOptions(closeButton = F, noHide = T, textsize = "22px", style= list("color" = "rgb(243, 150, 25)"))) %>%
    
         addFlows(lng0 = data$lngp, lat0 = data$latp, lng1 = data$lngm, lat1 = data$latm, 
                  color = "indianred", flow = data$quantities, opacity = 1, minThickness = 0, 
                  maxThickness = 12, 
                  layerId = paste0("From ", data$i, " to ", data$j), popup = popupArgs())
  output$trnsport <- leaflet::renderLeaflet(map)
}