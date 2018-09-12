simple1Output <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  # set default height
  if(is.null(height)){
    height <- 800
  }
  
  leaflet::leafletOutput(ns("simple"), height = height)
}

renderSimple1 <- function(input, output, server, data, options = NULL, path = NULL){

  tryCatch({
    map.data <- read_sf(paste0(path, options$geojsonloc))
  }, error = function(e){
    stop(paste0("Problems reading GEOJSON file. Error message: ", e), call. = FALSE)
  })
  labels <- map.data$NAME_1
  
  # generate a color palette
  getPalette = colorRampPalette(brewer.pal(12, "Set3"))
  pal <- colorFactor(palette = getPalette(length(unique(labels))), map.data$ID_1)

  #generate map
  map <- leaflet(map.data) %>%
         addTiles() %>%
         addPolygons(smoothFactor = 0.5, fillOpacity = 0.85, highlightOptions = highlightOptions(color = "white", weight = 2), color = ~pal(ID_1), label = labels,
                     labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
         addMinicharts(lng = data$lng, lat = data$lat, time = data[['time steps']], type = "pie", chartdata = data[, c("Renewable", "Fossil")], 
                       width = 100 * data$Total / max(data$Total), transitionTime = 0, layerId = labels, popup = popupArgs())
  output$simple <- leaflet::renderLeaflet(map)
}

simple2Output <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  if(is.null(height)){
    height <- 800
  }
  leaflet::leafletOutput(ns("simple"), height = height)
}

renderSimple2 <- function(input, output, server, data, options = NULL, path = NULL){

  tryCatch({
    map.data <- sf::read_sf(paste0(path, options$geojsonloc))
  }, error = function(e){
    stop(paste0("Problems reading GEOJSON file. Error message: ", e), call. = FALSE)
  })
  labels <- map.data$NAME_1
  
  #mapping of Ids (from GAMS) and labels (R)
  map.id.name <- as.list(labels)
  names(map.id.name) <- sapply(1:16, function(i){ paste0("ID_", i)})
  label0 <- sapply(data$Regions, function(el){map.id.name[[el]]})
  label1 <- sapply(data$Regions1, function(el){map.id.name[[el]]})
  
  # generate a color palette
  getPalette = colorRampPalette(brewer.pal(12, "Set3"))
  pal <- colorFactor(palette = getPalette(length(unique(labels))), map.data$ID_1)

  map <- leaflet(map.data) %>%
         addTiles() %>%
         addPolygons(smoothFactor = 0.5, fillOpacity = 0.8, highlightOptions = highlightOptions(color = "white", weight = 2), color = ~pal(ID_1),
                     labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
         addFlows(lng0 = data$Lng0, lat0 = data$Lat0, lng1 = data$Lng1, lat1 = data$Lat1, color = "indianred", flow = data$Flow, 
                  time = data[['time steps']], opacity = 1, minThickness = 1, maxThickness = 12, 
                  #Popup for flows - label0 and label1 need to be filled before
                  layerId = paste0("From ", label0, " to ", label1), popup = popupArgs())
  
  
  output$simple <- leaflet::renderLeaflet(map)
}