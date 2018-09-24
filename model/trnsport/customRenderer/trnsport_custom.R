trnsport1Output <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  # set default height
  if(is.null(height)){
    height <- 800
  }
  
  leaflet::leafletOutput(ns("trnsport"), height = height)
}

renderTrnsport1 <- function(input, output, server, data, options = NULL, path = NULL){

  tryCatch({
    map.data <- read_sf(paste0(path, options$geojsonloc))
  }, error = function(e){
    stop(paste0("Problems reading GEOJSON file. Error message: ", e), call. = FALSE)
  })
  
  labels <- map.data$name
  
  # generate a color palette
  getPalette = colorRampPalette(brewer.pal(12, "Set3"))
  pal <- colorFactor(palette = getPalette(length(unique(labels))), map.data$id)

  #generate map
  map <- leaflet(map.data) %>%
         addTiles() %>%
         setView(lng = -98.5795, lat = 39.8283, zoom = 5) %>%
         addPolygons(smoothFactor = 0.5, fillOpacity = 1, 
                     highlightOptions = highlightOptions(color = "white", weight = 2), 
                     color = ~pal(id), label = labels,
                     labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                                     textsize = "15px", direction = "auto")) %>%
   #       addPopups(
   #         data$lngP,data$latP,
   #         paste(sep = "<br/>",
   #               "<b style = 'font-size: 16px;color:#008f01'>" %+% 
   #                 htmltools::htmlEscape(data[["canning plants"]]) %+% "</b>",
   #               "Capacity: " %+% htmltools::htmlEscape(data$cap)
   #         ), options = popupOptions(closeButton = FALSE)
   #       ) %>%
   # addPopups(
   #   data$lngM,data$latM,
   #   paste(sep = "<br/>",
   #         "<b style = 'font-size: 16px;color:#c60000'>" %+% 
   #           htmltools::htmlEscape(data$markets) %+% "</b>",
   #         "Demand: " %+% htmltools::htmlEscape(data$demand)
   #   ), options = popupOptions(closeButton = FALSE)
   # ) %>%
      addMarkers(
        lng = data$lngP, lat = data$latP,
        label = paste0(data[["canning plants"]], "(Capacity: ", data$cap, ")"),
        labelOptions = labelOptions(noHide = T, textsize = "15px", style = list("color" = "#008f01"))) %>%
      addMarkers(
        lng = data$lngM, lat = data$latM,
        label = paste(sep = "\n", data$markets, "(Demand: ", data$demand, ")"),
        labelOptions = labelOptions(noHide = T, textsize = "15px", style= list("color" = "#2E86C1"))) %>%
    
         addFlows(lng0 = data$lngP, lat0 = data$latP, lng1 = data$lngM, lat1 = data$latM, 
                  color = "indianred", flow = data$quantities, 
                  time = data[['time steps']], opacity = 1, minThickness = 0, 
                  maxThickness = 12, 
                  layerId = paste0("From ", data[["canning plants"]], " to ", data$markets), popup = popupArgs())
  output$trnsport <- leaflet::renderLeaflet(map)
}
#
#renderSimple2 <- function(input, output, server, data, options = NULL, path = NULL){
#  
#  
#  #mapping of Ids (from GAMS) and labels (R)
#  map.id.name <- as.list(labels)
#  names(map.id.name) <- sapply(1:16, function(i){ paste0("ID_", i)})
#  label0 <- sapply(data$Regions, function(el){map.id.name[[el]]})
#  label1 <- sapply(data$Regions1, function(el){map.id.name[[el]]})
#  
#  
#    addFlows(lng0 = data$Lng0, lat0 = data$Lat0, lng1 = data$Lng1, lat1 = data$Lat1, 
#             color = "indianred", flow = data$Flow, 
#             time = data[['time steps']], opacity = 1, minThickness = 1, 
#             maxThickness = 12, 
#             #Popup for flows - label0 and label1 need to be filled before
#             layerId = paste0("From ", label0, " to ", label1), popup = popupArgs())
#  
#  
#  output$simple <- leaflet::renderLeaflet(map)
#}