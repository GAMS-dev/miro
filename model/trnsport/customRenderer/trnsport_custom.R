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
         addPolygons(smoothFactor = 0.5, fillOpacity = 0.4, 
                     highlightOptions = highlightOptions(color = "white", weight = 1), 
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
        label = paste0(data[["canning plants"]], " (capacity: ", data$cap, ")"),
        labelOptions = labelOptions(noHide = T, textsize = "22px", style = list("background-color" = "rgb(243, 150, 25)"))) %>%
      addMarkers(
        lng = data$lngM, lat = data$latM,
        label = paste(sep = "\n", data$markets, " (demand: ", data$demand, ")"),
        labelOptions = labelOptions(closeButton = F, noHide = T, textsize = "22px", style= list("color" = "rgb(243, 150, 25)"))) %>%
    
         addFlows(lng0 = data$lngP, lat0 = data$latP, lng1 = data$lngM, lat1 = data$latM, 
                  color = "indianred", flow = data$quantities, 
                  time = data[['time steps']], opacity = 1, minThickness = 0, 
                  maxThickness = 12, 
                  layerId = paste0("From ", data[["canning plants"]], " to ", data$markets), popup = popupArgs())
  output$trnsport <- leaflet::renderLeaflet(map)
}