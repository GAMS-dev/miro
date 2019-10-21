source(file.path("tools", "admin", "util.R"))

appDisconnected <- FALSE
oneLayerEl <- c("dygraphs")
twoLayerEl <- c("pie", "hist")
configJSONFileName <- file.path(currentModelDir, "conf", 
                                modelName %+% ".json")
dateFormatChoices <- c("1910-06-22" = "yyyy-mm-dd", "22.06.1910" = "dd.mm.yyyy")

inputSymMultiDim <- setNames(names(modelInRaw), vapply(modelInRaw, "[[", character(1L), "alias", USE.NAMES = FALSE))
inputSymHeaders <- lapply(inputSymMultiDim, function(el){
  headers <- modelInRaw[[el]]$headers
  return(setNames(names(headers), vapply(headers, "[[", character(1L), "alias", USE.NAMES = FALSE)))
})
names(inputSymHeaders) <- unname(inputSymMultiDim)
outputSymHeaders <- lapply(modelOut, function(el){
  vapply(el$headers, "[[", character(1L), "alias", USE.NAMES = FALSE)
})
allInputSymHeaders <- setNames(unlist(inputSymHeaders, use.names = FALSE), 
                               unlist(lapply(inputSymHeaders, names), use.names = FALSE))
allInputSymHeaders <- allInputSymHeaders[!duplicated(allInputSymHeaders)]

configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName, 
                                                  simplifyDataFrame = FALSE, 
                                                  simplifyMatrix = FALSE))

server_admin <- function(input, output, session){
  rv <- reactiveValues(plotly_type = 0L, saveGraphConfirm = 0L, resetRE = 0L,
                       graphConfig = list(outType = "graph", graph = list()), 
                       widgetConfig = list(), generalConfig = list(), customLogoChanged = 1L,
                       initData = FALSE, refreshContent = 0L, widget_type = 0L, widget_symbol = 0L, 
                       saveWidgetConfirm = 0L, updateLeafletGroups = 0L)
  session$sendCustomMessage("gms-setGAMSSymbols", list(gamsSymbols = list(inSym = unname(inputSymMultiDim), 
                                                                          inAlias = names(inputSymMultiDim),
                                                                          outSym = names(modelOut),
                                                                          outAlias = modelOutAlias),
                                                       lang = lang$adminMode$graphs$js))
  # ------------------------------------------------------
  #     General settings
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_general.R"), local = TRUE)  
  # ------------------------------------------------------
  #     Table settings
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_tables.R"), local = TRUE)  
  # ------------------------------------------------------
  #     CUSTOMIZE GRAPHS
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_graphs.R"), local = TRUE)
  # ------------------------------------------------------
  #     Input widgets
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_widgets.R"), local = TRUE)
  
  # ------------------------------------------------------
  #     DB MANAGEMENT
  # ------------------------------------------------------
  source(file.path("tools", "admin", "db_management.R"), local = TRUE)
  
  hideEl(session, "#loading-screen")
  session$onSessionEnded(function() {
    appDisconnected <<- TRUE
    if(!interactive()){
      stopApp()
    }
  })
}