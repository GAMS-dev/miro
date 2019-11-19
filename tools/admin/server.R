source(file.path("tools", "admin", "util.R"))

appDisconnected <- FALSE
oneLayerEl <- c("dygraphs")
twoLayerEl <- c("pie", "hist")
configJSONFileName <- file.path(currentModelDir, "conf", 
                                modelName %+% ".json")
dateFormatChoices <- c("1910-06-22" = "yyyy-mm-dd", "22.06.1910" = "dd.mm.yyyy")

outputSymMultiDimChoices <- setNames(names(modelOut), 
                                     paste0(names(modelOut), ": ", modelOutAlias))

inputSymMultiDim <- setNames(names(modelInRaw), 
                             vapply(modelInRaw, "[[", 
                                    character(1L), "alias", USE.NAMES = FALSE))
inputSymMultiDimChoices <- setNames(names(modelInRaw), vapply(seq_along(modelInRaw), function(idx){
  paste0(names(modelInRaw)[[idx]], ": ", modelInRaw[[idx]][["alias"]])}, character(1L), USE.NAMES = FALSE))

inputSymHeaders <- lapply(inputSymMultiDim, function(el){
  headers <- modelInRaw[[el]]$headers
  return(setNames(names(headers), 
                  vapply(headers, 
                         "[[", character(1L), "alias", USE.NAMES = FALSE)))
})
inputSymHeaderChoices <- lapply(inputSymMultiDim, function(el){
  headers <- modelInRaw[[el]]$headers
  return(setNames(names(headers), vapply(seq_along(headers), function(idx){
    paste0(names(headers)[idx], ": ", headers[[idx]][["alias"]])}, character(1L),  USE.NAMES = FALSE)))
})

widgetSymbols <- c(unlist(lapply(seq_along(modelIn), function(idx){
  if(identical(modelIn[[idx]][["symtype"]], "set") &&
     length(modelIn[[idx]]$headers) == 2L){
    return(names(modelIn)[idx])
  }else if(length(modelIn[[idx]][["headers"]])){
    return(NULL)
  }
  return(names(modelIn)[idx])
}), use.names = FALSE),
if(length(modelIn[[scalarsFileName]]))
  modelIn[[scalarsFileName]]$symnames)

if(length(widgetSymbols)){
  widgetSymbolIds <- match(widgetSymbols, names(modelIn))
  widgetSymbolIds <- widgetSymbolIds[!is.na(widgetSymbolIds)]
  widgetSymbolsChoices <- setNames(widgetSymbols, paste0(widgetSymbols, ": ", c(
    modelInAlias[widgetSymbolIds], 
    if(length(modelIn[[scalarsFileName]]))
      modelIn[[scalarsFileName]]$symtext
  )))
  widgetSymbols <- setNames(widgetSymbols, c(
    modelInAlias[widgetSymbolIds], 
    if(length(modelIn[[scalarsFileName]]))
      modelIn[[scalarsFileName]]$symtext
  ))
}else{
  widgetSymbolsChoices <- character(0L)
  widgetSymbols <- character(0L)
}
if(length(inputSymHeaders)){
  names(inputSymHeaders) <- unname(inputSymMultiDim)
}else{
  inputSymHeaders <- character(0L)
}

outputSymHeaders <- lapply(modelOut, function(el){
  vapply(el$headers, "[[", 
    character(1L), "alias", USE.NAMES = FALSE)
})
outputSymHeaderChoices <- lapply(modelOut, function(el){
  vapply(seq_along(el$headers),  function(idx){
    paste0(names(el$headers)[[idx]], ": ", el$headers[[idx]][["alias"]])}, 
    character(1L), USE.NAMES = FALSE)
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
                       saveWidgetConfirm = 0L, updateLeafletGroups = 0L, 
                       saveTableConfirm = 0L, widgetTableConfig = list(), table_symbol = 0L,
                       reset_table_input = 0L)
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
  #     Input widgets
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_widgets.R"), local = TRUE)
  # ------------------------------------------------------
  #     Table settings
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_tables.R"), local = TRUE)  
  # ------------------------------------------------------
  #     CUSTOMIZE GRAPHS
  # ------------------------------------------------------
  source(file.path("tools", "admin", "cg_graphs.R"), local = TRUE)
  # ------------------------------------------------------
  #     DB MANAGEMENT
  # ------------------------------------------------------
  source(file.path("tools", "admin", "db_management.R"), local = TRUE)
  
  observeEvent(input$fetchUpdateString, {
    insertUI(".miro-update-text", where = "afterBegin", 
             HTML(getUpdateString()))
  })
  
  hideEl(session, "#loading-screen")
  session$onSessionEnded(function() {
    appDisconnected <<- TRUE
    if(!interactive()){
      stopApp()
    }
  })
}