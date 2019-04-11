appDisconnected <- FALSE
oneLayerEl <- c("dygraphs")
twoLayerEl <- c("pie", "hist")
configJSONFileName <- paste0(currentModelDir, configDir, 
                             modelName, ".json")
addArrayEl <- function(session, arrayID, plotly_chart_type = ""){
  arrayID <- paste0(arrayID, plotly_chart_type)
  session$sendCustomMessage("gms-addArrayEl", arrayID)
  HTML(paste0('<div id="', arrayID, '_wrapper" class="shiny-input-container" style="margin:20px;">\n
 <hr>\n
 <div class="array-wrapper"></div>\n
 <button onclick="addArrayDataEl(\'', arrayID, '\')" type="button" class="btn btn-default bt-icon btn-add-array-el" style="font-size:20px;">\n
   <i class="far fa-plus-square"></i>\n
 </button>\n
</div>'))
}
optionSection <- function(title, ..., collapsed = FALSE){
  tags$div(class = "shiny-input-container",
           tags$h4(class = "box-title option-section-header", title, icon("plus"), style = "cursor:pointer", 
                   onclick = "$(this).next().toggle();"),
           tags$div(class = "option-section", ..., style = if(collapsed) "display:none;" else "")
           )
}
colorPickerInput <- function(id, value, label = NULL){
  HTML(paste0('<div class="form-group shiny-input-container">
    <label for="', id, '">', label, '</label>
      <input id="', id, '" type="text" class="form-control miro-color-picker" value="', value, '" />'))
}
inputSymMultiDim <- setNames(names(modelIn), modelInAlias)
inputSymMultiDim <- inputSymMultiDim[vapply(modelIn, function(el){
  if(is.null(el$headers))
    return(FALSE)
  else
    return(TRUE)}, logical(1L), USE.NAMES = FALSE)]
inputSymHeaders <- lapply(inputSymMultiDim, function(el){
  headers <- modelIn[[el]]$headers
  return(setNames(names(headers), vapply(headers, "[[", character(1L), "alias", USE.NAMES = FALSE)))
})
names(inputSymHeaders) <- unname(inputSymMultiDim)
allInputSymHeaders <- setNames(unlist(inputSymHeaders, use.names = FALSE), unlist(lapply(inputSymHeaders, names), use.names = FALSE))
allInputSymHeaders <- allInputSymHeaders[!duplicated(allInputSymHeaders)]

server_admin <- function(input, output, session){
  rv <- reactiveValues(plotly_type = 0L, saveGraphConfirm = 0L, resetRE = 0L,
                       graphConfig = list(outType = "graph", graph = list()), 
                       widgetConfig = list(), generalConfig = list(), customLogoChanged = 1L,
                       initData = FALSE, widget_type = 0L, widget_symbol = 0L, saveWidgetConfirm = 0L)
  configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName, 
                                                    simplifyDataFrame = FALSE, 
                                                    simplifyMatrix = FALSE))
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