activeSymbol <- list(id = integer(1L), name = character(1L), 
                     alias = character(1L), indices = c())


modelInputData   <- vector("list", length(modelIn))
modelOutputData  <- vector("list", length(modelOut))
scalarOutputData <- tibble()
hotInit          <- vector("logical", length(modelIn))
isEmptyInput     <- vector(mode = "logical", length = length(modelIn))
isEmptyOutput    <- vector(mode = "logical", length = length(modelOut))
isEmptyOutput[]  <- TRUE
inputInitialized <- vector(mode = "logical", length = length(modelInWithDep))
currentSelection <- list("plotly", "pie")
idLabelMap       <- list(chart_ydata = list(), animation_options = list(), hist_xdata = list(), dy_dyEvent = list(), 
                         dy_dyLimit = list(), leaflet_markers = list(), leaflet_flows = list(), leaflet_minicharts = list(), 
                         timevis_series = list())
currentConfig    <- list()
optionsInserted  <- c()
noOutputData     <- TRUE
allDataAvailable <- FALSE
leafletGroups    <- CharArray$new()

validateGraphConfig <- function(graphJSON){
  if(identical(is.na(graphJSON$graph$xaxis$rangefrom), TRUE) || identical(is.na(graphJSON$graph$xaxis$rangeto), TRUE)){
    return("Could not set range for x axis! Check for correct data format!")
  }
  return("")
}
saveAndReload <- function(...){
  selected    <- list(...)
  subsetIdx   <- 3L
  if(currentSelection[[1]] %in% oneLayerEl){
    subsetIdx <- 1L
  }else if(currentSelection[[2]] %in% twoLayerEl){
    subsetIdx <- 2L
  }
  currentConfig[[paste(currentSelection[seq_len(subsetIdx)], collapse = "")]] <<- isolate(rv$graphConfig$graph)
  subsetIdx   <- 3L
  if(selected[[1]] %in% oneLayerEl){
    subsetIdx <- 1L
  }else if(selected[[2]] %in% twoLayerEl){
    subsetIdx <- 2L
  }
  currentSelection <<- selected[seq_len(subsetIdx)]
  current <- paste(currentSelection, collapse = "")
  if(length(currentConfig[[current]])){
    isolate(rv$graphConfig$graph <- currentConfig[[current]])
    allDataAvailable <<- TRUE
  }else if(subsetIdx == 1L){
    isolate({
      title <- rv$graphConfig$graph$title
      rv$graphConfig$graph <- list()
      rv$graphConfig$graph$title <- title
    })
  }else{
    isolate({
      title <- rv$graphConfig$graph$title
      tool  <- rv$graphConfig$graph$tool
      rv$graphConfig$graph <- list()
      rv$graphConfig$graph$title <- title
      rv$graphConfig$graph$tool  <- tool
    })
  }
}
changeActiveSymbol <- function(id){
  if(id <= length(modelIn)){
    headers       <- modelIn[[id]]$headers
    headerAliases <- vapply(headers, '[[', character(1L), 
                            "alias", USE.NAMES = FALSE)
    activeSymbol  <<- list(id = id, name = names(modelIn)[id], 
                           alias = modelInAlias[id],
                           indices = setNames(names(headers), 
                                              headerAliases), 
                           indexTypes = vapply(headers, '[[', character(1L), 
                                               "type", USE.NAMES = FALSE))
    scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "parameter"]
    session$sendCustomMessage("gms-setIndices", list(indices = names(headers), 
                                                     aliases = headerAliases,
                                                     scalarIndices = unname(scalarIndices),
                                                     scalarAliases = names(scalarIndices)))
  }else{
    id_out <- id - length(modelIn)
    headers <- modelOut[[id_out]]$headers
    headerAliases <- vapply(headers, '[[', character(1L), 
                            "alias", USE.NAMES = FALSE)
    activeSymbol <<- list(id = id, name = names(modelOut)[id_out], 
                          alias = modelOutAlias[id_out],
                          indices = setNames(names(headers), 
                                             headerAliases), 
                          indexTypes = vapply(headers, '[[', character(1L), 
                                              "type", USE.NAMES = FALSE))
    scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "parameter"]
    session$sendCustomMessage("gms-setIndices", list(indices = names(headers), 
                                                     aliases = headerAliases,
                                                     scalarIndices = unname(scalarIndices),
                                                     scalarAliases = names(scalarIndices)))
  }
  indices       <- activeSymbol$indices
  rv$graphConfig$graph$title <- activeSymbol$alias
  rv$initData <- FALSE
  rv$initData <- TRUE
}
observeEvent(input$localInput, {
  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  rv$initData <- FALSE
  # read Excel file
  tryCatch({
    xlsWbNames <- excel_sheets(input$localInput$datapath)
  }, error = function(e) {
    flog.error("Some error occurred reading the file: '%s'. Error message: %s.", 
               as.character(isolate(input$localInput$name)), e)
    errMsg <<- sprintf(lang$errMsg$GAMSInput$excelRead, 
                       as.character(isolate(input$localInput$name)))
  })
  xlsWbNames <- vapply(strsplit(xlsWbNames, " ", fixed = TRUE), "[[", character(1L), 1L)
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  # extract only sheets which are also in list of input parameters
  datasetsToFetch <- xlsWbNames[tolower(xlsWbNames) %in% modelInTabularData]
  # load input data 
  loadMode <- "xls"
  overwriteInput <- TRUE
  source("./modules/input_load.R", local = TRUE)
  if(!is.null(errMsg)){
    return(NULL)
  }
  isEmptyInput         <<- vapply(modelInputData, function(el){
    if(length(el) && nrow(el))
      FALSE
    else
      TRUE
  }, logical(1L), USE.NAMES = FALSE)
  tabularInputWithData <- setNames(names(modelIn)[!isEmptyInput], 
                                   modelInAlias[!isEmptyInput])
  tabularOutputWithData <- NULL
  if(any(names(modelOut) %in% xlsWbNames)){
    tryCatch({
      outputDataTmp <- loadGAMSResults(scalarsOutName = scalarsOutName, modelOut = modelOut, 
                                       workDir = dirname(isolate(input$localInput$datapath)), 
                                       modelName = modelName, errMsg = lang$errMsg$GAMSOutput,
                                       scalarsFileHeaders = scalarsFileHeaders, 
                                       colTypes = db$getDbSchema()$colTypes,
                                       modelOutTemplate = modelOutTemplate, method = "xls", 
                                       hiddenOutputScalars = config$hiddenOutputScalars,
                                       fileName = basename(isolate(input$localInput$datapath)))
    }, error = function(e){
      flog.error("Problems loading output data. Error message: %s.", e)
      errMsg <<- lang$errMsg$readOutput$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
      return()
    }
    if(!is.null(outputDataTmp$scalar)){
      scalarOutputData <<- outputDataTmp$scalar
    }
    if(!is.null(outputDataTmp$tabular)){
      modelOutputData        <<- outputDataTmp$tabular
      names(modelOutputData) <<- names(modelOut)
    }
    isEmptyOutput         <<- vapply(modelOutputData, function(el){
      if(length(el) && nrow(el))
        FALSE
      else
        TRUE
    }, logical(1L), USE.NAMES = FALSE)
    tabularOutputWithData <- setNames(names(modelOut)[!isEmptyOutput], 
                                      modelOutAlias[!isEmptyOutput])
  }
  
  if(any(!isEmptyInput)){
    changeActiveSymbol(which(!isEmptyInput)[1])
  }else if(any(!isEmptyOutput)){
    changeActiveSymbol(which(!isEmptyOutput)[1] + length(modelIn))
  }else{
    showErrorMsg("No valid data", "No valid datasets were found. Please upload a properly formatted Excel file with data.")
    return()
  }
  showEl(session, "#preview_wrapper")
  updateSelectInput(session, "gams_symbols", choices = c(tabularInputWithData, tabularOutputWithData))
  if(identical(length(scalarOutputData), 3L) && nrow(scalarOutputData)){
    session$sendCustomMessage("gms-setScalarOutputs", list(indices = scalarOutputData[[1]], 
                                                           aliases = scalarOutputData[[2]]))
  }
  rv$initData <<- TRUE
})
observeEvent(input$chart_title, {
  rv$graphConfig$graph$title <<- input$chart_title
})
observeEvent(input$plotly_type, {
  allDataAvailable <<- FALSE
  saveAndReload("plotly", input$plotly_type)
  removeUI(selector = "#plotly_options .shiny-input-container", multiple = TRUE)
  
  if(identical(isolate(input$plotly_type), "pie")){
    rv$graphConfig$graph$type <<- "pie"
    insertUI(selector = "#plotly_options", getPieOptions(), where = "beforeEnd")
    allDataAvailable <<- TRUE
  }else if(identical(isolate(input$plotly_type), "bar")){
    rv$resetRE <- rv$resetRE + 1L
    rv$graphConfig$graph$type <<- "bar"
    insertUI(selector = "#plotly_options", getBarOptions(), where = "beforeEnd")
    allDataAvailable <<- TRUE
  }else if(identical(isolate(input$plotly_type), "scatter")){
    rv$resetRE <- rv$resetRE + 1L
    rv$graphConfig$graph$type <<- "scatter"
    insertUI(selector = "#plotly_options", getScatterOptions(), where = "beforeEnd")
    allDataAvailable <<- TRUE
  }else if(identical(isolate(input$plotly_type), "line")){
    rv$resetRE <- rv$resetRE + 1L
    rv$graphConfig$graph$type <<- "scatter"
    insertUI(selector = "#plotly_options", getLineOptions(), where = "beforeEnd")
    allDataAvailable <<- TRUE
  }

  
  
  #else if(identical(isolate(input$plotly_type), "chart")){
  #  rv$resetRE <- rv$resetRE + 1L
  #  rv$graphConfig$graph$type <<- "bar"
  #  insertUI(selector = "#plotly_options",
  #           tagList(
  #             selectInput("plotly_chart_type", "Select a chart type", 
  #                         setNames(c("bar", "scatter", "line"), 
  #                                  c("Bar chart", "Scatter plot", "Line chart"))),
  #             tags$div(id = "plotly_chart_options", class = "shiny-input-container",
  #                      getBarOptions()),
  #             tags$div(id = "plotly_animation_options", class = "shiny-input-container")
  #           ), where = "beforeEnd")
  #}
  
  
  else if(identical(isolate(input$plotly_type), "hist")){
    rv$graphConfig$graph$type <<- "hist"
    insertUI(selector = "#plotly_options", getHistOptions(), where = "beforeEnd")
    allDataAvailable <<- TRUE
  }
})



#observeEvent(input$plotly_chart_type, {
#  saveAndReload("plotly", "chart", input$plotly_chart_type)
#  removeUI(selector = "#plotly_chart_options .shiny-input-container", multiple = TRUE)
#  rv$resetRE <- rv$resetRE + 1L
#  allDataAvailable <<- FALSE
#  
#  if(identical(input$plotly_chart_type, "bar")){
#    rv$graphConfig$graph$type <<- "bar"
#    insertUI(selector = "#plotly_chart_options",
#             getBarOptions(), where = "beforeEnd")
#  }else if(identical(input$plotly_chart_type, "scatter")){
#    rv$graphConfig$graph$type <<- "scatter"
#    insertUI(selector = "#plotly_chart_options",
#             getScatterOptions(), where = "beforeEnd")
#  }else{
#    rv$graphConfig$graph$type <<- "scatter"
#    insertUI(selector = "#plotly_chart_options",
#             getLineOptions(), where = "beforeEnd")
#  }
#  allDataAvailable <<- TRUE
#})



observeEvent(input$leafFlow_lng, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_lng[1])]]]]$lng0 <<- input$leafFlow_lng[2]
})
observeEvent(input$leafFlow_lat1, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_lat1[1])]]]]$lat1 <<- input$leafFlow_lat1[2]
})
observeEvent(input$leafFlow_lng1, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_lng1[1])]]]]$lng1 <<- input$leafFlow_lng1[2]
})
observeEvent(input$leafFlow_flow, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_flow[1])]]]]$flow <<- input$leafFlow_flow[2]
})
observeEvent(input$leafFlow_time, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_time[1])]]]]$time <<- input$leafFlow_time[2]
})
observeEvent(input$leafFlow_label, {
  if(nchar(input$leafFlow_label[2])){
    label <- input$leafFlow_label[2]
  }else{
    label <- NULL
  }
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_label[1])]]]]$layerId <<- label
})
observeEvent(input$leafFlow_color, {
  if(nchar(input$leafFlow_color[2]))
    rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_color[1])]]]]$color <<- input$leafFlow_color[2]
  else
    rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_color[1])]]]]$color <<- NULL
})
observeEvent(input$leafFlow_minThickness, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_minThickness[1])]]]]$minThickness <<- input$leafFlow_minThickness[2]
})
observeEvent(input$leafFlow_maxThickness, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_maxThickness[1])]]]]$maxThickness <<- input$leafFlow_maxThickness[2]
})


observeEvent(input$leafChart_lng, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_lng[1])]]]]$lng <<- input$leafChart_lng[2]
})
observeEvent(input$leafChart_lat, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_lat[1])]]]]$lat <<- input$leafChart_lat[2]
})
observeEvent(input$leafChart_chartdata, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_chartdata[1])]]]]$chartdata <<- input$leafChart_chartdata[2]
})
observeEvent(input$leafChart_time, {
  if(!identical(input$leafChart_time, "_"))
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_time[1])]]]]$time <<- input$leafChart_time[2]
  else
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_time[1])]]]]$time <<- NULL
})
observeEvent(input$leafChart_type, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_type[1])]]]]$type <<- input$leafChart_type[2]
})
observeEvent(input$leafChart_width, {
  if(nchar(input$leafChart_width[2]))
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_width[1])]]]]$width <<- as.numeric(input$leafChart_width[2])
  else
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_width[1])]]]]$width <<- NULL
})
observeEvent(input$leafChart_height, {
  if(nchar(input$leafChart_height[2]))
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_height[1])]]]]$height <<- as.numeric(input$leafChart_height[2])
  else
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_height[1])]]]]$height <<- NULL
})
observeEvent(input$leafChart_opacity, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_opacity[1])]]]]$opacity <<- as.numeric(input$leafChart_opacity[2])
})
observeEvent(input$leafChart_showlabels, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_showlabels[1])]]]]$showlabels <<- as.logical(input$leafChart_showlabels[2])
})
observeEvent(input$leafChart_transitionTime, {
  if(!identical(input$leafChart_time, "_") && nchar(input$leafChart_transitionTime[2]))
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_transitionTime[1])]]]]$transitionTime <<- as.numeric(input$leafChart_transitionTime[2])
  else
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_transitionTime[1])]]]]$transitionTime <<- NULL
})
observeEvent(input$leafChart_layerId, {
  if(!identical(input$leafChart_time, "_"))
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_layerId[1])]]]]$layerId <<- input$leafChart_layerId[2]
  else
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_layerId[1])]]]]$layerId <<- NULL
})
observeEvent(input$leafChart_legend, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_legend[1])]]]]$legend <<- as.logical(input$leafChart_legend[2])
})
observeEvent(input$leafChart_legendPosition, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_legendPosition[1])]]]]$legendPosition <<- input$leafChart_legendPosition[2]
})

observeEvent(input$leafMark_lng, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_lng[1])]]]]$lng <<- input$leafMark_lng[2]
})
observeEvent(input$leafMark_label, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_label[1])]]]]$label <<- input$leafMark_label[2]
})
observeEvent(input$leafMark_groupName, {
  id <- idLabelMap$leaflet_markers[[as.integer(input$leafMark_groupName[1])]]
  if(nchar(input$leafMark_groupName[2])){
    groupName <- input$leafMark_groupName[2]
  }else{
    groupName <- NULL
  }
  oldName <- rv$graphConfig$graph$markers[[id]]$group
  if(length(oldName) || length(groupName))
    leafletGroups$update(old = oldName, new = groupName)
  else
    return()
  rv$updateLeafletGroups <- rv$updateLeafletGroups + 1L
  
  rv$graphConfig$graph$markers[[id]]$group <<- groupName
})
observeEvent(input$leafMark_labelcolor, {
  if(nchar(input$leafMark_labelcolor[2])){
    color <- input$leafMark_labelcolor[2]
  }else{
    color <- NULL
  }
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelcolor[1])]]]]$labelOptions$style$color <<- color
})
observeEvent(input$leafMark_labelbgcolor, {
  if(nchar(input$leafMark_labelbgcolor[2])){
    color <- input$leafMark_labelbgcolor[2]
  }else{
    color <- NULL
  }
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelbgcolor[1])]]]]$labelOptions$style[["background-color"]] <<- color
})
observeEvent(input$leafMark_labelsize, {
  if(nchar(input$leafMark_labelsize[2])){
    textsize <- paste0(input$leafMark_labelsize[2], "px")
  }else{
    textsize <- NULL
  }
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelsize[1])]]]]$labelOptions$textsize <<- textsize
})
observeEvent(input$leafMark_labelPermanent, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelPermanent[1])]]]]$labelOptions$permanent <<- identical(input$leafMark_labelPermanent[2], 0L)
})
observeEvent(rv$updateLeafletGroups, {
  updateSelectInput(session, "leaflc_baseGroups", choices = leafletGroups$get())
  updateSelectInput(session, "leaflc_overlayGroups", choices = leafletGroups$get())
  updateSelectInput(session, "leaflet_hideGroups", choices = leafletGroups$get())
})
observe(rv$graphConfig$graph$hideGroups <<- input$leaflet_hideGroups)
observe(rv$graphConfig$graph$layersControl$baseGroups <<- input$leaflc_baseGroups)
observe(rv$graphConfig$graph$layersControl$overlayGroups <<- input$leaflc_overlayGroups)
observe(rv$graphConfig$graph$layersControl$position <<- input$leaflc_position)
observe(rv$graphConfig$graph$layersControl$options$collapsed <<- input$leaflc_collapsed)

#change behaviour like for datatable buttons?
observeEvent(input$pivot_rows, {
  if(length(input$pivot_rows) > 0)
    rv$graphConfig$pivottable$rows <<- input$pivot_rows
  else
    rv$graphConfig$pivottable$rows <<- NULL
})
observeEvent(input$pivot_cols, {
  if(length(input$pivot_cols) > 0)
    rv$graphConfig$pivottable$cols <<- input$pivot_cols
  else
    rv$graphConfig$pivottable$cols <<- NULL
})
observeEvent(input$pivot_aggregatorName, {
  rv$graphConfig$pivottable$aggregatorName <<- input$pivot_aggregatorName
  rv$graphConfig$pivottable$vals <<- NULL
  updateSelectInput(session, "pivot_vals", selected = "_")
  updateSelectInput(session, "pivot_vals2", selected = "_")
})
observeEvent(c(input$pivot_vals, input$pivot_vals2), {
  if(identical(input$pivot_vals, "_") || identical(input$pivot_vals, NULL))
    valstmp1 <<- NULL
  else
    valstmp1 <<- input$pivot_vals
  if(identical(input$pivot_vals2, "_") || identical(input$pivot_vals2, NULL))
    valstmp2 <<- NULL
  else
    valstmp2 <<- input$pivot_vals2
  if(identical(valstmp2, NULL))
    rv$graphConfig$pivottable$vals <<- valstmp1
  else if(identical(valstmp1, NULL))
    rv$graphConfig$pivottable$vals <<- c("", valstmp2)
  else
    rv$graphConfig$pivottable$vals <<- c(valstmp1,valstmp2)
})
observeEvent(input$pivot_rendererName, {
  rv$graphConfig$pivottable$rendererName <<- input$pivot_rendererName
})
observeEvent(input$pivot_locale, {
  rv$graphConfig$pivottable$locale <<- input$pivot_locale
})
observeEvent(input$pivot_subtotals, {
  rv$graphConfig$pivottable$subtotals <<- input$pivot_subtotals
})
observeEvent(input$pivot_width, {
  if(is.na(input$pivot_width))
    rv$graphConfig$pivottable$width <<- NULL
  else
    rv$graphConfig$pivottable$width <<- input$pivot_width
})

#observeEvent(input$timevis_series, {
#  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]] <<- input$timevis_series
#})
#TODO: groups as new file or in uploaded file? -> needs to be saved in json somehow!
#observeEvent(input$timevis_groups_input, {
#  rv$graphConfig$graph$timevis_series <<- input$timevis_groups_input
#})
observeEvent(input$timedata_start, {
  rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_start[1])]]]]$start <<- input$timedata_start[2]
}, priority = -500)
observeEvent(input$timedata_end, {
  if(!identical(input$timedata_end,"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_end[1])]]]]$end <<- input$timedata_end[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_end[1])]]]]$end <<- NULL
}, priority = -500)
observeEvent(input$timedata_id, {
  rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_id[1])]]]]$id <<- input$timedata_id[2]
}, priority = -500)
observeEvent(input$timedata_type, {
  rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_type[1])]]]]$type <<- input$timedata_type[2]
}, priority = -500)
observeEvent(input$timedata_title, {
  if(!identical(input$timedata_title,"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_title[1])]]]]$title <<- input$timedata_title[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_title[1])]]]]$title <<- NULL
}, priority = -500)
observeEvent(input$timedata_group, {
  if(!identical(input$timedata_group,"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_group[1])]]]]$group <<- input$timedata_group[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_group[1])]]]]$group <<- NULL
}, priority = -500)
observeEvent(input$timedata_subgroup, {
  if(!identical(input$timedata_subgroup,"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgroup[1])]]]]$subgroup <<- input$timedata_subgroup[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgroup[1])]]]]$subgroup <<- NULL
}, priority = -500)



observeEvent(input$timevis_showZoom, {
  rv$graphConfig$graph$showZoom <<- input$timevis_showZoom
})
observeEvent(input$timevis_zoomFactor, {
  rv$graphConfig$graph$zoomFactor <<- input$timevis_zoomFactor
})
observeEvent(input$timevis_fit, {
  rv$graphConfig$graph$fit <<- input$timevis_fit
})
observeEvent(input$timevis_loadDependencies, {
  rv$graphConfig$graph$loadDependencies <<- input$timevis_loadDependencies
})

observeEvent(input$hist_norm, {
  if(identical(input$hist_norm, " "))
    value <- ""
  else
    value <- input$hist_norm
  rv$graphConfig$graph$histnorm <<- value
})
observeEvent(input$hist_nbins, {
  rv$graphConfig$graph$nbins <<- input$hist_nbins
})
observeEvent(input$hist_barmode, {
  rv$graphConfig$graph$barmode <<- input$hist_barmode
})
observeEvent(input$pie_labels, {
  rv$graphConfig$graph$labels <<- input$pie_labels
})
observeEvent(input$pie_values, {
  rv$graphConfig$graph$values <<- input$pie_values
})
observeEvent(input$marker_symbol, {
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]]$marker$symbol <<- input$marker_symbol[2]
}, priority = -500)
observeEvent(input$marker_color, {
  if(nchar(input$marker_color[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_color[1])]]]]$marker$color <<- input$marker_color[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_color[1])]]]]$marker$color <<- NULL
}, priority = -500)
observeEvent(input$marker_size, {
  if(nchar(input$marker_size[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_size[1])]]]]$marker$size <<- input$marker_size[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_size[1])]]]]$marker$size <<- NULL
}, priority = -500)
observeEvent(input$marker_line_width, {
  if(nchar(input$marker_line_width[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_width[1])]]]]$marker$line$width <<- input$marker_line_width[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_width[1])]]]]$marker$line$width <<- NULL
}, priority = -500)
observeEvent(input$marker_line_color, {
  if(nchar(input$marker_line_color[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_color[1])]]]]$marker$line$color <<- input$marker_line_color[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_color[1])]]]]$marker$line$color <<- NULL
}, priority = -500)
observeEvent(input$line_color, {
  if(nchar(input$line_color[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_color[1])]]]]$line$color <<- input$line_color[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_color[1])]]]]$line$color <<- NULL
}, priority = -500)
observeEvent(input$line_width, {
  if(nchar(input$line_width[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_width[1])]]]]$line$width <<- input$line_width[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_width[1])]]]]$line$width <<- NULL
}, priority = -500)
observeEvent(input$line_shape, {
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_shape[1])]]]]$line$shape <<- input$line_shape[2]
})
observeEvent(input$line_dash, {
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_dash[1])]]]]$line$dash <<- input$line_dash[2]
}, priority = -500)
observeEvent(input$trace_legend, {
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$trace_legend[1])]]]]$showlegend <<- as.logical(input$trace_legend[2])
}, priority = -500)
observeEvent(input$trace_frame, {
  if(!is.null(input$trace_frame[2]) && (!identical(input$trace_frame[2], "_"))){
    removeUI(selector = "#plotly_animation_options .shiny-input-container", multiple = TRUE)
    insertUI(selector = "#plotly_animation_options", getAnimationOptions(), where = "beforeEnd")
    traceframetmp <<- input$trace_frame[2]
  }else{
    removeUI(selector = "#plotly_animation_options .shiny-input-container", multiple = FALSE)
    traceframetmp <<- NULL
  }
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$trace_frame[1])]]]]$frame <<- traceframetmp
  rv$graphConfig$graph$animation <<- NULL
}, priority = -500)
observeEvent(input$animation_frame, {
  #frame = amount of time between frames (in milliseconds)
  frametmp <- input$animation_frame
  frameopt <- 1/(frametmp*1000)
  rv$graphConfig$graph$animation$frame <<- frameopt
})
observeEvent(input$animation_transition, {
  transitiontmp <- input$animation_transition
  transitionopt <- 1/(transitiontmp*1000)
  rv$graphConfig$graph$animation$transition <<- transitionopt
})
observeEvent(input$animation_easing, {
  rv$graphConfig$graph$animation$easing <<- input$animation_easing
})
observeEvent(input$animation_redraw, {
  rv$graphConfig$graph$animation$redraw <<- input$animation_redraw
})
observeEvent(input$animation_mode, {
  rv$graphConfig$graph$animation$mode <<- input$animation_mode
})
observeEvent(input$animation_slider_hide, {
  rv$graphConfig$graph$animation$slider$hide <<- input$animation_slider_hide
})
observeEvent(input$animation_slider_label, {
  rv$graphConfig$graph$animation$slider$label <<- input$animation_slider_label
})
observeEvent(input$animation_slider_prefix, {
  rv$graphConfig$graph$animation$slider$prefix <<- input$animation_slider_prefix
})
observeEvent(input$animation_slider_font_color, {
  rv$graphConfig$graph$animation$slider$fontcolor <<- input$animation_slider_font_color
})
observeEvent(input$dyrange_activate, {
  if(identical(input$dyrange_activate, TRUE)){
    rv$graphConfig$graph$dyRangeSelector <<- list(height = input$dyrange_height, strokeColor = input$dyrange_strokeColor,
                                                  fillColor = input$dyrange_fillColor, retainDateWindow = input$dyrange_retainDateWindow,
                                                  keepMouseZoom = input$dyrange_keepMouseZoom)
  }else{
    rv$graphConfig$graph$dyRangeSelector <<- NULL
  }
})
observeEvent(input$dyrange_height, {
  if(identical(input$dyrange_activate, TRUE)){
    rv$graphConfig$graph$dyRangeSelector$height <<- input$dyrange_height
  }
})
observeEvent(input$dyrange_strokeColor, {
  if(identical(input$dyrange_activate, TRUE)){
    rv$graphConfig$graph$dyRangeSelector$strokeColor <<- input$dyrange_strokeColor
  }
})
observeEvent(input$dyrange_fillColor, {
  if(identical(input$dyrange_activate, TRUE)){
    rv$graphConfig$graph$dyRangeSelector$fillColor <<- input$dyrange_fillColor
  }
})
observeEvent(input$dyrange_keepMouseZoom, {
  if(identical(input$dyrange_activate, TRUE)){
    rv$graphConfig$graph$dyRangeSelector$keepMouseZoom <<- input$dyrange_keepMouseZoom
  }
})
observeEvent(input$dyrange_retainDateWindow, {
  if(identical(input$dyrange_activate, TRUE)){
    rv$graphConfig$graph$dyRangeSelector$retainDateWindow <<- input$dyrange_retainDateWindow
  }
})
observeEvent(input$dyrange_height, {
  if(identical(input$dyrange_activate, TRUE)){
    rv$graphConfig$graph$dyRangeSelector$height <<- input$dyrange_height
  }
})
observeEvent(input$dyopt_incZero, {
  rv$graphConfig$graph$dyOptions$includeZero <<- input$dyopt_incZero
})
observeEvent(input$dyopt_logscale, {
  rv$graphConfig$graph$dyOptions$logscale <<- input$dyopt_logscale
})
observeEvent(input$dyopt_drawGrid, {
  rv$graphConfig$graph$dyOptions$drawGrid <<- input$dyopt_drawGrid
})
observeEvent(input$dyser_color, {
  if(nchar(input$dyser_color[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyser_color[1])]]]]$color <<- input$dyser_color[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyser_color[1])]]]]$color <<- NULL
})
observeEvent(input$dyopt_fillGraph, {
  if(length(input$dyopt_fillGraph) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_fillGraph[1])]]]]$fillGraph <<- input$dyopt_fillGraph[2]
  }else{
    rv$graphConfig$graph$dyOptions$fillGraph <<- input$dyopt_fillGraph
  }
})
observeEvent(input$dyopt_stepPlot, {
  if(length(input$dyopt_stepPlot) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_stepPlot[1])]]]]$stepPlot <<- input$dyopt_stepPlot[2]
  }else{
    rv$graphConfig$graph$dyOptions$stepPlot <<- input$dyopt_stepPlot
  }
})
observeEvent(input$dyopt_stemPlot, {
  if(length(input$dyopt_stemPlot) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_stemPlot[1])]]]]$stemPlot <<- input$dyopt_stemPlot[2]
  }else{
    rv$graphConfig$graph$dyOptions$stemPlot <<- input$dyopt_stemPlot
  }
})
observeEvent(input$dyopt_fillAlpha, {
  rv$graphConfig$graph$dyOptions$fillAlpha <<- input$dyopt_fillAlpha
})
observeEvent(input$dyopt_drawPoints, {
  if(length(input$dyopt_drawPoints) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_drawPoints[1])]]]]$drawPoints <<- input$dyopt_drawPoints[2]
  }else{
    rv$graphConfig$graph$dyOptions$drawPoints <<- input$dyopt_drawPoints
  }
})
observeEvent(input$dyopt_pointShape, {
  if(length(input$dyopt_pointShape) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_pointShape[1])]]]]$pointShape <<- input$dyopt_pointShape[2]
  }else{
    rv$graphConfig$graph$dyOptions$pointShape <<- input$dyopt_pointShape
  }
})
observeEvent(input$dyopt_pointSize, {
  if(length(input$dyopt_pointSize) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_pointSize[1])]]]]$pointSize <<- input$dyopt_pointSize[2]
  }else{
    rv$graphConfig$graph$dyOptions$pointSize <<- input$dyopt_pointSize
  }
})
observeEvent(input$dyhigh_circleSize, {
  rv$graphConfig$graph$dyHighlight$highlightCircleSize <<- input$dyhigh_circleSize
})
observeEvent(input$dyhigh_seriesBackgroundAlpha, {
  rv$graphConfig$graph$dyHighlight$highlightSeriesBackgroundAlpha <<- input$dyhigh_seriesBackgroundAlpha
})
observeEvent(input$dyhigh_hideOnMouseOut, {
  rv$graphConfig$graph$dyHighlight$hideOnMouseOut <<- input$dyhigh_hideOnMouseOut
})

observeEvent(input$dyEvent_label, {
  if(identical(input$dyEvent_label[2], "")){
    eventLabel <- NULL
  }else{
    eventLabel <- input$dyEvent_label[2]
  }
  rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_label[1])]]]]$label <<- eventLabel
})
observeEvent(input$dyEvent_labelLoc, {
  rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_labelLoc[1])]]]]$labelLoc <<- input$dyEvent_labelLoc[2]
})
observeEvent(input$dyEvent_color, {
  if(nchar(input$dyEvent_color[2]))
    rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_color[1])]]]]$color <<- input$dyEvent_color[2]
  else
    rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_color[1])]]]]$color <<- NULL
})
observeEvent(input$dyEvent_strokePattern, {
  rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_strokePattern[1])]]]]$strokePattern <<- input$dyEvent_strokePattern[2]
})
observeEvent(input$dyLimit_label, {
  if(identical(input$dyLimit_label[2], "")){
    limitLabel <- NULL
  }else{
    limitLabel <- input$dyLimit_label[2]
  }
  rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_label[1])]]]]$label <<- limitLabel
})
observeEvent(input$dyLimit_labelLoc, {
  rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_labelLoc[1])]]]]$labelLoc <<- input$dyLimit_labelLoc[2]
})
observeEvent(input$dyLimit_color, {
  if(nchar(input$dyLimit_color[2]))
    rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_color[1])]]]]$color <<- input$dyLimit_color[2]
  else
    rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_color[1])]]]]$color <<- NULL
})
observeEvent(input$dyLimit_strokePattern, {
  rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_strokePattern[1])]]]]$strokePattern <<- input$dyLimit_strokePattern[2]
})
observeEvent(input$dyAnnotation_text, {
  if(nchar(input$dyAnnotation_text[2]))
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_text[1])]]]]$text <<- input$dyAnnotation_text[2]
  else
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_text[1])]]]]$text <<- NULL
})
observeEvent(input$dyAnnotation_tooltip, {
  if(nchar(input$dyAnnotation_tooltip[2]))
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_tooltip[1])]]]]$tooltip <<- input$dyAnnotation_tooltip[2]
  else
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_tooltip[1])]]]]$tooltip <<- NULL
})
observeEvent(input$dyAnnotation_width, {
  if(identical(input$dyAnnotation_width[2], "0")){
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_width[1])]]]]$width <<- NULL
  }else{
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_width[1])]]]]$width <<- input$dyAnnotation_width[2]
  }
})
observeEvent(input$dyAnnotation_height, {
  if(identical(input$dyAnnotation_width[2], "0")){
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_height[1])]]]]$height <<- NULL
  }else{
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_height[1])]]]]$height <<- input$dyAnnotation_height[2]
  }
})
observeEvent(input$dyAnnotation_attachAtBottom, {
  rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_attachAtBottom[1])]]]]$attachAtBottom <<- identical(input$dyAnnotation_attachAtBottom[2], 1L)
})
observeEvent(input$dyShading_up, {
  chart_id <- as.integer(input$dyShading_up[1])
  if(is.na(chart_id))
    return()
  rv$graphConfig$graph$dyShading[[idLabelMap$dy_dyShading[[chart_id]]]]$to <<- input$dyShading_up[2]
})
observeEvent(input$dyShading_color, {
  chart_id <- as.integer(input$dyShading_color[1])
  if(is.na(chart_id))
    return()
  rv$graphConfig$graph$dyShading[[idLabelMap$dy_dyShading[[chart_id]]]]$color <<- input$dyShading_color[2]
})
observeEvent(input$dyShading_axis, {
  chart_id <- as.integer(input$dyShading_axis[1])
  if(is.na(chart_id))
    return()
  rv$graphConfig$graph$dyShading[[idLabelMap$dy_dyShading[[chart_id]]]]$axis <<- input$dyShading_axis[2]
})
observeEvent(input$paper_bgcolor, {
  if(nchar(input$paper_bgcolor))
    rv$graphConfig$graph$paper_bgcolor <<- input$paper_bgcolor
  else
    rv$graphConfig$graph$paper_bgcolor <<- NULL
})
observeEvent(input$plot_bgcolor, {
  if(nchar(input$plot_bgcolor))
    rv$graphConfig$graph$plot_bgcolor <<- input$plot_bgcolor
  else
    rv$graphConfig$graph$plot_bgcolor <<- NULL
})
observeEvent(input$showlegend, {
  rv$graphConfig$graph$showlegend <<- input$showlegend
})
observeEvent(input$outType, {
  if(identical(input$outType, TRUE))
    outTypetmp <<- "dtGraph"
  else
    outTypetmp <<- "graph"
  rv$graphConfig$graph$outType <<- outTypetmp
})
observeEvent(input$x_title, {
  rv$graphConfig$graph$xaxis$title <<- input$x_title
})
observeEvent(input$x_showgrid, {
  rv$graphConfig$graph$xaxis$showgrid <<- input$x_showgrid
})
observeEvent(input$x_zeroline, {
  rv$graphConfig$graph$xaxis$zeroline <<- input$x_zeroline
})
observeEvent(input$x_showticklabels, {
  rv$graphConfig$graph$xaxis$showticklabels <<- input$x_showticklabels
})
observeEvent(c(input$x_rangefrom,input$x_data_format_selector), {
  if(nchar(input$x_rangefrom)){
    if(identical(input$x_data_format_selector, FALSE))
      xfromtmp <<- as.numeric(as.character(input$x_rangefrom))
    else
      xfromtmp <<- input$x_rangefrom
  }else{
    xfromtmp <<- NULL
  }
  rv$graphConfig$graph$xaxis$rangefrom <<- xfromtmp
})
observeEvent(c(input$x_rangeto,input$x_data_format_selector), {
  if(nchar(input$x_rangeto)){
    if(identical(input$x_data_format_selector, FALSE))
      xtotmp <<- as.numeric(as.character(input$x_rangeto))
    else
      xtotmp <<- input$x_rangeto
  }else{
    xtotmp <<- NULL
  }
  rv$graphConfig$graph$xaxis$rangeto <<- xtotmp
})
observeEvent(input$y_title, {
  rv$graphConfig$graph$yaxis$title <<- input$y_title
})
observeEvent(input$y_showgrid, {
  rv$graphConfig$graph$yaxis$showgrid <<- input$y_showgrid
})
observeEvent(input$y_zeroline, {
  rv$graphConfig$graph$yaxis$zeroline <<- input$y_zeroline
})
observeEvent(input$y_showticklabels, {
  rv$graphConfig$graph$yaxis$showticklabels <<- input$y_showticklabels
})
observeEvent(c(input$y_rangefrom,input$y_data_format_selector), {
  if(nchar(input$y_rangefrom)){
    if(identical(input$y_data_format_selector, FALSE))
      yfromtmp <<- as.numeric(as.character(input$y_rangefrom))
    else
      yfromtmp <<- input$y_rangefrom
  }else{
    yfromtmp <<- NULL
  }
  rv$graphConfig$graph$yaxis$rangefrom <<- yfromtmp
})
observeEvent(c(input$y_rangeto,input$y_data_format_selector), {
  if(nchar(input$y_rangeto)){
    if(identical(input$y_data_format_selector, FALSE))
      ytotmp <<- as.numeric(as.character(input$y_rangeto))
    else
      ytotmp <<- input$y_rangeto
  }else{
    ytotmp <<- NULL
  }
  rv$graphConfig$graph$yaxis$rangeto <<- ytotmp
})
observeEvent(input$hist_label, {
  if(nchar(input$hist_label[[2]]))
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_label[1])]]]]$labels <<- input$hist_label[2]
  else
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_label[1])]]]]$labels <<- NULL
})
observeEvent(input$hist_color, {
  if(nchar(input$hist_color[[2]]))
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_color[1])]]]]$color <<- input$hist_color[2]
  else
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_color[1])]]]]$color <<- NULL
#  rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_color[1])]]]]$color <<- input$hist_color[2]
})
observeEvent(input$chart_color, {
  if(identical(input$chart_color, "_")){
    rv$graphConfig$graph$color <<- NULL
  }else{
    rv$graphConfig$graph$color <<- input$chart_color
  }
})
observeEvent(input$bar_width, {
  if(identical(input$bar_width, "_"))
    rv$graphConfig$graph$width <<- NULL
  else
    rv$graphConfig$graph$width <<- input$bar_width
})
observeEvent(input$bar_orientation, {
  if(identical(input$bar_orientation, "horizontal"))
    rv$graphConfig$graph$orientation <<- "h"
  else
    rv$graphConfig$graph$orientation <<- NULL
})
observeEvent(input$chart_xdata, {
  if(!(length(input$x_title) && nchar(input$x_title))){
    updateTextInput(session, "x_title", value = names(activeSymbol$indices)[match(input$chart_xdata, 
                                                                                  activeSymbol$indices)][1])
  }
  rv$graphConfig$graph$xdata <<- input$chart_xdata
})
observeEvent(input$add_array_el, {
  chart_id    <- input$add_array_el[1]
  chart_label <- input$add_array_el[2]
  el_id       <- input$add_array_el[3]
  if(el_id %in% c("dy_dyShading", "dy_dyLimit", "leaflet_markers", "leaflet_flows", "leaflet_minicharts", "timevis_series")){
    # ID is number instead of string as string is not unique
    chart_label <- chart_id
  }
  chart_id    <- as.integer(chart_id)
  JSON_id     <- gsub("^[^_]*_", "", el_id)
  
  if(is.na(chart_id))
    return()
  # label didnt change
  if(length(idLabelMap[[el_id]]) >= chart_id && 
     identical(idLabelMap[[el_id]][[chart_id]], chart_label)){
    if(identical(el_id, "dy_dyShading")){
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$from <- input$add_array_el[2]
    }else if(identical(el_id, "dy_dyLimit")){
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$limit <- input$add_array_el[2]
    }else if(identical(el_id, "leaflet_markers")){
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$lat <- input$add_array_el[2]
    }else if(identical(el_id, "leaflet_flows")){
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$lat0 <- input$add_array_el[2]
    }else if(identical(el_id, "leaflet_minicharts")){
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$lat <- input$add_array_el[2]
    }else if(identical(el_id, "timevis_series")){
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$content <- input$add_array_el[2]
    }else{
      return()
    }
  }
  
  # if same label already exists, remove it
  currentContent <- NULL
  if(chart_id <= length(idLabelMap[[el_id]])){
    labelID <- idLabelMap[[el_id]][[chart_id]]
    if(sum(labelID == idLabelMap[[el_id]]) < 1.5){
      if(length(input$add_array_el) > 3L){
        # when label is merely changed not added, we must preserve the previous config
        currentContent <- rv$graphConfig$graph[[JSON_id]][[labelID]]
      }
      rv$graphConfig$graph[[JSON_id]][labelID] <<- NULL
    }
  }
  
  idLabelMap[[el_id]][[chart_id]] <<- chart_label
  
  if(length(currentContent)){
    rv$graphConfig$graph[[JSON_id]][[chart_label]] <<- currentContent
    return()
  }
  
  if(identical(el_id, "chart_ydata")){
    label       <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
    newContent  <- list(label = label, 
                        mode = if(identical(input$plotly_type, "scatter"))
                          "markers" else "lines")
  }else if(identical(el_id, "hist_xdata")){
    label       <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
    newContent  <- list(labels = label, color = "#000000", alpha = 1L)
  }else if(identical(el_id, "dy_dyEvent")){
    newContent  <- list(labelLoc = "top", color = "rgb(0,0,0)", strokePattern = "dashed")
  }else if(identical(el_id, "dy_dyLimit")){
    newContent  <- list(limit = input$add_array_el[2], labelLoc = "left", color = "rgb(0,0,0)", strokePattern = "dashed")
  }else if(identical(el_id, "dy_dyAnnotation")){
    newContent <- list(text = scalarOutputData[[2]][1], tooltip = "", attachAtBottom = FALSE)
  }else if(identical(el_id, "dy_dyShading")){
    newContent <- list(from = input$add_array_el[2], 
                       to = input$add_array_el[2], 
                       color = "#efefef", 
                       axis = "x")
  }else if(identical(el_id, "leaflet_markers")){
    newContent <- list(lng = input$add_array_el[2], 
                       lat = input$add_array_el[2],
                       labelOptions = list(textsize = "12px", permanent = FALSE))
  }else if(identical(el_id, "leaflet_flows")){
    newContent <- list(lng0 = input$add_array_el[2], 
                       lat0 = input$add_array_el[2],
                       lng1 = input$add_array_el[2], 
                       lat1 = input$add_array_el[2], 
                       flow = input$add_array_el[2], 
                       color = "#0000ff",
                       minThickness = 1,
                       maxThickness = 20)
  }else if(identical(el_id, "leaflet_minicharts")){
    newContent <- list(lng = input$add_array_el[2], 
                       lat = input$add_array_el[2],
                       chartdata = input$add_array_el[2],
                       type = "auto",
                       width = 30, 
                       height = 30, 
                       showLabels = FALSE,
                       transitionTime = 750,
                       legend = TRUE,
                       legendPosition = "topright")
  }else if(identical(el_id, "timevis_series")){
    newContent  <- list(content = input$add_array_el[2], 
                        start = input$add_array_el[2],
                        id = "Count from 1",
                        type = "box")
  }else{
    newContent <- NULL
  }
  rv$graphConfig$graph[[JSON_id]][[chart_label]] <<- newContent
})
observeEvent(input$remove_array_el, {
  array_id <- input$remove_array_el[1]
  el_id    <- as.integer(as.integer(input$remove_array_el[3]))
  JSON_id     <- gsub("^[^_]*_", "", array_id)
  chart_label <- idLabelMap[[array_id]][[el_id]]
  
  if(sum(input$remove_array_el[2] == chart_label) < 1.5){
    rv$graphConfig$graph[[JSON_id]][chart_label] <- NULL
  }
  idLabelMap[[array_id]][el_id] <<- "_NULL"
})
observeEvent(input$chart_ylabel, {
  tryCatch({
    labelID <- idLabelMap$chart_ydata[[as.integer(input$chart_ylabel[1])]]
    if(nchar(input$chart_ylabel[2])){
      label <- input$chart_ylabel[2]
    }else{
      label <- NULL
    }
    rv$graphConfig$graph$ydata[[labelID]]$label <<- label
  }, error = function(e){
    flog.info("Could not change label for y-data. Error message: '%s'.", e)
  })
})
observeEvent(input$bar_mode, {
  rv$graphConfig$graph$barmode <<- input$bar_mode
})
observeEvent(input$gams_symbols, {
  req(input$gams_symbols)
  symbolID <- match(isolate(input$gams_symbols), names(modelIn))
  if(is.na(symbolID)){
    symbolID <- match(isolate(input$gams_symbols), names(modelOut)) + length(modelIn)
  }
  changeActiveSymbol(symbolID)
  updateTextInput(session, "chart_title", value = activeSymbol$alias)
})
observeEvent({
  input$chart_tool
  rv$initData}, {
    req(rv$initData)
    allDataAvailable <<- FALSE
    saveAndReload(isolate(input$chart_tool), "pie")
    removeUI(selector = "#tool_options div", multiple = TRUE)
    rv$graphConfig$graph$tool <<- input$chart_tool
    if(identical(isolate(input$chart_tool), "plotly")){
      insertUI(selector = "#tool_options",
               tags$div(id = "plotly_type_container",
                        selectInput("plotly_type", "Select the type of chart you want to plot",
                                    choices = setNames(c("pie", "bar", "scatter", "line", "hist"), 
                                                       c("Pie chart", "Bar chart", "Scatter plot", "Line chart", "Histogram"))),
                        tags$div(id = "plotly_options", getPieOptions())
               ), where = "beforeEnd")
    }else if(identical(input$chart_tool, "dygraphs")){
      currentSelection$noLayers <<- 1L
      insertUI(selector = "#tool_options",
               tags$div(id = "dygraph_options", getDygraphsOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(input$chart_tool, "leaflet")){
      currentSelection$noLayers <<- 1L
      insertUI(selector = "#tool_options",
               tags$div(id = "leaflet_options", getLeafletOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(input$chart_tool, "timevis")){
      currentSelection$noLayers <<- 1L
      insertUI(selector = "#tool_options",
               tags$div(id = "timevis_options", getTimevisOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
     }
    else if(identical(input$chart_tool, "pivot")){
    currentSelection$noLayers <<- 1L
    insertUI(selector = "#tool_options",
             tags$div(id = "pivot_options", getPivotOptions()), where = "beforeEnd")
    allDataAvailable <<- TRUE
  }
  })
getPieOptions <- reactive({
  req(rv$initData)
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  isolate({
    rv$graphConfig$graph$labels <<- indices[[1]]
    if(length(scalarIndices)){
      rv$graphConfig$graph$values <<- scalarIndices[[1]]
    }else{
      rv$graphConfig$graph$values <<- ""
    }
  })
  tagList(
    selectInput("pie_labels", "Labels",
                choices = indices),
    selectInput("pie_values", "Values",
                choices = scalarIndices)
  )
})
getAxisOptions <- function(id, title, labelOnly = FALSE){
  isolate({
    rv$graphConfig$graph[[id %+% "axis"]]$title <<- title
    if(!labelOnly){
      rv$graphConfig$graph[[id %+% "axis"]]$showgrid <<- FALSE
      rv$graphConfig$graph[[id %+% "axis"]]$zeroline <<- FALSE
      rv$graphConfig$graph[[id %+% "axis"]]$showticklabels <<- TRUE
      rv$graphConfig$graph[[id %+% "axis"]]$rangefrom <<- NULL
      rv$graphConfig$graph[[id %+% "axis"]]$rangeto <<- NULL
    }
  })
  if(labelOnly){
    return(tagList(
      textInput(id %+% "_title", sprintf("Axis label (%s axis)", id), value = title)
    ))
  }
  tagList(
    textInput(id %+% "_title", sprintf("Axis label (%s axis)", id), value = title),
    checkboxInput(id %+% "_showgrid", sprintf("Show grid (%s axis)?", id)),
    checkboxInput(id %+% "_zeroline", sprintf("Show zero line (%s axis)?", id)),
    checkboxInput(id %+% "_showticklabels", sprintf("Show tick labels(%s axis)?", id), TRUE),
    if(identical(input$plotly_type, "scatter") || identical(input$plotly_type, "line")){
      tags$div(style = "width:100%;",
               tags$label(class = "cb-label shiny-input-container", "for" = "range-wrapper", "Set axis range. The format has to match the data format. When not specified, a default is used."),
               tags$div(id = "range-wrapper", class = "col-sm-7 shiny-input-container", style = "padding-left:0px;", 
                        div(style="display:inline-block", textInput(id %+% "_rangefrom", sprintf("Range start (%s axis). ", id), value = NULL)),
                        div(style="display:inline-block", textInput(id %+% "_rangeto", sprintf("Range end (%s axis).", id), value = NULL))
               ),
               tags$div(class = "col-sm-5",
                        tags$div(class = "shiny-input-container",
                                 tags$label(class = "cb-label", style="display:block;" ,"for" = "animation_data_format_selector", "Data in date format?"),
                                   tags$label(class = "checkbox-material", 
                                              checkboxInput(id %+% "_data_format_selector", 
                                                            value = FALSE, label = NULL)
                                   )
                        ))
      )
    }
  )
}
getChartOptions <- reactive({
  req(rv$resetRE > 0L)
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  isolate({
    rv$graphConfig$graph$xdata      <<- indices[[1]]
    rv$graphConfig$graph$showlegend <<- FALSE
    rv$graphConfig$graph$outType <<- "graph" 
  })
  tagList(
    selectInput("chart_xdata", "What should be plotted on the x axis?",
                choices = indices),
    getAxisOptions("x", names(indices)[1]),
    addArrayEl(session, "chart_ydata", "Add data series", isolate(input$plotly_type)),
    getAxisOptions("y", names(scalarIndices)[1]),
    selectInput("chart_color", "Symbol that is used to select different colors. Note: The color is only valid, when no line or marker colors are set!",
                choices = c("_", indices)),
    optionSection(title = "Options", collapsed = TRUE,
                  colorPickerInput("paper_bgcolor", "What background color shall the paper have?", value = NULL),
                  colorPickerInput("plot_bgcolor", "What background color shall the plot have?", value = NULL),
                  checkboxInput("showlegend", "Show legend?"),
                  checkboxInput("outType", "Show graphic and data table in split screen? (no live preview available)", value = FALSE)
    ),
    tags$div(id = "plotly_animation_options", class = "shiny-input-container")
  )
})
getBarOptions  <- reactive({
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  isolate({
    rv$graphConfig$graph$barmode <<- "group"
    rv$graphConfig$graph$ydata[[indices[[1]]]] <<- list(label = names(indices)[1],
                                                        mode = "lines",
                                                        marker = list(color = "rgb(0,0,0)", 
                                                                      line = list(width = 0L)))
    idLabelMap$chart_ydata[[1]] <<- indices[[1]]
  })
  tagList(selectInput("bar_mode", "Select barmode", choices = c("group", "stack")),
          selectInput("bar_orientation", "Orientation",
                      choices = c("vertical", "horizontal")),
          selectInput("bar_width", "Symbol that is used to select different bar widths",
                      choices = c("_", scalarIndices)),
          getChartOptions())
})
getScatterOptions  <- reactive({
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  isolate({
    rv$graphConfig$graph$ydata <- list()
    if(length(scalarIndices)){
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1], 
                                                                mode = "markers",
                                                                marker = list(
                                                                  symbol = "circle",
                                                                  color = "rgb(0,0,0)",
                                                                  opacity = 1L,
                                                                  size = 6L,
                                                                  line = list(width = 0L)
                                                                ),
                                                                showlegend = FALSE)
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    }else{
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  getChartOptions()
})
getLineOptions  <- reactive({
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  isolate({
    rv$graphConfig$graph$ydata <- list()
    if(length(scalarIndices)){
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1], 
                                                              mode = "lines",
                                                              line = list(color = "rgb(0,0,0)",
                                                                          width = 2L,
                                                                          shape = "linear",
                                                                          dash = "solid"),
                                                              showlegend = FALSE)
    idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    }else{
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  getChartOptions()
})
getAnimationOptions  <- reactive({
  tagList(
    optionSection(title = "Animation options", collapsed = TRUE,
                  numericInput("animation_frame", "Frames per second.", min = 0L, value = 500L), 
                  numericInput("animation_transition", "duration of the smooth transition between frames (in milliseconds).", min = 0L, value = 500L),
                  selectInput("animation_easing", "Type of transition easing", choices = c("linear","quad","cubic","sin","exp","circle","elastic","back","bounce","linear-in",
                                                                                           "quad-in","cubic-in","sin-in","exp-in","circle-in","elastic-in","back-in","bounce-in","linear-out","quad-out","cubic-out","sin-out","exp-out",
                                                                                           "circle-out","elastic-out","back-out","bounce-out","linear-in-out","quad-in-out","cubic-in-out","sin-in-out","exp-in-out","circle-in-out",
                                                                                           "elastic-in-out","back-in-out","bounce-in-out")),
                  checkboxInput("animation_redraw", "Trigger a redraw of the plot at completion of the transition? (this may significantly impact 
                                performance, but may be necessary to update graphical elements that can't be transitioned.)", value = TRUE),
                  selectInput("animation_mode", "How shall new animate calls interact with currently-running animations?", choices = c("immediate", "next", "afterall")),
                  getAnimationSliderOptions()))
})
getAnimationSliderOptions  <- reactive({
  tagList(checkboxInput("animation_slider_hide", "remove the animation slider?"), 
          textInput("animation_slider_label", "Label to appear on the slider"),
          textInput("animation_slider_prefix", "Prefix of the label"),
          colorPickerInput("animation_slider_font_color", "What font color should be used for the slider label?", "#000000"))
})
getHistOptions <- reactive({
  scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "parameter"]
  isolate({
    label <- names(activeSymbol$indices)[match(scalarIndices[1], 
                                               activeSymbol$indices)][1]
    rv$graphConfig$graph$xdata[[scalarIndices[1]]] <<- list(labels = label, 
                                                            color = "#000000", 
                                                            alpha = 1L)
    rv$graphConfig$graph$histnorm   <<- ""
    rv$graphConfig$graph$nbins      <<- 2L
    rv$graphConfig$graph$barmode    <<- "overlay"
    rv$graphConfig$graph$xaxis$title <<- label
    if(length(scalarIndices)){
      idLabelMap$hist_xdata[[1]]       <<- scalarIndices[[1]]
    }else{
      idLabelMap$hist_xdata[[1]]       <<- "1"
    }
  })
  tagList(
    addArrayEl(session, "hist_xdata", "Add data series"),
    selectInput("hist_norm", "Type of normalization to use",
                choices = setNames(c(" ", "percent", "density"), c("Number of occurances", 
                                                                   "Percentage of occurances", 
                                                                   "Number of occurances/ Bin interval size"))),
    numericInput("hist_nbins", "Number of bins",
                 min = 0L, value = 2L),
    selectInput("hist_barmode", "How do you want the bars to be displayed?",
                choices = c("overlay", "stack", "group", "relative")),
    getAxisOptions("x", label, labelOnly = TRUE),
    getAxisOptions("y", "", labelOnly = TRUE)
  )
})
getDygraphsOptions <- reactive({
  rv$initData
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  if(!length(scalarIndices)){
    showElReplaceTxt(session, "#preview-error", "No scalar indices found in your data.")
    return()
  }
  isolate({
    rv$graphConfig$graph$xdata <<- unname(indices[1])
    rv$graphConfig$graph$ydata <<- NULL
    rv$graphConfig$graph$ydata[[scalarIndices[1]]] <<- list(label = unname(scalarIndices[1]), 
                                                            mode = if(identical(input$plotly_type, "scatter"))
                                                              "markers" else "lines", 
                                                            stemPlot = FALSE, stepPlot = FALSE, 
                                                            fillGraph = FALSE, drawPoints = FALSE, pointShape = "dot",
                                                            pointSize = 2L)
    rv$graphConfig$graph$dyEvent <<- NULL
    rv$graphConfig$graph$dyAnnotation <<- NULL
    rv$graphConfig$graph$dyShading <<- NULL
    rv$graphConfig$graph$dyOptions <<- list(includeZero = FALSE, logscale = FALSE, drawGrid = FALSE,
                                            stepPlot = FALSE, stemPlot = FALSE, fillGraph = FALSE,
                                            fillAlpha = 0.15, drawPoints = FALSE, pointShape = "dot",
                                            pointSize = 2L)
    rv$graphConfig$graph$dyHighlight <<- NULL
    if(length(scalarIndices)){
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    }else{
      idLabelMap$chart_ydata[[1]]       <<- "1" 
    }
  })
  tagList(
    selectInput("chart_xdata", "What index do you want to plot on the x-axis?",
                choices = indices),
    addArrayEl(session, "dy_ydata", "Add data series"),
    selectInput("chart_color", "What symbol do you want to use to plot different chart lines?",
                choices = c("_", indices)),
    if(length(scalarOutputData) && nrow(scalarOutputData)){
      tagList(
        addArrayEl(session, "dy_dyEvent", "Add event line", autoCreate = FALSE),
        addArrayEl(session, "dy_dyLimit", "Add limit line", autoCreate = FALSE),
        addArrayEl(session, "dy_dyAnnotation", "Add annotation", autoCreate = FALSE),
        addArrayEl(session, "dy_dyShading", "Add shading", autoCreate = FALSE)
      )
    },
    optionSection(title = "Range selector options", collapsed = TRUE,
                  checkboxInput("dyrange_activate", "Do you want to see a range selector?"),
                  numericInput("dyrange_height", "How high should it be (in px)?", min = 0L, value = 40L),
                  checkboxInput("dyrange_retainDateWindow", "Should current date range be kept when graph is redrawn?", FALSE),
                  checkboxInput("dyrange_keepMouseZoom", "Do you still want to be able to zoom with the mouse?", TRUE),
                  colorPickerInput("dyrange_fillColor", "What fill color should be used?", "#A7B1C4"),
                  colorPickerInput("dyrange_strokeColor", "What stroke color should be used?", "#808FAB")
    ),
    optionSection(title = "General options", collapsed = TRUE,
                  checkboxInput("dyopt_incZero", "Should y-axis start at 0?"),
                  checkboxInput("dyopt_logscale", "Show y-axis in log scale?"),
                  checkboxInput("dyopt_drawGrid", "Should grid lines be displayed?", TRUE),
                  checkboxInput("dyopt_stepPlot", "Do you want a step plot?"),
                  checkboxInput("dyopt_stemPlot", "Do you want a stem plot?"),
                  checkboxInput("dyopt_fillGraph", "Should the area underneath the graph be filled?"),
                  numericInput("dyopt_fillAlpha", "Transparency for filled regions", min = 0L, max = 1L, value = 0.15),
                  checkboxInput("dyopt_drawPoints", "Should points be drawn?"),
                  selectInput("dyopt_pointShape", "What shape should points have?", 
                              choices = c("dot", "triangle", "square", "diamond", "pentagon", "hexagon", 
                                          "circle", "star", "plus", "ex")),
                  numericInput("dyopt_pointSize", "What size should points be?", min = 0L, value = 2L)
    ),
    optionSection(title = "Highlighting options", collapsed = TRUE,
                  numericInput("dyhigh_circleSize", "What shall the circle size be (in px)?", min = 0L, value = 3L),
                  sliderInput("dyhigh_seriesBackgroundAlpha", "Choose the opacity of the background", 
                              min = 0L, max = 1L, step = 0.1, value = 0.5),
                  checkboxInput("dyhigh_hideOnMouseOut", "Hide highlighting effects when mouse is moved out of chart?", TRUE)
    ),
    getAxisOptions("x", names(indices)[1], labelOnly = TRUE),
    getAxisOptions("y", names(scalarIndices)[1], labelOnly = TRUE)
  )
})
getLeafletOptions <- reactive({
  rv$initData
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  leafletGroups$reset()
  isolate({
    rv$graphConfig$graph$markers <<- NULL
    rv$graphConfig$graph$flows <<- NULL
    rv$graphConfig$graph$minicharts <<- NULL
    rv$graphConfig$graph$layersControl$position <<- "topright"
    rv$graphConfig$graph$layersControl$options$collapsed <<- TRUE
  })
  tagList(
    tagList(
      addArrayEl(session, "leaflet_markers", "Add Markers", autoCreate = FALSE),
      addArrayEl(session, "leaflet_flows", "Add Flows", autoCreate = FALSE),
      addArrayEl(session, "leaflet_minicharts", "Add Charts", autoCreate = FALSE),
      selectInput("leaflet_hideGroups", "Select groups that should be hidden on startup", choices = c(),
                  multiple = TRUE),
      optionSection(title = "Layer control options", collapsed = TRUE,
                    selectInput("leaflc_baseGroups", "Which base groups shall be selectable?", choices = c(),
                                multiple = TRUE),
                    selectInput("leaflc_overlayGroups", "Which overlay groups shall be selectable?", 
                                choices = c(), multiple = TRUE),
                    selectInput("leaflc_position", "Should grid lines be displayed?", 
                                setNames(c("topright", "bottomright", "bottomleft", "topleft"), c("Top right", "Bottom right",
                                                                                                  "Bottom left", "Top left"))),
                    checkboxInput("leaflc_collapsed", "Shall layer control panel be collapsed?", value = TRUE)
      )
    )
  )
})
getTimevisOptions<- reactive({
  rv$initData
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  isolate({
    rv$graphConfig$graph$showZoom <<- TRUE
    rv$graphConfig$graph$fit <<- TRUE
    rv$graphConfig$graph$loadDependencies <<- TRUE
    rv$graphConfig$graph$zoomFactor <<- 0.5
    rv$graphConfig$graph$series <- NULL
    #rv$graphConfig$graph$series <- list()
    #if(length(scalarIndices)){
    #  rv$graphConfig$graph$series[[scalarIndices[[1]]]] <<- list(content = names(scalarIndices)[1], 
    #                                                            start = names(scalarIndices)[1],
    #                                                            id = "Count from 1",
    #                                                            type = "box")
    #  idLabelMap$timevis_series[[1]] <<- scalarIndices[[1]]
    #}else{
    #  idLabelMap$timevis_series[[1]] <<- "1"
    #}
  })
  tagList(
    tagList(
      addArrayEl(session, "timevis_series", "Add data series", autoCreate = FALSE),
      tags$label(class = "cb-label", "for" = "timevis_groups", "Use groups?"),
      tags$div(
        tags$label(class = "checkbox-material", 
                   checkboxInput("timevis_groups", label = NULL)
        )),
      tags$div(class = "shiny-input-container",
               style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;padding-left:40px;",
               conditionalPanel(condition = "input.timevis_groups===true",
                                fileInput("timevis_groups_input", span("Upload an Excel spreadsheet with group data. See ", tags$a(href="https://github.com/daattali/timevis#groups", target = "_blank", "https://github.com/daattali/timevis#groups"), " for more information"),
                                          width = "100%",
                                          multiple = FALSE,
                                          accept = c("application/vnd.ms-excel",
                                                     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                                     ".xlsx"))
               )
      ),
      tags$label(class = "cb-label", "for" = "timevis_showZoom", "Include 'Zoom In' / 'Zoom Out' buttons?"),
      tags$div(
        tags$label(class = "checkbox-material", 
                   checkboxInput("timevis_showZoom", label = NULL, value = TRUE)
        )),
      numericInput("timevis_zoomFactor", "Zoomfaktor? Zooming out from 20 days with a zoomFactor of 1 will results in showing 40 days.", min = 0, value = 0.5, step = 0.1),
      tags$label(class = "cb-label", "for" = "timevis_fit", "Fit all data on the timeline when the timeline initializes? Otherwise, the timeline will be set to show the current date."),
      tags$div(
        tags$label(class = "checkbox-material", 
                   checkboxInput("timevis_fit", label = NULL, value = TRUE)
        )),
      tags$label(class = "cb-label", "for" = "timevis_loadDependencies", "Load JQuery and bootstrap dependencies? (recommended)"),
      tags$div(
        tags$label(class = "checkbox-material", 
                   checkboxInput("timevis_loadDependencies", label = NULL, value = TRUE)
        ))
    )
  )
})
getPivotOptions<- reactive({
  
  rv$initData
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
  isolate({
      ##rv$graphConfig$graph <<- NULL
      if(length(input$chart_height))
        rv$graphConfig$pivottable$height <<- input$height
      rv$graphConfig$pivottable$rows <<- NULL
      rv$graphConfig$pivottable$cols <<- NULL
      rv$graphConfig$pivottable$aggregatorName <<- "Count"
      rv$graphConfig$pivottable$vals <<- "_"
      rv$graphConfig$pivottable$rendererName <<- "Table"
      rv$graphConfig$pivottable$locale <<- "en"
      rv$graphConfig$pivottable$subtotals <<- FALSE
      rv$graphConfig$pivottable$width <<- 800L
    })
  tagList(
    selectInput("pivot_rows", "Prepopulate rows of the pivot table", choices = indices, multiple = TRUE, selected = NULL),
    selectInput("pivot_cols", "Prepopulate columns of the pivot table", choices = indices, multiple = TRUE, selected = NULL),
    selectInput("pivot_aggregatorName", "Aggregator to use", choices = c("Count","Count Unique Values","List Unique Values",
                                                              "Sum","Integer Sum","Average","Median","Sample Variance",
                                                              "Sample Standard Deviation","Minimum","Maximum","First","Last",
                                                              "Sum over Sum","80% Upper Bound","80% Lower Bound","Sum as Fraction of Total",
                                                              "Sum as Fraction of Total","Sum as Fraction of Rows",
                                                              "Sum as Fraction of Columns","Count as Fraction of Total",
                                                              "Count as Fraction of Rows","Count as Fraction of Columns"),
                selected = "Count"),
    tags$div(class = "shiny-input-container",
             style = "max-height:800px;max-height: 80vh;padding-right:30px;padding-left:40px;",
             conditionalPanel(condition = "input.pivot_aggregatorName != 'Count' && input.pivot_aggregatorName != 'Count as Fraction of Total' && 
                     input.pivot_aggregatorName != 'Count as Fraction of Rows' && input.pivot_aggregatorName != 'Count as Fraction of Columns'",
                              selectInput("pivot_vals", "String name of the column in the dataset to use with aggregatorName.", choices = c("_", indices))
             ),  
             conditionalPanel(condition = "input.pivot_aggregatorName === 'Sum over Sum' || input.pivot_aggregatorName === '80% Upper Bound' || 
                     input.pivot_aggregatorName === '80% Lower Bound'",
                              selectInput("pivot_vals2", "String name of the column in the dataset to use with aggregatorName.", choices = c("_", indices))
             )),  
    selectInput("pivot_rendererName", "Name of the renderer selected.", choices = c("Table","Table Barchart","Heatmap","Row Heatmap",
                                                                                "Col Heatmap","Treemap","Horizontal Bar Chart",
                                                                                "Horizontal Stacked Bar Chart","Bar Chart",
                                                                                "Stacked Bar Chart","Line Chart","Area Chart",
                                                                                "Scatter Chart"), selected = "Table"),
    optionSection(title = "Pivot table options", collapsed = TRUE,
                  selectInput("pivot_locale", "Localization options. A change from 'en' to another language will cause 
                              the configuration of the used graphic to be lost.", choices = c("cs","da","de","en","es","fr",
                                                                                               "it","nl","pl","pt","ru","sq","tr","zh"), 
                              selected = "en"),
                  checkboxInput("pivot_subtotals", span("Should subtotal plugin be used? For examples, see: ", tags$a(href="http://nagarajanchinnasamy.com/subtotal/", target = "_blank", "http://nagarajanchinnasamy.com/subtotal/")), 
                                value = FALSE),
                  numericInput("pivot_width", "Width of pivottable ", min = 0L, value = 800L)
    )
    
  )
}) 
observe({
  req(rv$graphConfig$graph$tool, activeSymbol$id > 0L, allDataAvailable)
  if(identical(rv$graphConfig$graph$tool, "plotly") && identical(length(rv$graphConfig$graph$type), 0L))
    return()
  print("+++++++++++++++++++++++++++++++++++++++")
  print(rv$graphConfig$graph)
  if(activeSymbol$id > length(modelIn)){
    data <- modelOutputData[[activeSymbol$id - length(modelIn)]]
  }else{
    data <- modelInputData[[activeSymbol$id]]
  }
  tryCatch({
    if(isolate(rv$graphConfig$graph$tool) == "plotly"){
      callModule(renderData, "preview_output_plotly", type = rv$graphConfig$outType, 
                 data = data, configData = scalarOutputData, 
                 graphOptions = rv$graphConfig$graph,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      showEl(session, "#preview-content-plotly")
      hideEl(session, "#preview-content-dygraph")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-timevis")
    }else if(isolate(rv$graphConfig$graph$tool) == "dygraphs"){
      callModule(renderData, "preview_output_dygraph", type = rv$graphConfig$outType, 
                 data = data, configData = scalarOutputData, 
                 graphOptions = rv$graphConfig$graph,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      showEl(session, "#preview-content-dygraph")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-timevis")
    }else if(isolate(rv$graphConfig$graph$tool) == "pivot"){
      pivotOptions <- rv$graphConfig$pivottable
      callModule(renderData, "preview_output_pivot", type = "pivot", 
                 data = data, configData = scalarOutputData, 
                 pivotOptions = pivotOptions,
                 roundPrecision = 2, modelDir = modelDir)
      showEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-dygraph")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-timevis")
    }else if(isolate(rv$graphConfig$graph$tool) == "timevis"){
      callModule(renderData, "preview_output_timevis", type = rv$graphConfig$outType, 
                 data = data, configData = scalarOutputData, 
                 graphOptions = rv$graphConfig$graph,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      showEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-dygraph")
    }else{
      callModule(renderData, "preview_output_leaflet", type = rv$graphConfig$outType, 
                 data = data, configData = scalarOutputData, 
                 graphOptions = rv$graphConfig$graph,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      showEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#preview-content-dygraph")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-timevis")
    }
    hideEl(session, "#preview-error")
  }, error = function(e) {
    hideEl(session, "#preview-content-dygraph")
    hideEl(session, "#preview-content-plotly")
    hideEl(session, "#preview-content-leaflet")
    hideEl(session, "#preview-content-pivot")
    hideEl(session, "#preview-content-timevis")
    showElReplaceTxt(session, "#preview-error", "An error occurred: " %+% toString(e))
  })
}, priority = -1000)

#  ==============================
#          SAVE JSON
#  ==============================
observeEvent(input$saveGraph, {
  req(nchar(activeSymbol$name) > 0L)
  errMsg <- validateGraphConfig(rv$graphConfig)
  if(nchar(errMsg)){
    showHideEl(session, "#graphValidationErr", 5000L, errMsg)
    return()
  }
  if(tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))){
    showModal(modalDialog(title = "Data exists", sprintf("A graph configuration already exists for symbol: '%s'. Do you want to overwrite this configuration? This cannot be undone!", activeSymbol$name), 
                          footer = tagList(modalButton("Cancel"), 
                                           actionButton("saveGraphConfirm", "Overwrite"))))
    return()
  }
  rv$saveGraphConfirm <- rv$saveGraphConfirm + 1L
})
observeEvent(input$saveGraphConfirm, rv$saveGraphConfirm <- rv$saveGraphConfirm + 1L)
observeEvent(rv$saveGraphConfirm, {
  req(rv$saveGraphConfirm > 0L)
  configJSON$dataRendering[[activeSymbol$name]] <<- rv$graphConfig
  if(length(input$chart_height))
    configJSON$dataRendering[[activeSymbol$name]]$height <<- input$chart_height
  
  write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
  removeModal()
  showHideEl(session, "#graphUpdateSuccess", 4000L)
})