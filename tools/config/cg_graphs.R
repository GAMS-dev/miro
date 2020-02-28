activeSymbol <- list(id = integer(1L), name = character(1L), 
                     alias = character(1L), indices = c())

newChartTool     <- character(0L)
plotlyChartTools <- c("pie", "bar", "scatter", "line", "bubble", "hist")
modelInputData   <- vector("list", length(modelIn))
modelOutputData  <- vector("list", length(modelOut))
configScalars <- tibble()
hotInit          <- vector("logical", length(modelIn))
isEmptyInput     <- vector(mode = "logical", length = length(modelIn))
isEmptyOutput    <- vector(mode = "logical", length = length(modelOut))
isEmptyOutput[]  <- TRUE
inputInitialized <- vector(mode = "logical", length = length(modelInWithDep))
currentSelection <- list("plotly", "pie")
idLabelMap       <- list(chart_ydata = list(), animation_options = list(), hist_xdata = list(), dy_dyEvent = list(), 
                         dy_dyLimit = list(), leaflet_markers = list(), leaflet_flows = list(), leaflet_minicharts = list(), 
                         timevis_series = list(), timevis_custom = list())
currentConfig    <- list()
optionsInserted  <- c()
noOutputData     <- TRUE
allDataAvailable <- FALSE
leafletGroups    <- CharArray$new()

langSpecificGraphs <- list()
langSpecificGraphs$barmode <- c("group" = "group", "stack" = "stack")
names(langSpecificGraphs$barmode) <- lang$adminMode$graphs$barOptions$choicesMode
langSpecificGraphs$barOrientation <- c("vertical" = "vertical", "horizontal" = "horizontal")
names(langSpecificGraphs$barOrientation) <- lang$adminMode$graphs$barOptions$choicesOrientation
langSpecificGraphs$valueboxColor <- c("red" ="red", "yellow" = "yellow", "aqua" = "aqua", "blue" = "blue", 
                                      "light-blue" = "light-blue", "green" = "green", "navy" = "navy", "teal" = "teal", 
                                      "olive" = "olive", "lime" = "lime", "orange" = "orange", "fuchsia" = "fuchsia", 
                                      "purple" = "purple", "maroon" = "maroon", "black" = "black")
names(langSpecificGraphs$valueboxColor) <- lang$adminMode$graphs$valueboxOptions$colorChoices
langSpecificGraphs$libChoices <- c("Font-Awesome" = "font-awesome", "Glyphicons" = "glyphicon")
names(langSpecificGraphs$libChoices) <- lang$adminMode$graphs$valueboxOptions$libChoices
langSpecificGraphs$easingChoices <- c("linear" = "linear","quad" = "quad","cubic" = "cubic","sin" = "sin",
                                      "exp" = "exp","circle" = "circle","elastic" = "elastic","back" = "back","bounce" = "bounce",
                                      "linear-in" = "linear-in", "quad-in" = "quad-in","cubic-in" = "cubic-in",
                                      "sin-in" = "sin-in","exp-in" = "exp-in","circle-in" = "circle-in",
                                      "elastic-in" = "elastic-in","back-in" = "back-in","bounce-in" = "bounce-in",
                                      "linear-out" = "linear-out","quad-out" = "quad-out","cubic-out" = "cubic-out",
                                      "sin-out" = "sin-out","exp-out" = "exp-out","circle-out" = "circle-out",
                                      "elastic-out" = "elastic-out","back-out" = "back-out","bounce-out" = "bounce-out",
                                      "linear-in-out" = "linear-in-out","quad-in-out" = "quad-in-out",
                                      "cubic-in-out" = "cubic-in-out","sin-in-out" = "sin-in-out","exp-in-out" = "exp-in-out",
                                      "circle-in-out" = "circle-in-out","elastic-in-out" = "elastic-in-out",
                                      "back-in-out" = "back-in-out","bounce-in-out" = "bounce-in-out")
names(langSpecificGraphs$easingChoices) <- lang$adminMode$graphs$animationOptions$easingChoices
langSpecificGraphs$modeChoices <- c("immediate" = "immediate", "next"= "next", "afterall" = "afterall")
names(langSpecificGraphs$modeChoices) <- lang$adminMode$graphs$animationOptions$modeChoices
langSpecificGraphs$normChoices <- c("Number of occurances" = " ", "Percentage of occurances" = "percent", 
                                    "Number of occurances/ Bin interval size" = "density")
names(langSpecificGraphs$normChoices) <- lang$adminMode$graphs$histOptions$normChoices
langSpecificGraphs$barmodeChoices <- c("overlay" = "overlay", "stack" = "stack", "group" = "group", 
                                       "relative" = "relative")
names(langSpecificGraphs$barmodeChoices) <- lang$adminMode$graphs$histOptions$barmodeChoices   
langSpecificGraphs$orientationChoices <- c("vertical" = "vertical", "horizontal" = "horizontal")
names(langSpecificGraphs$orientationChoices) <- lang$adminMode$graphs$histOptions$orientationChoices    
langSpecificGraphs$pointShapeChoices <- c("dot" = "dot", "triangle" = "triangle", "square" = "square", 
                                          "diamond" = "diamond", "pentagon" = "pentagon", "hexagon" = "hexagon", 
                                          "circle" = "circle", "star" = "star", "plus" = "plus", "ex" = "ex")
names(langSpecificGraphs$pointShapeChoices) <- lang$adminMode$graphs$dygraphsOptions$generalOptions$pointShapeChoices 
langSpecificGraphs$positionChoices <- c("Top right" = "topright", "Bottom right" = "bottomright",
                                          "Bottom left" = "bottomleft", "Top left" = "topleft")
names(langSpecificGraphs$positionChoices) <- lang$adminMode$graphs$leafletOptions$positionChoices 
langSpecificGraphs$aggregatorChoices <- c("Count" = "Count","Count Unique Values" = "Count Unique Values",
                                          "List Unique Values" = "List Unique Values","Sum" = "Sum",
                                          "Integer Sum" = "Integer Sum","Average" = "Average","Median" = "Median",
                                          "Sample Variance" = "Sample Variance","Sample Standard Deviation" = "Sample Standard Deviation",
                                          "Minimum" = "Minimum","Maximum" = "Maximum","First" = "First","Last" = "Last",
                                          "Sum over Sum" = "Sum over Sum","80% Upper Bound" = "80% Upper Bound",
                                          "80% Lower Bound" = "80% Lower Bound","Sum as Fraction of Total" = "Sum as Fraction of Total",
                                          "Sum as Fraction of Rows" = "Sum as Fraction of Rows",
                                          "Sum as Fraction of Columns" = "Sum as Fraction of Columns",
                                          "Count as Fraction of Total" = "Count as Fraction of Total",
                                          "Count as Fraction of Rows" = "Count as Fraction of Rows",
                                          "Count as Fraction of Columns" = "Count as Fraction of Columns")
names(langSpecificGraphs$aggregatorChoices) <- lang$adminMode$graphs$pivotOptions$aggregatorChoices 
langSpecificGraphs$rendererChoices <- c("Table" = "Table","Table Barchart" = "Table Barchart","Heatmap" = "Heatmap",
                                        "Row Heatmap" = "Row Heatmap","Col Heatmap" = "Col Heatmap","Treemap" = "Treemap",
                                        "Horizontal Bar Chart" = "Horizontal Bar Chart",
                                        "Horizontal Stacked Bar Chart" = "Horizontal Stacked Bar Chart","Bar Chart" = "Bar Chart",
                                        "Stacked Bar Chart" = "Stacked Bar Chart","Line Chart" = "Line Chart",
                                        "Area Chart" = "Area Chart","Scatter Chart" = "Scatter Chart")
names(langSpecificGraphs$rendererChoices) <- lang$adminMode$graphs$pivotOptions$rendererChoices 
langSpecificGraphs$localeChoices <- c("cs" = "cs","da" = "da","de" = "de","en" = "en","es" = "es","fr" = "fr",
                                      "it" = "it","nl" = "nl","pl" = "pl","pt" = "pt","ru" = "ru","sq" = "sq",
                                      "tr" = "tr","zh" = "zh")
names(langSpecificGraphs$localeChoices) <- lang$adminMode$graphs$pivotOptions$options$localeChoices 
langSpecificGraphs$categoryorderChoices <- c("trace" = "trace", "category ascending" = "category ascending", "category descending" = "category descending", 
                                      "total ascending" = "total ascending", "total descending" = "total descending", 
                                      "min ascending" = "min ascending", "min descending" = "min descending", "max ascending" = "max ascending", 
                                      "max descending" = "max descending", "sum ascending" = "sum ascending", "sum descending" = "sum descending", 
                                      "mean ascending" = "mean ascending", "mean descending" = "mean descending", "median ascending" = "median ascending", 
                                      "median descending" = "median descending")
names(langSpecificGraphs$categoryorderChoices) <- lang$adminMode$graphs$axisOptions$categoryorderChoices

hideFilter <- function(){
  hideEl(session, "#preview_output_plotly-data_filter")
  hideEl(session, "#preview_output_dygraphs-data_filter")
  hideEl(session, "#preview_output_leaflet-data_filter")
  hideEl(session, "#preview_output_timevis-data_filter")
}
hideFilter()
#hideEl(session, any(startsWith("#preview_output_") && endsWith("-data_filter")))
scenMetaDb <- NULL
tryCatch({
  scenMetaDb <- db$fetchScenList(scode = SCODEMAP[['scen']])
}, error = function(e){
  flog.error("Problems fetching list of scenarios from database. Error message: %s.", e)
  errMsg <<- lang$errMsg$fetchScenData$desc
})
showErrorMsg(lang$errMsg$fetchScenData$title, errMsg)
if(!is.null(scenMetaDb) && nrow(scenMetaDb)){
  # by default, put most recently saved scenario first
  scenList <- db$formatScenList(scenMetaDb, stimeIdentifier, desc = TRUE)
  updateSelectInput(session, "scenList", choices = scenList)
  hideEl(session, "#noDbScen")
  showEl(session, "#dbScen")
}else{
  hideEl(session, "#dbScen")
  showEl(session, "#noDbScen")
}
updatePreviewData <- function(tabularInputWithData, tabularOutputWithData, configScalars){
  if(any(!isEmptyInput)){
    changeActiveSymbol(which(!isEmptyInput)[1])
  }else if(any(!isEmptyOutput)){
    changeActiveSymbol(which(!isEmptyOutput)[1] + length(modelIn))
  }else{
    showErrorMsg(lang$adminMode$graphs$errMsg$errTitle1, 
                 lang$adminMode$graphs$errMsg$errContent1)
    return()
  }
  showEl(session, "#preview_wrapper")
  updateSelectInput(session, "gams_symbols", 
                    choices = setNames(list(c(tabularInputWithData), 
                                            c(tabularOutputWithData)), 
                                       c(lang$adminMode$graphs$ui$input, 
                                         lang$adminMode$graphs$ui$output)))
  if(identical(length(configScalars), 3L) && nrow(configScalars)){
    session$sendCustomMessage("gms-setScalarOutputs", 
                              list(indices = configScalars[[1]], 
                                   aliases = configScalars[[2]]))
  }
  slideToggleEl(session, "#previewDataInputWrapper", 
                toggleIconDiv = "#previewDataInputToggle")
  rv$initData <<- TRUE
}
validateGraphConfig <- function(graphJSON){
  if(identical(is.na(graphJSON$graph$xaxis$rangefrom), TRUE) || identical(is.na(graphJSON$graph$xaxis$rangeto), TRUE)){
    return(lang$adminMode$graphs$validate$val1)
  }
  if(identical(input$chart_tool, "custom") && identical(nchar(trimws(graphJSON$outType)), 0L)){
    return("please specify a name!")
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
    isolate({
      rv$graphConfig$graph <- currentConfig[[current]]
      rv$graphConfig$graph$title <- activeSymbol$alias
    })
    allDataAvailable <<- TRUE
  }else if(subsetIdx == 1L){
    isolate({
      rv$graphConfig$graph <- list()
    })
  }else{
    isolate({
      rv$graphConfig$graph <- list(title = activeSymbol$alias, 
                                   tool = rv$graphConfig$graph$tool)
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
    scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "numeric"]
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
    scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "numeric"]
    session$sendCustomMessage("gms-setIndices", list(indices = names(headers), 
                                                     aliases = headerAliases,
                                                     scalarIndices = unname(scalarIndices),
                                                     scalarAliases = names(scalarIndices)))
  }
  indices       <- activeSymbol$indices
  rv$graphConfig$graph$title <- activeSymbol$alias
  if(isFALSE(rv$initData) || identical(input$chart_tool, "pie")){
    rv$refreshOptions <- rv$refreshOptions + 1L
  }
  rv$initData <- FALSE
  rv$initData <- TRUE
}




observeEvent(input$dbInput, {
  req(identical(length(input$scenList), 1L))
  
  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  rv$initData <- FALSE
  
  scenSelected <- regmatches(input$scenList, 
                             regexpr("_", input$scenList), 
                             invert = TRUE)
  sidToLoad  <- suppressWarnings(as.integer(lapply(scenSelected, '[[', 1L)[[1L]]))
  if(is.na(sidToLoad)){
    flog.error("Bad scenario ID selected: '%s'. This seems like the user tried to tamper with the app!", 
               lapply(scenSelected, '[[', 1L)[[1L]])
    return(NULL)
  }
  tryCatch({
    scenDataTmp <- db$loadScenarios(sidToLoad, 
                                    msgProgress = lang$progressBar$loadScenDb)[[1L]]
  }, error = function(e){
    flog.error("Some error occurred loading scenarios: '%s' from database. Error message: %s.", 
               paste(sidsToLoad, collapse = ", "), e)
    errMsg <<- lang$errMsg$loadScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
    return(NULL)
  }
  if(length(modelOut)){
    scenInputData          <- scenDataTmp[-seq_along(modelOut)]
    modelOutputData        <<- scenDataTmp[seq_along(modelOut)]
    names(modelOutputData) <<- names(modelOut)
  }else{
    scenInputData  <- scenDataTmp
    scenOutputData <- NULL
  }
  names(scenInputData)         <- inputDsNames
  newInputCount                <- 0L
  
  if(scalarsFileName %in% inputDsNames && 
     length(scenInputData[[length(scenInputData)]]) && 
     nrow(scenInputData[[length(scenInputData)]])){
    configScalars <<- scenInputData[[length(scenInputData)]]
  }else{
    configScalars <<- tibble()
  }
  idxScalarOut <- match(scalarsOutName, names(modelOut))
  if(!is.na(idxScalarOut) && length(modelOutputData[[idxScalarOut]]) && 
     nrow(modelOutputData[[idxScalarOut]])){
    configScalars <<- bind_rows(configScalars, modelOutputData[[idxScalarOut]])
  }
  
  errMsg    <-  NULL
  loadMode  <-  "scen"
  datasetsToFetch <- modelInTabularData
  overwriteInput <- TRUE
  
  source("./modules/input_load.R", local = TRUE)
  if(!is.null(errMsg)){
    return(NULL)
  }
  isEmptyInput         <<- isNonemptyDataset(modelInputData)
  tabularInputWithData <- inputSymMultiDimChoices[!isEmptyInput]
  isEmptyOutput        <<- isNonemptyDataset(modelOutputData)
  tabularOutputWithData <- outputSymMultiDimChoices[!isEmptyOutput]
  
  updatePreviewData(tabularInputWithData, 
                    tabularOutputWithData, 
                    configScalars)
})
observeEvent(input$localInput, {
  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  rv$initData <- FALSE
  
  fileType <- tolower(tools::file_ext(isolate(input$localInput$datapath)))
  errMsg <- NULL
  if(identical(fileType, "gdx") && useGdx){
    loadMode <- "gdx"
    datasetsToFetch <- c(modelInTabularData, scalarsFileName)
  }else if(fileType %in% c("xls", "xlsx")){
    loadMode <- "xlsx"
    tryCatch({
      xlsWbNames <- excel_sheets(input$localInput$datapath)
    }, error = function(e) {
      flog.error("Some error occurred reading the file: '%s'. Error message: %s.", 
                 as.character(isolate(input$localInput$name)), e)
      errMsg <<- sprintf(lang$errMsg$GAMSInput$excelRead, 
                         as.character(isolate(input$localInput$name)))
    })
    xlsWbNames <- vapply(strsplit(xlsWbNames, " ", fixed = TRUE), "[[", character(1L), 1L)
    # extract only sheets which are also in list of input parameters
    datasetsToFetch <- xlsWbNames[tolower(xlsWbNames) %in% modelInTabularData]
  }else{
    errMsg <- lang$errMsg$GAMSInput$desc
  }
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  # load input data 
  overwriteInput <- TRUE
  loadModeWorkDir  <- dirname(isolate(input$localInput$datapath))
  loadModeFileName <- basename(isolate(input$localInput$datapath))
  source("./modules/input_load.R", local = TRUE)
  if(!is.null(errMsg)){
    return(NULL)
  }
  isEmptyInput         <<- isNonemptyDataset(modelInputData)
  tabularInputWithData <- inputSymMultiDimChoices[!isEmptyInput]
  
  scalarIdTmp <- match(scalarsFileName, tolower(names(scenInputData)))[[1L]]
  if(!is.na(scalarIdTmp)){
    configScalars <<- scenInputData[[scalarIdTmp]]
  }else{
    configScalars <<- tibble()
  }
  tabularOutputWithData <- NULL
  if(identical(loadMode, "gdx") || any(names(modelOut) %in% xlsWbNames)){
    tryCatch({
        outputDataTmp <- loadScenData(scalarsName = scalarsOutName, metaData = modelOut, 
                                      workDir = dirname(isolate(input$localInput$datapath)), 
                                      modelName = modelName, errMsg = lang$errMsg$GAMSOutput,
                                      scalarsFileHeaders = scalarsFileHeaders, 
                                      templates = modelOutTemplate, method = loadMode, 
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
      configScalars <<- bind_rows(configScalars, outputDataTmp$scalar)
    }
    if(!is.null(outputDataTmp$tabular)){
      modelOutputData        <<- outputDataTmp$tabular
      names(modelOutputData) <<- names(modelOut)
    }
    isEmptyOutput         <<- isNonemptyDataset(modelOutputData)
    tabularOutputWithData <- outputSymMultiDimChoices[!isEmptyOutput]
  }
  updatePreviewData(tabularInputWithData, 
                    tabularOutputWithData, 
                    configScalars)
})
observeEvent(input$chart_title, {
  rv$graphConfig$graph$title <<- input$chart_title
})
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
  minThickness <- as.numeric(input$leafFlow_minThickness[2])
  if(is.na(minThickness)){
    flog.error("Minimum thickness: '%s' could not be converted to numeric.", 
               input$leafFlow_minThickness[2])
    return()
  }
  rv$graphConfig$graph$flows[[idLabelMap$
                                leaflet_flows[[as.integer(input$
                                                            leafFlow_minThickness[1])]]]]$minThickness <<- minThickness
})
observeEvent(input$leafFlow_maxThickness, {
  maxThickness <- as.numeric(input$leafFlow_maxThickness[2])
  if(is.na(maxThickness)){
    flog.error("Minimum thickness: '%s' could not be converted to numeric.", 
               input$leafFlow_maxThickness[2])
    return()
  }
  rv$graphConfig$graph$flows[[idLabelMap$
                                leaflet_flows[[as.integer(input$
                                                            leafFlow_maxThickness[1])]]]]$maxThickness <<- maxThickness
})


observeEvent(input$leafChart_lng, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_lng[1])]]]]$lng <<- input$leafChart_lng[2]
})
observeEvent(input$leafChart_lat, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_lat[1])]]]]$lat <<- input$leafChart_lat[2]
})
observeEvent(input$leafChart_chartdata, {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_chartdata[1])]]]]$chartdata <<- input$leafChart_chartdata[2:length(input$leafChart_chartdata)]
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
  if(nchar(input$leafChart_opacity[2]) && as.numeric(input$leafChart_opacity[2]) >= 0 && as.numeric(input$leafChart_opacity[2]) <= 1)
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_opacity[1])]]]]$opacity <<- as.numeric(input$leafChart_opacity[2])
  else
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_opacity[1])]]]]$opacity <<- NULL
})
observeEvent(input$leafChart_showlabels, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_showlabels[1])]]]]$showLabels <<- as.logical(input$leafChart_showlabels[2])
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
observeEvent(input$leafMark_icon, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_icon[1])]]]]$iconOptions$icon <<- input$leafMark_icon[2]
})
observeEvent(input$leafMark_iconColor, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_iconColor[1])]]]]$iconOptions$iconColor <<- input$leafMark_iconColor[2]
})
observeEvent(input$leafMark_markerColor, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_markerColor[1])]]]]$iconOptions$markerColor <<- input$leafMark_markerColor[2]
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

observeEvent(input$pivot_rows, ignoreNULL = FALSE, {
  if(length(input$pivot_rows) > 0)
    rv$graphConfig$pivottable$rows <<- input$pivot_rows
  else
    rv$graphConfig$pivottable$rows <<- NULL
})
observeEvent(input$pivot_cols, ignoreNULL = FALSE, {
  if(length(input$pivot_cols) > 0)
    rv$graphConfig$pivottable$cols <<- input$pivot_cols
  else
    rv$graphConfig$pivottable$cols <<- NULL
})
observeEvent(input$pivot_aggregatorName, {
  rv$graphConfig$pivottable$aggregatorName <<- input$pivot_aggregatorName
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

#observeEvent(input$timevis_series, {
#  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]] <<- input$timevis_series
#})

observeEvent(input$timedata_start, {
  rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_start[1])]]]]$start <<- input$timedata_start[2]
}, priority = -500)
observeEvent(input$timedata_end, {
  if(!identical(input$timedata_end[2],"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_end[1])]]]]$end <<- input$timedata_end[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_end[1])]]]]$end <<- NULL
}, priority = -500)
observeEvent(input$timedata_type, {
  rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_type[1])]]]]$type <<- input$timedata_type[2]
}, priority = -500)
observeEvent(input$timedata_title, {
  if(!identical(input$timedata_title[2],"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_title[1])]]]]$title <<- input$timedata_title[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_title[1])]]]]$title <<- NULL
}, priority = -500)
observeEvent(input$timedata_group, {
  if(!identical(input$timedata_group[2],"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_group[1])]]]]$group <<- input$timedata_group[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_group[1])]]]]$group <<- NULL
}, priority = -500)
observeEvent(input$timedata_subgroup, {
  if(!identical(input$timedata_subgroup[2],"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgroup[1])]]]]$subgroup <<- input$timedata_subgroup[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgroup[1])]]]]$subgroup <<- NULL
}, priority = -500)


observeEvent(input$timedata_grouptitle, {
  if(!identical(input$timedata_grouptitle[2],"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_grouptitle[1])]]]]$groupTitle <<- input$timedata_grouptitle[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_grouptitle[1])]]]]$groupTitle <<- NULL
}, priority = -500)
observeEvent(input$timedata_subgrouporder, {
  if(!identical(input$timedata_subgrouporder[2],"_"))
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgrouporder[1])]]]]$subgroupOrder <<- input$timedata_subgrouporder[2]
  else
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgrouporder[1])]]]]$subgroupOrder <<- NULL
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
observeEvent(input$timevis_editable, {
  rv$graphConfig$graph$editable <<- input$timevis_editable
})
observeEvent(input$timevis_multiselect, {
  rv$graphConfig$graph$multiselect <<- input$timevis_multiselect
})
observeEvent(input$timevis_showCurrentTime, {
  rv$graphConfig$graph$showCurrentTime <<- input$timevis_showCurrentTime
})

observeEvent(input$valuebox_width, {
  rv$graphConfig$options$width <<- as.numeric(input$valuebox_width)
})
observeEvent(input$valuebox_color, {
  rv$graphConfig$options$color <<- input$valuebox_color
})
observeEvent(c(input$valuebox_use_icon, input$valuebox_icon), {
  if(!identical(input$valuebox_icon, "") && identical(input$valuebox_use_icon, TRUE)){
    rv$graphConfig$options$icon$name <<- input$valuebox_icon
    rv$graphConfig$options$icon$lib <<- input$valuebox_icon_lib
  }else{
    rv$graphConfig$options$icon <<- NULL
  }
})
observeEvent(c(input$valuebox_use_icon, input$valuebox_icon_lib), {
  if(!is.null(input$valuebox_icon_lib) && identical(input$valuebox_use_icon, TRUE))
    rv$graphConfig$options$icon$lib <<- input$valuebox_icon_lib
  else
    rv$graphConfig$options$icon <<- NULL
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
# useful when color is specified automatically based on a domain!
observeEvent(input$hist_alpha, {
  rv$graphConfig$graph$alpha <<- input$hist_alpha
})
observeEvent(input$hist_cumulative, {
  rv$graphConfig$graph$cumulative <<- input$hist_cumulative
})
observeEvent(input$hist_horizontal, {
  if(identical(input$hist_horizontal, "horizontal"))
    rv$graphConfig$graph$horizontal <<- TRUE
  else
    rv$graphConfig$graph$horizontal <<- FALSE
})
observeEvent(input$add_piedata, {
  if(length(input$add_piedata) < 3L){
    return()
  }
  arrayId <- input$add_piedata[1]
  if(!length(arrayId) || !nchar(arrayId)){
    return()
  }
  if(!arrayId %in% names(rv$graphConfig$graph$traces)){
    rv$graphConfig$graph$traces[[arrayId]] <<- list(labels = activeSymbol$indices[[1]],
                                                    values = input$add_piedata[2],
                                                    name = names(activeSymbol$indices)[1],
                                                    hole = 0)
  }else{
    rv$graphConfig$graph$traces[[arrayId]]$values <<- input$add_piedata[2]
    rv$graphConfig$graph$traces[[arrayId]]$name   <<- names(activeSymbol$indices)[match(input$add_piedata[2],
                                                                                        activeSymbol$indices)[1]]
  }
})
observeEvent(input$remove_piedata, {
  if(length(input$remove_piedata) < 3L){
    return()
  }
  arrayId <- input$remove_piedata[3]
  if(!length(arrayId) || 
     !arrayId %in% names(rv$graphConfig$graph$traces)){
    return()
  }
  rv$graphConfig$graph$traces[[arrayId]] <<- NULL
})
observeEvent(input$chart_pielabel, {
  if(length(input$chart_pielabel) < 2L){
    return()
  }
  arrayId <- input$chart_pielabel[1]
  if(!length(arrayId) || 
     !arrayId %in% names(rv$graphConfig$graph$traces)){
    return()
  }
  rv$graphConfig$graph$traces[[arrayId]]$labels <- input$chart_pielabel[2]
})
observeEvent(input$chart_piehole, {
  if(length(input$chart_piehole) < 2L){
    return()
  }
  arrayId <- input$chart_piehole[1]
  if(!length(arrayId) || 
     !arrayId %in% names(rv$graphConfig$graph$traces)){
    return()
  }
  holeTmp <- suppressWarnings(as.numeric(input$chart_piehole[2]))
  if(is.na(holeTmp)){
    return()
  }
  rv$graphConfig$graph$traces[[arrayId]]$hole <- holeTmp
})
observeEvent(input$chart_piename, {
  if(length(input$chart_piename) < 2L){
    return()
  }
  arrayId <- input$chart_piename[1]
  if(!length(arrayId) || 
     !arrayId %in% names(rv$graphConfig$graph$traces)){
    return()
  }
  if(nchar(input$chart_piename[2])){
    rv$graphConfig$graph$traces[[arrayId]]$name <- input$chart_piename[2]
  }else{
    rv$graphConfig$graph$traces[[arrayId]]$name <- NULL
  }
})
observeEvent(input$marker_symbol, {
  if(identical(input$marker_symbol[2], "_")){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]]$marker$symbol <<- NULL
  }else{
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]]$marker$symbol <<- input$marker_symbol[2]
  }
}, priority = -500)
observeEvent(input$marker_color, {
  if(nchar(input$marker_color[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_color[1])]]]]$marker$color <<- input$marker_color[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_color[1])]]]]$marker$color <<- NULL
}, priority = -500)
observeEvent(input$marker_colorDep, {
  if(nchar(input$marker_colorDep[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_colorDep[1])]]]]$marker$color <<- input$marker_colorDep[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_colorDep[1])]]]]$marker$color <<- NULL
}, priority = -500)
observeEvent(input$marker_size, {
  if(nchar(input$marker_size[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_size[1])]]]]$marker$size <<- input$marker_size[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_size[1])]]]]$marker$size <<- NULL
}, priority = -500)
observeEvent(input$marker_sizemode, {
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_sizemode[1])]]]]$marker$sizemode <<- input$marker_sizemode[2]
}, priority = -500)
observeEvent(input$marker_maxsize, {
  if(nchar(input$marker_maxsize[2]) && !identical(as.numeric(input$marker_maxsize[2]), 0))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_maxsize[1])]]]]$marker$maxsize <<- as.numeric(input$marker_maxsize[2])
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_maxsize[1])]]]]$marker$maxsize <<- NULL
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
observeEvent(input$line_fill, {
  if(nchar(input$line_fill[2]))
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_fill[1])]]]]$fill <<- input$line_fill[2]
  else
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_fill[1])]]]]$fill <<- NULL
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
    # rv$graphConfig$graph$animation <- list(easing = "linear",
    #                                        mode = "immediate",
    #                                        redraw = TRUE,
    #                                        frame = 2e-06,
    #                                        transition = 2e-06,
    #                                        slider = list(fontcolor = "#000000",
    #                                                      hide = FALSE))
    # 
    frameTmp <- if(!is.null(input$animation_frame) && length(input$animation_frame)) {
      1000/input$animation_frame
    }else {
      500
    }
    rv$graphConfig$graph$animation <- list(easing = if(!is.null(input$animation_easing) && length(input$animation_easing)) input$animation_easing else "linear",
                                           mode = if(!is.null(input$animation_mode) && length(input$animation_mode)) input$animation_mode else "immediate",
                                           redraw = if(!is.null(input$animation_redraw) && length(input$animation_redraw)) input$animation_redraw else TRUE,
                                           frame = frameTmp,
                                           transition = if(!is.null(input$animation_transition) && length(input$animation_transition)) {
                                             min(input$animation_transition, frameTmp)
                                           }else {
                                             frameTmp
                                           },
                                           slider = list(fontcolor = if(!is.null(input$animation_slider_font_color) && length(input$animation_slider_font_color)) input$animation_slider_font_color else "#000000",
                                                         hide = if(!is.null(input$animation_slider_hide) && length(input$animation_slider_hide)) input$animation_slider_hide else FALSE))
    hideEl(session, "#no_plotly_animation_options")
    showEl(session, "#plotly_animation_options")
    traceframetmp <<- input$trace_frame[2]
  }else{
    hideEl(session, "#plotly_animation_options")
    showEl(session, "#no_plotly_animation_options")
    traceframetmp <<- NULL
    rv$graphConfig$graph$animation <<- NULL
  }
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$trace_frame[1])]]]]$frame <<- traceframetmp
}, priority = -450)
observeEvent(input$animation_frame, {
  req(input$trace_frame, rv$graphConfig$graph$animation, input$animation_frame)
  #frame = amount of time between frames (in milliseconds)
  frameTmp <- 1000/input$animation_frame
  rv$graphConfig$graph$animation$frame <<- frameTmp
  if(length(rv$graphConfig$graph$animation$transition) && 
     rv$graphConfig$graph$animation$transition > rv$graphConfig$graph$animation$frame){
    rv$graphConfig$graph$animation$transition <<- frameTmp
  }
}, priority = -500)
observeEvent(input$animation_transition, {
  req(rv$graphConfig$graph$animation)
  if(length(input$animation_frame))
    return(rv$graphConfig$graph$animation$transition <<- min(1000/input$animation_frame, input$animation_transition))
  rv$graphConfig$graph$animation$transition <<- input$animation_transition
}, priority = -500)
observeEvent(input$animation_easing, {
  req(rv$graphConfig$graph$animation)
  rv$graphConfig$graph$animation$easing <<- input$animation_easing
}, priority = -500)
observeEvent(input$animation_redraw, {
  req(rv$graphConfig$graph$animation)
    rv$graphConfig$graph$animation$redraw <<- as.logical(input$animation_redraw)
}, priority = -500)
observeEvent(input$animation_mode, {
  req(rv$graphConfig$graph$animation)
  rv$graphConfig$graph$animation$mode <<- input$animation_mode
})
observeEvent(input$animation_slider_hide, {
  req(rv$graphConfig$graph$animation)
  rv$graphConfig$graph$animation$slider$hide <<- as.logical(input$animation_slider_hide)
}, priority = -500)
observeEvent(input$animation_slider_label, {
  req(rv$graphConfig$graph$animation)
  if(length(input$animation_slider_label) && !identical(input$animation_slider_prefix, ""))
    rv$graphConfig$graph$animation$slider$label <<- input$animation_slider_label
  else 
    rv$graphConfig$graph$animation$slider$label <<- NULL
}, priority = -500)
observeEvent(input$animation_slider_prefix, {
  req(rv$graphConfig$graph$animation)
  if(length(input$animation_slider_prefix) && !identical(input$animation_slider_prefix, ""))
    rv$graphConfig$graph$animation$slider$prefix <<- input$animation_slider_prefix
  else
    rv$graphConfig$graph$animation$slider$prefix <<- NULL
}, priority = -500)
observeEvent(input$animation_slider_font_color, {
  req(rv$graphConfig$graph$animation)
  if(nchar(input$animation_slider_font_color))
    rv$graphConfig$graph$animation$slider$fontcolor <<- input$animation_slider_font_color
  else
    rv$graphConfig$graph$animation$slider$fontcolor <<- NULL
}, priority = -500)
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
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_fillGraph[1])]]]]$fillGraph <<- as.logical(input$dyopt_fillGraph[2])
  }else{
    rv$graphConfig$graph$dyOptions$fillGraph <<- input$dyopt_fillGraph
  }
})
observeEvent(input$dyopt_stepPlot, {
  if(length(input$dyopt_stepPlot) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_stepPlot[1])]]]]$stepPlot <<- as.logical(input$dyopt_stepPlot[2])
  }else{
    rv$graphConfig$graph$dyOptions$stepPlot <<- input$dyopt_stepPlot
  }
})
observeEvent(input$dyopt_stemPlot, {
  if(length(input$dyopt_stemPlot) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_stemPlot[1])]]]]$stemPlot <<- as.logical(input$dyopt_stemPlot[2])
  }else{
    rv$graphConfig$graph$dyOptions$stemPlot <<- input$dyopt_stemPlot
  }
})
observeEvent(input$dyopt_fillAlpha, {
  rv$graphConfig$graph$dyOptions$fillAlpha <<- input$dyopt_fillAlpha
})
observeEvent(input$dyopt_drawPoints, {
  if(length(input$dyopt_drawPoints) > 1){
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_drawPoints[1])]]]]$drawPoints <<- as.logical(input$dyopt_drawPoints[2])
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
observeEvent(input$dyhighlight_activate, {
  if(isTRUE(input$dyhighlight_activate)){
    rv$graphConfig$graph$dyHighlight <<- list(highlightCircleSize = input$dyhigh_circleSize, 
                                              highlightSeriesBackgroundAlpha = input$dyhigh_seriesBackgroundAlpha,
                                              hideOnMouseOut = input$dyhigh_hideOnMouseOut)
  }else{
    rv$graphConfig$graph$dyHighlight <<- list(highlightCircleSize = 0L, 
                                              highlightSeriesBackgroundAlpha = 1L,
                                              hideOnMouseOut = TRUE)
  }
})
observeEvent(input$dyhigh_circleSize, {
  if(isTRUE(input$dyhighlight_activate))
    rv$graphConfig$graph$dyHighlight$highlightCircleSize <<- input$dyhigh_circleSize
})
observeEvent(input$dyhigh_seriesBackgroundAlpha, {
  if(isTRUE(input$dyhighlight_activate))
    rv$graphConfig$graph$dyHighlight$highlightSeriesBackgroundAlpha <<- input$dyhigh_seriesBackgroundAlpha
})
observeEvent(input$dyhigh_hideOnMouseOut, {
  if(isTRUE(input$dyhighlight_activate))
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
  if(input$dyAnnotation_width[2] == 0L){
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_width[1])]]]]$width <<- NULL
  }else{
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_width[1])]]]]$width <<- input$dyAnnotation_width[2]
  }
})
observeEvent(input$dyAnnotation_height, {
  if(input$dyAnnotation_width[2] == 0L){
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
observeEvent(input$staticPlot, {
  if(isTRUE(input$staticPlot)){
    rv$graphConfig$graph$staticPlot <<- TRUE
  }else{
    rv$graphConfig$graph$staticPlot <<- FALSE
  }
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
observe({
  req(input$chart_tool %in% plotlyChartTools)
  if(isFALSE(input$fixedHeightCheck) ||
     !is.numeric(input$fixedHeight) ||
     input$fixedHeight < 0){
    rv$graphConfig$graph$fixedHeight <<- NULL
    return()
  }
  rv$graphConfig$graph$fixedHeight <<- input$fixedHeight
})
observe({
  req(input$chart_tool %in% plotlyChartTools)
  if(isFALSE(input$fixedWidthCheck) ||
     !is.numeric(input$fixedWidth) ||
     input$fixedWidth < 0){
    rv$graphConfig$graph$fixedWidth <<- NULL
    return()
  }
  rv$graphConfig$graph$fixedWidth <<- input$fixedWidth
})

observeEvent(input$outType, {
  if(identical(input$outType, TRUE))
    outTypetmp <<- "dtGraph"
  else
    outTypetmp <<- "graph"
  rv$graphConfig$outType <<- outTypetmp
})
observeEvent(input$x_title, {
  rv$graphConfig$graph$xaxis$title <<- input$x_title
})
observeEvent(input$x_categoryorder, {
  rv$graphConfig$graph$xaxis$categoryorder <<- input$x_categoryorder
})
observe({
  req(input$chart_tool %in% plotlyChartTools, !identical(input$chart_tool, "pie"))
  if(isFALSE(input$scaleratio_check) ||
     !is.numeric(input$scaleratio) ||
     input$scaleratio < 0.1){
    rv$graphConfig$graph$yaxis$scaleratio <<- NULL
    rv$graphConfig$graph$yaxis$scaleanchor <<- NULL
    return()
  }
  rv$graphConfig$graph$yaxis$scaleratio <<- input$scaleratio
  rv$graphConfig$graph$yaxis$scaleanchor <<- "x"
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
observeEvent(input$x_rangefrom, {
  val <- NULL
  if(nchar(input$x_rangefrom)){
    val <- suppressWarnings(as.numeric(input$x_rangefrom))
    if(is.na(val))
      val <- input$x_rangefrom
  }
  rv$graphConfig$graph$xaxis$rangefrom <<- val
})
observeEvent(input$x_rangeto, {
  val <- NULL
  if(nchar(input$x_rangeto)){
    val <- suppressWarnings(as.numeric(input$x_rangeto))
    if(is.na(val)){
      val <- input$x_rangeto
    }
  }
  rv$graphConfig$graph$xaxis$rangeto <<- val
})
observeEvent(input$y_title, {
  rv$graphConfig$graph$yaxis$title <<- input$y_title
})
observeEvent(input$y_categoryorder, {
  rv$graphConfig$graph$yaxis$categoryorder <<- input$y_categoryorder
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
observeEvent(input$y_rangefrom, {
  val <- NULL
  if(nchar(input$y_rangefrom)){
    val <- suppressWarnings(as.numeric(input$y_rangefrom))
    if(is.na(val))
      val <- input$y_rangefrom
  }
  rv$graphConfig$graph$yaxis$rangefrom <<- val
})
observeEvent(input$y_rangeto, {
  val <- NULL
  if(nchar(input$y_rangeto)){
    val <- suppressWarnings(as.numeric(input$y_rangeto))
    if(is.na(val))
      val <- input$y_rangeto
  }
  rv$graphConfig$graph$yaxis$rangeto <<- val
})
observeEvent(input$hist_label, {
  if(nchar(input$hist_label[[2]]))
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_label[1])]]]]$labels <<- input$hist_label[2]
  else
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_label[1])]]]]$labels <<- NULL
})
observeEvent(input$hist_color, {
  if(nchar(input$hist_color[[2]])){
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_color[1])]]]]$color <<- input$hist_color[2]
  }else{
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_color[1])]]]]$color <<- NULL
  }
})
observeEvent(input$chart_color, {
  if(identical(input$chart_color, "_")){
    rv$graphConfig$graph$color <<- NULL
  }else{
    rv$graphConfig$graph$color <<- input$chart_color
  }
})
observeEvent(input$chart_symbol, {
  if(identical(input$chart_symbol, "_")){
    rv$graphConfig$graph$symbol <<- NULL
  }else{
    rv$graphConfig$graph$symbol <<- input$chart_symbol
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
  if(el_id %in% c("dy_dyShading", "dy_dyLimit", "leaflet_markers", "leaflet_flows", "leaflet_minicharts", "timevis_series", "timevis_custom")){
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
    }else if(identical(el_id, "timevis_custom")){
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$time <- input$add_array_el[2]
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
        # change labels
        if(identical(el_id, "chart_ydata")){
          currentContent$label <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
        }else if(identical(el_id, "hist_xdata")){
          currentContent$labels <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
        }else if(identical(el_id, "dy_dyAnnotation")){
          currentContent$text <- configScalars[[2]][match(chart_label, configScalars[[1]])][1]
        }
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
    if(input$chart_tool %in% plotlyChartTools){
      if(identical(input$chart_tool, "scatter")){
        newContent  <- list(label = label, 
                            mode = "markers",
                            fill = "none",
                            marker = list(
                              opacity = 1L,
                              size = 6L,
                              line = list(width = 0L)
                            ),
                            showlegend = TRUE)
      }else if(identical(input$chart_tool, "bubble")){
        newContent  <- list(label = label, 
                            mode = "markers",
                            marker = list(
                              symbol = "circle",
                              opacity = 1L,
                              size = label,
                              sizemode = "area",
                              color = label,
                              line = list(width = 0L)
                            ),
                            showlegend = TRUE)
      }else if(identical(input$chart_tool, "line")){
        newContent  <- list(label = label, 
                            mode = "lines",
                            line = list(
                              width = 2L,
                              shape = "linear",
                              dash = "solid"),
                            showlegend = TRUE)
      }else{
        newContent <- list(label = label,
                           mode = "lines",
                           marker = list(line = list(width = 0L)))
      }
    }else{
      newContent  <- list(label = label, 
                          stemPlot = FALSE, stepPlot = FALSE, 
                          fillGraph = FALSE, drawPoints = FALSE, 
                          pointShape = "dot",
                          pointSize = 2L)
    }
  }else if(identical(el_id, "hist_xdata")){
    label       <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
    newContent  <- list(labels = label, color = "#000000")
  }else if(identical(el_id, "dy_dyEvent")){
    newContent  <- list(labelLoc = "top", color = "rgb(0,0,0)", strokePattern = "dashed")
  }else if(identical(el_id, "dy_dyLimit")){
    newContent  <- list(limit = input$add_array_el[2], 
                        labelLoc = "left", 
                        color = "rgb(0,0,0)", 
                        strokePattern = "dashed")
  }else if(identical(el_id, "dy_dyAnnotation")){
    newContent <- list(text = configScalars[[2]][1], attachAtBottom = FALSE)
  }else if(identical(el_id, "dy_dyShading")){
    newContent <- list(from = input$add_array_el[2], 
                       to = input$add_array_el[2], 
                       color = "#efefef", 
                       axis = "x")
  }else if(identical(el_id, "leaflet_markers")){
    newContent <- list(lng = input$add_array_el[2], 
                       lat = input$add_array_el[2],
                       iconOptions = list(icon = "circle", 
                                          iconColor = "#000000",
                                          markerColor = "blue"),
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
                       chartdata = input$add_array_el[2][1],
                       type = "auto",
                       width = 30, 
                       height = 30, 
                       opacity = 1,
                       showLabels = FALSE,
                       transitionTime = 750,
                       legend = TRUE,
                       legendPosition = "topright")
  }else if(identical(el_id, "timevis_series")){
    newContent  <- list(content = input$add_array_el[2], 
                        start = input$add_array_el[2],
                        type = "box")
  }else if(identical(el_id, "timevis_custom")){
    newContent  <- list(time = input$add_array_el[2])
  }else{
    newContent <- NULL
  }
  rv$graphConfig$graph[[JSON_id]][[chart_label]] <<- newContent
})
observeEvent(input$remove_array_el, {
  array_id <- input$remove_array_el[1]
  el_id    <- as.integer(input$remove_array_el[3])
  JSON_id     <- gsub("^[^_]*_", "", array_id)
  if(startsWith(JSON_id, "ydata")){
    array_id <- "chart_ydata"
    JSON_id     <- "ydata"
  }
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

observeEvent(input$custom_name, {
  rv$graphConfig$outType <<- gsub(" ", "", input$custom_name, fixed = TRUE)
})
observeEvent(input$custom_packages, {
  req(length(input$custom_packages))
  rv$graphConfig$packages <<- input$custom_packages
})

observeEvent(input$filter_dim, {
  if(input$chart_tool %in% plotlyChartTools){
    chartToolTmp <- "plotly"
  }else{
    chartToolTmp <- input$chart_tool
  }
 if(isFALSE(input$filter_dim)){
   rv$graphConfig$graph$filter <<- NULL
   hideEl(session, paste0("#preview_output_", chartToolTmp, "-data_filter"))
 }else{
   rv$graphConfig$graph$filter <<- list(col = input$filter_col,
                                        label = input$filter_label,
                                        multiple = input$filter_multiple,
                                        date = FALSE)
   showEl(session, paste0("#preview_output_", chartToolTmp, "-data_filter"))
 }
})
observeEvent(input$filter_col, {
  req(isTRUE(input$filter_dim))
  rv$graphConfig$graph$filter$col <<- input$filter_col
})
observeEvent(input$filter_label, {
  req(isTRUE(input$filter_dim))
  newLabel <- ""
  if(nchar(input$filter_label)){
    rv$graphConfig$graph$filter$label <<- input$filter_label
    newLabel <- input$filter_label
  }else{
    rv$graphConfig$graph$filter$label <<- NULL
  }
  updateSelectInput(session, paste0("#preview_output_", input$chart_tool, "-data_filter"), 
                    label = newLabel)
})
observeEvent(input$filter_multiple, {
  req(isTRUE(input$filter_dim))
  rv$graphConfig$graph$filter$multiple <<- input$filter_multiple
})
observeEvent(input$filter_date, {
  req(isTRUE(input$filter_dim))
  rv$graphConfig$graph$filter$date <<- input$filter_date
})

observeEvent(input$gams_symbols, {
  req(input$gams_symbols)
  symbolID <- match(isolate(input$gams_symbols), names(modelIn))
  if(is.na(symbolID)){
    symbolID <- match(isolate(input$gams_symbols), names(modelOut)) + length(modelIn)
  }
  changeActiveSymbol(symbolID)
  
  if(identical(input$gams_symbols, scalarsOutName)){
    updateSelectInput(session, "chart_tool", choices = setNames(c("valuebox"),
                                                                lang$adminMode$graphs$updateToolScalars))
    newChartTool <<- "valuebox"
  }else{
    updateSelectInput(session, "chart_tool", choices = setNames(c("pie", "bar", "scatter", "line", "bubble", "hist", "dygraphs", "leaflet", "timevis", "pivot", "custom"),
                                                                lang$adminMode$graphs$updateToolNoScalars))
    newChartTool <<- "pie"
  }
  if(tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))){
    showEl(session, "#deleteGraph")
  }else{
    hideEl(session, "#deleteGraph")
  }
})
observeEvent({
  input$chart_tool
  rv$refreshOptions}, {
    req(rv$initData)
    allDataAvailable <<- FALSE
    if(length(newChartTool)){
      chartTool <- newChartTool
      newChartTool <<- character(0L)
    }else{
      chartTool <- input$chart_tool
    }
    if(!identical(chartTool, "pivot"))
      rv$graphConfig$pivottable <<- NULL
    if(!identical(chartTool, "leaflet"))
      rv$graphConfig$graph$layersControl <<- NULL
    if(!identical(chartTool, "valuebox"))
      rv$graphConfig$options <<- NULL
    saveAndReload(isolate(chartTool), "pie")
    hideFilter()
    removeUI(selector = "#tool_options div", multiple = TRUE)
    if(chartTool %in% plotlyChartTools){
      rv$graphConfig$graph$tool <<- "plotly"
    }else{
      rv$graphConfig$graph$tool <<- chartTool
    }
    removeClassEl(session, ".category-btn", "category-btn-active")
    hideEl(session, ".category-btn")
    if(identical(chartTool, "pie")){
      rv$graphConfig$graph$type <<- "pie"
      showEl(session, ".category-btn-pie")
      addClassEl(session, id = "#categoryPie1", "category-btn-active")
      insertUI(selector = "#tool_options", getPieOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "bar")){
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "bar"
      showEl(session, ".category-btn-bar")
      addClassEl(session, id = "#categoryBar1", "category-btn-active")
      insertUI(selector = "#tool_options", getBarOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "scatter")){
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "scatter"
      showEl(session, ".category-btn-scatter")
      addClassEl(session, id = "#categoryScatter1", "category-btn-active")
      insertUI(selector = "#tool_options", getScatterOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "line")){
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "scatter"
      showEl(session, ".category-btn-line")
      addClassEl(session, id = "#categoryLine1", "category-btn-active")
      insertUI(selector = "#tool_options", getLineOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "bubble")){
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "bubble"
      showEl(session, ".category-btn-bubble")
      addClassEl(session, id = "#categoryBubble1", "category-btn-active")
      insertUI(selector = "#tool_options", getBubbleOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "hist")){
      rv$graphConfig$graph$type <<- "hist"
      showEl(session, ".category-btn-hist")
      addClassEl(session, id = "#categoryHist1", "category-btn-active")
      insertUI(selector = "#tool_options", getHistOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "dygraphs")){
      currentSelection$noLayers <<- 1L
      showEl(session, ".category-btn-dygraphs")
      addClassEl(session, id = "#categoryDygraphs1", "category-btn-active")
      insertUI(selector = "#tool_options",
               tags$div(id = "dygraph_options", getDygraphsOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "leaflet")){
      currentSelection$noLayers <<- 1L
      showEl(session, ".category-btn-leaflet")
      addClassEl(session, id = "#categoryLeaflet1", "category-btn-active")
      insertUI(selector = "#tool_options",
               tags$div(id = "leaflet_options", getLeafletOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "timevis")){
      currentSelection$noLayers <<- 1L
      showEl(session, ".category-btn-timevis")
      addClassEl(session, id = "#categoryTimevis1", "category-btn-active")
      insertUI(selector = "#tool_options",
               tags$div(id = "timevis_options", getTimevisOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "pivot")){
      currentSelection$noLayers <<- 1L
      showEl(session, ".category-btn-pivot")
      addClassEl(session, id = "#categoryPivot1", "category-btn-active")
      insertUI(selector = "#tool_options",
               tags$div(id = "pivot_options", getPivotOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "custom")){
      currentSelection$noLayers <<- 1L
      showEl(session, ".category-btn-custom")
      addClassEl(session, id = "#categoryCustom1", "category-btn-active")
      insertUI(selector = "#tool_options",
               tags$div(id = "custom_options", getCustomOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(chartTool, "valuebox")){
      currentSelection$noLayers <<- 1L
      showEl(session, ".category-btn-valuebox")
      addClassEl(session, id = "#categoryValuebox1", "category-btn-active")
      insertUI(selector = "#tool_options",
               tags$div(id = "valuebox_options", getValueboxOptions()), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }
    rv$refreshContent <- rv$refreshContent + 1L
  })
getPieOptions <- reactive({
  req(rv$initData)
  if(rv$refreshContent == 0){
    return()
  }
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    valuesTmp <- if(length(scalarIndices)) scalarIndices[[1]] else ""
    rv$graphConfig$graph$traces <<- list()
    rv$graphConfig$graph$showlegend <<- TRUE
    rv$graphConfig$graph$traces[['1']] <<- list(labels = indices[[1]],
                                              values = valuesTmp,
                                              hole = 0,
                                              name = valuesTmp)
  })
  # We have to provide the symbol name with create array el, because else Javascript 
  # will insert the new array element inside the wrapper of the previous selected symbol 
  # due to the delay between R sending the new HTML content and the javascript call to be executed
  tagList(
    tags$div(class="cat-body cat-body-1",
             createArray(session, "chart_piedata", lang$adminMode$graphs$chartOptions$ydata, 
                         class_outer="array-wrapper-outer-graph", hr = FALSE, 
                         symbolName = activeSymbol$name)),
    tags$div(class="cat-body cat-body-2", style="display:none;",
             getOptionSection()
    )
  )
})
getChartOptions <- reactive({
  req(rv$resetRE > 0L)
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$xdata      <<- indices[[1]]
    rv$graphConfig$outType <<- "graph" 
    rv$graphConfig$graph$showlegend <<- TRUE
  })
  tagList(
    tags$div(class="cat-body cat-body-3 cat-body-8 cat-body-13 cat-body-18", 
             selectInput("chart_xdata", lang$adminMode$graphs$chartOptions$xdata,
                         choices = indices),
             getAxisOptions("x", names(indices)[1])
    ),
    tags$div(class="cat-body cat-body-4 cat-body-9 cat-body-14 cat-body-19", style="display:none;",
             createArray(session, "chart_ydata", lang$adminMode$graphs$chartOptions$ydata, isolate(input$chart_tool), 
                         class_outer="array-wrapper-outer-graph", hr = FALSE),
             getAxisOptions("y", names(scalarIndices)[1])
    ),
    tags$div(class="cat-body cat-body-5 cat-body-10 cat-body-15 cat-body-20", style="display:none;",
             getColorPivotOptions(),
             getFilterOptions()
    ),
    tags$div(class="cat-body cat-body-6 cat-body-12 cat-body-17 cat-body-22", style="display:none;",
             getOptionSection()
    ),
    tags$div(class="cat-body cat-body-11 cat-body-16 cat-body-21", style="display:none;",
               tags$div(id = "no_plotly_animation_options", class = "shiny-input-container config-message", 
                        style = "display: block;", lang$adminMode$graphs$animationOptions$noAnimation),
               tags$div(id = "plotly_animation_options", class = "shiny-input-container", 
                        style = "display: none;", getAnimationOptions())
    )
  )
})
getBarOptions  <- reactive({
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$barmode <<- "group"
    rv$graphConfig$graph$ydata   <<- list()
    rv$graphConfig$graph$ydata[[indices[[1]]]] <<- list(label = names(indices)[1],
                                                        mode = "lines",
                                                        marker = list(line = list(width = 0L)))
    idLabelMap$chart_ydata[[1]] <<- indices[[1]]
  })
  tagList(
    tags$div(class="cat-body cat-body-7", style="display:none;",
             selectInput("bar_mode", lang$adminMode$graphs$barOptions$mode, choices = langSpecificGraphs$barmode),
             selectInput("bar_orientation", lang$adminMode$graphs$barOptions$orientation,
                         choices = langSpecificGraphs$barOrientation)
             ),
    getChartOptions(),
    tags$div(class="cat-body cat-body-5", style="display:none;",
             selectInput("bar_width", lang$adminMode$graphs$barOptions$width,
                         choices = c("_", scalarIndices))
             ))
})
getAxisOptions <- function(id, title, labelOnly = FALSE){
  isolate({
    rv$graphConfig$graph[[id %+% "axis"]]$title <<- title
    if(!labelOnly){
      rv$graphConfig$graph[[id %+% "axis"]]$showgrid <<- FALSE
      rv$graphConfig$graph[[id %+% "axis"]]$zeroline <<- FALSE
      rv$graphConfig$graph[[id %+% "axis"]]$showticklabels <<- TRUE
      rv$graphConfig$graph[[id %+% "axis"]]$categoryorder <<- "trace"
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
    textInput(id %+% "_title", lang$adminMode$graphs$axisOptions$title, value = title),
    selectInput(id %+% "_categoryorder", lang$adminMode$graphs$axisOptions$categoryorder, choices = langSpecificGraphs$categoryorderChoices),
    if(!identical(rv$graphConfig$graph$type, "pie") && identical(id, "y")){
      tags$div(class = "shiny-input-container", style = "display:inline-block;",
               tags$div(
                        tags$div(style = "max-width:400px;",
                                 tags$div(style="display:inline-block", 
                                          checkboxInput_MIRO("scaleratio_check", 
                                                             lang$adminMode$graphs$axisOptions$scaleRatioCheck, 
                                                             value = FALSE)),
                                 conditionalPanel(condition = 'input.scaleratio_check===true', 
                                                  style="display:inline-block;padding-left:35px;", 
                                                  tags$div(
                                                    numericInput("scaleratio", 
                                                                 lang$adminMode$graphs$axisOptions$scaleRatio, 
                                                                 min = 0.1, value = 1L, step = 0.1))
                                 ))
               )
      )
    },
    checkboxInput_MIRO(id %+% "_showgrid", lang$adminMode$graphs$axisOptions$showgrid),
    checkboxInput_MIRO(id %+% "_zeroline", lang$adminMode$graphs$axisOptions$zeroline),
    checkboxInput_MIRO(id %+% "_showticklabels", lang$adminMode$graphs$axisOptions$showticklabels, TRUE),
    if(identical(input$chart_tool, "scatter") || identical(input$chart_tool, "line") || identical(input$chart_tool, "bubble")){
      tags$div(class = "shiny-input-container", style = "display:inline-block;",
               tags$label(class = "cb-label shiny-input-container", "for" = "range-wrapper", lang$adminMode$graphs$axisOptions$range),
               tags$div(style = "padding-top: 10px;",
                 tags$div(id = "range-wrapper",
                          tags$div(style = "max-width:400px;",
                                   tags$div(style="display:inline-block", textInput(id %+% "_rangefrom", lang$adminMode$graphs$axisOptions$rangeFrom, value = NULL)),
                                   tags$div(style="display:inline-block", textInput(id %+% "_rangeto", lang$adminMode$graphs$axisOptions$rangeTo, value = NULL)))
                 ))
      )
    }
  )
}
getOptionSection <- reactive({
  req(rv$initData)
  isolate({
    rv$graphConfig$graph$showlegend <<- TRUE
  })
  tagList(
    textInput("chart_title", lang$adminMode$graphs$ui$chartTitle, value = activeSymbol$alias),
    checkboxInput_MIRO("showlegend", lang$adminMode$graphs$chartOptions$options$showlegend, value = TRUE),
      #tagList(
        tags$div(class = "shiny-input-container", 
                   tags$div(style = "max-width:400px;",
                            tags$div(style="display:inline-block", 
                                     checkboxInput_MIRO("fixedHeightCheck", 
                                                        lang$adminMode$graphs$chartOptions$options$fixedHeightCheck, 
                                                        value = FALSE)),
                            conditionalPanel(condition = 'input.fixedHeightCheck===true', 
                                             style="display:inline-block; padding-left:35px;", 
                                             numericInput("fixedHeight", 
                                                          lang$adminMode$graphs$chartOptions$options$fixedHeight, 
                                                          min = 1L, value = 700L, step = 1L)
                            ))
        ),
        tags$div(class = "shiny-input-container", 
                 tags$div(style = "max-width:400px;",
                          tags$div(style="display:inline-block", 
                                   checkboxInput_MIRO("fixedWidthCheck", 
                                                      lang$adminMode$graphs$chartOptions$options$fixedWidthCheck, 
                                                      value = FALSE)),
                          conditionalPanel(condition = 'input.fixedWidthCheck===true', 
                                           style="display:inline-block; padding-left:35px;",
                                           numericInput("fixedWidth", 
                                                        lang$adminMode$graphs$chartOptions$options$fixedWidth, 
                                                        min = 1L, value = 1335L, step = 1L)
                          ))
        ),
      #)
    colorPickerInput("paper_bgcolor", lang$adminMode$graphs$chartOptions$options$paperBgColor, value = NULL),
    colorPickerInput("plot_bgcolor", lang$adminMode$graphs$chartOptions$options$plotBgColor, value = NULL),
    checkboxInput_MIRO("staticPlot", lang$adminMode$graphs$chartOptions$options$staticPlot, value = FALSE),
    getOuttype()
  )
})
getOuttype <- reactive({
  tagList(
    checkboxInput_MIRO("outType", tags$div(lang$adminMode$graphs$chartOptions$options$outType, tags$a("", class="info-wrapper", href="https://gams.com/miro/charts.html#table-graph-split-screen", 
                                                                                                      tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")), value = FALSE)
  )
})
getScatterOptions  <- reactive({
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$ydata <- list()
    if(length(scalarIndices)){
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1], 
                                                                mode = "markers",
                                                                fill = "none",
                                                                marker = list(
                                                                  opacity = 1L,
                                                                  size = 6L,
                                                                  line = list(width = 0L)
                                                                ),
                                                                showlegend = TRUE)
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    }else{
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  tagList(
    getChartOptions(),
    tags$div(class="cat-body cat-body-10", style="display:none;",
             selectInput("chart_symbol", lang$adminMode$graphs$chartOptions$symbol,
                         choices = c("_", indices))
    ))
})
getBubbleOptions  <- reactive({
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$ydata <- list()
    if(length(scalarIndices)){
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1], 
                                                                mode = "markers",
                                                                marker = list(
                                                                  symbol = "circle",
                                                                  opacity = 1L,
                                                                  size = scalarIndices[1],
                                                                  sizemode = "area",
                                                                  color = scalarIndices[1],
                                                                  line = list(width = 0L)
                                                                ),
                                                                showlegend = TRUE)
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    }else{
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  getChartOptions()
})
getLineOptions  <- reactive({
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$ydata <- list()
    if(length(scalarIndices)){
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1], 
                                                              mode = "lines",
                                                              line = list(width = 2L,
                                                                          shape = "linear",
                                                                          dash = "solid"),
                                                              showlegend = TRUE)
    idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    }else{
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  getChartOptions()
})
getValueboxOptions  <- reactive({
  rv$initData
  rv$refreshContent
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  #isolate({
  #  rv$graphConfig$options$width <<- 4L
  #  rv$graphConfig$options$color <<- "aqua"
  #})
  tagList(
    tags$div(class="cat-body cat-body-49",
             selectInput("valuebox_width", lang$adminMode$graphs$valueboxOptions$width,
                         choices = c("1" = 12, "2" = 6, "3" = 4, "4" = 3), 
                         selected = 4),
             selectInput("valuebox_color", lang$adminMode$graphs$valueboxOptions$color,
                         choices = langSpecificGraphs$valueboxColor, 
                         selected = "aqua"),
             checkboxInput_MIRO("valuebox_use_icon", lang$adminMode$graphs$valueboxOptions$useIcon),
             conditionalPanel(condition = 'input.valuebox_use_icon===true',
                              tags$div(class = "shiny-input-container", style = "padding-left: 20px;",
                                       selectInput("valuebox_icon_lib", lang$adminMode$graphs$valueboxOptions$iconLib,
                                                   choices = langSpecificGraphs$libChoices,
                                                   selected = "font-awesome"),
                                       textInput("valuebox_icon", span(HTML(htmltools::htmlEscape(lang$adminMode$graphs$valueboxOptions$icon1)), tags$br(), 
                                                                       tags$a(href="http://fontawesome.io/icons/", target = "_blank", "http://fontawesome.io/icons/"), 
                                                                       htmltools::htmlEscape(lang$adminMode$graphs$valueboxOptions$icon2), tags$a(href="http://getbootstrap.com/components/#glyphicons", 
                                                                                                                                                  target = "_blank", "http://getbootstrap.com/components/#glyphicons"), 
                                                                       htmltools::htmlEscape(lang$adminMode$graphs$valueboxOptions$icon3)),
                                                 value = "", placeholder = "dollar-sign")))
    )
  )
})
getAnimationOptions  <- reactive({
  tagList(
    numericInput("animation_frame", lang$adminMode$graphs$animationOptions$frame, min = 0L, value = 1L), 
    numericInput("animation_transition", lang$adminMode$graphs$animationOptions$transition, min = 0L, value = 500L),
    selectInput("animation_easing", lang$adminMode$graphs$animationOptions$easing, choices = langSpecificGraphs$easingChoices),
    checkboxInput_MIRO("animation_redraw", lang$adminMode$graphs$animationOptions$redraw, value = TRUE),
    selectInput("animation_mode", lang$adminMode$graphs$animationOptions$mode, choices = langSpecificGraphs$modeChoices),
    getAnimationSliderOptions()
  )
})
getAnimationSliderOptions  <- reactive({
  tagList(checkboxInput_MIRO("animation_slider_hide", lang$adminMode$graphs$animationSliderOptions$hide), 
          textInput("animation_slider_label", lang$adminMode$graphs$animationSliderOptions$label),
          textInput("animation_slider_prefix", lang$adminMode$graphs$animationSliderOptions$prefix),
          colorPickerInput("animation_slider_font_color", lang$adminMode$graphs$animationSliderOptions$fontColor, "#000000"))
})
getHistOptions <- reactive({
  scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    label <- names(activeSymbol$indices)[match(scalarIndices[1], 
                                               activeSymbol$indices)][1]
    rv$graphConfig$graph$xdata <<- list()
    rv$graphConfig$graph$xdata[[scalarIndices[1]]] <<- list(labels = label, 
                                                            color = "#000000")
    rv$graphConfig$graph$histnorm    <<- ""
    rv$graphConfig$graph$nbins       <<- 2L
    rv$graphConfig$graph$barmode     <<- "overlay"
    rv$graphConfig$graph$alpha       <<- 0.6
    rv$graphConfig$graph$xaxis$title <<- label
    rv$graphConfig$graph$cumulative  <<- FALSE
    rv$graphConfig$graph$horizontal  <<- FALSE
    rv$graphConfig$graph$showlegend  <<- TRUE
    if(length(scalarIndices)){
      idLabelMap$hist_xdata[[1]]       <<- scalarIndices[[1]]
    }else{
      idLabelMap$hist_xdata[[1]]       <<- "1"
    }
  })
  tagList(
    tags$div(class="cat-body cat-body-23",
             createArray(session, "hist_xdata", lang$adminMode$graphs$histOptions$xdata,
                         class_outer="array-wrapper-outer-graph", hr = FALSE)),
    tags$div(class="cat-body cat-body-24", style="display:none;",
             selectInput("hist_norm", lang$adminMode$graphs$histOptions$norm,
                         choices = langSpecificGraphs$normChoices),
             numericInput("hist_nbins", lang$adminMode$graphs$histOptions$nbins,
                          min = 0L, value = 2L),
             selectInput("hist_barmode", lang$adminMode$graphs$histOptions$barmode,
                         choices = langSpecificGraphs$barmodeChoices),
             numericInput("hist_alpha", lang$adminMode$graphs$histOptions$alpha,
                          min = 0L, max = 1L, step = 0.1, value = 0.6),
             checkboxInput_MIRO("hist_cumulative", lang$adminMode$graphs$histOptions$cumulative,
                                value = FALSE),
             selectInput("hist_horizontal", lang$adminMode$graphs$histOptions$horizontal,
                         choices = langSpecificGraphs$orientationChoices),
             getAxisOptions("x", label, labelOnly = TRUE),
             getAxisOptions("y", "", labelOnly = TRUE)),
    tags$div(class="cat-body cat-body-25", style="display:none;",
             getColorPivotOptions(),
             getFilterOptions()),
    tags$div(class="cat-body cat-body-26", style="display:none;",
             getOptionSection()
    )
  )
})
getDygraphsOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  if(!length(scalarIndices)){
    showElReplaceTxt(session, "#preview-error", lang$adminMode$graphs$dygraphsOptions$previewError)
    return()
  }
  isolate({
    rv$graphConfig$graph$xdata <<- unname(indices[1])
    rv$graphConfig$graph$ydata <<- NULL
    rv$graphConfig$graph$ydata[[scalarIndices[1]]] <<- list(label = names(scalarIndices)[[1]], 
                                                            stemPlot = FALSE, stepPlot = FALSE, 
                                                            fillGraph = FALSE, drawPoints = FALSE, 
                                                            pointShape = "dot",
                                                            pointSize = 2L)
    rv$graphConfig$graph$dyEvent <<- NULL
    rv$graphConfig$graph$dyLimit <<- NULL
    rv$graphConfig$graph$dyAnnotation <<- NULL
    rv$graphConfig$graph$dyShading <<- NULL
    rv$graphConfig$graph$dyOptions <<- list(includeZero = FALSE, logscale = FALSE, drawGrid = TRUE,
                                            stepPlot = FALSE, stemPlot = FALSE, fillGraph = FALSE,
                                            fillAlpha = 0.15, drawPoints = FALSE, pointShape = "dot",
                                            pointSize = 2L)
    rv$graphConfig$graph$dyHighlight <<- list(highlightSeriesBackgroundAlpha = 0.5,
                                              hideOnMouseOut = TRUE, 
                                              highlightCircleSize = 3L)
    rv$graphConfig$graph$color <<- NULL
    if(length(scalarIndices)){
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    }else{
      idLabelMap$chart_ydata[[1]]       <<- "1" 
    }
  })
  tagList(
    tags$div(class="cat-body cat-body-27",
             selectInput("chart_xdata", lang$adminMode$graphs$dygraphsOptions$xdata,
                         choices = indices),
             getAxisOptions("x", names(indices)[1], labelOnly = TRUE)),
    tags$div(class="cat-body cat-body-28", style="display:none;",
             createArray(session, "dy_ydata", lang$adminMode$graphs$dygraphsOptions$ydata,
                         class_outer="array-wrapper-outer-graph", hr = FALSE),
             getAxisOptions("y", names(scalarIndices)[1], labelOnly = TRUE)),
    tags$div(class="cat-body cat-body-29", style="display:none;",
             selectInput("chart_color", tags$div(lang$adminMode$graphs$dygraphsOptions$color, tags$a("", class="info-wrapper", href="https://gams.com/miro/charts.html#group-domain", 
                                                                                            tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")),
                         choices = c("_", indices)),
             getFilterOptions()),
    if(length(configScalars) && nrow(configScalars)){
      tagList(
        tags$div(class="cat-body cat-body-30", style="display:none;",
                 createArray(session, "dy_dyEvent", lang$adminMode$graphs$dygraphsOptions$dyEvent, autoCreate = FALSE,
                             class_outer="array-wrapper-outer-graph", hr = FALSE)),
        tags$div(class="cat-body cat-body-31", style="display:none;",
                 createArray(session, "dy_dyLimit", lang$adminMode$graphs$dygraphsOptions$dyLimit, autoCreate = FALSE,
                             class_outer="array-wrapper-outer-graph", hr = FALSE)),
        tags$div(class="cat-body cat-body-32", style="display:none;",
                 createArray(session, "dy_dyAnnotation", lang$adminMode$graphs$dygraphsOptions$dyAnnotation, autoCreate = FALSE,
                             class_outer="array-wrapper-outer-graph", hr = FALSE)),
        tags$div(class="cat-body cat-body-33", style="display:none;",
                 createArray(session, "dy_dyShading", lang$adminMode$graphs$dygraphsOptions$dyShading, autoCreate = FALSE,
                             class_outer="array-wrapper-outer-graph", hr = FALSE))
      )
    },
    tags$div(class="cat-body cat-body-34", style="display:none;",
             title = lang$adminMode$graphs$dygraphsOptions$rngSelOpts$title, collapsed = TRUE,
             checkboxInput_MIRO("dyrange_activate", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$activate),
             numericInput("dyrange_height", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$height, min = 0L, value = 40L),
             checkboxInput_MIRO("dyrange_retainDateWindow", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$retainDateWindow, FALSE),
             checkboxInput_MIRO("dyrange_keepMouseZoom", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$keepMouseZoom, TRUE),
             colorPickerInput("dyrange_fillColor", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$fillColor, "#A7B1C4"),
             colorPickerInput("dyrange_strokeColor", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$strokeColor, "#808FAB")
    ),
    tags$div(class="cat-body cat-body-36", style="display:none;",
                  textInput("chart_title", lang$adminMode$graphs$ui$chartTitle),
                  checkboxInput_MIRO("dyopt_incZero", lang$adminMode$graphs$dygraphsOptions$generalOpts$incZero),
                  checkboxInput_MIRO("dyopt_logscale", lang$adminMode$graphs$dygraphsOptions$generalOpts$logscale),
                  checkboxInput_MIRO("dyopt_drawGrid", lang$adminMode$graphs$dygraphsOptions$generalOpts$drawGrid, TRUE),
                  checkboxInput_MIRO("dyopt_stepPlot", lang$adminMode$graphs$dygraphsOptions$generalOpts$stepPlot),
                  checkboxInput_MIRO("dyopt_stemPlot", lang$adminMode$graphs$dygraphsOptions$generalOpts$stemPlot),
                  checkboxInput_MIRO("dyopt_fillGraph", lang$adminMode$graphs$dygraphsOptions$generalOpts$fillGraph),
                  numericInput("dyopt_fillAlpha", lang$adminMode$graphs$dygraphsOptions$generalOpts$fillAlpha, min = 0L, max = 1L, value = 0.15),
                  checkboxInput_MIRO("dyopt_drawPoints", lang$adminMode$graphs$dygraphsOptions$generalOpts$drawPoints),
                  selectInput("dyopt_pointShape", lang$adminMode$graphs$dygraphsOptions$generalOpts$pointShape, 
                              choices = langSpecificGraphs$pointShapeChoices),
                  numericInput("dyopt_pointSize", lang$adminMode$graphs$dygraphsOptions$generalOpts$pointSize, min = 0L, value = 2L),
                  getOuttype()
    ),
    tags$div(class="cat-body cat-body-35", style="display:none;",
             checkboxInput_MIRO("dyhighlight_activate", lang$adminMode$graphs$dygraphsOptions$highOpts$activate, value = TRUE),
             numericInput("dyhigh_circleSize", lang$adminMode$graphs$dygraphsOptions$highOpts$circleSize, min = 0L, value = 3L),
             sliderInput("dyhigh_seriesBackgroundAlpha", lang$adminMode$graphs$dygraphsOptions$highOpts$seriesBackgroundAlpha, 
                         min = 0L, max = 1L, step = 0.1, value = 0.5),
             checkboxInput_MIRO("dyhigh_hideOnMouseOut", lang$adminMode$graphs$dygraphsOptions$highOpts$hideOnMouseOut, TRUE)
    )
  )
})
getLeafletOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
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
      tags$div(class="cat-body cat-body-37",
               createArray(session, "leaflet_markers", lang$adminMode$graphs$leafletOptions$markers, autoCreate = FALSE,
                           class_outer="array-wrapper-outer-graph", hr = FALSE)),
      tags$div(class="cat-body cat-body-38", style="display:none;",
               createArray(session, "leaflet_flows", lang$adminMode$graphs$leafletOptions$flows, autoCreate = FALSE,
                           class_outer="array-wrapper-outer-graph", hr = FALSE)),
      tags$div(class="cat-body cat-body-39", style="display:none;",
               createArray(session, "leaflet_minicharts", lang$adminMode$graphs$leafletOptions$minicharts, autoCreate = FALSE,
                           class_outer="array-wrapper-outer-graph", hr = FALSE)),
      tags$div(class="cat-body cat-body-40", style="display:none;",
               selectInput("leaflet_hideGroups", lang$adminMode$graphs$leafletOptions$hideGroups, choices = c(),
                           multiple = TRUE),
               selectInput("leaflc_baseGroups", lang$adminMode$graphs$leafletOptions$layer$baseGroups, choices = c(),
                           multiple = TRUE),
               selectInput("leaflc_overlayGroups", lang$adminMode$graphs$leafletOptions$layer$overlayGroups, 
                           choices = c(), multiple = TRUE),
               selectInput("leaflc_position", lang$adminMode$graphs$leafletOptions$layer$position, 
                           choices = langSpecificGraphs$positionChoices),
               checkboxInput_MIRO("leaflc_collapsed", lang$adminMode$graphs$leafletOptions$layer$collapsed, value = TRUE)
      ),
      tags$div(class="cat-body cat-body-41", style="display:none;",
               getFilterOptions()),
      tags$div(class="cat-body cat-body-42", style="display:none;",
               getOuttype())
    )
  )
})
getTimevisOptions<- reactive({
  rv$initData
  rv$refreshContent
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$showZoom <<- TRUE
    rv$graphConfig$graph$fit <<- TRUE
    rv$graphConfig$graph$zoomFactor <<- 0.5
    rv$graphConfig$graph$series <- list()
    rv$graphConfig$graph$editable <- FALSE
    rv$graphConfig$graph$multiselect <- FALSE
    rv$graphConfig$graph$showCurrentTime <- FALSE
    rv$graphConfig$graph$series[["1"]] <<- list(content = indices[[1]], 
                                                start = indices[[1]],
                                                type = "box")
  })
  tagList(
    tagList(
      tags$div(class="cat-body cat-body-43",
               createArray(session, "timevis_series", lang$adminMode$graphs$timevisOptions$series,
                           class_outer="array-wrapper-outer-graph", hr = FALSE),
               createArray(session, "timevis_custom", lang$adminMode$graphs$timevisOptions$custom, autoCreate = FALSE,
                           class_outer="array-wrapper-outer-graph", hr = FALSE)),
      tags$div(class="cat-body cat-body-44", style="display:none;",
               getFilterOptions()),
      tags$div(class="cat-body cat-body-45", style="display:none;",
               checkboxInput_MIRO("timevis_showZoom", lang$adminMode$graphs$timevisOptions$options$showZoom, TRUE),
               numericInput("timevis_zoomFactor", lang$adminMode$graphs$timevisOptions$options$zoomFactor, min = 0, max = 1, value = 0.5, step = 0.1),
               checkboxInput_MIRO("timevis_fit", lang$adminMode$graphs$timevisOptions$options$fit, TRUE),
               checkboxInput_MIRO("timevis_editable", lang$adminMode$graphs$timevisOptions$options$editable),
               checkboxInput_MIRO("timevis_multiselect", lang$adminMode$graphs$timevisOptions$options$multiselect),
               checkboxInput_MIRO("timevis_showCurrentTime", lang$adminMode$graphs$timevisOptions$options$showCurrentTime),
               getOuttype())
    )
  )
})
getPivotOptions <- reactive({
  #TODO: refresh rv's when switching back to pivot as graphing tool (currently the defauts are not loaded then)
  rv$initData
  rv$refreshContent
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$pivottable <<- NULL
    rv$graphConfig$pivottable$rows <<- NULL
    rv$graphConfig$pivottable$cols <<- NULL
    rv$graphConfig$pivottable$aggregatorName <<- "Count"
    rv$graphConfig$pivottable$vals <<- "_"
    rv$graphConfig$pivottable$rendererName <<- "Table"
    rv$graphConfig$pivottable$locale <<- "en"
    rv$graphConfig$pivottable$subtotals <<- FALSE
  })
  tagList(
    tags$div(class="cat-body cat-body-46",
             selectInput("pivot_rows", lang$adminMode$graphs$pivotOptions$rows, choices = indices, multiple = TRUE, selected = NULL),
             selectInput("pivot_cols", lang$adminMode$graphs$pivotOptions$cols, choices = indices, multiple = TRUE, selected = NULL),
             selectInput("pivot_aggregatorName", lang$adminMode$graphs$pivotOptions$aggregator, choices = langSpecificGraphs$aggregatorChoices,
                         selected = "Sum"),
             tags$div(class = "shiny-input-container",
                      style = "max-height:800px;max-height: 80vh;padding-right:30px;padding-left:40px;",
                      conditionalPanel(condition = "input.pivot_aggregatorName && !input.pivot_aggregatorName.startsWith('Count')",
                                       selectInput("pivot_vals", lang$adminMode$graphs$pivotOptions$vals, choices = c("_", indices),
                                                   selected = if(length(scalarIndices)) scalarIndices[[1]])
                      ),  
                      conditionalPanel(condition = "input.pivot_aggregatorName === 'Sum over Sum' || input.pivot_aggregatorName === '80% Upper Bound' || 
                     input.pivot_aggregatorName === '80% Lower Bound'",
                                       selectInput("pivot_vals2", lang$adminMode$graphs$pivotOptions$vals, choices = c("_", indices))
                      )),  
             selectInput("pivot_rendererName", lang$adminMode$graphs$pivotOptions$renderer, choices = langSpecificGraphs$rendererChoices, selected = "Table")),
    tags$div(class="cat-body cat-body-47", style="display:none;",
             selectInput("pivot_locale", lang$adminMode$graphs$pivotOptions$options$locale, choices = langSpecificGraphs$localeChoices, 
                         selected = "en"),
             checkboxInput_MIRO("pivot_subtotals", span(lang$adminMode$graphs$pivotOptions$options$subtotals, tags$a(href="http://nagarajanchinnasamy.com/subtotal/", target = "_blank", "http://nagarajanchinnasamy.com/subtotal/")), 
                                value = FALSE)
    )
    
  )
}) 
getCustomOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices       <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$packages <- NULL
    rv$graphConfig$name <- NULL
  })
  tagList(
    tags$div(class="cat-body cat-body-48",
             textInput("custom_name", lang$adminMode$graphs$customOptions$name),
             selectizeInput("custom_packages", tags$div(lang$adminMode$graphs$customOptions$packages, 
                                                        tags$a("", class="info-wrapper", href="https://gams.com/miro/customize.html#custom-renderers", 
                                                               tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")),
                            choices = c(), 
                            multiple = TRUE, options = list('create' = TRUE,'persist' = FALSE))
    )
    
  )
}) 
getColorPivotOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices    <- activeSymbol$indices
  tagList(
  selectInput("chart_color", tags$div(lang$adminMode$graphs$chartOptions$color, 
              tags$a("", class="info-wrapper", href="https://gams.com/miro/charts.html#group-domain", 
                     tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")),
              choices = c("_", indices)))
})
getFilterOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices    <- activeSymbol$indices
  setIndices <- indices[activeSymbol$indexTypes == "string"]
  isolate({
    rv$graphConfig$graph$filter <- NULL
  })
  tagList(
    tags$label(class = "cb-label info-position", "for" = "filter_dim", 
               tags$div(lang$adminMode$graphs$filterOptions$filter, tags$a("", class="info-wrapper", href="https://gams.com/miro/charts.html#filter-option", 
                                                                                                        tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
    tags$div(
      tags$label(class = "checkbox-material", 
                 checkboxInput("filter_dim", value = FALSE, label = NULL)
      )),
    tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;padding-left:40px;",
             conditionalPanel(
               condition = "input.filter_dim == true",
               selectInput("filter_col", lang$adminMode$graphs$filterOptions$dimension,
                           choices = setIndices),
               textInput("filter_label", lang$adminMode$graphs$filterOptions$label, placeholder = lang$adminMode$graphs$filterOptions$placeholder),
               checkboxInput_MIRO("filter_multiple", lang$adminMode$graphs$filterOptions$multiple, 
                                  value = FALSE),
               checkboxInput_MIRO("filter_date", lang$adminMode$graphs$filterOptions$date, 
                                  value = FALSE)
             ))
  )
}) 
observe({
  req(rv$graphConfig$graph$tool, activeSymbol$id > 0L, allDataAvailable)
  if(identical(rv$graphConfig$graph$tool, "plotly") && identical(length(rv$graphConfig$graph$type), 0L))
    return()
  if(activeSymbol$id > length(modelIn)){
    data <- modelOutputData[[activeSymbol$id - length(modelIn)]]
  }else{
    data <- modelInputData[[activeSymbol$id]]
  }
  tryCatch({
    if(isolate(rv$graphConfig$graph$tool) == "plotly"){
      if(identical(rv$graphConfig$graph$type, "pie") && 
         nrow(data) > 100){
          showEl(session, "#pieValues")
          hideEl(session, "#preview-content-plotly")
      }else{
        callModule(renderData, "preview_output_plotly", type = "graph", 
                   data = data, configData = configScalars, 
                   graphOptions = rv$graphConfig$graph,
                   roundPrecision = roundPrecision, modelDir = modelDir)
        showEl(session, "#preview-content-plotly")
        hideEl(session, "#pieValues")
      }
      hideEl(session, "#preview-content-dygraphs")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-valuebox")
      hideEl(session, "#preview-content-custom")
    }else if(isolate(rv$graphConfig$graph$tool) == "dygraphs"){
      callModule(renderData, "preview_output_dygraphs", type = "graph", 
                 data = data, configData = configScalars, 
                 graphOptions = rv$graphConfig$graph,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      showEl(session, "#preview-content-dygraphs")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#pieValues")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-valuebox")
      hideEl(session, "#preview-content-custom")
    }else if(isolate(rv$graphConfig$graph$tool) == "pivot"){
      pivotOptions <- rv$graphConfig$pivottable
      callModule(renderData, "preview_output_pivot", type = "pivot", 
                 data = data, configData = configScalars, 
                 pivotOptions = pivotOptions,
                 roundPrecision = 2, modelDir = modelDir)
      showEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-dygraphs")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#pieValues")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-valuebox")
      hideEl(session, "#preview-content-custom")
    }else if(isolate(rv$graphConfig$graph$tool) == "timevis"){
      callModule(renderData, "preview_output_timevis", type = "graph", 
                 data = data, configData = configScalars, 
                 graphOptions = rv$graphConfig$graph,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      showEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#pieValues")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-dygraphs")
      hideEl(session, "#preview-content-valuebox")
      hideEl(session, "#preview-content-custom")
    }else if(isolate(rv$graphConfig$graph$tool) == "valuebox"){
      customOptionstmp <- as.vector(rv$graphConfig$options, mode = "list")
      customOptionstmp$count <- configGraphsOut[[i]]$options$count
      callModule(renderData, "preview_output_valuebox", type = "valuebox", 
                 data = data, configData = configScalars, 
                 customOptions = customOptionstmp,
                 modelDir = modelDir)
      showEl(session, "#preview-content-valuebox")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-dygraphs")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#pieValues")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-custom")
    }else if(isolate(rv$graphConfig$graph$tool) == "custom"){
      nameTmp = rv$graphConfig$outType
      customR <- paste0(nameTmp, "Output <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
 
    # set default height
    if(is.null(height)){
      height <- 700
    } 
    tagList( 
      #define rendererOutput function here 
    ) 
}
                            
",
"render", toupper(substr(nameTmp, 1, 1)), substr(nameTmp, 2, nchar(nameTmp)), " <- function(input, output, session, data, options = NULL, path = NULL){ 
    #renderer 

}")
      output[["preview_output_custom"]] <- renderText(customR)
      showEl(session, "#preview-content-custom")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-dygraphs")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#pieValues")
      hideEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-valuebox")
    }else{
      callModule(renderData, "preview_output_leaflet", type = "graph", 
                 data = data, configData = configScalars, 
                 graphOptions = rv$graphConfig$graph,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      showEl(session, "#preview-content-leaflet")
      hideEl(session, "#preview-content-plotly")
      hideEl(session, "#pieValues")
      hideEl(session, "#preview-content-dygraphs")
      hideEl(session, "#preview-content-pivot")
      hideEl(session, "#preview-content-timevis")
      hideEl(session, "#preview-content-valuebox")
      hideEl(session, "#preview-content-custom")
    }
    hideEl(session, "#preview-error")
  }, error = function(e) {
    hideEl(session, "#preview-content-dygraphs")
    hideEl(session, "#preview-content-plotly")
    hideEl(session, "#pieValues")
    hideEl(session, "#preview-content-leaflet")
    hideEl(session, "#preview-content-pivot")
    hideEl(session, "#preview-content-timevis")
    hideEl(session, "#preview-content-valuebox")
    hideEl(session, "#preview-content-custom")
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
    showModal(modalDialog(title = lang$adminMode$graphs$saveJson$warnTitle, sprintf(lang$adminMode$graphs$saveJson$warnContent, activeSymbol$name), 
                          footer = tagList(modalButton(lang$adminMode$graphs$saveJson$cancel), 
                                           actionButton("saveGraphConfirm", lang$adminMode$graphs$saveJson$overwrite))))
    return()
  }
  rv$saveGraphConfirm <- rv$saveGraphConfirm + 1L
})
observeEvent(input$saveGraphConfirm, rv$saveGraphConfirm <- rv$saveGraphConfirm + 1L)
observeEvent(rv$saveGraphConfirm, {
  req(rv$saveGraphConfirm > 0L)
  configJSON$dataRendering[[activeSymbol$name]] <<- rv$graphConfig
  configJSON$dataRendering[[activeSymbol$name]]$height <<- 700
  if(rv$graphConfig$graph$tool == "pivot"){
    configJSON$dataRendering[[activeSymbol$name]]$graph <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$options <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$outType <<- "pivot"
  }else if(rv$graphConfig$graph$tool == "valuebox"){
    configJSON$dataRendering[[activeSymbol$name]]$graph <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$height <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$pivottable <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$outType <<- "valueBox"
  }else if(rv$graphConfig$graph$tool == "custom"){
    configJSON$dataRendering[[activeSymbol$name]]$graph <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$height <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$options <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$pivottable <<- NULL
  }else{
    configJSON$dataRendering[[activeSymbol$name]]$pivottable <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$options <<- NULL
    if(!identical(rv$graphConfig$graph$tool, "leaflet")){
      configJSON$dataRendering[[activeSymbol$name]]$graph$layersControl <<- NULL
    }
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  removeModal()
  showHideEl(session, "#graphUpdateSuccess", 4000L)
  showEl(session, "#deleteGraph")
})

observeEvent(input$deleteGraph, {
  req(nchar(activeSymbol$name) > 0L)
  
  if(!tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))){
    return()
  }
  showModal(modalDialog(title = lang$adminMode$graphs$removeDialog$title, lang$adminMode$graphs$removeDialog$message, 
                        footer = tagList(modalButton(lang$adminMode$graphs$removeDialog$cancel), 
                                         actionButton("deleteGraphConfirm", lang$adminMode$graphs$removeDialog$confirm))))
})
observeEvent(input$deleteGraphConfirm, {
  graphId <- match(tolower(activeSymbol$name), tolower(names(configJSON$dataRendering)))
  if(is.na(graphId)){
    return()
  }
  configJSON$dataRendering[[graphId]] <<- NULL
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  removeModal()
  showHideEl(session, "#graphUpdateSuccess", 4000L)
  hideEl(session, "#deleteGraph")
})