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
  tags$div(class = "shiny-input-container", onclick = "$(this).children('.option-section').toggle();",
           tags$h4(class = "box-title option-section-header", title, icon("plus"), style = "cursor:pointer", 
                   onclick = "$(this)next().toggle();"),
           tags$div(class = "option-section", ..., style = if(collapsed) "display:none;" else "")
           )
}

server_admin <- function(input, output, session){
  rv <- reactiveValues(plotly_type = 0L, saveJSONConfirm = 0L, resetRE = 0L,
                       graphConfig = list(outType = "graph", graph = list()), initData = FALSE)
  activeSymbol <- list(id = integer(1L), name = character(1L), 
                       alias = character(1L), indices = c())
  configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName, 
                                                    simplifyDataFrame = FALSE, 
                                                    simplifyMatrix = FALSE))
  # ------------------------------------------------------
  #     CUSTOMIZE GRAPHS
  # ------------------------------------------------------
  modelInputData   <- vector("list", length(modelIn))
  modelOutputData  <- vector("list", length(modelOut))
  hiddenOutputData <- tibble()
  hotInit          <- vector("logical", length(modelIn))
  isEmptyInput     <- vector(mode = "logical", length = length(modelIn))
  isEmptyOutput    <- vector(mode = "logical", length = length(modelOut))
  isEmptyOutput[]  <- TRUE
  inputInitialized <- vector(mode = "logical", length = length(modelInWithDep))
  currentSelection <- list("plotly", "pie")
  idLabelMap       <- list(chart_ydata = list(), hist_data = list())
  currentConfig    <- list()
  optionsInserted  <- c()
  noOutputData     <- TRUE
  allDataAvailable <- FALSE
  
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
    if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
      return(NULL)
    }
    # extract only sheets which are also in list of input parameters
    datasetsToFetch <- xlsWbNames[tolower(xlsWbNames) %in% modelInTabularData]
    # load input data 
    loadMode <- "xls"
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
                                         modelOutTemplate = modelOutTemplate, method = "xlsx", 
                                         hiddenOutputScalars = config$hiddenOutputScalars,
                                         xlsxName = basename(isolate(input$localInput$datapath)))
      }, error = function(e){
        flog.error("Problems loading output data. Error message: %s.", e)
        errMsg <<- lang$errMsg$readOutput$desc
      })
      if(is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))){
        return()
      }
      if(!is.null(outputDataTmp$scalar)){
        hiddenOutputData <<- outputDataTmp$scalar
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
      changeActiveSymbol(which(!isEmptyOutput)[1])
    }else{
      showErrorMsg("No valid data", "No valid datasets were found. Please upload a properly formatted Excel file with data.")
      return()
    }
    showEl(session, "#preview_wrapper")
    updateSelectInput(session, "gams_symbols", choices = c(tabularInputWithData, tabularOutputWithData))
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
      insertUI(selector = "#plotly_options",
               getPieOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }else if(identical(isolate(input$plotly_type), "chart")){
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "bar"
      insertUI(selector = "#plotly_options",
               tagList(
                 selectInput("plotly_chart_type", "Select a chart type", 
                             setNames(c("bar", "scatter", "line"), 
                                      c("Bar chart", "Scatter plot", "Line chart"))),
                 tags$div(id = "plotly_chart_options", class = "shiny-input-container",
                          getBarOptions())
               ), where = "beforeEnd")
    }else if(identical(isolate(input$plotly_type), "hist")){
      rv$graphConfig$graph$type <<- "hist"
      insertUI(selector = "#plotly_options", getHistOptions(), where = "beforeEnd")
      allDataAvailable <<- TRUE
    }
  })
  observeEvent(input$plotly_chart_type, {
    saveAndReload("plotly", "chart", input$plotly_chart_type)
    removeUI(selector = "#plotly_chart_options .shiny-input-container", multiple = TRUE)
    rv$resetRE <- rv$resetRE + 1L
    allDataAvailable <<- FALSE
    
    if(identical(input$plotly_chart_type, "bar")){
      rv$graphConfig$graph$type <<- "bar"
      insertUI(selector = "#plotly_chart_options",
               getBarOptions(), where = "beforeEnd")
    }else if(identical(input$plotly_chart_type, "scatter")){
      rv$graphConfig$graph$type <<- "scatter"
      insertUI(selector = "#plotly_chart_options",
               getScatterOptions(), where = "beforeEnd")
    }else{
      rv$graphConfig$graph$type <<- "scatter"
      insertUI(selector = "#plotly_chart_options",
               getLineOptions(), where = "beforeEnd")
    }
    allDataAvailable <<- TRUE
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
  })
  observeEvent(input$marker_color, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_color[1])]]]]$marker$color <<- input$marker_color[2]
  })
  observeEvent(input$marker_opacity, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_opacity[1])]]]]$marker$opacity <<- input$marker_opacity[2]
  })
  observeEvent(input$marker_size, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_size[1])]]]]$marker$size <<- input$marker_size[2]
  })
  observeEvent(input$marker_line_width, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_width[1])]]]]$marker$line$width <<- input$marker_line_width[2]
  })
  observeEvent(input$marker_line_color, {
    if(nchar(input$marker_line_color[2]))
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_color[1])]]]]$marker$line$color <<- input$marker_line_color[2]
    else
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_color[1])]]]]$marker$line$color <<- NULL
  })
  observeEvent(input$line_color, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_color[1])]]]]$line$color <<- input$line_color[2]
  })
  observeEvent(input$line_width, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_width[1])]]]]$line$width <<- input$line_width[2]
  })
  observeEvent(input$line_shape, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_shape[1])]]]]$line$shape <<- input$line_shape[2]
  })
  observeEvent(input$line_dash, {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_dash[1])]]]]$line$dash <<- input$line_dash[2]
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
    if(identical(input$dyser_color[2], '')){
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyser_color[1])]]]]$color <<- NULL
    }else{
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyser_color[1])]]]]$color <<- input$dyser_color[2]
    }
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
  observeEvent(input$hist_label, {
    rv$graphConfig$graph$xdata[[idLabelMap$hist_data[[as.integer(input$hist_label[1])]]]]$labels <<- input$hist_label[2]
  })
  observeEvent(input$hist_color, {
    rv$graphConfig$graph$xdata[[idLabelMap$hist_data[[as.integer(input$hist_color[1])]]]]$color <<- input$hist_color[2]
  })
  observeEvent(input$hist_alpha, {
    rv$graphConfig$graph$xdata[[idLabelMap$hist_data[[as.integer(input$hist_alpha[1])]]]]$alpha <<- input$hist_alpha[2]
  })
  observeEvent(input$chart_color, {
    if(identical(input$chart_color, "_"))
      rv$graphConfig$graph$color <<- NULL
    else
      rv$graphConfig$graph$color <<- input$chart_color
  })
  observeEvent(input$chart_xdata, {
    if(!(length(input$x_title) && nchar(input$x_title))){
      updateTextInput(session, "x_title", value = names(activeSymbol$indices)[match(input$chart_xdata, 
                                                                              activeSymbol$indices)][1])
    }
    rv$graphConfig$graph$xdata <<- input$chart_xdata
  })
  observeEvent(input$hist_data, {
    hist_id <- as.integer(input$hist_data[1])
    if(is.na(hist_id))
      return()
      
    if(hist_id <= length(idLabelMap$hist_data)){
      labelID <- idLabelMap$hist_data[[as.integer(hist_id)]]
      if(sum(labelID == idLabelMap$hist_data) < 1.5){
        rv$graphConfig$graph$xdata[labelID] <<- NULL
      }
    }
    idLabelMap$hist_data[[hist_id]] <<- input$hist_data[2]
    label <- names(activeSymbol$indices)[match(input$hist_data[2], 
                                               activeSymbol$indices)][1]
    if(identical(hist_id, 1L) && !(length(input$x_title) && nchar(input$x_title))){
      updateTextInput(session, "x_title", value = label)
    }
    rv$graphConfig$graph$xdata[[input$hist_data[2]]] <<- list(labels = label)
  })
  observeEvent(input$chart_ydata, {
    chart_ydata <- input$chart_ydata[2]
    chart_id    <- as.integer(input$chart_ydata[1])
    if(is.na(chart_id))
      return()
    
    if(chart_id <= length(idLabelMap$chart_ydata)){
      labelID <- idLabelMap$chart_ydata[[chart_id]]
      if(sum(labelID == idLabelMap$chart_ydata) < 1.5){
        rv$graphConfig$graph$ydata[labelID] <<- NULL
      }
    }
    
    idLabelMap$chart_ydata[[chart_id]] <<- chart_ydata
    label       <- names(activeSymbol$indices)[match(chart_ydata, activeSymbol$indices)][1]
    if(chart_id == 1 && !(length(input$y_title) && nchar(input$y_title))){
      updateTextInput(session, "y_title", value = label)
    }
    rv$graphConfig$graph$ydata[[chart_ydata]] <<- list(label = label, 
                                                          mode = if(identical(input$plotly_chart_type, "scatter"))
                                                            "markers" else "lines")
  })
  observeEvent(input$chart_ylabel, {
    tryCatch({
      labelID <- idLabelMap$chart_ydata[[as.integer(input$chart_ylabel[1])]]
      rv$graphConfig$graph$ydata[[labelID]]$label <<- input$chart_ylabel[2]
    }, error = function(e){
      flog.info("Could not change label for y-data. Error message: '%s'.", e)
    })
  })
  observeEvent(input$remove_array_el, {
    if(startsWith(input$remove_array_el[1], "chart_ydata")){
      if(sum(input$remove_array_el[2] == idLabelMap$chart_ydata) < 1.5){
        rv$graphConfig$graph$ydata[input$remove_array_el[2]] <- NULL
      }
      idLabelMap$chart_ydata[as.integer(input$remove_array_el[3])] <<- NULL
    }else if(identical(input$remove_array_el[1], "hist_data")){
      if(sum(input$remove_array_el[2] == idLabelMap$hist_data) < 1.5){
        rv$graphConfig$graph$xdata[input$remove_array_el[2]] <- NULL
      }
      idLabelMap$chart_ydata[as.integer(input$remove_array_el[3])] <<- NULL
    }
  })
  observeEvent(input$bar_mode, {
    rv$graphConfig$graph$barmode <<- input$bar_mode
  })
  observeEvent(input$gams_symbols, {
    req(input$gams_symbols)
    changeActiveSymbol(match(isolate(input$gams_symbols), names(modelIn)))
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
                                      choices = setNames(c("pie", "chart", "hist"), 
                                                         c("Pie chart", "Chart", "Histogram"))),
                          tags$div(id = "plotly_options", getPieOptions())
                 ), where = "beforeEnd")
      }else if(identical(input$chart_tool, "dygraphs")){
        currentSelection$noLayers <<- 1L
        insertUI(selector = "#tool_options",
                 tags$div(id = "dygraph_options", getDygraphsOptions()), where = "beforeEnd")
        allDataAvailable <<- TRUE
      }
    })
  getPieOptions <- reactive({
    req(rv$initData)
    indices       <- activeSymbol$indices
    scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
    isolate({
      rv$graphConfig$graph$labels <<- indices[[1]]
      rv$graphConfig$graph$values <<- scalarIndices[[1]]
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
      checkboxInput(id %+% "_showticklabels", sprintf("Show tick labels(%s axis)?", id), TRUE)
    )
  }
  getChartOptions <- reactive({
    req(rv$resetRE > 0L)
    indices       <- activeSymbol$indices
    scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
    isolate({
      rv$graphConfig$graph$xdata  <<- indices[[1]]
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    })
    tagList(
      selectInput("chart_xdata", "What should be plotted on the x axis?",
                  choices = indices),
      addArrayEl(session, "chart_ydata", isolate(input$plotly_chart_type)),
      selectInput("chart_color", "Symbol that is used to select different colors",
                  choices = c("_", indices)),
      getAxisOptions("x", names(indices)[1]),
      getAxisOptions("y", names(scalarIndices)[1])
    )
  })
  getBarOptions  <- reactive({
    indices       <- activeSymbol$indices
    scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
    isolate({
      rv$graphConfig$graph$barmode <<- "group"
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1])
    })
    tagList(selectInput("bar_mode", "Select barmode", choices = c("group", "stack")),
            getChartOptions())
  })
  getScatterOptions  <- reactive({
    indices       <- activeSymbol$indices
    scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
    isolate({
      rv$graphConfig$graph$ydata <- list()
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1], 
                                                                mode = "markers",
                                                                markers = list(
                                                                  symbol = "circle",
                                                                  color = "black",
                                                                  opacity = 1L,
                                                                  size = 6L,
                                                                  line = list(width = 0L)
                                                                ))
    })
    getChartOptions()
  })
  getLineOptions  <- reactive({
    indices       <- activeSymbol$indices
    scalarIndices <- indices[activeSymbol$indexTypes == "parameter"]
    isolate({
      rv$graphConfig$graph$ydata <- list()
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(label = names(scalarIndices)[1], 
                                                                mode = "lines",
                                                                line = list(color = "black",
                                                                            width = 2L,
                                                                            shape = "linear",
                                                                            dash = "dash"))
    })
    getChartOptions()
  })
  getHistOptions <- reactive({
    scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "parameter"]
    isolate({
      label <- names(activeSymbol$indices)[match(scalarIndices[1], 
                                                 activeSymbol$indices)][1]
      rv$graphConfig$graph$xdata[[scalarIndices[1]]] <<- list(labels = label, 
                                                              color = "black", 
                                                              alpha = 1L)
      rv$graphConfig$graph$histnorm   <<- ""
      rv$graphConfig$graph$nbins      <<- 2L
      rv$graphConfig$graph$barmode    <<- "overlay"
      rv$graphConfig$graph$xaxis$title <<- label
      idLabelMap$hist_data[[1]]       <<- scalarIndices[[1]]
    })
    tagList(
      addArrayEl(session, "hist_data"),
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
      output[["preview-errmsg"]] <- renderText("No scalar indices found in your data.")
      showEl(session, "#preview-error")
      return()
    }
    isolate({
      rv$graphConfig$graph$xdata <<- unname(indices[1])
      rv$graphConfig$graph$ydata[[scalarIndices[1]]] <<- list(label = unname(scalarIndices[1]), 
                                                              mode = if(identical(input$plotly_chart_type, "scatter"))
                                                                "markers" else "lines", 
                                                              stemPlot = FALSE, stepPlot = FALSE, 
                                                              fillGraph = FALSE, drawPoints = FALSE, pointShape = "dot",
                                                              pointSize = 2L)
      rv$graphConfig$graph$dyOptions$includeZero <<- FALSE
      rv$graphConfig$graph$dyOptions$logscale    <<- FALSE
      rv$graphConfig$graph$dyOptions$drawGrid    <<- FALSE
      rv$graphConfig$graph$dyOptions$stepPlot    <<- FALSE
      rv$graphConfig$graph$dyOptions$stemPlot    <<- FALSE
      rv$graphConfig$graph$dyOptions$fillGraph   <<- FALSE
      rv$graphConfig$graph$dyOptions$fillAlpha   <<- 0.15
      rv$graphConfig$graph$dyOptions$drawPoints  <<- FALSE
      rv$graphConfig$graph$dyOptions$pointShape  <<- "dot"
      rv$graphConfig$graph$dyOptions$pointSize   <<- 2L
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    })
    tagList(
      selectInput("chart_xdata", "What index do you want to plot on the x-axis?",
                  choices = indices),
      addArrayEl(session, "dy_ydata"),
      selectInput("chart_color", "What symbol do you want to use to plot different chart lines?",
                  choices = c("_", indices)),
      optionSection(title = "Options", collapsed = TRUE,
        checkboxInput("dyopt_fillGraph", "Should the area underneath the graph be filled?"),
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
      getAxisOptions("x", names(indices)[1], labelOnly = TRUE),
      getAxisOptions("y", names(scalarIndices)[1], labelOnly = TRUE)
    )
  })
  observe({
    req(rv$graphConfig$graph$tool, activeSymbol$id > 0L, allDataAvailable)
    if(identical(rv$graphConfig$graph$tool, "plotly") && identical(length(rv$graphConfig$graph$type), 0L))
      return()
    print("+++++++++++++++++++++++++++++++++++++++")
    print(rv$graphConfig$graph)
    if(activeSymbol$id > length(modelIn)){
      data <- modelOutputData[[activeSymbol$id]]
    }else{
      data <- modelInputData[[activeSymbol$id]]
    }
    tryCatch({
      if(isolate(rv$graphConfig$graph$tool) == "plotly"){
        callModule(renderData, "preview_output_plotly", type = rv$graphConfig$outType, 
                   data = data, configData = hiddenOutputData, 
                   graphOptions = rv$graphConfig$graph,
                   roundPrecision = roundPrecision, modelDir = modelDir)
        showEl(session, "#preview-content-plotly")
        hideEl(session, "#preview-content-dygraph")
      }else{
        callModule(renderData, "preview_output_dygraph", type = rv$graphConfig$outType, 
                   data = data, configData = hiddenOutputData, 
                   graphOptions = rv$graphConfig$graph,
                   roundPrecision = roundPrecision, modelDir = modelDir)
        showEl(session, "#preview-content-dygraph")
        hideEl(session, "#preview-content-plotly")
      }
      hideEl(session, "#preview-error")
    }, error = function(e) {
      hideEl(session, "#preview-content-dygraph")
      hideEl(session, "#preview-content-plotly")
      showEl(session, "#preview-error")
      output[["preview-errmsg"]] <- renderText(toString(e))
    })
  }, priority = -1000)
  observeEvent(input$saveJSON, {
    req(nchar(activeSymbol$name) > 0L)
    if(tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))){
      showModal(modalDialog(title = "Data exists", sprintf("A graph configuration already exists for symbol: '%s'. Do you want to overwrite this configuration? This cannot be undone!", activeSymbol$name), 
                            footer = tagList(modalButton("Cancel"), 
                                             actionButton("saveJSONConfirm", "Overwrite"))))
      return()
    }
    rv$saveJSONConfirm <- rv$saveJSONConfirm + 1L
  })
  observeEvent(virtualActionButton(input$saveJSONConfirm, rv$saveJSONConfirm), {
    configJSON$dataRendering[[activeSymbol$name]] <- rv$graphConfig
    write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
    removeModal()
  })
  
  # ------------------------------------------------------
  #     DB MANAGEMENT
  # ------------------------------------------------------
  observeEvent(input$removeDbTables, {
    showModal(modalDialog(paste0(
      "Please confirm that you want to remove all database tables that belong to the model: ",
      modelName, " by typing \"confirm\" in the text field below."),
      textInput("removeDbConfirmTxt", NULL),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("removeDbTablesConfirm", "Confirm", class = "bt-highlight-1 bt-gms-confirm")
      ),
      title = "Remove database tables"))
  })
  observeEvent(input$removeDbTablesConfirm, {
    if(!identical(input$removeDbConfirmTxt, "confirm")){
      return()
    }
    disableEl(session, "#removeDbTablesConfirm")
    tryCatch({
      db$removeTablesModel()
    }, error = function(e){
      flog.error("Unexpected error: '%s'. Please contact GAMS if this error persists.", e)
      showHideEl(session, "#unknownError", 6000L)
    })
    removeModal()
    showHideEl(session, "#removeSuccess", 3000L)
  })
  output$dbSaveAll <- downloadHandler(
    filename = function() {
      paste0("db_save_", tolower(modelName), ".zip")
    },
    content = function(file) {
      tryCatch({
        prog <- Progress$new()
        on.exit(prog$close(), add = TRUE)
        prog$set(message = "Database is being saved...", value = 0.2)
        tempDir <- file.path(tempdir(), "db_save")
        dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
        wd      <- getwd()
        setwd(tempDir)
        on.exit(setwd(wd), add = TRUE)
        on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)
        db$saveTablesModel(tempDir)
        prog$inc(amount = 0.8, detail = "Compressing files...")
        zip(file, list.files(recursive = FALSE), compression_level = 9)
      }, error = function(e){
        switch(conditionMessage(e),
               'maxRowException' = {
                 flog.info("Maximum number of rows to export were exceeded. You need to backup your database manually.")
                 showHideEl(session, "#maxRowError", 6000L)
               },
               {
                 flog.error("Unexpected error: '%s'. Please contact GAMS if this error persists.", e)
                 showHideEl(session, "#unknownError", 6000L)
               })
      })
    },
    contentType = "application/zip"
  )
  observeEvent(input$restoreDb, {
    req(input$dbBackupZip)
    noErr <- TRUE
    tryCatch({
      prog <- Progress$new()
      on.exit(prog$close(), add = TRUE)
      prog$set(message = "Data is being uploaded...", value = 0.2)
      tempDir <- file.path(tempdir(), "db_restore")
      if(dir.exists(tempDir) && 
         unlink(tempDir, recursive = TRUE, force = TRUE) == 1){
        stop(sprinft("Can't remove existing directory: '%s'.", tempDir), 
             call. = FALSE)
      }
      if(!all(dir.create(tempDir, showWarnings = FALSE, recursive = TRUE))){
        stop(sprinft("Can't create directory: '%s'.", tempDir), 
             call. = FALSE)
      }
      on.exit(unlink(tempDir, recursive = TRUE, force = TRUE), add = TRUE)
      grepEx <- "^((?!\\.\\.).)*\\.csv$"
      prog$inc(amount = 0.2, detail = "Decompressing files...")
      zipFilePath <- isolate(input$dbBackupZip$datapath)
      filesToUnzip <- grep(grepEx, unzip(zipFilePath, list = TRUE)$Name, 
                           ignore.case = TRUE, value = TRUE, perl = TRUE)
      if(!length(filesToUnzip)){
        stop("noData", call. = FALSE)
      }
      unzip(zipFilePath, filesToUnzip,
            exdir = gsub("/?$", "", tempDir))
      unzippedFiles  <- list.files(tempDir, pattern = "\\.csv$", full.names = TRUE)
      dbSchema       <- db$getDbSchema()
      tableNames     <- tolower(gsub(".csv", "", 
                                     basename(unzippedFiles), fixed = TRUE))
      metaId         <- match(dbSchema$tabName[["_scenMeta"]], tableNames)[[1L]]
      if(!is.na(metaId)){
        # make sure to read metadata table first
        tableNames[c(1L, metaId)]    <- tableNames[c(metaId, 1L)]
        unzippedFiles[c(1L, metaId)] <- unzippedFiles[c(metaId, 1L)]
      }
      scenTableNames <- db$getTableNamesScenario()
      if(!all(tableNames %in% dbSchema$tabName)){
        flog.info("Some tables in your archive do not exist in the current schema: '%s'.", 
                  paste(tableNames[!tableNames %in% scenTableNames], collapse = "', '"))
        stop("valErr", call. = FALSE)
      }
      prog$inc(amount = 0.2, detail = "Validating files...")
      validatedTables <- vector("list", length(unzippedFiles))
      validatedTables <- lapply(seq_along(unzippedFiles), function(i){
        data      <- read_csv(unzippedFiles[i], col_names = TRUE, 
                              col_types = cols(), na = character())
        tableName <- tableNames[i]
        if(!tableName %in% scenTableNames){
          tabID    <- match(tableName, dbSchema$tabName)[[1L]]
          
          if(is.na(tabID)){
            flog.info("Table name: '%s' could not be found in current database schema. Thus, it was rejected.", 
                      tableName)
            stop("valErr", call. = FALSE)
          }
          colNames <- dbSchema$colNames[[tabID]]
          colTypes <- dbSchema$colTypes[[tabID]]
        }else{
          tabID <- match(tableName, dbSchema$tabName)[[1L]]
          colNames <- c(sidIdentifier, dbSchema$colNames[[tabID]])
          colTypes <- "i" %+% dbSchema$colTypes[[tabID]]
        }
        if(!validateHeaders(data, colNames,
                            headerTypes = colTypes)){
          flog.warn("Dataset: '%s' has invalid headers.\nHeaders are: '%s'.\nHeaders should be: '%s'.\n
  Column types are: '%s'.\n Column types should be: '%s'.", 
                    tableName, paste(names(data), collapse = "', '"), 
                    paste(colNames, collapse = "', '"),
                    paste(vapply(data, function(el) return(class(el)[[1L]]), 
                                 character(1L), USE.NAMES = FALSE), collapse = "', '"),
                    colTypes)
                    
          stop("valErr", call. = FALSE)
        }
        
        return(data)
      })
      prog$inc(amount = 0.2, detail = "Uploading files...")
      lapply(seq_along(validatedTables), function(i){
        tableName <- tableNames[i]
        if(i == 1L){
          db$writeMetadata(validatedTables[[i]])
          return()
        }else if(identical(tableName, dbSchema$tabName[["_hcubeMeta"]])){
          db$writeMetadata(validatedTables[[i]], hcubeMetadata = TRUE)
          return()
        }
        db$exportScenDataset(validatedTables[[i]], tableName, 
                             addForeignKey = tableName %in% scenTableNames)
        prog$inc(amount = 0.2/length(validatedTables), detail = "Finished...")
      })
      prog$inc(amount = 0.2, detail = "Finished...")
      
    }, error = function(e){
      noErr <<- FALSE
      switch(conditionMessage(e),
             noData = {
               flog.info("No data found in archive. Nothing was restored.")
               showHideEl(session, "#restoreNoData", 6000L)
             },
             valErr = {
               flog.info("At least one of the tables is invalid. Nothing was restored.")
               showHideEl(session, "#restoreInvalidData", 6000L)
             },
             {
               flog.error("Unexpected error: '%s'.", e)
               showHideEl(session, "#unknownError", 6000L)
             })
    })
    if(!noErr)
      return()
    showHideEl(session, "#restoreSuccess", 3000L)
  })
  #configGenData <- config[!names(config) %in% c("pageTitle", 
  #                                              "MIROSwitch", 
  #                                              "gamsMetaDelim", 
  #                                              "fileExchange", 
  #                                              "csvDelim", "db")]
  #session$sendCustomMessage("parseConfig", 
  #                          list(config = configGenData[!names(configGenData) %in% c("gamsInputFiles",
  #                                                                                  "gamsOutputFiles")],
  #                               gmsio = configGenData[c("gamsInputFiles", "gamsOutputFiles")]))
  #observe({
  #  if(appDisconnected){
  #    stop("Please don't refresh the page via the browser as the JSON does not refresh.")
  #  }
  #  data <- input$updatedConfig
  #  if(!length(data))
  #    return()
  #  noErr <- TRUE
  #  tryCatch({
  #    confFilePath    <- file.path(currentModelDir, configDir, "config.json")
  #    confFilePathOld <- file.path(currentModelDir, configDir, "config_old.json")
  #    if(file.exists(confFilePath)[1]){
  #      file.copy(confFilePath, confFilePathOld, overwrite = TRUE)
  #      unlink(confFilePath, force = TRUE)
  #    }
  #    if(!length(data$datatable$options$columnDefs[[1]])){
  #      data$datatable$options$columnDefs <- NULL
  #    }
  #    jsonlite::write_json(data, confFilePath, pretty = TRUE, auto_unbox = TRUE,
  #                         null = "null")
  #  }, error = function(e){
  #    flog.error("Problems writing config.json file. Error message: '%s'.", e)
  #    showHideEl(session, "#updateConfigError", 4000L)
  #    noErr <<- FALSE
  #  })
  #  if(!noErr)
  #    return()
  #  hideEl(session, "#configGenForm")
  #  showEl(session, "#btConfigGenNew")
  #  showHideEl(session, "#updateConfigSuccess", 4000L)
  #})
  hideEl(session, "#loading-screen")
  session$onSessionEnded(function() {
    appDisconnected <<- TRUE
    if(!interactive()){
      stopApp()
    }
  })
}