appDisconnected <- FALSE
genSelectInput <- function(id, choices, selected){
  HTML(paste0('<div class="form-group shiny-input-container">\n
<label class="control-label" for="', id, '">', htmltools::htmlEscape("What should be plotted on the y axis?"), '</label>
              <div>
              <select id="', id, '">', paste(mapply(choices, names(choices), 
                                                    FUN = function(choice, label) {
                                                      sprintf(
                                                        '<option value="%s"%s>%s</option>',
                                                        htmltools::htmlEscape(choice, TRUE),
                                                        if (choice %in% selected) ' selected' else '',
                                                        htmltools::htmlEscape(label)
                                                      )}), collapse = "\n"), '</select><script type="application/json" data-for="', id, '" data-nonempty="">{}</script>
              </div>
              </div>'))
}
ydataInput <- function(indices){
  selected <- indices[1]
  HTML(paste0('<div class="shiny-input-container" style="margin-left:50px;"><hr>\n
<div id="chart_ydata_container">\n', 
              genSelectInput("chart_ydata", indices, indices[1]), '<hr></div>\n
<button onclick="addSelectInput(id=\'chart_ydata\')" type="button" class = "btn btn-default">Add trace</button>\n
              </div>'))
}
server_admin <- function(input, output, session){
  rv <- reactiveValues(chart_tool = 0L, plotly_type = 0L, saveJSONConfirm = 0L,
                       graphConfig = list(outType = "graph", graph = list()),
                       activeSymbol = list(id = integer(1L), name = character(1L), 
                                                alias = character(1L), indices = c()))
  configJSON <- suppressWarnings(jsonlite::fromJSON(paste0(currentModelDir, configDir, 
                                                           modelName, ".json"), 
                                                    simplifyDataFrame = FALSE, 
                                                    simplifyMatrix = FALSE))
  # ------------------------------------------------------
  #     CUSTOMIZE GRAPHS
  # ------------------------------------------------------
  modelInputData   <- vector("list", length(modelIn))
  hotInit          <- vector("logical", length(modelIn))
  isEmptyInput     <- vector(mode = "logical", length = length(modelIn))
  inputInitialized <- vector(mode = "logical", length = length(modelInWithDep))
  currentSelection <- list("tool" = character(0L), "plotly_type" = character(0L), 
                           "plotly_chart_type" = character(0L))
  currentConfig    <- list()
  optionsInserted  <- c()
  noOutputData     <- TRUE
  changeActiveSymbol <- function(id){
    headers <- modelIn[[id]]$headers
    isolate({
      headerAliases <- vapply(headers, '[[', character(1L), 
                              "alias", USE.NAMES = FALSE)
      rv$activeSymbol <- list(id = id, name = names(modelIn)[id], alias = modelInAlias[id],
                          indices = setNames(names(headers), 
                                             headerAliases), 
                          indexTypes = vapply(headers, '[[', character(1L), 
                                              "type", USE.NAMES = FALSE))
      scalarIndices <- rv$activeSymbol$indices[rv$activeSymbol$indexTypes == "parameter"]
      session$sendCustomMessage("gms-setIndices", list(indices = names(headers), 
                                                       aliases = headerAliases,
                                                       scalarIndices = unname(scalarIndices),
                                                       scalarAliases = names(scalarIndices)))
      })
    
  }
  observeEvent(input$localInput, {
    # initialize new imported sheets counter
    newInputCount <- 0L
    errMsg <- NULL
    scalarDataset <- NULL
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
    if(!length(tabularInputWithData)){
      showErrorMsg("No valid data", "No valid datasets were found. Please upload a properly formatted Excel file with data.")
      return()
    }
    showEl(session, "#preview_wrapper")
    updateSelectInput(session, "gams_symbols", choices = tabularInputWithData)
    rv$activeSymbol  <<- changeActiveSymbol(which(!isEmptyInput)[1])
    isolate(rv$chart_tool <- rv$chart_tool + 1L)
  })
  observeEvent(input$chart_title, {
    rv$graphConfig$graph$title <<- input$chart_title
  })
  observe({
    req(input$plotly_type)
    rv$plotly_type
    isolate({
      if(length(currentSelection$plotly_type)){
        currentConfig[[paste0("plotly", 
                              currentSelection$plotly_type)]] <<- rv$graphConfig$graph
      }
      currentSelection$plotly_type <<- input$plotly_type
      if(length(currentConfig[[paste0("plotly", input$plotly_type)]])){
        rv$graphConfig$graph <<- currentConfig[[paste0("plotly", input$plotly_type)]]
      }else{
        print('2')
        rv$graphConfig$graph[!names(rv$graphConfig$graph) %in% c("tool", "title")] <<- NULL
      }
    })
    removeUI(selector = "#plotly_options .shiny-input-container", multiple = TRUE)
    if(identical(isolate(input$plotly_type), "pie")){
      insertUI(selector = "#plotly_options",
               getPieOptions(), where = "beforeEnd")
    }else if(identical(isolate(input$plotly_type), "chart")){
      insertUI(selector = "#plotly_options",
               tagList(
                 selectInput("plotly_chart_type", "Select a chart type", 
                             setNames(c("bar", "scatter", "line"), 
                                      c("Bar chart", "Scatter plot", "Line chart"))),
                 tags$div(id = "plotly_chart_options", class = "shiny-input-container",
                          getChartOptions())
               ), where = "beforeEnd")
    }else if(identical(isolate(input$plotly_type), "hist")){
      insertUI(selector = "#plotly_options", getHistOptions(), where = "beforeEnd")
      
    }
    if(identical(input$plotly_type, "chart")){
      isolate(rv$graphConfig$graph$type <<- "bar")
    }else{
      isolate(rv$graphConfig$graph$type <<- input$plotly_type)
    }
  })
  observe({
    req(input$plotly_chart_type)
    isolate({
      if(length(currentSelection$plotly_chart_type)){
        currentConfig[[paste0("plotlychart", 
                              currentSelection$plotly_chart_type)]] <<- rv$graphConfig$graph
      }
      currentSelection$plotly_chart_type <<- input$plotly_chart_type
      if(length(currentConfig[[paste0("plotlychart", input$plotly_chart_type)]])){
        rv$graphConfig$graph <<- currentConfig[[paste0("plotlychart", input$plotly_chart_type)]]
      }else{
        rv$graphConfig$graph[!names(rv$graphConfig$graph) %in% c("tool", "type", "title")] <<- NULL
      }
    })
    removeUI(selector = "#plotly_chart_options .shiny-input-container", multiple = TRUE)
    if(identical(input$plotly_chart_type, "bar")){
      isolate(rv$graphConfig$graph$type <<- "bar")
      insertUI(selector = "#plotly_chart_options",
               getBarOptions(), where = "beforeEnd")
    }else if(identical(input$plotly_chart_type, "scatter")){
      isolate({
        lapply(seq_along(rv$graphConfig$graph$ydata), function(i){
          rv$graphConfig$graph$ydata[[i]]$mode <<- "markers"
          rv$graphConfig$graph$ydata[[i]]$line <<- list()})
        rv$graphConfig$graph$type <<- "scatter"
      })
      insertUI(selector = "#plotly_chart_options",
               getScatterOptions(), where = "beforeEnd")
    }else{
      isolate({
        lapply(seq_along(rv$graphConfig$graph$ydata), function(i){
          rv$graphConfig$graph$ydata[[i]]$mode <<- "lines"
          rv$graphConfig$graph$ydata[[i]]$marker <<- list()})
        rv$graphConfig$graph$type <<- "scatter"
      })
      insertUI(selector = "#plotly_chart_options",
               getLineOptions(), where = "beforeEnd")
    }
    
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
    rv$graphConfig$graph$ydata[[1]]$marker$symbol <<- input$marker_symbol
  })
  observeEvent(input$marker_color, {
    rv$graphConfig$graph$ydata[[1]]$marker$color <<- input$marker_color
  })
  observeEvent(input$marker_opacity, {
    rv$graphConfig$graph$ydata[[1]]$marker$opacity <<- input$marker_opacity
  })
  observeEvent(input$marker_size, {
    rv$graphConfig$graph$ydata[[1]]$marker$size <<- input$marker_size
  })
  observeEvent(input$marker_line_width, {
    rv$graphConfig$graph$ydata[[1]]$marker$line$width <<- input$marker_line_width
  })
  observeEvent(input$marker_line_color, {
    rv$graphConfig$graph$ydata[[1]]$marker$line$color <<- input$marker_line_color
  })
  observeEvent(input$line_color, {
    rv$graphConfig$graph$ydata[[1]]$line$color <<- input$line_color
  })
  observeEvent(input$line_width, {
    rv$graphConfig$graph$ydata[[1]]$line$width <<- input$line_width
  })
  observeEvent(input$line_shape, {
    rv$graphConfig$graph$ydata[[1]]$line$shape <<- input$line_shape
  })
  observeEvent(input$line_dash, {
    rv$graphConfig$graph$ydata[[1]]$line$dash <<- input$line_dash
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
  observeEvent(input$dyopt_fillGraph, {
    rv$graphConfig$graph$dyOptions$fillGraph <<- input$dyopt_fillGraph
  })
  observeEvent(input$dyopt_fillAlpha, {
    rv$graphConfig$graph$dyOptions$fillAlpha <<- input$dyopt_fillAlpha
  })
  observeEvent(input$dyopt_drawPoints, {
    rv$graphConfig$graph$dyOptions$drawPoints <<- input$dyopt_drawPoints
  })
  observeEvent(input$dyopt_pointShape, {
    rv$graphConfig$graph$dyOptions$pointShape <<- input$dyopt_pointShape
  })
  observeEvent(input$dyopt_pointSize, {
    rv$graphConfig$graph$dyOptions$pointSize <<- input$dyopt_pointSize
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
    rv$graphConfig$graph$xdata[[1]]$label <<- input$hist_label
  })
  observeEvent(input$hist_color, {
    rv$graphConfig$graph$xdata[[1]]$color <<- input$hist_color
  })
  observeEvent(input$hist_alpha, {
    rv$graphConfig$graph$xdata[[1]]$alpha <<- input$hist_alpha
  })
  observeEvent(input$chart_color, {
    if(identical(input$chart_color, "_"))
      rv$graphConfig$graph$color <<- NULL
    else
      rv$graphConfig$graph$color <<- input$chart_color
  })
  observeEvent(input$chart_xdata, {
    if(!(length(input$x_title) && nchar(input$x_title))){
      updateTextInput(session, "x_title", value = names(rv$activeSymbol$indices)[match(input$chart_xdata, 
                                                                              rv$activeSymbol$indices)][1])
    }
    rv$graphConfig$graph$xdata <<- input$chart_xdata
  })
  observeEvent(input$hist_data, {
    label <- input$hist_label
    if(!(length(label) && nchar(label))){
      updateTextInput(session, "hist_label", value = names(rv$activeSymbol$indices)[match(input$hist_data, 
                                                                                          rv$activeSymbol$indices)][1])
    }
    rv$graphConfig$graph$xdata[[input$hist_data]] <<- list(labels = label)
  })
  observeEvent(input$chart_ydata, {
    if(length(input$chart_ydata) < 2){
      chart_ydata <- c(1, input$chart_ydata)
    }else{
      chart_ydata <- input$chart_ydata
    }
    if(chart_ydata[1] == 1 && !(length(input$y_title) && nchar(input$y_title))){
      updateTextInput(session, "y_title", value = names(rv$activeSymbol$indices)[match(chart_ydata[2], 
                                                                                       rv$activeSymbol$indices)][1])
    }
    rv$graphConfig$graph$ydata[[chart_ydata[2]]] <<- list(label = chart_ydata[2], 
                                                             mode = if(identical(input$plotly_chart_type, "scatter"))
"markers" else "lines")
  })
  observeEvent(input$bar_mode, {
    rv$graphConfig$graph$barmode <<- input$bar_mode
  })
  observeEvent(input$gams_symbols, {
    req(input$gams_symbols)
    changeActiveSymbol(match(isolate(input$gams_symbols), names(modelIn)))
    updateTextInput(session, "chart_title", value = rv$activeSymbol$alias)
  })
  observe({
    rv$chart_tool
    input$chart_tool
    req(input$chart_tool)
    isolate({
      if(length(currentSelection$chart_tool)){
        currentConfig[[currentSelection$chart_tool]] <<- rv$graphConfig$graph
      }
      currentSelection$chart_tool <<- input$chart_tool
      rv$graphConfig$graph[!names(rv$graphConfig$graph) %in% c("title")] <<- NULL
    })
    removeUI(selector = "#tool_options div", multiple = TRUE)
    if(identical(isolate(input$chart_tool), "plotly")){
      insertUI(selector = "#tool_options",
               tags$div(id = "plotly_type_container",
                        selectInput("plotly_type", "Select the type of chart you want to plot",
                                    choices = setNames(c("pie", "chart", "hist"), 
                                                       c("Pie chart", "Chart", "Histogram"))),
                        tags$div(id = "plotly_options")
               ), where = "beforeEnd")
    }else if(identical(isolate(input$chart_tool), "dygraphs")){
      isolate({
        if(length(currentConfig[[currentSelection$chart_tool]])){
          rv$graphConfig$graph <<- currentConfig[[currentSelection$chart_tool]]
        }
      })
      insertUI(selector = "#tool_options",
               tags$div(id = "dygraph_options", getDygraphsOptions()), where = "beforeEnd")
    }
    isolate({
      rv$graphConfig$graph$tool <<- input$chart_tool
      rv$plotly_type            <<- rv$plotly_type + 1L
    })
  })
  getPieOptions <- reactive({
    indices       <- rv$activeSymbol$indices
    scalarIndices <- indices[rv$activeSymbol$indexTypes == "parameter"]
    isolate({
      rv$graphConfig$graph$labels <<- unname(indices[1])
      rv$graphConfig$graph$values <<- unname(scalarIndices[1])
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
    indices       <- rv$activeSymbol$indices
    scalarIndices <- indices[rv$activeSymbol$indexTypes == "parameter"]
    isolate({
      rv$graphConfig$graph$xdata <<- unname(indices[1])
      rv$graphConfig$graph$ydata[[scalarIndices[1]]] <<- list(label = unname(scalarIndices[1]), 
                                                               mode = if(identical(input$plotly_chart_type, "scatter"))
                                                                 "markers" else "lines")
    })
    tagList(
      selectInput("chart_xdata", "What should be plotted on the x axis?",
                  choices = indices),
      ydataInput(indices = scalarIndices),
      selectInput("chart_color", "Symbol that is used to select different colors",
                  choices = c("_", indices)),
      getAxisOptions("x", names(indices)[1]),
      getAxisOptions("y", names(scalarIndices)[1])
    )
  })
  getBarOptions  <- reactive({
    isolate({
      rv$graphConfig$graph$barmode <<- "group"
    })
    tagList(selectInput("bar_mode", "Select barmode", choices = c("group", "stack")),
            getChartOptions())
  })
  getScatterOptions  <- reactive({
    isolate({
      rv$graphConfig$graph$ydata[[1]]$marker$symbol  <<- "circle"
      rv$graphConfig$graph$ydata[[1]]$marker$color   <<- "black"
      rv$graphConfig$graph$ydata[[1]]$marker$opacity <<- 1L
      rv$graphConfig$graph$ydata[[1]]$marker$size    <<- 6L
      rv$graphConfig$graph$ydata[[1]]$marker$line$width <<- 0L
    })
    tagList(
      getChartOptions(),
      selectInput("marker_symbol", "Select marker symbol", choices= c("circle", "circle-open", "circle-dot", 
                                                                    "circle-open-dot", "square", "square-open",
                                                                    "square-dot", "square-open-dot", "diamond",
                                                                    "diamond-open", "diamond-dot", "diamond-open-dot",
                                                                    "cross", "cross-open", "cross-dot", "cross-open-dot",
                                                                    "x", "x-open", "x-dot", "x-open-dot", "triangle-up",
                                                                    "triangle-up-open", "triangle-up-dot", "triangle-up-open-dot",
                                                                    "triangle-down", "triangle-down-open", "triangle-down-dot",
                                                                    "triangle-down-open-dot", "triangle-left", "triangle-left-open", 
                                                                    "triangle-left-dot", "triangle-left-open-dot", "triangle-right",
                                                                    "triangle-right-open", "triangle-right-dot", "triangle-right-open-dot",
                                                                    "triangle-ne", "triangle-ne-open", "triangle-ne-dot", "triangle-ne-open-dot",
                                                                    "triangle-se", "triangle-se-open", "triangle-se-dot", "triangle-se-open-dot",
                                                                    "triangle-sw", "triangle-sw-open", "triangle-sw-dot", "triangle-sw-open-dot",
                                                                    "triangle-nw", "triangle-nw-open", "triangle-nw-dot", "triangle-nw-open-dot",
                                                                    "pentagon", "pentagon-open", "pentagon-dot", "pentagon-open-dot", "hexagon",
                                                                    "hexagon-open", "hexagon-dot", "hexagon-open-dot", "hexagon2", "hexagon2-open", 
                                                                    "hexagon2-dot", "hexagon2-open-dot", "octagon", "octagon-open", "octagon-dot",
                                                                    "octagon-open-dot", "star", "star-open", "star-dot", "star-open-dot", "hexagram",
                                                                    "hexagram-open", "hexagram-dot", "hexagram-open-dot", "star-triangle-up",
                                                                    "star-triangle-up-open", "star-triangle-up-dot", "star-triangle-up-open-dot",
                                                                    "star-triangle-down", "star-triangle-down-open", "star-triangle-down-dot", "star-triangle-down-open-dot",
                                                                    "star-square", "star-square-open", "star-square-dot", "star-square-open-dot", "star-diamond", 
                                                                    "star-diamond-open", "star-diamond-dot", "star-diamond-open-dot", "diamond-tall",
                                                                    "diamond-tall-open", "diamond-tall-dot", "diamond-tall-open-dot", "diamond-wide",
                                                                    "diamond-wide-open", "diamond-wide-dot", "diamond-wide-open-dot", "hourglass",
                                                                    "hourglass-open", "bowtie", "bowtie-open", "circle-cross", "circle-cross-open", 
                                                                    "circle-x", "circle-x-open", "square-cross", "square-cross-open", "square-x", "square-x-open",
                                                                    "diamond-cross", "diamond-cross-open", "diamond-x", "diamond-x-open", "cross-thin",
                                                                    "cross-thin-open", "x-thin", "x-thin-open", "asterisk", "asterisk-open", "hash", "hash-open",
                                                                    "hash-dot", "hash-open-dot", "y-up", "y-up-open", "y-down", "y-down-open", "y-left", 
                                                                    "y-left-open", "y-right", "y-right-open", "line-ew", "line-ew-open", "line-ns", 
                                                                    "line-ns-open", "line-ne", "line-ne-open", "line-nw", "line-nw-open")),
      textInput("marker_color", "Select marker color", value = "black"),
      numericInput("marker_opacity", "Select marker opacity", value = 1L, min = 0L, max = 1L),
      numericInput("marker_size", "Select marker size", value = 6L, min = 0L),
      numericInput("marker_line_width", "Select marker outline width", value = 0L, min = 0L),
      textInput("marker_line_color", "Select marker outline color")
    )
  })
  getLineOptions  <- reactive({
    isolate({
      rv$graphConfig$graph$ydata[[1]]$line$color <- "black"
      rv$graphConfig$graph$ydata[[1]]$line$width <- 2L
      rv$graphConfig$graph$ydata[[1]]$line$shape <- "linear"
      rv$graphConfig$graph$ydata[[1]]$line$width <- "dash"
    })
    tagList(
      getChartOptions(),
      textInput("line_color", "Select line color", value = "black"),
      numericInput("line_width", "Select line width", value = 2L, min = 0L),
      selectInput("line_shape", "Select line shape", choices = c("linear", "spline", "hv",
                                                                 "vh", "hvh", "vhv")),
      selectInput("line_dash", "Select line dash type", choices = c("solid", "dot", "dash", 
                                                                    "longdash", "dashdot", "longdashdot"))
    )
  })
  getHistOptions <- reactive({
    scalarIndices <- rv$activeSymbol$indices[rv$activeSymbol$indexTypes == "parameter"]
    isolate({
      label <- names(rv$activeSymbol$indices)[match(scalarIndices[1], 
                                                    rv$activeSymbol$indices)][1]
      rv$graphConfig$graph$xdata      <<- list()
      rv$graphConfig$graph$xdata[[scalarIndices[1]]] <<- list(label = unname(label))
      rv$graphConfig$graph$histnorm   <<- ""
      rv$graphConfig$graph$nbins      <<- 2L
      rv$graphConfig$graph$barmode    <<- "overlay"
    })
    tagList(
      selectInput("hist_data", "Select column to plot", scalarIndices),
      textInput("hist_label", "Select data label", value = label),
      textInput("hist_color", "Select bar color", value = "black"),
      numericInput("hist_alpha", "Choose bar transparency", min = 0L, max = 1L, value = 1L),
      selectInput("hist_norm", "Type of normalization to use",
                  choices = setNames(c(" ", "percent", "density"), c("Number of occurances", 
                                                                    "Percentage of occurances", 
                                                                    "Number of occurances/ Bin interval size"))),
      numericInput("hist_nbins", "Number of bins",
                   min = 0L, value = 2L),
      selectInput("hist_barmode", "How do you want the bars to be displayed?",
                  choices = c("overlay", "stack", "group", "relative")),
      getAxisOptions("x", "", labelOnly = TRUE),
      getAxisOptions("y", "", labelOnly = TRUE)
    )
  })
  getDygraphsOptions <- reactive({
    indices       <- rv$activeSymbol$indices
    scalarIndices <- indices[rv$activeSymbol$indexTypes == "parameter"]
    if(!length(scalarIndices)){
      output[["preview-errmsg"]] <- renderText("No scalar indices found in your data.")
      return()
    }
    isolate({
      print('asd')
      rv$graphConfig$graph$xdata <<- unname(indices[1])
      rv$graphConfig$graph$ydata[[scalarIndices[1]]] <<- list(label = unname(scalarIndices[1]), 
                                                              mode = if(identical(input$plotly_chart_type, "scatter"))
                                                                "markers" else "lines")
      rv$graphConfig$graph$dyOptions$includeZero <<- FALSE
      rv$graphConfig$graph$dyOptions$logscale    <<- FALSE
      rv$graphConfig$graph$dyOptions$drawGrid    <<- FALSE
      rv$graphConfig$graph$dyOptions$fillGraph   <<- FALSE
      rv$graphConfig$graph$dyOptions$fillAlpha   <<- 0.15
      rv$graphConfig$graph$dyOptions$drawPoints  <<- FALSE
      rv$graphConfig$graph$dyOptions$pointShape  <<- "dot"
      rv$graphConfig$graph$dyOptions$pointSize   <<- 2L
    })
    tagList(
      selectInput("chart_xdata", "What index do you want to plot on the x-axis?",
                  choices = indices),
      selectInput("chart_ydata", "What index do you want to plot on the y-axis?",
                  choices = scalarIndices),
      selectInput("chart_color", "What symbol do you want to use to plot different chart lines?",
                  choices = c("_", indices)),
      checkboxInput("dyopt_incZero", "Should y-axis start at 0?"),
      checkboxInput("dyopt_logscale", "Show y-axis in log scale?"),
      checkboxInput("dyopt_drawGrid", "Should grid lines be displayed?", TRUE),
      checkboxInput("dyopt_fillGraph", "Should the area underneath the graph be filled?"),
      numericInput("dyopt_fillAlpha", "Transparency for filled regions", min = 0L, max = 1L, value = 0.15),
      checkboxInput("dyopt_drawPoints", "Should points be drawn?"),
      selectInput("dyopt_pointShape", "What shape should points have?", 
                  choices = c("dot", "triangle", "square", "diamond", "pentagon", "hexagon", 
                              "circle", "star", "plus", "ex")),
      numericInput("dyopt_pointSize", "What size should points be?", min = 0L, value = 2L),
      getAxisOptions("x", names(indices)[1], labelOnly = TRUE),
      getAxisOptions("y", names(scalarIndices)[1], labelOnly = TRUE)
    )
  })
  observe({
    req(rv$graphConfig$graph$tool, rv$activeSymbol$id > 0L)
    print("++++++++++++++++++")
    print(rv$graphConfig$graph)
    tryCatch({
      if(isolate(rv$graphConfig$graph$tool) == "plotly"){
        callModule(renderData, "preview_output_plotly", type = rv$graphConfig$outType, 
                   data = modelInputData[[rv$activeSymbol$id]],
                   graphOptions = rv$graphConfig$graph,
                   roundPrecision = roundPrecision, modelDir = modelDir)
        showEl(session, "#preview-content-plotly")
        hideEl(session, "#preview-content-dygraph")
      }else{
        callModule(renderData, "preview_output_dygraph", type = rv$graphConfig$outType, 
                   data = modelInputData[[rv$activeSymbol$id]],
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
    if(tolower(rv$activeSymbol$name) %in% tolower(names(configJSON$dataRendering))){
      showModal(modalDialog(title = "Data exists", sprintf("A graph configuration already exists for symbol: '%s'. Do you want to overwrite this configuration? This cannot be undone!", rv$activeSymbol$name), 
                            footer = tagList(modalButton("Cancel"), 
                                             actionButton("saveJSONConfirm", "Overwrite"))))
      return()
    }
    rv$saveJSONConfirm <- rv$saveJSONConfirm + 1L
  })
  observeEvent(virtualActionButton(input$saveJSONConfirm, rv$saveJSONConfirm), {
    configJSON$dataRendering[[rv$activeSymbol$name]] <- rv$graphConfig
    toJSON(configJSON, pretty = TRUE)
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