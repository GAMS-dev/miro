renderDataUI <- function(id, type, graph.tool = NULL, height= NULL, custom.options = NULL, 
                          modelDir = NULL, no.data.txt = "no data"){
  ns <- NS(id)
  # make output type case insensitive
  type <- tolower(type)
  
  if(type == "pivot"){
    # set default height
    if(is.null(height)){
      height <- pivot.default.height
    }
    data <- rpivotTable::rpivotTableOutput(ns("pivottable"), height = height)
  }else if(type == "datatable"){
    data <- DT::dataTableOutput(ns("datatable"))
  }else if(type == "dtgraph"){
    if(graph.tool == "plotly"){
      data <- tagList(
        fluidRow(
          column(6, DT::dataTableOutput(ns("datatable"), height = height)),
          column(6, plotly::plotlyOutput(ns("graph"), height = height))
        )
      )
    }else if(graph.tool == "dygraph"){
      data <- tagList(
        fluidRow(
          column(6, DT::dataTableOutput(ns("datatable"), height = height)),
          column(6, dygraphs::dygraphOutput(ns("graph"), height = height))
        )
      )
    }else{
      stop(paste0("The tool you selected for: '", id,"' is not supported by the current version of GAMS WebUI."))
    }
  }else if(type == "graph"){
    if(graph.tool == "plotly"){
      data <- plotly::plotlyOutput(ns("graph"), height = height)
    }else if(graph.tool == "dygraph"){
      data <- dygraphs::dygraphOutput(ns("graph"), height = height)
    }else{
      stop(paste0("The tool you selected for: '", id,"' is not supported by the current version of GAMS WebUI."))
    }
  }else{
    tryCatch({
      customOutput <- match.fun(tolower(type) %+% "Output")
    }, error = function(e){
      stop(sprintf("An output function for the custom renderer: '%s' was not found. 
                   Please make sure you first define such a function.", type), call. = FALSE)
    })
    
    data <- customOutput(ns("custom"), height = height, options = custom.options, path = modelDir %+% dir.custom.renderer)
  }
  return(tagList(
    tags$div(id = ns("noData"), class = "out-no-data", no.data.txt),
    tags$div(id = ns("data"), data)
  ))
}

renderData <- function(input, output, session, data, type, config.data = NULL, dt.options = NULL, 
                        graph.options = NULL, pivot.options = NULL, custom.options = NULL, 
                        roundPrecision = 2, modelDir = NULL){
  if(!is.null(graph.options)){
    graph.tool <- graph.options$tool
  }
  if(identical(nrow(data), 0L)){
    shinyjs::show("noData")
    shinyjs::hide("data")
    return(NULL)
  }else{
    shinyjs::show("data")
    shinyjs::hide("noData")
  }
  # make output type case insensitive
  type <- tolower(type)
  
  if(type == "pivot"){
    output$pivottable <- renderPivot(data, options = pivot.options, roundPrecision = roundPrecision)
  }else if(type == "graph"){
    output$graph <- renderGraph(data, config.data = config.data, options = graph.options)
  }else if(type == "datatable" || type == "dtgraph"){
    output$datatable <- renderDTable(data, options = dt.options, roundPrecision = roundPrecision)
    if(!is.null(graph.options)){
      output$graph <- renderGraph(data, config.data = config.data, options = graph.options)
    }
  }else{
    tryCatch({
      customRenderer <- match.fun("render" %+% toupper(substr(type, 1, 1)) %+% tolower(substr(type, 2, nchar(type))))
    }, error = function(e){
      stop(sprintf("A custom renderer function: '%s' was not found. 
                   Please make sure you first define such a function.", type), call. = FALSE)
    })
    tryCatch({
      callModule(customRenderer, "custom", as_tibble(data), options = custom.options, path = modelDir %+% dir.custom.renderer)
    }, error = function(e){
      stop(sprintf("An error occured in the custom renderer function: '%s'. Error message: %s.", type, e), call. = FALSE)
    })
  }
  
}