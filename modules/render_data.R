renderDataUI <- function(id, type, graphTool = NULL, height= NULL, customOptions = NULL, 
                          modelDir = NULL, noDataTxt = "no data"){
  ns <- NS(id)
  # make output type case insensitive
  type <- tolower(type)
  
  if(type == "pivot"){
    # set default height
    if(is.null(height)){
      height <- pivotDefaultHeight
    }
    data <- rpivotTable::rpivotTableOutput(ns("pivottable"), height = height)
  }else if(type == "datatable"){
    data <- DT::dataTableOutput(ns("datatable"))
  }else if(type == "dtgraph"){
    if(graphTool == "plotly"){
      data <- tagList(
        fluidRow(
          column(6, DT::dataTableOutput(ns("datatable"), height = height)),
          column(6, plotly::plotlyOutput(ns("graph"), height = height))
        )
      )
    }else if(graphTool == "dygraph"){
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
    if(graphTool == "plotly"){
      data <- plotly::plotlyOutput(ns("graph"), height = height)
    }else if(graphTool == "dygraph"){
      data <- dygraphs::dygraphOutput(ns("graph"), height = height)
    }else{
      stop(paste0("The tool you selected for: '", id,"' is not supported by the current version of GAMS WebUI."))
    }
  }else if(type == "valuebox"){
    data <- lapply(seq_len(customOptions$count), function(i){
      valueBoxOutput(ns("valBox" %+% i),
                     width = if(identical(customOptions$width, NULL)) 4 else customOptions$width)
    })
  }else{
    tryCatch({
      customOutput <- match.fun(tolower(type) %+% "Output")
    }, error = function(e){
      stop(sprintf("An output function for the custom renderer: '%s' was not found. 
                   Please make sure you first define such a function.", type), call. = FALSE)
    })
    data <- customOutput(ns("custom"), height = height, options = customOptions,
                         path = customRendererDir)
  }
  return(tagList(
    tags$div(id = ns("noData"), class = "out-no-data", noDataTxt),
    tags$div(id = ns("data"), data)
  ))
}

renderData <- function(input, output, session, data, type, configData = NULL, dtOptions = NULL, 
                        graphOptions = NULL, pivotOptions = NULL, customOptions = NULL, 
                        roundPrecision = 2, modelDir = NULL){
  if(!is.null(graphOptions)){
    graphTool <- graphOptions$tool
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
    output$pivottable <- renderPivot(data, options = pivotOptions, roundPrecision = roundPrecision)
  }else if(type == "graph"){
    output$graph <- renderGraph(data, configData = configData, options = graphOptions)
  }else if(type == "datatable" || type == "dtgraph"){
    output$datatable <- renderDTable(data, options = dtOptions, roundPrecision = roundPrecision)
    if(!is.null(graphOptions)){
      output$graph <- renderGraph(data, configData = configData, options = graphOptions)
    }
  }else if(type == "valuebox"){
    lapply(seq_len(customOptions$count), function(i){
      output[["valBox" %+% i]] <- renderValueBox({
        valueBox(
          round(data[[3]][[i]], roundPrecision), data[[2]][[i]], 
          getIcon(customOptions$icon$name, customOptions$icon$lib),
          if(identical(customOptions$color, NULL)) "aqua" else customOptions$color
        )
      })
    })
  }else{
    tryCatch({
      customRenderer <- match.fun("render" %+% toupper(substr(type, 1, 1)) %+% tolower(substr(type, 2, nchar(type))))
    }, error = function(e){
      stop(sprintf("A custom renderer function: '%s' was not found. 
                   Please make sure you first define such a function.", type), call. = FALSE)
    })
    tryCatch({
      callModule(customRenderer, "custom", as_tibble(data), options = customOptions, 
                 path = customRendererDir)
    }, error = function(e){
      stop(sprintf("An error occured in the custom renderer function: '%s'. Error message: %s.", type, e), call. = FALSE)
    })
  }
  
}