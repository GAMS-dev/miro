renderDataUI <- function(id, type, graphTool = NULL, height= NULL, customOptions = NULL, 
                          modelDir = NULL, noDataTxt = "no data"){
  ns <- NS(id)
  # make output type case insensitive
  typeCustom <- type
  type <- tolower(type)
  
  if(type == "pivot"){
    # set default height
    if(is.null(height)){
      height <- pivotDefaultHeight
    }
    data <- rpivotTableOutput(ns("pivottable"), height = height)
  }else if(type == "datatable"){
    data <- dataTableOutput(ns("datatable"))
  }else if(type == "dtgraph"){
    if(graphTool == "plotly"){
      data <- tagList(
        tags$div(style = "overflow-x:hidden;",
                 column(6, dataTableOutput(ns("datatable")), style = "overflow-x:auto;"),
                 column(6, tags$div(class = "renderer-wrapper", 
                                    genSpinner(externalStyle = TRUE),
                                    plotlyOutput(ns("graph"), height = height)), style = "overflow-x:auto;")
        )
      )
    }else if(graphTool == "dygraphs"){
      data <- tagList(
        tags$div(style = "overflow-x:hidden;",
          column(6, dataTableOutput(ns("datatable")), style = "overflow-x:auto;"),
          column(6, dygraphOutput(ns("graph")), height = height, style = "overflow-x:auto;")
        )
      )
    }else if(graphTool == "leaflet"){
      data <- tagList(
        tags$div(style = "overflow-x:hidden;",
                 column(6, dataTableOutput(ns("datatable")), style = "overflow-x:auto;"),
                 column(6, leafletOutput(ns("graph")), height = height, style = "overflow-x:auto;")
        )
      )
    }else if(graphTool == "timevis"){
      data <- tagList(
        tags$div(style = "overflow-x:hidden;",
                 column(6, dataTableOutput(ns("datatable")), style = "overflow-x:auto;"),
                 column(6, timevisOutput(ns("graph")), height = height, style = "overflow-x:auto;")
        )
      )
    }else{
      stop(paste0("The tool you selected for: '", id,"' is not supported by the current version of GAMS WebUI."))
    }
  }else if(type == "graph"){
    if(graphTool == "plotly"){
      data <- plotlyOutput(ns("graph"), height = height)
    }else if(graphTool == "dygraphs"){
      data <- dygraphOutput(ns("graph"), height = height)
    }else if(graphTool == "leaflet"){
      data <- leafletOutput(ns("graph"), height = height)
    }else if(graphTool == "timevis"){
      data <- timevisOutput(ns("graph"), height = height)
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
      customOutput <- match.fun(typeCustom %+% "Output")
    }, error = function(e){
      stop(sprintf("An output function for the custom renderer: '%s' was not found. 
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
    })
    data <- customOutput(ns("custom"), height = height, options = customOptions,
                         path = customRendererDirs[[2L]])
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
  if(!length(data) || identical(nrow(data), 0L)){
    showEl(session, "#" %+% session$ns("noData"))
    hideEl(session, "#" %+% session$ns("data"))
    return()
  }else{
    showEl(session, "#" %+% session$ns("data"))
    hideEl(session, "#" %+% session$ns("noData"))
  }
  # make output type case insensitive
  typeCustom <- type
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
    lapply(seq_len(min(length(data[[1]]), customOptions$count)), function(i){
      scalarDataTmp <- suppressWarnings(as.numeric(data[[3]][[i]]))
      if(is.na(scalarDataTmp)){
        scalarData <- data[[3]][[i]]
      }else{
        scalarData <- formatC(round(scalarDataTmp, roundPrecision), big.mark = " ", 
                              big.interval = 3, format = "f", drop0trailing = TRUE)
      }
      output[["valBox" %+% i]] <- renderValueBox({
        valueBox(
          scalarData, data[[2]][[i]], 
          getIcon(customOptions$icon$name, customOptions$icon$lib),
          if(identical(customOptions$color, NULL)) "aqua" else customOptions$color
        )
      })
    })
  }else{
    tryCatch({
      customRenderer <- match.fun(paste0("render", toupper(substr(typeCustom, 1, 1)),
                                         substr(typeCustom, 2, nchar(typeCustom))))
    }, error = function(e){
      stop(sprintf("A custom renderer function: '%s' was not found. 
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
    })
    tryCatch({
      callModule(customRenderer, "custom", as_tibble(data), options = customOptions, 
                 path = customRendererDirs[[2L]])
    }, error = function(e){
      stop(sprintf("An error occured in the custom renderer function: '%s'. Error message: %s.", typeCustom, e), call. = FALSE)
    })
  }
  
}