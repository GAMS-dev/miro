renderDataUI <- function(id, type, graphTool = NULL, height= NULL, customOptions = NULL, 
                         filterOptions = NULL, modelDir = NULL, createdDynamically = FALSE, showNoDataTxt = TRUE){
  ns <- NS(id)
  # make output type case insensitive
  typeCustom <- type
  type <- tolower(type)
  if(!length(type))
    type <- "datatable"
  
  if(type == "pivot"){
    # set default height
    if(is.null(height)){
      height <- pivotDefaultHeight
    }
    data <- rpivotTableOutput(ns("pivottable"), height = height)
  }else if(type == "datatable"){
    data <- dataTableOutput(ns("datatable"))
  }else if(type %in% c("graph", "dtgraph")){
    if(graphTool == "plotly"){
      if(type == "graph"){
        dataGraph <- plotlyOutput(ns("graph"), height = height)
      }else{
        dataGraph <- tags$div(class = "renderer-wrapper", 
                              genSpinner(externalStyle = character(0L)),
                              plotlyOutput(ns("graph"), height = height))
      }
    }else if(graphTool == "dygraphs"){
      dataGraph <- dygraphOutput(ns("graph"), height = height)
    }else if(graphTool == "leaflet"){
      dataGraph <- leafletOutput(ns("graph"), height = height)
    }else if(graphTool == "timevis"){
      dataGraph <- timevisOutput(ns("graph"), height = height)
    }else{
      stop(paste0("The tool you selected for: '", id,"' is not supported by the current version of GAMS MIRO."))
    }
    if(length(filterOptions$col)){
      data <- tagList(tags$div(class = "data-filter-wrapper", 
                               if(isTRUE(filterOptions$date)){
                                 dateRangeInput(ns("data_filter"),
                                                filterOptions$label)
                               }else{
                                 selectInput(ns("data_filter"), 
                                             filterOptions$label, 
                                             choices = c(), multiple = isTRUE(filterOptions$multiple))
                               },
                               dataGraph))
    }else{
      data <- dataGraph
    }
    if(type == "dtgraph"){
      data <- tagList(
        tags$div(style = "overflow-x:hidden;",
                 column(6, dataTableOutput(ns("datatable")), style = "overflow-x:auto;"),
                 column(6, data, style = "overflow-x:auto;")
        )
      )
    }
  }else if(type == "valuebox"){
    data <- lapply(seq_len(customOptions$count), function(i){
      valueBoxOutput(ns("valBox" %+% i),
                     width = if(is.null(customOptions$width)) 4 else customOptions$width)
    })
  }else if(type == "miropivot"){
    data <- miroPivotOutput(ns("miroPivot"), height = height, options = customOptions)
  }else{
    tryCatch({
      customOutput <- match.fun(typeCustom %+% "Output")
    }, error = function(e){
      stop(sprintf("An output function for the custom renderer: '%s' was not found. 
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
    })
    data <- customOutput(ns("custom"), height = height, options = customOptions,
                         path = customRendererDir)
    }
  return(tagList(
    if(showNoDataTxt) tags$div(id = ns("noData"), class = "out-no-data",
                                if(!createdDynamically) lang$nav$outputScreen$boxResults$noData),
    tags$div(id = ns("data"), style = if(createdDynamically) "" else "display:none", data)
  ))
}

renderData <- function(input, output, session, data, type, configData = NULL, dtOptions = NULL, 
                       graphOptions = NULL, pivotOptions = NULL, customOptions = NULL, 
                       roundPrecision = 2, modelDir = NULL, rendererEnv = NULL, views = NULL){
  if(!is.null(graphOptions)){
    graphTool <- graphOptions$tool
  }
  if(!length(type))
    type <- "datatable"
  if(inherits(data, "data.frame")){
    if(!length(data) || identical(nrow(data), 0L)){
      showEl(session, "#" %+% session$ns("noData"))
      hideEl(session, "#" %+% session$ns("data"))
      return()
    }else{
      showEl(session, "#" %+% session$ns("data"))
      hideEl(session, "#" %+% session$ns("noData"))
    }
  }else{
    if(!length(data) || !length(data[[1]]) || identical(nrow(data[[1]]), 0L)){
      showEl(session, "#" %+% session$ns("noData"))
      hideEl(session, "#" %+% session$ns("data"))
      return()
    }else{
      showEl(session, "#" %+% session$ns("data"))
      hideEl(session, "#" %+% session$ns("noData"))
    }
  }
  
  # make output type case insensitive
  typeCustom <- type
  type <- tolower(type)
  filterCol <- NULL
  if(length(graphOptions$filter) && graphOptions$filter$col %in% names(data)){
    showEl(session, "#" %+% session$ns("data_filter_wrapper"))
    filterCol <- as.name(graphOptions$filter$col)
    if(isTRUE(graphOptions$filter$date)){
      choices <- data[[graphOptions$filter$col]]
      updateDateRangeInput(session, "data_filter", min = choices[1], 
                           max = choices[length(choices)], 
                           start = choices[1], end = choices[length(choices)])
    }else{
      choices <- data[[graphOptions$filter$col]]
      updateSelectInput(session, "data_filter", choices = choices, 
                        selected = choices[1])
    }
  }
  if(type == "pivot"){
    output$pivottable <- renderPivot(data, options = pivotOptions, roundPrecision = roundPrecision)
  }else if(type == "graph"){
    output$graph <- renderGraph(data, configData = configData, options = graphOptions, 
                                input = input, filterCol = if(!is.null(filterCol)) filterCol)
  }else if(type == "datatable" || type == "dtgraph"){
    output$datatable <- renderDTable(data, options = dtOptions, roundPrecision = roundPrecision)
    if(!is.null(graphOptions)){
      output$graph <- renderGraph(data, configData = configData, options = graphOptions,
                                  input = input, filterCol = if(!is.null(filterCol)) filterCol)
    }
  }else if(type == "valuebox"){
    lapply(seq_len(min(length(data[[1]]), customOptions$count)), function(i){
      scalarDataTmp <- suppressWarnings(as.numeric(data[[3]][[i]]))
      if(is.na(scalarDataTmp)){
        scalarData <- data[[3]][[i]]
      }else{
        scalarData <- formatC(round(scalarDataTmp, roundPrecision), digits = roundPrecision, 
                              big.mark = " ", big.interval = 3, format = "f", drop0trailing = TRUE)
      }
      output[["valBox" %+% i]] <- renderValueBox({
        valueBox(
          scalarData, data[[2]][[i]], 
          getIcon(customOptions$icon$name, customOptions$icon$lib),
          if(identical(customOptions$color, NULL)) "aqua" else customOptions$color
        )
      })
    })
  }else if(type == "miropivot"){
    renderMiroPivot("miroPivot", data, options = customOptions, 
                    roundPrecision = roundPrecision, 
                    rendererEnv = rendererEnv, views = views)
  }else{
    tryCatch({
      customRenderer <- match.fun(paste0("render", toupper(substr(typeCustom, 1, 1)),
                                         substr(typeCustom, 2, nchar(typeCustom))))
    }, error = function(e){
      stop(sprintf("A custom renderer function: '%s' was not found. 
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
    })
    tryCatch({
      callModule(customRenderer, "custom", data, options = customOptions, 
                 path = customRendererDirs[[2L]], rendererEnv = rendererEnv, views = views)
    }, error = function(e){
      stop(sprintf("An error occured in the custom renderer function: '%s'. Error message: %s.", typeCustom, e), call. = FALSE)
    })
  }
  
}