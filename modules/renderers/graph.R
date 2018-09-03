renderGraph <- function(data, config.data, options, height = NULL){
  # Renders the graph for a dataframe using the provided configuration.
  #
  # Args:
  #   data:                     dataframe that is to be plotted
  #   config.data:              data used for configuration of graphs
  #   options:                  options for customizing graph
  #   height:                   height of graph object (default: automatic sizing)
  #
  # Returns:
  #   rendered graph for the provided dataframe
  
  if(options$tool == 'plotly'){
    if(options$type == 'pie'){
      # set defaults (no axes in pie chart)
      # pie chart
      p <- plot_ly(data, labels = ~try(get(options$labels)), values = ~try(get(options$values)), type = 'pie', height = height)
      p <- layout(p, title = options$title, showlegend = options$showlegend,
                  xaxis = options$xaxis,
                  yaxis = options$yaxis)
    }else if(options$type == 'bar'){
      # bar plot
      p <- NULL
      lapply(seq_along(options$ydata), function(j){
        if(j==1){
          p <<- plot_ly(data, x = ~try(get(options$xdata)), y = ~try(get(options$ydata)), type = 'bar', name = options$ydata[[j]]$labels, height = height)
        }else{
          p <<- add_trace(p, y = ~try(get(options$ydata[[j]]$values)), name = options$ydata[[j]]$labels)
        }
      })
      p <- layout(p,title=options$title, barmode=options$barmode,
                  xaxis = list(title = options$xaxis$title, showgrid = options$xaxis$showgrid, zeroline = options$xaxis$zeroline, showticklabels = options$xaxis$showticklabels),
                  yaxis = list(title = options$yaxis$title, showgrid = options$yaxis$showgrid, zeroline = options$yaxis$zeroline, showticklabels = options$yaxis$showticklabels))
    }else if(options$type=='scatter'){
      # scatter plot
      p <- NULL
      lapply(seq_along(options$ydata), function(j){
        if(j==1){
          p <<- plot_ly(data, x = ~try(get(options$xdata)), y = ~try(get(names(options$ydata)[[1]])), name = options$ydata[[1]]$label, 
                        mode = options$ydata[[1]]$mode, marker=list(size = options$ydata[[1]]$marker$size,
                                                                    color = options$ydata[[1]]$marker$color,
                                                                    line = list(color = options$ydata[[1]]$marker$line$color,
                                                                                width = options$ydata[[1]]$marker$line$width)),
                        color = if(!is.null(options$color)){~try(get(options$color))}, symbol = if(!is.null(options$symbol)){~try(get(options$symbol))}, 
                        colors = options$colors, symbols = options$symbols, size = options$ydata[[1]]$size, type = 'scatter', height = height)
        }else{
          p <<- add_trace(p, y = ~try(get(names(options$ydata)[[j]])), name = options$ydata[[j]]$label, mode = options$ydata[[j]]$mode, 
                          marker = list(size = options$ydata[[j]]$marker$size,color = options$ydata[[j]]$marker$color,
                                        line = list(color = options$ydata[[j]]$marker$line$color, 
                                                    width = options$ydata[[j]]$marker$line$width)),
                          color = if(!is.null(options$ydata[[j]]$color)){~try(get(options$ydata[[j]]$color))}, 
                          symbol= if(!is.null(options$ydata[[j]]$symbol)){~try(get(options$ydata[[j]]$symbol))}, colors = options$ydata[[j]]$colors,
                          symbols = options$ydata[[j]]$symbols, size=options$ydata[[j]]$size)
        }
      })
      p <- layout(p, title = options$title, barmode = options$barmode, margin = options$margins,
                  xaxis = list(title = options$xaxis$title, showgrid = options$xaxis$showgrid, zeroline = options$xaxis$zeroline, showticklabels = options$xaxis$showticklabels),
                  yaxis = list(title = options$yaxis$title, showgrid = options$yaxis$showgrid, zeroline = options$yaxis$zeroline, showticklabels = options$yaxis$showticklabels))
    }else if(options$type == 'hist'){
      # histogram
      #first calculate the width of the bins
      minx <- min(data[, match(tolower(names(options$xdata)), tolower(colnames(data)))], na.rm = T)
      maxx <- max(data[, match(tolower(names(options$xdata)), tolower(colnames(data)))], na.rm = T)
      p <- NULL
      lapply(seq_along(options$xdata), function(j){
        if(j==1){
          p <<- plot_ly(data, type = 'histogram', histnorm = options$histnorm, height = height, autobinx = if(is.null(options$nbins)) T else F, 
                        xbins = list(start = minx, end = maxx, size = (maxx-minx)/options$nbins))
          p <<- add_histogram(p, x = ~try(get(names(options$xdata)[[j]])), name = options$xdata[[j]]$labels, marker = list(color = toRGB(options$xdata[[j]]$color, options$xdata[[j]]$alpha)))
        }else{
          p <<- add_histogram(p, x = ~try(get(names(options$xdata)[[j]])), name = options$xdata[[j]]$labels, marker = list(color = toRGB(options$xdata[[j]]$color, options$xdata[[j]]$alpha)))
        }
      })
      p <- layout(p, title = options$title, barmode = options$barmode, xaxis = list(title = options$xaxis$title), yaxis = list(title = options$yaxis$title))
    }else{
      stop("The plot type you selected is currently not supported for tool plotly.", call. = F)
    }
    
    return(plotly::renderPlotly(p))
    
  }else if(options$tool == 'dygraph'){
    # set defaults
    # time series chart
    p <- NULL
    lapply(seq_along(options$ydata), function(j){
      
      if(j==1){
        #check whether data is already correctly formatted and if y variables are labeled in config.json
        if(!is.null(options$color)){
          x <- as.symbol(options$xdata)
          y <- as.symbol(c(options$color))
          # bring data into right matrix format
          xts_data <- reshape2::acast(data, as.formula(paste(x, y, sep = "~")), value.var = names(options$ydata)[[j]]) 
          p <<- dygraph(xts_data, main = options$title, xlab = options$xaxis$title, ylab = options$yaxis$title,  periodicity = NULL, group = NULL, elementId = NULL)
        }else{
          idx.vector <- match(tolower(names(options$ydata)), tolower(colnames(data)))
          xts_data <- as.matrix(data[, idx.vector])
          row.names(xts_data) <- as.character(data[[1]])
          p <<- dygraph(xts_data, main = options$title, xlab = options$xaxis$title, ylab = options$yaxis$title,  periodicity = NULL, group = NULL, elementId = NULL)
          p <<- dySeries(p, name = names(options$ydata)[[j]], label = options$ydata[[j]]$label, color = options$ydata[[j]]$color, axis = "y",
                         stepPlot = options$ydata[[j]]$stepPlot, stemPlot = options$ydata[[j]]$stemPlot, fillGraph = options$ydata[[j]]$fillGraph, drawPoints = options$ydata[[j]]$drawPoints,
                         pointSize = options$ydata[[j]]$pointSize, strokeWidth = options$ydata[[j]]$strokeWidth, strokePattern = options$ydata[[j]]$strokePattern,
                         strokeBorderWidth = options$ydata[[j]]$strokeBorderWidth, strokeBorderColor = options$ydata[[j]]$strokeBorderColor)
          }
        
      }else{
        p <<- dySeries(p, name = names(options$ydata)[[j]], label = options$ydata[[j]]$label, color = options$ydata[[j]]$color, axis = "y",
                       stepPlot = options$ydata[[j]]$stepPlot, stemPlot = options$ydata[[j]]$stemPlot, fillGraph = options$ydata[[j]]$fillGraph, drawPoints = options$ydata[[j]]$drawPoints,
                       pointSize = options$ydata[[j]]$pointSize, strokeWidth = options$ydata[[j]]$strokeWidth, strokePattern = options$ydata[[j]]$strokePattern,
                       strokeBorderWidth = options$ydata[[j]]$strokeBorderWidth, strokeBorderColor = options$ydata[[j]]$strokeBorderColor)
      }
    })
    # add graph options specified in config.json
    if(!is.null (options$dyOptions)){
      p <- do.call(dyOptions, c(list(dygraph = p), options$dyOptions))
    }
    # lenged options
    if(!is.null (options$dylegend)){
      p <- do.call(dyLegend, c(list(dygraph = p), options$dylegend))
    }
    # highlighting options - highlight hovered series
    if(!is.null (options$dyHighlight)){
      p <- do.call(dyHighlight, c(list(dygraph = p), options$dyHighlight))
    }
    # use a selector for panning and zooming
    if(!is.null (options$dyRangeSelector)){
      p <- do.call(dyRangeSelector, c(list(dygraph = p), options$dyRangeSelector))
    }
    # Candlestick charts: use the first four data series to plot, the rest of the data series (if any) are rendered with line plotter.
    if(!is.null (options$dyCandlestick)){
      p <- do.call(dyCandlestick, c(list(dygraph = p), options$dyCandlestick))
    }
    # Event lines to note points within a time series. 
    if(!is.null (options$dyEvent)){
      lapply(seq_along(names(options$dyEvent)), function(j){
        event <- getEvent(config.data, names(options$dyEvent)[[j]])
        p <<- do.call(dyEvent, c(list(dygraph = p, x = event), options$dyEvent[[j]]))
      })
    }
    # Add a shading effect to the graph background for one or more time ranges. 
    if(!is.null (options$dyShading)){
      lapply(seq_along(options$dyShading), function(j){
        p <<- do.call(dyShading, c(list(dygraph = p), options$dyShading[[j]]))
      })
    }
    return(dygraphs::renderDygraph(p))
  }else{
    stop("The tool you selected for plotting graphs is not currently supported.", call. = F)
  }
}
getEvent <- function(configData, eventId){
  # extracts event from config.data
  #
  # args:
  # configData :     configuration dataframe
  # eventId    :     id of the event
  #
  # returns:
  # string with event information extracted from config.data
  
  if(!is.null(configData)){
    idx <- match(tolower(eventId), tolower(configData[[1]][[1]]))
    if(is.na(idx)){
      # index could not be found so return the string (fixed value)
      return(eventId)
    }else{
      return(configData[[3]][[idx]])
    }
  }else{
    # config data does not exist, so return string
    return(eventId)
  }
}