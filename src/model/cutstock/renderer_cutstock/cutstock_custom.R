cuttingstockOutput <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  # set default height
  if(is.null(height)){
    height <- 700
  } 
  tagList( 
    #define rendererOutput function here 
    plotly::plotlyOutput(ns("plot"))
  ) 
}

renderCuttingstock <- function(input, output, session, data, options = NULL, path = NULL, views = NULL, ...){ 
  
  #renderer 
  print(data)
  raw_width <- data$`_scalars_out`[1, "value"] %>% as.numeric()
  rolls_used <- data$`_scalars_out`[2, "value"] %>% as.numeric()
  data <- data$patterns_used
  
  # Use the time each pattern is used as an xlabel
  ylabel <- data %>% 
    dplyr::filter(ptuhdr == "used") %>% 
    dplyr::select(p, value)
  
  # Bring from wide to long
  data <- data %>% tidyr::spread(ptuhdr, value)
  
  # Calculate total space of pattern each width uses
  data <- data %>% dplyr::mutate(total_width = times * width)
  
  # Create waste entry for every pattern
  new_entries <- data.frame(
    i = rep("Waste", length(unique(data$p))),
    p = unique(data$p),
    times = 1,
    used = 1,
    width = purrr::map_dbl(
      unique(data$p), ~ {
        raw_width - data %>% dplyr::filter(p == .x) %>% pull %>% sum()
      }
    )
  )
  
  # New entries need a total_width column too
  new_entries$total_width <- new_entries$width
  
  # Bind original data and waste data together
  data <- rbind(data, new_entries)
  
  # The ylabel is replaced by the number of times a pattern is used.
  # waste_data <- data %>% dplyr::filter(i == "Waste")
  # levels <- waste_data[order(waste_data$width), ] %>% dplyr::pull(p)
  # labels <- waste_data[order(waste_data$width), ] %>% dplyr::pull(used)
  # data$ylabel <- factor(data$p, 
  #                       levels = levels,
  #                       labels = paste0(
  #                         labels, " "
  #                       )
  # )

  data$ylabel <- factor(data$p,
                        levels = unique(data$p),
                        labels = paste0(
                          unique(ylabel$value), " "
                        )
  )

  # Create a custom label for every width. To avoid a times hover info for
  # waste replace the label by a special label.
  data$hover_info <- paste0("Times used: ", data$times)
  data[data$i == "waste", "hover_info"] <- "(Waste)"
  data$legend <- paste0(data$i, ": ", data$width)
  data[data$i == "Waste", "legend"] <- "Waste"
  
  # Repeat a data entry every time a width is used in a pattern
  data <- data[rep(seq_len(nrow(data)), data$times), ]
  
  # Create colorpalette manually so the bar for waste is white
  number_colors <- data$legend %>% unique() %>% length()
  palette_name <- "Dark2"
  if(number_colors > 11){
    
    colors <- colorRampPalette(
      RColorBrewer::brewer.pal(
        n = 11,
        name = palette_name
      )
    )
    
  }else{
    
    colors <- RColorBrewer::brewer.pal(
      n = number_colors,
      name = palette_name
    )
    
  }
  
  names(colors) <- data$i %>% unique()
  colors["Waste"] <- "#FFFFFF"
  
  # Repeat colors for every time a width is used in a pattern
  colors <- rep(colors, table(data$i))
  
  # Calculate total length, total waste and percentage of waste
  waste_total <- data %>% filter(i == "Waste") %>% mutate(x = used * width) %>% pull(x) %>% sum(na.rm = TRUE)
  waste_relative <- (100 * waste_total / (raw_width * rolls_used)) %>% round(2)
  
  output$plot <- plotly::renderPlotly({
    
    # Create bars for waste first
    fig <- plotly::plot_ly(data, x = ~width, y = ~ylabel, color = ~legend, type = "bar", 
                           orientation = "h", hoverinfo = "text",
                           text = ~paste0(
                             "</br> Width: ", width,
                             "</br> ", hover_info
                             ),
                           marker = list(
                             line = list(
                               width = 2,
                               color = "rgb(0, 0, 0)"
                             ),
                             color = colors
                           )
    )
    
    fig <- fig %>% plotly::layout(
      barmode = "stack",
      title = "Patterns used",
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      annotations = list(
        list(
          text = paste0("Rolls used: ", rolls_used),
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          x = 0,
          y = -0.15
        ),
        list(
          text = paste0(
            paste0(
              c("Waste total", "Waste relative"),
              ": ",
              c(waste_total, paste0(waste_relative, " %"))
            ),
            collapse = "; "
          ),
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          x = 1,
          y = -0.15
        )
      )
    )
    
    return(fig)
    
  })
  
}