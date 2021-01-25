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
  scalars <- data[["_scalars_out"]][[3]]
  raw_width <- as.numeric(scalars[1])
  rolls_used <- as.numeric(scalars[2])
  data <- data$patterns_used
  
  # Calculate total space of pattern each width uses
  data <- data %>% dplyr::mutate(total_width = times * width)
  
  # Create waste entry for every pattern
  data$p <- factor(data$p)
  new_entries <- data.frame(
    i = rep("Waste", length(unique(data$p))),
    p = levels(data$p),
    times = 1,
    used = purrr::map_dbl(
      levels(data$p), ~{
        data %>% dplyr::filter(p == .x) %>% dplyr::pull(used) %>% unique()
      }
    ),
    width = purrr::map_dbl(
      levels(data$p), ~ {
        raw_width - (data %>% dplyr::filter(p == .x) %>% dplyr::pull(total_width) %>% sum())
      }
    )
  )
  
  # New entries need a total_width column too
  new_entries$total_width <- new_entries$width
  
  # Bind original data and waste data together
  data <- rbind(data, new_entries)
  
  # Order patterns by width of waste
  order_by <- data[data$i == "Waste", c("p", "width")]
  patterns_order <- order(order_by$width)
  data$p <- factor(data$p, levels(data$p)[patterns_order])
  
  # Create a custom label for every width. To avoid a times hover info for
  # waste replace the label by a special label.
  data$hover_info <- paste0("Times used: ", data$times)
  data[data$i == "waste", "hover_info"] <- "(Waste)"
  data$legend <- factor(data$width, levels = c(unique(data$width[data$i != "Waste"]), "Waste"))
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
    fig <- plotly::plot_ly(data, x = ~width, y = ~p, color = ~legend, type = "bar", 
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
    
    # Need ticktext for y axis. Have to order by p
    y_ticktext <- data %>% dplyr::select(p, used) %>% unique()
    y_ticktext_vec <- y_ticktext %>% dplyr::pull(used)
    names(y_ticktext_vec) <- y_ticktext$p
    
    # Change layout
    fig <- fig %>% plotly::layout(
      barmode = "stack",
      title = "Patterns used",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(b = 100),
      xaxis = list(title = ""),
      yaxis = list(
        title = "", 
        tickvals = y_ticktext$p, 
        ticktext = y_ticktext$used,
        tickmode = "array",
        categoryarray = levels(data$p),
        categoryorder = "array"
      ),
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