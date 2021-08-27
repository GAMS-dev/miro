mirorenderer_patterns_usedOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  if (is.null(height)) {
    height <- 700
  }
  tagList(plotly::plotlyOutput(ns("plot")))
}

renderMirorenderer_patterns_used <- function(input, output, session, data, options = NULL, path = NULL, views = NULL, ...) {
  scalars <- data[["_scalars_out"]][[3]]
  raw_width <- as.numeric(scalars[1])
  rolls_used <- as.numeric(scalars[2])
  data <- data$patterns_used
  data <- data %>% dplyr::mutate(total_width = times * width)
  data$p <- factor(data$p)
  new_entries <- data.frame(
    i = rep("Waste", length(unique(data$p))),
    p = levels(data$p), times = 1, used = purrr::map_dbl(
      levels(data$p),
      ~ {
        data %>%
          dplyr::filter(p == .x) %>%
          dplyr::pull(used) %>%
          unique()
      }
    ), width = purrr::map_dbl(levels(data$p), ~ {
      raw_width - (data %>% dplyr::filter(p == .x) %>%
        dplyr::pull(total_width) %>% sum())
    })
  )
  new_entries$total_width <- new_entries$width
  data <- rbind(data, new_entries)
  order_by <- data[data$i == "Waste", c("p", "width")]
  patterns_order <- order(order_by$width)
  data$p <- factor(data$p, levels(data$p)[patterns_order])
  data$hover_info <- paste0("Times used: ", data$times)
  data[data$i == "waste", "hover_info"] <- "(Waste)"
  data$legend <- factor(data$width, levels = c(unique(data$width[data$i !=
    "Waste"]), "Waste"))
  data[data$i == "Waste", "legend"] <- "Waste"
  data <- data[rep(seq_len(nrow(data)), data$times), ]
  number_colors <- data$legend %>%
    unique() %>%
    length()
  palette_name <- "Dark2"
  if (number_colors > 11) {
    colors <- colorRampPalette(RColorBrewer::brewer.pal(
      n = 11,
      name = palette_name
    ))
  } else {
    colors <- RColorBrewer::brewer.pal(
      n = number_colors,
      name = palette_name
    )
  }
  names(colors) <- data$i %>% unique()
  colors["Waste"] <- "#FFFFFF"
  colors <- rep(colors, table(data$i))
  waste_total <- data %>%
    filter(i == "Waste") %>%
    mutate(x = used *
      width) %>%
    pull(x) %>%
    sum(na.rm = TRUE)
  waste_relative <- (100 * waste_total / (raw_width * rolls_used)) %>%
    round(2)
  output$plot <- plotly::renderPlotly({
    fig <- plotly::plot_ly(data,
      x = ~width, y = ~p, color = ~legend,
      type = "bar", orientation = "h", hoverinfo = "text",
      text = ~ paste0(
        "</br> Width: ", width, "</br> ",
        hover_info
      ), marker = list(line = list(
        width = 2,
        color = "rgb(0, 0, 0)"
      ), color = colors)
    )
    y_ticktext <- data %>%
      dplyr::select(p, used) %>%
      unique()
    y_ticktext_vec <- y_ticktext %>% dplyr::pull(used)
    names(y_ticktext_vec) <- y_ticktext$p
    fig <- fig %>% plotly::layout(
      barmode = "stack", title = "Patterns used",
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(b = 100), xaxis = list(title = ""),
      yaxis = list(
        title = "", tickvals = y_ticktext$p,
        ticktext = y_ticktext$used, tickmode = "array",
        categoryarray = levels(data$p), categoryorder = "array"
      ),
      annotations = list(list(
        text = paste0(
          "Rolls used: ",
          rolls_used
        ), showarrow = FALSE, xref = "paper",
        yref = "paper", x = 0, y = -0.15
      ), list(
        text = paste0(paste0(c(
          "Waste total",
          "Waste relative"
        ), ": ", c(waste_total, paste0(
          waste_relative,
          " %"
        ))), collapse = "; "), showarrow = FALSE,
        xref = "paper", yref = "paper", x = 1, y = -0.15
      ))
    )
    return(fig)
  })
}
