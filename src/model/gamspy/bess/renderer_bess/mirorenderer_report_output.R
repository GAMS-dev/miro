mirorenderer_report_outputOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("hour"), "Hour:",
      min = 0, max = 23,
      value = 0, step = 1,
      animate = animationOptions(
        interval = 1000, loop = FALSE,
        playButton = actionButton("play", "Play", icon = icon("play"), width = "100px", style = "margin-top: 10px; color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        pauseButton = actionButton("pause", "Pause", icon = icon("pause"), width = "100px", style = "margin-top: 10px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
      )
    ),
    plotly::plotlyOutput(ns("sankey"), height = "100%")
  )
}

renderMirorenderer_report_output <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...) {
  output$sankey <- plotly::renderPlotly({
    hour_to_dislay <- sprintf("hour%03d", input$hour)
    sankey_source <- list()
    sankey_target <- list()
    sankey_value <- list()

    battery_to_dislay <- filter(data, power_output_header == "battery") %>%
      filter(j == hour_to_dislay)
    gen_to_dislay <- filter(data, power_output_header == "generators") %>%
      filter(j == hour_to_dislay)
    extern_to_dislay <- filter(data, power_output_header == "external_grid") %>%
      filter(j == hour_to_dislay)

    if (dim(battery_to_dislay)[1] != 0) {
      if (battery_to_dislay[["value"]] > 0) {
        sankey_source <- c(sankey_source, 0)
        sankey_target <- c(sankey_target, 3)
        sankey_value <- c(sankey_value, battery_to_dislay[["value"]])
      } else {
        sankey_source <- c(sankey_source, 3)
        sankey_target <- c(sankey_target, 0)
        sankey_value <- c(sankey_value, -battery_to_dislay[["value"]])
      }
    }

    if (dim(gen_to_dislay)[1] != 0) {
      sankey_source <- c(sankey_source, 1)
      sankey_target <- c(sankey_target, 3)
      sankey_value <- c(sankey_value, gen_to_dislay[["value"]])
    }

    if (dim(extern_to_dislay)[1] != 0) {
      sankey_source <- c(sankey_source, 2)
      sankey_target <- c(sankey_target, 3)
      sankey_value <- c(sankey_value, extern_to_dislay[["value"]])
    }

    plotly::plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = c("BESS", "Generators", "External Grid", "City"),
        color = c("blue", "green", "red", "black"),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = sankey_source,
        target = sankey_target,
        value =  sankey_value
      )
    )
  })
}
