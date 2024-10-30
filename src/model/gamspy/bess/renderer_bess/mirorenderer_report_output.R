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
    # since plotly is a added package need to specify it directly
    plotly::plotlyOutput(ns("sankey"), height = "100%")
  )
}

renderMirorenderer_report_output <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...) {
  # since renderPlotly (or any render) is also an observer we are already in an reactive context
  output$sankey <- plotly::renderPlotly({
    hour_to_dislay <- sprintf("hour%02d", input$hour)

    # start with empty lists for the sankey links
    sankey_source <- list()
    sankey_target <- list()
    sankey_value <- list()

    # since the GAMS output is melted, first need to extract the differen power sources
    battery_to_dislay <- filter(data, power_output_header == "battery") %>%
      filter(j == hour_to_dislay)
    gen_to_dislay <- filter(data, power_output_header == "generators") %>%
      filter(j == hour_to_dislay)
    extern_to_dislay <- filter(data, power_output_header == "external_grid") %>%
      filter(j == hour_to_dislay)

    # go over each source and check if they exist and if so add the corresponding link
    if (dim(battery_to_dislay)[1] != 0) {
      # for the battery need to check if is charged, or discharged
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

    # finally generate the sankey diagram using plotly
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
