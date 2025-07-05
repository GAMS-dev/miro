mirorenderer_report_outputOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("hour"), "Hour:",
      min = 0, max = 23,
      value = 0, step = 1,
      animate = animationOptions(
        interval = 1000, loop = FALSE,
        playButton = actionButton("play", "Play", icon = icon("play"), style = "margin-top: 10px;"),
        pauseButton = actionButton("pause", "Pause", icon = icon("pause"), style = "margin-top: 10px;")
      )
    ),
    # since plotly is a custom package, it is not attached by MIRO to avoid name collisions
    # Thus, we have to prefix functions exported by plotly via the "double colon operator":
    # plotly::renderPlotly
    plotly::plotlyOutput(ns("sankey"), height = "100%")
  )
}

renderMirorenderer_report_output <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...) {
  # since renderPlotly (or any other render function) is also an observer we are already in an reactive context
  output$sankey <- plotly::renderPlotly({
    hour_to_display <- sprintf("hour%02d", input$hour)

    # start with empty lists for the sankey links
    sankey_source <- list()
    sankey_target <- list()
    sankey_value <- list()

    # since the GAMS output is melted, first need to extract the different power sources
    battery_to_display <- filter(data, power_output_header == "battery") %>%
      filter(j == hour_to_display)
    gen_to_display <- filter(data, power_output_header == "generators") %>%
      filter(j == hour_to_display)
    extern_to_display <- filter(data, power_output_header == "external_grid") %>%
      filter(j == hour_to_display)

    # go over each source and check if they exist and if so add the corresponding link
    if (dim(battery_to_display)[1] != 0) {
      # for the battery need to check if is charged, or discharged
      if (battery_to_display[["value"]] > 0) {
        sankey_source <- c(sankey_source, 0)
        sankey_target <- c(sankey_target, 3)
        sankey_value <- c(sankey_value, battery_to_display[["value"]])
      } else {
        sankey_source <- c(sankey_source, 3)
        sankey_target <- c(sankey_target, 0)
        sankey_value <- c(sankey_value, -battery_to_display[["value"]])
      }
    }

    if (dim(gen_to_display)[1] != 0) {
      sankey_source <- c(sankey_source, 1)
      sankey_target <- c(sankey_target, 3)
      sankey_value <- c(sankey_value, gen_to_display[["value"]])
    }

    if (dim(extern_to_display)[1] != 0) {
      sankey_source <- c(sankey_source, 2)
      sankey_target <- c(sankey_target, 3)
      sankey_value <- c(sankey_value, extern_to_display[["value"]])
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
        value = sankey_value,
        color = "#bdbdbd"
      )
    ) %>% plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  })
}
