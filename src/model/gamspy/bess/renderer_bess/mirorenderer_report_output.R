mirorenderer_report_outputOutput <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
    tagList(
        plotly::plotlyOutput(ns("sankey")),
        sliderInput(ns("hour"), "Hour:",
                  min = 0, max = 24,
                  value = 0, step = 1)
    )
}

renderMirorenderer_report_output <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){
all_powers <- data
print(filter(all_powers, power_output_header == "load_demand"))

output$sankey <- plotly::renderPlotly({
    plotly::plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = c("A1", "A2", "B1", "B2", "C1", "C2"),
          color = c("blue", "blue", "blue", "blue", "blue", "blue"),
          pad = 15,
          thickness = 20,
          line = list(
            color = "black",
            width = 0.5
          )
        ),
        link = list(
          source = c(0,1,0,2,3,3),
          target = c(2,3,3,4,4,5),
          value =  c(8,4,2,8,4,2)
    )
  )
})
}
