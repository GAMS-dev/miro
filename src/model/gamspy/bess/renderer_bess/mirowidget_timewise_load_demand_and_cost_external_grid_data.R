mirowidget_timewise_load_demand_and_cost_external_grid_dataOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      plotOutput(ns("timeline"))
    ),
    column(
      width = 12,
      DT::DTOutput(ns("table"))
    )
  )
}

renderMirowidget_timewise_load_demand_and_cost_external_grid_data <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...) {
  # The whole code is run at the beginning, even though no actions are performed yet.
  # init is used to only perfom action in observe() after this initial run.
  # Therfore, it is set to TRUE in the last occuring observe()
  init <- FALSE
  rv <- reactiveValues(
    timewise_input_data = NULL
  )

  # set the initial data
  observe({
    rv$timewise_input_data <- data()
  })

  tableProxy <- DT::dataTableProxy("table")

  resetTable <- function() {
    DT::replaceData(tableProxy, isolate(rv$timewise_input_data), resetPaging = FALSE, rownames = FALSE)
  }

  # observe if the table is edited
  observe({
    input$table_cell_edit
    if (!init) {
      init <<- TRUE
      return()
    }
    row <- input$table_cell_edit$row
    # need to add one since the first column is the index
    clmn <- input$table_cell_edit$col + 1

    # if the new value would be empty restore the value from before
    if (input$table_cell_edit$value == "") {
      resetTable()
      return()
    }

    # else update the corresponding value in the reaciveValue 
    isolate({
      rv$timewise_input_data[row, clmn] <- input$table_cell_edit$value
    })
  })

  # return the render for the placeholder "table"
  output$table <- DT::renderDT({
    DT::datatable(rv$timewise_input_data, editable = TRUE, rownames = FALSE, options = list(scrollX = TRUE)) %>%
      DT::formatRound(c("cost_external_grid"), digits = 2L)
  })

  # return the render for the placeholder "timeline"
  output$timeline <- renderPlot({
    # first extract all the needed information
    x <- rv$timewise_input_data[["j"]]
    y1 <- rv$timewise_input_data[["load_demand"]]
    y2 <- rv$timewise_input_data[["cost_external_grid"]]

    # set the margin for the graph
    par(mar = c(5, 4, 4, 5))

    # first plot the load demand
    plot(y1,
      type = "l", col = "green",
      ylab = "Load demand in W", lwd = 3, xlab = "", xaxt = "n", las = 2
    )
    points(y1, col = "green", pch = 16, cex = 1.5)
    grid()

    # add second plot on the same graph for the external cost
    par(new = TRUE) # overlay a new plot
    plot(y2,
      type = "l", col = "blue",
      axes = FALSE, xlab = "", ylab = "", lwd = 3
    )
    points(y2, col = "blue", pch = 16, cex = 1.5)

    # add a new y-axis on the right for the second line
    axis(side = 4, las = 2) 
    mtext("External grid cost in $", side = 4, line = 3) 
    grid()

    # add the x values to the axis
    axis(side = 1, at = 1:length(x), labels = x, las = 2)

    legend("topleft",
      legend = c("Load demand", "External grid cost"),
      col = c("green", "blue"), lty = 1, lwd = 2, pch = 16
    )
  })

  # since this is an input, need to return the final data
  return(reactive({
    rv$timewise_input_data
  }))
}
