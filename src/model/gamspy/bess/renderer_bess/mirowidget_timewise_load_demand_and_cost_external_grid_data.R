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
  init <- FALSE
  rv <- reactiveValues(
    timewise_input_data = NULL
  )

  observe({
    rv$timewise_input_data <- data()
  })

  tableProxy <- DT::dataTableProxy("table")

  resetTable <- function() {
    DT::replaceData(tableProxy, isolate(rv$timewise_input_data), resetPaging = FALSE, rownames = FALSE)
  }

  observe({
    input$table_cell_edit
    if (!init) {
      init <<- TRUE
      return()
    }
    row <- input$table_cell_edit$row
    clmn <- input$table_cell_edit$col + 1

    if (input$table_cell_edit$value == "") {
      resetTable()
      return()
    }

    isolate({
      rv$timewise_input_data[row, clmn] <- input$table_cell_edit$value
    })
  })

  output$table <- DT::renderDT({
    DT::datatable(rv$timewise_input_data, editable = TRUE, rownames = FALSE, options = list(scrollX = TRUE)) %>%
      DT::formatRound(c("cost_external_grid"), digits = 2L)
  })

  output$timeline <- renderPlot({
    x <- rv$timewise_input_data[["j"]]
    y1 <- rv$timewise_input_data[["load_demand"]]
    y2 <- rv$timewise_input_data[["cost_external_grid"]]

    par(mar = c(5, 4, 4, 5))
    # First plot
    plot(y1,
      type = "l", col = "green",
      ylab = "Load demand in W", lwd = 3, xlab = "", xaxt = "n", las = 2
    )
    points(y1, col = "green", pch = 16, cex = 1.5)
    grid()

    # Add second plot on the same graph
    par(new = TRUE) # Overlay a new plot
    plot(y2,
      type = "l", col = "blue",
      axes = FALSE, xlab = "", ylab = "", lwd = 3
    ) # Suppress axes and labels
    points(y2, col = "blue", pch = 16, cex = 1.5)
    axis(side = 1, at = 1:length(x), labels = x, las = 2)
    # Add a new y-axis on the right for the second line
    axis(side = 4, las = 2) # Right side
    mtext("External grid cost in $", side = 4, line = 3) # Label for the right y-axis
    grid()
    # Add a legend
    legend("topleft",
      legend = c("Load demand", "External grid cost"),
      col = c("green", "blue"), lty = 1, lwd = 2, pch = 16
    )
  })

  return(reactive({
    rv$timewise_input_data
  }))
}
