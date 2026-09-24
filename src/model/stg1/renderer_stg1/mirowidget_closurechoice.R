mirowidget_closurechoiceOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "custom-widget",
      fluidRow(
        id = ns("assumptions"), class = "display-flex valueboxes custom-flex-row", style = "margin:0px;",
        column(12,
          class = "custom-block col-xs-12 col-sm-12 col-md-6 col-lg-4",
          tags$div(
            class = "radio-block",
            tags$h4("Government financing", class = "highlight-block"),
            radioButtons(
              ns("closurechoice"),
              label = widgetTooltip(
                "Government financing method",
                "Determines whether any decrease (increase) in government revenue is financed by increasing (decreasing) borrowing or raising (lowering) taxes"
              ),
              choiceNames = c("Borrowing", "Income Tax"),
              choiceValues = c(1, 2),
              selected = 1
            )
          )
        ),
        column(12,
          class = "custom-block col-xs-12 col-sm-12 col-md-6 col-lg-4",
          tags$div(
            actionButton(ns("resetAssumptions"), "Reset settings to default", class = "btn-custom custom-2")
          )
        )
      )
    )
  )
}

renderMirowidget_closurechoice <- function(input, output, session, data, options = NULL,
                                           path = NULL, rendererEnv = NULL, views = NULL,
                                           attachments = NULL, outputScalarsFull = NULL, ...) {
  rv <- reactiveValues(
    closurechoice = character(0)
  )

  observe({
    rv$closurechoice <<- data()

    isolate({
      updateRadioButtons(session, "closurechoice",
        selected = if (length(rv$closurechoice)) {
          as.numeric(rv$closurechoice)
        } else {
          1
        }
      )
    })
  })

  observeEvent(input$closurechoice, {
    if (!identical(as.numeric(input$closurechoice), as.numeric(rv$closurechoice))) {
      rv$closurechoice <- input$closurechoice
    }
  })

  observeEvent(input$resetAssumptions, {
    updateRadioButtons(session, "closurechoice", selected = 1)
  })

  return(
    reactive({
      rv$closurechoice
    })
  )
}
