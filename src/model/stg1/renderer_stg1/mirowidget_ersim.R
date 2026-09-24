mirowidget_ersimOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "custom-widget",
      fluidRow(
        id = ns("valueboxes"), class = "display-flex valueboxes custom-flex-row", style = "margin:0px;",
        column(12,
          class = "custom-block col-xs-12 col-sm-12 col-md-6 col-lg-4",
          tags$div(
            class = "slider-block",
            tags$h4("Scenario: World import prices", class = "highlight-block"),
            tags$h6("Change in world import prices (1 = no change)", class = "highlight-block"),
            sliderInput(
              ns("pwmsim_agr"),
              label = widgetTooltip(
                "Agricultural import prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwmsim_nat"),
              label = widgetTooltip(
                "Mineral import prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwmsim_fd"),
              label = widgetTooltip(
                "Food import prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwmsim_ind"),
              label = widgetTooltip(
                "Industrial import prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwmsim_ser"),
              label = widgetTooltip(
                "Other service import prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            )
          )
        ),
        column(12,
          class = "custom-block col-xs-12 col-sm-12 col-md-6 col-lg-4",
          tags$div(
            class = "slider-block",
            tags$h4("Scenario: World export prices", class = "highlight-block"),
            tags$h6("Change in world export prices (1 = no change)", class = "highlight-block"),
            sliderInput(
              ns("pwesim_agr"),
              label = widgetTooltip(
                "Agricultural export prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwesim_nat"),
              label = widgetTooltip(
                "Mineral export prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwesim_fd"),
              label = widgetTooltip(
                "Food export prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwesim_ind"),
              label = widgetTooltip(
                "Industrial export prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            ),
            sliderInput(
              ns("pwesim_ser"),
              label = widgetTooltip(
                "Service export prices",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.9,
              max = 1.1,
              value = 1,
              step = 0.02
            )
          )
        ),
        column(12,
          class = "custom-block col-xs-12 col-sm-12 col-md-12 col-lg-4",
          tags$div(
            class = "column-inner custom-flex-row",
            column(12,
              class = "custom-block col-xs-12 col-sm-12 col-md-6 col-lg-12",
              tags$div(
                class = "slider-block",
                tags$h4("Exchange rate", class = "highlight-block"),
                tags$h6("(1 = no change)", class = "highlight-block"),
                sliderInput(
                  ns("ersim"),
                  label = widgetTooltip(
                    "Change in the exchange rate",
                    "Exchange rate is domestic currency per unit of foreign currency. Example: 0.95 = 5% appreciation, 1.05 = 5% depreciation"
                  ),
                  min = 0.9,
                  max = 1.1,
                  value = 1,
                  step = 0.02
                )
              )
            )
          ),
          tags$div(
            class = "custom-button",
            actionButton(ns("resetAll"), "Reset settings to default", class = "btn-custom custom-2")
          )
        )
      )
    )
  )
}

renderMirowidget_ersim <- function(input, output, session, data, options = NULL,
                                   path = NULL, rendererEnv = NULL, views = NULL,
                                   attachments = NULL, outputScalarsFull = NULL, ...) {
  # all symbols
  keys <- c(
    "ersim",
    "pwmsim_agr", "pwmsim_nat", "pwmsim_fd", "pwmsim_ind", "pwmsim_ser",
    "pwesim_agr", "pwesim_nat", "pwesim_fd", "pwesim_ind", "pwesim_ser"
  )

  # numeric, fall back to 1 when empty
  toNumOrDefault <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    if (length(num) && !all(is.na(num))) num else 1
  }

  # initialize reactive store
  rv <- do.call(reactiveValues, setNames(rep(list(numeric(0)), length(keys)), keys))

  # sync from data -> rv and update sliders (defaulting to 1 if empty)
  observe({
    vals <- stats::setNames(lapply(keys, function(k) data[[k]]()), keys)
    for (k in keys) {
      rv[[k]] <- vals[[k]]
      updateSliderInput(session, k, value = toNumOrDefault(vals[[k]]))
    }
  })

  # reset-all button
  observeEvent(input$resetAll,
    {
      for (k in keys) updateSliderInput(session, k, value = 1)
    },
    ignoreInit = TRUE
  )

  # keep rv in sync with user input
  lapply(keys, function(k) {
    observeEvent(input[[k]],
      {
        newVal <- input[[k]]
        if (!identical(as.numeric(newVal), as.numeric(rv[[k]]))) {
          rv[[k]] <- newVal
        }
      },
      ignoreInit = TRUE
    )
  })

  # return named list of reactives
  out <- stats::setNames(lapply(keys, function(k) reactive(rv[[k]])), keys)
  return(out)
}
