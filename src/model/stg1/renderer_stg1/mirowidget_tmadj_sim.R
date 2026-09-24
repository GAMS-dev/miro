mirowidget_tmadj_simOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "custom-widget",
      fluidRow(
        id = ns("taxpolicy"), class = "display-flex valueboxes custom-flex-row adjust-height", style = "margin:0px;",
        column(12,
          class = "custom-block custom-block-left col-xs-12 col-sm-12 col-md-6 col-lg-4",
          tags$div(
            class = "slider-block",
            tags$h4("Scenario: Tax policy", class = "highlight-block"),
            tags$h6("Change in tax policies (1 = no change)", class = "highlight-block"),
            sliderInput(
              ns("tmadj_sim"),
              label = widgetTooltip(
                "Import tariffs",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.5,
              max = 1.5,
              value = 1,
              step = 0.01
            ),
            sliderInput(
              ns("tsadj_sim"),
              label = widgetTooltip(
                "Sales taxes",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.5,
              max = 1.5,
              value = 1,
              step = 0.01
            ),
            sliderInput(
              ns("texadj_sim"),
              label = widgetTooltip(
                "Excise taxes",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.5,
              max = 1.5,
              value = 1,
              step = 0.01
            ),
            sliderInput(
              ns("txadj_sim"),
              label = widgetTooltip(
                "Production taxes",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.5,
              max = 1.5,
              value = 1,
              step = 0.01
            )
          )
        ),
        column(12,
          class = "custom-block custom-block-right col-xs-12 col-sm-12 col-md-6 col-lg-4",
          tags$div(
            class = "slider-block",
            tags$h4("Scenario: Tax policy", class = "highlight-block empty-header"),
            tags$h6("Change in tax policies (1 = no change)", class = "highlight-block empty-header"),
            sliderInput(
              ns("tfadj_sim"),
              label = widgetTooltip(
                "Factor use taxes",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.5,
              max = 1.5,
              value = 1,
              step = 0.01
            ),
            sliderInput(
              ns("tyhadj_sim"),
              label = widgetTooltip(
                "Household income taxes",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.5,
              max = 1.5,
              value = 1,
              step = 0.01
            ),
            sliderInput(
              ns("tyeadj_sim"),
              label = widgetTooltip(
                "Enterprise income tax",
                "Example: 0.98 = 2% reduction, 1.02 = 2% increase"
              ),
              min = 0.5,
              max = 1.5,
              value = 1,
              step = 0.01
            )
          )
        ),
        column(12,
          class = "custom-block col-xs-12 col-sm-12 col-md-12 col-lg-4",
          tags$div(
            actionButton(ns("resetAll"), "Reset to defaults", icon = icon("rotate-left"), class = "btn-custom custom-2")
          )
        )
      )
    )
  )
}

renderMirowidget_tmadj_sim <- function(input, output, session, data, options = NULL,
                                       path = NULL, rendererEnv = NULL, views = NULL,
                                       attachments = NULL, outputScalarsFull = NULL, ...) {
  # all symbols
  keys <- c(
    "tmadj_sim", "tsadj_sim", "texadj_sim", "txadj_sim",
    "tfadj_sim", "tyhadj_sim", "tyeadj_sim"
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
    vals <- setNames(lapply(keys, function(k) data[[k]]()), keys)
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
  out <- setNames(lapply(keys, function(k) reactive(rv[[k]])), keys)
  return(out)
}
