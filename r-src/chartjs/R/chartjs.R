#' chartjs
#'
#' Draw a graph using the Chart.js library.
#'
#' @param width the width of the widget
#' @param height the height of the widget
#' @param palette either the name of a RColorBrewer palette or a list of custom colours as specified in \code{\link{colorTypes}}
#' @param customColors character vector with colors (overwrites palette argument)
#' @param debug if TRUE, prints information to the JS console
#'
#' @import htmlwidgets
#' @import htmltools
#'
#' @export
chartjs <- function(width = NULL, height = NULL, palette = "Paired", customColors = NULL,
                    title = NULL, debug = FALSE) {
  chartOptions <- baseOptions()

  x = list(options = chartOptions,
           customColors = customColors,
           palette = palette,
           debug = debug)
  if(!is.null(title)){
    x$options$plugins$title <- list(display = TRUE, text = title,
                                    font = list(size = 20))
  }

  # Create widget
  htmlwidgets::createWidget(
    name = 'chartjs',
    x = x,
    width = width,
    height = height,
    package = 'chartjs'
  )
}

chartjs_html <- function(id, class, ...){
  # Inside a div to ensure flexdashboards sees the widget
  tags$div(id = paste0(id, "-container"), class = "htmlwidget_container miro-pivot-height", tags$canvas(id = id, class = class, ...))
}
