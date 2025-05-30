#' Draw charts
#'
#' These functions are used to draw the different types of graphs.
#'
#' @param chartjs a \code{\link{chartjs}} object
#' @param labels a vector of characters
#' @name charts
NULL

#' @rdname charts
#' @param stacked a logical, defaults to FALSE. If TRUE, bars will be stacked
#' at each x value.
#' @export
cjsBar <- function(chartjs, labels, stacked = FALSE, xTitle = NULL, yTitle = NULL){
  cjs_base_chart(chartjs, labels, "bar") %>%
    cjsAddScale("x", "category", stacked = stacked,
                title = if(!is.null(xTitle)) list(text = xTitle, display = TRUE)) %>%
    cjsAddScale("y", "linear", stacked = stacked,
      ticks = list(beginAtZero = TRUE),
      title = if(!is.null(yTitle)) list(text = yTitle, display = TRUE))
}

#' @export
cjsHorizontalBar <- function(chartjs, labels, stacked = FALSE){
  cjs_base_chart(chartjs, labels, "horizontalBar") %>%
    cjsAddScale("x", "linear", stacked = stacked) %>%
    cjsAddScale("y", "category", stacked = stacked)
}

#' @rdname charts
#' @export
cjsLine <- function(chartjs, labels, xTitle = NULL, yTitle = NULL){
  cjs_base_chart(chartjs, labels, "line") %>%
    cjsAddScale("x", "category", title = if(!is.null(xTitle)) list(text = xTitle, display = TRUE)) %>%
    cjsAddScale("y", "linear", title = if(!is.null(yTitle)) list(text = yTitle, display = TRUE))
}

#' @rdname charts
#' @export
cjsPie <- function(chartjs, labels){
  cjs_base_chart(chartjs, labels, "pie")
}

#' @rdname charts
#' @param cutout the optional percentage of the inner cutout for Doughnut
#' charts (defaults to 50)
#' @export
cjsDoughnut <- function(chartjs, labels, cutout = 50){
  chartjs$x$options$cutout <- cutout
  cjs_base_chart(chartjs, labels, "pie")
}

#' @rdname charts
#' @export
cjsPolar <- function(chartjs, labels){
  cjs_base_chart(chartjs, labels, "polarArea") %>%
    cjsAddScale(NULL, "radialLinear")
}

#' @rdname charts
#' @export
cjsRadar <- function(chartjs, labels){
  cjs_base_chart(chartjs, labels, "radar") %>%
    cjsAddScale(NULL, "radialLinear")
}

cjs_base_chart <- function(chartjs, labels, type){
  class(chartjs) <- c(class(chartjs), paste0("cjs_", type))
  chartjs$x$type <- type
  chartjs$x$data$labels <- I(labels)
  chartjs
}
