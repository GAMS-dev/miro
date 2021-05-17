#' Add a scale to an axis
#'
#' \code{cjsAddScale} adds a scale to an axis.
#'
#' See the \href{http://www.chartjs.org/docs/#scales}{Charts.js documentation} for values in ...
#'
#' @param cjs a \code{\link{chartjs}} object
#' @param axis the axis on which the scale lives
#' @param type the type of scale to add
#' @param ... additional parameters
#'
#'
#' @export
cjsAddScale <- function(cjs, axis = c("x", "y", NULL), type = "category", title = NULL, ...){
  if (!is.null(axis)) {
    cjs$x$scales[[axis]] <- list(type = type, ...)
    if(!is.null(title)) {
      cjs$x$scales[[axis]]$title <- title
    }
  } else cjs$x$scale <- list(type = type, ...)
  cjs
}
