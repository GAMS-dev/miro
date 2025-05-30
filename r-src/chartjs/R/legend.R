#' Legend options (legend is displayed by default)
#'
#' @param chartjs a \code{\link{chartjs}} object
#' @param position "top" or "bottom", defaults to "top"
#' @param onClick a JS function as a character
#' @param fullWidth a logical, defaults to TRUE
#' @param labels a list of parameters for the labels : \itemize{
#'  \item boxWidth an integer, defaults to 40
#'  \item fontColor a character, defaults to "#666"
#'  \item fontFamily a character, defaults to "Helvetica Neue"
#'  \item fontSize an integer, defaults to 12
#'  \item fontStyle a character, defaults to "normal"
#'  \item padding a integer, defaults to 10 (in pixels)
#'  \item display a boolean, defaults to TRUE
#' }
#'
#'
#' @export

cjsLegend <- function(chartjs, position = NULL, onClick = NULL, fullWidth = NULL, labels = NULL, display = TRUE){
  chartjs$x$options$plugins$legend <- c(list(display = display), createOptions())
  chartjs
}
