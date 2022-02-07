customExporter <- function(data, ...) {
  if (is.list(data) && identical(names(data), c("a", "schedule")) &&
    identical(dplyr::all_equal(data$a, tibble::tibble(i = c("Seattle", "San-Diego"), value = c(350L, 600L))), TRUE) &&
    identical(data$schedule$quantities, c(50, 300, NA, 275, NA, 275))) {
    return()
  }
  stop_custom("error_custom", "Bad, bad, bad...", call. = FALSE)
}
