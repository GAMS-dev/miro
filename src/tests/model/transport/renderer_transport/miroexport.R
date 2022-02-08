miroexport_test <- function(data, path = NULL, ...) {
  if (is.list(data) && identical(names(data), c("schedule", "a")) &&
    identical(dplyr::all_equal(data$a, tibble::tibble(i = c("Seattle", "San-Diego"), value = c(350, 600))), TRUE) &&
    identical(data$schedule$quantities, c(50, 300, NA, 275, NA, 275))) {
    return()
  }
  abortSafe("Bad, bad, bad...")
}

miroexport_JSON <- function(data, path = NULL, ...) {
  jsonlite::write_json(data, path = path, dataframe = "columns")
}
