miroexport_test <- function(data, path = NULL, views = NULL,
                            attachments = NULL, metadata = NULL, ...) {
  if (is.list(data) && identical(names(data), c("schedule", "a")) &&
    identical(dplyr::all_equal(data$a, tibble::tibble(i = c("Seattle", "San-Diego"), value = c(350, 600))), TRUE) &&
    identical(data$schedule$quantities, c(50, 300, NA, 275, NA, 275))) {
    return()
  }
  abortSafe("Bad, bad, bad...")
}

miroexport_JSON <- function(data, path = NULL, views = NULL,
                            attachments = NULL, metadata = NULL, ...) {
  data$isValid <- (identical(attachments$getIds(), c("bad1.miroscen", "scalars.csv")) &&
    identical(views$getIds("d"), "My view 1") && identical(metadata$name, "Test Scenario") &&
    identical(metadata$tags, c("asd", "def")))
  jsonlite::write_json(data, path = path, dataframe = "columns")
}
