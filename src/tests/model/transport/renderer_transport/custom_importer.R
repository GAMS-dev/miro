customImporter <- function(symbolNames, localFile = NULL, ...) {
  if (!is.null(localFile) || !identical(symbolNames, c("a", "b"))) {
    stop_custom("error_custom",
      "Bad, bad, bad...",
      call. = FALSE
    )
  }
  return(list(a = tibble(i = "isBadA", value = 0), b = tibble(j = "isBadB", value = 0)))
}

customImporterWithFile <- function(symbolNames, localFile = NULL, ...) {
  if (!identical(symbolNames, "b")) {
    stop_custom("error_custom",
      "Bad, bad, bad...",
      call. = FALSE
    )
  }
  if (is.null(localFile)) {
    stop_custom("error_custom",
      "No file provided",
      call. = FALSE
    )
  }
  if (!dplyr::all_equal(
    readr::read_csv(localFile$datapath, col_types = "cd"),
    tibble::tibble(i = c("Seattle", "Boston"), value = c(123, 456))
  )) {
    return(list(b = tibble(j = "isGoodB", value = 2)))
  }
  return(list(b = tibble(j = "isGoodB", value = 1)))
}
