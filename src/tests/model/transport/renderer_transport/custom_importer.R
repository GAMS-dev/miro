customImporter <- function(symbolName, localFile = NULL, ...) {
  if (!is.null(localFile) || !symbolName %in% c("a", "b")) {
    return(tibble(i = "isBad", value = 1))
  }
  if (identical(symbolName, "a")) {
    return(tibble(i = "isBadA", value = 0))
  }
  return(tibble(j = "isBadB", value = 0))
}

customImporterWithFile <- function(symbolName, localFile = NULL, ...) {
  if (is.null(localFile) || !identical(symbolName, "b")) {
    return(tibble(j = "isGoodB", value = 0))
  }
  if (!dplyr::all_equal(
    readr::read_csv(localFile$datapath, col_types = "cd"),
    tibble::tibble(i = c("Seattle", "Boston"), value = c(123, 456))
  )) {
    return(tibble(j = "isGoodB", value = 2))
  }
  return(tibble(j = "isGoodB", value = 1))
}
