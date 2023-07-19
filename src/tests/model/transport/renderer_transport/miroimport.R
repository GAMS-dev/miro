miroimport_noFile <- function(symbolNames, localFile = NULL, views = NULL,
                              attachments = NULL, metadata = NULL,
                              customRendererDir = NULL, ...) {
  if (!is.null(localFile) || !identical(symbolNames, c("a", "b")) ||
    !startsWith(readr::read_file(file.path(customRendererDir, "miroimport.R")), "miroimport_noFile")) {
    abortSafe("Bad, bad, bad...")
  }
  metadata$name <- "HeyHey"
  metadata$tags <- c("heyhey", "hoho")
  return(list(a = tibble(i = "isBadA", value = 0), b = tibble(j = "isBadB", value = 0)))
}

miroimport_withFile <- function(symbolNames, localFile = NULL, views = NULL,
                                attachments = NULL, metadata = NULL,
                                customRendererDir = NULL, ...) {
  if (!identical(symbolNames, "b")) {
    abortSafe("Bad, bad, bad...")
  }
  if (is.null(localFile)) {
    abortSafe("No file provided")
  }
  if (!dplyr::all_equal(
    readr::read_csv(localFile$datapath, col_types = "cd"),
    tibble::tibble(i = c("Seattle", "Boston"), value = c(123, 456))
  )) {
    return(list(b = tibble(j = "isGoodB", value = 2)))
  }
  attachments$add(NULL, localFile$datapath, fileNames = "test.csv", execPerm = FALSE)
  if (!identical(attachments$getIds(), "test.csv")) {
    return(list(b = tibble(j = "isGoodB", value = 2)))
  }

  return(list(b = tibble(j = "isGoodB", value = 1)))
}

miroimport_JSON <- function(symbolNames, localFile = NULL, views = NULL,
                            attachments = NULL, metadata = NULL, ...) {
  if (is.null(localFile) || !identical(length(localFile$datapath), 1L)) {
    abortSafe("Please upload a single, valid JSON file")
  }
  tryCatch(
    {
      jsonData <- jsonlite::read_json(localFile$datapath, simplifyVector = TRUE)
    },
    error = function(e) {
      abortSafe("Could not parse JSON file. Is the syntax correct?")
    }
  )
  dataTmp <- lapply(symbolNames, function(symbolName) {
    if (!symbolName %in% names(jsonData)) {
      return(NULL)
    }
    tryCatch(
      {
        return(tibble::as_tibble(jsonData[[symbolName]]))
      },
      error = function(e) {
        abortSafe("Could not parse JSON file. It does not seem to follow the expected structure.")
      }
    )
  })
  names(dataTmp) <- symbolNames
  return(dataTmp)
}
