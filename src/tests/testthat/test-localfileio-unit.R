context("Unit tests - LocalFileIO class")

source("../../components/localfileio.R")

ioConfig <<- list(
  modelInRaw = list(
    "_scalars" = list(
      symnames = c("baseyear", "repco", "gr", "growthq", "drc", "the1", "lstd", "trcap", "twcap", "twefac", "labfac"),
      symtext = c("", "", "", "", "", "", "", "", "", "", ""),
      symtypes = c("set", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter"),
      colTypes = "ccc",
      alias = "Input scalars",
      headers = list(
        scalar = list(),
        description = list(),
        value = list()
      )
    ),
    "_gmsopt_solver" = list()
  )
)

localfileIO <- LocalFileIO$new()


test_that("Fixing scalar table works", {
  expect_error(
    localfileIO$fixScalarDf(tibble(
      value = c(10, 20, 30),
      scalar = c("bla", "lstd", "repco")
    ), "_scalars"),
    class = "error_validation",
    regexp = "Invalid scalar\\(s\\) found in scalar dataframe: bla"
  )
  expect_identical(
    localfileIO$fixScalarDf(
      tibble(
        value = c(10, 20, 30),
        scalar = c("bla", "lstd", "repco")
      ),
      "_scalars",
      ignoreInvalidScalars = TRUE
    ),
    tibble(
      scalar = c(
        "baseyear", "repco", "gr", "growthq", "drc", "the1", "lstd",
        "trcap", "twcap", "twefac", "labfac", "_gmsopt_solver"
      ),
      description = "",
      value = c(
        NA_character_, "30", NA_character_, NA_character_,
        NA_character_, NA_character_, "20", NA_character_,
        NA_character_, NA_character_, NA_character_, NA_character_
      )
    )
  )
})
