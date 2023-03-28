context("UI tests - MIRO log works")
skip_if(
  identical(Sys.getenv("GAMS_SYS_DIR"), ""),
  "GAMS_SYS_DIR environment variable not set. Skipping tests."
)

createTestDb()

additionalGamsClArgs <- character(0L)
if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
  additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
}
miroModelDir <- file.path(testDir, "..", "model", "pickstock")
Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, "pickstock.gms"))
Sys.setenv(GMSMODELNAME = "pickstock")
extraClArgs <- c(additionalGamsClArgs, "MIP=CBC")
if (length(additionalGamsClArgs)) {
  saveAdditionalGamsClArgs(miroModelDir, "pickstock", c(additionalGamsClArgs, extraClArgs))
}
test_that(
  "MIRO log works",
  {
    source(file.path(testDir, "shinytest", "mirolog_test.R"), local = TRUE)
  }
)

if (length(additionalGamsClArgs)) {
  file.rename(
    file.path(miroModelDir, "conf_pickstock", "pickstock_tmp.json"),
    file.path(miroModelDir, "conf_pickstock", "pickstock.json")
  )
}

createTestDb()

miroModelDir <- file.path(testDir, "model", "sudoku_custom_widget")
Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, "sudoku.gms"))
Sys.setenv(GMSMODELNAME = "sudoku")
# deactivate log file (only MIRO log activated)
configJSONFileName <- file.path(miroModelDir, "conf_sudoku", "sudoku.json")
file.copy(configJSONFileName,
  file.path(dirname(configJSONFileName), "sudoku_tmp.json"),
  overwrite = TRUE
)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))
configJSON$activateModules$logFile <- FALSE
jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
  saveAdditionalGamsClArgs(
    miroModelDir, "sudoku",
    paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
  )
}
test_that(
  "MIRO log works with custom input widgets",
  {
    source(file.path(testDir, "shinytest", "mirolog_test_custom_widget.R"), local = TRUE)
  }
)

file.rename(
  file.path(miroModelDir, "conf_sudoku", "sudoku_tmp.json"),
  file.path(miroModelDir, "conf_sudoku", "sudoku.json")
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
