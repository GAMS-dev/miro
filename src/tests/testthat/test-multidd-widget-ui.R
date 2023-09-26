context("UI tests - Multi-dropdown works")

skip_if(
  identical(Sys.getenv("GAMS_SYS_DIR"), ""),
  "GAMS_SYS_DIR environment variable not set. Skipping tests."
)

additionalGamsClArgs <- character(0L)
if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
  additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
}

createTestDb()
miroModelDir <- file.path(testDir, "model", "multi_dd")
Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, "multi_dd.gms"))

if (length(additionalGamsClArgs)) {
  saveAdditionalGamsClArgs(miroModelDir, "multi_dd", additionalGamsClArgs)
}
test_that(
  "Multi-dropdown menus work as input widgets",
  {
    source(file.path(testDir, "shinytest", "multi_dd_widget_test.R"), local = TRUE)
  }
)
if (length(additionalGamsClArgs)) {
  file.rename(
    file.path(miroModelDir, "conf_multi_dd", "multi_dd_tmp.json"),
    file.path(miroModelDir, "conf_multi_dd", "multi_dd.json")
  )
}
Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
