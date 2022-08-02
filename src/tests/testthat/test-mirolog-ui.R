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
Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
