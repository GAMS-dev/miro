context("UI tests - Model without input data works")
skip_if(
  identical(Sys.getenv("GAMS_SYS_DIR"), ""),
  "GAMS_SYS_DIR environment variable not set. Skipping tests."
)

additionalGamsClArgs <- character(0L)
if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
  additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
}

createTestDb()
miroModelDir <- file.path(testDir, "model", "test_miro")
Sys.setenv(MIRO_MODEL_PATH = file.path(testDir, "model", "test_miro", "test_miro.gms"))
if (length(additionalGamsClArgs)) {
  saveAdditionalGamsClArgs(miroModelDir, "test_miro", additionalGamsClArgs)
}
test_that(
  "Model without input data works",
  expect_pass(testApp(file.path(testDir, ".."), "solve_no_input_test",
    compareImages = FALSE
  ))
)
Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
