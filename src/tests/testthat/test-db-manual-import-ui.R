context("UI tests - Loading particular inputs from database")

testModelPath <- file.path(testDir, "model", "pickstock_with_data")
createTestDb()
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "pickstock_with_data.gms"
))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Manually importing inputs from database scenario works",
  expect_pass(testApp(file.path(testDir, ".."), "load_from_db_test_manual_import",
    compareImages = FALSE
  ))
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_MODE"))
