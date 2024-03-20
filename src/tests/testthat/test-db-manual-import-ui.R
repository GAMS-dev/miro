test_that(
  "Loading particular inputs from database works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    testModelPath <- file.path(testDir, "model", "pickstock_with_data")
    createTestDb()
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "pickstock_with_data.gms"
    ))
    Sys.setenv(MIRO_MODE = "base")

    source(file.path(testDir, "shinytest", "load_from_db_test_manual_import.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_MODE"))
  })
)
