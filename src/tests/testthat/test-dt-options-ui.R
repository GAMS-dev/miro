test_that(
  "datatable options work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    testModelPath <- file.path(testDir, "model", "symbol_groups")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "symbol_groups.gms"
    ))
    source(file.path(testDir, "shinytest", "dt_options_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH"))
  })
)
