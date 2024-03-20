test_that(
  "Readonly mode works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    testModelPath <- file.path(testDir, "model", "transport")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "transport.gms"
    ))
    Sys.setenv(MIRO_MODE = "readonly")

    source(file.path(testDir, "shinytest", "readonly_mode_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
