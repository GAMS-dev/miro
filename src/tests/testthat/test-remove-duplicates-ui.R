test_that(
  "Removing duplicates works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    miroModelDir <- file.path(testDir, "..", "model", "transport")
    Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, "transport.gms"))
    Sys.setenv(GMSMODELNAME = "transport")

    source(file.path(testDir, "shinytest", "remove_duplicates_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
  })
)
