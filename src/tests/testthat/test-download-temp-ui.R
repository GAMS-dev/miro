test_that(
  "Downloading temporary files works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    Sys.setenv(MIRO_MODEL_PATH = file.path(
      getwd(), "..", "model", "pickstock_output_tables",
      "pickstock_output_tables.gms"
    ))
    Sys.setenv(MIRO_MODE = "base")

    source(file.path(testDir, "shinytest", "download_temp_files_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
