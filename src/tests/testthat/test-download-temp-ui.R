context("UI tests - Download temporary files")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "pickstock_output_tables",
  "pickstock_output_tables.gms"
))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Downloading temporary files works.",
  {
    source(file.path(testDir, "shinytest", "download_temp_files_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
