context("UI tests - Download temporary files")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "pickstock_output_tables",
  "pickstock_output_tables.gms"
))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Downloading temporary files works.",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "download_temp_files_test",
    compareImages = FALSE
  )))
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
