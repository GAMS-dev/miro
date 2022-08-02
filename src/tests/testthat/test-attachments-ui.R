context("UI tests - Attachments")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(getwd(), "..", "model", "pickstock_with_data", "pickstock_with_data.gms"))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Attachments work",
  {
    source(file.path(testDir, "shinytest", "attachments_test.R"), local = TRUE)
  }
)
test_that(
  "Save as dialog (attachments) works",
  {
    source(file.path(testDir, "shinytest", "attachments_save_as_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
