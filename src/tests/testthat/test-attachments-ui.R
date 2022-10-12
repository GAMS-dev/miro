context("UI tests - Attachments")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(getwd(), "..", "model", "pickstock_with_data", "pickstock_with_data.gms"))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Attachments work",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "attachments_test",
    compareImages = FALSE
  )))
)
test_that(
  "Save as dialog (attachments) works",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "attachments_save_as_test",
    compareImages = FALSE
  )))
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
