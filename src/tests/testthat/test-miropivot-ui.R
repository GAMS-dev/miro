context("UI tests - MIRO Pivot")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "transport_miropivot",
  "transport_miropivot.gms"
))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "MIRO Pivot renderer works.",
  {
    source(file.path(testDir, "shinytest", "miropivot_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
