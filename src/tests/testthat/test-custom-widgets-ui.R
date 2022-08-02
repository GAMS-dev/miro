context("UI tests - Custom widgets")

createTestDb()

Sys.setenv("MIRO_MODE" = "base")

Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "sudoku_custom_widget",
  "sudoku.gms"
))

test_that(
  "Custom widgets with multiple symbols work.",
  {
    source(file.path(testDir, "shinytest", "custom_widgets_multiple_symbols_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
