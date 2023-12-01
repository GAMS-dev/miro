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

skip_if(
  identical(Sys.getenv("ENGINE_URL"), ""),
  "Skipping custom widget scalar test as no ENGINE_URL was not set."
)

Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "sudoku_custom_widget_scalar",
  "sudoku.gms"
))

Sys.setenv(MIRO_REMOTE_EXEC = "true")

if (file.exists(file.path("~", ".miro", ".cred_sudoku"))) {
  unlink(file.path("~", ".miro", ".cred_sudoku"), force = TRUE)
}

test_that(
  "Custom widgets for scalars work.",
  {
    source(file.path(testDir, "shinytest", "custom_widgets_scalars_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_REMOTE_EXEC"))
