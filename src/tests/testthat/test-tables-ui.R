context("UI tests - Table settings")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "pickstock_output_tables",
  "pickstock_output_tables.gms"
))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Output table config works.",
  {
    source(file.path(testDir, "shinytest", "output_table_settings.R"), local = TRUE)
  }
)

test_that(
  "Adding/removing rows in input table works.",
  {
    source(file.path(testDir, "shinytest", "bigdata_table.R"), local = TRUE)
  }
)

test_that(
  "Input table markdown works",
  {
    source(file.path(testDir, "shinytest", "input_table_markdown_test.R"), local = TRUE)
  }
)
Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "transport",
  "transport.gms"
))
file.copy2(
  file.path("..", "model", "transport", "conf_transport", "transport.json"),
  file.path("..", "model", "transport", "conf_transport", "bk_transport.json")
)
createTestDb()
test_that(
  "Input table dropdownCols work",
  {
    source(file.path(testDir, "shinytest", "input_table_dropdowncols_test.R"), local = TRUE)
  }
)
file.move(
  file.path("..", "model", "transport", "conf_transport", "bk_transport.json"),
  file.path("..", "model", "transport", "conf_transport", "transport.json")
)

file.copy2(
  file.path("..", "model", "transport", "conf_transport", "transport.json"),
  file.path("..", "model", "transport", "conf_transport", "bk_transport.json")
)
createTestDb()
test_that(
  "Input table column formatting works",
  {
    source(file.path(testDir, "shinytest", "input_table_col_format_test.R"), local = TRUE)
  }
)
file.move(
  file.path("..", "model", "transport", "conf_transport", "bk_transport.json"),
  file.path("..", "model", "transport", "conf_transport", "transport.json")
)

file.copy2(
  file.path("..", "model", "transport", "conf_transport", "transport.json"),
  file.path("..", "model", "transport", "conf_transport", "bk_transport.json")
)
file.copy2(
  file.path("..", "model", "transport", "conf_transport", "transport_io.json"),
  file.path("..", "model", "transport", "conf_transport", "bk_transport_io.json")
)
file.copy2(
  file.path("..", "data", "transport_io.json"),
  file.path("..", "model", "transport", "conf_transport", "transport_io.json")
)
createTestDb()
test_that(
  "Input table column validation works",
  {
    source(file.path(testDir, "shinytest", "input_table_validate_cols_test.R"), local = TRUE)
  }
)
file.move(
  file.path("..", "model", "transport", "conf_transport", "bk_transport.json"),
  file.path("..", "model", "transport", "conf_transport", "transport.json")
)
file.move(
  file.path("..", "model", "transport", "conf_transport", "bk_transport_io.json"),
  file.path("..", "model", "transport", "conf_transport", "transport_io.json")
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
