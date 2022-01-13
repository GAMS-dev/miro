context("UI tests - Table settings")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(
  getwd(), "..", "model", "pickstock_output_tables",
  "pickstock_output_tables.gms"
))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Output table config works.",
  expect_pass(testApp(file.path(testDir, ".."), "output_table_settings",
    compareImages = FALSE
  ))
)

test_that(
  "Adding/removing rows in input table works.",
  expect_pass(testApp(file.path(testDir, ".."), "bigdata_table",
    compareImages = FALSE
  ))
)

test_that(
  "Input table markdown works",
  expect_pass(testApp(file.path(testDir, ".."), "input_table_markdown_test",
    compareImages = FALSE
  ))
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
  expect_pass(testApp(file.path(testDir, ".."), "input_table_dropdowncols_test",
    compareImages = FALSE
  ))
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
  expect_pass(testApp(file.path(testDir, ".."), "input_table_col_format_test",
    compareImages = FALSE
  ))
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
  expect_pass(testApp(file.path(testDir, ".."), "input_table_validate_cols_test",
    compareImages = FALSE
  ))
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
