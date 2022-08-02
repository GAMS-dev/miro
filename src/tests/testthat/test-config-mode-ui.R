context("UI tests - Configuration Mode")

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(
  testDir, "model", "pickstock_configuration",
  "pickstock_configuration.gms"
))
Sys.setenv(MIRO_MODE = "config")
configJSONPath <- file.path(testDir, "model", "pickstock_configuration", "conf_pickstock_configuration")
file.copy(file.path(configJSONPath, "pickstock_configuration_expected.json"),
  file.path(configJSONPath, "pickstock_configuration.json"),
  overwrite = TRUE
)

test_that(
  "Configuration mode - general settings works.",
  {
    source(file.path(testDir, "shinytest", "config_mode_general_test.R"), local = TRUE)
  }
)
test_that(
  "Configuration mode - table settings works.",
  {
    source(file.path(testDir, "shinytest", "config_mode_table_test.R"), local = TRUE)
  }
)
test_that(
  "Configuration mode - widget settings works.",
  {
    source(file.path(testDir, "shinytest", "config_mode_widget_test.R"), local = TRUE)
  }
)
test_that(
  "Configuration mode - graph settings works.",
  {
    source(file.path(testDir, "shinytest", "config_mode_graph_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
