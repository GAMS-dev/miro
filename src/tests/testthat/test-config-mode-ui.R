context("UI tests - Configuration Mode")
library(shinytest)

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
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "config_mode_general_test",
    compareImages = FALSE
  )))
)
test_that(
  "Configuration mode - table settings works.",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "config_mode_table_test",
    compareImages = FALSE
  )))
)
test_that(
  "Configuration mode - widget settings works.",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "config_mode_widget_test",
    compareImages = FALSE
  )))
)
test_that(
  "Configuration mode - graph settings works.",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "config_mode_graph_test",
    compareImages = FALSE
  )))
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
