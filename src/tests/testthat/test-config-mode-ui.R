context("UI tests - Configuration Mode")
library(shinytest)

source("../../components/util.R")

if(!dependenciesInstalled()){
  installDependencies()
}
testDir <- file.path(getwd(), "..")

createTestDb()
Sys.setenv(MIRO_DB_PATH = testDir)

Sys.setenv(MIRO_MODEL_PATH = file.path(testDir, "model", "pickstock_configuration",
                                       "pickstock_configuration.gms"))
Sys.setenv(MIRO_MODE = "config")
configJSONPath <- file.path(testDir, "model", "pickstock_configuration", "conf_pickstock_configuration")
file.copy(file.path(configJSONPath,"pickstock_configuration_expected.json"),
          file.path(configJSONPath,"pickstock_configuration.json"), overwrite = TRUE)

test_that("Configuration mode - general settings works.",
          expect_pass(testApp(file.path(testDir, ".."), "config_mode_general_test",
                              compareImages = FALSE)))
test_that("Configuration mode - table settings works.",
          expect_pass(testApp(file.path(testDir, ".."), "config_mode_table_test",
                              compareImages = FALSE)))
test_that("Configuration mode - widget settings works.",
          expect_pass(testApp(file.path(testDir, ".."), "config_mode_widget_test",
                              compareImages = FALSE)))
test_that("Configuration mode - graph settings works.",
          expect_pass(testApp(file.path(testDir, ".."), "config_mode_graph_test",
                              compareImages = FALSE)))

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))