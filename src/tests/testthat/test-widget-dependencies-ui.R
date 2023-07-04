context("UI tests - widget dependencies")

createTestDb()
modelToTest <- "pickstock_widget_dependencies"
testModelDir <- file.path(testDir, "model", modelToTest)
modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))

test_that(
  "Widget foward dependencies work as expected",
  {
    source(file.path(testDir, "shinytest", "widget_dependencies_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
