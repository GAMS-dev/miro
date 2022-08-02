context("UI tests - widget dependencies")

createTestDb()
modelToTest <- "pickstock"
testModelDir <- file.path(testDir, "model", modelToTest)
modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
configJSONFileName <- file.path(
  testModelDir, paste0("conf_", modelToTest),
  paste0(modelToTest, ".json")
)

# use --widget for new 'solver' symbol, set input/output symbol order
file.copy(configJSONFileName, file.path(
  dirname(configJSONFileName),
  paste0(tolower(modelToTest), "_tmp.json")
), overwrite = TRUE)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))

configJSON$inputWidgets$maxstock <- list(
  widgetType = "dropdown",
  noHcube = FALSE,
  clearValue = FALSE,
  multiple = FALSE,
  alias = "maximum number of stocks to select",
  label = "select a stock",
  choices = "$price$symbol$"
)

configJSON$defaultScenName <- "default"

jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
file.copy2(file.path(testDir, "data", "pickstock.gdx"), file.path(modelDataPath, "default.gdx"))

test_that(
  "Widget foward and backward dependencies work as expected",
  {
    source(file.path(testDir, "shinytest", "widget_dependencies_test.R"), local = TRUE)
  }
)

file.rename(
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json"))
)

unlink(modelDataPath, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
