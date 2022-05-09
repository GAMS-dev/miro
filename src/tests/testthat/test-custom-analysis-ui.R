context("UI tests - custom analysis renderers")

createTestDb()
modelToTest <- "pickstock"
testModelDir <- file.path(testDir, "model", modelToTest)
modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
configJSONFileName <- file.path(
  testModelDir, paste0("conf_", modelToTest),
  paste0(modelToTest, ".json")
)
globalViewsFileName <- file.path(
  testModelDir, paste0("conf_", modelToTest),
  "views.json"
)

file.copy(configJSONFileName, file.path(
  dirname(configJSONFileName),
  paste0(tolower(modelToTest), "_tmp.json")
), overwrite = TRUE)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))

configJSON$analysisModules <- list(list(
  id = "test1",
  label = "My first custom analysis renderer"
), list(id = "test2", label = "My second custom analyzzzer"))

configJSON$defaultScenName <- "default"

jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
file.copy2(file.path(testDir, "data", "pickstock.gdx"), file.path(modelDataPath, "default.gdx"))
jsonlite::write_json(list(`_customcomp_test1` = list(test123 = list(b = "def"))),
  globalViewsFileName,
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

test_that(
  "Custom analysis renderers work",
  expect_pass(testApp(file.path(testDir, ".."), "custom_analysis_test",
    compareImages = FALSE
  ))
)

file.rename(
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json"))
)

unlink(modelDataPath, recursive = TRUE, force = TRUE)
unlink(globalViewsFileName, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
