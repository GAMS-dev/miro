context("UI tests - views in pivot compare mode")

createTestDb()

testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport.gms"
))
Sys.setenv(MIRO_MODE = "base")

modelDataPath <- file.path(testModelPath, "data_transport")
globalViewsFilePath <- file.path(testModelPath, "conf_transport", "views.json")

file.copy2(
  file.path(testDir, "data", "transport_full.gdx"),
  file.path(modelDataPath, "default.gdx")
)
jsonlite::write_json(list(`_pivotcomp_schedule` = list(
  test = list(
    rows = c("i", "j"),
    cols = list(`_scenName` = NULL),
    filter = list(Hdr = "quantities"),
    pivotRenderer = "bar"
  )
)), globalViewsFilePath, pretty = TRUE, auto_unbox = TRUE, null = "null")
configJSONFileName <- file.path(testModelPath, "conf_transport", "transport.json")
file.copy(configJSONFileName, file.path(
  dirname(configJSONFileName),
  "transport_tmp.json"
), overwrite = TRUE)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))
configJSON$pivotCompSettings$symbolConfig <- list(schedule = list(
  externalDefaultView = "test"
))
jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
test_that(
  "Loading/saving views in Pivot Compare Mode works",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "pivot_comp_views_test",
    compareImages = FALSE
  )))
)
unlink(globalViewsFilePath)
unlink(modelDataPath, recursive = TRUE, force = TRUE)
file.rename(
  file.path(dirname(configJSONFileName), "transport_tmp.json"),
  file.path(dirname(configJSONFileName), "transport.json")
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
