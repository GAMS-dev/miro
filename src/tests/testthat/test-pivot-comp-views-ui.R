context("UI tests - views in pivot compare mode")

createTestDb()

testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport.gms"
))
Sys.setenv(MIRO_MODE = "base")

modelDataPath <- file.path(testModelPath, "data_transport")

file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default.gdx")
)
test_that(
  "Loading/saving views in Pivot Compare Mode works",
  expect_pass(testApp(file.path(testDir, ".."), "pivot_comp_views_test",
    compareImages = FALSE
  ))
)
unlink(modelDataPath, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
