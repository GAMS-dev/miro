context("UI tests - Miscellaneous")

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
  file.path(modelDataPath, "default1.gdx")
)
file.copy2(
  file.path(testDir, "data", "transport2.gdx"),
  file.path(modelDataPath, "default2.gdx")
)
test_that(
  "Loading scenario while input graph view is active works",
  expect_pass(testApp(file.path(testDir, ".."), "load_input_graph_test",
    compareImages = FALSE
  ))
)
unlink(modelDataPath, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
