context("UI tests - sandbox compare mode")

createTestDb()
testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport.gms"
))
Sys.setenv(MIRO_MODE = "base")

file.copy2(
  file.path(testModelPath, "conf_transport", "transport.json"),
  file.path(testModelPath, "conf_transport", "transport_copy.json")
)

modelDataPath <- file.path(testModelPath, "data_transport")

file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default1.gdx")
)
file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default2.gdx")
)
file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default3.gdx")
)
file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default4.gdx")
)
test_that(
  "Loading sandbox scenarios works",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "sandbox_compare_test",
    compareImages = FALSE
  )))
)
unlink(modelDataPath, recursive = TRUE, force = TRUE)

file.move(
  file.path(testModelPath, "conf_transport", "transport_copy.json"),
  file.path(testModelPath, "conf_transport", "transport.json")
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
