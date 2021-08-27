context("UI tests - pivot compare mode")

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
test_that(
  "Default compare mode works",
  expect_pass(testApp(file.path(testDir, ".."), "default_compare_test",
    compareImages = FALSE
  ))
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
  "Loading scenarios works",
  expect_pass(testApp(file.path(testDir, ".."), "load_pivot_compare_test",
    compareImages = FALSE
  ))
)
unlink(modelDataPath, recursive = TRUE, force = TRUE)

file.move(
  file.path(testModelPath, "conf_transport", "transport_copy.json"),
  file.path(testModelPath, "conf_transport", "transport.json")
)

testModelPath <- file.path(testDir, "model", "pickstock_with_data")
modelDataPath <- file.path(testModelPath, "data_pickstock_with_data")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "pickstock_with_data.gms"
))
createTestDb()

file.copy2(
  file.path(modelDataPath, "default.gdx"),
  file.path(modelDataPath, "pickstock.gdx")
)

context("UI tests - comparison mode in split/tab view")
test_that(
  "Scenario comparison mode in split/tab view works",
  expect_pass(testApp(file.path(testDir, ".."), "comparison_mode_test",
    compareImages = FALSE
  ))
)
unlink(file.path(modelDataPath, "pickstock.gdx"), force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
