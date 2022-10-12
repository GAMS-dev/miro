context("UI tests - export sandbox scenario")

createTestDb()

testModel <- "transport"
testModelPath <- file.path(testDir, "model", testModel)
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath, paste0(testModel, ".gms")))
Sys.setenv(MIRO_MODE = "base")
modelDataPath <- file.path(testModelPath, paste0("data_", testModel))

file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default.gdx")
)
test_that(
  "Export scenario works",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "download_files_test",
    compareImages = FALSE
  )))
)

unlink(modelDataPath, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
