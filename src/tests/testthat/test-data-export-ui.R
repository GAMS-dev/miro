test_that(
  "Export sandbox scenario works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
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
    source(file.path(testDir, "shinytest", "download_files_test.R"), local = TRUE)

    unlink(modelDataPath, recursive = TRUE, force = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
