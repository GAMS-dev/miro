test_that(
  "Loading sandbox scenarios works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
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
    source(file.path(testDir, "shinytest", "sandbox_compare_test.R"), local = TRUE)
    unlink(modelDataPath, recursive = TRUE, force = TRUE)

    file.move(
      file.path(testModelPath, "conf_transport", "transport_copy.json"),
      file.path(testModelPath, "conf_transport", "transport.json")
    )

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
