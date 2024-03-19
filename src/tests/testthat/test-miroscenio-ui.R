test_that(
  "Import/Export miroscen files works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    testModelPath <- file.path(testDir, "model", "transport")
    modelDataPath <- file.path(testModelPath, "data_transport")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "transport.gms"
    ))
    Sys.setenv(GMSMODELNAME = "transport")
    Sys.setenv(MIRO_MODE = "base")

    file.copy2(
      file.path("..", "data", "bla.miroscen"),
      file.path(modelDataPath, "bla.miroscen")
    )
    source(file.path(testDir, "shinytest", "miroscenio_test.R"), local = TRUE)
    unlink(file.path(modelDataPath, "bla.miroscen"), force = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_MODE"))
  })
)
