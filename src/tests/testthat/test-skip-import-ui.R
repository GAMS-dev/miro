test_that(
  "Skip import of data files works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
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
    source(file.path(testDir, "shinytest", "skip_scen_import_test.R"), local = TRUE)
    file.copy2(
      file.path(testDir, "data", "transport2.gdx"),
      file.path(modelDataPath, "default2.gdx")
    )
    source(file.path(testDir, "shinytest", "skip_scen_import_test.R"), local = TRUE)
    file.copy2(
      file.path(testDir, "data", "transport2.gdx"),
      file.path(modelDataPath, "default.gdx")
    )
    source(file.path(testDir, "shinytest", "skip_scen_import_test.R"), local = TRUE)
    Sys.setenv(MIRO_FORCE_SCEN_IMPORT = "true")
    Sys.setenv(MIRO_DATA_DIR = file.path(modelDataPath, "default.gdx"))
    source(file.path(testDir, "shinytest", "skip_scen_import_test.R"), local = TRUE)
    unlink(modelDataPath, recursive = TRUE, force = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_FORCE_SCEN_IMPORT", "MIRO_DATA_DIR"))
  })
)
