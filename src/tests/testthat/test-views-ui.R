test_that(
  "Views work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    testModelPath <- file.path(testDir, "model", "transport")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "transport.gms"
    ))
    Sys.setenv(MIRO_MODE = "base")

    source(file.path(testDir, "shinytest", "views_metadata_test.R"), local = TRUE)

    modelDataPath <- file.path(testModelPath, "data_transport")

    file.copy2(
      file.path(testDir, "data", "good-views2.json"),
      file.path(modelDataPath, "default_views.json")
    )
    file.copy2(
      file.path(testDir, "data", "transport.gdx"),
      file.path(modelDataPath, "default.gdx")
    )
    source(file.path(testDir, "shinytest", "views_import_test.R"), local = TRUE)
    unlink(modelDataPath, recursive = TRUE, force = TRUE)

    source(file.path(testDir, "shinytest", "views_save_as_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
