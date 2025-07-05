test_that(
  "Compare modes work",
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
    source(file.path(testDir, "shinytest", "default_compare_test.R"), local = TRUE)

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
    source(file.path(testDir, "shinytest", "load_pivot_compare_test.R"), local = TRUE)
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
    source(file.path(testDir, "shinytest", "comparison_mode_test.R"), local = TRUE)
    unlink(file.path(modelDataPath, "pickstock.gdx"), force = TRUE)

    # Dashboard comparison
    createTestDb()
    Sys.setenv(MIRO_MODE = "base")
    testModelPath <- file.path(testDir, "model", "pickstock_dashboard")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "pickstock.gms"
    ))

    modelDataPath <- file.path(testModelPath, "data_pickstock")

    file.copy2(
      file.path(testDir, "data", "pickstock1.gdx"),
      file.path(modelDataPath, "pickstock1.gdx")
    )
    file.copy2(
      file.path(testDir, "data", "pickstock2.gdx"),
      file.path(modelDataPath, "pickstock2.gdx")
    )

    context("UI tests - dashboard comparison mode")
    source(file.path(testDir, "shinytest", "dashboard_comparison_mode_test.R"), local = TRUE)
    unlink(file.path(modelDataPath, "pickstock1.gdx"), force = TRUE)
    unlink(file.path(modelDataPath, "pickstock2.gdx"), force = TRUE)


    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
