test_that(
  "Sandbox scenario and baseline can be selected when batch comparing scenarios",
  retryUiTest(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()
    modelToTest <- "pickstock"
    testModelDir <- file.path(testDir, "model", modelToTest)
    modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
    Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
    configJSONFileName <- file.path(
      testModelDir, paste0("conf_", modelToTest),
      paste0(modelToTest, ".json")
    )

    file.copy(configJSONFileName, file.path(
      dirname(configJSONFileName),
      paste0(tolower(modelToTest), "_tmp.json")
    ), overwrite = TRUE)
    configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))

    configJSON$customCompareModules <- list(
      list(
        id = "sbtest",
        label = "Sandbox test",
        externalRendererId = "test2",
        selectBaseline = TRUE
      ),
      list(
        id = "nobaseline",
        label = "No baseline test",
        externalRendererId = "test2"
      )
    )
    configJSON$defaultScenName <- "default"

    jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
    file.copy2(file.path(testDir, "data", "pickstock.gdx"), file.path(modelDataPath, "default.gdx"))

    source(file.path(testDir, "shinytest", "custom_comparison_sandbox_test.R"), local = TRUE)

    file.rename(
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json"))
    )

    unlink(modelDataPath, recursive = TRUE, force = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
