test_that(
  "Valuebox settings works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    modelToTest <- "pickstock_with_data"
    testModelDir <- file.path(testDir, "model", modelToTest)
    Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))

    testModelPath <- file.path(getwd(), "..", "model", modelToTest)
    Sys.setenv(MIRO_MODE = "base")

    configJSONFileName <- file.path(testModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
    file.copy(configJSONFileName, file.path(
      dirname(configJSONFileName),
      paste0(modelToTest, "_tmp.json")
    ), overwrite = TRUE)

    oldValueboxConfigJSON <- file.path(testModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_valuebox_old.json"))
    newValueboxConfigJSON <- file.path(testModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_valuebox_new.json"))

    file.copy(oldValueboxConfigJSON, file.path(
      dirname(configJSONFileName),
      paste0(modelToTest, ".json")
    ), overwrite = TRUE)

    context("UI tests - Old valuebox settings")
    source(file.path(testDir, "shinytest", "valuebox_old_config_test.R"), local = TRUE)

    file.copy(newValueboxConfigJSON, file.path(
      dirname(configJSONFileName),
      paste0(modelToTest, ".json")
    ), overwrite = TRUE)

    context("UI tests - new valuebox settings")
    source(file.path(testDir, "shinytest", "valuebox_new_config_test.R"), local = TRUE)

    file.rename(
      file.path(dirname(configJSONFileName), paste0(modelToTest, "_tmp.json")),
      file.path(dirname(configJSONFileName), paste0(modelToTest, ".json"))
    )


    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
