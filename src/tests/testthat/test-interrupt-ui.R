test_that(
  "Interrupt model works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    skip_if(
      identical(Sys.getenv("GAMS_SYS_DIR"), ""),
      "GAMS_SYS_DIR environment variable not set. Skipping tests."
    )

    additionalGamsClArgs <- character(0L)
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
    }
    modelToTest <- "interrupt"
    miroModelDir <- file.path(testDir, "model", modelToTest)
    createTestDb()

    Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, paste0(modelToTest, ".gms")))
    Sys.setenv(GMSMODELNAME = modelToTest)

    if (length(additionalGamsClArgs)) {
      saveAdditionalGamsClArgs(miroModelDir, modelToTest, additionalGamsClArgs)
    }
    source(file.path(testDir, "shinytest", "interrupt_model_test.R"), local = TRUE)
    if (length(additionalGamsClArgs)) {
      file.rename(
        file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_tmp.json")),
        file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
      )
    }
    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
  })
)
