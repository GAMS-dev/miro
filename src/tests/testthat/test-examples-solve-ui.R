test_that(
  "Example models solve",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    skip_if(
      identical(Sys.getenv("GAMS_SYS_DIR"), ""),
      "GAMS_SYS_DIR environment variable not set. Skipping tests."
    )

    additionalGamsClArgs <- character(0L)
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
    }

    for (modelToTest in c(
      "pickstock", "transport", "sudoku", "farming",
      "inscribedsquare", "tsp", "cpack", "lubrication", "indus89"
    )) {
      createTestDb()
      if (modelToTest %in% c("indus89")) {
        miroModelDir <- file.path(testDir, "model", modelToTest)
      } else {
        miroModelDir <- file.path(testDir, "..", "model", modelToTest)
      }
      Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, paste0(modelToTest, ".gms")))
      Sys.setenv(GMSMODELNAME = modelToTest)

      if (length(additionalGamsClArgs)) {
        saveAdditionalGamsClArgs(miroModelDir, modelToTest, additionalGamsClArgs)
      }
      for (testFile in c("solve_model_test")) {
        file.copy(file.path(testDir, "shinytest", paste0(testFile, ".R")),
          file.path(testDir, "shinytest", paste0(testFile, "_", modelToTest, ".R")),
          overwrite = TRUE
        )
      }
      source(file.path(testDir, "shinytest", paste0("solve_model_test_", modelToTest, ".R")), local = TRUE)
      if (modelToTest == "pickstock") {
        source(file.path(testDir, "shinytest", "interrupt_model_test.R"), local = TRUE)
      }
      if (length(additionalGamsClArgs)) {
        file.rename(
          file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_tmp.json")),
          file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
        )
      }
    }
    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
  })
)
