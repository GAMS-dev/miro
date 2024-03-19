test_that(
  "Language tests work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    modelsToTest <- c("pickstock")
    langsToTest <- c("en", "de", "cn")

    createTestDb()
    # END setup

    errMsg <- NULL
    testFiles <- c("lang_test")
    for (langToTest in langsToTest) {
      Sys.setenv(MIRO_LANG = langToTest)
      for (modelToTest in modelsToTest) {
        testModelPath <- file.path(testDir, "model", modelToTest)
        Sys.setenv(MIRO_MODEL_PATH = file.path(
          testModelPath,
          paste0(modelToTest, ".gms")
        ))
        Sys.setenv(GMSMODELNAME = modelToTest)
        Sys.setenv(MIRO_MODE = "base")

        for (testFile in testFiles) {
          file.copy(file.path(testDir, "shinytest", paste0(testFile, ".R")),
            file.path(testDir, "shinytest", paste0(testFile, "_", langToTest, ".R")),
            overwrite = TRUE
          )
        }
        if (dir.exists(file.path(testModelPath, paste0("data_", modelToTest))) &&
          unlink(file.path(testModelPath, paste0("data_", modelToTest)),
            recursive = TRUE, force = TRUE
          ) != 0L) {
          warning(sprintf("Couldn't remove data dir of model: '%s'", modelToTest))
        }
        source(file.path(testDir, "shinytest", "lang_test.R"), local = TRUE)
      }
    }
    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_LANG", "MIRO_MODE"))
  })
)
