context("UI tests - Language tests")

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
    test_that(
      paste0("Language: ", langToTest, " works"),
      expect_pass(testApp(file.path(testDir, ".."), paste0("lang_test_", langToTest),
        compareImages = FALSE
      ))
    )
  }
}
Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_LANG", "MIRO_MODE"))
