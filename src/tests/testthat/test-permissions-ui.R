context("UI tests - Scenario permissions")
skip_if(identical(Sys.getenv("GAMS_SYS_DIR"), ""),
        "GAMS_SYS_DIR environment variable not set. Skipping tests.")

testDir <- file.path(getwd(), "..")

createTestDb()
# END setup

additionalGamsClArgs <- character(0L)
if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")){
  additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
}
Sys.setenv(MIRO_DB_PATH = testDir)

modelToTest <- "pickstock_with_data"
miroModelDir <- file.path(testDir, "model", modelToTest)
Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, "pickstock_with_data.gms"))
Sys.setenv(MIRO_MODE="base")

saveAdditionalGamsClArgs(miroModelDir, modelToTest, additionalGamsClArgs)
Sys.setenv(MIRO_FORCE_SCEN_IMPORT = "true")
test_that("Permissions work",
          expect_pass(testApp(file.path(testDir, ".."), "permissions_test",
                              compareImages = FALSE)))
test_that("Save as dialog (permissions) works",
          expect_pass(testApp(file.path(testDir, ".."), "permissions_save_as_test",
                              compareImages = FALSE)))
if(length(additionalGamsClArgs)){
  file.rename(file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_tmp.json")),
              file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json")))
}
Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_USERNAME", "MIRO_USERGROUPS",
               "MIRO_FORCE_SCEN_IMPORT"))