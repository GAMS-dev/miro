context("UI tests - Valuebox settings")

testDir <- file.path(getwd(), "..")

createTestDb()

Sys.setenv(MIRO_DB_PATH = testDir)
# END setup


modelToTest <- "pickstock_with_data"
testModelDir <- file.path(testDir, "model", modelToTest)
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))

testModelPath <- file.path(getwd(), "..", "model", modelToTest)
Sys.setenv(MIRO_MODE="base")

configJSONFileName <- file.path(testModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
file.copy(configJSONFileName, file.path(dirname(configJSONFileName), 
                                        paste0(modelToTest, "_tmp.json")), overwrite = TRUE)

oldValueboxConfigJSON <- file.path(testModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_valuebox_old.json"))
newValueboxConfigJSON <- file.path(testModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_valuebox_new.json"))

file.copy(oldValueboxConfigJSON, file.path(dirname(configJSONFileName),
                                        paste0(modelToTest, ".json")), overwrite = TRUE)

context("UI tests - Old valuebox settings")
test_that("Valueboxes with old (global) configuration works",
          expect_pass(testApp(file.path(testDir, ".."), "valuebox_old_config_test",
                              compareImages = FALSE)))

file.copy(newValueboxConfigJSON, file.path(dirname(configJSONFileName),
                                           paste0(modelToTest, ".json")), overwrite = TRUE)

context("UI tests - new valuebox settings")
test_that("Valueboxes with new (individual) configuration works",
          expect_pass(testApp(file.path(testDir, ".."), "valuebox_new_config_test",
                              compareImages = FALSE)))

file.rename(file.path(dirname(configJSONFileName), paste0(modelToTest, "_tmp.json")),
            file.path(dirname(configJSONFileName), paste0(modelToTest, ".json")))


Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))