context("UI tests - asynchronous solve")

testDir <- file.path(getwd(), "..")

createTestDb()

modelToTest <- "transport"
testModelDir <- file.path(testDir, "model", modelToTest)
Sys.setenv(MIRO_DB_PATH = testDir)
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
Sys.setenv(MIRO_MODE="base")
Sys.setenv(MIRO_REMOTE_EXEC = "true")

testModelPath <- file.path(getwd(), "..", "model", modelToTest)
modelDataPath <- file.path(testModelPath, paste0("data_", modelToTest))

#add --sleep=1 to extraClArgs and hide log and lst file
configJSONFileName <- file.path(testModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
file.copy(configJSONFileName, file.path(dirname(configJSONFileName), 
                                        paste0(modelToTest, "_tmp.json")), overwrite = TRUE)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName, simplifyDataFrame = FALSE, 
                                                  simplifyMatrix = FALSE))
configJSON$extraClArgs <- c(configJSON$extraClArgs, "--sleep=1")
configJSON$activateModules$logFile <- FALSE
configJSON$activateModules$lstFile <- FALSE
jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
file.copy2(file.path(testDir, "data", "transport.gdx"), file.path(modelDataPath, "default.gdx"))

if(file.exists(file.path("~", ".miro", paste0(".cred_", modelToTest)))){
  unlink(file.path("~", ".miro", paste0(".cred_", modelToTest)), force = TRUE)
}

test_that("Solve asynchronously with GAMS MIRO Engine works",
          expect_pass(testApp(file.path(testDir, ".."), "async_solve_test",
                              compareImages = FALSE)))

file.rename(file.path(dirname(configJSONFileName), paste0(modelToTest, "_tmp.json")),
            file.path(dirname(configJSONFileName), paste0(modelToTest, ".json")))

unlink(modelDataPath, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_REMOTE_EXEC"))
