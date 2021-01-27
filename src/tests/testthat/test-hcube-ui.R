context("UI tests - Hypercube mode")

testDir <- file.path(getwd(), "..")

createTestDb()
Sys.setenv(MIRO_DB_PATH = testDir)

modelToTest <- "pickstock_configuration"
testModelDir <- file.path(testDir, "model", modelToTest)
modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
configJSONFileName <- file.path(testModelDir, paste0("conf_", modelToTest),
                                paste0(modelToTest, ".json"))
# END setup

Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
Sys.setenv(MIRO_MODEL_NAME = modelToTest)
Sys.setenv(MIRO_MODE="hcube")


#activate local upload module, deactivate 
file.copy(file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_expected.json")), 
          file.path(dirname(configJSONFileName),paste0(tolower(modelToTest), ".json")), overwrite = TRUE)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName, simplifyDataFrame = FALSE, 
                                                  simplifyMatrix = FALSE))
configJSON$activateModules$loadLocal <- TRUE
configJSON$activateModules$attachments <- TRUE
configJSON$extraClArgs <- c(configJSON$extraClArgs, "--sleep=1")
gamsliceFile <- file.path(path.expand("~"), ".local", "share", "GAMS", "gamslice.txt")
if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "") && !file.exists(gamsliceFile)){
  if(!grepl("linux-gnu", R.version$os)) stop("Not on Linux")
  file.copy2(Sys.getenv("MIRO_TEST_GAMS_LICE"), gamsliceFile)
  on.exit(unlink(gamsliceFile), add = TRUE)
}

configJSON$inputWidgets[["_gmspar_sliderrange"]]$noHcube <- FALSE
configJSON$inputWidgets[["trainingdays"]]$noHcube <- TRUE
configJSON$inputWidgets[["_gmsopt_LstTitleLeftAligned"]] <- configJSON$inputWidgets[["_gmsopt_checkbox"]]
configJSON$inputWidgets[["_gmsopt_checkbox"]] <- NULL
configJSON$outputAttachments <- list(list(filename = "dowjones2016.csv",
                                     execPerm = TRUE, throwError = FALSE))

configJSON$scripts$hcube <- list(list(title = "Test analysis",
                                      id = "script1",
                                      command = "gams",
                                      args = c("test_script.gms", "--testVar", "test"),
                                      outputFile = "out.txt"))

jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

test_that("Hypercube mode works",
          expect_pass(testApp(file.path(testDir, ".."), "hcube_test",
                              compareImages = FALSE)))

context("UI tests - Hypercube mode Engine")
skip_if(identical(Sys.getenv("ENGINE_URL"), ""),
        "Skipping asynchronous solve tests as no ENGINE_URL was not set.")
skip_if(identical(Sys.getenv("ENGINE_USER"), ""),
        "Skipping asynchronous solve tests as no ENGINE_USER was not set.")
skip_if(identical(Sys.getenv("ENGINE_PASSWORD"), ""),
        "Skipping asynchronous solve tests as no ENGINE_PASSWORD was not set.")
skip_if(identical(Sys.getenv("ENGINE_NS"), ""),
        "Skipping asynchronous solve tests as no ENGINE_NS was not set.")

Sys.setenv(MIRO_REMOTE_EXEC = "true")
test_that("Remote (Engine) Hypercube mode works",
          expect_pass(testApp(file.path(testDir, ".."), "hcube_engine_test",
                              compareImages = FALSE)))

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_REMOTE_EXEC"))
