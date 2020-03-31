context("Functional UI tests")
library(shinytest)

if(!dependenciesInstalled()){
  installDependencies()
}

modelsToTest <- c("pickstock", "transport")
testDir <- file.path(getwd(), "..")

if(file.exists(file.path(testDir, "miro.sqlite3"))){
  if(unlink(file.path(testDir, "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
Sys.setenv(MIRO_DB_PATH = testDir)
# END setup

errMsg <- NULL
testFiles <- c("excel_upload_test", "excel_upload_overwrite_test", 
               "load_from_db_test", "gams_interrupt_test",
               "compare_scen_split_test", "compare_scen_tab_test")
for(modelToTest in modelsToTest){
  testModelPath <- file.path(testDir, "model", modelToTest)
  Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath,
                                         paste0(modelToTest, ".gms")))
  Sys.setenv(GMSMODELNAME = modelToTest)
  Sys.setenv(MIRO_MODE="base")

  for(testFile in testFiles){
    file.copy(paste0(testDir, .Platform$file.sep, testFile, ".R"),
              paste0(testDir, .Platform$file.sep, testFile, "_", modelToTest, ".R"), overwrite = TRUE)

  }
  if(dir.exists(file.path(testModelPath, paste0("data_", modelToTest))) &&
     unlink(file.path(testModelPath, paste0("data_", modelToTest)),
            recursive = TRUE, force = TRUE) != 0L){
    warning(sprintf("Couldn't remove data dir of model: '%s'", modelToTest))
  }
  test_that(paste0("Uploading Excel file works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("excel_upload_test_", modelToTest),
                        compareImages = FALSE)))
  test_that(paste0("Uploading Excel file and overwriting data works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("excel_upload_overwrite_test_", modelToTest),
                        compareImages = FALSE)))
  test_that(paste0("Loading and saving data in db works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("load_from_db_test_", modelToTest),
                        compareImages = FALSE)))
  test_that(paste0("Comparing scenarios in split screen mode works for model: ", modelToTest),
            expect_pass(testApp(file.path(testDir, ".."), paste0("compare_scen_split_test_", modelToTest),
                                compareImages = FALSE)))
  test_that(paste0("Comparing scenarios in tab view mode works for model: ", modelToTest),
            expect_pass(testApp(file.path(testDir, ".."), paste0("compare_scen_tab_test_", modelToTest),
                                compareImages = FALSE)))
}
testModelPath <- file.path(testDir, "model", "transport_custom_map")
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath,
                                       "transport_custom_map.gms"))
Sys.setenv(GMSMODELNAME = "transport_custom_map")
Sys.setenv(MIRO_MODE="base")

test_that("Custom renderers with multiple (hidden) datasets work",
          expect_pass(testApp(file.path(testDir, ".."), "multiple_symbol_renderer",
                              compareImages = FALSE)))
if(identical(Sys.getenv("GAMS_SYS_DIR"), "")){
  message("GAMS_SYS_DIR environment variable not set. Skipping GAMS tests.")
}else{
  additionalGamsClArgs <- character(0L)
  if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")){
    additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
  }
  for(modelToTest in c("pickstock", "transport", "sudoku", "farming", "inscribedsquare", "tsp", "cpack")){
    miroModelDir <- file.path(getwd(), "..", "..", "model", modelToTest)
    Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir,  paste0(modelToTest, ".gms")))
    Sys.setenv(GMSMODELNAME = modelToTest)
    if(modelToTest %in% c("pickstock")){
      additionalGamsClArgs <- c(additionalGamsClArgs, "MIP=CBC")
    }
    if(length(additionalGamsClArgs)){
      configJSONFileName <- file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
      file.copy(configJSONFileName,
                file.path(dirname(configJSONFileName), paste0(modelToTest, "_tmp.json")), overwrite = TRUE)
      configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName, 
                                                        simplifyDataFrame = FALSE, 
                                                        simplifyMatrix = FALSE))
      configJSON$extraClArgs <- c(configJSON$extraClArgs, additionalGamsClArgs)
      jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
    }
    for(testFile in c("solve_model_test")){
      file.copy(paste0(testDir, .Platform$file.sep, testFile, ".R"),
                paste0(testDir, .Platform$file.sep, testFile, "_", modelToTest, ".R"), overwrite = TRUE)
      
    }
    test_that(sprintf("Example app: '%s' solves: ", modelToTest),
              expect_pass(testApp(file.path(testDir, ".."), paste0("solve_model_test_", modelToTest),
                                  compareImages = FALSE)))
    if(length(additionalGamsClArgs) && !modelToTest %in% c("pickstock")){
      file.rename(file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_tmp.json")),
                  file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json")))
    }
  }
  Sys.setenv(MIRO_MODEL_PATH = file.path(getwd(), "..", "..", "model", "pickstock",
                                         "pickstock.gms"))
  test_that("Interupting model run works.",
            expect_pass(testApp(file.path(testDir, ".."), "interrupt_model_test",
                                compareImages = FALSE)))
  file.rename(file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_tmp.json")),
              file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json")))
  Sys.unsetenv(c("MIRO_MODEL_PATH", "GMSMODELNAME", "MIRO_DB_PATH", "MIRO_MODE"))
}
