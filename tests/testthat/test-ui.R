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