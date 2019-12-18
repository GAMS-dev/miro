context("Functional UI tests")
library(shinytest)

if(!dependenciesInstalled()){
  installDependencies()
}
RLibPath <- Sys.getenv("LIB_PATH")
if(!identical(RLibPath, "")) {
  .libPaths( c( RLibPath, .libPaths()) )
}

modelsToTest <- c("pickstock", "transport")
testDir <- file.path(getwd(), "..")

if(file.exists(file.path(testDir, "miro.sqlite3"))){
  if(unlink(file.path(testDir, "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
Sys.setenv(DBPATH = testDir)
# END setup

errMsg <- NULL
testFiles <- c("excel_upload_test", "excel_upload_overwrite_test", 
               "load_from_db_test", "gams_interrupt_test",
               "compare_scen_split_test", "compare_scen_tab_test")
for(modelToTest in modelsToTest){
  Sys.setenv(MIRO_MODEL_PATH = file.path(testDir, "..", "model", 
                                         modelToTest, paste0(modelToTest, ".gms")))
  Sys.setenv(GMSMODELNAME = modelToTest)
  
  for(testFile in testFiles){
    file.copy(paste0(testDir, .Platform$file.sep, testFile, ".R"), 
              paste0(testDir, .Platform$file.sep, testFile, "_", modelToTest, ".R"), overwrite = TRUE)
    
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