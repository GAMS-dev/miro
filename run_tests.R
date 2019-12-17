library(testthat)
library(shinytest)

if(!dependenciesInstalled()){
  installDependencies()
}

modelsToTest <- c()

# BEGIN setup
options(testthat.output_file = "test-out.xml")
options(testthat.default_reporter = "junit")

if(file.exists(file.path("tests", "miro.sqlite3"))){
  if(unlink(file.path("tests", "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
Sys.setenv(DBPATH = file.path(getwd(), "tests"))
# END setup

errMsg <- NULL
testFiles <- c("excel_upload_test", "excel_upload_overwrite_test", 
               "load_from_db_test", "gams_interrupt_test",
               "compare_scen_split_test", "compare_scen_tab_test")
for(modelToTest in modelsToTest){
  Sys.setenv(MIRO_MODEL_PATH = file.path(".", "model", modelToTest, paste0(modelToTest, ".gms")))
  for(testFile in testFiles){
    file.copy(paste0("tests/", testFile, ".R"), 
              paste0("tests/", testFile, "_", modelToTest, ".R"), overwrite = TRUE)
  }
  tryCatch(
    test_that(paste0("Functional tests pass for model: ", modelToTest), {
      expect_pass(testApp(".", paste0("excel_upload_test_", modelToTest), 
                          compareImages = FALSE))
      expect_pass(testApp(".", paste0("excel_upload_overwrite_test_", modelToTest), 
                          compareImages = FALSE))
      expect_pass(testApp(".", paste0("load_from_db_test_", modelToTest), 
                          compareImages = FALSE))
      expect_pass(testApp(".", paste0("gams_interrupt_test_", modelToTest), 
                          compareImages = FALSE))
      expect_pass(testApp(".", paste0("compare_scen_split_test_", modelToTest), 
                          compareImages = FALSE))
      expect_pass(testApp(".", paste0("compare_scen_tab_test_", modelToTest), 
                          compareImages = FALSE))
    })
    , error = function(e){
      errMsg <<- e
    })
}
source(file.path("tests", "testthat.R"))

# BEGIN teardown
if(!is.null(errMsg)){
  stop(errMsg)
}
# END teardown
