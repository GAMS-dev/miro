library(testthat)
library(shinytest)

modelsToTest <- c("pickstock", "pickstock_live",
                  "transport", "transport_live",
                  "kport")

# BEGIN setup
options(testthat.output_file = "test-out.xml")
options(testthat.default_reporter = "junit")

Sys.setenv(SHINYTEST = "yes")

if(file.exists("sqlitetests.sqlite3")){
  if(unlink("sqlitetests.sqlite3", force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
if(!file.rename("conf/db_config.json", "conf/db_config_prod.json")){
  stop("Could not rename db_config file")
}
if(!file.copy("tests/data/db_config.json", "conf/db_config.json", overwrite = FALSE)){
  stop("Could not copy database config file for tests")
}
# END setup

errMsg <- NULL
testFiles <- c("excel_upload_test", "excel_upload_overwrite_test", 
               "load_from_db_test", "gams_interrupt_test",
               "compare_scen_split_test", "compare_scen_tab_test")
for(modelToTest in modelsToTest){
  Sys.setenv(GMSMODELNAME = modelToTest)
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

# BEGIN teardown
Sys.setenv(SHINYTEST = "")
Sys.setenv(GMSMODELNAME = "")
if(!file.remove("conf/db_config.json")){
  stop("Could not remove database config file for tests")
}
if(!file.rename("conf/db_config_prod.json", "conf/db_config.json")){
  stop("Could not rename production database config file")
}
if(!file.remove("sqlitetests.sqlite3")){
  stop("Could not remove database SQLite file for tests")
}
if(!is.null(errMsg)){
  stop(errMsg)
}
# END teardown
