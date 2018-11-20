library(testthat)
library(shinytest)

Sys.setenv(SHINYPROXY_USERNAME = "test", SHINYPROXY_USERGROUPS = "test", SHINYPROXY_MODELNAME = "pickstock")

# BEGIN setup
options(testthat.output_file = "test-out.xml")
options(testthat.default_reporter = "junit")
if(file.exists("test.sqlite3")){
  if(!unlink("test.sqlite3", force = TRUE)){
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


test_that("Functional tests pass", {
  expect_pass(testApp(".", "excel_upload", compareImages = FALSE))
  expect_pass(testApp(".", "excel_upload_overwrite", compareImages = FALSE))
  expect_pass(testApp(".", "load_from_db", compareImages = FALSE))
})

# BEGIN teardown
if(!file.remove("conf/db_config.json")){
  stop("Could not remove database config file for tests")
}
if(!file.rename("conf/db_config_prod.json", "conf/db_config.json")){
  stop("Could not rename production database config file")
}
if(!file.remove("test.sqlite3")){
  stop("Could not remove database SQLite file for tests")
}
# END teardown
