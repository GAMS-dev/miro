RLibPath <- Sys.getenv("LIB_PATH")
if(!identical(RLibPath, "")) {
  .libPaths( c( RLibPath, .libPaths()) )
}
print(sessionInfo())

library("testthat")
library("stringi")
library("tibble")
library("R6")
library("shinytest")

source("global.R")
source(file.path("tests", "util.R"))
source(file.path("components", "js_util.R"))
source(file.path("components", "util.R"))

if(!dependenciesInstalled()){
  installDependencies()
}

reporter <- MultiReporter$new(list(
    ProgressReporter$new(),
    JunitReporter$new(file = "test-out.xml")
))

test_file("tests/testthat/test-attachments-unit.R", reporter = reporter)
#test_dir("tests/testthat", reporter = reporter)