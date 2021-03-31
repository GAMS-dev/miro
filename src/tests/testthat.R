RLibPath <- Sys.getenv("LIB_PATH")
if(!identical(RLibPath, "")) {
  .libPaths( c( RLibPath, .libPaths()) )
}
print(sessionInfo())

print("Library paths:")
print(.libPaths())
print("Home directory:")
print(path.expand("~"))
print("Working directory:")
print(getwd())

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
    ProgressReporter$new(max_failures = 100),
    JunitReporter$new(file = "test-out.xml")
))

stopOnFailure <- identical(commandArgs(trailingOnly=TRUE), "--stop")

#test_file("tests/testthat/test-db-migrator-unit.R", reporter = reporter)
test_dir("tests/testthat", reporter = reporter, stop_on_failure = stopOnFailure)
