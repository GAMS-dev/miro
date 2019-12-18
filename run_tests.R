library(testthat)

options(testthat.output_file = "test-out.xml")
options(testthat.default_reporter = "junit")

RLibPath <- Sys.getenv("LIB_PATH")
if(!identical(RLibPath, "")) {
  .libPaths( c( RLibPath, .libPaths()) )
}

source(file.path("tests", "testthat.R"))
