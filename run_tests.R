RLibPath <- Sys.getenv("LIB_PATH")
if(!identical(RLibPath, "")) {
  .libPaths( c( RLibPath, .libPaths()) )
}

options(testthat.output_file = "test-out.xml")
options(testthat.default_reporter = "junit")

source(file.path("tests", "testthat.R"))
