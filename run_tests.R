RLibPath <- Sys.getenv("LIB_PATH")
if(!identical(RLibPath, "")) {
  .libPaths( c( RLibPath, .libPaths()) )
}

options(testthat.output_file = "test-out.xml")
options(testthat.default_reporter = "junit")

if(.Platform$OS.type != 'windows'){
  Sys.setlocale("LC_ALL","en_US.UTF-8")
}

print(sessionInfo())

source(file.path("tests", "testthat.R"))
