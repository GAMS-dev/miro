library(testthat)

options(testthat.output_file = "test-out.xml")
options(testthat.default_reporter = "junit")


source(file.path("tests", "testthat.R"))
