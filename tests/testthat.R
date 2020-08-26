library("testthat")
library("stringi")
library("tibble")
library("R6")
library("shinytest")

source("global.R")
source(file.path("tests", "util.R"))
source(file.path("components", "util.R"))

if(!dependenciesInstalled()){
  installDependencies()
}

test_file("tests/testthat/test-views-ui.R")
#test_dir("tests/testthat")
