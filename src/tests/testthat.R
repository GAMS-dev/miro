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
logPathTests <- file.path(getwd(), "tests", "logs-tests")
print("Log file directory:")
print(logPathTests)

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

if(dir.exists(logPathTests) && !identical(unlink(logPathTests, recursive = TRUE), 0L)){
  stop(sprintf("Can't remove existing log file directory: '%s'. Do you lack write permissions?",
               logPathTests),
       call. = FALSE)
}
if(!dir.create(logPathTests)){
  stop(sprintf("Can't create log file directory: '%s'. Do you lack write permissions?",
               logPathTests),
       call. = FALSE)
}

Sys.setenv(MIRO_LOG_PATH = logPathTests)

reporter <- MultiReporter$new(list(
  ProgressReporter$new(max_failures = 100),
  JunitReporter$new(file = file.path(getwd(), "test-out.xml"))
))

stopOnFailure <- identical(commandArgs(trailingOnly=TRUE), "--stop")
testDir <- file.path(getwd(), "tests")

#test_file("tests/testthat/test-attachments-ui.R", reporter = reporter)
test_dir("tests/testthat", reporter = reporter, stop_on_failure = stopOnFailure)

Sys.unsetenv("MIRO_LOG_PATH")
