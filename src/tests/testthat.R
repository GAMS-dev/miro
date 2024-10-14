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
library("dplyr")
library("tidyr")
library("R6")
library("shinytest2")
library("futile.logger")

source("global.R")
source(file.path("tests", "util.R"))
source(file.path("components", "js_util.R"))
source(file.path("components", "util.R"))

if (dir.exists(logPathTests) && !identical(unlink(logPathTests, recursive = TRUE), 0L)) {
  stop(
    sprintf(
      "Can't remove existing log file directory: '%s'. Do you lack write permissions?",
      logPathTests
    ),
    call. = FALSE
  )
}
if (!dir.create(logPathTests)) {
  stop(
    sprintf(
      "Can't create log file directory: '%s'. Do you lack write permissions?",
      logPathTests
    ),
    call. = FALSE
  )
}

Sys.setenv(MIRO_LOG_PATH = logPathTests)
Sys.setenv(MIRO_TEST_LOAD_TIMEOUT = "30000")
Sys.setenv(MIRO_TEST_TIMEOUT = "10000")
Sys.setenv(NOT_CRAN = "true")
if (Sys.info()[["sysname"]] == "Darwin") {
  # need to set chromium path manually until https://github.com/rstudio/chromote/issues/91 is closed
  chromePath <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
  if (!file.exists(chromePath)) {
    chromePath <- Sys.which("chromium-browser")
  }
  if (nchar(chromePath) == 0) {
    chromePath <- Sys.which("chromium")
  }
  if (nchar(chromePath) == 0) {
    message("Google Chrome and Chromium were not found. Try setting the `CHROMOTE_CHROME` environment variable.")
    stop()
  }
  Sys.setenv(CHROMOTE_CHROME = chromePath)
}

reporter <- MultiReporter$new(list(
  ProgressReporter$new(max_failures = as.integer(Sys.getenv("MIRO_MAX_TEST_FAILURES", "1"))),
  JunitReporter$new(file = file.path(getwd(), "test-out.xml"))
))

stopOnFailure <- identical(commandArgs(trailingOnly = TRUE), "--stop") || !identical(Sys.getenv("FORCE_RELEASE"), "yes")
testDir <- file.path(getwd(), "tests")

# test_file("tests/testthat/test-hcube-module-ui.R", reporter = reporter, stop_on_failure = stopOnFailure)
test_dir("tests/testthat", reporter = reporter, stop_on_failure = stopOnFailure)

Sys.unsetenv(c("MIRO_LOG_PATH", "MIRO_TEST_TIMEOUT", "MIRO_TEST_LOAD_TIMEOUT"))
