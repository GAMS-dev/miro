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
Sys.setenv(CHROMOTE_HEADLESS = "new")
totalNodes <- as.integer(Sys.getenv("CI_NODE_TOTAL", "1"))
nodeIndex <- as.integer(Sys.getenv("CI_NODE_INDEX", "1"))
platform <- Sys.info()[["sysname"]]
testDir <- file.path(getwd(), "tests")
testThatDir <- file.path(testDir, "testthat")
allFiles <- sort(list.files(testThatDir, pattern = "^test-.*\\.R$", full.names = FALSE))
nFiles <- length(allFiles)
if (nFiles == 0) {
  cat("No test files found. Exiting.\n")
  quit(status = 0)
}
filesPerNode <- ceiling(nFiles / totalNodes)
startIndex <- (nodeIndex - 1) * filesPerNode + 1
endIndex <- min(nodeIndex * filesPerNode, nFiles)
if (startIndex > nFiles) {
  cat("No files for this node to test. Exiting successfully.\n")
  quit(status = 0)
}
filesToRun <- allFiles[startIndex:endIndex]
if (!"test-zzz-check-logs-unit.R" %in% filesToRun) {
  # always check logs
  filesToRun <- c(filesToRun, "test-zzz-check-logs-unit.R")
}
cat("This node will run the following test files:\n")
print(filesToRun)
regexFilter <- paste0("^", gsub("\\.", "\\\\.", gsub("\\.R$", "", gsub("^test-", "", filesToRun))), "$", collapse = "|")
if (platform == "Darwin") {
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

if (is.na(Sys.getenv("GAMSPY_INTERPRETER_PATH", NA_character_))) {
  runGAMSPyTests <- FALSE
} else {
  runGAMSPyTests <- TRUE
  Sys.setenv(PYTHON_EXEC_PATH = Sys.getenv("GAMSPY_INTERPRETER_PATH"))
}

if (!is.na(Sys.getenv("MIRO_TEST_GAMS_LICENSE_KEY", NA_character_))) {
  gamsLicFullPath <- file.path(getwd(), "gamslice.txt")
  if (runGAMSPyTests) {
    processx::run("gamspy", c(
      "install",
      "license", Sys.getenv("MIRO_TEST_GAMS_LICENSE_KEY")
    ))
  }
  proc <- processx::run(
    file.path(Sys.getenv("GAMS_SYS_DIR"), "gamsgetkey"),
    Sys.getenv("MIRO_TEST_GAMS_LICENSE_KEY")
  )
  writeLines(proc$stdout, gamsLicFullPath)
  Sys.setenv(MIRO_TEST_GAMS_LICE = gamsLicFullPath)
}
maxTestFailures <- as.integer(Sys.getenv("MIRO_MAX_TEST_FAILURES", "1"))

reporter <- MultiReporter$new(list(
  ProgressReporter$new(max_failures = maxTestFailures),
  JunitReporter$new(file = file.path(getwd(), "test-out.xml"))
))

stopOnFailure <- identical(commandArgs(trailingOnly = TRUE), "--stop") || !identical(Sys.getenv("FORCE_RELEASE"), "yes")

print(sprintf("Maximum test failures: %i, stop on failure: %s", maxTestFailures, stopOnFailure))

# test_file("tests/testthat/test-hcube-module-ui.R", reporter = reporter, stop_on_failure = stopOnFailure, stop_on_warning = stopOnFailure)
test_dir("tests/testthat",
  filter = regexFilter,
  reporter = reporter,
  stop_on_failure = stopOnFailure,
  stop_on_warning = stopOnFailure
)

Sys.unsetenv(c("MIRO_LOG_PATH", "MIRO_TEST_TIMEOUT", "MIRO_TEST_LOAD_TIMEOUT"))
