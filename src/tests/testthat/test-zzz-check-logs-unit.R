context("Unit tests - check log files")

logFilePath <- Sys.getenv("MIRO_LOG_PATH")

logFiles <- list.files(logFilePath, pattern = ".log$", full.names = TRUE)

for(logFile in logFiles){
  logContent <- readr::read_lines(logFile)
  errorsInLog <- startsWith(logContent, "ERROR") | startsWith(logContent, "FATAL")
  test_that(sprintf("Log file: %s contains no errors.", basename(logFile)),
            expect_false(any(errorsInLog)))
  if(any(errorsInLog)){
    print(logContent[errorsInLog])
  }
  warningsInLog <- startsWith(logContent, "WARN")
  test_that(sprintf("Log file: %s contains no warnings.", basename(logFile)),
            expect_false(any(warningsInLog)))
  if(any(warningsInLog)){
    print(logContent[warningsInLog])
  }
}
