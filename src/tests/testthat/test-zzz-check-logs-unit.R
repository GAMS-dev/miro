context("Unit tests - check log files")

validFatals <- c("GAMS symbol: 'd': Invalid column name(s): 'bla' in configuration for dropdown columns",
                 "The column: 'i' of the GAMS symbol: 'd': cannot be declared both as a pivot column and a drop-down column",
                 "The column(s): 'value2' of table: 'd' specified in colFormat does not exist.",
                 "colFormat is not supported when pivotCols are active (table: d).")
validErrors <- c("Confirm download of temporary files button clicked without any files specified.")
validWarnings <- c()

logFilePath <- Sys.getenv("MIRO_LOG_PATH")

logFiles <- list.files(logFilePath, pattern = ".log$", full.names = TRUE)

for(logFile in logFiles){
  logContent <- readr::read_lines(logFile)
  fatalsInLog <- startsWith(logContent, "FATAL")
  errorMsg <- strsplit(logContent[fatalsInLog], "] ", fixed = TRUE)
  errorMsg <- vapply(seq_along(errorMsg), function(lineIdx){
    if(length(errorMsg[[lineIdx]]) > 1L){
      return(errorMsg[[lineIdx]][2])
    }
    return(logContent[[which(fatalsInLog)[lineIdx] + 1L]])
  }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
  for(i in seq_along(validFatals)){
    falsePositives <- startsWith(errorMsg, validFatals[i])
    errorMsg <- errorMsg[!falsePositives]
    if(any(falsePositives)){
      validFatals <- validFatals[-i]
    }
  }
  test_that(sprintf("Log file: %s contains no fatal errors.", basename(logFile)),
            expect_false(length(errorMsg) > 0L))
  if(length(errorMsg)){
    print(sprintf("FATALS in: %s", basename(logFile)))
    print(errorMsg)
  }
  errorsInLog <- startsWith(logContent, "ERROR")
  errorMsg <- strsplit(logContent[errorsInLog], "] ", fixed = TRUE)
  errorMsg <- vapply(seq_along(errorMsg), function(lineIdx){
    if(length(errorMsg[[lineIdx]]) > 1L){
      return(errorMsg[[lineIdx]][2])
    }
    return(logContent[[which(errorsInLog)[lineIdx] + 1L]])
  }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
  for(i in seq_along(validErrors)){
    falsePositives <- startsWith(errorMsg, validErrors[i])
    errorMsg <- errorMsg[!falsePositives]
    if(any(falsePositives)){
      validErrors <- validErrors[-i]
    }
  }
  test_that(sprintf("Log file: %s contains no errors.", basename(logFile)),
            expect_false(length(errorMsg) > 0L))
  if(length(errorMsg)){
    print(sprintf("ERRORS in: %s", basename(logFile)))
    print(errorMsg)
  }
  warningsInLog <- startsWith(logContent, "WARN")
  errorMsg <- strsplit(logContent[warningsInLog], "] ", fixed = TRUE)
  errorMsg <- vapply(seq_along(errorMsg), function(lineIdx){
    if(length(errorMsg[[lineIdx]]) > 1L){
      return(errorMsg[[lineIdx]][2])
    }
    return(logContent[[which(warningsInLog)[lineIdx] + 1L]])
  }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
  for(i in seq_along(validWarnings)){
    falsePositives <- startsWith(errorMsg, validWarnings[i])
    errorMsg <- errorMsg[!falsePositives]
    if(any(falsePositives)){
      validWarnings <- validWarnings[-i]
    }
  }
  test_that(sprintf("Log file: %s contains no warnings.", basename(logFile)),
            expect_false(length(errorMsg) > 0L))
  if(length(errorMsg)){
    print(sprintf("WARNINGS in: %s", basename(logFile)))
    print(errorMsg)
  }
}
