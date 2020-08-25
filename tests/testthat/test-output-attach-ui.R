context("UI tests - Output attachments")

testDir <- file.path(getwd(), "..")

if(file.exists(file.path(testDir, "miro.sqlite3"))){
  if(unlink(file.path(testDir, "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
if(identical(Sys.getenv("GAMS_SYS_DIR"), "")){
  message("GAMS_SYS_DIR environment variable not set. Skipping tests.")
}else{
  additionalGamsClArgs <- character(0L)
  miroModelDir <- file.path(testDir, "model", "pickstock_configuration")
  if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")){
    additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
    saveAdditionalGamsClArgs(miroModelDir, modelToTest, additionalGamsClArgs)
  }
  Sys.setenv(MIRO_DB_PATH = testDir)
  Sys.setenv(MIRO_MODEL_PATH = file.path(testDir, "model", "transport_outputAttach",
                                         "transport.gms"))
  if(file.exists(file.path(Sys.getenv("MIRO_DB_PATH"), "miro.sqlite3"))){
    if(unlink(file.path(Sys.getenv("MIRO_DB_PATH"), "miro.sqlite3"), force = TRUE)){
      stop("Could not remove old database SQLite file for tests")
    }
  }
  if(file.exists(file.path(getwd(), "..", "model", "transport_outputAttach",
                           "report.put"))){
    unlink(file.path(getwd(), "..", "model", "transport_outputAttach",
                     "report.put"), force = TRUE)
  }
  test_that("Output attachments work (part 1)",
            expect_pass(testApp(file.path(testDir, ".."), "output_attach_test",
                                compareImages = FALSE)))
  test_that("Output attachments work (part2)",
            expect_pass(testApp(file.path(testDir, ".."), "output_attach_test_2",
                                compareImages = FALSE)))
  Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH"))
}
