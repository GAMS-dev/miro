test_that(
  "Output attachments work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    skip_if(
      identical(Sys.getenv("GAMS_SYS_DIR"), ""),
      "Skipping output attachments tests as GAMS_SYS_DIR was not set."
    )

    createTestDb()

    additionalGamsClArgs <- character(0L)
    miroModelDir <- file.path(testDir, "model", "transport_outputAttach")
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
      saveAdditionalGamsClArgs(miroModelDir, "transport", additionalGamsClArgs)
    }
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testDir, "model", "transport_outputAttach",
      "transport.gms"
    ))
    if (file.exists(file.path(
      getwd(), "..", "model", "transport_outputAttach",
      "report.put"
    ))) {
      unlink(file.path(
        getwd(), "..", "model", "transport_outputAttach",
        "report.put"
      ), force = TRUE)
    }
    source(file.path(testDir, "shinytest", "output_attach_test.R"), local = TRUE)
    source(file.path(testDir, "shinytest", "output_attach_test_2.R"), local = TRUE)
    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH"))
  })
)
