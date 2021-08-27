context("UI tests - Output attachments")
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
test_that(
  "Output attachments work (part 1)",
  expect_pass(testApp(file.path(testDir, ".."), "output_attach_test",
    compareImages = FALSE
  ))
)
test_that(
  "Output attachments work (part2)",
  expect_pass(testApp(file.path(testDir, ".."), "output_attach_test_2",
    compareImages = FALSE
  ))
)
Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH"))
