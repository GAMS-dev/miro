context("UI tests - Removing duplicates works")

createTestDb()

miroModelDir <- file.path(testDir, "..", "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, "transport.gms"))
Sys.setenv(GMSMODELNAME = "transport")

test_that(
  "Removing duplicate records works",
  {
    source(file.path(testDir, "shinytest", "remove_duplicates_test.R"), local = TRUE)
  }
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))
