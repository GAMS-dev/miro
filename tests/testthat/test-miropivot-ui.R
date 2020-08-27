context("UI tests - MIRO Pivot")

testDir <- file.path(getwd(), "..")

if(file.exists(file.path(testDir, "miro.sqlite3"))){
  if(unlink(file.path(testDir, "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
Sys.setenv(MIRO_DB_PATH = testDir)
# END setup

Sys.setenv(MIRO_MODEL_PATH = file.path(getwd(), "..", "model", "transport_miropivot",
                                       "transport_miropivot.gms"))
Sys.setenv(MIRO_MODE="base")

test_that("MIRO Pivot renderer works.",
          expect_pass(testApp(file.path(testDir, ".."), "miropivot_test",
                              compareImages = FALSE)))

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
