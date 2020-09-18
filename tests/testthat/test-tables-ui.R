context("UI tests - Table settings")

testDir <- file.path(getwd(), "..")

if(file.exists(file.path(testDir, "miro.sqlite3"))){
  if(unlink(file.path(testDir, "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
Sys.setenv(MIRO_DB_PATH = testDir)
# END setup

Sys.setenv(MIRO_MODEL_PATH = file.path(getwd(), "..", "model", "pickstock_output_tables",
                                       "pickstock_output_tables.gms"))
Sys.setenv(MIRO_MODE="base")

test_that("Output table config works.",
          expect_pass(testApp(file.path(testDir, ".."), "output_table_settings",
                              compareImages = FALSE)))

test_that("Input table markdown works",
          expect_pass(testApp(file.path(testDir, ".."), "input_table_markdown_test",
                              compareImages = FALSE)))

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
