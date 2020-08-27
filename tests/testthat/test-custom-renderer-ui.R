context("UI tests - Custom renderers")

testDir <- file.path(getwd(), "..")

if(file.exists(file.path(testDir, "miro.sqlite3"))){
  if(unlink(file.path(testDir, "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
Sys.setenv(MIRO_DB_PATH = testDir)
# END setup

testModelPath <- file.path(testDir, "model", "transport_custom_map")
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath,
                                       "transport_custom_map.gms"))
Sys.setenv(MIRO_MODE="base")

test_that("Custom renderers with multiple (hidden) datasets work",
          expect_pass(testApp(file.path(testDir, ".."), "multiple_symbol_renderer",
                              compareImages = FALSE)))

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
