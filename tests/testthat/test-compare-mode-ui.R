context("UI tests - pivot compare mode")

testDir <- file.path(getwd(), "..")

if(file.exists(file.path(testDir, "miro.sqlite3"))){
  if(unlink(file.path(testDir, "miro.sqlite3"), force = TRUE)){
    stop("Could not remove old database SQLite file for tests")
  }
}
Sys.setenv(MIRO_DB_PATH = testDir)
testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath,
                                       "transport.gms"))
Sys.setenv(MIRO_MODE="base")

file.copy2(file.path(testModelPath, "conf_transport", "transport.json"),
           file.path(testModelPath, "conf_transport", "transport_copy.json"))
test_that("Default compare mode works",
          expect_pass(testApp(file.path(testDir, ".."), "default_compare_test",
                              compareImages = FALSE)))

modelDataPath <- file.path(testModelPath, "data_transport")

file.copy2(file.path(testDir, "data", "transport.gdx"),
           file.path(modelDataPath, "default.gdx"))
file.copy2(file.path(testDir, "data", "transport.gdx"),
           file.path(modelDataPath, "default2.gdx"))
file.copy2(file.path(testDir, "data", "transport.gdx"),
           file.path(modelDataPath, "default3.gdx"))
file.copy2(file.path(testDir, "data", "transport.gdx"),
           file.path(modelDataPath, "default4.gdx"))
test_that("Loading scenarios works",
          expect_pass(testApp(file.path(testDir, ".."), "load_pivot_compare_test",
                              compareImages = FALSE)))
unlink(modelDataPath, recursive = TRUE, force = TRUE)

file.move(file.path(testModelPath, "conf_transport", "transport_copy.json"),
          file.path(testModelPath, "conf_transport", "transport.json"))

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
