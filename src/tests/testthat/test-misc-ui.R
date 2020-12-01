context("UI tests - Miscellaneous")
skip_if(identical(Sys.getenv("GAMS_SYS_DIR"), ""),
        "GAMS_SYS_DIR environment variable not set. Skipping tests.")

testDir <- file.path(getwd(), "..")

createTestDb()

Sys.setenv(MIRO_DB_PATH = testDir)

testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath,
                                       "transport.gms"))
Sys.setenv(MIRO_MODE = "base")

modelDataPath <- file.path(testModelPath, "data_transport")

file.copy2(file.path(testDir, "data", "transport.gdx"),
           file.path(modelDataPath, "default1.gdx"))
file.copy2(file.path(testDir, "data", "transport2.gdx"),
           file.path(modelDataPath, "default2.gdx"))
if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")){
  saveAdditionalGamsClArgs(testModelPath, "transport",
                           paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"'))
}
test_that("Loading scenario while input graph view is active works",
          expect_pass(testApp(file.path(testDir, ".."), "load_input_graph_test",
                              compareImages = FALSE)))
unlink(modelDataPath, recursive = TRUE, force = TRUE)

if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")){
  file.rename(file.path(testModelPath, "conf_transport", "transport_tmp.json"),
              file.path(testModelPath, "conf_transport", "transport.json"))
}

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
