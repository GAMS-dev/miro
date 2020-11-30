context("UI tests - Import/Export miroscen files")

testDir <- file.path(getwd(), "..")

createTestDb()

Sys.setenv(MIRO_DB_PATH = testDir)
# END setup

testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath,
                                       "transport.gms"))
Sys.setenv(GMSMODELNAME = "transport")
Sys.setenv(MIRO_MODE="base")

test_that("Import/Export MIROSCEN files works",
          expect_pass(testApp(file.path(testDir, ".."), "miroscenio_test",
                              compareImages = FALSE)))

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_MODE"))
