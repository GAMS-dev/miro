context("UI tests - Import/Export miroscen files")

createTestDb()

testModelPath <- file.path(testDir, "model", "transport")
modelDataPath <- file.path(testModelPath, "data_transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport.gms"
))
Sys.setenv(GMSMODELNAME = "transport")
Sys.setenv(MIRO_MODE = "base")

file.copy2(
  file.path("..", "data", "bla.miroscen"),
  file.path(modelDataPath, "bla.miroscen")
)
test_that(
  "Import/Export MIROSCEN files works",
  {
    source(file.path(testDir, "shinytest", "miroscenio_test.R"), local = TRUE)
  }
)
unlink(file.path(modelDataPath, "bla.miroscen"), force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_MODE"))
