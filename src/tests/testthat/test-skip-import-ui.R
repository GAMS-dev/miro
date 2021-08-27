context("UI tests - skip import of data files")

createTestDb()

testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport.gms"
))
Sys.setenv(MIRO_MODE = "base")

modelDataPath <- file.path(testModelPath, "data_transport")
file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default.gdx")
)
test_that(
  "Skipping import of data files if unchanged works",
  expect_pass(testApp(file.path(testDir, ".."), "skip_scen_import_test",
    compareImages = FALSE
  ))
)
file.copy2(
  file.path(testDir, "data", "transport2.gdx"),
  file.path(modelDataPath, "default2.gdx")
)
test_that(
  "Import new scenario works",
  expect_pass(testApp(file.path(testDir, ".."), "skip_scen_import_test",
    compareImages = FALSE
  ))
)
file.copy2(
  file.path(testDir, "data", "transport2.gdx"),
  file.path(modelDataPath, "default.gdx")
)
test_that(
  "Skipping import of data files if changed works",
  expect_pass(testApp(file.path(testDir, ".."), "skip_scen_import_test",
    compareImages = FALSE
  ))
)
Sys.setenv(MIRO_FORCE_SCEN_IMPORT = "true")
test_that(
  "MIRO_FORCE_SCEN_IMPORT option works",
  expect_pass(testApp(file.path(testDir, ".."), "skip_scen_import_test",
    compareImages = FALSE
  ))
)
unlink(modelDataPath, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_FORCE_SCEN_IMPORT"))
