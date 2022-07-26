context("UI tests - Readonly Mode")

createTestDb()

testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport.gms"
))
Sys.setenv(MIRO_MODE = "readonly")

test_that(
  "Readonly mode works",
  expect_pass(testApp(file.path(testDir, ".."), "readonly_mode_test",
    compareImages = FALSE
  ))
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
