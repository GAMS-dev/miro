context("UI tests - views")

createTestDb()

testModelPath <- file.path(testDir, "model", "transport")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport.gms"
))
Sys.setenv(MIRO_MODE = "base")

test_that(
  "Views metadata menu works",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "views_metadata_test",
    compareImages = FALSE
  )))
)

modelDataPath <- file.path(testModelPath, "data_transport")

file.copy2(
  file.path(testDir, "data", "good-views2.json"),
  file.path(modelDataPath, "default_views.json")
)
file.copy2(
  file.path(testDir, "data", "transport.gdx"),
  file.path(modelDataPath, "default.gdx")
)
test_that(
  "Import views from data folder works",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "views_import_test",
    compareImages = FALSE
  )))
)
unlink(modelDataPath, recursive = TRUE, force = TRUE)

test_that(
  "Saving views to database works",
  try_again(3L, expect_pass(testApp(file.path(testDir, ".."), "views_save_as_test",
    compareImages = FALSE
  )))
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
