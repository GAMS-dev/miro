context("UI tests - Loading and comparing scenarios")

for (modelToTest in c("pickstock", "transport")) {
  testModelPath <- file.path(testDir, "model", modelToTest)
  createTestDb()
  Sys.setenv(MIRO_MODEL_PATH = file.path(
    testModelPath,
    paste0(modelToTest, ".gms")
  ))
  Sys.setenv(GMSMODELNAME = modelToTest)
  Sys.setenv(MIRO_MODE = "base")

  for (testFile in c(
    "gdx_upload_test", "csv_upload_test",
    "excel_upload_test", "excel_upload_overwrite_test",
    "load_from_db_test", "compare_scen_split_test", "compare_scen_tab_test"
  )) {
    file.copy(file.path(testDir, "shinytest", paste0(testFile, ".R")),
      file.path(testDir, "shinytest", paste0(testFile, "_", modelToTest, ".R")),
      overwrite = TRUE
    )
  }
  if (dir.exists(file.path(testModelPath, paste0("data_", modelToTest))) &&
    unlink(file.path(testModelPath, paste0("data_", modelToTest)),
      recursive = TRUE, force = TRUE
    ) != 0L) {
    warning(sprintf("Couldn't remove data dir of model: '%s'", modelToTest))
  }
  test_that(
    paste0("Uploading CSV file works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("csv_upload_test_", modelToTest),
      compareImages = FALSE
    ))
  )
  test_that(
    paste0("Uploading GDX file works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("gdx_upload_test_", modelToTest),
      compareImages = FALSE
    ))
  )
  test_that(
    paste0("Uploading Excel file works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("excel_upload_test_", modelToTest),
      compareImages = FALSE
    ))
  )
  test_that(
    paste0("Uploading Excel file and overwriting data works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("excel_upload_overwrite_test_", modelToTest),
      compareImages = FALSE
    ))
  )
  test_that(
    paste0("Loading and saving data in db works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("load_from_db_test_", modelToTest),
      compareImages = FALSE
    ))
  )
  test_that(
    paste0("Comparing scenarios in split screen mode works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("compare_scen_split_test_", modelToTest),
      compareImages = FALSE
    ))
  )
  test_that(
    paste0("Comparing scenarios in tab view mode works for model: ", modelToTest),
    expect_pass(testApp(file.path(testDir, ".."), paste0("compare_scen_tab_test_", modelToTest),
      compareImages = FALSE
    ))
  )
}

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME", "MIRO_MODE"))
