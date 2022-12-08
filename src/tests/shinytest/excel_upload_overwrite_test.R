app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("excel_upload_overwrite_test_", Sys.getenv("GMSMODELNAME")))

widgetSheetId <- 1L
if (identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  widgetSheetId <- 2L
} else if (identical(Sys.getenv("GMSMODELNAME"), "transport")) {
  widgetSheetId <- 7L
}

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(tb_importData = "tb_importData_local", wait_ = FALSE, values_ = FALSE)
app$uploadFile(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
app$setInputs(btImportLocal = "click")
Sys.sleep(2)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(tb_importData = "tb_importData_local", wait_ = FALSE, values_ = FALSE)
app$uploadFile(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
app$setInputs(btImportLocal = "click")
app$setInputs(btReplaceInputData = "click")
Sys.sleep(1)
app$snapshot(
  items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))),
  screenshot = TRUE
)
if (identical(Sys.getenv("GMSMODELNAME"), "transport")) {
  app$setInputs(btImport = "click")
  Sys.sleep(0.5)
  app$setInputs(tb_importData = "tb_importData_local", wait_ = FALSE, values_ = FALSE)
  app$uploadFile(localInput = paste0("../data/transport_merge.xlsx"))
  app$setInputs(btImportLocal = "click")
  app$setInputs(btMergeInputData = "click")
  Sys.sleep(1)
  expect_equal(getHotData(app, "in_1"),
    tibble(
      i = c("Seattle", "San-Diego", "Boston"),
      value = c(123L, 600L, 456L)
    ),
    check.attributes = FALSE
  )
  app$setInputs(inputTabset = "inputTabset_4")
  Sys.sleep(1)
  expect_equal(getHotData(app, "in_4"),
    tibble(
      i = c("Seattle", "San-Diego", "Test123"),
      lat = c(0.1234, 32.715736, NA),
      lng = c(-122.335167, -117.161087, 2.345)
    ),
    check.attributes = FALSE
  )
  expect_identical(app$getValue("slider_7"), 123)
  app$setInputs(btImport = "click")
  Sys.sleep(0.5)
  app$setInputs(tb_importData = "tb_importData_local", wait_ = FALSE, values_ = FALSE)
  app$uploadFile(localInput = paste0("../data/transport_replace.xlsx"))
  app$setInputs(btImportLocal = "click")
  app$setInputs(btReplaceInputData = "click")
  Sys.sleep(1)
  app$setInputs(inputTabset = "inputTabset_1")
  expect_equal(getHotData(app, "in_1"),
    tibble(
      i = c("Seattle", "Boston"),
      value = c(100L, 200L)
    ),
    check.attributes = FALSE
  )
  app$setInputs(inputTabset = "inputTabset_4")
  Sys.sleep(1)
  expect_equal(getHotData(app, "in_4"),
    tibble(
      i = c("Seattle", "San-Diego", "Test123"),
      lat = c(0.1234, 32.715736, NA),
      lng = c(-122.335167, -117.161087, 2.345)
    ),
    check.attributes = FALSE
  )
  expect_identical(app$getValue("slider_7"), 123)
}
app$stop()
