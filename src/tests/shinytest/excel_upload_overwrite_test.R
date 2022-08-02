app <- AppDriver$new("../../", name = paste0("excel_upload_overwrite_test_", Sys.getenv("GMSMODELNAME")), variant = NULL, load_timeout = 20000)

widgetSheetId <- 1L
if (identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  widgetSheetId <- 2L
} else if (identical(Sys.getenv("GMSMODELNAME"), "transport")) {
  widgetSheetId <- 7L
}

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE)
app$upload_file(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
app$set_inputs(btImportLocal = "click")
Sys.sleep(2)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE)
app$upload_file(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
Sys.sleep(1)
app$expect_values(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L)))
if (identical(Sys.getenv("GMSMODELNAME"), "transport")) {
  app$set_inputs(btImport = "click")
  Sys.sleep(0.5)
  app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE)
  app$upload_file(localInput = paste0("../data/transport_merge.xlsx"))
  app$set_inputs(btImportLocal = "click")
  app$set_inputs(btMergeInputData = "click")
  Sys.sleep(1)
  expect_equal(getHotData(app, "in_1"),
    tibble(
      i = c("Seattle", "San-Diego", "Boston"),
      value = c(123L, 600L, 456L)
    ),
    check.attributes = FALSE
  )
  app$set_inputs(inputTabset = "inputTabset_4")
  Sys.sleep(1)
  expect_equal(getHotData(app, "in_4"),
    tibble(
      i = c("Seattle", "San-Diego", "Test123"),
      lat = c(0.1234, 32.715736, NA),
      lng = c(-122.335167, -117.161087, 2.345)
    ),
    check.attributes = FALSE
  )
  expect_identical(app$get_value(input = "slider_7"), 123L)
  app$set_inputs(btImport = "click")
  Sys.sleep(0.5)
  app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE, values_ = FALSE)
  app$upload_file(localInput = paste0("../data/transport_replace.xlsx"))
  app$set_inputs(btImportLocal = "click")
  app$set_inputs(btReplaceInputData = "click")
  Sys.sleep(1)
  app$set_inputs(inputTabset = "inputTabset_1")
  expect_equal(getHotData(app, "in_1"),
    tibble(
      i = c("Seattle", "Boston"),
      value = c(100L, 200L)
    ),
    check.attributes = FALSE
  )
  app$set_inputs(inputTabset = "inputTabset_4")
  Sys.sleep(1)
  expect_equal(getHotData(app, "in_4"),
    tibble(
      i = c("Seattle", "San-Diego", "Test123"),
      lat = c(0.1234, 32.715736, NA),
      lng = c(-122.335167, -117.161087, 2.345)
    ),
    check.attributes = FALSE
  )
  expect_identical(app$get_value(input = "slider_7"), 123L)
}
app$stop()
