app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("excel_upload_test_", Sys.getenv("GMSMODELNAME")))

widgetSheetId <- 1L
if (identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  widgetSheetId <- 2L
} else if (identical(Sys.getenv("GMSMODELNAME"), "transport")) {
  widgetSheetId <- 7L
}
app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
if (!identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  Sys.sleep(1)
  app$snapshot(
    items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))),
    screenshot = TRUE
  )
}
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
Sys.sleep(0.5)
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
Sys.sleep(1)
app$snapshot(
  items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))),
  screenshot = TRUE
)
app$setInputs(btSave = "click")
Sys.sleep(1)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(2)
if (identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  app$findElement("#btRemove1")$click()
  Sys.sleep(1)
  app$findElement(".modal-footer .bt-gms-confirm")$click()
  Sys.sleep(1)
  app$setInputs(btImport = "click")
  app$setInputs(tb_importData = "tb_importData_local")
  app$uploadFile(localInput = "../data/pickstock_index2.xlsx")
  Sys.sleep(1)
  expect_identical(app$getValue("selExcelIndexSheet"), "_index")
  optionsTmp <- getSelectizeOptions(app, "#selExcelIndexSheet")
  expect_length(optionsTmp, 10L)
  expect_true(all(optionsTmp %in% c(
    "-", " Info", "_scalars_out (Output)", "stock_weight (Output)",
    "dowvsindex (Output)", "abserror (Output)", "pricemerge (Output)",
    "price (Input)", "_scalars (Input)", "_index"
  )))
  expect_true(app$waitFor("$('#localDataImportError').text().includes('idontexist');", timeout = 50))
  app$uploadFile(localInput = "../data/pickstock_index.xlsx")
  Sys.sleep(2)
  optionsTmp <- getSelectizeOptions(app, "#selExcelIndexSheet")
  expect_length(optionsTmp, 10L)
  expect_true(all(optionsTmp %in% c(
    "-", " Info", "_scalars_out (Output)", "stock_weight (Output)",
    "dowvsindex (Output)", "abserror (Output)", "pricemerge (Output)",
    "price (Input)", "_scalars (Input)", "index"
  )))
  expect_true(app$waitFor("$('#localDataImportError').is(':hidden');", timeout = 50))
  expect_identical(app$getValue("selExcelIndexSheet"), "-")
  app$setInputs(selExcelIndexSheet = "index")
  Sys.sleep(0.5)
  app$setInputs(excelIndexSheetRng = "I10")
  Sys.sleep(0.5)
  app$setInputs(btImportLocal = "click")
  Sys.sleep(4)
  expect_true(app$waitFor("$('.modal-body').text().includes('asd');", timeout = 50L))
  expect_identical(app$getAllValues()$input[["slider_2"]], 28L)
  expect_identical(app$getAllValues()$input[["slider_3"]], 101L)
  Sys.sleep(1)
  app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId - 1L))
  Sys.sleep(1)
  stockData <- getHotData(app, "in_1")
  expect_identical(nrow(stockData), 7056L)
  expect_false("DD" %in% stockData[[2]])
  expect_false("MCD" %in% stockData[[2]])
  expect_true("AAPL" %in% stockData[[2]])
}
app$stop()
