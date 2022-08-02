app <- AppDriver$new("../../", name = paste0("excel_upload_test_", Sys.getenv("GMSMODELNAME")), variant = NULL, load_timeout = 20000)

widgetSheetId <- 1L
if (identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  widgetSheetId <- 2L
} else if (identical(Sys.getenv("GMSMODELNAME"), "transport")) {
  widgetSheetId <- 7L
}
app$set_inputs(inputTabset = paste0("inputTabset_", widgetSheetId))
if (!identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  Sys.sleep(1)
  app$expect_values(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L)))
}
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
Sys.sleep(0.5)
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = paste0("inputTabset_", widgetSheetId))
Sys.sleep(1)
app$expect_values(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L)))
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$click(selector = ".modal-footer #dialogSaveInit .bt-gms-confirm")
Sys.sleep(2)
if (identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  app$click(selector = "#btRemove1")
  Sys.sleep(1)
  app$click(selector = ".modal-footer .bt-gms-confirm")
  Sys.sleep(1)
  app$set_inputs(btImport = "click")
  app$set_inputs(tb_importData = "tb_importData_local")
  app$upload_file(localInput = "../data/pickstock_index2.xlsx")
  Sys.sleep(1)
  expect_identical(app$get_value(input = "selExcelIndexSheet"), "_index")
  optionsTmp <- getSelectizeOptions(app, "#selExcelIndexSheet")
  expect_length(optionsTmp, 10L)
  expect_true(all(optionsTmp %in% c(
    "-", " Info", "_scalars_out (Output)", "stock_weight (Output)",
    "dowvsindex (Output)", "abserror (Output)", "pricemerge (Output)",
    "price (Input)", "_scalars (Input)", "_index"
  )))
  expect_true(grepl("idontexist", app$get_text(selector = "#localDataImportError"), fixed = TRUE))
  app$upload_file(localInput = "../data/pickstock_index.xlsx")
  Sys.sleep(2)
  optionsTmp <- getSelectizeOptions(app, "#selExcelIndexSheet")
  expect_length(optionsTmp, 10L)
  expect_true(all(optionsTmp %in% c(
    "-", " Info", "_scalars_out (Output)", "stock_weight (Output)",
    "dowvsindex (Output)", "abserror (Output)", "pricemerge (Output)",
    "price (Input)", "_scalars (Input)", "index"
  )))
  expect_true(app$get_js("$('#localDataImportError').is(':hidden');"))
  expect_identical(app$get_value(input = "selExcelIndexSheet"), "-")
  app$set_inputs(selExcelIndexSheet = "index")
  Sys.sleep(0.5)
  app$set_inputs(excelIndexSheetRng = "I10")
  Sys.sleep(0.5)
  app$set_inputs(btImportLocal = "click")
  Sys.sleep(4)
  expect_true(app$get_js("$('.modal-body').text().includes('asd');", timeout = 50L))
  expect_identical(app$get_values()$input[["slider_2"]], 28L)
  expect_identical(app$get_values()$input[["slider_3"]], 101L)
  Sys.sleep(1)
  app$set_inputs(inputTabset = paste0("inputTabset_", widgetSheetId - 1L))
  Sys.sleep(1)
  stockData <- getHotData(app, "in_1")
  expect_identical(nrow(stockData), 7056L)
  expect_false("DD" %in% stockData[[2]])
  expect_false("MCD" %in% stockData[[2]])
  expect_true("AAPL" %in% stockData[[2]])
}
app$stop()
