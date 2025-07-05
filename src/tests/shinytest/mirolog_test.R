app <- AppDriver$new("../../",
  name = "mirolog_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
logContainerId <- Sys.getenv("MIRO_LOG_CONTAINER_ID", "miroLogContainer")
app$set_inputs(btImport = "click")
Sys.sleep(1)
app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE)
app$upload_file(localInput = "../data/pickstock_negative_price.gdx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
Sys.sleep(1)
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#valErr_price').is(':visible');", timeout = 10000L), NA)
expect_error(app$wait_for_js("$('#valErr_price').text().includes('negative');", timeout = 2000L), NA)
app$run_js("$('#valErr_price li').click();")
expect_true(app$get_js("$('#modelStatus').is(':visible')===false;"))
app$run_js("$('#valErr_price li').dblclick();")
expect_error(app$wait_for_js(paste0(
  "$('#modelStatus').is(':visible')&&$('#",
  logContainerId, "').is(':visible');"
), timeout = 5000L), NA)
logContent <- app$get_js(paste0("$('#", logContainerId, "').text()"))
expect_match(logContent, "Symbol IBM  has negative price at the date: 2016-01-04", fixed = TRUE)
expect_match(logContent, "Symbol GS   has negative price at the date: 2016-01-12", fixed = TRUE)
app$click(selector = 'a[data-value="inputData"]')
Sys.sleep(0.5)
app$click(selector = "#btRemove1")
Sys.sleep(1)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(1L)
expect_error(app$wait_for_js("$('#valErr_price').is(':visible')===false;", timeout = 5000L), NA)
app$click(selector = 'a[data-value="gamsinter"]')
Sys.sleep(0.5)
expect_error(app$wait_for_js(paste0(
  "$('#modelStatus').text()===''&&$('#", logContainerId,
  "').text()==='';"
), timeout = 5000L), NA)
if (identical(logContainerId, "miroLogContainer")) {
  app$set_inputs(logFileTabsset = "log")
  Sys.sleep(0.5)
  expect_error(app$wait_for_js("$('#logStatusContainer').text()==='';", timeout = 5000L), NA)
}
app$set_inputs(logFileTabsset = "listfile")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#listFileContainer').text()==='';", timeout = 5000L), NA)
app$stop()
