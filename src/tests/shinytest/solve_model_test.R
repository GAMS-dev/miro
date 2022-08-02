app <- AppDriver$new("../../", name = paste0("solve_model_test_", Sys.getenv("GMSMODELNAME")), variant = NULL, load_timeout = 20000)
if (Sys.getenv("GMSMODELNAME") %in% c("sudoku", "tsp", "kport")) {
  app$set_inputs(btImport = "click")
  Sys.sleep(1)
  app$set_inputs(btLoadScenConfirm = "click")
  if (Sys.getenv("GMSMODELNAME") %in% c("tsp")) {
    app$set_inputs(btOverwriteScen = "click")
  }
}
Sys.sleep(2)
if (Sys.getenv("GMSMODELNAME") %in% c("kport")) {
  Sys.sleep(2)
}
if (Sys.getenv("GMSMODELNAME") == "inscribedsquare") {
  app$set_inputs(inputTabset = "inputTabset_1")
  app$set_inputs(dropdown_3 = "CONOPT")
}
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#outputTableView').is(':visible')", timeout = 150000), NA)
if (Sys.getenv("GMSMODELNAME") %in% c("pickstock")) {
  # test download log files buttons
  app$click(selector = 'a[data-value="gamsinter"]')
  Sys.sleep(1)
  app$run_js("$('.dropdown-toggle:visible').click();", timeout = 50)
  Sys.sleep(1)
  expect_download(app, "btDownloadLogFilesMiroLog", "pickstock.mirolog")
  expect_download_size(app, "btDownloadLogFilesLog", "pickstock.log")
  expect_download_size(app, "btDownloadLogFilesLst", "pickstock.lst")
}
app$stop()
