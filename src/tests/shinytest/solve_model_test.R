app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("solve_model_test_", Sys.getenv("GMSMODELNAME")))
if (Sys.getenv("GMSMODELNAME") %in% c("sudoku", "tsp", "kport")) {
  app$setInputs(btImport = "click")
  Sys.sleep(1)
  app$setInputs(btLoadScenConfirm = "click")
  if (Sys.getenv("GMSMODELNAME") %in% c("tsp")) {
    app$setInputs(btOverwriteScen = "click")
  }
}
Sys.sleep(2)
if (Sys.getenv("GMSMODELNAME") %in% c("kport")) {
  Sys.sleep(2)
}
app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)
if (Sys.getenv("GMSMODELNAME") == "inscribedsquare") {
  app$setInputs(inputTabset = "inputTabset_1")
  app$setInputs(dropdown_3 = "CONOPT")
}
app$setInputs(btSolve = "click")
Sys.sleep(10)
expect_error(app$findElement("#outputTableView")$click(), NA)
if (Sys.getenv("GMSMODELNAME") %in% c("pickstock")) {
  app$findElement('a[data-value="gamsinter"]')$click()
  Sys.sleep(1)
  app$waitFor("$('.dropdown-toggle:visible').click();", timeout = 50)
  Sys.sleep(1)
  expect_download(app, "btDownloadLogFilesMiroLog", "pickstock.mirolog")
  expect_download_size(app, "btDownloadLogFilesLog", "pickstock.log")
  expect_download_size(app, "btDownloadLogFilesLst", "pickstock.lst")
}
app$stop()
