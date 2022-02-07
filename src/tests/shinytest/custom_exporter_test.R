app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("custom_exporter_test")

app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)

app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "My custom exporter")
app$setInputs(scenRemoteExportHandler = "click")
Sys.sleep(1)
expect_true(app$waitFor("$('.modal-body').is(':visible')&&$('.modal-body').text().trim()==='Bad, bad, bad...'", timeout = 50L))
app$waitFor('$(\'button[data-dismiss="modal"]:visible\').click();true;', timeout = 50)
Sys.sleep(1L)

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = "../data/transport_full.gdx")
Sys.sleep(0.5)
app$setInputs(btImportLocal = "click")
Sys.sleep(1L)
app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "My custom exporter")
app$setInputs(scenRemoteExportHandler = "click")
expect_false(app$waitFor("$('.modal-body').is(':visible')", timeout = 2000L))

app$stop()
