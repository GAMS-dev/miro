app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("download_files_test")
Sys.sleep(2)

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$snapshot(items = list(output = "outputDataTitle"),screenshot = TRUE)

#export all file types
app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$snapshotDownload("scenExportHandler", "scenExport.gdx")

app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "xls")
expect_download_size(app, "scenExportHandler", "scenExport.xlsx")

app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "csv")
expect_download_size(app, "scenExportHandler", "scenExport.zip")

#export all file types for manually selected symbol
app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "gdx")
app$setInputs(cbSelectManuallyExp = TRUE)
app$setInputs(selDataToExport = "a")
app$snapshotDownload("scenExportHandler", "scenExport_manual.gdx")

app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "xls")
app$setInputs(cbSelectManuallyExp = TRUE)
app$setInputs(selDataToExport = "a")
expect_download_size(app, "scenExportHandler", "scenExport_manual.xlsx")

app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "csv")
app$setInputs(cbSelectManuallyExp = TRUE)
app$setInputs(selDataToExport = "a")
app$snapshotDownload("scenExportHandler", "scenExport_manual.csv")

app$stop()