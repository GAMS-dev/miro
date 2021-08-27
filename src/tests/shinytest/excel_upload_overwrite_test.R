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
app$setInputs(btOverwriteInput = "click")
Sys.sleep(1)
app$snapshot(
  items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))),
  screenshot = TRUE
)
app$stop()
