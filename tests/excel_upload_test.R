app <- ShinyDriver$new("../", loadTimeout = 20000)
app$snapshotInit(paste0("excel_upload_test_", Sys.getenv("GMSMODELNAME")))
widgetSheetId <- 1L
if(identical(Sys.getenv("GMSMODELNAME"), "pickstock")){
  widgetSheetId <- 2L
}else if(identical(Sys.getenv("GMSMODELNAME"), "transport")){
  widgetSheetId <- 7L
}
app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
Sys.sleep(1)
app$snapshot(items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))), 
             screenshot = TRUE)
app$setInputs(btImport = "click")
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = paste0("data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
Sys.sleep(1)
app$snapshot(items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))), 
             screenshot = TRUE)
app$setInputs(btSave = "click")
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(2)
app$stop()
