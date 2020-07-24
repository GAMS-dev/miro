app <- ShinyDriver$new("../", loadTimeout = 20000)
app$snapshotInit(paste0("csv_upload_test_", Sys.getenv("GMSMODELNAME")))
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
app$uploadFile(localInput = paste0("data/", Sys.getenv("GMSMODELNAME"), ".zip"))
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
app$snapshot(items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))), 
             screenshot = TRUE)
if(identical(Sys.getenv("GMSMODELNAME"), "pickstock")){
  app$setInputs(inputTabset = "inputTabset_1")
  expect_identical(length(jsonlite::fromJSON(app$getAllValues()$output[["in_1"]])$x$data[[1]]), 
                   7560L)
  app$findElement("#btRemove1")$click()
  Sys.sleep(0.5)
  app$findElement(".modal-footer .bt-gms-confirm")$click()
  Sys.sleep(0.5)
  app$setInputs(btImport = "click")
  app$setInputs(tb_importData = "tb_importData_local")
  app$uploadFile(localInput = paste0("data/price.csv"))
  app$setInputs(btImportLocal = "click")
  app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
  app$snapshot(items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))), 
               screenshot = TRUE)
  app$setInputs(btImport = "click")
  app$setInputs(tb_importData = "tb_importData_local")
  app$uploadFile(localInput = paste0("data/_scalars.csv"))
  app$setInputs(btImportLocal = "click")
  app$setInputs(btOverwriteInput = "click")
  app$snapshot(items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))), 
               screenshot = TRUE)
}
app$stop()
