app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("gdx_upload_test_", Sys.getenv("GMSMODELNAME")))

widgetSheetId <- 1L
if(identical(Sys.getenv("GMSMODELNAME"), "pickstock")){
  widgetSheetId <- 2L
}else if(identical(Sys.getenv("GMSMODELNAME"), "transport")){
  widgetSheetId <- 7L
}
app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
if(!identical(Sys.getenv("GMSMODELNAME"), "pickstock")){
  Sys.sleep(1)
  app$snapshot(items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))),
               screenshot = TRUE)
}
app$setInputs(btImport = "click")
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".gdx"))
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = paste0("inputTabset_", widgetSheetId))
Sys.sleep(1)
app$snapshot(items = list(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L))),
             screenshot = TRUE)
app$stop()
