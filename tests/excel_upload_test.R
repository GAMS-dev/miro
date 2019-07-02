app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit(paste0("excel_upload_test_", Sys.getenv("GMSMODELNAME")))

app$setInputs(btImport = "click")
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = paste0("data/", Sys.getenv("GMSMODELNAME"), ".xlsx"))
app$findElement(css = "#btCheckSnameLocal")$click()
app$snapshot(items = list(output = c("in_1")), screenshot = TRUE)
app$stop()
