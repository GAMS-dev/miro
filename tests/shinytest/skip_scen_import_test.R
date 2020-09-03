app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("skip_scen_import_test")

app$setInputs(btImport = "click")
Sys.sleep(1)
app$snapshot(items = list(input = "tb_importData"),
             screenshot = TRUE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# remove scenario
app$setInputs(btDelete = "click")
Sys.sleep(0.5)
app$findElement("#btDeleteConfirm")$click()
app$stop()

# launch again. Import of data should be skipped now
app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$setInputs(btImport = "click")
Sys.sleep(1)
expect_identical(app$getValue("tb_importData"), "tb_importData_local")
app$stop()
