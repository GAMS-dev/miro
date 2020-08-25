app <- ShinyDriver$new("../../", loadTimeout = 20000)

app$snapshotInit("output_attach_test_2")
app$snapshot(items = list(output = "outputDataTitle"),
             screenshot = TRUE)
app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$snapshot(items = list(output = "outputDataTitle"),
             screenshot = TRUE)
expect_identical(startsWith(app$getValue("selLoadScen"), "1_"), TRUE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$setInputs(btSolve = "click")
Sys.sleep(4)
app$setInputs(btEditMeta = "click")
Sys.sleep(2)
app$findElement("[data-value='Attachments']")$click()
app$snapshot(items = list(output = "modelStatus"),
             screenshot = TRUE)
expect_error(app$findElement("#outputTableView")$click())
app$stop()

