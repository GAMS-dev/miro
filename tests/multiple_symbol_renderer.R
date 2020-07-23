app <- ShinyDriver$new("../", loadTimeout = 20000)
app$snapshotInit("multiple_symbol_renderer")

app$setInputs(sidebarMenuId = "outputData")
app$setInputs(outputTableView = "click")
app$snapshot(items = list(input = "tab_1-custom-trnsport_center"), screenshot = TRUE)
expect_error(app$findElement("a[data-value='outputTabset1_2']")$click())
app$stop()
