app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("metadata_no_scalars_test")

app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$findElement("a[data-value='views']")$click()
Sys.sleep(0.1)

app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)

app$stop()
