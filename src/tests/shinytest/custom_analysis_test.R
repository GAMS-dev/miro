app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("custom_analysis_test")

app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)

app$stop()
