app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("startup_performance_test")
app$snapshot(items = list(output = "inputDataTitle"),
             screenshot = TRUE)
app$stop()
