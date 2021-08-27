app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("interrupt_model_test")

Sys.sleep(2)
app$setInputs(btSolve = "click")
Sys.sleep(2)
app$setInputs(btInterrupt = "click")
Sys.sleep(2)
app$snapshot(
  items = list(output = "outputDataTitle"),
  screenshot = TRUE
)
expect_error(app$findElement("#outputTableView")$click(), NA)
app$stop()
