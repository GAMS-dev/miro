app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("compare_scen_split_test_", Sys.getenv("GMSMODELNAME")))

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
app$findElement("a[data-value='scenarios']")$click()
app$setInputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
scenOptions <- strsplit(getSelectizeAliases(app, "#selLoadScen"), " (", fixed = TRUE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$setInputs(btScenSplit2_open = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)

expect_true(app$waitFor(paste0(
  "$('#cmpScenTitle_2').text()==='",
  scenOptions[[2]][1], "';"
), timeout = 50))
expect_true(app$waitFor(paste0(
  "$('#cmpScenTitle_3').text()==='",
  scenOptions[[1]][1], "';"
), timeout = 50))
app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
app$stop()
