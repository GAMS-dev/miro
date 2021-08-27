app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("compare_scen_tab_test_", Sys.getenv("GMSMODELNAME")))

app$findElement("a[data-value='scenarios']")$click()
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='tab']")[[1]]$click()
Sys.sleep(0.5)
app$findElement("#cmpTabNoScenWrapper .action-button")$click()
Sys.sleep(1)
scenOptions <- setNames(
  getSelectizeAliases(app, "#selLoadScen"),
  getSelectizeOptions(app, "#selLoadScen")
)
scenToCompare <- paste0(c("1_", "2_"), Sys.info()[["user"]])
app$setInputs(selLoadScen = scenToCompare, wait_ = FALSE, values_ = FALSE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
expect_true(app$waitFor(paste0(
  "$('#cmpScenTitle_4').text()==='",
  strsplit(scenOptions[scenToCompare[1]], " (", fixed = TRUE)[[1]][1],
  "';"
), timeout = 50))
expect_true(app$waitFor(paste0(
  "$('#cmpScenTitle_5').text()==='",
  strsplit(scenOptions[scenToCompare[2]], " (", fixed = TRUE)[[1]][1],
  "';"
), timeout = 50))
app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
app$findElement("#btScenTableView5")$click()

app$stop()
