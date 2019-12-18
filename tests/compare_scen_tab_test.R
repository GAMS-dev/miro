app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit(paste0("compare_scen_tab_test_", Sys.getenv("GMSMODELNAME")))

app$findElement("a[data-value='scenarios']")$click()
app$setInputs(btSplitView = "click")
app$findElement("#no-scen .action-button")$click()
Sys.sleep(1)
app$setInputs(selLoadScen = paste0(c("1_", "2_"), Sys.info()[["user"]]), wait_ = FALSE, values_ = FALSE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$snapshot(items = list(output = c("title_4", "title_5")), screenshot = TRUE)
app$setInputs(table_5 = "click")

app$stop()