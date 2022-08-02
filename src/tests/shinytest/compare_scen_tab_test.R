app <- AppDriver$new("../../", name = paste0("compare_scen_tab_test_", Sys.getenv("GMSMODELNAME")), variant = NULL, load_timeout = 20000)

app$click(selector = "a[data-value='scenarios']")
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='tab']")
Sys.sleep(0.5)
app$click(selector = "#cmpTabNoScenWrapper .action-button")
Sys.sleep(1)
scenOptions <- setNames(
  getSelectizeAliases(app, "#selLoadScen"),
  getSelectizeOptions(app, "#selLoadScen")
)
scenToCompare <- paste0(c("1_", "2_"), Sys.info()[["user"]])
app$set_inputs(selLoadScen = scenToCompare, wait_ = FALSE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)
expect_error(app$get_js(paste0(
  "$('#cmpScenTitle_4').text()==='",
  strsplit(scenOptions[scenToCompare[1]], " (", fixed = TRUE)[[1]][1],
  "'"
), timeout = 50), NA)
expect_error(app$wait_for_js(paste0(
  "$('#cmpScenTitle_5').text()==='",
  strsplit(scenOptions[scenToCompare[2]], " (", fixed = TRUE)[[1]][1],
  "'"
), timeout = 50), NA)
app$click(selector = "#btScenTableView5")

app$stop()
