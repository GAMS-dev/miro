app <- AppDriver$new("../../",
  name = paste0("compare_scen_split_test_", Sys.getenv("GMSMODELNAME")), variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
app$click(selector = "a[data-value='scenarios']")
app$set_inputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
scenOptions <- strsplit(getSelectizeAliases(app, "#selLoadScen"), " (", fixed = TRUE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$set_inputs(btScenSplit2_open = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)

expect_error(app$wait_for_js(paste0(
  "$('#cmpScenTitle_2').text()==='",
  scenOptions[[1]][1], "'"
), timeout = 50), NA)
expect_error(app$wait_for_js(paste0(
  "$('#cmpScenTitle_3').text()==='",
  scenOptions[[2]][1], "'"
), timeout = 50), NA)
app$stop()
