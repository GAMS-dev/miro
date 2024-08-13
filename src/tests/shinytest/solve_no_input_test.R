app <- AppDriver$new("../../",
  name = "solve_no_input_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$run_js('$(\'button[data-dismiss="modal"]:visible\').click();')
Sys.sleep(1L)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js('$("#outputTableView").is(":visible")', timeout = 10000), NA)
app$set_inputs(btSave = "click")
expect_error(app$wait_for_js('$("#scenName").is(":visible")', timeout = 3000), NA)
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
expect_error(app$wait_for_js('$("#shiny-modal").is(":hidden")', timeout = 3000), NA)
Sys.sleep(2)
app$click(selector = 'a[data-value="scenarios"]')
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='pivot']")

app$click(selector = "#pivotCompBtWrapper button")
Sys.sleep(1)
savedScen <- getSelectizeOptions(app, "#selLoadScen")
app$set_inputs(selLoadScen = savedScen[1])
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$set_inputs("tab_0_1-miroPivot-pivotRenderer" = "stackedbar")
expect_chartjs(
  app,
  "tab_0_1-miroPivot-pivotChart",
  rep.int(c(1L, 10L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L), 2L),
  c(
    "New Scenario.i1",
    "New Scenario.i10",
    "New Scenario.i2",
    "New Scenario.i3",
    "New Scenario.i4",
    "New Scenario.i5",
    "New Scenario.i6",
    "New Scenario.i7",
    "New Scenario.i8",
    "New Scenario.i9",
    "New Scenario (Sandbox).i1",
    "New Scenario (Sandbox).i10",
    "New Scenario (Sandbox).i2",
    "New Scenario (Sandbox).i3",
    "New Scenario (Sandbox).i4",
    "New Scenario (Sandbox).i5",
    "New Scenario (Sandbox).i6",
    "New Scenario (Sandbox).i7",
    "New Scenario (Sandbox).i8",
    "New Scenario (Sandbox).i9"
  )
)
log <- app$get_logs()
log <- subset(log, location == "shiny")
expect_false(any(grepl("error", log$message, ignore.case = TRUE)))
app$stop()
