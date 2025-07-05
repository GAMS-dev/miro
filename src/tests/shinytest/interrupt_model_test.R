app <- AppDriver$new("../../",
  name = "interrupt_model_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

Sys.sleep(2)
app$set_inputs(btSolve = "click")
Sys.sleep(0.5)
app$wait_for_js("$('#logStatusContainer').is(':visible') && $('#logStatusContainer').text().includes('starting')", timeout = 5000L)
Sys.sleep(0.5)
app$set_inputs(btInterrupt = "click")
Sys.sleep(1L)
app$wait_for_js("$('#logStatusContainer').is(':hidden')", timeout = 10000L)
Sys.sleep(0.5)
expect_error(app$click(selector = "#outputTableView"), NA)
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
Sys.sleep(0.5)
app$wait_for_js("$('#logStatusContainer').text().includes('interrupt received')",
  timeout = 5000L
)
expect_true(app$get_js("$('#btInterrupt .fa-skull').is(':visible')"))
# when solving again, hardkill button should be reset
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
Sys.sleep(0.5)
app$set_inputs(btSolve = "click")
Sys.sleep(0.5)
app$wait_for_js("$('#logStatusContainer').is(':visible') && $('#logStatusContainer').text().includes('starting')", timeout = 5000L)
expect_true(app$get_js("$('#btInterrupt .fa-skull').is(':visible')===false"))
app$set_inputs(btInterrupt = "click")
Sys.sleep(0.5)
expect_true(app$get_js("$('#btInterrupt .fa-skull').is(':visible')"))
app$wait_for_js("$('#btInterrupt').is(':enabled')", timeout = 2000L)
app$set_inputs(btInterrupt = "click")
app$wait_for_js("$('#modelStatus').text().includes('interrupted')", timeout = 5000L)
app$stop()
