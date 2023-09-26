app <- AppDriver$new("../../",
  name = "multi_dd_widget_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2L)

app$set_inputs(inputTabset = "inputTabset_7")


expect_true(identical(app$get_value(input = "dropdown_7"), c("Seattle", "San-Diego")))

app$set_inputs(dropdown_7 = c())
app$set_inputs(btSolve = "click")
app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().startsWith('Run did not terminate successfully: There was a compilation error');",
  timeout = 10000L
)

app$set_inputs(miroSidebar = "inputData")
app$set_inputs(dropdown_7 = c("Seattle"))
app$set_inputs(btSolve = "click")
app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().startsWith('Run did not terminate successfully: There was an execution error');",
  timeout = 10000L
)

app$set_inputs(dropdown_7 = c("Seattle", "San-Diego"))
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#outputTableView').is(':visible')", timeout = 15000L), NA)

app$stop()
