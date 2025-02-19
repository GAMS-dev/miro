app <- AppDriver$new("../../",
  name = "extra_cl_args_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(1)
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().includes('Process terminated with status: 123')", timeout = 10000), NA)
app$stop()
