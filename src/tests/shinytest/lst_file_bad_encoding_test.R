app <- AppDriver$new("../../",
  name = "lst_file_bad_encoding_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(1)
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().includes('compilation error')", timeout = 150000), NA)
app$set_inputs(logFileTabsset = "listfile")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#listFileContainer').text().includes('�');", timeout = 5000L), NA)
app$stop()
