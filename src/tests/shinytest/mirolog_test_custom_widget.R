app <- AppDriver$new("../../",
  name = "mirolog_test_custom_widget", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(1)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE)
Sys.sleep(0.5)
app$upload_file(localInput = "../data/sudoku_mirolog_fail.gdx")
app$set_inputs(btImportLocal = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#valErr_initial_state').is(':visible');", timeout = 10000L), NA)
expect_error(app$wait_for_js("$('#valErr_initial_state').text()===' test123 must be positive!';", timeout = 5000L), NA)
app$stop()
