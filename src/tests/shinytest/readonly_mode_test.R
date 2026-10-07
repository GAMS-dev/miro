app <- AppDriver$new("../../",
  name = "readonly_mode_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

expect_true(app$get_js("$('a[data-value=\"gamsinter\"]').is(':visible')===false", timeout = 10000L))
expect_true(app$get_js("$('#btSolve').is(':visible')===false", timeout = 10000L))
expect_true(app$get_js("Shiny.setInputValue('btDelete',1)==null", timeout = 10000L))
Sys.sleep(1)
expect_true(app$get_js("$('.modal-body').is(':visible')===false", timeout = 10000L))

expect_true(app$get_js("Shiny.setInputValue('btSaveAs',1)==null", timeout = 10000L))
Sys.sleep(1)
expect_true(app$get_js("$('.modal-body').is(':visible')===false", timeout = 10000L))
app$stop()
