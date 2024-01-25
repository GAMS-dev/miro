app <- AppDriver$new("../../",
  name = "hcube_module_widget_groups_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

context("UI tests - Hypercube module - widget groups")

# load base scenario
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(1L)

expect_identical(app$get_js("$('.hc-widget-group:nth(0) .control-label:visible').text()"), "select the slider rangeselect checkbox")
expect_identical(app$get_js("$('.hc-widget-group:nth(0) .hc-widget-group-title').text()"), "")

expect_identical(app$get_js("$('.hc-widget-group:nth(1) .control-label:visible').text()"), "Solver to use")
expect_identical(app$get_js("$('.hc-widget-group:nth(1) .hc-widget-group-title').text()"), "test group 1")

expect_identical(app$get_js("$('.hc-widget-group:nth(2) .control-label:visible').text()"), "select the maximum number of stocksStep size")
expect_identical(app$get_js("$('.hc-widget-group:nth(2) .hc-widget-group-title').text()"), "2nd tests group")

app$stop()
