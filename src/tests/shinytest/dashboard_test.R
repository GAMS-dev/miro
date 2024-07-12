app <- AppDriver$new("../../",
  name = "dashboard_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
getData <- function(id = "tab_1_2-dashboard-stockWeight2Table") {
  return(jsonlite::fromJSON(app$get_values()$output[[id]])$x$data$datasets$data)
}

Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)

# switch data view
expect_true(app$get_js("$('#tab_1_2-dashboard-dowVSindexChart').is(':visible')"))
expect_true(app$get_js("$('#tab_1_2-dashboard-stockWeightChart').is(':hidden');"))
app$click(selector = "div[id='tab_1_2-dashboard-error_train']")
Sys.sleep(1)
expect_true(app$get_js("$('#tab_1_2-dashboard-dowVSindexChart').is(':hidden')"))
expect_true(app$get_js("$('#tab_1_2-dashboard-stockWeightChart').is(':visible');"))

# userFilter
expect_equal(getData("tab_1_2-dashboard-stockWeightChart"), list(c(0.60, 0.51)))
expect_equal(getData("tab_1_2-dashboard-stockWeight2Chart"), list(c(0.60, 0.51)))
app$set_inputs(`tab_1_2-dashboard-stockWeightuserFilter_symbol` = "DD")
Sys.sleep(0.5)
expect_equal(getData("tab_1_2-dashboard-stockWeightChart"), list(0.6))
expect_equal(getData("tab_1_2-dashboard-stockWeight2Chart"), list(0.6))
app$stop()
