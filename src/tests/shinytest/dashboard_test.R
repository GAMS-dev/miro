app <- AppDriver$new("../../",
  name = "dashboard_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
rendererName <- Sys.getenv("DASHBOARD_RENDERER_NAME", "dashboard")
getData <- function(id = paste0("tab_1_2-", rendererName, "-stockWeight2Table")) {
  return(jsonlite::fromJSON(app$get_values()$output[[id]])$x$data$datasets$data)
}

Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)

# switch data view
expect_true(app$get_js(paste0("$('#tab_1_2-", rendererName, "-dowVSindexChart').is(':visible')")))
expect_true(app$get_js(paste0("$('#tab_1_2-", rendererName, "-stockWeightChart').is(':hidden');")))
app$click(selector = paste0("div[id='tab_1_2-", rendererName, "-error_train']"))
Sys.sleep(1)
expect_true(app$get_js(paste0("$('#tab_1_2-", rendererName, "-dowVSindexChart').is(':hidden')")))
expect_true(app$get_js(paste0("$('#tab_1_2-", rendererName, "-stockWeightChart').is(':visible');")))

# userFilter
expect_equal(getData(paste0("tab_1_2-", rendererName, "-stockWeightChart")), list(c(0.60, 0.51)))
expect_equal(getData(paste0("tab_1_2-", rendererName, "-stockWeight2Chart")), list(c(0.60, 0.51)))
do.call(app$set_inputs, setNames(
  list("DD"),
  paste0("tab_1_2-", rendererName, "-stockWeightuserFilter_symbol")
))

Sys.sleep(0.5)
expect_equal(getData(paste0("tab_1_2-", rendererName, "-stockWeightChart")), list(0.6))
expect_equal(getData(paste0("tab_1_2-", rendererName, "-stockWeight2Chart")), list(0.6))
app$stop()
