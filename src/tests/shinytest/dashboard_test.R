app <- AppDriver$new("../../",
  name = "dashboard_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
rendererName <- Sys.getenv("DASHBOARD_RENDERER_NAME", "dashboard")
getData <- function(id = paste0("tab_1_3-", rendererName, "-stockWeight2Table")) {
  return(jsonlite::fromJSON(app$get_values()$output[[id]])$x$data$datasets$data)
}

Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)

expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_test')[0].innerText"), timeout = 50), "ERROR TEST\n79.61"))
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_test .info-box-number').css('color')"), timeout = 50), "rgb(61, 153, 112)"))
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_train')[0].innerText"), timeout = 50), "ERROR TRAIN\n$951.17$"))
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_train .info-box-number').css('color')"), timeout = 50), "rgb(221, 75, 57)"))
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-testnegative')[0].innerText"), timeout = 50), "TESTNEGATIVE\n-1,001$"))
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-testnegative .info-box-number').css('color')"), timeout = 50), "rgb(51, 51, 51)"))
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-testpositive')[0].innerText"), timeout = 50), "TESTPOSITIVE\n+1,001$"))

# check whether grouping dimensions in a stacked bar chart works
configuration <- app$get_js(paste0("Chart.getChart('tab_1_3-", rendererName, "-dowVSindexStackChart').config._config"), timeout = 50)
chart_id <- paste0("tab_1_3-", rendererName, "-dowVSindexStackChart")
configuration <- app$get_js(paste0(
  "(function () {",
  "  const c = Chart.getChart('", chart_id, "');",
  "  if (!c) return null;",
  "  const cfg = c.config;",
  "  return {",
  "    type:    cfg.type,",
  "    data:    cfg.data,",
  "    options: cfg.options",
  "  };",
  "})()"
), timeout = 50)

expect_true(identical(configuration$type, "bar"))
expect_true(identical(configuration$data$datasets[[1]]$stack, "stack1"))
expect_true(identical(configuration$data$datasets[[2]]$stack, "stack2"))

# switch data view
expect_true(app$get_js(paste0("$('#tab_1_3-", rendererName, "-dowVSindexChart').is(':visible')")))
expect_true(app$get_js(paste0("$('#tab_1_3-", rendererName, "-abserrorTable').is(':visible')")))
expect_identical(unname(app$get_js(paste0("$('#tab_1_3-", rendererName, "-stockWeightChart')"))), list())
app$click(selector = paste0("div[id='tab_1_3-", rendererName, "-error_train'] .custom-info-box"))
Sys.sleep(1)
expect_true(app$get_js(paste0("$('#tab_1_3-", rendererName, "-dowVSindexChart').is(':hidden')")))
expect_true(app$get_js(paste0("$('#tab_1_3-", rendererName, "-stockWeightChart').is(':visible');")))
expect_true(app$get_js(paste0("$('#tab_1_3-", rendererName, "-dowVSindex0Chart').is(':visible');")))

# userFilter
expect_equal(getData(paste0("tab_1_3-", rendererName, "-stockWeightChart")), list(c(0.60, 0.51)))
expect_equal(getData(paste0("tab_1_3-", rendererName, "-stockWeight2Chart")), list(c(0.60, 0.51)))
do.call(app$set_inputs, setNames(
  list("DD"),
  paste0("tab_1_3-", rendererName, "-stockWeightuserFilter_symbol")
))

Sys.sleep(0.5)
expect_equal(getData(paste0("tab_1_3-", rendererName, "-stockWeightChart")), list(0.6))
expect_equal(getData(paste0("tab_1_3-", rendererName, "-stockWeight2Chart")), list(0.6))

app$click(selector = paste0("div[id='tab_1_3-", rendererName, "-error_ratio'] .custom-info-box"))
Sys.sleep(1)
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-pricemergeuserFilter_date')[0].multiple")), TRUE))
expect_true(identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-pricemergeuserFilter_uni-selectized')[0].multiple")), FALSE))
expect_identical(
  app$get_js(paste0("$('#tab_1_3-", rendererName, "-abserrorTable td').map(function(index){return $(this).text()}).toArray()")),
  list("reference", "0.55 (0%)", "", "2016-01-04", "2.18 (294.23%)", "", "2016-01-06", "0.01 (-98.40%)", "", "2016-01-08", "0.49 (-11.73%)", "")
)
app$stop()
