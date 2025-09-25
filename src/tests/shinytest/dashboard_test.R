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

expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_test')[0].innerText"), timeout = 2000), "ERROR TEST\n79.61357")
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_test .info-box-number').css('color')"), timeout = 2000), "rgb(61, 153, 112)")
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_train')[0].innerText"), timeout = 2000), "ERROR TRAIN\n$951.1662$")
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_train .info-box-number').css('color')"), timeout = 2000), "rgb(221, 75, 57)")
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-error_ratio')[0].innerText"), timeout = 2000), "ERROR RATIO\n+11.95$")
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-testnegative')[0].innerText"), timeout = 2000), "TESTNEGATIVE\n-1,001$")
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-testpositive')[0].innerText"), timeout = 2000), "TESTPOSITIVE\n+1,001$")

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

expect_identical(configuration$type, "bar")
expect_identical(configuration$data$datasets[[1]]$stack, "stack1")
expect_identical(configuration$data$datasets[[2]]$stack, "stack2")

# switch data view
expect_true(app$get_js(paste0("$('#tab_1_3-", rendererName, "-dowVSindexChart').is(':visible')")))
expect_true(app$get_js(paste0("$('#tab_1_3-", rendererName, "-abserrorTable').is(':visible')")))
expect_identical(unname(app$get_js(paste0("$('#tab_1_3-", rendererName, "-stockWeightChart')"))), list())
app$click(selector = paste0("div[id='tab_1_3-", rendererName, "-error_train'] .custom-info-box"))
Sys.sleep(2)
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

# userFilter with alias dimensions in cols
expect_equal(getData(paste0("tab_1_3-", rendererName, "-testaliasfilterChart")), list(3, 2, 1, 4))
do.call(app$set_inputs, setNames(
  list("DD"),
  paste0("tab_1_3-", rendererName, "-testaliasfilteruserFilter_s")
))
Sys.sleep(0.5)
expect_equal(getData(paste0("tab_1_3-", rendererName, "-testaliasfilterChart")), list(2))
do.call(app$set_inputs, setNames(
  list(character(0)),
  paste0("tab_1_3-", rendererName, "-testaliasfilteruserFilter_s")
))
do.call(app$set_inputs, setNames(
  list("DD"),
  paste0("tab_1_3-", rendererName, "-testaliasfilteruserFilter_symbol")
))
Sys.sleep(0.5)
expect_equal(getData(paste0("tab_1_3-", rendererName, "-testaliasfilterChart")), list(1))

app$click(selector = paste0("div[id='tab_1_3-", rendererName, "-error_ratio'] .custom-info-box"))
Sys.sleep(1)
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-pricemergeuserFilter_date')[0].multiple")), TRUE)
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-pricemergeuserFilter_uni-selectized')[0].multiple")), FALSE)
expect_identical(
  app$get_js(paste0("$('#tab_1_3-", rendererName, "-abserrorTable td').map(function(index){return $(this).text()}).toArray()")),
  list("reference", "0.552 (0%)", "", "2016-01-04", "2.175 (294.231%)", "", "2016-01-06", "0.009 (-98.405%)", "", "2016-01-08", "0.487 (-11.733%)", "")
)
app$click(selector = paste0("div[id='tab_1_3-", rendererName, "-error_test'] .custom-info-box"))
do.call(app$set_inputs, setNames(
  list(c("reference", "2016-01-04"), c("training error")),
  paste0("tab_1_3-", rendererName, "-abserroruserFilter_", c("date", "Hdr"))
))
app$wait_for_idle()
expect_identical(
  app$get_js(paste0("$('#tab_1_3-", rendererName, "-abserrorTable td').map(function(index){return $(this).text()}).toArray()")),
  list("reference", "0.552 (0%)", "2016-01-04", "2.175 (294.231%)")
)
# new userFilter format
app$click(selector = paste0("div[id='tab_1_3-", rendererName, "-error_ratio'] .custom-info-box"))
Sys.sleep(1)
idUni <- sprintf("#tab_1_3-%s-pricemerge2userFilter_uni", rendererName)
idDate <- sprintf("#tab_1_3-%s-pricemerge2userFilter_date", rendererName)
idDatePlaceholder <- sprintf("#tab_1_3-%s-pricemerge3userFilter_date-selectized", rendererName)

expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-pricemerge2userFilter_date')[0].multiple")), TRUE)
expect_identical(app$get_js(paste0("$('#tab_1_3-", rendererName, "-pricemerge2userFilter_uni-selectized')[0].multiple")), FALSE)
expect_equal(app$get_js(sprintf("$('%s-label').text().trim()", idUni)), "testlabel")
expect_equal(app$get_js(sprintf("$('%s-label').text().trim()", idDate)), "testlabel2")
expect_equal(app$get_js(sprintf("$('%s').attr('placeholder')", idDatePlaceholder)), "All items")
expect_equal(app$get_js(sprintf("(function(){var el=$('%s')[0]; return el.selectize ? el.selectize.getValue() : $('%s').val();})()", idUni, idUni)), "DD")
expect_identical(
  getDisplayedDtData(app, paste0("tab_1_3-", rendererName, "-pricemerge2Table")),
  tibble(
    value = c(
      "2016-01-04", "63.07", "2016-01-05", "64.279999", "2016-01-06", "63.380001", "2016-12-30", "73.400002"
    )
  )
)
# overwriteHeaderAliases used in table
hdr2 <- getVisibleDtHeader(app, sprintf("tab_1_3-%s-pricemerge2Table", rendererName))
hdr3 <- getVisibleDtHeader(app, sprintf("tab_1_3-%s-pricemerge3Table", rendererName))
expect_equal(hdr2, c("test Header", "DD"))
expect_equal(hdr3, c("test Header", "12345", "value"))
app$stop()
