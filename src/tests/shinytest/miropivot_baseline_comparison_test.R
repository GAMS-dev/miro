app <- AppDriver$new("../../",
  name = "miropivot_baseline_comparison_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)

app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
app$upload_file(file_addViews = "../data/transport_views4.json")
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#tab_1_1-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$get_js("$('#tab_1_1-miroPivot-savedViewsDD li').eq(1).children('.dropdown-item').click();", timeout = 50), NA)
Sys.sleep(1)
app$wait_for_js("$('#tab_1_1-miroPivot-pivotTable .dt-title').is(':visible')", timeout = 2000L)
expect_identical(app$get_js("$('#tab_1_1-miroPivot-pivotTable .dt-title').text()"), "test123")

expect_identical(
  app$get_js("$('#tab_1_1-miroPivot-pivotTable td').map(function(index){return $(this).text()}).toArray()"),
  list("San-Diego", "", "275.00 (0%)", "275.00 (0%)", "Seattle", "300.00 (500.00%)", "50.00 (0%)", "", "San-Diego", "Seattle")
)

app$set_inputs(`tab_1_1-miroPivot-showSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
Sys.sleep(0.5)
app$click(selector = "#tab_1_1-miroPivot-settingsTabs a[data-value='Baseline Comparison']")
Sys.sleep(0.5)
expect_true(app$get_value(input = "tab_1_1-miroPivot-enableBaselineComparison"))
expect_identical(getSelectizeOptions(app, "#tab_1_1-miroPivot-baselineCompDomain"), list("i", "j", "Hdr"))
expect_identical(app$get_value(input = "tab_1_1-miroPivot-baselineCompDomain"), "j")
expect_identical(getSelectizeOptions(app, "#tab_1_1-miroPivot-baselineCompRecord"), list("New-york", "Chicago", "Topeka"))
expect_identical(app$get_value(input = "tab_1_1-miroPivot-baselineCompRecord"), "New-york")
expect_identical(getSelectizeOptions(app, "#tab_1_1-miroPivot-baselineCompMetrics"), list("percentage difference", "absolute difference", "normalization", "value"))
expect_identical(app$get_value(input = "tab_1_1-miroPivot-baselineCompMetrics"), c("value", "percentage difference"))

app$set_inputs(`tab_1_1-miroPivot-baselineCompDomain` = "i")
Sys.sleep(0.5)
expect_identical(getSelectizeOptions(app, "#tab_1_1-miroPivot-baselineCompRecord"), list("Seattle", "San-Diego"))
app$set_inputs(`tab_1_1-miroPivot-baselineCompRecord` = "Seattle")
app$set_inputs(`tab_1_1-miroPivot-baselineCompMetrics` = c("normalization"))
app$set_inputs(`tab_1_1-miroPivot-updateSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)
Sys.sleep(0.5)
expect_identical(
  app$get_js("$('#tab_1_1-miroPivot-pivotTable td').map(function(index){return $(this).text()}).toArray()"),
  list("San-Diego", "", "5.50", "", "Seattle", "1.00", "1.00", "", "San-Diego", "Seattle")
)

app$click(selector = "#tab_1_1-miroPivot-saveView")
Sys.sleep(1)
app$set_inputs("tab_1_1-miroPivot-newViewName" = "abc")
app$set_inputs("tab_1_1-miroPivot-saveViewConfirm" = "click")
Sys.sleep(1)

app$click(selector = "#tab_1_1-miroPivot-toggleViewButton")
Sys.sleep(0.5)
app$run_js("$('#tab_1_1-miroPivot-savedViewsDD .view-dropdown-item').filter(function(el){return $(this).text()==='abc'}).click()")
Sys.sleep(1)
app$click(selector = "#tab_1_1-miroPivot-toggleViewButton")
Sys.sleep(0.5)
app$run_js("$('#tab_1_1-miroPivot-savedViewsDD .view-dropdown-item').filter(function(el){return $(this).text()==='default'}).click()")
Sys.sleep(1)
expect_chartjs(
  app,
  "tab_1_1-miroPivot-pivotChart",
  list(c(NA, 300), c(275, 50), c(275, NA)),
  c("San-Diego", "Seattle")
)

app$stop()
