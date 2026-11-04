app <- AppDriver$new("../../",
  name = "custom_comparison_sandbox_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

rowCount <- function(tableHtml) {
  stringi::stri_count_fixed(tableHtml, "<tr>")
}
app$set_inputs(btSave = "click")
Sys.sleep(0.5)
app$set_inputs(btSaveOutput = "click")
Sys.sleep(2L)

openBatchLoadDialog <- function() {
  app$wait_for_js("$(\"a[data-value='loadResults']\").length > 0", timeout = 10000L)
  app$click(selector = "a[data-value='loadResults']")
  Sys.sleep(0.5)
  app$set_inputs(btSendQuery = "click")
  app$set_inputs(batchLoadAll = "click")
  Sys.sleep(1L)
}
selectCompareMode <- function(label) {
  app$run_js("$('#btBatchCompare+.dropdown-toggle').click()")
  Sys.sleep(0.5)
  app$run_js(paste0("$('#btBatchCompare, #btBatchCompare~.dropdown-menu a').filter(':contains(\"", label, "\")').click();"))
  Sys.sleep(0.5)
}

openBatchLoadDialog()
selectCompareMode("Compare (Split)")
expect_true(app$get_js("$('#cbBatchCompareSandbox').is(':visible')"))
expect_false(app$get_js("$('#batchCompareBaselineOuter').is(':visible')"))
app$run_js("Miro.batchCompareBack()")
expect_true(app$get_js("$('#btBatchCompare').is(':visible')"))
expect_false(app$get_js("$('#btBatchCompareConfirm').is(':visible')"))

selectCompareMode("Sandbox test")
expect_true(app$get_js("$('#btBatchCompareConfirm').is(':visible')"))
expect_false(app$get_js("$('#cbBatchCompareSandbox').is(':checked')"))
expect_false(app$get_js("$('#batchCompareBaselineWrapper').is(':visible')"))
app$click(selector = "#btBatchCompareConfirm")
Sys.sleep(2L)
noRowsWithoutSandbox <- rowCount(app$get_values()$output[["cmpCustom_sbtest-tdVsErrorRatioTable"]])

app$run_js("$(\"button[onclick*='btCloseScenCmp']:visible\").click();")
Sys.sleep(0.5)
openBatchLoadDialog()
selectCompareMode("Sandbox test")
app$set_inputs(cbBatchCompareSandbox = TRUE)
expect_error(app$wait_for_js("$('#batchCompareBaselineWrapper').is(':visible')", timeout = 5000L), NA)
expect_identical(app$get_js("$('#selBatchCompareBaseline')[0].selectize.items.length"), 1L)
expect_identical(
  app$get_js("Object.keys($('#selBatchCompareBaseline')[0].selectize.options).length"),
  2L
)
app$set_inputs(selBatchCompareBaseline = "sb")
app$click(selector = "#btBatchCompareConfirm")
Sys.sleep(2L)
noRowsWithSandbox <- rowCount(app$get_values()$output[["cmpCustom_sbtest-tdVsErrorRatioTable"]])

expect_identical(noRowsWithSandbox, noRowsWithoutSandbox + 1L)

app$run_js("$(\"button[onclick*='btCloseScenCmp']:visible\").click();")
Sys.sleep(0.5)
openBatchLoadDialog()
# without baseline selection, the sandbox scenario is always included
selectCompareMode("No baseline test")
expect_false(app$get_js("$('#btBatchCompareConfirm').is(':visible')"))
Sys.sleep(2L)
expect_identical(
  rowCount(app$get_values()$output[["cmpCustom_nobaseline-tdVsErrorRatioTable"]]),
  noRowsWithSandbox
)

app$stop()
