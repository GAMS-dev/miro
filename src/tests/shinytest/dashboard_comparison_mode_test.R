app <- AppDriver$new("../../",
  name = "dashboard_comparison_mode_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

currentUser <- Sys.info()[["user"]]

# test dashboard compare mode
app$click(selector = "a[data-value='loadResults']")

Sys.sleep(0.5)
app$set_inputs(newLine_1 = "_sys_metadata_._sname")
Sys.sleep(0.5)
app$set_inputs(val_1_1 = "pickstock1")

app$set_inputs(btNewBlock = "click")
Sys.sleep(0.5)
app$set_inputs(newLine_2 = "_sys_metadata_._sname")
Sys.sleep(0.5)
app$set_inputs(val_2_1 = "pickstock2")
Sys.sleep(0.5)
app$set_inputs(btSendQuery = "click")
app$set_inputs(batchLoadAll = "click")

expect_error(app$get_js("$('#btBatchCompare+.dropdown-toggle').click()"), NA)
Sys.sleep(1L)
expect_true(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(2)').text()==='Dashboard comparison view'"))
expect_error(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(2)').click();"), NA)
Sys.sleep(6L)

expect_true(app$get_js("$('#cmpCustom___dashboard_1-dowVSindexChart').is(':visible')"))
expect_true(app$get_js("$('#cmpCustom___dashboard_1-stockWeightChart').is(':hidden');"))
app$click(selector = "div[id='cmpCustom___dashboard_1-error_train']")
Sys.sleep(1)
expect_true(app$get_js("$('#cmpCustom___dashboard_1-dowVSindexChart').is(':hidden')"))
expect_true(app$get_js("$('#cmpCustom___dashboard_1-stockWeightChart').is(':visible');"))

expect_true(app$get_js("$('#cmpCustom___dashboard_1-error_train .info-box-number').text()===''"))
selectSelectizeOption(app, "#cmpCustom___dashboard_1-scenarioSelect", "pickstock2")
expect_true(app$get_js("$('#cmpCustom___dashboard_1-error_train .info-box-number').text()==='618.84'"))
Sys.sleep(0.5)


app$stop()
