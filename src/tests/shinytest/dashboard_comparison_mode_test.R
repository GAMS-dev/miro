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

app$set_inputs(btNewBlock = "click")
Sys.sleep(0.5)
app$set_inputs(newLine_3 = "_sys_metadata_._sname")
Sys.sleep(0.5)
app$set_inputs(val_3_1 = "default")
Sys.sleep(0.5)
app$set_inputs(btSendQuery = "click")
app$set_inputs(batchLoadAll = "click")

expect_error(app$get_js("$('#btBatchCompare+.dropdown-toggle').click()"), NA)
Sys.sleep(1L)
expect_true(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(1)').text()==='Dashboard comparison view'"))
expect_error(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(1)').click();"), NA)
Sys.sleep(6L)

expect_true(app$get_js("$('#cmpCustom___dashboard_1-dowVSindexChart').is(':visible')"))
expect_true(app$get_js("$('#cmpCustom___dashboard_1-stockWeightChart').is(':hidden');"))

expect_identical(
  app$get_js("$('#cmpCustom___dashboard_1-abserrorTable tr').filter(function(el){return ['2016-06-02','2016-06-07'].includes($('td:first-child',this).text())}).map(function(el){return $('td',this).map(function(el2){return $(this).text()}).toArray()}).toArray()"),
  list("2016-06-02", "1.31 (0%)", "1.31 (0%)", "1.87 (42.39%)", "2016-06-07", "1.08 (0%)", "1.08 (0%)", "0.95 (-11.89%)")
)

app$click(selector = "div[id='cmpCustom___dashboard_1-error_train'] .custom-info-box")
Sys.sleep(1)
expect_true(app$get_js("$('#cmpCustom___dashboard_1-dowVSindexChart').is(':hidden')"))
expect_true(app$get_js("$('#cmpCustom___dashboard_1-stockWeightChart').is(':visible');"))

expect_true(app$get_js("$('#cmpCustom___dashboard_1-error_train .info-box-number').text()===''"))
selectSelectizeOption(app, "#cmpCustom___dashboard_1-scenarioSelect", "pickstock2")
Sys.sleep(1)

expect_true(app$get_js("$('#cmpCustom___dashboard_1-error_train .info-box-number').text()==='$618.84$'"))
expect_true(app$get_js("$('#cmpCustom___dashboard_1-error_test .info-box-number').text()==='0'"))
Sys.sleep(0.5)

selectSelectizeOption(app, "#cmpCustom___dashboard_1-scenarioSelect", "default")
Sys.sleep(1)

expect_true(identical(app$get_js(paste0("$('#cmpCustom___dashboard_1-error_test')[0].innerText"), timeout = 50), "ERROR TEST\n79.61"))
expect_true(identical(app$get_js(paste0("$('#cmpCustom___dashboard_1-error_test .info-box-number').css('color')"), timeout = 50), "rgb(61, 153, 112)"))
expect_true(identical(app$get_js(paste0("$('#cmpCustom___dashboard_1-error_train')[0].innerText"), timeout = 50), "ERROR TRAIN\n$951.17$"))
expect_true(identical(app$get_js(paste0("$('#cmpCustom___dashboard_1-error_train .info-box-number').css('color')"), timeout = 50), "rgb(221, 75, 57)"))
expect_true(identical(app$get_js(paste0("$('#cmpCustom___dashboard_1-testnegative')[0].innerText"), timeout = 50), "TESTNEGATIVE\n-1,001$"))
expect_true(identical(app$get_js(paste0("$('#cmpCustom___dashboard_1-testnegative .info-box-number').css('color')"), timeout = 50), "rgb(51, 51, 51)"))
expect_true(identical(app$get_js(paste0("$('#cmpCustom___dashboard_1-testpositive')[0].innerText"), timeout = 50), "TESTPOSITIVE\n+1,001$"))


app$stop()
