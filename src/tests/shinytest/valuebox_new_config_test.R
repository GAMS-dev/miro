app <- AppDriver$new("../../",
  name = "valuebox_new_config_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)
app$expect_values(output = "outputDataTitle")
expect_length(getSelectizeOptions(app, "#tab_1_5-data_filter"), 31L)
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row')[1].childElementCount===2", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row')[2].childElementCount===1", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row')[3].childElementCount===1", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row')[4].childElementCount===1", timeout = 50))

expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-green h3')[0].innerText==='79.6136'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-green p')[0].innerText==='Absolute error in entire training phase'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-yellow h3')[0].innerText==='951.1662'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-yellow p')[0].innerText==='asd'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-red h3')[0].innerText==='11.9'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-red p')[0].innerText===''", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-aqua h3')[0].innerText==='2016-01-04'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-aqua p')[0].innerText==='first date of training period'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-aqua h3')[1].innerText==='2016-05-24'", timeout = 50))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-aqua p')[1].innerText==='last date of training period'", timeout = 50))

expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row .bg-aqua .fa-circle-play').length==1", timeout = 50))

app$stop()
