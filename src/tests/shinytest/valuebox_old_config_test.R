app <- AppDriver$new("../../", name = "valuebox_old_config_test", variant = NULL, load_timeout = 20000)
Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .small-box.bg-olive .fa-euro-sign').length===5"))

expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row')[0].childElementCount===2"))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row')[1].childElementCount===2"))
expect_true(app$get_js("$('div[data-value=\"outputTabset_1\"] .row')[2].childElementCount===1"))

app$expect_values(output = "outputDataTitle")
app$stop()
