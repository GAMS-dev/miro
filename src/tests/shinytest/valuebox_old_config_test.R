app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("valuebox_old_config_test")
Sys.sleep(2)
app$findElement("a[data-value='outputData']")$click()
Sys.sleep(1)
expect_true(app$waitFor("$('div[data-value=\"outputTabset_1\"] .small-box.bg-olive .fa-euro-sign').length===5"))

expect_true(app$waitFor("$('div[data-value=\"outputTabset_1\"] .row')[0].childElementCount===2"))
expect_true(app$waitFor("$('div[data-value=\"outputTabset_1\"] .row')[1].childElementCount===2"))
expect_true(app$waitFor("$('div[data-value=\"outputTabset_1\"] .row')[2].childElementCount===1"))

app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)
app$stop()
