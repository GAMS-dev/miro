app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("layout_expanded_tabs_test")

app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)

expect_true(app$waitFor("$('#inputTabset > li').length===3", timeout = 50L))
expect_true(app$waitFor("$('#outputTabset > li').length===4", timeout = 50L))

# tab view
app$findElement('a[data-value="scenarios"]')$click()
app$waitFor("$('#btLoadScen').click();true", timeout = 50L)
Sys.sleep(1L)
app$setInputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2L)
expect_true(app$waitFor("$('#outputTabset > li').length===4", timeout = 50L))
expect_true(app$waitFor("$('#contentScen_4 > li').length===7", timeout = 50L))

# pivot view
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='pivot']")[[1]]$click()
expect_true(app$waitFor("$('.box-title:visible button').eq(0).click();true;", timeout = 50))
Sys.sleep(1)
app$setInputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
Sys.sleep(0.2)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
expect_true(app$waitFor("$('#contentScen_0 > li').length===5", timeout = 50L))

# split view
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='split']")[[1]]$click()
app$waitFor("$('.scenSplit-button-load:nth(1)').click();true", timeout = 50L)
Sys.sleep(1)
expect_true(app$waitFor("$('#contentScen_2 > li').length===2", timeout = 50L))

app$stop()
