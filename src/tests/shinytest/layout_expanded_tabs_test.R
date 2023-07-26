app <- AppDriver$new("../../",
  name = "layout_expanded_tabs_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
expect_true(app$get_js("$('#inputTabset > li').length===3", timeout = 50L))
expect_true(app$get_js("$('#outputTabset > li').length===4", timeout = 50L))

# tab view
app$click(selector = 'a[data-value="scenarios"]')
app$run_js("$('#btLoadScen').click();", timeout = 50L)
Sys.sleep(1L)
app$set_inputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
app$set_inputs(btLoadScenConfirm = "click")
expect_error(app$wait_for_js("$('#outputTabset > li').length===4", timeout = 5000L), NA)
expect_true(app$get_js("$('#contentScen_4 > li').length===7", timeout = 2000L))

# pivot view
app$click(selector = ".btSplitView button")
app$run_js("$('.btSplitView a[data-view=\\'pivot\\']').get(0).click();")
expect_error(app$wait_for_js("$('.box-title:visible button').is(':visible');", timeout = 5000L), NA)
app$run_js("$('.box-title:visible button').eq(0).click();")
expect_error(app$wait_for_js("$('#btLoadScenConfirm').is(':visible');", timeout = 5000L), NA)
app$set_inputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
Sys.sleep(0.2)
app$set_inputs(btLoadScenConfirm = "click")
expect_error(app$wait_for_js("$('#contentScen_0 > li').length===5;", timeout = 5000L), NA)

# split view
app$click(selector = ".btSplitView button")
app$run_js("$('.btSplitView a[data-view=\\'split\\']').get(0).click()")
expect_error(app$wait_for_js("$('.scenSplit-button-load:nth(1)').is(':visible');", timeout = 5000L), NA)
app$run_js("$('.scenSplit-button-load:nth(1)').click();")
expect_error(app$wait_for_js("$('#contentScen_2 > li').length===2;", timeout = 5000L), NA)

app$stop()
