app <- AppDriver$new("../../",
  name = "sandbox_compare_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

currentUser <- Sys.info()[["user"]]

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(selLoadScen = paste0("1_", currentUser))
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1.5)
app$set_inputs(inputTabset = "inputTabset_5")
Sys.sleep(0.5)
app$set_inputs(slider_6 = 102)

app$click(selector = 'a[data-value="scenarios"]')
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='tab']")
Sys.sleep(0.5)

app$click(selector = "#cmpTabNoScenWrapper .action-button")
Sys.sleep(0.5)
scenToSelect <- paste0(c("1_", "sb_", "3_"), currentUser)
app$set_inputs(selLoadScen = scenToSelect, wait_ = FALSE, values_ = FALSE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$set_inputs(scenTabset = "scen_4_")
app$set_inputs(contentScen_4 = "contentScen_4_3")
expect_error(app$wait_for_js("$('#tab_4_8-scalarBoxes h3:contains(\"90\")').length>0", timeout = 5000L), NA)
app$set_inputs(btCompareScen = "click")
app$set_inputs(scenTabset = "scen_5_")
expect_error(app$wait_for_js("$('#tab_5_8-scalarBoxes h3:contains(\"102\")').length>0", timeout = 5000L), NA)
expect_true(app$get_js("$('#cmpScenTitle_4').text()==='default1'", timeout = 50))
expect_true(app$get_js("$('#cmpScenTitle_5').text()==='default1 (Sandbox)'", timeout = 50))
expect_true(app$get_js("$('#cmpScenTitle_6').text()==='default3'", timeout = 50))
app$click(selector = 'a[data-value="inputData"]')
app$expect_values(output = "inputDataTitle")
app$set_inputs(slider_6 = 22)
app$click(selector = 'a[data-value="scenarios"]')
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='split']")

app$run_js("$('.scenSplit-button-load').eq(1).click();", timeout = 50)
Sys.sleep(1)
app$set_inputs(contentScen_2 = "contentScen_2_3")
expect_error(app$wait_for_js("$('#tab_2_8-scalarBoxes h3:contains(\"22\")').length>0", timeout = 5000L), NA)
expect_true(app$get_js("$('#cmpScenTitle_2').text()==='default1 (Sandbox)'", timeout = 50))

app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$set_inputs(editMetaName = "bliblablub")
app$set_inputs(btUpdateMeta = "click")

expect_error(app$wait_for_js("$('#cmpScenTitle_2').text()==='default1 (Sandbox)'", timeout = 5000L), NA)
app$click(selector = "#refreshSandbox_2 button")
expect_error(app$wait_for_js("$('#cmpScenTitle_2').text()==='bliblablub (Sandbox)'", timeout = 5000L), NA)
Sys.sleep(1)
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='pivot']")

expect_error(app$wait_for_js("$('.box-title button').is(':visible');", timeout = 5000L), NA)
app$run_js("$('.box-title:visible button').eq(0).click();")
expect_error(app$wait_for_js("$('.base-scen').text()==='bliblablub (Sandbox)'", timeout = 5000L), NA)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$set_inputs(contentScen_0 = "contentScen_0_4")
Sys.sleep(0.5)
app$click(selector = 'a[data-value="inputData"]')
app$set_inputs(inputTabset = "inputTabset_1")
app$run_js("HTMLWidgets.getInstance($('.rhandsontable:visible').get(0)).hot.setDataAtRowProp(0,1,200);")
Sys.sleep(0.5)
app$click(selector = 'a[data-value="scenarios"]')

app$set_inputs("tab_0_3-miroPivot-pivotRenderer" = "stackedbar")
Sys.sleep(0.5)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  c(600, 350, 600, 350, 600, 350),
  c(
    "bliblablub (Sandbox).San-Diego",
    "bliblablub (Sandbox).Seattle",
    "default1.San-Diego",
    "default1.Seattle",
    "default3.San-Diego",
    "default3.Seattle"
  )
)
# click refresh button in pivot compare mode
app$run_js("$('.box-title:visible button').eq(1).click();")
Sys.sleep(2)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  c(600, 200, 600, 350, 600, 350),
  c(
    "bliblablub (Sandbox).San-Diego",
    "bliblablub (Sandbox).Seattle",
    "default1.San-Diego",
    "default1.Seattle",
    "default3.San-Diego",
    "default3.Seattle"
  )
)

app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='tab']")
expect_error(app$wait_for_js("$('#tab_5_8-scalarBoxes h3:contains(\"102\")').length>0", timeout = 5000L), NA)
# click refresh button in tab compare mode
Sys.sleep(1)
expect_error(app$wait_for_js("$('.scen-button').is(':visible');", timeout = 5000L), NA)
app$run_js("$('.scen-button:visible').eq(0).click();")
expect_error(app$wait_for_js("$('#tab_5_8-scalarBoxes h3:contains(\"22\")').length>0", timeout = 5000L), NA)
app$stop()
