app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("sandbox_compare_test")

expect_chartjs <- function(id, data, labels) {
  chartjsData <- jsonlite::fromJSON(app$getAllValues()$output[[id]])$x$data
  expect_equal(chartjsData$datasets$data[[1]], data)
  expect_identical(chartjsData$labels, labels)
}

currentUser <- Sys.info()[["user"]]

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(selLoadScen = paste0("1_", currentUser))
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1.5)
app$setInputs(inputTabset = "inputTabset_5")
Sys.sleep(0.5)
app$setInputs(slider_6 = 102)

app$findElement('a[data-value="scenarios"]')$click()
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='tab']")[[1]]$click()
Sys.sleep(0.5)

app$findElement("#cmpTabNoScenWrapper .action-button")$click()
Sys.sleep(0.5)
scenToSelect <- paste0(c("1_", "sb_", "3_"), currentUser)
app$setInputs(selLoadScen = scenToSelect, wait_ = FALSE, values_ = FALSE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$setInputs(scenTabset = "scen_4_")
app$setInputs(contentScen_4 = "contentScen_4_3")
expect_true(app$waitFor("$('#tab_4_8-scalarBoxes h3:contains(\"90\")').length>0", timeout = 50))
app$setInputs(btCompareScen = "click")
app$setInputs(scenTabset = "scen_5_")
Sys.sleep(1)
expect_true(app$waitFor("$('#tab_5_8-scalarBoxes h3:contains(\"102\")').length>0", timeout = 50))
expect_true(app$waitFor("$('#cmpScenTitle_4').text()==='default1'", timeout = 50))
expect_true(app$waitFor("$('#cmpScenTitle_5').text()==='default1 (Sandbox)'", timeout = 50))
expect_true(app$waitFor("$('#cmpScenTitle_6').text()==='default3'", timeout = 50))
app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
app$findElement('a[data-value="inputData"]')$click()
app$setInputs(slider_6 = 22)
app$findElement('a[data-value="scenarios"]')$click()
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='split']")[[1]]$click()

app$waitFor("$('.scenSplit-button-load').eq(1).click();true;", timeout = 50)
Sys.sleep(1)
app$setInputs(contentScen_2 = "contentScen_2_3")
Sys.sleep(1)
expect_true(app$waitFor("$('#tab_2_8-scalarBoxes h3:contains(\"22\")').length>0", timeout = 50))
expect_true(app$waitFor("$('#cmpScenTitle_2').text()==='default1 (Sandbox)'", timeout = 50))

app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$setInputs(editMetaName = "bliblablub")
app$setInputs(btUpdateMeta = "click")
Sys.sleep(1)

expect_true(app$waitFor("$('#cmpScenTitle_2').text()==='default1 (Sandbox)'", timeout = 50))
app$findElement("#refreshSandbox_2 button")$click()
Sys.sleep(1)
expect_true(app$waitFor("$('#cmpScenTitle_2').text()==='bliblablub (Sandbox)'", timeout = 50))

app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='pivot']")[[1]]$click()
Sys.sleep(0.5)

expect_true(app$waitFor("$('.box-title:visible button').eq(0).click();true;", timeout = 50))
Sys.sleep(1)
expect_true(app$waitFor("$('.base-scen').text()==='bliblablub (Sandbox)'", timeout = 50))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$setInputs(contentScen_0 = "contentScen_0_4")
Sys.sleep(0.5)
app$findElement('a[data-value="inputData"]')$click()
app$setInputs(inputTabset = "inputTabset_1")
Sys.sleep(0.5)
expect_true(app$waitFor("HTMLWidgets.getInstance($('.rhandsontable:visible').get(0)).hot.setDataAtRowProp(0,1,200);true;", timeout = 50))
Sys.sleep(0.5)
app$findElement('a[data-value="scenarios"]')$click()

app$setInputs("tab_0_3-miroPivot-pivotRenderer" = "stackedbar")
Sys.sleep(0.5)
expect_chartjs(
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
expect_true(app$waitFor("$('.box-title:visible button').eq(1).click();true;", timeout = 50))
Sys.sleep(3)
app$setInputs("tab_0_3-miroPivot-pivotRenderer" = "stackedbar")
Sys.sleep(0.5)
expect_chartjs(
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

app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='tab']")[[1]]$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_5_8-scalarBoxes h3:contains(\"102\")').length>0", timeout = 50))
# click refresh button in tab compare mode
expect_true(app$waitFor("$('.scen-button:visible').eq(0).click();true;", timeout = 50))
Sys.sleep(2)
expect_true(app$waitFor("$('#tab_5_8-scalarBoxes h3:contains(\"22\")').length>0", timeout = 50))
app$stop()
