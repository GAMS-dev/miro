app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("pivot_comp_views_test")

expect_chartjs <- function(id, data, labels) {
  chartjsData <- jsonlite::fromJSON(app$getAllValues()$output[[id]])$x$data
  if (is.list(data)) {
    expect_equal(chartjsData$datasets$data, data)
  } else {
    expect_equal(chartjsData$datasets$data[[1]], data)
  }
  expect_identical(chartjsData$labels, labels)
}
app$snapshot(items = list(output = "inputDataTitle"), screenshot = TRUE)

app$setInputs(btImport = "click")
Sys.sleep(0.5)
savedScen <- getSelectizeOptions(app, "#selLoadScen")
app$setInputs(selLoadScen = savedScen[1])
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1.5)
expect_true(app$waitFor("HTMLWidgets.getInstance($('.rhandsontable:visible').get(0)).hot.setDataAtRowProp(0,1,200);true;", timeout = 50))
Sys.sleep(0.5)
app$findElement('a[data-value="scenarios"]')$click()
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='pivot']")[[1]]$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('.box-title:visible button').eq(0).click();true;", timeout = 50))
Sys.sleep(1)
app$setInputs(selLoadScen = savedScen[1])
Sys.sleep(0.2)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$setInputs(contentScen_0 = "contentScen_0_4")
Sys.sleep(0.5)
app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(1)
expect_identical(length(app$findElements("#tab_0_3-miroPivot-savedViewsDD li")), 1L)


# add pivot comparison views
app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$findElement('#editMetaUI a[data-value="Views"]')$click()
Sys.sleep(0.5)
app$uploadFile(file_addViews = "../data/pivot-comp-views.json")
Sys.sleep(1)
expect_true(app$waitFor("$('#currentViewsTable tbody td')[2].innerHTML==='Pivot Comparison: capacity of plant i in cases'",
  timeout = 50
))
app$findElement('button[data-dismiss="modal"]')$click()
Sys.sleep(1)
app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(1)
expect_identical(length(app$findElements("#tab_0_3-miroPivot-savedViewsDD li")), 2L)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD li')[1].children[0].innerText==='<script>alert(\\\\'asd\\\\')</script>'", timeout = 50))
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(1).click();true;", timeout = 50))
Sys.sleep(1)
expect_chartjs(
  "tab_0_3-miroPivot-pivotChart",
  list(350, 200),
  c("value")
)
app$findElement("#tab_0_3-miroPivot-saveView")$click()
Sys.sleep(1)
app$setInputs("tab_0_3-miroPivot-newViewName" = "new test view")
app$setInputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
Sys.sleep(1)
app$findElement("#tab_0_3-miroPivot-saveView")$click()
Sys.sleep(1)
app$setInputs("tab_0_3-miroPivot-newViewName" = "new test view")
app$setInputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
expect_true(app$waitFor("$('#tab_0_3-miroPivot-errUniqueName').is(':visible');", timeout = 50))
app$setInputs("tab_0_3-miroPivot-saveViewCancelOverwrite" = "click")
expect_true(app$waitFor("$('#tab_0_3-miroPivot-errUniqueName').is(':visible') === false;", timeout = 50))
app$setInputs("tab_0_3-miroPivot-newViewName" = "abc")
app$setInputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
Sys.sleep(1)

app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(3).text()==='abc';", timeout = 50))
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(3).click();true;", timeout = 50))
expect_chartjs(
  "tab_0_3-miroPivot-pivotChart",
  list(350, 200),
  c("value")
)

expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(0).click();true;", timeout = 50))
app$setInputs(`tab_0_3-miroPivot-pivotRenderer` = "bar")
Sys.sleep(0.5)
expect_chartjs(
  "tab_0_3-miroPivot-pivotChart",
  list(c(600, 350, 600, 200)),
  c(
    "default.San-Diego", "default.Seattle", "default (Sandbox).San-Diego",
    "default (Sandbox).Seattle"
  )
)

app$findElement("#tab_0_3-miroPivot-saveView")$click()
Sys.sleep(1)
app$setInputs("tab_0_3-miroPivot-newViewName" = "abc")
app$setInputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
expect_true(app$waitFor("$('#tab_0_3-miroPivot-errUniqueName').is(':visible');", timeout = 50))
app$setInputs("tab_0_3-miroPivot-saveViewOverwrite" = "click")
Sys.sleep(1)

# check that view 'abc' was overwritten successfully
app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(0).click();true;", timeout = 50))
Sys.sleep(0.5)
app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(3).text()==='abc';", timeout = 50))
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(3).click();true;", timeout = 50))
Sys.sleep(1)
expect_chartjs(
  "tab_0_3-miroPivot-pivotChart",
  list(c(600, 350, 600, 200)),
  c(
    "default.San-Diego", "default.Seattle", "default (Sandbox).San-Diego",
    "default (Sandbox).Seattle"
  )
)

# edit 'abc' view
app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .miro-pivot-view-button').eq(4).click();", timeout = 50))
Sys.sleep(1)
app$setInputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
Sys.sleep(1)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-errUniqueName').is(':visible')===false;", timeout = 50))

app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .miro-pivot-view-button').eq(4).click();", timeout = 50))
Sys.sleep(1)
app$setInputs("tab_0_3-miroPivot-newViewName" = "new test view")
app$setInputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
expect_true(app$waitFor("$('#tab_0_3-miroPivot-errUniqueName').is(':visible');", timeout = 50))
app$setInputs("tab_0_3-miroPivot-saveViewOverwrite" = "click")
Sys.sleep(1)

app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(0).click();true;", timeout = 50))
Sys.sleep(0.5)
app$findElement("#tab_0_3-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_identical(length(app$findElements("#tab_0_3-miroPivot-savedViewsDD li")), 3L)
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).text()==='new test view';", timeout = 50))
expect_true(app$waitFor("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).click();true;", timeout = 50))
Sys.sleep(1)
expect_chartjs(
  "tab_0_3-miroPivot-pivotChart",
  list(c(600, 350, 600, 200)),
  c(
    "default.San-Diego", "default.Seattle", "default (Sandbox).San-Diego",
    "default (Sandbox).Seattle"
  )
)

# check that new views were saved
app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$findElement('#editMetaUI a[data-value="Views"]')$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#currentViewsTable tbody td')[2].innerHTML==='Pivot Comparison: capacity of plant i in cases'",
  timeout = 50
))
expect_true(app$waitFor("$('#currentViewsTable tbody td')[3].innerHTML==='&lt;script&gt;alert(\\\\'asd\\\\')&lt;/script&gt;'",
  timeout = 50
))
expect_true(app$waitFor("$('#currentViewsTable tbody td')[5].innerHTML==='new test view'",
  timeout = 50
))

app$stop()
