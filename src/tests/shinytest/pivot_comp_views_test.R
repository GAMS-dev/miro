app <- AppDriver$new("../../",
  name = "pivot_comp_views_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
savedScen <- getSelectizeOptions(app, "#selLoadScen")
app$set_inputs(selLoadScen = savedScen[1])
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1.5)
app$run_js("HTMLWidgets.getInstance($('.rhandsontable:visible').get(0)).hot.setDataAtRowProp(0,1,200);")
Sys.sleep(0.5)
app$click(selector = 'a[data-value="scenarios"]')
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='pivot']")
Sys.sleep(0.5)
app$run_js("$('.box-title:visible button').eq(0).click();")
Sys.sleep(1)
app$set_inputs(selLoadScen = savedScen[1])
Sys.sleep(0.2)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$set_inputs(contentScen_0 = "contentScen_0_4")
expect_error(app$wait_for_js("$('#tab_0_3-miroPivot-toggleViewButton').is(':visible')", timeout = 4000L), NA)
app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(1)
expect_identical(app$get_js("$('#tab_0_3-miroPivot-savedViewsDD li').length"), 1L)


# add pivot comparison views
app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
Sys.sleep(0.5)
app$upload_file(file_addViews = "../data/pivot-comp-views.json")
Sys.sleep(1)
expect_error(app$wait_for_js("$('#currentViewsTable tbody td')[2].innerHTML==='Pivot comparison: capacity of plant i in cases'",
  timeout = 50
), NA)
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(1)
expect_identical(app$get_js("$('#tab_0_3-miroPivot-savedViewsDD li').length"), 2L)
expect_true(app$get_js("$('#tab_0_3-miroPivot-savedViewsDD li')[1].children[0].innerText==='<script>alert(\\'asd\\')</script>'", timeout = 50))
app$run_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(1).click()")
Sys.sleep(1)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  list(350, 200),
  c("value")
)
app$click(selector = "#tab_0_3-miroPivot-saveView")
Sys.sleep(1)
app$set_inputs("tab_0_3-miroPivot-newViewName" = "new test view")
app$set_inputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
Sys.sleep(1)
app$click(selector = "#tab_0_3-miroPivot-saveView")
Sys.sleep(1)
app$set_inputs("tab_0_3-miroPivot-newViewName" = "new test view")
app$set_inputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
expect_true(app$get_js("$('#tab_0_3-miroPivot-errUniqueName').is(':visible');", timeout = 50))
app$set_inputs("tab_0_3-miroPivot-saveViewCancelOverwrite" = "click")
expect_true(app$get_js("$('#tab_0_3-miroPivot-errUniqueName').is(':visible') === false;", timeout = 50))
app$set_inputs("tab_0_3-miroPivot-newViewName" = "abc")
app$set_inputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
Sys.sleep(1)

app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(1)
expect_true(app$get_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).text()==='abc';", timeout = 50))
app$run_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).click()")
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  list(350, 200),
  c("value")
)

app$run_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(0).click()")
Sys.sleep(1)
app$set_inputs(`tab_0_3-miroPivot-pivotRenderer` = "bar")
Sys.sleep(1)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  list(c(600, 350, 600, 200)),
  c(
    "default.San-Diego", "default.Seattle", "default (Sandbox).San-Diego",
    "default (Sandbox).Seattle"
  )
)

app$click(selector = "#tab_0_3-miroPivot-saveView")
Sys.sleep(1)
app$set_inputs("tab_0_3-miroPivot-newViewName" = "abc")
app$set_inputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
expect_true(app$get_js("$('#tab_0_3-miroPivot-errUniqueName').is(':visible');", timeout = 50))
app$set_inputs("tab_0_3-miroPivot-saveViewOverwrite" = "click")
Sys.sleep(1)

# check that view 'abc' was overwritten successfully
app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(0.5)
app$run_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(0).click();")
Sys.sleep(0.5)
app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_true(app$get_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).text()==='abc';", timeout = 50))
app$run_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).click()")
Sys.sleep(1)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  list(c(600, 350, 600, 200)),
  c(
    "default.San-Diego", "default.Seattle", "default (Sandbox).San-Diego",
    "default (Sandbox).Seattle"
  )
)

# edit 'abc' view
app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#tab_0_3-miroPivot-savedViewsDD .miro-pivot-view-button').eq(2).click()", timeout = 50), NA)
Sys.sleep(1)
app$set_inputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
Sys.sleep(1)
expect_true(app$get_js("$('#tab_0_3-miroPivot-errUniqueName').is(':visible')===false;", timeout = 50))

app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#tab_0_3-miroPivot-savedViewsDD .miro-pivot-view-button').eq(2).click()", timeout = 50), NA)
Sys.sleep(1)
app$set_inputs("tab_0_3-miroPivot-newViewName" = "new test view")
app$set_inputs("tab_0_3-miroPivot-saveViewConfirm" = "click")
expect_true(app$get_js("$('#tab_0_3-miroPivot-errUniqueName').is(':visible');", timeout = 50))
app$set_inputs("tab_0_3-miroPivot-saveViewOverwrite" = "click")
Sys.sleep(1)

app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(0.5)
app$run_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(0).click()")
Sys.sleep(0.5)
app$click(selector = "#tab_0_3-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_identical(app$get_js("$('#tab_0_3-miroPivot-savedViewsDD li').length"), 3L)
expect_true(app$get_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).text()==='new test view';", timeout = 50))
app$run_js("$('#tab_0_3-miroPivot-savedViewsDD .view-dropdown-item').eq(2).click()")
Sys.sleep(1)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  list(c(600, 350, 600, 200)),
  c(
    "default.San-Diego", "default.Seattle", "default (Sandbox).San-Diego",
    "default (Sandbox).Seattle"
  )
)

# check that new views were saved
app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#currentViewsTable tbody td')[2].innerHTML==='Pivot comparison: capacity of plant i in cases'",
  timeout = 50
), NA)
expect_true(app$get_js("$('#currentViewsTable tbody td')[3].innerHTML==='&lt;script&gt;alert(\\'asd\\')&lt;/script&gt;'",
  timeout = 50
))
expect_error(app$wait_for_js("$('#currentViewsTable tbody td')[5].innerHTML==='new test view'",
  timeout = 50
), NA)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)

# check default view
app$set_inputs(contentScen_0 = "contentScen_0_1")
Sys.sleep(1)
expect_chartjs(
  app,
  "tab_0_1-miroPivot-pivotChart",
  list(
    c(NA, 275, 275, 300, 50, NA),
    c(NA, 275, 275, 300, 50, NA)
  ),
  c(
    "San-Diego.Chicago", "San-Diego.New-york", "San-Diego.Topeka", "Seattle.Chicago", "Seattle.New-york", "Seattle.Topeka"
  )
)

app$stop()
