app <- AppDriver$new("../../", name = "views_metadata_test", variant = NULL, load_timeout = 20000)

getData <- function(id) {
  return(jsonlite::fromJSON(app$get_values()$output[[id]])$x$data$datasets$data)
}

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_local")
expect_error(app$wait_for_js("$('#localInput.shiny-bound-input').is(':visible');", timeout = 2000L), NA)
app$upload_file(localInput = "../data/transport.gdx")
app$set_inputs(btImportLocal = "click")
Sys.sleep(0.5)
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
app$click(selector = '#editMetaUI a[data-value="views"]')
app$upload_file(file_addViews = "../data/transport.gdx")
expect_true(app$get_js("$('#viewsInvalidDataError').is(':visible');", timeout = 1000))
app$upload_file(file_addViews = "../data/bad-views.json")
expect_true(app$get_js("$('#viewsInvalidDataError').is(':visible');", timeout = 1000))
app$upload_file(file_addViews = "../data/bad-views2.json")
expect_true(app$get_js("$('#viewsInvalidDataError').is(':visible');", timeout = 1000))
app$upload_file(file_addViews = "../data/good-views.json")
Sys.sleep(0.5)
expect_error(app$get_js("#currentViewsTable tbody tr"), NA)
expect_true(app$get_js("$('#viewsCustomError')[0].innerText.includes('idontexist')",
  timeout = 50
))
app$click(selector = "#editMetaUI .bt-remove")
expect_true(app$get_js("$('#viewsNoneSelected').is(':visible');", timeout = 50))
app$run_js("$('#currentViewsTable tbody tr').get(0).click()")
app$click(selector = "#editMetaUI .bt-remove")
Sys.sleep(0.5)
expect_true(app$get_js("$('#currentViewsTable-noData').is(':visible');", timeout = 50))
expect_true(app$get_js("$('#currentViewsTable').is(':hidden');", timeout = 50))
app$upload_file(file_addViews = "../data/good-views2.json")
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 3L)
app$run_js("$('#currentViewsTable tbody tr').get(0).click()")
app$run_js("$('#currentViewsTable tbody tr').get(1).click()")
app$run_js("$('#currentViewsTable tbody tr').get(2).click()")
app$run_js("$('#editMetaUI .btn-default').get(2).click()")
app$expect_download("downloadViews", name = "views.json")
app$run_js("$('#currentViewsTable tbody tr').get(1).click()")
app$run_js("$('#editMetaUI .btn-default').get(2).click()")
app$expect_download("downloadViews", name = "views2.json")

expect_true(app$get_js("$('#currentViewsTable tbody tr')[0].innerHTML==='<td data-val=\"YQ==\">capacity of plant i in cases</td><td>&lt;script&gt;alert(\\'asd\\')&lt;/script&gt;</td>'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody tr')[1].innerHTML==='<td data-val=\"YQ==\"></td><td>view2</td>'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody tr')[2].innerHTML==='<td data-val=\"Yg==\">demand at market j in cases</td><td>&lt;script&gt;location.reload();&lt;/script&gt;</td>'",
  timeout = 50
))
app$upload_file(file_addViews = "../data/good-views.json")
expect_true(app$get_js("$('#viewsCustomError')[0].innerText.includes('<script>alert(\\'asd\\')</script>')",
  timeout = 50
))
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#btGraphIn")
Sys.sleep(2.5)
app$click(selector = "#in_1-miroPivot-toggleViewButton")
app$wait_for_js("$('#in_1-miroPivot-savedViewsDD li').length===3", timeout = 5000L)
expect_true(app$get_js("$('#in_1-miroPivot-savedViewsDD li')[1].children[0].innerText==='<script>alert(\\'asd\\')</script>'"))
app$click(selector = "#in_1-miroPivot-saveView")
Sys.sleep(0.5)
app$set_inputs("in_1-miroPivot-newViewName" = "\t\n ")
Sys.sleep(1L)
expect_true(app$get_js("$('#in_1-miroPivot-newViewName').is(':visible')"))
app$set_inputs("in_1-miroPivot-newViewName" = "new test view")
app$set_inputs("in_1-miroPivot-saveViewConfirm" = "click")
Sys.sleep(0.5)
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
app$click(selector = '#editMetaUI a[data-value="views"]')
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 4L)
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#in_1-miroPivot-toggleViewButton")
Sys.sleep(0.5)
app$run_js("$('#in_1-miroPivot-savedViewsDD .dropdown-item').get(1).click()")
Sys.sleep(1)
expect_identical(getData("in_1-miroPivot-pivotChart"), list(600L))
app$click(selector = "#in_1-miroPivot-toggleViewButton")
Sys.sleep(0.5)
app$run_js("$('#in_1-miroPivot-savedViewsDD .dropdown-item').get(3).click()")
Sys.sleep(0.5)
expect_equal(getData("in_1-miroPivot-pivotChart"), list(350L))
app$click(selector = "#in_1-miroPivot-toggleViewButton")
app$run_js("$('.miro-pivot-view-button').get(1).click()")
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
app$click(selector = '#editMetaUI a[data-value="views"]')
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 3L)
expect_true(app$get_js("$('#currentViewsTable tbody td')[0].innerHTML==='capacity of plant i in cases'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[1].innerHTML==='view2'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[2].innerHTML===''",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[3].innerHTML==='new test view'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[4].innerHTML==='demand at market j in cases'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[5].innerHTML==='&lt;script&gt;location.reload();&lt;/script&gt;'",
  timeout = 50
))
app$stop()
