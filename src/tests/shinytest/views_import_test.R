app <- AppDriver$new("../../", name = "views_import_test", variant = NULL, load_timeout = 20000)

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
Sys.sleep(0.5)
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 3L)
app$run_js("$('#currentViewsTable tbody tr').get(0).click()")
app$run_js("$('#currentViewsTable tbody tr').get(1).click()")
app$run_js("$('#currentViewsTable tbody tr').get(2).click()")
app$run_js("$('#editMetaUI .btn-default').get(2).click()")
app$expect_download("downloadViews", name = "views.json")

expect_true(app$get_js("$('#currentViewsTable tbody td')[0].innerHTML==='capacity of plant i in cases'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[1].innerHTML==='&lt;script&gt;alert(\\'asd\\')&lt;/script&gt;'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[2].innerHTML===''",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[3].innerHTML==='view2'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[4].innerHTML==='demand at market j in cases'",
  timeout = 50
))
expect_true(app$get_js("$('#currentViewsTable tbody td')[5].innerHTML==='&lt;script&gt;location.reload();&lt;/script&gt;'",
  timeout = 50
))
app$upload_file(file_addViews = "../data/good-views.json")
expect_true(app$get_js("$('#viewsCustomError')[0].innerText.includes('<script>alert(\\'asd\\')</script>')",
  timeout = 50
))
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#btGraphIn")
Sys.sleep(2)
app$click(selector = "#in_1-miroPivot-toggleViewButton")
Sys.sleep(1)
expect_identical(app$get_js("$('#in_1-miroPivot-savedViewsDD li').length"), 3L)
expect_true(app$get_js("$('#in_1-miroPivot-savedViewsDD li')[1].children[0].innerText==='<script>alert(\\'asd\\')</script>'"))
app$run_js("$('#in_1-miroPivot-savedViewsDD li').eq(1).children('.dropdown-item').click();")
expect_error(app$wait_for_js("$('#in_1-miroPivot-customError').is(':visible');", timeout = 5000L), NA)
expect_true(app$get_js("$('#in_1-miroPivot-customError').text().includes('idontexist')", timeout = 50))
app$expect_values(output = "inputDataTitle")
app$stop()
