app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("views_import_test")

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$findElement('#editMetaUI a[data-value="views"]')$click()
Sys.sleep(0.5)
expect_identical(length(app$findElements("#currentViewsTable tbody tr")), 3L)
app$findElements("#currentViewsTable tbody tr")[[1]]$click()
app$findElements("#currentViewsTable tbody tr")[[2]]$click()
app$findElements("#currentViewsTable tbody tr")[[3]]$click()
app$findElements("#editMetaUI .btn-default")[[3]]$click()
app$snapshotDownload("downloadViews", "views.json")

expect_true(app$waitFor("$('#currentViewsTable tbody td')[0].innerHTML==='capacity of plant i in cases'",
  timeout = 50
))
expect_true(app$waitFor("$('#currentViewsTable tbody td')[1].innerHTML==='&lt;script&gt;alert(\\\\'asd\\\\')&lt;/script&gt;'",
  timeout = 50
))
expect_true(app$waitFor("$('#currentViewsTable tbody td')[2].innerHTML===''",
  timeout = 50
))
expect_true(app$waitFor("$('#currentViewsTable tbody td')[3].innerHTML==='view2'",
  timeout = 50
))
expect_true(app$waitFor("$('#currentViewsTable tbody td')[4].innerHTML==='demand at market j in cases'",
  timeout = 50
))
expect_true(app$waitFor("$('#currentViewsTable tbody td')[5].innerHTML==='&lt;script&gt;location.reload();&lt;/script&gt;'",
  timeout = 50
))
app$uploadFile(file_addViews = "../data/good-views.json")
expect_true(app$waitFor("$('#viewsCustomError')[0].innerText.includes('<script>alert(\\\\'asd\\\\')</script>')",
  timeout = 50
))
app$findElement('button[data-dismiss="modal"]')$click()
Sys.sleep(1)
app$findElement("#btGraphIn")$click()
Sys.sleep(1.5)
app$findElement("#in_1-miroPivot-toggleViewButton")$click()
Sys.sleep(1)
expect_identical(length(app$findElements("#in_1-miroPivot-savedViewsDD li")), 3L)
expect_true(app$waitFor("$('#in_1-miroPivot-savedViewsDD li')[1].children[0].innerText==='<script>alert(\\\\'asd\\\\')</script>'"))
expect_true(app$waitFor("$('#in_1-miroPivot-savedViewsDD li').eq(1).children('.dropdown-item').click();true;", timeout = 50))
Sys.sleep(1)
expect_true(app$waitFor("$('#in_1-miroPivot-customError').is(':visible')", timeout = 50))
expect_true(app$waitFor("$('#in_1-miroPivot-customError').text().includes('idontexist')", timeout = 50))
app$snapshot(items = list(output = c("cmpScenTitle_2")), screenshot = TRUE)
app$stop()
