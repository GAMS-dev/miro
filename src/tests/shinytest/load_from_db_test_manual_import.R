app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("load_from_db_test_manual_import")

app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)
Sys.sleep(1)
expect_true(app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,1,'GAMS');true;", timeout = 50L))
expect_true(app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,2,6);true;", timeout = 50L))
expect_true(app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(1,2,7);true;", timeout = 50L))
app$setInputs(inputTabset = "inputTabset_2")
expect_true(app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_2')).hot.setDataAtCell(1,2,7);true;", timeout = 50L))
app$setInputs(btSaveAs = "click")
Sys.sleep(0.5)
app$setInputs(btRemoveOutput = "click")
Sys.sleep(0.5)
app$setInputs(scenName = "blabla123")
app$findElement("#dialogSaveInit .bt-gms-confirm")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#inputDataTitle').text()==='blabla123'", timeout = 2000L))

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$setInputs(btOverwriteScen = "click")
Sys.sleep(1)
expect_true(app$waitFor("$('#inputDataTitle').text()==='default'", timeout = 2000L))
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(cbSelectManuallyDb = "click")
app$setInputs(selInputDataDb = c("price"))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$setInputs(btMergeInputData = "click")
Sys.sleep(1)
expect_equivalent(
  getHotData(app, "in_2"),
  tibble::tibble(
    scalar = c("maxstock", "trainingdays"),
    description = c("maximum number of stocks to select", "number of days for training"), value = c("2", "99")
  )
)
app$setInputs(inputTabset = "inputTabset_1")
Sys.sleep(0.5)
expect_equivalent(dplyr::filter(getHotData(app, "in_1"), symbol == "GAMS"), tibble::tibble(
  date = c("2016-01-04"),
  symbol = c("GAMS"), value = c(6)
))
expect_equivalent(dplyr::filter(getHotData(app, "in_1"), symbol == "AXP", date == "2016-01-04"), tibble::tibble(
  date = c("2016-01-04"),
  symbol = c("AXP"), value = c(7)
))
expect_identical(nrow(getHotData(app, "in_1")), 7561L)
expect_true(app$waitFor("$('#inputDataTitle').text()==='default (*)'&&$('#dirtyFlagIcon').is(':visible');", timeout = 2000L))
app$stop()
