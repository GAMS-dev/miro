app <- AppDriver$new("../../", name = "load_from_db_test_manual_import", variant = NULL, load_timeout = 20000)

Sys.sleep(2)
app$run_js("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,1,'GAMS');")
app$run_js("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,2,6);")
app$run_js("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(1,2,7);")
app$set_inputs(inputTabset = "inputTabset_2")
Sys.sleep(0.5)
app$run_js("HTMLWidgets.getInstance(document.getElementById('in_2')).hot.setDataAtCell(1,2,7);")
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
addSelectizeOption(app, "#editMetaTags", "tag1")
selectSelectizeOption(app, "#editMetaTags", "tag1")
addSelectizeOption(app, "#editMetaTags", "tag2")
selectSelectizeOption(app, "#editMetaTags", "tag2")
app$click(selector = "a[data-value='attachments']")
app$upload_file(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(0.5)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)

app$set_inputs(btSaveAs = "click")
Sys.sleep(0.5)
app$set_inputs(btRemoveOutput = "click")
Sys.sleep(0.5)
app$set_inputs(scenName = "blabla123")
app$click(selector = "#dialogSaveInit .bt-gms-confirm")
Sys.sleep(0.5)
app$wait_for_js("$('#inputDataTitle').text()==='blabla123'", timeout = 5000L)

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$set_inputs(btOverwriteScen = "click")
Sys.sleep(1)
expect_error(app$wait_for_js("$('#inputDataTitle').text()==='default'", timeout = 2000L), NA)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(cbSelectManuallyDb = "click")
app$set_inputs(selInputDataDb = c("price"))
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$set_inputs(btMergeInputData = "click")
Sys.sleep(1)
expect_equivalent(
  getHotData(app, "in_2"),
  tibble::tibble(
    scalar = c("maxstock", "trainingdays"),
    description = c("maximum number of stocks to select", "number of days for training"), value = c("2", "99")
  )
)
app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(1)
expect_equivalent(dplyr::filter(getHotData(app, "in_1"), symbol == "GAMS"), tibble::tibble(
  date = c("2016-01-04"),
  symbol = c("GAMS"), value = c(6)
))
expect_equivalent(dplyr::filter(getHotData(app, "in_1"), symbol == "AXP", date == "2016-01-04"), tibble::tibble(
  date = c("2016-01-04"),
  symbol = c("AXP"), value = c(7)
))
expect_identical(nrow(getHotData(app, "in_1")), 7561L)
expect_true(app$get_js("$('#inputDataTitle').text()==='default (*)'&&$('#dirtyFlagIcon').is(':visible');", timeout = 2000L))


# metadata should not be updated when manually importing data
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
expect_identical(app$get_values()$input[["editMetaName"]], "default")
expect_identical(app$get_values()$input[["editMetaTags"]], NULL)
app$click(selector = "a[data-value='attachments']")
Sys.sleep(0.5)
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 0L)

app$stop()
