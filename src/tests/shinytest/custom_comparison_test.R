app <- AppDriver$new("../../", name = "custom_comparison_test", variant = NULL, load_timeout = 20000)

app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
app$upload_file(file_addAttachments = "../data/bad-views2.json")
Sys.sleep(1)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)

app$set_inputs(btSave = "click")
Sys.sleep(0.5)
app$set_inputs(btSaveOutput = "click")
Sys.sleep(2L)

app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
app$run_js("$('.attachment-line input').click();")
Sys.sleep(1)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)

app$click(selector = "a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(btSendQuery = "click")
app$set_inputs(batchLoadAll = "click")
Sys.sleep(1L)
# first custom analysis
app$run_js("$('#btBatchCompare+.dropdown-toggle').click()")
Sys.sleep(0.5)
expect_true(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(2)').text()==='My first custom analysis renderer'"))
app$run_js("$('#btBatchCompare+.dropdown-toggle').click()")
Sys.sleep(0.5)
expect_true(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(3)').text()==='My second custom analyzzzer'"))
app$run_js("$('#btBatchCompare+.dropdown-toggle').click()")
Sys.sleep(0.5)
expect_true(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(4)').text()==='External renderer'"))
app$run_js("$('#btBatchCompare+.dropdown-toggle').click()")
Sys.sleep(0.5)
app$run_js("$('#btBatchCompare~.dropdown-menu a:eq(2)').click();")
Sys.sleep(4L)
expect_true(grepl("<tdalign=\"right\">4.00</td><tdalign=\"right\">0.80</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">0.80</td>",
  gsub(" ", "", app$get_values()$output[["cmpCustom_test1-maxstockVsErrorTrainTable"]], fixed = TRUE),
  fixed = TRUE
))
expect_true(grepl("<tdalign=\"right\">4.00</td><tdalign=\"right\">9.51</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">9.51</td>",
  gsub(" ", "", app$get_values()$output[["cmpCustom_test1-maxstockVsErrorTestTable"]], fixed = TRUE),
  fixed = TRUE
))
expect_error(app$wait_for_js("$('#cmpCustom_test1-title').text()==='default'", timeout = 50L), NA)

app$click(selector = "a[data-value='loadResults']")
app$set_inputs(batchLoadAll = "click")
Sys.sleep(1L)
expect_error(app$get_js("$('#btBatchCompare+.dropdown-toggle').click()"), NA)
Sys.sleep(1L)
expect_error(app$get_js("$('#btBatchCompare~.dropdown-menu a:eq(4)').click();"), NA)
Sys.sleep(2L)
expect_error(app$wait_for_js("$('#cmpCustom_test3-title').text()==='bla blubB'", timeout = 2000L), NA)
expect_error(app$wait_for_js("$('#btSelectCompareMode').text()==='External renderer'", timeout = 50L), NA)
app$run_js("$('#btSelectCompareMode').click()&&$('.change-dd-button[data-action-val=\"test1\"]').click()")

app$click(selector = "a[data-value='inputData']")
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/pickstock_custom_analysis.miroscen"))
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
Sys.sleep(1)
app$click(selector = "a[data-value='scenarios']")
app$click(selector = "#btRefreshCustomCmp_1")
Sys.sleep(2)
expect_true(grepl("<tdalign=\"right\">8.00</td><tdalign=\"right\">0.12</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">0.80</td>",
  gsub(" ", "", app$get_values()$output[["cmpCustom_test1-maxstockVsErrorTrainTable"]], fixed = TRUE),
  fixed = TRUE
))
expect_true(grepl("<tdalign=\"right\">8.00</td><tdalign=\"right\">1.68</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">9.51</td>",
  gsub(" ", "", app$get_values()$output[["cmpCustom_test1-maxstockVsErrorTestTable"]], fixed = TRUE),
  fixed = TRUE
))
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
app$click(selector = '#editMetaUI a[data-value="views"]')
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 1L)
expect_error(app$wait_for_js("$('#currentViewsTable tbody td')[0].innerHTML==='My first custom analysis renderer'",
  timeout = 50
), NA)
expect_error(app$wait_for_js("$('#currentViewsTable tbody td')[1].innerHTML==='test1'",
  timeout = 50
), NA)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$run_js("$(\"button[onclick*='btCloseScenCmp']:visible\").click();")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#cmpCustomNoScenWrapper_1').is(':visible')", timeout = 50L), NA)
app$run_js("$('#cmpCustomNoScenWrapper_1 button').click();")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#btSendQuery').is(':visible')", timeout = 50L), NA)

app$click(selector = "a[data-value='inputData']")
Sys.sleep(0.5)
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
app$upload_file(file_addAttachments = "../data/bad-views2.json")
Sys.sleep(1)
app$run_js("$('.attachment-line input').click();")
Sys.sleep(0.5)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)
app$click(selector = "a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(batchLoadAll = "click")
# first custom analysis
app$run_js("$('#btBatchCompare+.dropdown-toggle').click()&&$('#btBatchCompare~.dropdown-menu a:eq(4)').click();")
Sys.sleep(2L)
expect_true(app$get_js("$('.modal-body').is(':visible')===false"))

app$stop()
