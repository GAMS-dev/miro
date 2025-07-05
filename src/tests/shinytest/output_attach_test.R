app <- AppDriver$new("../../",
  name = "output_attach_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(btSolve = "click")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('.shiny-notification-content').text()==='Added new output attachments: report.put, miro.log.'", timeout = 5000L), NA)
Sys.sleep(1)
errMsgText <- app$get_text(selector = "#shiny-modal .modal-body")
expect_match(errMsgText, "report2.put", fixed = TRUE)
expect_no_match(errMsgText, "report3.put", fixed = TRUE)

app$click(selector = "#shiny-modal .btn")
app$set_inputs(btEditMeta = "click")
Sys.sleep(2)
app$run_js("$('[data-value=\\'attachments\\']').get(0)")
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 4L)
expect_true(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]').get(1).checked"))
# miro.log as output attachment with execPerm=true
expect_true(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]').get(0).checked"))

# solve again to test previous local path allocation bug (Windows) b67fe02c3e959d1ad2a82b4e587e1b92a1c4cdca
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$click(selector = "a[data-value='inputData']")
app$set_inputs(btSolve = "click")
Sys.sleep(4L)
expect_error(app$click(selector = "#shiny-modal .btn"))
app$click(selector = "a[data-value='inputData']")
app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$run_js("$('[data-value=\\'attachments\\']').get(0).click()")
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 4L)

app$run_js("$('#btRemoveAttachment_1').click();")
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 3L)
app$run_js("$('[data-value=\\'general\\']').get(0)")
app$set_inputs(editMetaName = "test123")
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")
Sys.sleep(0.5)
app$set_inputs(btSaveOutput = "click")
Sys.sleep(0.5)
expect_error(app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm"), NA)
Sys.sleep(1)

app$stop()
