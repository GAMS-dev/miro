app <- AppDriver$new("../../",
  name = "attachments_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

# add attachment and save scenario
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 0L)
app$upload_file(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(0.5)
# try deleting file and adding it back
app$click(selector = "#btRemoveAttachment_1")
Sys.sleep(0.2)
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 0L)
app$upload_file(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(0.5)
# try to upload same file again, should not result in second attachment
app$upload_file(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(1)
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 1L)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$set_inputs(btSaveOutput = "click")
Sys.sleep(1)

# remove scenario from sandbox and load it again
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)

# download attachment
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
Sys.sleep(0.1)
app$click(selector = ".attachment-line > div:nth-child(1) > a")
app$expect_download("downloadAttachmentData", name = "attachment.md")

# un-check model read permissions for attachment several times, save and remove scenario from sandbox
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 1L)
expect_true(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]')[0].checked"),
  label = "1"
)
app$click(selector = ".attachment-line > div:nth-child(2) input[type=checkbox]")
expect_false(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]')[0].checked"),
  label = "2"
)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$set_inputs(btSaveOutput = "click")
Sys.sleep(1)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
Sys.sleep(0.1)
# attachmentList <- app$findElements(".attachment-line")
expect_false(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]')[0].checked"),
  label = "3"
)
app$click(selector = ".attachment-line > div:nth-child(2) input[type=checkbox]")
app$click(selector = ".attachment-line > div:nth-child(2) input[type=checkbox]")
app$click(selector = ".attachment-line > div:nth-child(2) input[type=checkbox]")
app$click(selector = ".attachment-line > div:nth-child(2) input[type=checkbox]")
app$click(selector = ".attachment-line > div:nth-child(2) input[type=checkbox]")
app$click(selector = ".attachment-line > div:nth-child(2) input[type=checkbox]")
expect_false(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]')[0].checked"),
  label = "4"
)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
Sys.sleep(0.1)
expect_false(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]')[0].checked"),
  label = "5"
)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = "#confirmModal .modal-footer .bt-gms-confirm")
Sys.sleep(0.5)

# load scenario, delete attachment and save scenario
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='attachments']")
Sys.sleep(0.1)
expect_false(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]')[0].checked"),
  label = "6"
)
app$click(selector = "#btRemoveAttachment_1")
Sys.sleep(0.2)
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 0L)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")

app$stop()
