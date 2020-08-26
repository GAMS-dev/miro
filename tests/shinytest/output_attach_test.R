app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("output_attach_test")

app$snapshot(items = list(output = "outputDataTitle"),
             screenshot = TRUE)

app$setInputs(btSolve = "click")
Sys.sleep(4)
errMsgText <- app$findElement("#shiny-modal .modal-body")$getText()
expect_identical(grepl("report2.put", errMsgText, fixed = TRUE), TRUE)
expect_identical(grepl("report3.put", errMsgText, fixed = TRUE), FALSE)

app$findElement("#shiny-modal .btn")$click()
app$setInputs(btEditMeta = "click")
Sys.sleep(2)
app$findElement("[data-value='Attachments']")$click()
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 4L)
expect_identical(attachmentList[[2]]$findElement(".checkbox input")$getAttribute("checked"), "true")
app$findElement("#btRemoveAttachment_1")$click()
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 3L)
app$findElement("[data-value='General options']")$click()
app$setInputs(editMetaName = "test123")
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$setInputs(btSave = "click")
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(0.5)

app$snapshot(items = list(output = "outputDataTitle"),
             screenshot = TRUE)
app$stop()

