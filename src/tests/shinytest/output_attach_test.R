app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("output_attach_test")

app$snapshot(
  items = list(output = "outputDataTitle"),
  screenshot = TRUE
)

app$setInputs(btSolve = "click")
Sys.sleep(0.5)
expect_true(app$waitFor("$('.shiny-notification-content').text()==='Added new output attachments: report.put, miro.log.'", timeout = 5000L))
Sys.sleep(1)
errMsgText <- app$findElement("#shiny-modal .modal-body")$getText()
expect_identical(grepl("report2.put", errMsgText, fixed = TRUE), TRUE)
expect_identical(grepl("report3.put", errMsgText, fixed = TRUE), FALSE)

app$findElement("#shiny-modal .btn")$click()
app$setInputs(btEditMeta = "click")
Sys.sleep(2)
app$findElement("[data-value='attachments']")$click()
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 4L)
expect_identical(attachmentList[[2]]$findElement(".checkbox input")$getAttribute("checked"), "true")
# miro.log as output attachment with execPerm=true
expect_identical(attachmentList[[1]]$findElement(".checkbox input")$getAttribute("checked"), "true")

# solve again to test previous local path allocation bug (Windows) b67fe02c3e959d1ad2a82b4e587e1b92a1c4cdca
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='inputData']")$click()
app$setInputs(btSolve = "click")
Sys.sleep(4L)
expect_error(app$findElement("#shiny-modal .btn")$click())
app$findElement("a[data-value='inputData']")$click()
app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$findElement("[data-value='attachments']")$click()
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 4L)

app$findElement("#btRemoveAttachment_1")$click()
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 3L)
app$findElement("[data-value='general']")$click()
app$setInputs(editMetaName = "test123")
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$setInputs(btSave = "click")
Sys.sleep(0.5)
app$setInputs(btSaveOutput = "click")
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)

app$snapshot(
  items = list(output = "outputDataTitle"),
  screenshot = TRUE
)
app$stop()
