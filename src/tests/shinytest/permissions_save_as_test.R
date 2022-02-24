app <- ShinyDriver$new("../../", loadTimeout = 100000)
app$snapshotInit("permissions_save_as_test")

app$snapshot(
  items = list(output = "outputDataTitle"),
  screenshot = TRUE
)

# get user name first
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='accessPerm']")$click()
Sys.sleep(0.5)
user <- app$getValue("editMetaWritePerm")[[1]]
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)

saveNewDefault <- function(newName = NULL, discardPerm = FALSE) {
  app$findElement("#btRemove1")$click()
  Sys.sleep(0.5)
  app$findElement(".modal-footer .bt-gms-confirm")$click()
  Sys.sleep(0.5)
  app$setInputs(btImport = "click")
  Sys.sleep(0.5)
  app$setInputs(selLoadScen = paste0("1_", user))
  expect_identical(startsWith(app$getValue("selLoadScen"), "1_"), TRUE)
  app$setInputs(btLoadScenConfirm = "click")
  Sys.sleep(1)
  app$setInputs(btSaveAs = "click")
  Sys.sleep(0.5)
  app$setInputs(scenName = newName)
  if (isTRUE(discardPerm)) {
    app$setInputs(newScenDiscardPerm = "click")
  }
  Sys.sleep(0.5)
  app$findElement("#shiny-modal .bt-gms-confirm")$click()
  Sys.sleep(1)
}

# save as new name, do not discard permissions
saveNewDefault("permission1")
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='accessPerm']")$click()
Sys.sleep(0.5)
expect_identical(length(app$getValue("editMetaReadPerm")), 2L)
expect_identical(length(app$getValue("editMetaWritePerm")), 1L)
expect_identical(length(app$getValue("editMetaExecPerm")), 2L)

# add attachment in order to test whether attachment is stored when permissions are discarded
app$findElement("a[data-value='attachments']")$click()
app$uploadFile(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(1)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$setInputs(btSave = "click")
Sys.sleep(1)

# save as same name and discard permissions
app$setInputs(btSaveAs = "click")
Sys.sleep(0.5)
app$setInputs(newScenDiscardPerm = "click")
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(0.5)
app$setInputs(btSaveConfirm = "click")
Sys.sleep(1)
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='accessPerm']")$click()
Sys.sleep(0.5)
expect_identical(length(app$getValue("editMetaReadPerm")), 1L)
expect_identical(length(app$getValue("editMetaWritePerm")), 1L)
expect_identical(length(app$getValue("editMetaExecPerm")), 1L)
app$findElement("a[data-value='attachments']")$click()
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 1L)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)

# save as new name and discard permissions
saveNewDefault("permission2", TRUE)
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='accessPerm']")$click()
Sys.sleep(0.5)
expect_identical(length(app$getValue("editMetaReadPerm")), 1L)
expect_identical(length(app$getValue("editMetaWritePerm")), 1L)
expect_identical(length(app$getValue("editMetaExecPerm")), 1L)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)

# save as same name and do not discard permissions
saveNewDefault("permission3")
app$setInputs(btSaveAs = "click")
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(0.5)
app$setInputs(btSaveConfirm = "click")
Sys.sleep(1)
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='accessPerm']")$click()
Sys.sleep(0.5)
expect_identical(length(app$getValue("editMetaReadPerm")), 2L)
expect_identical(length(app$getValue("editMetaWritePerm")), 1L)
expect_identical(length(app$getValue("editMetaExecPerm")), 2L)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)


app$stop()
