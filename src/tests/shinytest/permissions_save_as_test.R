app <- AppDriver$new("../../",
  name = "permissions_save_as_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

# get user name first
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(0.5)
app$wait_for_js("$('.access-perm-spinner').is(':visible')===false", timeout = 5000L)
user <- app$get_values()$input[["editMetaWritePerm"]][[1]]
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)

saveNewDefault <- function(newName = NULL, discardPerm = FALSE) {
  app$click(selector = "#btRemove1")
  Sys.sleep(0.5)
  app$click(selector = ".modal-footer .bt-gms-confirm")
  Sys.sleep(0.5)
  app$set_inputs(btImport = "click")
  Sys.sleep(0.5)
  app$set_inputs(selLoadScen = paste0("1_", user))
  expect_identical(startsWith(app$get_values()$input[["selLoadScen"]], "1_"), TRUE)
  app$set_inputs(btLoadScenConfirm = "click")
  Sys.sleep(1)
  app$set_inputs(btSaveAs = "click")
  Sys.sleep(0.5)
  app$set_inputs(scenName = newName)
  app$wait_for_js("$('.access-perm-spinner').is(':visible')===false", timeout = 5000L)
  if (isTRUE(discardPerm)) {
    app$set_inputs(
      editMetaReadPerm = user,
      editMetaWritePerm = user,
      editMetaExecPerm = user
    )
  }
  Sys.sleep(0.5)
  app$click(selector = "#shiny-modal #dialogSaveInit  .bt-gms-confirm")
  Sys.sleep(1)
}

# save as new name, do not discard permissions
saveNewDefault("permission1")
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(0.5)
app$wait_for_js("$('.access-perm-spinner').is(':visible')===false", timeout = 5000L)
expect_identical(length(app$get_values()$input[["editMetaReadPerm"]]), 2L)
expect_identical(length(app$get_values()$input[["editMetaWritePerm"]]), 1L)
expect_identical(length(app$get_values()$input[["editMetaExecPerm"]]), 2L)

# add attachment in order to test whether attachment is stored when permissions are discarded
app$click(selector = "a[data-value='attachments']")
app$upload_file(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(1)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")
Sys.sleep(0.5)
app$set_inputs(btSaveOutput = "click")
Sys.sleep(1)

# save as same name and discard permissions
app$set_inputs(btSaveAs = "click")
Sys.sleep(0.5)
app$wait_for_js("$('.access-perm-spinner').is(':visible')===false", timeout = 5000L)
app$set_inputs(
  editMetaReadPerm = user,
  editMetaWritePerm = user,
  editMetaExecPerm = user
)
Sys.sleep(0.5)
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btSaveConfirm = "click")
Sys.sleep(1)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(0.5)
app$wait_for_js("$('.access-perm-spinner').is(':visible')===false", timeout = 5000L)
expect_identical(length(app$get_values()$input[["editMetaReadPerm"]]), 1L)
expect_identical(length(app$get_values()$input[["editMetaWritePerm"]]), 1L)
expect_identical(length(app$get_values()$input[["editMetaExecPerm"]]), 1L)
app$click(selector = "a[data-value='attachments']")
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 1L)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)

# save as new name and discard permissions
saveNewDefault("permission2", TRUE)
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(0.5)
app$wait_for_js("$('.access-perm-spinner').is(':visible')===false", timeout = 5000L)
expect_identical(length(app$get_values()$input[["editMetaReadPerm"]]), 1L)
expect_identical(length(app$get_values()$input[["editMetaWritePerm"]]), 1L)
expect_identical(length(app$get_values()$input[["editMetaExecPerm"]]), 1L)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)

# save as same name and do not discard permissions
saveNewDefault("permission3")
app$set_inputs(btSaveAs = "click")
Sys.sleep(0.5)
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btSaveConfirm = "click")
Sys.sleep(1)
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(0.5)
app$wait_for_js("$('.access-perm-spinner').is(':visible')===false", timeout = 5000L)
expect_identical(length(app$get_values()$input[["editMetaReadPerm"]]), 2L)
expect_identical(length(app$get_values()$input[["editMetaWritePerm"]]), 1L)
expect_identical(length(app$get_values()$input[["editMetaExecPerm"]]), 2L)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)


app$stop()
