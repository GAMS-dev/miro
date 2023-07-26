# launch user1
Sys.setenv(MIRO_USERNAME = "testuser1")
Sys.setenv(MIRO_USERGROUPS = "users")
app <- AppDriver$new("../../",
  name = "permissions_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

user1 <- Sys.getenv("MIRO_USERNAME")
# launch user2
Sys.setenv(MIRO_USERNAME = "testuser2")
app2 <- AppDriver$new("../../",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

user2 <- Sys.getenv("MIRO_USERNAME")
Sys.unsetenv("MIRO_USERNAME")

# scenarios visible for user2
# 1_ default user1
# 2_ default user2
# 3_ rx permissions from user1
# 4_ r permissions from user1
# 5_ rwx permissions from user1

# user1 saves scenario with user2 permissions rx
app$set_inputs(btSaveAs = "click")
Sys.sleep(0.5)
app$set_inputs(scenName = "user1Scenario1")
Sys.sleep(0.5)
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
Sys.sleep(1)

# user1 saves scenario with user2 permissions r
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(0.5)
app$set_inputs(editMetaExecPerm = user1)
Sys.sleep(0.5)
app$set_inputs(btUpdateMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(btSaveAs = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(scenName = "user1Scenario3")
Sys.sleep(0.5)
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")

# user1 saves scenario where user2 has no permissions
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(1)
app$set_inputs(editMetaReadPerm = user1)
app$set_inputs(btUpdateMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(btSaveAs = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(scenName = "noUser2")
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)

# user1 saves scenario with user2 permissions rwx
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(1)
app$set_inputs(editMetaReadPerm = c(getSelectizeOptions(app, "#editMetaReadPerm"), "#users"))
app$set_inputs(editMetaWritePerm = c(getSelectizeOptions(app, "#editMetaWritePerm"), "#users"))
app$set_inputs(editMetaExecPerm = c(getSelectizeOptions(app, "#editMetaExecPerm"), "#users"))
app$set_inputs(btUpdateMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(btSaveAs = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(scenName = "user1Scenario2")
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
Sys.sleep(1)
# user1 stays in rwx scenario for locked scenario test


# user2 loads locked scenario with permissions rwx
app2$click(selector = "#btRemove1")
Sys.sleep(0.5)
app2$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app2$set_inputs(btImport = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
# scen noUser2 should not be visible since user2 has no permissions there
expect_length(getSelectizeOptions(app2, "#selLoadScen"), 5L)
app2$set_inputs(selLoadScen = paste0("6_", user1))
app2$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# expect notification and lock symbol to be visible
expect_error(app2$click(selector = "#shiny-notification-panel .shiny-notification-close"), NA)
expect_identical(app2$get_text(selector = "#inputDataTitle .badge"), user1)
expect_error(app2$click(selector = "#inputDataTitle .fa-lock"), NA)
# not possible to delete locked scenario
app2$set_inputs(btDelete = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app2$click(selector = "#btDeleteConfirm")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#shiny-modal .btn-default"), NA)
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
# user2 is allowed to change name, tags, attachments in sandbox, no permissions tab visible
app2$set_inputs(btEditMeta = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app2$set_inputs(editMetaName = "newNameForReadonly")
app2$set_inputs(editMetaTags = c("tag1", "tag2"))
app2$click(selector = "a[data-value='attachments']")
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 0L)
app2$upload_file(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(1)
attachmentList <- app2$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 1L)
expect_error(app2$click(selector = "a[data-value='accessPerm']"))
app2$set_inputs(btUpdateMeta = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
app2$set_inputs(btSave = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app2$set_inputs(btSaveOutput = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#btSaveReadonly"), NA)
Sys.sleep(0.5)
# discard permissions true per default for not owned scenarios
expect_true(app2$get_js("$('#newScenDiscardPerm').get(0).checked"))
app2$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
Sys.sleep(1)


# user1 closes rwx scenario, and overwrites default scenario permissions, should be reverted after new start
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(selLoadScen = paste0("1_", user1))
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(1)
app$set_inputs(editMetaReadPerm = user1, editMetaExecPerm = user1)
app$set_inputs(btUpdateMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$stop()
Sys.setenv(MIRO_USERNAME = "testuser1")
app <- AppDriver$new("../../",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$click(selector = "a[data-value='accessPerm']")
Sys.sleep(1)
expect_identical(length(app$get_values()$input[["editMetaReadPerm"]]), 2L)
expect_identical(length(app$get_values()$input[["editMetaWritePerm"]]), 1L)
expect_identical(length(app$get_values()$input[["editMetaExecPerm"]]), 2L)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)


################# Start general permission tests #################

# user2 loads scenario with permissions rwx
app2$set_inputs(btImport = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app2$set_inputs(selLoadScen = paste0("5_", user1))
app2$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# expect permissions section visible in edit metadata
app2$set_inputs(btEditMeta = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
expect_error(app2$click(selector = "a[data-value='accessPerm']"), NA)
Sys.sleep(1)
expect_length(getSelectizeOptions(app2, "#editMetaWritePerm"), 2L)
# expect user2 not be allowed to remove user1 (owner) from permissions
app2$set_inputs(editMetaReadPerm = "#users")
app2$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#editMetaIncapOwner"), NA)
Sys.sleep(2)
# empty permissions not allowed
app2$set_inputs(editMetaReadPerm = c())
app2$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#editMetaEmptyPerm"), NA)
Sys.sleep(2)
app2$set_inputs(editMetaReadPerm = c(user1, "#users"), editMetaWritePerm = "#users")
app2$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#editMetaIncapOwner"), NA)
Sys.sleep(2)
app2$set_inputs(editMetaWritePerm = c(user1, "#users"), editMetaExecPerm = "#users")
app2$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#editMetaIncapOwner"), NA)
Sys.sleep(2)
app2$set_inputs(editMetaExecPerm = c(user1, "#users"))
app2$set_inputs(btUpdateMeta = "click")
# allowed to save
app2$set_inputs(btSave = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#btSaveReadonly"))
Sys.sleep(0.5)
app2$click(selector = "#btRemove1")
Sys.sleep(0.5)
app2$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)


# user2 loads scenario with permissions rx
app2$click(selector = "#btRemove1")
Sys.sleep(0.5)
app2$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app2$set_inputs(btImport = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app2$set_inputs(selLoadScen = paste0("3_", user1))
app2$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# expect no permissions section in edit metadata
app2$set_inputs(btEditMeta = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
expect_error(app2$click(selector = "a[data-value='accessPerm']"))
app2$set_inputs(btUpdateMeta = "click")
# is able to solve
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.1)
app2$set_inputs(btSolve = "click")
expect_error(app2$wait_for_js("$('#outputTableView').is(':visible')", timeout = 15000L), NA)
Sys.sleep(0.5)
app2$set_inputs(btSave = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#btSaveReadonly"), NA)
Sys.sleep(0.5)
# discard permissions for new user2 scenario
# discard permissions true per default for not owned scenarios
expect_true(app2$get_js("$('#newScenDiscardPerm').get(0).checked"))
app2$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
Sys.sleep(1)
# btSaveConfirm should not be visible since scenario is newly created for user2
expect_error(app2$click(selector = "#btSaveConfirm']"))
# expect only user2 for rwx permissions
app2$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
expect_error(app2$click(selector = "a[data-value='accessPerm']"), NA)
Sys.sleep(1)
expect_identical(length(app2$get_values()$input[["editMetaReadPerm"]]), 1L)
expect_identical(length(app2$get_values()$input[["editMetaWritePerm"]]), 1L)
expect_identical(length(app2$get_values()$input[["editMetaExecPerm"]]), 1L)
app2$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app2$click(selector = "a[data-value='inputData']")
Sys.sleep(0.1)
app2$click(selector = "#btRemove1")
Sys.sleep(0.5)
app2$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)


# user2 loads scenario with permissions r
app2$set_inputs(btImport = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app2$set_inputs(selLoadScen = paste0("4_", user1))
app2$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# not able to solve
app2$set_inputs(btSolve = "click")
Sys.sleep(2)
expect_error(app2$click(selector = "#shiny-modal button"), NA)
Sys.sleep(0.5)
# not able to delete
app2$set_inputs(btDelete = "click")
app2$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app2$click(selector = "#btDeleteConfirm")
Sys.sleep(0.5)
expect_error(app2$click(selector = "#shiny-modal .btn-default"), NA)
Sys.sleep(0.5)
# user1 can work in this scenario since user2 has no write permissions
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(0.1)
app$set_inputs(selLoadScen = paste0("4_", user1))
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
expect_error(app$click(selector = "#inputDataTitle .fa-lock"))
app$stop()
app2$click(selector = "#btRemove1")
Sys.sleep(0.5)
app2$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)

################# END general permission tests #################

app2$stop()
