#launch user1
Sys.setenv(MIRO_USERNAME="testuser1")
Sys.setenv(MIRO_USERGROUPS="users")
app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("permissions_test")

app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)
user1 <- Sys.getenv("MIRO_USERNAME")
#launch user2
Sys.setenv(MIRO_USERNAME="testuser2")
app2 <- ShinyDriver$new("../../", loadTimeout = 20000)

user2 <- Sys.getenv("MIRO_USERNAME")
Sys.unsetenv("MIRO_USERNAME")

#scenarios visible for user2
#1_ default user1
#2_ default user2
#3_ rx permissions from user1
#4_ r permissions from user1
#5_ rwx permissions from user1

#user1 saves scenario with user2 permissions rx 
app$setInputs(btSaveAs = "click")
Sys.sleep(0.5)
app$setInputs(scenName = "user1Scenario1")
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)

#user1 saves scenario with user2 permissions r 
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='Access permissions']")$click()
Sys.sleep(0.5)
app$setInputs(editMetaExecPerm = user1)
Sys.sleep(0.5)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$setInputs(btSaveAs = "click")
Sys.sleep(0.5)
app$setInputs(scenName = "user1Scenario3")
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()

#user1 saves scenario where user2 has no permissions 
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='Access permissions']")$click()
app$setInputs(editMetaReadPerm = user1)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$setInputs(btSaveAs = "click")
Sys.sleep(0.5)
app$setInputs(scenName = "noUser2")
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(0.5)

#user1 saves scenario with user2 permissions rwx 
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='Access permissions']")$click()
app$setInputs(editMetaReadPerm = c(getSelectizeOptions(app, "#editMetaReadPerm"), "users"))
app$setInputs(editMetaWritePerm = c(getSelectizeOptions(app, "#editMetaWritePerm"), "users"))
app$setInputs(editMetaExecPerm = c(getSelectizeOptions(app, "#editMetaExecPerm"), "users"))
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$setInputs(btSaveAs = "click")
Sys.sleep(0.5)
app$setInputs(scenName = "user1Scenario2")
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)
#user1 stays in rwx scenario for locked scenario test


#user2 loads locked scenario with permissions rwx
app2$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app2$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)
app2$setInputs(btImport = "click")
Sys.sleep(0.5)
#scen noUser2 should not be visible since user2 has no permissions there
expect_length(getSelectizeOptions(app2, "#selLoadScen"), 5L)
app2$setInputs(selLoadScen = paste0("6_", user1))
app2$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
#expect notification and lock symbol to be visible
expect_error(app2$findElement("#shiny-notification-panel .shiny-notification-close")$click(), NA)
expect_identical(app2$findElement("#inputDataTitle .badge")$getAttribute("textContent"), user1)
expect_error(app2$findElement("#inputDataTitle .fa-lock")$click(), NA)
#not possible to delete locked scenario
app2$setInputs(btDelete = "click")
Sys.sleep(0.5)
app2$findElement("#btDeleteConfirm")$click()
Sys.sleep(0.5)
expect_error(app2$findElement("#shiny-modal .btn-default")$click(), NA)
Sys.sleep(0.5)
#user2 is allowed to change name, tags, attachments in sandbox, no permissions tab visible
app2$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app2$setInputs(editMetaName = "newNameForReadonly")
app2$setInputs(editMetaTags = c("tag1", "tag2"))
app2$findElement("a[data-value='Attachments']")$click()
attachmentList <- app2$findElements(".attachment-line")
expect_identical(length(attachmentList), 0L)
app2$uploadFile(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(1)
attachmentList <- app2$findElements(".attachment-line")
expect_identical(length(attachmentList), 1L)
expect_error(app2$findElement("a[data-value='Access permissions']")$click())
app2$setInputs(btUpdateMeta = "click")
Sys.sleep(1)
app2$setInputs(btSave = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("#btSaveReadonly")$click(), NA)
Sys.sleep(0.5)
#discard permissions true per default for not owned scenarios
expect_identical(app2$findElement("#newScenDiscardPerm")$getAttribute("checked"), "true")
app2$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)


#user1 closes rwx scenario, and overwrites default scenario permissions, should be reverted after new start
app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(selLoadScen = paste0("1_", user1))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='Access permissions']")$click()
app$setInputs(editMetaReadPerm = user1, editMetaExecPerm = user1)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$setInputs(btSave = "click")
Sys.sleep(1)
app$stop()
Sys.setenv(MIRO_USERNAME="testuser1")
app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(2)
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
app$findElement("a[data-value='Access permissions']")$click()
expect_identical(length(app$getValue("editMetaReadPerm")), 2L)
expect_identical(length(app$getValue("editMetaWritePerm")), 1L)
expect_identical(length(app$getValue("editMetaExecPerm")), 2L)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)


################# Start general permission tests ################# 

#user2 loads scenario with permissions rwx
app2$setInputs(btImport = "click")
Sys.sleep(0.5)
app2$setInputs(selLoadScen = paste0("5_", user1))
app2$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
#expect permissions section visible in edit metadata
app2$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("a[data-value='Access permissions']")$click(), NA)
expect_length(getSelectizeOptions(app2, "#editMetaWritePerm"), 2L)
#expect user2 not be allowed to remove user1 (owner) from permissions  
app2$setInputs(editMetaReadPerm = "users")
app2$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("#editMetaIncapOwner")$click(), NA)
Sys.sleep(2)
#empty permissions not allowed
app2$setInputs(editMetaReadPerm = c())
app2$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("#editMetaEmptyPerm")$click(), NA)
Sys.sleep(2)
app2$setInputs(editMetaReadPerm = c(user1, "users"), editMetaWritePerm = "users")
app2$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("#editMetaIncapOwner")$click(), NA)
Sys.sleep(2)
app2$setInputs(editMetaWritePerm = c(user1, "users"), editMetaExecPerm = "users")
app2$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("#editMetaIncapOwner")$click(), NA)
Sys.sleep(2)
app2$setInputs(editMetaExecPerm = c(user1, "users"))
app2$setInputs(btUpdateMeta = "click")
#allowed to save
app2$setInputs(btSave = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("#btSaveReadonly")$click())
Sys.sleep(0.5)
app2$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app2$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)


#user2 loads scenario with permissions rx
app2$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app2$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)
app2$setInputs(btImport = "click")
Sys.sleep(0.5)
app2$setInputs(selLoadScen = paste0("3_", user1))
app2$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
#expect no permissions section in edit metadata
app2$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("a[data-value='Access permissions']")$click())
app2$setInputs(btUpdateMeta = "click")
#is able to solve
Sys.sleep(0.5)
app2$setInputs(btSolve = "click")
Sys.sleep(10)
expect_error(app2$findElement("#outputTableView")$click(), NA)
Sys.sleep(0.5)
app2$setInputs(btSave = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("#btSaveReadonly")$click(), NA)
Sys.sleep(0.5)
#discard permissions for new user2 scenario
#discard permissions true per default for not owned scenarios
expect_identical(app2$findElement("#newScenDiscardPerm")$getAttribute("checked"), "true")
app2$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)
#btSaveConfirm should not be visible since scenario is newly created for user2
expect_error(app2$findElement("#btSaveConfirm']")$click())
#expect only user2 for rwx permissions
app2$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
expect_error(app2$findElement("a[data-value='Access permissions']")$click(), NA)
expect_identical(length(app2$getValue("editMetaReadPerm")), 1L)
expect_identical(length(app2$getValue("editMetaWritePerm")), 1L)
expect_identical(length(app2$getValue("editMetaExecPerm")), 1L)
app2$setInputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app2$findElement("a[data-value='inputData']")$click()
Sys.sleep(0.1)
app2$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app2$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)


#user2 loads scenario with permissions r
app2$setInputs(btImport = "click")
Sys.sleep(0.5)
app2$setInputs(selLoadScen = paste0("4_", user1))
app2$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
#not able to solve
app2$setInputs(btSolve = "click")
Sys.sleep(2)
expect_error(app2$findElement("#shiny-modal button")$click(), NA)
Sys.sleep(0.5)
#not able to delete
app2$setInputs(btDelete = "click")
Sys.sleep(0.5)
app2$findElement("#btDeleteConfirm")$click()
Sys.sleep(0.5)
expect_error(app2$findElement("#shiny-modal .btn-default")$click(), NA)
Sys.sleep(0.5)
#user1 can work in this scenario since user2 has no write permissions
app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(selLoadScen = paste0("4_", user1))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
expect_error(app$findElement("#inputDataTitle .fa-lock")$click())
app$stop()
app2$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app2$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)

################# END general permission tests ################# 

app2$stop()