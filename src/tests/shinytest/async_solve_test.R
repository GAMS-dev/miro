app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("async_solve_test")

authHeader <- paste0("Basic ", 
                     processx::base64_encode(charToRaw(
                       paste0(Sys.getenv("ENGINE_USER"), 
                              ":", Sys.getenv("ENGINE_PASSWORD")))))
getLatestJobToken <- function(){
  httr::content(httr::GET(paste0(Sys.getenv("ENGINE_URL"), "/jobs/"),
                          httr::add_headers(Authorization = authHeader),
                          httr::timeout(2L)),
                type = "application/json", 
                encoding = "utf-8")$results[[1]]$token
}
getJobStatus <- function(token){
  httr::content(httr::GET(paste0(Sys.getenv("ENGINE_URL"), "/jobs/", token),
                          httr::add_headers(Authorization = authHeader),
                          httr::timeout(2L)),
                type = "application/json", 
                encoding = "utf-8")$status
}

context("UI tests - asynchronous solve - solve without login")
#expect login screen to appear. Don't log in first.
Sys.sleep(1)
app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)
expect_true(app$waitFor("$('#shiny-modal .bt-gms-confirm').is(':visible');", timeout = 50))
app$findElement("#shiny-modal .btn-default")$click()
Sys.sleep(1)
#load data
app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
expect_identical(startsWith(app$getValue("selLoadScen"), "1_"), TRUE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
#try to solve and to submit (not logged in)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
Sys.sleep(0.5)
expect_error(app$findElement("#shiny-modal .btn-default")$click(), NA)
Sys.sleep(1)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit job']")$click()
Sys.sleep(0.5)
expect_error(app$findElement("#shiny-modal .btn-default")$click(), NA)

#login with wrong credentials (test each field)
context("UI tests - asynchronous solve - login with wrong credentials")
app$findElement("#btRemoteExecLogin")$click()
Sys.sleep(1)
app$setInputs(remoteCredUrl = paste0(Sys.getenv("ENGINE_URL"), "s"))
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER"))
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = FALSE)
app$setInputs(remoteCredRemember = TRUE)
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(2)
expect_true(app$waitFor("$('#remoteLoginHostNotFound').is(':visible');", timeout = 50))
Sys.sleep(1)
app$setInputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$setInputs(remoteCredUser = paste0(Sys.getenv("ENGINE_USER"), "s"))
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(2)
expect_true(app$waitFor("$('#remoteLoginInvalidCred').is(':visible');", timeout = 50))
Sys.sleep(1)
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER"))
app$setInputs(remoteCredPass = paste0(Sys.getenv("ENGINE_PASSWORD"), "s"))
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(2)
expect_true(app$waitFor("$('#remoteLoginInvalidCred').is(':visible');", timeout = 50))
Sys.sleep(1)
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = paste0(Sys.getenv("ENGINE_NS"), "s"))
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(2)
expect_true(app$waitFor("$('#remoteLoginNsNotFound').is(':visible');", timeout = 50))
Sys.sleep(1)
#check that model is registered (it's not) and try to solve
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = TRUE)
Sys.sleep(1)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)
expect_true(app$waitFor("$('#remoteLoginModelNotFound').is(':visible');", timeout = 50))
Sys.sleep(1)
# expect_identical(app$waitFor("if ($('#modelStatus')[0].textContent.match('^Run did not terminate successfully: Host could not be reached').length) true", timeout = 50), TRUE)

#correctly login and remember credentials
context("UI tests - asynchronous solve - login and solve both synchronous and asynchronous")
app$setInputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER"))
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = FALSE)
app$setInputs(remoteCredRemember = TRUE)
Sys.sleep(1)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)
expect_false(app$waitFor("$('#shiny-modal .btn-default').is(':visible');", timeout = 50))

#solve 3 jobs synchronous, interrupt first one, stop app before last one finishes
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
timeout <- 10
repeat{
  isRunning <- app$waitFor("$('#modelStatus').is(':visible') && $('#modelStatus').text().startsWith('Model execution phase');", timeout = 50)
  if(isRunning){
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if(timeout <= 0L){
    stop("Engine seems to be busy. Try again later..")
  }
}
app$findElement("#btInterrupt")$click()
timeout <- 600L
repeat{
  isTerminated <- app$waitFor("$('#modelStatus').text().startsWith('Run did not')", timeout = 50)
  if(isTerminated){
    break
  }
  if(timeout < 560L){
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if(timeout <= 0L){
    stop("Engine seems to be busy. Try again later..")
  }
}
app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement("#logFileTabsset a[data-value='mirolog']")$click()
expect_identical(app$waitFor("$('#logStatusContainer')[0].textContent.length == 0", timeout = 50), TRUE)
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
timeout <- 600L
repeat{
  outputTableVisible <- app$waitFor("$('#outputTableView').is(':visible')", timeout = 50)
  if(outputTableVisible){
    break
  }
  if(timeout < 560L){
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if(timeout <= 0L){
    stop("Engine seems to be busy. Try again later..")
  }
}
expect_error(app$findElement("#outputTableView")$click(), NA)
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
Sys.sleep(3)
app$stop()
token <- getLatestJobToken()
timeout <- 600L
repeat{
  if(getJobStatus(token) == 10L){
    break
  }
  if(timeout < 560L){
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if(timeout <= 0L){
    stop("Engine seems to be busy. Try again later..")
  }
}
app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(5)
expect_error(app$findElement("#shiny-modal button[onclick*='showJobsDialog']")$click(), NA)
Sys.sleep(2)
expect_error(app$findElements("#jImport_output button[onclick*='discardJob']")[[1]]$click(), NA)
Sys.sleep(2)
app$findElement("#confirmModal .bt-gms-confirm")$click()
Sys.sleep(2)

# modify metadata (add attachments, views, tags)
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
addSelectizeOption(app, "#editMetaTags", "tag1")
selectSelectizeOption(app, "#editMetaTags", "tag1")
addSelectizeOption(app, "#editMetaTags", "tag2")
selectSelectizeOption(app, "#editMetaTags", "tag2")
app$findElement("a[data-value='Attachments']")$click()
app$uploadFile(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(0.5)
app$findElement('#editMetaUI a[data-value="Views"]')$click()
app$uploadFile(file_addViews = "../data/good-views.json")
Sys.sleep(0.5)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(1)

#solve asynchronous 3 jobs, interrupt last one
app$findElement("a[data-value='inputData']")$click()
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit job']")$click()
Sys.sleep(1)
app$setInputs(jobSubmissionName = "test1")
app$findElement("#btSubmitAsyncJob")$click()
Sys.sleep(5)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit job']")$click()
Sys.sleep(1)
app$setInputs(jobSubmissionName = "test2")
app$findElement("#btSubmitAsyncJob")$click()
Sys.sleep(5)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit job']")$click()
Sys.sleep(1)
app$setInputs(jobSubmissionName = "test3")
app$findElement("#btSubmitAsyncJob")$click()
Sys.sleep(5)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit job']")$click()
Sys.sleep(1)
token1 <- getLatestJobToken()
app$setInputs(jobSubmissionName = "test4")
app$findElement("#btSubmitAsyncJob")$click()
Sys.sleep(5)
token2 <- getLatestJobToken()

#joblist section
context("UI tests - asynchronous solve - joblist section")
app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement('#refreshActiveJobs')$click()
Sys.sleep(3)
expect_true(app$waitFor(paste0("$('#jImport_output td')[0].textContent==='", Sys.info()[["user"]], "'"), timeout = 50)) 
expect_true(app$waitFor("$('#jImport_output td')[2].textContent==='test4'", timeout = 50)) 
expect_true(app$waitFor("$('#jImport_output td')[7].textContent==='test3'", timeout = 50)) 
expect_true(app$waitFor("$('#jImport_output td')[4].childElementCount===1", timeout = 50)) 

#discard test4
expect_error(app$findElements("#jImport_output button[onclick*='discardJob']")[[1]]$click(), NA)
Sys.sleep(1)
app$findElement("#confirmModal .bt-gms-confirm")$click()
Sys.sleep(3)
expect_true(app$waitFor("$('#jImport_output td')[2].textContent==='test3'", timeout = 50)) 

#restart app
app$stop()
timeout <- 600L
repeat{
  if(getJobStatus(token1) %in% c(10L, -3L) && getJobStatus(token2) %in% c(10L, -3L)){
    break
  }
  if(timeout < 550L){
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if(timeout <= 0L){
    stop("Engine seems to be busy. Try again later..")
  }
}
app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(3)
expect_error(app$findElement("#shiny-modal button[onclick*='showJobsDialog']")$click(), NA)
Sys.sleep(1)

#expect to be logged in. log out and test whether user can import/delete scenario or see log
expect_error(app$findElement("#remoteExecLogoutDiv")$click(), NA)
Sys.sleep(1)
app$findElement("#confirmModal .bt-gms-confirm")$click()
Sys.sleep(1)
app$findElements("#jImport_output button[onclick*='downloadJobData']")[[1]]$click()
Sys.sleep(2L)
expect_true(app$waitFor("$('#remoteCredUrl').is(':visible');", timeout = 50))
app$findElement("#shiny-modal button[data-dismiss='modal']")$click()
Sys.sleep(1)

#try to download a second time due to an bug that occurred here before
app$findElements("#jImport_output td button[onclick*='downloadJobData']")[[1]]$click()
Sys.sleep(2L)
expect_true(app$waitFor("$('#remoteCredUrl').is(':visible');", timeout = 50))
app$findElement("#shiny-modal button[data-dismiss='modal']")$click()
Sys.sleep(1)
app$findElements("#jImport_output button[onclick*='showJobLog']")[[1]]$click()
Sys.sleep(2L)
expect_true(app$waitFor("$('#remoteCredUrl').is(':visible');", timeout = 50))
app$findElement("#shiny-modal button[data-dismiss='modal']")$click()
Sys.sleep(1)
app$findElements("#jImport_output button[onclick*='discardJob']")[[1]]$click()
Sys.sleep(0.5)
app$findElement("#confirmModal .bt-gms-confirm")$click()
Sys.sleep(2L)
expect_true(app$waitFor("$('#remoteCredUrl').is(':visible');", timeout = 50))
app$findElement("#shiny-modal button[data-dismiss='modal']")$click()
Sys.sleep(1)

#login and inspect get results
app$findElement("#btRemoteExecLogin")$click()
Sys.sleep(1)
app$setInputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER"))
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = FALSE)
app$setInputs(remoteCredRemember = TRUE)
Sys.sleep(1)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)

#download job test3 and test2 directly one after the other. test3 will be imported to sandbox test2 can be imported afterwards
app$snapshot()
app$findElements("#jImport_output button[onclick*='downloadJobData']")[[1]]$click()
Sys.sleep(8)
expect_error(app$findElement("#outputTableView")$click(), NA)
app$snapshot()
app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement('#refreshActiveJobs')$click()
Sys.sleep(3)
app$snapshot()
app$findElements("#jImport_output button[onclick*='downloadJobData']")[[1]]$click()
Sys.sleep(3)
app$findElement("#shiny-modal button[data-dismiss='modal']")$click()
Sys.sleep(1)
app$findElements("#jImport_output button[onclick*='importJob']")[[1]]$click()
Sys.sleep(1)
expect_error(app$findElement("#shiny-modal .bt-gms-confirm")$click(), NA)
Sys.sleep(2)

#show test1 log 
app$snapshot()
expect_error(app$findElement("#outputTableView")$click(), NA)
app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement('#refreshActiveJobs')$click()
Sys.sleep(3)
app$findElements("#jImport_output button[onclick*='showJobLog']")[[1]]$click()
Sys.sleep(1)
#no error message in log
expect_true(app$waitFor("/^An unexpected error occurred/.test($('#asyncMiroLogContainer')[0].textContent)===false", timeout = 50))
app$findElement("#shiny-modal .btn-default")$click()
Sys.sleep(0.5)

#discard test1 job
app$findElements("#jImport_output button[onclick*='discardJob']")[[1]]$click()
Sys.sleep(0.5)
app$findElement("#confirmModal .bt-gms-confirm")$click()
Sys.sleep(1)
expect_true(app$waitFor("$('#jImport_output td').length===0"))

#check job history
app$findElement("#btShowHistory")$click()
Sys.sleep(1)
expect_true(app$waitFor("$('#shiny-modal tr').length===8", timeout = 50))
app$findElement("#shiny-modal .btn-default")$click()
Sys.sleep(1)

# check that metadata was successfully restored
app$setInputs(btEditMeta = "click")
Sys.sleep(0.5)
expect_identical(app$getValue("editMetaName"), "test2")
expect_identical(app$getValue("editMetaTags"), list("tag1", "tag2"))
app$findElement("a[data-value='Attachments']")$click()
Sys.sleep(0.5)
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 2L)
expect_identical(attachmentList[[2]]$findElement(".checkbox input")$getAttribute("checked"), "true")
Sys.sleep(0.5)
app$findElement('#editMetaUI a[data-value="Views"]')$click()
Sys.sleep(0.5)
expect_identical(length(app$findElements('#currentViewsTable tbody tr')), 1L)

app$stop()