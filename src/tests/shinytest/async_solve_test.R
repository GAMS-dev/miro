app <- AppDriver$new("../../", name = "async_solve_test", variant = NULL, load_timeout = 20000)

authHeader <- paste0(
  "Basic ",
  processx::base64_encode(charToRaw(
    paste0(
      Sys.getenv("ENGINE_USER_INVITEE"),
      ":", Sys.getenv("ENGINE_PASSWORD")
    )
  ))
)
getLatestJobToken <- function() {
  httr::content(
    httr::GET(
      paste0(Sys.getenv("ENGINE_URL"), "/jobs/"),
      httr::add_headers(Authorization = authHeader),
      httr::timeout(2L)
    ),
    type = "application/json",
    encoding = "utf-8"
  )$results[[1]]$token
}
getJobStatus <- function(token) {
  httr::content(
    httr::GET(
      paste0(Sys.getenv("ENGINE_URL"), "/jobs/", token),
      httr::add_headers(Authorization = authHeader),
      httr::timeout(2L)
    ),
    type = "application/json",
    encoding = "utf-8"
  )$status
}

context("UI tests - asynchronous solve - solve without login")
# expect login screen to appear. Don't log in first.
expect_error(app$wait_for_js("$('#shiny-modal .bt-gms-confirm').is(':visible');", timeout = 5000L), NA)
app$run_js("$('#shiny-modal .btn-default').get(0).click()")
Sys.sleep(1)
# load data
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
expect_identical(startsWith(app$get_values()$input[["selLoadScen"]], "1_"), TRUE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# try to solve and to submit (not logged in)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
Sys.sleep(1)
expect_error(app$run_js("$('#shiny-modal .btn-default').get(0).click()"), NA)
Sys.sleep(1)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitJob']")
Sys.sleep(0.5)
expect_error(app$run_js("$('#shiny-modal .btn-default').get(0).click()"), NA)

# login with wrong credentials (test each field)
context("UI tests - asynchronous solve - login with wrong credentials")
app$click(selector = "#btRemoteExecLogin")
Sys.sleep(1)
app$set_inputs(remoteCredUrl = paste0(Sys.getenv("ENGINE_URL"), "s"))
app$set_inputs(remoteCredUser = Sys.getenv("ENGINE_USER_INVITEE"))
app$set_inputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$set_inputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$set_inputs(remoteCredReg = FALSE)
app$set_inputs(remoteCredRemember = TRUE)
Sys.sleep(0.5)
app$click(selector = "#shiny-modal .bt-gms-confirm")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#remoteLoginHostNotFound').is(':visible')", timeout = 5000), NA)
Sys.sleep(2)
app$set_inputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$set_inputs(remoteCredUser = paste0(Sys.getenv("ENGINE_USER_INVITEE"), "s"))
app$click(selector = "#shiny-modal .bt-gms-confirm")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#remoteLoginInvalidCred').is(':visible')", timeout = 5000), NA)
Sys.sleep(2)
app$set_inputs(remoteCredUser = Sys.getenv("ENGINE_USER_INVITEE"))
app$set_inputs(remoteCredPass = paste0(Sys.getenv("ENGINE_PASSWORD"), "s"))
app$click(selector = "#shiny-modal .bt-gms-confirm")
expect_error(app$wait_for_js("$('#remoteLoginInvalidCred').is(':visible')", timeout = 5000), NA)
Sys.sleep(2)
app$set_inputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$set_inputs(remoteCredNs = paste0(Sys.getenv("ENGINE_NS"), "s"))
app$click(selector = "#shiny-modal .bt-gms-confirm")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#remoteLoginNsNotFound').is(':visible')", timeout = 5000), NA)
Sys.sleep(2)
# check that model is registered (it's not) and try to solve
app$set_inputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$set_inputs(remoteCredReg = TRUE)
app$click(selector = "#shiny-modal .bt-gms-confirm")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#remoteLoginModelNotFound').is(':visible')", timeout = 50000), NA)
Sys.sleep(2)
# expect_identical(app$get_js("if ($('#modelStatus')[0].textContent.match('^Run did not terminate successfully: Host could not be reached').length) true"), TRUE)

# correctly login and remember credentials
context("UI tests - asynchronous solve - login and solve both synchronous and asynchronous")
app$set_inputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$set_inputs(remoteCredUser = Sys.getenv("ENGINE_USER_INVITEE"))
app$set_inputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$set_inputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$set_inputs(remoteCredReg = FALSE)
app$set_inputs(remoteCredRemember = TRUE)
Sys.sleep(1)
app$click(selector = "#shiny-modal .bt-gms-confirm")
Sys.sleep(1)
expect_error(app$wait_for_js("$('#shiny-modal .btn-default').length===0", timeout = 5000), NA)

# solve 3 jobs synchronous, interrupt first one, stop app before last one finishes
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
timeout <- 20
repeat{
  isRunning <- app$get_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().startsWith('Model execution phase');")
  if (isRunning) {
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (1)")
  }
}
app$click(selector = "#btInterrupt")
timeout <- 600L
repeat{
  isTerminated <- app$get_js("$('#modelStatus').text().startsWith('Run did not')")
  if (isTerminated) {
    break
  }
  if (timeout < 560L) {
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (2)")
  }
}
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = "#logFileTabsset a[data-value='mirolog']")
expect_identical(app$get_js("$('#logStatusContainer')[0].textContent.length == 0"), TRUE)
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
timeout <- 600L
repeat{
  outputTableVisible <- app$get_js("$('#outputTableView').is(':visible')")
  if (outputTableVisible) {
    break
  }
  if (timeout < 560L) {
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (3)")
  }
}
expect_error(app$click(selector = "#outputTableView"), NA)
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
timeout <- 30
repeat{
  if (identical(app$get_js("$('#modelStatus').text().startsWith('Model queued')||$('#modelStatus').text()==='Model execution phase'"), TRUE)) {
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if (timeout <= 0) {
    stop("Engine seems to be busy. Try again later.. (4)")
  }
}
app$stop()
token <- getLatestJobToken()
timeout <- 600L
repeat{
  if (getJobStatus(token) == 10L) {
    break
  }
  if (timeout < 560L) {
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (5)")
  }
}
app <- AppDriver$new("../../", variant = NULL, load_timeout = 20000)
expect_error(app$wait_for_js("$(\"#shiny-modal button[onclick*='showJobsDialog']\").is(':visible')", timeout = 20000), NA)
expect_error(app$click(selector = "#shiny-modal button[onclick*='showJobsDialog']"), NA)
Sys.sleep(2)
expect_error(app$run_js("$('#jImport_output button[onclick*=\\'discardJob\\']').get(0).click()"), NA)
Sys.sleep(2)
app$click(selector = "#confirmModal .bt-gms-confirm")
Sys.sleep(2)

# modify metadata (add attachments, views, tags)
app$set_inputs(btEditMeta = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
addSelectizeOption(app, "#editMetaTags", "tag1")
selectSelectizeOption(app, "#editMetaTags", "tag1")
addSelectizeOption(app, "#editMetaTags", "tag2")
selectSelectizeOption(app, "#editMetaTags", "tag2")
app$click(selector = "a[data-value='attachments']")
app$upload_file(file_addAttachments = "../model/pickstock_with_data/README.md")
Sys.sleep(0.5)
app$click(selector = '#editMetaUI a[data-value="views"]')
app$upload_file(file_addViews = "../data/good-views.json")
Sys.sleep(0.5)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(0.5)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)

# solve asynchronous 3 jobs, interrupt last one
app$click(selector = "a[data-value='inputData']")
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitJob']")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
Sys.sleep(0.5)
app$set_inputs(jobSubmissionName = "test1")
app$click(selector = "#btSubmitAsyncJob")
Sys.sleep(0.5)
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L), NA)
app$click(selector = "#btSolve")
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L), NA)
Sys.sleep(0.5)
app$set_inputs(jobSubmissionName = "test2")
app$click(selector = "#btSubmitAsyncJob")
Sys.sleep(0.5)
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L), NA)
app$click(selector = "#btSolve")
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L), NA)
Sys.sleep(0.5)
app$set_inputs(jobSubmissionName = "test3")
app$click(selector = "#btSubmitAsyncJob")
Sys.sleep(0.5)
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L), NA)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitJob']")
Sys.sleep(1)
token1 <- getLatestJobToken()
app$set_inputs(jobSubmissionName = "test4")
app$click(selector = "#btSubmitAsyncJob")
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L), NA)
token2 <- getLatestJobToken()

# joblist section
context("UI tests - asynchronous solve - joblist section")
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
app$click(selector = "#refreshActiveJobs")
expect_error(app$wait_for_js(paste0("$('#jImport_output td')[0]?.textContent==='", Sys.info()[["user"]], "'"), timeout = 5000L), NA)
expect_true(app$get_js("$('#jImport_output td')[2].textContent==='test4'"))
expect_true(app$get_js("$('#jImport_output td')[7].textContent==='test3'"))
expect_true(app$get_js("$('#jImport_output td')[4].childElementCount===1"))

# discard test4
expect_error(app$run_js("$('#jImport_output button[onclick*=\\'discardJob\\']').get(0).click()"), NA)
Sys.sleep(1)
app$click(selector = "#confirmModal .bt-gms-confirm")
expect_error(app$wait_for_js("$('#jImport_output td')[2]?.textContent==='test3'", timeout = 5000L), NA)

# restart app
app$stop()
timeout <- 600L
repeat{
  if (getJobStatus(token1) %in% c(10L, -3L) && getJobStatus(token2) %in% c(10L, -3L)) {
    break
  }
  if (timeout < 550L) {
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (6)")
  }
}
app <- AppDriver$new("../../", variant = NULL, load_timeout = 20000)
expect_error(app$wait_for_js("$(\"#shiny-modal button[onclick*='showJobsDialog']\").is(':visible')", timeout = 20000), NA)
expect_error(app$click(selector = "#shiny-modal button[onclick*='showJobsDialog']"), NA)
Sys.sleep(1)

# expect to be logged in. log out and test whether user can import/delete scenario or see log
expect_error(app$click(selector = "#remoteExecLogoutDiv"), NA)
Sys.sleep(1)
app$click(selector = "#confirmModal .bt-gms-confirm")
Sys.sleep(1)
expect_error(app$run_js("$('#jImport_output button[onclick*=\\'downloadJobData\\']').get(0).click()"), NA)
Sys.sleep(2L)
expect_true(app$get_js("$('#remoteCredUrl').is(':visible');"))
app$click(selector = "#shiny-modal button[data-dismiss='modal']")
Sys.sleep(1)

# try to download a second time due to an bug that occurred here before
app$run_js("$('#jImport_output button[onclick*=\\'downloadJobData\\']').get(0).click()")
Sys.sleep(2L)
expect_true(app$get_js("$('#remoteCredUrl').is(':visible');"))
app$click(selector = "#shiny-modal button[data-dismiss='modal']")
Sys.sleep(1)
app$run_js("$('#jImport_output button[onclick*=\\'showJobLog\\']').get(0).click()")
Sys.sleep(2L)
expect_true(app$get_js("$('#remoteCredUrl').is(':visible');"))
app$click(selector = "#shiny-modal button[data-dismiss='modal']")
Sys.sleep(1)
app$run_js("$('#jImport_output button[onclick*=\\'discardJob\\']').get(0).click()")
Sys.sleep(0.5)
app$click(selector = "#confirmModal .bt-gms-confirm")
Sys.sleep(2L)
expect_true(app$get_js("$('#remoteCredUrl').is(':visible');"))
app$click(selector = "#shiny-modal button[data-dismiss='modal']")
Sys.sleep(1)

# login and inspect get results
app$click(selector = "#btRemoteExecLogin")
Sys.sleep(1)
app$set_inputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$set_inputs(remoteCredUser = Sys.getenv("ENGINE_USER_INVITEE"))
app$set_inputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$set_inputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$set_inputs(remoteCredReg = FALSE)
app$set_inputs(remoteCredRemember = TRUE)
Sys.sleep(1)
app$click(selector = "#shiny-modal .bt-gms-confirm")
Sys.sleep(1)

# download job test3 and test2 directly one after the other. test3 will be imported to sandbox test2 can be imported afterwards
app$run_js("$('#jImport_output button[onclick*=\\'downloadJobData\\']').get(0).click()")
app$wait_for_js("$('#outputTableView').is(':visible');", timeout = 10000)
Sys.sleep(1)
expect_error(app$click(selector = "#outputTableView"), NA)
Sys.sleep(1)
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
Sys.sleep(0.5)
app$click(selector = "#refreshActiveJobs")
Sys.sleep(1)
app$wait_for_js("$('#jImport_output button[onclick*=\\'downloadJobData\\']').is(':visible')", timeout = 10000)
app$run_js("$('#jImport_output button[onclick*=\\'downloadJobData\\']').get(0).click()")
Sys.sleep(1)
app$wait_for_js("$('#shiny-modal button[data-dismiss=\\'modal\\']').is(':visible')", timeout = 10000)
app$click(selector = "#shiny-modal button[data-dismiss='modal']")
Sys.sleep(1)
app$run_js("$('#jImport_output button[onclick*=\\'importJob\\']').get(0).click()")
Sys.sleep(1)
expect_error(app$click(selector = "#shiny-modal .bt-gms-confirm"), NA)
Sys.sleep(2)

# show test1 log
expect_error(app$click(selector = "#outputTableView"), NA)
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
app$click(selector = "#refreshActiveJobs")
Sys.sleep(1)
app$wait_for_js("$(\"#jImport_output button[onclick*='showJobLog']\").is(':visible')", timeout = 10000)
app$run_js("$('#jImport_output button[onclick*=\\'showJobLog\\']').get(0).click()")
Sys.sleep(1)
# no error message in log
expect_true(app$get_js("/^An unexpected error occurred/.test($('#asyncMiroLogContainer')[0].textContent)===false"))
app$run_js("$('#asyncLogFileTabsset a[data-value^=\"mirolog_\"').click()")
app$wait_for_js("$('#asyncMiroLogContainer')[0].textContent.startsWith('ABC')===true", timeout = 2000)
app$run_js("$('#shiny-modal .btn-default').get(0).click()")
Sys.sleep(1)

# discard test1 job
expect_true(app$get_js("$('#jImport_output tr').length===2"))
app$run_js("$('#jImport_output button[onclick*=\\'discardJob\\']').get(0).click()")
Sys.sleep(1)
app$click(selector = "#confirmModal .bt-gms-confirm")
Sys.sleep(1.5)
expect_true(app$get_js("$('#jImport_output td').length===0"))

# check job history
app$click(selector = "#btShowHistory")
Sys.sleep(1)
expect_true(app$get_js("$('#shiny-modal tr').length===8"))
app$run_js("$('#shiny-modal .btn-default').get(0).click()")
Sys.sleep(1)

# check that metadata was successfully restored
app$set_inputs(btEditMeta = "click")
Sys.sleep(0.5)
expect_identical(app$get_values()$input[["editMetaName"]], "test2")
expect_identical(app$get_values()$input[["editMetaTags"]], c("tag1", "tag2"))
app$click(selector = "a[data-value='attachments']")
Sys.sleep(0.5)
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 2L)
idx <- which(app$get_js("$('.attachment-line a').map(function(){return $.trim($(this).text());}).get();") == "README.md")
expect_true(app$get_js(paste0("$('.attachment-line > div:nth-child(2) input[type=checkbox]').get(", idx - 1L, ").checked")))
Sys.sleep(0.5)
app$click(selector = '#editMetaUI a[data-value="views"]')
Sys.sleep(0.5)
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 1L)

# solve job when quotas are low/exceeded
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
expect_identical(startsWith(app$get_values()$input[["selLoadScen"]], "1_"), TRUE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
Sys.sleep(1)
app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().startsWith('Model execution phase');", timeout = 10000)
app$set_inputs(btInterrupt = "click")
Sys.sleep(1)
app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().includes('There was a compilation error');", timeout = 10000)
expect_true(app$get_js("$('.shiny-notification-content').is(':visible');"))
expect_true(app$get_js("$('.shiny-notification-content').text().includes('Quota warning');"))

expect_identical(httr::status_code(httr::PUT(
  paste0(Sys.getenv("ENGINE_URL"), "/usage/quota"),
  body = list(
    username = Sys.getenv("ENGINE_USER_INVITEE"),
    volume_quota = 0
  ),
  httr::authenticate(Sys.getenv("ENGINE_USER"), Sys.getenv("ENGINE_PASSWORD")),
  httr::timeout(2L)
)), 200L)

app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text().includes('Quota exceeded');", timeout = 10000)
expect_true(app$get_js("$('.shiny-notification-content').is(':visible');"))
expect_true(app$get_js("$('.shiny-notification-content').text().includes('Quota exceeded');"))
expect_true(app$get_js("/volume quota: -?\\d+ s/.test($('.shiny-notification-content').text())"))
app$run_js("$('.shiny-notification-close').click()")

app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitJob']")
Sys.sleep(1)
app$set_inputs(jobSubmissionName = "test1")
app$click(selector = "#btSubmitAsyncJob")
app$wait_for_js("$('#jobSubmitUnknownError').text().includes('quota was exceeded')", timeout = 10000)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_true(app$get_js("$('.shiny-notification-content').text().includes('Quota exceeded');"))
expect_true(app$get_js("/volume quota: -?\\d+ s/.test($('.shiny-notification-content').text())"))
app$run_js("$('.shiny-notification-close').click()")
app$stop()
