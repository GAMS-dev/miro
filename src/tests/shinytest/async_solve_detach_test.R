app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("async_solve_detach_test")

getNumberJobsInJobList <- function(app) {
  app$waitFor("console.log($('#jImport_output tr').length);true", timeout = 50L)
  numberJobs <- app$getDebugLog("browser")$message
  numberJobs <- as.integer(strsplit(numberJobs[length(numberJobs)], " ", fixed = TRUE)[[1]][1])
  return(max(0L, numberJobs - 1L))
}

Sys.sleep(1)

app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)

expect_error(app$findElement("#remoteExecLogoutDiv")$click(), NA)
Sys.sleep(1)
app$findElement("#confirmModal .bt-gms-confirm")$click()
Sys.sleep(1)

app$findElement("#btRemoteExecLogin")$click()
Sys.sleep(1)
app$setInputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER_INVITEE"))
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = FALSE)
app$setInputs(remoteCredRemember = TRUE)
Sys.sleep(1)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
expect_identical(startsWith(app$getValue("selLoadScen"), "1_"), TRUE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)

app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement("#refreshActiveJobs")$click()

timeout <- 20L
repeat{
  if (identical(app$waitFor("$('.cJob-wrapper').is(':visible') || $('#jImport_output div').is(':visible')", timeout = 100L), TRUE)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not get job status.", call. = FALSE)
  }
}

numberJobsBefore <- getNumberJobsInJobList(app)

app$findElement('#shiny-tab-gamsinter a[data-value="current"]')$click()

expect_true(app$waitFor("$('#btDetachCurrentJob').is(':disabled')", timeout = 50L))

app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()

app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".change-dd-button[data-action-id='btSolve']")$click()
timeout <- 40
repeat{
  isRunning <- app$waitFor("$('#modelStatus').is(':visible') && $('#modelStatus').text().startsWith('Model execution phase');", timeout = 50)
  if (isRunning) {
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (1)")
  }
}
Sys.sleep(2L)
expect_true(app$waitFor("$('#logStatusContainer').text()!=='';", timeout = 5000L))
expect_true(app$waitFor("$('#btDetachCurrentJob').is(':enabled')", timeout = 50L))
app$findElement("#btDetachCurrentJob")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#modelStatus').is(':visible') && $('#modelStatus').text()==='';", timeout = 2000L))
expect_true(app$waitFor("$('#logStatusContainer').text()==='';", timeout = 50))
expect_true(app$waitFor("$('#btDetachCurrentJob').is(':disabled')", timeout = 50L))

app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()

app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".change-dd-button[data-action-id='btSolve']")$click()
timeout <- 20
repeat{
  isRunning <- app$waitFor("$('#modelStatus').is(':visible') && ($('#modelStatus').text().startsWith('Model execution phase') || $('#modelStatus').text().includes('queued'));", timeout = 50)
  if (isRunning) {
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (2)")
  }
}

app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()

app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".change-dd-button[data-action-id='btSolve']")$click()
Sys.sleep(1)
expect_true(app$waitFor("$('#btSolveDetachCurrent').is(':visible')", timeout = 50))
app$findElement("#btSolveDetachCurrent")$click()
timeout <- 20
repeat{
  isRunning <- app$waitFor("$('#modelStatus').is(':visible') && ($('#modelStatus').text().startsWith('Model execution phase') || $('#modelStatus').text().includes('queued'));", timeout = 50)
  if (isRunning) {
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (3)")
  }
}


app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement("#refreshActiveJobs")$click()

timeout <- 20L
repeat{
  if (identical(app$waitFor("$('.cJob-wrapper').is(':visible')", timeout = 100L), TRUE)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not get job status.", call. = FALSE)
  }
}
expect_true(app$waitFor(paste0("$('#jImport_output tr').length===", numberJobsBefore + 1L + 3L), timeout = 50L))
