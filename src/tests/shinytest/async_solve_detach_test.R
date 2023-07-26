app <- AppDriver$new("../../",
  name = "async_solve_detach_test",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

getNumberJobsInJobList <- function(app) {
  numberJobs <- app$get_js("$('#jImport_output tr').length")
  return(max(0L, numberJobs - 1L))
}

Sys.sleep(1)

expect_error(app$click(selector = "#remoteExecLogoutDiv"), NA)
Sys.sleep(1)
app$click(selector = "#confirmModal .bt-gms-confirm")
Sys.sleep(1)

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

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(selLoadScen = paste0("1_", Sys.info()[["user"]]))
expect_identical(startsWith(app$get_values()$input[["selLoadScen"]], "1_"), TRUE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)

app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
app$click(selector = "#refreshActiveJobs")

timeout <- 20L
repeat{
  if (identical(app$get_js("$('.cJob-wrapper').is(':visible') || $('#jImport_output div').is(':visible')"), TRUE)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Could not get job status.", call. = FALSE)
  }
}

numberJobsBefore <- getNumberJobsInJobList(app)

app$click(selector = '#shiny-tab-gamsinter a[data-value="current"]')

expect_true(app$get_js("$('#btDetachCurrentJob').is(':disabled')"))

app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")

app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
timeout <- 40
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
Sys.sleep(2L)
app$wait_for_js("$('#logStatusContainer').text()!=='';", timeout = 5000L)
expect_true(app$get_js("$('#btDetachCurrentJob').is(':enabled')"))
app$click(selector = "#btDetachCurrentJob")
Sys.sleep(0.5)
app$wait_for_js("$('#modelStatus').is(':visible') && $('#modelStatus').text()==='';", timeout = 2000L)
expect_true(app$get_js("$('#logStatusContainer').text()==='';"))
expect_true(app$get_js("$('#btDetachCurrentJob').is(':disabled')"))

app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")

app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
timeout <- 20
repeat{
  isRunning <- app$get_js("$('#modelStatus').is(':visible') && ($('#modelStatus').text().startsWith('Model execution phase') || $('#modelStatus').text().includes('queued'));")
  if (isRunning) {
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (2)")
  }
}

app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")

app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
expect_error(app$wait_for_js("$('#btSolveDetachCurrent').is(':visible')", timeout = 5000L), NA)
app$click(selector = "#btSolveDetachCurrent")
timeout <- 20
repeat{
  isRunning <- app$get_js("$('#modelStatus').is(':visible') && ($('#modelStatus').text().startsWith('Model execution phase') || $('#modelStatus').text().includes('queued'));")
  if (isRunning) {
    break
  }
  Sys.sleep(0.5)
  timeout <- timeout - 0.5
  if (timeout <= 0L) {
    stop("Engine seems to be busy. Try again later.. (3)")
  }
}


app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
app$click(selector = "#refreshActiveJobs")
Sys.sleep(1L)
timeout <- 20L
repeat{
  if (identical(app$get_js("$('.cJob-wrapper').is(':visible')"), TRUE)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Could not get job status.", call. = FALSE)
  }
}
expect_true(app$get_js(paste0("$('#jImport_output tr').length===", numberJobsBefore + 1L + 3L)))
app$stop()
