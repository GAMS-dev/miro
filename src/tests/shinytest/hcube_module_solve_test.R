app <- AppDriver$new("../../", name = "hcube_module_solve_test", variant = NULL, load_timeout = 20000)

context("UI tests - Hypercube module - solve/discard/import")

# load base scenario
Sys.sleep(3)
app$set_inputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$set_inputs(remoteCredUser = Sys.getenv("ENGINE_USER_INVITEE"))
app$set_inputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$set_inputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$set_inputs(remoteCredReg = FALSE)
app$set_inputs(remoteCredRemember = TRUE)
app$click(selector = "#shiny-modal .bt-gms-confirm")
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L), NA)

app$expect_values(output = "inputDataTitle")

app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(1)
app$set_inputs(btImport = "click")
Sys.sleep(1)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(3)
# make sure that we set daterange to proper values so that hashes are reproducible

app$set_inputs(inputTabset = "inputTabset_1")
app$set_inputs(inputTabset1 = "inputTabset1_6")
app$set_inputs(daterange_9 = c(NA, "2021-08-09"))
app$set_inputs(btSave = "click")
Sys.sleep(1L)
app$set_inputs(btRemoveOutput = "click")
Sys.sleep(2L)

# open Hcube dialog and check that defaults are correct
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
Sys.sleep(3L)
expect_true(app$get_js("$('#hcWidget_1_step').is(':visible');", timeout = 50))
expect_true(app$get_js("$('#hcWidget_3').parents('.shiny-input-container').find('.irs-handle.from i').is(':visible')", timeout = 50))
expect_true(app$get_js("$('#hcWidget_3_combinations').is(':visible');", timeout = 50))
expect_true(app$get_js("$('#hcWidget_4').parents('.shiny-input-container').find('.selectize-control.multi').is(':visible');", timeout = 50))
expect_identical(app$get_values()$input[["hcWidget_1"]], c(2L, 2L))
expect_identical(app$get_js("$('#hcWidget_2 > option').attr('value')"), "CPLEX")
expect_identical(app$get_js("$('#hcWidget_4 > option').attr('value')"), "1")
expect_identical(app$get_values()$input[["hcWidget_3"]], c(7L, 22L))
expect_true(grepl("0 scenarios have already been solved", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':visible') === false", timeout = 50L))
app$run_js('$(\'button[data-dismiss="modal"]:visible\').click();', timeout = 50)
Sys.sleep(2L)

# create normal scenario first
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
timeout <- 600L
repeat{
  if (app$get_js("$('#outputDataTitle').is(':visible');", timeout = 50L)) {
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Engine server seems busy (1). Try again later.", call. = FALSE)
  }
}
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")
Sys.sleep(2L)

# solve again. We should not get same hash exists dialog since we have output attachment!
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
timeout <- 600L
repeat{
  if (app$get_js("$('#outputDataTitle').is(':visible');", timeout = 50L)) {
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Engine server seems busy (2). Try again later.", call. = FALSE)
  }
}
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
Sys.sleep(0.5)
app$set_inputs(btSave = "click")
Sys.sleep(2L)

# if we try solving again now, we should get hash exists dialog
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSolve']")
Sys.sleep(2L)
expect_true(app$get_js("$('.modal-body:visible').text().includes('default')===true;", timeout = 6000L))
app$run_js('$(\'button[data-dismiss="modal"]:visible\').click();', timeout = 50)
expect_true(app$get_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L))

# Submit HC, check that defaults are correct and that hash exists
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
Sys.sleep(3L)
expect_true(grepl("1 scenarios have already been solved", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':visible') === true", timeout = 50L))
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===false", timeout = 50L))

app$set_inputs(hcWidget_1 = c(2, 7))
app$set_inputs(hcWidget_1_step = c(2))
app$set_inputs(hcWidget_4 = c("0", "1"))
Sys.sleep(2L)
expect_true(grepl("selected 6 scenarios", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(grepl("1 scenarios have already been solved", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
app$set_inputs(hcWidget_3_combinations = TRUE)
Sys.sleep(1L)
expect_true(app$get_js("$('#hcWidget_3_step').is(':visible') === true", timeout = 50L))
expect_true(grepl("selected 126 scenarios", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
app$set_inputs(hcWidget_3_step = 9)
Sys.sleep(1)
expect_true(grepl("selected 18 scenarios", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
app$set_inputs(hcWidget_3_combinations = FALSE)
Sys.sleep(1)
expect_true(grepl("selected 6 scenarios", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
Sys.sleep(2L)
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===true", timeout = 50L))
addSelectizeOption(app, "#newHcubeTags", "bla")
addSelectizeOption(app, "#newHcubeTags", "blub")
selectSelectizeOption(app, "#newHcubeTags", "bla")
selectSelectizeOption(app, "#newHcubeTags", "blub")
app$click(selector = "#btSubmitHcJobConfirmUnsolved")
timeout <- 20L
repeat{
  if (app$get_js("$('#shiny-modal').is(':visible')", timeout = 50L)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Could not submit HC job.", call. = FALSE)
  }
}
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
app$click(selector = "#refreshActiveJobs")

timeout <- 20L
repeat{
  if (identical(app$get_js("$('#jImport_output td').get(2).innerHTML.trim().includes('badge-info\">HC')", timeout = 100L), TRUE)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not get HC job status.", call. = FALSE)
  }
}
expect_true(app$get_js("$('#jImport_output td').get(2).innerHTML.trim().startsWith('bla,blub')", timeout = 50))
app$run_js("$('#jImport_output button[onclick*=\\'showJobProgress\\']').get(0).click()")
Sys.sleep(1)
expect_true(app$get_js("$('#shiny-modal .progress-bar.progress-bar-striped.active').is(':visible')"))
timeout <- 600L
repeat{
  if (app$get_js("$('#shiny-modal .progress-bar.progress-bar-striped.active').is(':visible')===false;", timeout = 50)) {
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Engine server seems busy (3). Try again later.", call. = FALSE)
  }
}
Sys.sleep(1)
expect_error(app$run_js("$('#jImport_output button[onclick*=\\'downloadJobData\\']').get(0).click()"), NA)
timeout <- 30L
repeat{
  if (app$get_js("$('#jImport_output td').length === 0", timeout = 50)) {
    break
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Could not import HC job.", call. = FALSE)
  }
}
app$set_inputs(btShowHistory = "click")
Sys.sleep(2L)
expect_true(app$get_js("$('.cJob-wrapper td').get(2).innerHTML.trim().includes('badge-info\">HC')", timeout = 50))
expect_true(app$get_js("$('.cJob-wrapper td').get(2).innerHTML.trim().startsWith('bla,blub')", timeout = 50))
expect_true(app$get_js("$('.cJob-wrapper td').get(3).textContent.trim().includes('Imported')", timeout = 50))
app$run_js('$(\'button[data-dismiss="modal"]:visible\').click();', timeout = 50)
Sys.sleep(2L)

# Submit same HC job again and discard it
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
Sys.sleep(0.5)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
Sys.sleep(3L)
expect_true(grepl("1 scenarios have already been solved", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':visible') === true", timeout = 50L))
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===false", timeout = 50L))

app$set_inputs(hcWidget_1 = c(2, 7))
app$set_inputs(hcWidget_1_step = c(2))
app$set_inputs(hcWidget_4 = c("0", "1"))
Sys.sleep(2L)
expect_true(grepl("selected 6 scenarios", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(grepl("6 scenarios have already been solved", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===false", timeout = 50L))
addSelectizeOption(app, "#newHcubeTags", "<>")
addSelectizeOption(app, "#newHcubeTags", "&&")
selectSelectizeOption(app, "#newHcubeTags", "<>")
selectSelectizeOption(app, "#newHcubeTags", "&&")
app$click(selector = "#btSubmitHcJobConfirm")
timeout <- 20L
repeat{
  if (app$get_js("$('#shiny-modal').is(':visible')", timeout = 50L)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Could not submit HC job.", call. = FALSE)
  }
}
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
app$click(selector = "#refreshActiveJobs")
timeout <- 20L
repeat{
  if (identical(app$get_js("$('#jImport_output td').get(2).innerHTML.trim().startsWith('&lt;&gt;,&amp;&amp;')===true&&$('#jImport_output td').get(2).innerHTML.trim().includes('badge-info\">HC')===true;", timeout = 100L), TRUE)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not get HC job status.", call. = FALSE)
  }
}
expect_error(app$run_js("$('#jImport_output button[onclick*=\\'discardJob\\']').get(0).click()"), NA)
Sys.sleep(2)
app$click(selector = "#confirmModal .bt-gms-confirm")
Sys.sleep(2)
app$set_inputs(btShowHistory = "click")
expect_true(app$get_js("$('.cJob-wrapper td').get(2).innerHTML.trim().includes('badge-info\">HC')===true&&$('.cJob-wrapper td').get(2).innerHTML.trim().startsWith('&lt;&gt;,&amp;&amp;')===true;", timeout = 50000))
expect_true(app$get_js("$('.cJob-wrapper td').get(3).textContent.trim().startsWith('Discarded') && $('.cJob-wrapper td').get(3).textContent.trim().endsWith('The job was still active.')", timeout = 50))
app$run_js('$(\'button[data-dismiss="modal"]:visible\').click();', timeout = 50)
Sys.sleep(2L)

conn <- connectDb(modelName = "pickstock_configuration")
tryCatch(
  {
    # discarding job should also clean up scen data
    expect_identical(nrow(DBI::dbGetQuery(conn, paste0(
      "SELECT * FROM ",
      DBI::dbQuoteIdentifier(conn, "_sys__hc_scalars"),
      " WHERE ",
      DBI::dbQuoteIdentifier(conn, "_sid"), ">2"
    ))), 0L)
  },
  error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
  },
  finally = {
    DBI::dbDisconnect(conn)
  }
)

# need to submit 1 more HC job with only two scenarios (needed for load test)
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
Sys.sleep(0.5)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
Sys.sleep(2L)
expect_true(app$get_js("$('#hcWidget_1').is(':visible');", timeout = 3000))
Sys.sleep(1L)
app$set_inputs(hcWidget_1 = c(4, 8))
app$set_inputs(hcWidget_1_step = c(4))
app$set_inputs(hcWidget_4 = c("0"))
Sys.sleep(2L)
expect_true(grepl("selected 2 scenarios", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(grepl("1 scenarios have already been solved", app$get_values()$output[["newHcJobInfo"]], fixed = TRUE))
expect_true(app$get_js("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===true", timeout = 50L))
addSelectizeOption(app, "#newHcubeTags", "woff")
selectSelectizeOption(app, "#newHcubeTags", "woff")
app$click(selector = "#btSubmitHcJobConfirm")
timeout <- 20L
repeat{
  if (app$get_js("$('#shiny-modal').is(':visible')", timeout = 50L)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Could not submit HC job.", call. = FALSE)
  }
}
app$click(selector = "#sidebarItemExpanded a[data-value='gamsinter']")
app$click(selector = '#shiny-tab-gamsinter a[data-value="joblist"]')
app$click(selector = "#refreshActiveJobs")
Sys.sleep(4)
timeout <- 200L
repeat{
  app$click(selector = "#refreshActiveJobs")
  if (app$get_js("$(\"#jImport_output button[onclick*='downloadJobData']\").length>0;", timeout = 50L)) {
    break
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    app$expect_values()
    stop("Timeout reached. Engine server seems busy (4). Try again later.", call. = FALSE)
  }
}
expect_error(app$run_js("$('#jImport_output button[onclick*=\\'downloadJobData\\']').get(0).click()"), NA)
timeout <- 30L
repeat{
  if (app$get_js("$('#jImport_output td').length===0;", timeout = 50)) {
    break
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    stop("Timeout reached. Could not import HC job.", call. = FALSE)
  }
}

app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
Sys.sleep(0.5)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
Sys.sleep(0.5)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true;", timeout = 5000L)
app$wait_for_js("$('#btSubmitHcJobConfirm').is(':enabled');", timeout = 5000L)
Sys.sleep(0.5)
app$click(selector = "#btSubmitHcJobConfirm")
Sys.sleep(2)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true;", timeout = 10000L)
app$wait_for_js("$('.shiny-notification-content').is(':visible') && $('.shiny-notification-content').text().includes('Quota warning');", timeout = 5000L)

expect_identical(httr::status_code(httr::PUT(
  paste0(Sys.getenv("ENGINE_URL"), "/usage/quota"),
  body = list(
    username = Sys.getenv("ENGINE_USER_INVITEE"),
    volume_quota = 0
  ),
  httr::authenticate(Sys.getenv("ENGINE_USER"), Sys.getenv("ENGINE_PASSWORD")),
  httr::timeout(2L)
)), 200L)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
Sys.sleep(1)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true;", timeout = 5000L)
app$wait_for_js("$('#btSubmitHcJobConfirm').is(':enabled');", timeout = 5000L)
Sys.sleep(0.5)
app$click(selector = "#btSubmitHcJobConfirm")
Sys.sleep(2)
app$wait_for_js("$('#newHcJobError').text().includes('quota was exceeded');", timeout = 5000L)
expect_true(app$get_js("$('.shiny-notification-content').text().includes('Quota exceeded');", timeout = 50L))
expect_true(app$get_js("/volume quota: -?\\d+ s/.test($('.shiny-notification-content').text());", timeout = 50L))

app$stop()
