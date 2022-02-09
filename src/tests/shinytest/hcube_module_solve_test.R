app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("hcube_module_solve_test")

context("UI tests - Hypercube module - solve/discard/import")

# load base scenario
Sys.sleep(3)
app$setInputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER_INVITEE"))
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = FALSE)
app$setInputs(remoteCredRemember = TRUE)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
expect_true(app$waitFor("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L))

app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)

app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(1)
app$setInputs(btImport = "click")
Sys.sleep(1)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(3)
# make sure that we set daterange to proper values so that hashes are reproducible

app$setInputs(inputTabset = "inputTabset_1")
app$setInputs(inputTabset1 = "inputTabset1_6")
app$setInputs(daterange_9 = c(NA, "2021-08-09"))
app$setInputs(btSave = "click")
Sys.sleep(1L)
app$setInputs(btRemoveOutput = "click")
Sys.sleep(2L)

# open Hcube dialog and check that defaults are correct
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit Hypercube']")$click()
Sys.sleep(3L)
expect_true(app$waitFor("$('#hcWidget_1_step').is(':visible');", 50))
expect_true(app$waitFor("$('#hcWidget_3').parents('.shiny-input-container').find('.irs-handle.from i').is(':visible')", 50))
expect_true(app$waitFor("$('#hcWidget_3_combinations').is(':visible');", timeout = 50))
expect_true(app$waitFor("$('#hcWidget_4').parents('.shiny-input-container').find('.selectize-control.multi').is(':visible');", timeout = 50))
expect_identical(app$getValue("hcWidget_1"), c(2, 2))
expect_identical(app$findElement("#hcWidget_2")$getValue(), "CPLEX")
expect_identical(app$findElement("#hcWidget_4")$getValue(), "1")
expect_identical(app$getValue("hcWidget_3"), c(7, 22))
expect_true(grepl("0 scenarios have already been solved", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':visible') === false", timeout = 50L))
app$waitFor('$(\'button[data-dismiss="modal"]:visible\').click();true;', timeout = 50)
Sys.sleep(2L)

# create normal scenario first
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
timeout <- 600L
repeat{
  if (app$waitFor("$('#outputDataTitle').is(':visible');", timeout = 50L)) {
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Engine server seems busy (1). Try again later.", call. = FALSE)
  }
}
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
Sys.sleep(0.5)
app$setInputs(btSave = "click")
Sys.sleep(2L)

# solve again. We should not get same hash exists dialog since we have output attachment!
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
timeout <- 600L
repeat{
  if (app$waitFor("$('#outputDataTitle').is(':visible');", timeout = 50L)) {
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Engine server seems busy (2). Try again later.", call. = FALSE)
  }
}
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
Sys.sleep(0.5)
app$setInputs(btSave = "click")
Sys.sleep(2L)

# if we try solving again now, we should get hash exists dialog
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
expect_true(app$waitFor("$('.modal-body:visible').text().includes('default')===true;", timeout = 6000L))
app$waitFor('$(\'button[data-dismiss="modal"]:visible\').click();true;', timeout = 50)
expect_true(app$waitFor("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L))

# Submit HC, check that defaults are correct and that hash exists
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit Hypercube']")$click()
Sys.sleep(3L)
expect_true(grepl("1 scenarios have already been solved", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':visible') === true", timeout = 50L))
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===false", timeout = 50L))

app$setInputs(hcWidget_1 = c(2, 7))
app$setInputs(hcWidget_1_step = c(2))
app$setInputs(hcWidget_4 = c("0", "1"))
Sys.sleep(2L)
expect_true(grepl("selected 6 scenarios", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(grepl("1 scenarios have already been solved", app$getValue("newHcJobInfo"), fixed = TRUE))
app$setInputs(hcWidget_3_combinations = TRUE)
Sys.sleep(1L)
expect_true(app$waitFor("$('#hcWidget_3_step').is(':visible') === true", timeout = 50L))
expect_true(grepl("selected 126 scenarios", app$getValue("newHcJobInfo"), fixed = TRUE))
app$setInputs(hcWidget_3_step = 9)
Sys.sleep(1)
expect_true(grepl("selected 18 scenarios", app$getValue("newHcJobInfo"), fixed = TRUE))
app$setInputs(hcWidget_3_combinations = FALSE)
Sys.sleep(1)
expect_true(grepl("selected 6 scenarios", app$getValue("newHcJobInfo"), fixed = TRUE))
Sys.sleep(2L)
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===true", timeout = 50L))
addSelectizeOption(app, "#newHcubeTags", "bla")
addSelectizeOption(app, "#newHcubeTags", "blub")
selectSelectizeOption(app, "#newHcubeTags", "bla")
selectSelectizeOption(app, "#newHcubeTags", "blub")
app$findElement("#btSubmitHcJobConfirmUnsolved")$click()
timeout <- 20L
repeat{
  if (app$waitFor("$('#shiny-modal').is(':visible')", timeout = 50L)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not submit HC job.", call. = FALSE)
  }
}
app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement("#refreshActiveJobs")$click()
Sys.sleep(3.5)

expect_true(app$waitFor("$('#jImport_output td').get(2).innerHTML.trim().includes('badge-info\">HC')", timeout = 5000))
expect_true(app$waitFor("$('#jImport_output td').get(2).innerHTML.trim().startsWith('bla,blub')", timeout = 50))
expect_error(app$findElements("#jImport_output button[onclick*='showJobProgress']")[[1]]$click(), NA)
Sys.sleep(1)
expect_true(app$waitFor("$('#shiny-modal .progress-bar.progress-bar-striped.active').is(':visible');", 50))
timeout <- 600L
repeat{
  if (app$waitFor("$('#shiny-modal .progress-bar.progress-bar-striped.active').is(':visible')===false;", 50)) {
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Engine server seems busy (3). Try again later.", call. = FALSE)
  }
}
Sys.sleep(1)
expect_error(app$findElements("#jImport_output button[onclick*='downloadJobData']")[[1]]$click(), NA)
timeout <- 30L
repeat{
  if (app$waitFor("$('#jImport_output td').length === 0", 50)) {
    break
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not import HC job.", call. = FALSE)
  }
}
app$setInputs(btShowHistory = "click")
Sys.sleep(2L)
expect_true(app$waitFor("$('.cJob-wrapper td').get(2).innerHTML.trim().includes('badge-info\">HC')", timeout = 50))
expect_true(app$waitFor("$('.cJob-wrapper td').get(2).innerHTML.trim().startsWith('bla,blub')", timeout = 50))
expect_true(app$waitFor("$('.cJob-wrapper td').get(3).textContent.trim().includes('Imported')", timeout = 50))
app$waitFor('$(\'button[data-dismiss="modal"]:visible\').click();true;', timeout = 50)
Sys.sleep(2L)

# Submit same HC job again and discard it
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
Sys.sleep(0.5)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit Hypercube']")$click()
Sys.sleep(3L)
expect_true(grepl("1 scenarios have already been solved", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':visible') === true", timeout = 50L))
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===false", timeout = 50L))

app$setInputs(hcWidget_1 = c(2, 7))
app$setInputs(hcWidget_1_step = c(2))
app$setInputs(hcWidget_4 = c("0", "1"))
Sys.sleep(2L)
expect_true(grepl("selected 6 scenarios", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(grepl("6 scenarios have already been solved", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===false", timeout = 50L))
addSelectizeOption(app, "#newHcubeTags", "<>")
addSelectizeOption(app, "#newHcubeTags", "&&")
selectSelectizeOption(app, "#newHcubeTags", "<>")
selectSelectizeOption(app, "#newHcubeTags", "&&")
app$findElement("#btSubmitHcJobConfirm")$click()
timeout <- 20L
repeat{
  if (app$waitFor("$('#shiny-modal').is(':visible')", timeout = 50L)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not submit HC job.", call. = FALSE)
  }
}
app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement("#refreshActiveJobs")$click()
Sys.sleep(3L)
expect_true(app$waitFor("$('#jImport_output td').get(2).innerHTML.trim().startsWith('&lt;&gt;,&amp;&amp;')===true&&$('#jImport_output td').get(2).innerHTML.trim().includes('badge-info\">HC')===true;", timeout = 5000))
expect_error(app$findElements("#jImport_output button[onclick*='discardJob']")[[1]]$click(), NA)
Sys.sleep(2)
app$findElement("#confirmModal .bt-gms-confirm")$click()
Sys.sleep(2)
app$setInputs(btShowHistory = "click")
expect_true(app$waitFor("$('.cJob-wrapper td').get(2).innerHTML.trim().includes('badge-info\">HC')===true&&$('.cJob-wrapper td').get(2).innerHTML.trim().startsWith('&lt;&gt;,&amp;&amp;')===true;", timeout = 50000))
expect_true(app$waitFor("$('.cJob-wrapper td').get(3).textContent.trim().startsWith('Discarded') && $('.cJob-wrapper td').get(3).textContent.trim().endsWith('The job was still active.')", timeout = 50))
app$waitFor('$(\'button[data-dismiss="modal"]:visible\').click();true;', timeout = 50)
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
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
Sys.sleep(0.5)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit Hypercube']")$click()
Sys.sleep(3L)
app$setInputs(hcWidget_1 = c(4, 8))
app$setInputs(hcWidget_1_step = c(4))
app$setInputs(hcWidget_4 = c("0"))
Sys.sleep(2L)
expect_true(grepl("selected 2 scenarios", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(grepl("1 scenarios have already been solved", app$getValue("newHcJobInfo"), fixed = TRUE))
expect_true(app$waitFor("$('#btSubmitHcJobConfirmUnsolved').is(':enabled')===true", timeout = 50L))
addSelectizeOption(app, "#newHcubeTags", "woff")
selectSelectizeOption(app, "#newHcubeTags", "woff")
app$findElement("#btSubmitHcJobConfirm")$click()
timeout <- 20L
repeat{
  if (app$waitFor("$('#shiny-modal').is(':visible')", timeout = 50L)) {
    break
  }
  Sys.sleep(1L)
  timeout <- timeout - 1L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Could not submit HC job.", call. = FALSE)
  }
}
app$findElement("#sidebarItemExpanded a[data-value='gamsinter']")$click()
app$findElement('#shiny-tab-gamsinter a[data-value="joblist"]')$click()
app$findElement("#refreshActiveJobs")$click()
Sys.sleep(3)
timeout <- 200L
repeat{
  app$findElement("#refreshActiveJobs")$click()
  if (app$waitFor("$(\"#jImport_output button[onclick*='downloadJobData']\").length>0", timeout = 50L)) {
    break
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    app$snapshot()
    stop("Timeout reached. Engine server seems busy (4). Try again later.", call. = FALSE)
  }
}
expect_error(app$findElements("#jImport_output button[onclick*='downloadJobData']")[[1]]$click(), NA)
timeout <- 30L
repeat{
  if (app$waitFor("$('#jImport_output td').length === 0", 50)) {
    break
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if (timeout <= 0L) {
    stop("Timeout reached. Could not import HC job.", call. = FALSE)
  }
}

app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
Sys.sleep(0.5)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit Hypercube']")$click()
expect_true(app$waitFor("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L))
expect_true(app$waitFor("$('#btSubmitHcJobConfirm').is(':enabled')", timeout = 5000L))
Sys.sleep(0.5)
app$findElement("#btSubmitHcJobConfirm")$click()
expect_true(app$waitFor("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L))
Sys.sleep(0.5)
expect_true(app$waitFor("$('.shiny-notification-content').is(':visible');", timeout = 50L))
expect_true(app$waitFor("$('.shiny-notification-content').text().includes('Quota warning');", timeout = 50L))

expect_identical(httr::status_code(httr::PUT(
  paste0(Sys.getenv("ENGINE_URL"), "/usage/quota"),
  body = list(
    username = Sys.getenv("ENGINE_USER_INVITEE"),
    volume_quota = 0
  ),
  httr::authenticate(Sys.getenv("ENGINE_USER"), Sys.getenv("ENGINE_PASSWORD")),
  httr::timeout(2L)
)), 200L)
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit Hypercube']")$click()
expect_true(app$waitFor("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L))
expect_true(app$waitFor("$('#btSubmitHcJobConfirm').is(':enabled')", timeout = 5000L))
Sys.sleep(0.5)
app$findElement("#btSubmitHcJobConfirm")$click()
expect_true(app$waitFor("$('#newHcJobError').text().includes('quota was exceeded')", timeout = 5000L))
expect_true(app$waitFor("$('.shiny-notification-content').text().includes('Quota exceeded');", timeout = 50L))
expect_true(app$waitFor("/volume quota: -?\\\\d+ s/.test($('.shiny-notification-content').text())", timeout = 50L))

app$stop()
