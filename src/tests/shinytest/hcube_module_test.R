app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("hcube_module_test")

context("UI tests - Hypercube module - solve/discard/import")

#load base scenario
Sys.sleep(3)
app$setInputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER"))
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = FALSE)
app$setInputs(remoteCredRemember = TRUE)
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)
expect_false(app$waitFor("$('#shiny-modal .btn-default').is(':visible');", timeout = 50))

app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)

app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(1)
app$setInputs(btImport = "click")
Sys.sleep(1)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(3)

# create normal scenario first
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
timeout <- 600L
repeat{
  if(app$waitFor("$('#outputDataTitle').is(':visible');", timeout = 50L)){
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if(timeout <= 0L){
    stop("Timeout reached. Engine server seems busy. Try again later.", call. = FALSE)
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
  if(app$waitFor("$('#outputDataTitle').is(':visible');", timeout = 50L)){
    break
  }
  Sys.sleep(4L)
  timeout <- timeout - 4L
  if(timeout <= 0L){
    stop("Timeout reached. Engine server seems busy. Try again later.", call. = FALSE)
  }
}
app$findElement("#sidebarItemExpanded a[data-value='inputData']")$click()
Sys.sleep(0.5)
app$setInputs(btSave = "click")
Sys.sleep(2L)

# if we try solving again now, we should get hash exists dialog
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Solve model']")$click()
Sys.sleep(4)
app$snapshot()
expect_true(app$waitFor("$('.modal-body:visible').text().includes('default')===true;", timeout = 50L))
app$waitFor('$(\'[data-dismiss="modal"]\').click();true;', timeout = 50L)
Sys.sleep(2L)

# Submit HC, check that defaults are correct and that hash exists
app$findElement(".btSolve .dropdown-toggle")$click()
app$findElement(".sidebar-menu a[onclick*='Submit Hypercube']")$click()
Sys.sleep(3L)
expect_true(app$waitFor("$('#hcWidget_1_step').is(':visible');", 50))
expect_true(app$waitFor("$('#hcWidget_3').parents('.shiny-input-container').find('.irs-slider.from i').is(':visible')", 50))
expect_true(app$waitFor("$('#hcWidget_3_combinations').is(':visible');", timeout = 50))
expect_true(app$waitFor("$('#hcWidget_4').parents('.shiny-input-container').find('.selectize-control.multi').is(':visible');", timeout = 50))
expect_identical(app$getValue("hcWidget_1"), c(2,2))
expect_identical(app$findElement("#hcWidget_2")$getValue(), "CPLEX")
expect_identical(app$findElement("#hcWidget_4")$getValue(), "1")
expect_identical(app$getValue("hcWidget_3"), c(7,22))
expect_true(grepl("1 scenarios have already been solve", app$getValue("newHcJobInfo"), fixed = TRUE))

app$stop()
