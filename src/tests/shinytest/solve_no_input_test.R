app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("solve_no_input_test")
app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)
app$setInputs(btSolve = "click")
expect_true(app$waitFor('$("#outputTableView").is(":visible");', timeout = 10000))
app$setInputs(btSave = "click")
expect_true(app$waitFor('$("#scenName").is(":visible");', timeout = 3000))
app$findElement("#shiny-modal .bt-gms-confirm")$click()
expect_true(app$waitFor('$("#shiny-modal").is(":hidden");', timeout = 3000))
Sys.sleep(2)
expect_false(any(grepl("error", app$getDebugLog("shiny_console")$message, ignore.case = TRUE)))
app$stop()
