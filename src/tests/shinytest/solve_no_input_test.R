app <- AppDriver$new("../../",
  name = "solve_no_input_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js('$("#outputTableView").is(":visible")', timeout = 10000), NA)
app$set_inputs(btSave = "click")
expect_error(app$wait_for_js('$("#scenName").is(":visible")', timeout = 3000), NA)
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
expect_error(app$wait_for_js('$("#shiny-modal").is(":hidden")', timeout = 3000), NA)
Sys.sleep(2)
log <- app$get_logs()
log <- subset(log, location == "shiny")
expect_false(any(grepl("error", log$message, ignore.case = TRUE)))
app$stop()
