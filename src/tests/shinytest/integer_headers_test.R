app <- AppDriver$new("../../",
  name = "integer_headers_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(inputTabset = "inputTabset_5")
iLocData <- getHotData(app, "in_5")
expect_identical(iLocData[[1]], c("Seattle", "San-Diego"))
expect_equivalent(getHotData(app, "in_5"), tibble(i = c("Seattle", "San-Diego"), `1` = c(47.608013, 32.715736), `2` = c(-122.335167, -117.161087)),
  tolerance = 1e-7
)
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#outputTableView').is(':visible')", timeout = 4000), NA)
expect_chartjs(
  app,
  "tab_1_1-miroPivot-pivotChart",
  list(-718.4888, 240.9712, -514.5075, 243.3373, 2850, 1800, 900),
  c("value")
)
app$stop()
