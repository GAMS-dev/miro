app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("integer_headers_test")

app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)

app$setInputs(inputTabset = "inputTabset_5")
iLocData <- getHotData(app, "in_5")
expect_identical(iLocData[[1]], c("Seattle", "San-Diego"))
expect_equivalent(getHotData(app, "in_5"), tibble(i = c("Seattle", "San-Diego"), `1` = c(47.608013, 32.715736), `2` = c(-122.335167, -117.161087)),
  tolerance = 1e-7
)
app$setInputs(btSolve = "click")
expect_true(app$waitFor("$('#outputTableView').is(':visible')", timeout = 4000))
expect_chartjs(
  app,
  "tab_1_1-miroPivot-pivotChart",
  list(-718.4888, 240.9712, -514.5075, 243.3373, 2850, 1800, 900),
  c("value")
)
app$stop()
