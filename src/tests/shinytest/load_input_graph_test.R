app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("load_input_graph_test")

currentUser <- Sys.info()[["user"]]

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(selLoadScen = paste0("1_", currentUser))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$setInputs(btGraphIn = "click")
Sys.sleep(0.5)
expect_true(app$waitFor("$('#graph-in_1').is(':visible');", timeout = 50))
app$setInputs(`in_1-miroPivot-pivotRenderer` = "bar")
Sys.sleep(1)
expect_chartjs(
  app,
  "in_1-miroPivot-pivotChart", c(600, 350),
  c(
    "San-Diego",
    "Seattle"
  )
)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.2)
app$setInputs(btOverwriteScen = "click")
Sys.sleep(1.5)
expect_true(app$waitFor("$('#data-in_1').is(':visible');", timeout = 50))
app$setInputs(btGraphIn = "click")
Sys.sleep(0.5)
app$setInputs(`in_1-miroPivot-pivotRenderer` = "bar")
Sys.sleep(1)
expect_chartjs(
  app,
  "in_1-miroPivot-pivotChart", c(600, 300),
  c(
    "San-Diego",
    "Seattle"
  )
)

app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
app$stop()
