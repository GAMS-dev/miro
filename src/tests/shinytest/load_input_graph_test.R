app <- AppDriver$new("../../", name = "load_input_graph_test", variant = NULL, load_timeout = 20000)

currentUser <- Sys.info()[["user"]]

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(selLoadScen = paste0("1_", currentUser))
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$set_inputs(btGraphIn = "click")
Sys.sleep(0.5)
expect_true(app$get_js("$('#graph-in_1').is(':visible');", timeout = 50))
app$set_inputs(`in_1-miroPivot-pivotRenderer` = "bar")
Sys.sleep(1)
expect_chartjs(
  app,
  "in_1-miroPivot-pivotChart", c(600, 350),
  c(
    "San-Diego",
    "Seattle"
  )
)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.2)
app$set_inputs(btOverwriteScen = "click")
Sys.sleep(1.5)
expect_true(app$get_js("$('#data-in_1').is(':visible');", timeout = 50))
app$set_inputs(btGraphIn = "click")
Sys.sleep(0.5)
app$set_inputs(`in_1-miroPivot-pivotRenderer` = "bar")
Sys.sleep(1)
expect_chartjs(
  app,
  "in_1-miroPivot-pivotChart", c(600, 300),
  c(
    "San-Diego",
    "Seattle"
  )
)

app$stop()
