app <- AppDriver$new("../../",
  name = "multiple_symbol_renderer", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2L)
expect_identical(app$get_values()$input[["in_1"]]$data[[1]], list("f", "freight in dollars per case per thousand miles", "NA"))
app$set_inputs(btImport = "click")
Sys.sleep(1)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2L)
app$set_inputs(miroSidebar = "outputData")
Sys.sleep(2)
# check that custom styles were applied
expect_true(app$get_js("$('#tab_1_1-custom-path').css('color')==='rgb(255, 192, 203)';", timeout = 50L))
customRendererPath <- app$get_values()$output[["tab_1_1-custom-path"]]
expect_true(file.exists(file.path(customRendererPath, "model_transport_customRenderer_trnsport_custom.R")))
flowData <- jsonlite::fromJSON(app$get_values()$output[["tab_1_1-custom-trnsport"]])$x$calls$args[[4]][[1]]
expect_identical(flowData$dyn$value, list(0L, 275L, 275L, 300L, 50L, 0L))
expect_equal(flowData$static$lng1, c(-87.62318, -73.93524, -95.69531, -87.62318, -73.93524, -95.69531), tolerance = 1e-3)
app$set_inputs(outputTableView = "click")
app$expect_values(input = "tab_1_1-custom-trnsport_center")
expect_error(app$click(selector = "a[data-value='outputTabset_1_2']"))
app$set_inputs(miroSidebar = "inputData")
app$set_inputs(inputTabset = "inputTabset_2")
app$set_inputs(dropdown_9 = "mip")
app$set_inputs(btSolve = "click")
expect_error(app$wait_for_js("$('#outputTableView').is(':visible')", timeout = 10000L), NA)
app$set_inputs(outputTableView = "click")
Sys.sleep(1L)
expect_identical(
  jsonlite::fromJSON(app$get_values()$output[["tab_1_1-custom-trnsport"]])$x$calls$args[[4]][[1]]$dyn$value,
  list(0L, 325L, 275L, 300L, 0L, 0L)
)
app$stop()
