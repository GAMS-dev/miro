app <- AppDriver$new("../../",
  name = "output_table_settings", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)
app$set_inputs(outputTableView = "click")
Sys.sleep(1)
app$expect_values(output = "outputDataTitle")
expect_true(grepl("<th>Scalar Description</th>", jsonlite::fromJSON(app$get_values()$output[["table_tab_1_1-datatable"]])$x$container,
  fixed = TRUE
))
expect_identical(jsonlite::fromJSON(app$get_values()$output[["table_tab_1_1-datatable"]])$x$filter, "top")
expect_identical(jsonlite::fromJSON(app$get_values()$output[["table_tab_1_1-datatable"]])$x$options$decimals, 4L)
expect_identical(jsonlite::fromJSON(app$get_values()$output[["table_tab_1_1-datatable"]])$x$options$pageLength, 5L)
app$set_inputs(outputTabset = "outputTabset_2")
expect_error(app$wait_for_js("$('#tableOutLabel_2').html().trim()==='<h1>asd</h1>';", timeout = 5000L), NA)
app$stop()
