app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("output_table_settings")

Sys.sleep(2)
app$findElement("a[data-value='outputData']")$click()
Sys.sleep(1)
app$setInputs(outputTableView = "click")
Sys.sleep(1)
app$snapshot(items = list(output = "outputDataTitle"),
             screenshot = TRUE)
expect_true(grepl("<th>Scalar Description</th>", jsonlite::fromJSON(app$getAllValues()$output[["table_tab_1_1-datatable"]])$x$container,
                  fixed = TRUE))
expect_identical(jsonlite::fromJSON(app$getAllValues()$output[["table_tab_1_1-datatable"]])$x$filter, "top")
expect_identical(jsonlite::fromJSON(app$getAllValues()$output[["table_tab_1_1-datatable"]])$x$options$decimals, 4L)
expect_identical(jsonlite::fromJSON(app$getAllValues()$output[["table_tab_1_1-datatable"]])$x$options$pageLength, 5L)
app$stop()
