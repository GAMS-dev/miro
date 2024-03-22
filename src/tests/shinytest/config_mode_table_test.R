app <- AppDriver$new("../../",
  name = "config_mode_table_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

Sys.sleep(1)

jsonPath <- file.path("..", "model", "pickstock_config_mode", "conf_pickstock_configuration")
configRaw <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration_expected.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))
configNew <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))


# general input table settings
expect_identical(configRaw$handsontable$stretchH, configNew$handsontable$stretchH)
expect_identical(configRaw$handsontable$readonly, configNew$handsontable$readonly)
expect_identical(configRaw$handsontable$contextMenu$enabled, configNew$handsontable$contextMenu$enabled)
expect_identical(configRaw$handsontable$contextMenu$allowRowEdit, configNew$handsontable$contextMenu$allowRowEdit)
expect_identical(configRaw$handsontable$contextMenu$allowColEdit, configNew$handsontable$contextMenu$allowColEdit)
expect_identical(configRaw$handsontable$height, configNew$handsontable$height)
expect_identical(configRaw$handsontable$colWidths, configNew$handsontable$colWidths)

# general output table settings
expect_identical(configRaw$datatable$class, configNew$datatable$class)
expect_identical(configRaw$datatable$filter, configNew$datatable$filter)
expect_identical(configRaw$datatable$rownames, configNew$datatable$rownames)
expect_identical(configRaw$datatable$options$pageLength, configNew$datatable$options$pageLength)
expect_identical(configRaw$datatable$options$buttons, configNew$datatable$options$buttons)
expect_identical(configRaw$datatable$options$dom, configNew$datatable$options$dom)
expect_identical(configRaw$datatable$extensions, configNew$datatable$extensions)

# symbol table settings
app$click(selector = "a[data-value='tables_gen']")
Sys.sleep(1)
app$click(selector = "a[data-value='symbol']")
Sys.sleep(1)
app$set_inputs(table_symbol = "stock_weight")
Sys.sleep(1)
app$click(selector = "button[id='saveTableWidget']")
Sys.sleep(1)

configNew <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))

# output symbol stock_weight
expect_identical(configRaw$outputTables$stock_weight$class, configNew$outputTables$stock_weight$class)
expect_identical(configRaw$outputTables$stock_weight$filter, configNew$outputTables$stock_weight$filter)
expect_identical(configRaw$outputTables$stock_weight$options$pageLength, configNew$outputTables$stock_weight$options$pageLength)
expect_identical(configRaw$outputTables$stock_weight$options$decimals, configNew$outputTables$stock_weight$options$decimals)
expect_identical(configRaw$outputTables$stock_weight$options$buttons, configNew$outputTables$stock_weight$options$buttons)
expect_identical(configRaw$outputTables$stock_weight$options$dom, configNew$outputTables$stock_weight$options$dom)
expect_identical(configRaw$outputTables$stock_weight$rownames, configNew$outputTables$stock_weight$rownames)
expect_identical(configRaw$outputTables$stock_weight$extensions, configNew$outputTables$stock_weight$extensions)

app$stop()
