app <- ShinyDriver$new("../", loadTimeout = 1000000)
app$snapshotInit("config_mode_table_test")
Sys.sleep(2)
app$snapshot(screenshot = TRUE)



jsonPath <- file.path(getwd(), "model", "pickstock_configuration", "conf_pickstock_configuration")
configRaw <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration_expected.json"), 
                                                 simplifyDataFrame = FALSE, 
                                                 simplifyMatrix = FALSE))
configNew <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration.json"),
                                                 simplifyDataFrame = FALSE, 
                                                 simplifyMatrix = FALSE))


#general input table settings
expect_identical(configRaw$handsontable$stretchH, configNew$handsontable$stretchH)
expect_identical(configRaw$handsontable$readonly, configNew$handsontable$readonly)
expect_identical(configRaw$handsontable$highlightCol, configNew$handsontable$highlightCol)
expect_identical(configRaw$handsontable$highlightRow, configNew$handsontable$highlightRow)
expect_identical(configRaw$handsontable$columnSorting, configNew$handsontable$columnSorting)
expect_identical(configRaw$handsontable$manualColumnResize, configNew$handsontable$manualColumnResize)
expect_identical(configRaw$handsontable$contextMenu$enabled, configNew$handsontable$contextMenu$enabled)
expect_identical(configRaw$handsontable$contextMenu$allowRowEdit, configNew$handsontable$contextMenu$allowRowEdit)
expect_identical(configRaw$handsontable$contextMenu$allowColEdit, configNew$handsontable$contextMenu$allowColEdit)
expect_identical(configRaw$handsontable$contextMenu$allowReadOnly, configNew$handsontable$contextMenu$allowReadOnly)
expect_identical(configRaw$handsontable$height, configNew$handsontable$height)
expect_identical(configRaw$handsontable$colWidths, configNew$handsontable$colWidths)

#general output table settings
expect_identical(configRaw$datatable$class, configNew$datatable$class)
expect_identical(configRaw$datatable$filter, configNew$datatable$filter)
expect_identical(configRaw$datatable$rownames, configNew$datatable$rownames)
expect_identical(configRaw$datatable$options$pageLength, configNew$datatable$options$pageLength)
expect_identical(configRaw$datatable$options$buttons, configNew$datatable$options$buttons)
expect_identical(configRaw$datatable$options$dom, configNew$datatable$options$dom)
expect_identical(configRaw$datatable$extensions, configNew$datatable$extensions)


app$stop()