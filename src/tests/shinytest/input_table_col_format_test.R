jsonPath <- file.path("..", "model", "transport", "conf_transport")
configOld <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "bk_transport.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))

# invalid column name should throw error
configNew <- configOld
configNew$inputWidgets$d$colFormat <- list(value2 = list(format = "0"))
configNew$inputWidgets$d$pivotCols <- NULL
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../", name = "input_table_col_format_test", variant = NULL, load_timeout = 20000)
Sys.sleep(5)
app$expect_values(output = "errorMessages")
Sys.sleep(0.5)
app$stop()

# colFormat and pivotCols should not be set together
configNew <- configOld
configNew$inputWidgets$d$colFormat <- list(value = list(format = "0"))
configNew$inputWidgets$d$pivotCols <- "i"
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../", variant = NULL, load_timeout = 20000)
Sys.sleep(5)
expect_identical(app$get_values()$output[["errorMessages"]], " colFormat is not supported when pivotCols are active (table: d).")
app$stop()

configNew <- configOld
configNew$inputWidgets$d$colFormat <- list(value = list(format = "0"))
configNew$inputWidgets$d$pivotCols <- NULL
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../", variant = NULL, load_timeout = 20000)
Sys.sleep(5)
app$set_inputs(btImport = "click")
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = "../data/transport.gdx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$get_values()$output[["in_3"]])$x$columns
expect_identical(colDef$numericFormat$pattern, c(NA_character_, NA_character_, "0"))

app$stop()

configNew <- configOld
configNew$inputWidgets$d$colFormat <- list(value = list(format = "0", language = "ja-JP"))
configNew$inputWidgets$d$pivotCols <- NULL
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../", variant = NULL, load_timeout = 20000)
Sys.sleep(5)
app$set_inputs(btImport = "click")
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = "../data/transport.gdx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$get_values()$output[["in_3"]])$x$columns
expect_identical(colDef$numericFormat$pattern, c(NA_character_, NA_character_, "0"))
expect_identical(colDef$numericFormat$culture, c(NA_character_, NA_character_, "ja-JP"))

app$stop()
