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

app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("input_table_col_format_test")
Sys.sleep(3)
app$snapshot(
  items = list(output = "errorMessages"),
  screenshot = TRUE
)
Sys.sleep(0.5)
app$stop()

# colFormat and pivotCols should not be set together
configNew <- configOld
configNew$inputWidgets$d$colFormat <- list(value = list(format = "0"))
configNew$inputWidgets$d$pivotCols <- "i"
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app2 <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(3)
expect_identical(app2$getAllValues()$output[["errorMessages"]], " colFormat is not supported when pivotCols are active (table: d).")
app2$stop()

configNew <- configOld
configNew$inputWidgets$d$colFormat <- list(value = list(format = "0"))
configNew$inputWidgets$d$pivotCols <- NULL
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app3 <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(3)
app3$setInputs(btImport = "click")
app3$setInputs(tb_importData = "tb_importData_local")
app3$uploadFile(localInput = "../data/transport.gdx")
app3$setInputs(btImportLocal = "click")
app3$setInputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app3$getAllValues()$output[["in_3"]])$x$columns
expect_identical(colDef$numericFormat$pattern, c(NA_character_, NA_character_, "0"))

app3$stop()

configNew <- configOld
configNew$inputWidgets$d$colFormat <- list(value = list(format = "0", language = "ja-JP"))
configNew$inputWidgets$d$pivotCols <- NULL
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app4 <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(3)
app4$setInputs(btImport = "click")
app4$setInputs(tb_importData = "tb_importData_local")
app4$uploadFile(localInput = "../data/transport.gdx")
app4$setInputs(btImportLocal = "click")
app4$setInputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app4$getAllValues()$output[["in_3"]])$x$columns
expect_identical(colDef$numericFormat$pattern, c(NA_character_, NA_character_, "0"))
expect_identical(colDef$numericFormat$culture, c(NA_character_, NA_character_, "ja-JP"))

app4$stop()
