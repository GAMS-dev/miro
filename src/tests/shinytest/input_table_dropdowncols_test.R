jsonPath <- file.path("..", "model", "transport", "conf_transport")
configOld <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "bk_transport.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))
configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(i = list(static = letters))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("input_table_dropdowncols_test")

app$snapshot(
  items = list(output = "errorMessages"),
  screenshot = TRUE
)
app$stop()

configNew$inputWidgets$d$dropdownCols <- list(bla = list(static = letters))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(1)
expect_true(app$waitFor("$('#errorMessages').html().includes('bla');", timeout = 50))
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(static = letters))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(1)
app$setInputs(btImport = "click")
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = "../data/transport.gdx")
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$getAllValues()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("autocomplete", "numeric", "numeric"))
expect_identical(colDef$source, list(letters, NULL, NULL))
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(static = letters, colType = "dropdown"))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(1)
app$setInputs(btImport = "click")
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = "../data/transport.gdx")
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$getAllValues()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("dropdown", "numeric", "numeric"))
expect_identical(colDef$source, list(letters, NULL, NULL))
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(symbol = "a", column = "i"))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(1)
app$setInputs(btImport = "click")
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = "../data/transport.gdx")
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$getAllValues()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("autocomplete", "numeric", "numeric"))
expect_identical(colDef$source, list(c("Seattle", "San-Diego"), NULL, NULL))
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(symbol = "a", column = "i", colType = "dropdown"))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(1)
app$setInputs(btImport = "click")
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = "../data/transport.gdx")
app$setInputs(btImportLocal = "click")
app$setInputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$getAllValues()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("dropdown", "numeric", "numeric"))
expect_identical(colDef$source, list(c("Seattle", "San-Diego"), NULL, NULL))
app$setInputs(btSave = "click")
Sys.sleep(1)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(2)
app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(0.5)
app$setInputs(btImport = "click")
Sys.sleep(1)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
expect_identical(
  as.character(app$getAllValues()$output$inputDataTitle$html),
  "New Scenario"
)
app$stop()
