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

app <- AppDriver$new("../../",
  name = "input_table_dropdowncols_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(0.5)
app$expect_values(output = "errorMessages")
Sys.sleep(0.5)
app$stop()

configNew$inputWidgets$d$dropdownCols <- list(bla = list(static = letters))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(0.5)
expect_true(app$get_js("$('#errorMessages').html().includes('bla');", timeout = 50))
Sys.sleep(0.5)
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(static = letters))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = "../data/transport.gdx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$get_values()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("autocomplete", "numeric", "numeric"))
expect_identical(colDef$source, list(letters, NULL, NULL))
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(static = letters, colType = "dropdown"))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(1)
app$set_inputs(btImport = "click")
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = "../data/transport.gdx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$get_values()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("dropdown", "numeric", "numeric"))
expect_identical(colDef$source, list(letters, NULL, NULL))
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(symbol = "a", column = "i"))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(1)
app$set_inputs(btImport = "click")
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = "../data/transport.gdx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$get_values()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("autocomplete", "numeric", "numeric"))
expect_identical(colDef$source, list(c("Seattle", "San-Diego"), NULL, NULL))
app$stop()

configNew <- configOld
configNew$inputWidgets$d$dropdownCols <- list(j = list(symbol = "a", column = "i", colType = "dropdown"))
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- AppDriver$new("../../",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(1)
app$set_inputs(btImport = "click")
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = "../data/transport.gdx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = "inputTabset_3")
colDef <- jsonlite::fromJSON(app$get_values()$output[["in_3"]])$x$columns
expect_identical(colDef$type, c("dropdown", "numeric", "numeric"))
expect_identical(colDef$source, list(c("Seattle", "San-Diego"), NULL, NULL))
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$click(selector = ".modal-footer #dialogSaveInit .bt-gms-confirm")
Sys.sleep(2)
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
Sys.sleep(1)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)
expect_identical(
  as.character(app$get_values()$output$inputDataTitle$html),
  "New Scenario"
)
app$stop()
