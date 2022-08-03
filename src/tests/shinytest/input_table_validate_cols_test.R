jsonPath <- file.path("..", "model", "transport", "conf_transport")
configOld <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "bk_transport.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))
configNew <- configOld
configNew$inputWidgets <- NULL
configNew$dataRendering <- NULL
configNew$overwriteSheetOrder <- NULL
configNew$inputGroups <- NULL
configNew$inputWidgets <- list(d = list(
  widgetType = "table",
  tableType = "default",
  readonly = FALSE,
  hideIndexCol = FALSE,
  heatmap = FALSE,
  validateCols = list(
    `new-york` = list(
      min = 1,
      max = 10,
      exclude = 9,
      allowInvalid = TRUE
    ),
    topeka = list(
      choices = c(11, 12, 14),
      allowInvalid = FALSE
    )
  )
))

configWorking <- configNew

jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("input_table_validate_cols_test")
Sys.sleep(1)
app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,1,'6');true;", timeout = 50L)
expect_false(app$waitFor("$('#in_1 table tr:nth-child(1) td:nth-child(3)').hasClass('htInvalid')", timeout = 50L))
app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,1,'12');true;", timeout = 50L)
expect_true(app$waitFor("$('#in_1 table tr:nth-child(1) td:nth-child(3)').hasClass('htInvalid')", timeout = 50L))
expect_true(app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.getDataAtCell(0,1)===12", timeout = 50L))

app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,3,'10');true;", timeout = 50L)
expect_true(app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.getDataAtCell(0,3)===null", timeout = 50L))
app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,3,'11');true;", timeout = 50L)
expect_true(app$waitFor("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.getDataAtCell(0,3)===11", timeout = 50L))

app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
app$stop()

configNew <- configWorking
configNew$inputWidgets$d$validateCols$topeka <- list(
  min = 11,
  max = 10,
  exclude = 9,
  allowInvalid = TRUE
)
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(0.5)
expect_true(app$waitFor("$('#errorMessages').html().includes('greater than the specified maximum value');", timeout = 50))
Sys.sleep(0.5)
app$stop()

configNew <- configWorking
configNew$inputWidgets$d$validateCols$topeka <- list(
  choices = c(11, 12, 14),
  exclude = 11
)
jsonlite::write_json(configNew, file.path(jsonPath, "transport.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
Sys.sleep(0.5)
expect_true(app$waitFor("$('#errorMessages').html().includes('declared as choices and should be excluded');", timeout = 50))
Sys.sleep(0.5)
app$stop()
