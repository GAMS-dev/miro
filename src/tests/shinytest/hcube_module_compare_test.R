app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("hcube_module_compare_test")

context("UI tests - Hypercube module - compare mode")
app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)

# use batch loder module to fetch HC scenarios
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(btSendQuery = "click")
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===8", timeout = 50L))
scenData <- getVisibleDtData(app, "batchLoadResults")

# Makes sure hidden output scalars (firstdaytraining/lastdaytraining) were properly stored
expect_false(any(is.na(scenData[[20]])))
expect_false(any(is.na(scenData[[21]])))

if (.Platform$OS.type == "windows") {
  # CRLF in dowjones2016.csv results in different hash
  scenHashTmp <- "e54c36e3fb4340624db0436bae4561a29afb2404cb6fa2fa859a6c22e89ce154"
} else {
  scenHashTmp <- "5b34e4e1bdcf2ef45d5e95dbc3635bf8c3c5f0d193d4de1312f499da9462ed3c"
}
scenToCompare <- match(c("default", scenHashTmp), scenData[[2]])
expect_false(any(is.na(scenToCompare)))
app$setInputs(batchLoadResults_rows_selected = scenToCompare, allowInputNoBinding_ = TRUE)

# check that scalar-name-mapping works
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$setInputs(batchCompareNameCols = c("_gmsopt_lsttitleleftaligned", "maxstock"))
app$waitFor("$('#btBatchCompare').click()", timeout = 50L)
Sys.sleep(3L)
app$setInputs(contentScen_0 = "contentScen_0_2")
Sys.sleep(2L)
app$setInputs(`tab_0_2-miroPivot-pivotRenderer` = "line")
Sys.sleep(2L)
expect_identical(
  jsonlite::fromJSON(app$getAllValues()$output[["tab_0_2-miroPivot-pivotChart"]])$x$data$labels,
  c("0_6.AAPL", "0_6.AXP", "0_6.BA", "0_6.GS", "0_6.HD", "0_6.MMM", "1_2.DD", "1_2.MCD")
)

# load scenarios into tab comparison mode
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(batchLoadResults_rows_selected = scenToCompare, allowInputNoBinding_ = TRUE)

app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$waitFor("$('#btBatchCompare+.dropdown-toggle').click()&&$('#btBatchCompare~.dropdown-menu a:first').click();", timeout = 50L)
Sys.sleep(3L)
expect_true(app$waitFor(paste0(
  "$('#cmpScenTitle_5').text().startsWith('HC (",
  substr(scenHashTmp, 1, 8), "...)')"
), timeout = 50L))
graphData <- jsonlite::fromJSON(app$getAllValues()$output[["tab_5_3-graph"]], simplifyDataFrame = FALSE)$x$data
expect_identical(length(graphData), 756L)
expect_identical(graphData[, 1], c("2016-01-04T00:00:00.000Z", "100.572928794899", "101.703327363261"))
app$setInputs(contentScen_5 = "contentScen_5_11")
expect_true(app$waitFor("$('.small-box:visible')[0].textContent.trim().startsWith('6')", timeout = 50L))
expect_true(app$waitFor("$('.small-box:visible')[1].textContent.trim().startsWith('99')", timeout = 50))

# Download HC and normal scenario while remapping scenario names
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(batchLoadResults_rows_selected = scenToCompare, allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$setInputs(batchCompareNameCols = c("_gmsopt_lsttitleleftaligned", "maxstock"))
multiDimSym <- c(
  "stock_weight", "dowvsindex", "abserror", "pricemerge", "schedule",
  "mapnogroup", "gantt", "repc", "pressurethickness", "hovercraft",
  "price", "maptest"
)
scalarSym <- c(
  "error_train", "error_test", "error_ratio", "maxstock",
  "trainingdays", "firstdaytraining", "lastdaytraining", "solver",
  "clearvalueset"
)
expect_symbols_in_gdx(
  app, "btBatchDownloadGDX",
  setNames(rep.int(list(c(multiDimSym, scalarSym)), 2L), c("1_2", "0_6"))
)

Sys.sleep(0.5)
app$setInputs(batchLoadResults_rows_selected = scenToCompare, allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$setInputs(batchCompareNameCols = c("maxstock", "_gmsopt_lsttitleleftaligned"))
app$waitFor("$('#btBatchDownloadGDX+.dropdown-toggle').click()", timeout = 50L)
Sys.sleep(0.5)
expect_files_in_zip(
  app, "btBatchDownloadCSV",
  unlist(lapply(c("2_1", "6_0"), function(scenName) {
    paste0(scenName, "/", c("", paste0(c(
      multiDimSym, "_metadata_",
      "_scalars", "_scalars_out"
    ), ".csv")))
  }))
)

# Download HC and normal scenario
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(batchLoadResults_rows_selected = scenToCompare, allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
multiDimSym <- c(
  "stock_weight", "dowvsindex", "abserror", "pricemerge", "schedule",
  "mapnogroup", "gantt", "repc", "pressurethickness", "hovercraft",
  "price", "maptest"
)
scalarSym <- c(
  "error_train", "error_test", "error_ratio", "maxstock",
  "trainingdays", "firstdaytraining", "lastdaytraining", "solver",
  "clearvalueset"
)
expect_symbols_in_gdx(
  app, "btBatchDownloadGDX",
  setNames(rep.int(list(c(multiDimSym, scalarSym)), 2L), c("default", scenHashTmp))
)

Sys.sleep(0.5)
app$setInputs(batchLoadResults_rows_selected = scenToCompare, allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$waitFor("$('#btBatchDownloadGDX+.dropdown-toggle').click()", timeout = 50L)
Sys.sleep(0.5)
expect_files_in_zip(
  app, "btBatchDownloadCSV",
  unlist(lapply(c("default", scenHashTmp), function(scenName) {
    paste0(scenName, "/", c("", paste0(c(
      multiDimSym, "_metadata_",
      "_scalars", "_scalars_out"
    ), ".csv")))
  }))
)


# load second scenario into sandbox, change scalar value and save again as standard scenario
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(btSendQuery = "click")
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===8", timeout = 50L))
app$setInputs(batchLoadResults_rows_selected = scenToCompare[2], allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$setInputs(btBatchLoadSb = "click")
Sys.sleep(3L)
app$setInputs(inputTabset = "inputTabset_1")
Sys.sleep(1L)
app$setInputs(slider_3 = 12)
Sys.sleep(1L)

app$setInputs(btSave = "click")
Sys.sleep(2L)
app$setInputs(btRemoveOutput = "click")
Sys.sleep(2L)

app$findElement("#sidebarItemExpanded a[data-value='scenarios']")$click()
app$waitFor("$('#refreshSandbox_5 button').click()", timeout = 50L)
Sys.sleep(2L)
expect_true(app$waitFor(paste0(
  "$('#cmpScenTitle_5').text()==='",
  scenHashTmp, "'"
), timeout = 50L))
expect_true(app$waitFor("$('.small-box:visible')[0].textContent.trim().startsWith('12')", timeout = 50L))
expect_true(app$waitFor("$('.small-box:visible')[1].textContent.trim().startsWith('99')", timeout = 50))
app$setInputs(contentScen_5 = "contentScen_5_1")
expect_true(app$waitFor("$('#tab_5_3-noData').is(':visible')", timeout = 50))

app$stop()
