app <- AppDriver$new("../../", name = "hcube_module_compare_test", variant = NULL, load_timeout = 20000)

context("UI tests - Hypercube module - compare mode")

# use batch loader module to fetch HC scenarios
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===8;", timeout = 5000L), NA)
scenData <- getVisibleDtData(app, "batchLoadResults")

# Makes sure hidden output scalars (firstdaytraining/lastdaytraining) were properly stored
expect_false(any(is.na(scenData[[20]])))
expect_false(any(is.na(scenData[[21]])))

if (.Platform$OS.type == "windows") {
  # scenario wtih lstTitleLeftAligned (checkbox tab) = 0 and maxstock = 6
  # CRLF in dowjones2016.csv results in different hash
  scenHashTmp <- "543b744bb7d6f1116d3f95bf8c1656e4d0039f13d096f929edde05cff46bcbb1"
} else {
  scenHashTmp <- "16717a4fd5c24e8a054410dc20f6e6b57ee8903db21ea3e48164716e10471c5f"
}
scenToCompare <- match(c("default", scenHashTmp), scenData[[2]])
expect_false(any(is.na(scenToCompare)))
app$set_inputs(batchLoadResults_rows_selected = rev(scenToCompare), allow_no_input_binding_ = TRUE)

# check that scalar-name-mapping works
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$set_inputs(batchCompareNameCols = c("_gmsopt_lsttitleleftaligned", "maxstock"))
app$run_js("$('#btBatchCompare').click()", timeout = 50L)
Sys.sleep(3L)
app$set_inputs(contentScen_0 = "contentScen_0_2")
Sys.sleep(2L)
app$set_inputs(`tab_0_2-miroPivot-pivotRenderer` = "line")
Sys.sleep(2L)
expect_identical(
  jsonlite::fromJSON(app$get_values()$output[["tab_0_2-miroPivot-pivotChart"]])$x$data$labels,
  c("0_6.AAPL", "0_6.AXP", "0_6.BA", "0_6.GS", "0_6.HD", "0_6.MMM", "1_2.DD", "1_2.MCD")
)

# load scenarios into tab comparison mode
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(batchLoadResults_rows_selected = scenToCompare, allow_no_input_binding_ = TRUE)

app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$run_js("$('#btBatchCompare+.dropdown-toggle').click()&&$('#btBatchCompare~.dropdown-menu a:first').click();", timeout = 50L)
Sys.sleep(3L)
expect_true(app$get_js(paste0(
  "$('#cmpScenTitle_5').text().startsWith('HC (",
  substr(scenHashTmp, 1, 8), "...)');"
), timeout = 50L))
graphData <- jsonlite::fromJSON(app$get_values()$output[["tab_5_3-graph"]], simplifyDataFrame = FALSE)$x$data
expect_identical(length(graphData), 756L)
expect_identical(graphData[, 1], c("2016-01-04T00:00:00.000Z", "100.572928794899", "101.703327363261"))
app$set_inputs(contentScen_5 = "contentScen_5_11")
Sys.sleep(0.5)
expect_true(app$get_js("$('.small-box:visible')[0].textContent.trim().startsWith('6');", timeout = 1000L))
expect_true(app$get_js("$('.small-box:visible')[1].textContent.trim().startsWith('99');", timeout = 1000L))

# Download HC and normal scenario while remapping scenario names
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(batchLoadResults_rows_selected = scenToCompare, allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$set_inputs(batchCompareNameCols = c("_gmsopt_lsttitleleftaligned", "maxstock"))
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
app$set_inputs(batchLoadResults_rows_selected = scenToCompare, allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$set_inputs(batchCompareNameCols = c("maxstock", "_gmsopt_lsttitleleftaligned"))
app$run_js("$('#btBatchDownloadGDX+.dropdown-toggle').click()", timeout = 50L)
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
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(batchLoadResults_rows_selected = scenToCompare, allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
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
app$set_inputs(batchLoadResults_rows_selected = scenToCompare, allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$run_js("$('#btBatchDownloadGDX+.dropdown-toggle').click()", timeout = 50L)
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
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===8;", timeout = 5000L), NA)
app$set_inputs(batchLoadResults_rows_selected = scenToCompare[2], allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$set_inputs(btBatchLoadSb = "click")
Sys.sleep(3L)
app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(1L)
app$set_inputs(slider_3 = 12)
Sys.sleep(1L)

app$set_inputs(btSave = "click")
Sys.sleep(2L)
app$set_inputs(btRemoveOutput = "click")
Sys.sleep(2L)

app$click(selector = "#sidebarItemExpanded a[data-value='scenarios']")
app$run_js("$('#refreshSandbox_5 button').click()", timeout = 50L)
Sys.sleep(2L)
expect_true(app$get_js(paste0(
  "$('#cmpScenTitle_5').text()==='",
  scenHashTmp, "';"
), timeout = 2000L))
expect_true(app$get_js("$('.small-box:visible')[0].textContent.trim().startsWith('12');", timeout = 50L))
expect_true(app$get_js("$('.small-box:visible')[1].textContent.trim().startsWith('99');", timeout = 50))
app$set_inputs(contentScen_5 = "contentScen_5_1")
expect_true(app$get_js("$('#tab_5_3-noData').is(':visible');", timeout = 50))

app$stop()
