app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("hcube_module_compare_test")

context("UI tests - Hypercube module - compare mode")
app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)

# use batch loder module to fetch HC scenarios
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(btSendQuery = "click")
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===8", timeout = 50L))
app$setInputs(batchLoadResults_rows_selected = c(1, 2), allowInputNoBinding_ = TRUE)
scenData <- getVisibleDtData(app, "batchLoadResults")
scenHashTmp <- scenData[[2]][2]

app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$waitFor("$('#btBatchCompare').click()", timeout = 50L)
Sys.sleep(3L)
expect_true(app$waitFor(paste0("$('#cmpScenTitle_5').text().startsWith('HC (",
                               substr(scenHashTmp, 1, 8), "...)')"), timeout = 50L))
graphData <- jsonlite::fromJSON(app$getAllValues()$output[["tab_5_3-graph"]], simplifyDataFrame = FALSE)$x$data
expect_identical(length(graphData), 756L)
expect_identical(graphData[, 1], c("2016-01-04T00:00:00.000Z", "100.572928794899", "101.703327363261"))
app$setInputs(contentScen_5 = "contentScen_5_11")
expect_true(app$waitFor(paste0("$('.small-box:visible')[0].textContent.trim().startsWith('",
                               scenData[[14]][2], "')"), timeout = 50L))
expect_true(app$waitFor("$('.small-box:visible')[1].textContent.trim().startsWith('99')", timeout = 50))


# load second scenario into sandbox, change scalar value and save again as standard scenario
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(btSendQuery = "click")
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===8", timeout = 50L))
app$setInputs(batchLoadResults_rows_selected = c(2), allowInputNoBinding_ = TRUE)
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
expect_true(app$waitFor(paste0("$('#cmpScenTitle_5').text()==='",
                               scenHashTmp, "'"), timeout = 50L))
expect_true(app$waitFor("$('.small-box:visible')[0].textContent.trim().startsWith('12')", timeout = 50L))
expect_true(app$waitFor("$('.small-box:visible')[1].textContent.trim().startsWith('99')", timeout = 50))
app$setInputs(contentScen_5 = "contentScen_5_1")
expect_true(app$waitFor("$('#tab_5_3-noData').is(':visible')", timeout = 50))

app$stop()