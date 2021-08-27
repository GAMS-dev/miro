app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("miropivot_test")

getData <- function() {
  return(jsonlite::fromJSON(app$getAllValues()$output[["tab_1_1-miroPivot-pivotChart"]])$x$data$datasets$data)
}
Sys.sleep(2)
app$findElement("a[data-value='outputData']")$click()
Sys.sleep(1)
app$snapshot(
  items = list(output = "outputDataTitle"),
  screenshot = TRUE
)
expect_equal(getData(), list(c(NA, 300), c(275, 50), c(275, NA)))

# presentation mode
app$findElement("div[id='tab_1_1-miroPivot-hidePivotControls']")$click()
Sys.sleep(3)
app$snapshot(
  items = list(input = c(
    "tab_1_1-miroPivot-aggregationFunction",
    "tab_1_1-miroPivot-aggregationIndexList",
    "tab_1_1-miroPivot-colIndexList",
    "tab_1_1-miroPivot-filter_Hdr",
    "tab_1_1-miroPivot-filter_j",
    "tab_1_1-miroPivot-filterIndexList",
    "tab_1_1-miroPivot-pivotRenderer",
    "tab_1_1-miroPivot-rowIndexList",
    "tab_1_1-miroPivot-saveView"
  )),
  screenshot = TRUE
)
# filter row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-toggleViewButton').is(':visible')"))
expect_true(app$waitFor("$(\"a[data-proxy-id='tab_1_1-miroPivot-downloadCsv']\").is(':visible')", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-downloadCsv').is(':hidden')", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-filterIndexList').is(':hidden');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-saveView').is(':hidden');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-filter_Hdr').next().is(':hidden');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-aggregateDropdowns').is(':hidden');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-aggregationIndexList').is(':hidden');", 50))
# col row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.col-filter').is(':hidden');", 50))
# table row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-rowIndexList').is(':hidden');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.table-chart').children('.col-sm-12').is(':visible');", 50))
# data-section row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.data-section').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-rowsPresentation .drop-index-item-presentation')[0].innerText == 'Header';", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-rowsPresentation .uel-item-presentation')[0].innerText == 'quantities';", 50))

app$findElement("div[id='tab_1_1-miroPivot-showPivotControls']")$click()
Sys.sleep(3)

# filter row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-toggleViewButton').is(':visible')"))
expect_true(app$waitFor("$(\"a[data-proxy-id='tab_1_1-miroPivot-downloadCsv']\").is(':hidden')", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-downloadCsv').is(':visible')", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-filterIndexList').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-saveView').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-filter_Hdr').next().is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-aggregateDropdowns').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-aggregationIndexList').is(':visible');", 50))
# col row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.col-filter').is(':visible');", 50))
# table row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-rowIndexList').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.table-chart').children('.col-sm-10').is(':visible');", 50))
# data-section row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.data-section').is(':hidden');", 50))


app$stop()
