app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("miropivot_test")

getData <- function(id = "tab_1_1") {
  return(jsonlite::fromJSON(app$getAllValues()$output[[paste0(id, "-miroPivot-pivotChart")]])$x$data$datasets$data)
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
expect_true(app$waitFor("$('#tab_1_1-miroPivot-filterIndexList').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-saveView').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-filter_Hdr').next().is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-aggregateDropdowns').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-aggregationIndexList').is(':visible');", 50))
# col row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.col-filter').is(':visible');", 50))
# table row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-rowIndexList').is(':visible');", 50))
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.table-chart').children('.col-md-10').is(':visible');", 50))
# data-section row
expect_true(app$waitFor("$('#tab_1_1-miroPivot-container .row.data-section').is(':hidden');", 50))

app$setInputs(outputTabset = "outputTabset_2")
Sys.sleep(0.5)
expect_equal(getData("tab_1_2"), list(c(NA, 300), c(275, 50), c(275, NA)))
app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$findElement('#editMetaUI a[data-value="views"]')$click()
app$uploadFile(file_addViews = "../data/transport_views.json")
expect_identical(length(app$findElements("#currentViewsTable tbody tr")), 1L)
app$findElement('button[data-dismiss="modal"]')$click()
Sys.sleep(1)
app$findElement("#tab_1_2-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
# load new default view (external view)
expect_true(app$waitFor("$('#tab_1_2-miroPivot-savedViewsDD li').eq(0).children('.dropdown-item').click();true;", timeout = 50))
Sys.sleep(1)
expect_equal(getData("tab_1_2"), list(c(950, 950, 950), c(600, 650, 550)))
# delete view and load old default again
app$findElement("#tab_1_2-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_1_2-miroPivot-savedViewsDD li').eq(1).children('.miro-pivot-view-button').eq(1).click();true;", timeout = 50))
Sys.sleep(0.5)
app$findElement("#tab_1_2-miroPivot-toggleViewButton")$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tab_1_2-miroPivot-savedViewsDD li').eq(0).children('.dropdown-item').click();true;", timeout = 50))
Sys.sleep(0.5)
expect_equal(getData("tab_1_2"), list(c(NA, 300), c(275, 50), c(275, NA)))

app$setInputs("tab_1_2-miroPivot-pivotRenderer" = "table")
Sys.sleep(0.5)

app$setInputs(`tab_1_2-miroPivot-showSettings` = "click")
Sys.sleep(1)
app$setInputs(`tab_1_2-miroPivot-showTableSummary` = TRUE)
app$setInputs(`tab_1_2-miroPivot-colSummaryFunction` = "mean")
app$setInputs(`tab_1_2-miroPivot-rowSummaryFunction` = "count")
app$setInputs(`tab_1_2-miroPivot-updateSettings` = "click")
Sys.sleep(1)

expect_identical(
  getVisibleDtData(app, "tab_1_2-miroPivot-pivotTable"),
  structure(list(
    ...1 = c("San-Diego", "Seattle"), ...2 = c(
      NA,
      "300"
    ), ...3 = c("275", "50"), ...4 = c("275", NA),
    ...5 = c(
      "2",
      "2"
    )
  ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -2L
  ))
)
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Mean'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(1)').text()==='300'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='162.5'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='275'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(4)').text()==='2'",
  timeout = 50
))

app$setInputs(`tab_1_2-miroPivot-showSettings` = "click")
Sys.sleep(1)
app$setInputs(`tab_1_2-miroPivot-showTableSummary` = TRUE)
app$setInputs(`tab_1_2-miroPivot-colSummaryFunction` = "min")
app$setInputs(`tab_1_2-miroPivot-rowSummaryFunction` = "mean")
app$setInputs(`tab_1_2-miroPivot-updateSettings` = "click")
Sys.sleep(1)

expect_identical(
  getVisibleDtData(app, "tab_1_2-miroPivot-pivotTable"),
  structure(list(
    ...1 = c("San-Diego", "Seattle"),
    ...2 = c(
      NA,
      "300"
    ),
    ...3 = c("275", "50"),
    ...4 = c("275", NA),
    ...5 = c(
      "275",
      "175"
    )
  ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -2L
  ))
)
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Min'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(1)').text()==='300'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='50'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='275'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(4)').text()==='175'",
  timeout = 50
))

app$setInputs(`tab_1_2-miroPivot-showSettings` = "click")
Sys.sleep(1)
app$setInputs(`tab_1_2-miroPivot-showTableSummary` = TRUE)
app$setInputs(`tab_1_2-miroPivot-colSummaryFunction` = "max")
app$setInputs(`tab_1_2-miroPivot-rowSummaryFunction` = "sum")
app$setInputs(`tab_1_2-miroPivot-updateSettings` = "click")
Sys.sleep(1)

expect_identical(
  getVisibleDtData(app, "tab_1_2-miroPivot-pivotTable"),
  structure(list(
    ...1 = c("San-Diego", "Seattle"),
    ...2 = c(
      NA,
      "300"
    ),
    ...3 = c("275", "50"),
    ...4 = c("275", NA),
    ...5 = c(
      "550",
      "350"
    )
  ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -2L
  ))
)
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Max'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(1)').text()==='300'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='275'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='275'",
  timeout = 50
))
expect_true(app$waitFor("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(4)').text()==='550'",
  timeout = 50
))

app$stop()
