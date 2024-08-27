app <- AppDriver$new("../../",
  name = "miropivot_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
getData <- function(id = "tab_1_1") {
  return(jsonlite::fromJSON(app$get_values()$output[[paste0(id, "-miroPivot-pivotChart")]])$x$data$datasets$data)
}
Sys.sleep(2)
app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)

app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
app$upload_file(file_addViews = "../data/transport_views3.json")
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#tab_1_1-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$get_js("$('#tab_1_1-miroPivot-savedViewsDD li').eq(1).children('.dropdown-item').click();", timeout = 50), NA)
Sys.sleep(1)
# check chartOptions configuration
configuration <- app$get_js("Chart.instances[1].config._config", timeout = 50)
expect_true(identical(configuration$options$plugins$title$text, "test"))
expect_true(identical(configuration$options$scales$x$title$text, "xxx"))
expect_true(identical(configuration$options$scales$y$title$text, "yyy"))
expect_true(identical(configuration$options$scales$y2$title$text, "zzz"))
expect_true(identical(configuration$options$scales$x$grid$display, FALSE))
expect_true(identical(configuration$options$scales$y$grid$display, TRUE))
expect_true(identical(configuration$options$scales$y$min, 15L))
expect_true(identical(configuration$options$scales$y2$type, "logarithmic"))
expect_true(identical(configuration$data$datasets[[2]]$label, "New-york"))
expect_true(identical(configuration$data$datasets[[3]]$label, "Topeka"))
expect_true(identical(configuration$options$elements$point$radius, 0L))
expect_true(identical(configuration$data$datasets[[2]]$pointRadius, 3L))
expect_true(identical(configuration$data$datasets[[2]]$type, "line"))
expect_true(identical(configuration$data$datasets[[2]]$stepped, TRUE))
expect_true(identical(configuration$data$datasets[[2]]$stack, "2"))
expect_true(identical(configuration$type, "bar"))


Sys.sleep(1)
app$click(selector = "#tab_1_1-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$get_js("$('#tab_1_1-miroPivot-savedViewsDD li').eq(0).children('.dropdown-item').click();", timeout = 50), NA)

expect_equal(getData(), list(c(NA, 300), c(275, 50), c(275, NA)))

# presentation mode
app$click(selector = "div[id='tab_1_1-miroPivot-hidePivotControls']")
Sys.sleep(3)
app$expect_values(
  input = c(
    "tab_1_1-miroPivot-aggregationFunction",
    "tab_1_1-miroPivot-aggregationIndexList",
    "tab_1_1-miroPivot-colIndexList",
    "tab_1_1-miroPivot-filterIndexList",
    "tab_1_1-miroPivot-filter_Hdr",
    "tab_1_1-miroPivot-filter_j",
    "tab_1_1-miroPivot-pivotRenderer",
    "tab_1_1-miroPivot-rowIndexList",
    "tab_1_1-miroPivot-saveView"
  )
)
# filter row
expect_true(app$get_js("$('#tab_1_1-miroPivot-toggleViewButton').is(':visible')"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-filterIndexList').is(':hidden');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-saveView').is(':hidden');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-filter_Hdr').next().is(':hidden');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-aggregateDropdowns').is(':hidden');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-aggregationIndexList').is(':hidden');"))
# col row
expect_true(app$get_js("$('#tab_1_1-miroPivot-container .row.col-filter').is(':hidden');"))
# table row
expect_true(app$get_js("$('#tab_1_1-miroPivot-rowIndexList').is(':hidden');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-container .row.table-chart').children('.col-sm-12').is(':visible');"))
# data-section row
expect_true(app$get_js("$('#tab_1_1-miroPivot-container .row.data-section').is(':visible');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-rowsPresentation .drop-index-item-presentation')[0].innerText == 'Header';"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-rowsPresentation .uel-item-presentation')[0].innerText == 'quantities';"))

app$click(selector = "div[id='tab_1_1-miroPivot-showPivotControls']")
Sys.sleep(3)

# filter row
expect_true(app$get_js("$('#tab_1_1-miroPivot-toggleViewButton').is(':visible')"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-filterIndexList').is(':visible');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-saveView').is(':visible');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-filter_Hdr').next().is(':visible');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-aggregateDropdowns').is(':visible');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-aggregationIndexList').is(':visible');"))
# col row
expect_true(app$get_js("$('#tab_1_1-miroPivot-container .row.col-filter').is(':visible');"))
# table row
expect_true(app$get_js("$('#tab_1_1-miroPivot-rowIndexList').is(':visible');"))
expect_true(app$get_js("$('#tab_1_1-miroPivot-container .row.table-chart').children('.col-md-10').is(':visible');"))
# data-section row
expect_true(app$get_js("$('#tab_1_1-miroPivot-container .row.data-section').is(':hidden');"))

app$set_inputs(outputTabset = "outputTabset_2")
Sys.sleep(0.5)
expect_equal(getData("tab_1_2"), list(c(NA, 300), c(275, 50), c(275, NA)))
app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
app$upload_file(file_addViews = "../data/transport_views.json")
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 2L)
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#tab_1_2-miroPivot-toggleViewButton")
Sys.sleep(0.5)
# load new default view (external view)
expect_error(app$get_js("$('#tab_1_2-miroPivot-savedViewsDD li').eq(0).children('.dropdown-item').click();", timeout = 50), NA)
Sys.sleep(1)
expect_equal(getData("tab_1_2"), list(c(950, 950, 950), c(600, 650, 550)))
# delete view and load old default again
app$click(selector = "#tab_1_2-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$get_js("$('#tab_1_2-miroPivot-savedViewsDD li').eq(1).children('.miro-pivot-view-button').eq(1).click();", timeout = 50), NA)
Sys.sleep(0.5)
app$click(selector = "#tab_1_2-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$get_js("$('#tab_1_2-miroPivot-savedViewsDD li').eq(0).children('.dropdown-item').click();", timeout = 50), NA)
Sys.sleep(0.5)
expect_equal(getData("tab_1_2"), list(c(NA, 300), c(275, 50), c(275, NA)))

app$set_inputs("tab_1_2-miroPivot-pivotRenderer" = "table")
Sys.sleep(0.5)

app$set_inputs(`tab_1_2-miroPivot-showSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$set_inputs(`tab_1_2-miroPivot-showTableSummary` = TRUE)
app$set_inputs(`tab_1_2-miroPivot-colSummaryFunction` = "mean")
app$set_inputs(`tab_1_2-miroPivot-rowSummaryFunction` = "count")
app$set_inputs(`tab_1_2-miroPivot-updateSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)
Sys.sleep(0.5)
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
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Mean'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(1)').text()==='300'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='162.5'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='275'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(4)').text()==='2'",
  timeout = 50
))

app$set_inputs(`tab_1_2-miroPivot-showSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$set_inputs(`tab_1_2-miroPivot-showTableSummary` = TRUE)
app$set_inputs(`tab_1_2-miroPivot-colSummaryFunction` = "min")
app$set_inputs(`tab_1_2-miroPivot-rowSummaryFunction` = "mean")
app$set_inputs(`tab_1_2-miroPivot-updateSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)
Sys.sleep(0.5)
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
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Min'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(1)').text()==='300'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='50'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='275'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(4)').text()==='175'",
  timeout = 50
))

app$set_inputs(`tab_1_2-miroPivot-showSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$set_inputs(`tab_1_2-miroPivot-showTableSummary` = TRUE)
app$set_inputs(`tab_1_2-miroPivot-colSummaryFunction` = "max")
app$set_inputs(`tab_1_2-miroPivot-rowSummaryFunction` = "sum")
app$set_inputs(`tab_1_2-miroPivot-updateSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)
Sys.sleep(0.5)
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
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Max'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(1)').text()==='300'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='275'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='275'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(4)').text()==='550'",
  timeout = 50
))
app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
app$upload_file(file_addViews = "../data/transport_views2.json")
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#tab_1_2-miroPivot-toggleViewButton")
Sys.sleep(0.5)
expect_error(app$get_js("$('#tab_1_2-miroPivot-savedViewsDD li').eq(1).children('.dropdown-item').click();", timeout = 50), NA)
Sys.sleep(1)
expect_identical(
  getVisibleDtData(app, "tab_1_2-miroPivot-pivotTable"),
  structure(list(
    ...1 = c("San-Diego", "Seattle"),
    ...2 = c(
      "900",
      "950"
    ),
    ...3 = c("1200", "725"),
    ...4 = c("1150", "625")
  ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -2L
  ))
)
app$set_inputs(`tab_1_2-miroPivot-showSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
app$set_inputs(`tab_1_2-miroPivot-showTableSummary` = TRUE)
app$set_inputs(`tab_1_2-miroPivot-colSummaryFunction` = "max")
app$set_inputs(`tab_1_2-miroPivot-rowSummaryFunction` = "sum")
app$set_inputs(`tab_1_2-miroPivot-updateSettings` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)
Sys.sleep(0.5)
expect_identical(
  getVisibleDtData(app, "tab_1_2-miroPivot-pivotTable"),
  structure(list(
    ...1 = c("San-Diego", "Seattle"),
    ...2 = c(
      "900",
      "950"
    ),
    ...3 = c("1200", "725"),
    ...4 = c("1150", "625"),
    ...5 = c("3250", "2300")
  ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -2L
  ))
)
Sys.sleep(30)
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Max'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(1)').text()==='950'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='1200'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='1150'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_1_2-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(4)').text()==='3250'",
  timeout = 50
))

app$click(selector = "a[data-value='inputData']")
Sys.sleep(0.5)
app$set_inputs(inputTabset = "inputTabset_4")
Sys.sleep(1)
app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(1)
expect_identical(
  getVisibleDtData(app, "data-in_3-inputPivot-pivotTable"),
  structure(list(), class = c("tbl_df", "tbl", "data.frame"), row.names = integer(0), names = character(0))
)
app$set_inputs(`data-in_3-inputPivot-btAddRow` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 5000L)
Sys.sleep(0.5)
addSelectizeOption(app, "#data-in_3-inputPivot-newRow_1", "test1")
selectSelectizeOption(app, "#data-in_3-inputPivot-newRow_1", "test1")
addSelectizeOption(app, "#data-in_3-inputPivot-newRow_2", "test2")
selectSelectizeOption(app, "#data-in_3-inputPivot-newRow_2", "test2")
app$set_inputs(`data-in_3-inputPivot-newRow_3` = "1.2345")
app$set_inputs(`data-in_3-inputPivot-btAddRowConfirm` = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L)
Sys.sleep(0.5)
expect_identical(app$get_text(selector = "#inputDataTitle"), "<New Scenario> (*)")
expect_identical(
  getVisibleDtData(app, "data-in_3-inputPivot-pivotTable"),
  structure(list(`...1` = "test1", `...2` = "1.2345"), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))
)
app$stop()
