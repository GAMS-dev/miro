app <- AppDriver$new("../../",
  name = "load_pivot_compare_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

currentUser <- Sys.info()[["user"]]

app$click(selector = 'a[data-value="scenarios"]')
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='tab']")
Sys.sleep(0.5)

app$click(selector = "#cmpTabNoScenWrapper .action-button")
Sys.sleep(0.5)
scenToSelect <- paste0(c("1_", "3_"), currentUser)
app$set_inputs(selLoadScen = scenToSelect, wait_ = FALSE, values_ = FALSE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)

# scenarios loaded in tab view: 1, 3

# load scenarios into split view (1 from tab view, 1 from db)
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='split']")

app$set_inputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
app$set_inputs(tabsetLoadScen = "loadScenUI")
expect_options(getSelectizeOptions(app, "#selLoadScenUI"), scenToSelect)
app$set_inputs(selLoadScenUI = paste0("1_", currentUser))
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)

app$set_inputs(btScenSplit2_open = "click")
Sys.sleep(0.5)
expect_options(
  getSelectizeOptions(app, "#selLoadScen"),
  paste0(c("2_", "3_", "4_"), currentUser)
)
app$set_inputs(selLoadScen = paste0("4_", currentUser))
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
expect_identical(app$get_text(selector = "#cmpScenTitle_2"), "default1")
expect_identical(app$get_text(selector = "#cmpScenTitle_3"), "default4")
# scenarios loaded in split view: 1, 4

# load scenarios into pivot view
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='pivot']")

app$click(selector = "#pivotCompBtWrapper button")
Sys.sleep(0.5)
expect_options(
  app$get_value(input = "selLoadScen"),
  c(scenToSelect, paste0("4_", currentUser))
)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$set_inputs(contentScen_0 = "contentScen_0_4")
Sys.sleep(0.5)
app$set_inputs("tab_0_3-miroPivot-pivotRenderer" = "stackedbar")
Sys.sleep(0.5)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  c(600, 350, 600, 350, 600, 350),
  c(
    "default1.San-Diego",
    "default1.Seattle",
    "default3.San-Diego",
    "default3.Seattle",
    "default4.San-Diego",
    "default4.Seattle"
  )
)
app$set_inputs("tab_0_3-miroPivot-showSettings" = "click")
Sys.sleep(1)
expect_error(app$set_inputs(`tab_0_3-miroPivot-hideEmptyCols` = TRUE), NA)
app$set_inputs(`tab_0_3-miroPivot-updateSettings` = "click")
Sys.sleep(1)
app$click(selector = "#scen-pivot-view .box-title > button:nth-child(1)")
Sys.sleep(0.5)
expect_options(
  app$get_values()$input$selLoadScen,
  c(scenToSelect, paste0("4_", currentUser))
)
app$set_inputs(selLoadScen = paste0(c("1_", "2_"), currentUser))
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$set_inputs("tab_0_3-miroPivot-pivotRenderer" = "table")
Sys.sleep(0.5)
expect_identical(
  getVisibleDtData(app, "tab_0_3-miroPivot-pivotTable"),
  structure(list(
    ...1 = c("default1", "default1", "default2", "default2"), ...2 = c("San-Diego", "Seattle", "San-Diego", "Seattle"),
    ...3 = c("600", "350", "600", "350")
  ), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ), row.names = c(NA, -4L))
)
app$set_inputs(`tab_0_3-miroPivot-showSettings` = "click")
Sys.sleep(1)
app$set_inputs(`tab_0_3-miroPivot-showTableSummaryRow` = TRUE)
app$set_inputs(`tab_0_3-miroPivot-showTableSummaryCol` = TRUE)
app$set_inputs(`tab_0_3-miroPivot-colSummaryFunction` = "mean")
app$set_inputs(`tab_0_3-miroPivot-rowSummaryFunction` = "count")
app$set_inputs(`tab_0_3-miroPivot-updateSettings` = "click")
Sys.sleep(1)
expect_identical(
  getVisibleDtData(app, "tab_0_3-miroPivot-pivotTable"),
  structure(list(
    ...1 = c("default1", "default1", "default2", "default2"), ...2 = c("San-Diego", "Seattle", "San-Diego", "Seattle"),
    ...3 = c("600", "350", "600", "350"), ...4 = c(
      "1", "1",
      "1", "1"
    )
  ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
    NA,
    -4L
  ))
)
expect_true(app$get_js("$('#tab_0_3-miroPivot-pivotTable .dataTables_scrollFootInner th:first').text()==='Mean'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_0_3-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(2)').text()==='475'",
  timeout = 50
))
expect_true(app$get_js("$('#tab_0_3-miroPivot-pivotTable .dataTables_scrollFootInner th:nth(3)').text()==='1'",
  timeout = 50
))
app$set_inputs("tab_0_3-miroPivot-pivotRenderer" = "stackedbar")
Sys.sleep(0.5)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  c(600, 350, 600, 350),
  c(
    "default1.San-Diego",
    "default1.Seattle",
    "default2.San-Diego",
    "default2.Seattle"
  )
)
app$click(selector = "#scen-pivot-view .box-title > button:nth-child(1)")
Sys.sleep(0.5)
expect_identical(app$get_text(selector = "#cmpScenTitle_2"), "default1")
expect_identical(app$get_text(selector = "#cmpScenTitle_3"), "default4")
expect_options(
  app$get_values()$input$selLoadScen,
  paste0(c("1_", "2_"), currentUser)
)
app$click(selector = "button[data-dismiss='modal']")
Sys.sleep(0.5)
# scenarios loaded in pivot view: 1, 2

# check that both scenarios loaded in tab view as well as pivot view
# are displayed in UI tab in split view
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='split']")
app$set_inputs(btScenSplit1_close = "click")
Sys.sleep(0.5)
app$set_inputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
app$set_inputs(tabsetLoadScen = "loadScenUI")
expect_options(
  getSelectizeOptions(app, "#selLoadScenUI"),
  paste0(c("1_", "2_", "3_"), currentUser)
)
app$click(selector = "button[data-dismiss='modal']")
Sys.sleep(0.5)

# check that both scenarios loaded in split view as well as pivot view
# are selected when loading scenario in tab view
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='tab']")
Sys.sleep(0.5)

app$click(selector = "#btLoadScen")
Sys.sleep(0.5)
expect_options(
  getSelectizeOptions(app, "#selLoadScen"),
  paste0(c("2_", "4_", "sb_"), currentUser)
)
app$click(selector = "button[data-dismiss='modal']")
Sys.sleep(0.5)

app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='pivot']")
app$click(selector = "#btScenPivot_close")
Sys.sleep(0.5)
# scenarios loaded in pivot view: none

# check that both scenarios loaded in tab view as well as pivot view
# are displayed in UI tab in split view
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='split']")

app$set_inputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
app$set_inputs(tabsetLoadScen = "loadScenUI")
expect_options(
  getSelectizeOptions(app, "#selLoadScenUI"),
  paste0(c("1_", "3_"), currentUser)
)
app$click(selector = "button[data-dismiss='modal']")
Sys.sleep(0.5)

# close all in tab view
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='tab']")
Sys.sleep(0.5)
app$click(selector = "#btCmpTabCloseAll")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('.modal-footer .bt-gms-confirm').click()", timeout = 50), NA)
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#scenTabset li').length===1&&$('#cmpTabNoScenWrapper').is(':visible')", timeout = 50), NA)
app$click(selector = "#cmpTabNoScenWrapper .action-button")
Sys.sleep(0.5)
expect_true(startsWith(as.character(app$get_value(input = "selLoadScen")), "4_"))

app$stop()
