app <- AppDriver$new("../../",
  name = "custom_widgets_scalars_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2)
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L), NA)
app$set_inputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$set_inputs(remoteCredUser = Sys.getenv("ENGINE_USER"))
app$set_inputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$set_inputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$set_inputs(remoteCredReg = FALSE)
app$set_inputs(remoteCredRemember = TRUE)
app$run_js("$('#shiny-modal .bt-gms-confirm').click()")
expect_error(app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 5000L), NA)

app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(1)
expect_equivalent(
  getHotData(app, "data-in_8-custom-sudoku"),
  structure(
    list(
      col1 = c("", "", "", "", "", "", "", "", ""),
      col2 = c("", "", "", "", "", "", "", "", ""),
      col3 = c("", "", "", "", "", "", "", "", ""),
      col4 = c("", "", "", "", "", "", "", "", ""),
      col5 = c("", "", "", "", "", "", "", "", ""),
      col6 = c("", "", "", "", "", "", "", "", ""),
      col7 = c("", "", "", "", "", "", "", "", ""),
      col8 = c("", "", "", "", "", "", "", "", ""),
      col9 = c("", "", "", "", "", "", "", "", "")
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -9L)
  )
)
expect_identical(app$get_values()$output[["data-in_8-custom-testOutput"]], "1.1")
expect_identical(app$get_values()$input[["data-in_8-custom-force_unique_sol"]], FALSE)
expect_identical(app$get_values()$output[["data-in_8-custom-uniqueSolWarning"]], "")
app$set_inputs(btImport = "click")
Sys.sleep(1)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)
expect_equivalent(
  getHotData(app, "data-in_8-custom-sudoku"),
  structure(
    list(
      col1 = c("", "", "6", "8", "4", "2", "5", "", ""),
      col2 = c("", "7", "9", "", "", "", "", "", ""),
      col3 = c("", "", "", "", "", "9", "3", "", ""),
      col4 = c("", "9", "", "", "", "", "", "5", "3"),
      col5 = c("8", "", "", "9", "", "1", "", "", "7"),
      col6 = c("6", "2", "", "", "", "", "", "8", ""),
      col7 = c("", "", "2", "7", "", "", "", "", ""),
      col8 = c("", "", "", "", "", "", "7", "2", ""),
      col9 = c("", "", "8", "2", "3", "4", "6", "", "")
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -9L)
  )
)
expect_identical(app$get_values()$output[["data-in_8-custom-testOutput"]], "2.3")
expect_identical(app$get_values()$input[["data-in_8-custom-force_unique_sol"]], TRUE)
expect_identical(app$get_values()$output[["data-in_8-custom-uniqueSolWarning"]], "Model will abort if more than one solution exists.")
app$set_inputs(inputTabset = "inputTabset_6")
Sys.sleep(1)
expect_identical(app$get_values()$output[["data-in_11-custom-i"]], "seattle,san-diego")
expect_identical(app$get_values()$output[["data-in_11-custom-j"]], "new-york,chicago,topeka")
expect_identical(app$get_values()$output[["data-in_11-custom-initial_state2"]], "133")
expect_identical(app$get_values()$input[["data-in_11-custom-bla"]], "bla1")
expect_equivalent(
  getHotData(app, "data-in_11-custom-sudoku"),
  structure(
    list(
      i = c("seattle", "seattle", "seattle", "san-diego", "san-diego", "san-diego"),
      j = c("new-york", "chicago", "topeka", "new-york", "chicago", "topeka"),
      value = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -6L)
  )
)
app$run_js("HTMLWidgets.getInstance(document.getElementById('data-in_11-custom-sudoku')).hot.setDataAtCell(0,0,'test');")
app$click(selector = 'a[data-value="scenarios"]')
app$run_js("$('.scenSplit-button-load').eq(1).click();")
Sys.sleep(1)
app$set_inputs(contentScen_2 = "contentScen_2_2")
Sys.sleep(1)
expect_identical(
  getVisibleDtData(app, "tab_2_10-datatable"),
  structure(
    list(
      ...1 = c("1", "2", "3", "4", "5"),
      ...2 = c("force_unique_sol", "test", "test124", "test123", "_gmspar_bla"),
      ...3 = c("force_unique_sol", "test", "Distance", "test 123", "bla bla"),
      ...4 = c("1", "seattle", "3.3", "2.3", "bla1")
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -5L)
  )
)
app$set_inputs(contentScen_2 = "contentScen_2_4")
Sys.sleep(3)
expect_identical(
  getVisibleDtData(app, "tab_2_2-miroPivot-pivotTable"),
  structure(
    list(
      ...1 = c("san-diego", "seattle", "test"),
      ...2 = c("1", "1", "1")
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -3L)
  )
)
app$set_inputs(contentScen_2 = "contentScen_2_9")
Sys.sleep(2)
expect_identical(
  getVisibleDtData(app, "tab_2_7-miroPivot-pivotTable"),
  structure(
    list(
      ...1 = c("san-diego", "san-diego", "san-diego", "seattle", "seattle", "test"),
      ...2 = c("chicago", "new-york", "topeka", "chicago", "topeka", "new-york"),
      ...3 = c("1.8", "2.5", "1.4", "1.7", "1.8", "2.5")
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -6L)
  )
)
app$set_inputs(btSave = "click")
Sys.sleep(1)
app$set_inputs(btRemoveOutput = "click")
Sys.sleep(2)
app$run_js("$('.navbar-custom-menu a.dropdown-toggle').get(0).click()")
app$click(selector = ".navbar-custom-menu a[onclick*='btExportScen']")
Sys.sleep(1)
app$set_inputs(exportFileType = "miroscen")
Sys.sleep(2L)
expect_symbols_in_miroscen(app, "scenExportHandler", c(
  "a", "b", "d", "force_unique_sol", "i", "ii", "test", "j", "initial_state",
  "initial_state2", "test123", "test124", "results", "_gmspar_bla"
))
app$click(selector = 'button[data-dismiss="modal"]')
Sys.sleep(1)
app$click(selector = "#sidebarItemExpanded a[data-value='inputData']")
Sys.sleep(0.5)
app$click(selector = ".btSolve .dropdown-toggle")
app$click(selector = ".change-dd-button[data-action-id='btSubmitHcJob']")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
Sys.sleep(1L)
expect_true(app$get_js("$('#hcWidget_2-label').text()==='huuuuiii'"))
expect_options(getSelectizeOptions(app, "#hcWidget_2"), c("0", "1"))
app$stop()
