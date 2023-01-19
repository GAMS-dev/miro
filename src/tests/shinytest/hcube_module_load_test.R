app <- AppDriver$new("../../", name = "hcube_module_load_test", variant = NULL, load_timeout = 20000)

context("UI tests - Hypercube module - load")

# use batch loader module to fetch HC scenarios
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(btNewBlock = "click")
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===8;", timeout = 5000L), NA)
app$set_inputs(btRemoveBlock2 = "click")
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===8;", timeout = 5000L), NA)
scenData <- getVisibleDtData(app, "batchLoadResults")
for (staticColId in c(1, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16)) {
  allEqual <- length(unique(scenData[[staticColId]])) == 1
  expect_true(allEqual)
  if (!allEqual) {
    print(scenData[[staticColId]])
  }
}

app$set_inputs(newLine_1 = "_sys_metadata_._stag")
Sys.sleep(0.5)
app$set_inputs(val_1_1 = "blub")
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===5;", timeout = 5000L), NA)


app$set_inputs(newLine_1 = "_scalars._gmsopt_lsttitleleftaligned")
Sys.sleep(0.5)
app$set_inputs(val_1_2 = 0L)
app$set_inputs(newLine_1 = "_scalars.maxstock")
Sys.sleep(0.5)
app$set_inputs(val_1_3 = 4L)
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===1;", timeout = 5000L), NA)
app$set_inputs(batchLoadResults_rows_selected = 1, allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$set_inputs(btBatchLoadSb = "click")
Sys.sleep(3L)
app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(1L)
expect_identical(app$get_values()$input[["slider_3"]], 4L)
app$set_inputs(inputTabset1 = "inputTabset1_9")
Sys.sleep(1L)
expect_identical(app$get_values()$input[["cb_12"]], FALSE)
app$set_inputs(inputTabset = "inputTabset_4")
Sys.sleep(1L)
expect_equivalent(getHotData(app, "in_2"),
  tibble::tibble(
    j = c("New-york", "Chicago", "Topeka"),
    latitude = c("40.730610", "41.881832", "39.056198"),
    longitude = c("-73.935242", "-87.623177", "-95.695312"),
    text = ""
  ),
  ignore_attr = TRUE
)

# check that attachment was added
app$set_inputs(btEditMeta = "click")
Sys.sleep(2L)
app$click(selector = "a[data-value='attachments']")
Sys.sleep(1)
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 1L)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)

# save as new scenario
app$set_inputs(btSave = "click")
Sys.sleep(0.5)

# load scenario from HC job with only 2 scenarios
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(val_1_1 = "woff")
app$set_inputs(val_1_3 = 7L)
app$set_inputs(op_1_3 = ">")
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===1;", timeout = 5000L), NA)
app$set_inputs(batchLoadResults_rows_selected = 1, allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$set_inputs(btBatchLoadSb = "click")
Sys.sleep(1L)
app$set_inputs(btBatchLoadSbOverwrite = "click")
Sys.sleep(2L)

app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(1L)
app$set_inputs(inputTabset1 = "inputTabset1_1")
expect_identical(app$get_values()$input[["slider_3"]], 8L)
Sys.sleep(1L)
app$set_inputs(btSave = "click")
Sys.sleep(0.5)

# load second scenario of job. Saving this one should throw error as same hash already exists
app$click(selector = "#sidebarItemExpanded a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(op_1_3 = "<")
app$set_inputs(btSendQuery = "click")
expect_error(app$wait_for_js("$('#batchLoadResults')?.data('datatable')?.data()?.length===1;", timeout = 5000L), NA)
app$set_inputs(batchLoadResults_rows_selected = 1, allow_no_input_binding_ = TRUE)
app$set_inputs(batchLoadSelected = "click")
Sys.sleep(2L)
app$set_inputs(btBatchLoadSb = "click")
Sys.sleep(1L)
app$set_inputs(btBatchLoadSbOverwrite = "click")
Sys.sleep(2L)
expect_identical(app$get_values()$input[["slider_3"]], 4L)

app$set_inputs(btSave = "click")
Sys.sleep(2L)
expect_true(app$get_js("$('.modal-body').text().trim().includes('scenario with the name you have chosen already exists');",
  timeout = 50L
))
app$run_js('$(\'button[data-dismiss="modal"]:visible\').click();', timeout = 50)
Sys.sleep(2L)

# Save as new name
app$set_inputs(btEditMeta = "click")
Sys.sleep(2L)
app$set_inputs(editMetaName = "bliblablub")
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(2L)

app$set_inputs(btSave = "click")
Sys.sleep(2L)

conn <- connectDb(modelName = "pickstock_configuration")
tryCatch(
  {
    # check that HC job data of second job was removed as all scenarios of that job were saved
    # as standard scenarios
    hcJobData <- DBI::dbGetQuery(conn, paste0(
      "SELECT * FROM ",
      DBI::dbQuoteIdentifier(conn, "_sys_metadata_"),
      " WHERE ",
      DBI::dbQuoteIdentifier(conn, "_scode"), "=-2 AND ",
      DBI::dbQuoteIdentifier(conn, "_stag"), "=",
      DBI::dbQuoteString(conn, ",woff,")
    ))
    expect_identical(nrow(hcJobData), 0L)
  },
  error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
  },
  finally = {
    DBI::dbDisconnect(conn)
  }
)


app$stop()
