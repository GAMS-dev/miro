app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("hcube_module_load_test")

context("UI tests - Hypercube module - load")
app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)

# use batch loder module to fetch HC scenarios
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(btSendQuery = "click")
Sys.sleep(2)
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===8", timeout = 50L))
scenData <- getVisibleDtData(app, "batchLoadResults")
for(staticColId in c(1, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16)){
  allEqual <- length(unique(scenData[[staticColId]])) == 1
  expect_true(allEqual)
  if(!allEqual){
    print(scenData[[staticColId]])
  }
}

app$setInputs(newLine_1 = "_sys_metadata_._stag")
Sys.sleep(0.5)
app$setInputs(val_1_1 = "blub")
app$setInputs(btSendQuery = "click")
Sys.sleep(2)
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===5", timeout = 50L))


app$setInputs(newLine_1 = "_scalars._gmsopt_lsttitleleftaligned")
Sys.sleep(0.5)
app$setInputs(val_1_2 = 0L)
app$setInputs(newLine_1 = "_scalars.maxstock")
Sys.sleep(0.5)
app$setInputs(val_1_3 = 4L)
app$setInputs(btSendQuery = "click")
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===1", timeout = 50L))
app$setInputs(batchLoadResults_rows_selected = 1, allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$setInputs(btBatchLoadSb = "click")
Sys.sleep(3L)
app$setInputs(inputTabset = "inputTabset_1")
Sys.sleep(1L)
expect_identical(app$getValue("slider_3"), 4)
app$setInputs(inputTabset1 = "inputTabset1_9")
Sys.sleep(1L)
expect_identical(app$getValue("cb_12"), FALSE)
app$setInputs(inputTabset = "inputTabset_4")
Sys.sleep(1L)
expect_equal(getHotData(app, "in_2"),
             tibble::tibble(j = c("New-york", "Chicago", "Topeka"),
                            latitude = c("40.730610", "41.881832", "39.056198"),
                            longitude = c("-73.935242", "-87.623177", "-95.695312"),
                            text = ""))

# check that attachment was added
app$setInputs(btEditMeta = "click")
Sys.sleep(2L)
app$findElement("a[data-value='Attachments']")$click()
Sys.sleep(1)
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 1L)
app$setInputs(btUpdateMeta = "click")
Sys.sleep(1)

# save as new scenario
app$setInputs(btSave = "click")
Sys.sleep(0.5)

# load scenario from HC job with only 2 scenarios
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(val_1_1 = "woff")
app$setInputs(val_1_3 = 7L)
app$setInputs(op_1_3 = ">")
app$setInputs(btSendQuery = "click")
Sys.sleep(2)
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===1", timeout = 50L))
app$setInputs(batchLoadResults_rows_selected = 1, allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$setInputs(btBatchLoadSb = "click")
Sys.sleep(1L)
app$setInputs(btBatchLoadSbOverwrite = "click")
Sys.sleep(2L)

app$setInputs(inputTabset = "inputTabset_1")
Sys.sleep(1L)
app$setInputs(inputTabset1 = "inputTabset1_1")
expect_identical(app$getValue("slider_3"), 8)
Sys.sleep(1L)
app$setInputs(btSave = "click")
Sys.sleep(0.5)

# load second scenario of job. Saving this one should throw error as same hash already exists
app$findElement("#sidebarItemExpanded a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(op_1_3 = "<")
app$setInputs(btSendQuery = "click")
Sys.sleep(2)
expect_true(app$waitFor("$('#batchLoadResults').data('datatable').data().length===1", timeout = 50L))
app$setInputs(batchLoadResults_rows_selected = 1, allowInputNoBinding_ = TRUE)
app$setInputs(hcubeLoadSelected = "click")
Sys.sleep(2L)
app$setInputs(btBatchLoadSb = "click")
Sys.sleep(1L)
app$setInputs(btBatchLoadSbOverwrite = "click")
Sys.sleep(2L)
expect_identical(app$getValue("slider_3"), 4)

app$setInputs(btSave = "click")
Sys.sleep(2L)
expect_true(app$waitFor("$('.modal-body').text().trim().includes('scenario with the name you have chosen already exists')",
            timeout = 50L))
app$waitFor('$(\'button[data-dismiss="modal"]:visible\').click();true;', timeout = 50)
Sys.sleep(2L)

# Save as new name
app$setInputs(btEditMeta = "click")
Sys.sleep(2L)
app$setInputs(editMetaName = "bliblablub")
app$setInputs(btUpdateMeta = "click")
Sys.sleep(2L)

app$setInputs(btSave = "click")
Sys.sleep(2L)

conn <- connectDb(modelName = "pickstock_configuration")
tryCatch({
  # check that HC job data of second job was removed as all scenarios of that job were saved
  # as standard scenarios
  hcJobData <- DBI::dbGetQuery(conn, paste0("SELECT * FROM ",
                                            DBI::dbQuoteIdentifier(conn, "_sys_metadata_"),
                                            " WHERE ",
                                            DBI::dbQuoteIdentifier(conn, "_scode"), "==-2"))
  expect_identical(nrow(hcJobData), 1L)
}, error = function(e){
  warning(conditionMessage(e), call. = FALSE)
}, finally = {
  DBI::dbDisconnect(conn)
})


app$stop()