app <- AppDriver$new("../../",
  name = "batch_scripts_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(btImport = "click")
Sys.sleep(1)
expect_error(app$wait_for_js("$('#tb_importData').is(':visible')", timeout = 2000), NA)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/transport2.gdx"))
app$set_inputs(btImportLocal = "click")
app$set_inputs(btSaveAs = "click")
Sys.sleep(1)
app$set_inputs(scenName = "New Scenario2")
Sys.sleep(1)
app$click(selector = "#shiny-modal #dialogSaveInit .bt-gms-confirm")
Sys.sleep(1)
app$click(selector = "a[data-value='loadResults']")
Sys.sleep(0.5)
app$set_inputs(btSendQuery = "click")
app$set_inputs(batchLoadAll = "click")
Sys.sleep(1)
expect_error(app$wait_for_js("$('#btAnalysisConfig').is(':visible')", timeout = 2000), NA)
app$set_inputs(btAnalysisConfig = "click")
expect_identical(getSelectizeAliases(app, "#selHcubeAnalysisScript"), c("bla123"))
expect_error(app$wait_for_js("$('#btRunHcubeScript').is(':visible')", timeout = 2000), NA)
app$set_inputs(btRunHcubeScript = "click")
expect_error(app$wait_for_js("$('#scriptOutput_hcube .script-output').contents().find('body').html().replace(/\\s/g,'')==='<h1>Totaltransportationcost:NA</h1><p><strong>Totalfreightcost:189.00</strong></p>'",
  timeout = 10000
), NA)
app$stop()
