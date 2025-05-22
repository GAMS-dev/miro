app <- AppDriver$new("../../",
  name = "miroscenio_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(inputTabset = "inputTabset_7")
Sys.sleep(1)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
scenariosInDb <- getSelectizeAliases(app, "#selLoadScen")
expect_true(any(startsWith(scenariosInDb, "I am a scenario (")))
app$set_inputs(btLoadScenConfirm = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
app$run_js("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,0,'test');")
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$set_inputs(tb_importData = "tb_importData_local")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$upload_file(localInput = paste0("../data/transport.miroscen"))
app$set_inputs(btImportLocal = "click")
expect_true(app$get_js("$('#importDataClearSandbox').is(':visible');"))
expect_true(app$get_js("$('#importDataOverwrite').is(':hidden');"))
expect_true(app$get_js("$('#importDataOverwrite').is(':hidden');"))
expect_true(app$get_js("$('#btReplaceInputData').is(':hidden');"))
expect_true(app$get_js("$('#btMergeInputData').is(':hidden');"))
expect_true(app$get_js("$('#btOverwriteScenLocal').is(':visible');"))
app$set_inputs(btOverwriteScenLocal = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_match(app$get_js("$('.shiny-notification-content-text:visible').map(function(){return $(this).text();}).get().join(',')"),
  "10 datasets imported",
  fixed = TRUE
)
app$expect_values(
  input = paste0("slider_", c("7", "8"))
)
Sys.sleep(0.5)
app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
app$click(selector = '#editMetaUI a[data-value="views"]')
Sys.sleep(1)
expect_identical(app$get_js("$('#currentViewsTable tbody tr').length"), 1L)
app$click(selector = '#editMetaUI a[data-value="attachments"]')
Sys.sleep(1)
attachmentList <- app$get_js("$('.attachment-line').length")
expect_identical(attachmentList, 2L)
expect_true(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]').get(0).checked"))
expect_false(app$get_js("$('.attachment-line > div:nth-child(2) input[type=checkbox]').get(1).checked"))
app$click(selector = '#editMetaUI a[data-value="general"]')
Sys.sleep(1)
expect_identical(app$get_values()$input[["editMetaName"]], "Test Scenario")
expect_identical(app$get_values()$input[["editMetaTags"]], c("asd", "def"))
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)
app$run_js("$('.navbar-custom-menu a.dropdown-toggle').get(0).click();")
app$click(selector = ".navbar-custom-menu a[onclick*='btExportScen']")
Sys.sleep(1)
app$set_inputs(exportFileType = "miroscen")
Sys.sleep(0.5)
expect_true(app$get_js("$('#shiny-modal .gmsalert-error').is(':hidden');"))
expect_true(app$get_js("$('#shiny-modal .choose-input').is(':hidden');"))
expect_files_in_zip(app, "scenExportHandler", c(
  "data.gdx", "metadata.json", "views.json",
  "attachments/", "attachments/scalars.csv",
  "attachments/bad1.miroscen"
))
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$set_inputs(btOverwriteScen = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_equivalent(
  getHotData(app, "in_1"),
  tibble::tibble(i = "", value = "NA")
)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/transport.miroscen"))
app$set_inputs(cbSelectManuallyLoc = "click")
app$set_inputs(selInputDataLoc = c("a", "d"))
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_match(app$get_js("$('.shiny-notification-content-text:visible').map(function(){return $(this).text();}).get().join(',')"),
  "2 datasets imported",
  fixed = TRUE
)
expect_equivalent(
  getHotData(app, "in_1"),
  tibble::tibble(i = c("Seattle", "San-Diego"), value = c(350L, 600L))
)
app$set_inputs(inputTabset = "inputTabset_2")
Sys.sleep(0.5)
expect_equivalent(
  getHotData(app, "in_2"),
  tibble::tibble(j = "", value = "NA")
)
app$set_inputs(inputTabset = "inputTabset_3")
Sys.sleep(0.5)
expect_equivalent(
  getHotData(app, "in_3"),
  tibble::tibble(
    i = c("New-york", "Chicago", "Topeka"),
    Seattle = c(2.5, 1.7, 1.8), `San-Diego` = c(2.5, 1.8, 1.4)
  )
)
# metadata (scenario name) should not be updated when importing manually
# selected datasets
expect_identical(
  iconv(as.character(app$get_values()[["output"]][["inputDataTitle"]][["html"]])),
  "I am a scenario (*)"
)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/transport.miroscen"))
app$set_inputs(btImport = "click")
app$set_inputs(btOverwriteScenLocal = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_identical(
  iconv(as.character(app$get_values()[["output"]][["inputDataTitle"]][["html"]])),
  "<i>&lt;Test Scenario&gt; (*)</i>"
)
expect_identical(app$get_js("$('#dirtyFlagIcon').is(':visible')===false;", timeout = 50L), TRUE)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/transport.miroscen"))
app$set_inputs(cbSelectManuallyLoc = "click")
app$set_inputs(selInputDataLoc = c("a", "d"))
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
# when replacing data in scenario with output data, dirty flag should be visible
expect_identical(app$get_js("$('#dirtyFlagIcon').is(':visible')===true;", timeout = 50L), TRUE)
app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/transport.miroscen"))
app$set_inputs(cbSelectManuallyLoc = "click")
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
app$wait_for_js("$('.shiny-notification-content-text:visible').is(':visible');", timeout = 5000L)
expect_match(app$get_js("$('.shiny-notification-content-text:visible').map(function(){return $(this).text();}).get().join(',')"),
  "No dataset was imported.",
  fixed = TRUE
)
app$stop()
