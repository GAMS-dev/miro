app <- AppDriver$new("../../",
  name = "custom_importer_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_external")
expect_identical(
  getSelectizeOptions(app, "#selExternalSource"),
  list("Importer without file", "Importer with file", "JSON import", "Importer without file (scalars)")
)
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#externalSourceFile_2').is(':hidden')", timeout = 50L), NA)
app$set_inputs(selExternalSource = "Importer with file")
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('#externalSourceFile_2').is(':visible')", timeout = 50L), NA)
expect_error(app$wait_for_js("$('#externalSourceFile_2').attr('multiple')==='multiple'", timeout = 50L), NA)
expect_error(app$wait_for_js("$('#externalSourceFile_2').attr('accept')==='.csv,text/csv'", timeout = 50L), NA)
app$set_inputs(selExternalSource = "Importer without file")
Sys.sleep(0.1)
expect_error(app$wait_for_js("$('#externalSourceFile_2').is(':hidden')", timeout = 50L), NA)
app$set_inputs(btImportExternal = "click")
Sys.sleep(1)
expect_equivalent(getHotData(app, "in_1"), tibble(i = "isBadA", value = 0L))
app$set_inputs(inputTabset = "inputTabset_2")
expect_equivalent(getHotData(app, "in_2"), tibble(j = "isBadB", value = 0L))
expect_identical(app$get_text(selector = "#inputDataTitle"), "<HeyHey> (*)")
app$set_inputs(inputTabset = "inputTabset_5")
expect_identical(app$get_values()$input[["slider_6"]], 20L)
expect_identical(app$get_values()$input[["slider_8"]], 0.11)
app$set_inputs(inputTabset = "inputTabset_2")
app$set_inputs(btEditMeta = "click")
Sys.sleep(2)
expect_identical(app$get_values()$input$editMetaName, "HeyHey")
expect_identical(app$get_values()$input$editMetaTags, c("heyhey", "hoho"))
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_external")
app$set_inputs(selExternalSource = "Importer with file")
app$upload_file(externalSourceFile_2 = "../data/a.csv")
Sys.sleep(0.5)
app$set_inputs(btImportExternal = "click")
Sys.sleep(0.5)
app$set_inputs(btMergeInputData = "click")
Sys.sleep(1)
expect_equivalent(getHotData(app, "in_2"), tibble(j = c("isBadB", "isGoodB"), value = c(0L, 1L)))
app$set_inputs(btEditMeta = "click")
Sys.sleep(2)
app$click(selector = '#editMetaUI a[data-value="attachments"]')
Sys.sleep(0.5)
expect_error(app$wait_for_js("$('.attachment-line a').text().trim()==='test.csv'", timeout = 50L), NA)
expect_error(app$wait_for_js("$('.attachment-line input').is(':checked')===false", timeout = 50L), NA)
app$set_inputs(btUpdateMeta = "click")
Sys.sleep(1)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_external")
app$set_inputs(selExternalSource = "Importer with file")
Sys.sleep(0.5)
app$set_inputs(btImportExternal = "click")
Sys.sleep(0.5)
app$set_inputs(btReplaceInputData = "click")
Sys.sleep(1)
expect_error(app$wait_for_js("$('.modal-body').is(':visible')&&$('.modal-body').text().trim()==='No file provided'"), NA)

app$set_inputs(btImport = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/transport_full.gdx"))
app$set_inputs(btImportLocal = "click")
Sys.sleep(1)
app$set_inputs(btReplaceInputData = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
Sys.sleep(0.5)

app$set_inputs(inputTabset = "inputTabset_5")
app$set_inputs(slider_7 = 23L)

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_external")
app$set_inputs(selExternalSource = "Importer without file (scalars)")
Sys.sleep(0.5)
app$set_inputs(btImportExternal = "click")
Sys.sleep(0.5)
app$set_inputs(btMergeInputData = "click")
expect_identical(app$get_values()$input[["slider_6"]], 30L)
expect_identical(app$get_values()$input[["slider_7"]], 23L)
expect_identical(app$get_values()$input[["slider_8"]], 0.33)
expect_identical(app$get_values()$input[["dropdown_9"]], "minlp")

app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)
app$set_inputs(outputTableView = "click")
Sys.sleep(1)
expect_identical(nrow(getVisibleDtData(app, "table_tab_1_1-datatable")), 6L)
app$click(selector = "a[data-value='inputData']")
app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(0.5)
app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_external")
app$set_inputs(selExternalSource = "JSON import")
Sys.sleep(0.5)
app$upload_file(externalSourceFile_3 = "../data/a.json")
app$set_inputs(btImportExternal = "click")
Sys.sleep(0.5)
app$set_inputs(btReplaceInputData = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_equivalent(getHotData(app, "in_1"), tibble(i = c("Seattle", "Boston"), value = c(123L, 456L)))
expect_match(app$get_js("$('.shiny-notification-content-text:visible').map(function(){return $(this).text();}).get().join(',')"),
  "2 datasets imported from: JSON import",
  fixed = TRUE
)

app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)
app$set_inputs(outputTableView = "click")
Sys.sleep(1)
expect_equivalent(
  getVisibleDtData(app, "table_tab_1_1-datatable"),
  tibble(
    ...1 = "Seattle", ...2 = "New-York",
    ...3 = "100.123", ...4 = "123.456", ...5 = "321.432",
    ...6 = "543.345", ...7 = "1", ...8 = "123", ...9 = "432"
  )
)
app$set_inputs(outputTabset = "outputTabset_2")
app$wait_for_js("$('#tab_1_2-scalarBoxes').text().trim().startsWith('NA')", timeout = 5000L)
app$click(selector = "a[data-value='inputData']")

app$click(selector = "#btRemove1")
app$wait_for_js("($('.modal').data('bs.modal')||{}).isShown===true", timeout = 10000L)
app$run_js("$('.modal .bt-gms-confirm').click()")
app$wait_for_js("($('.modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_equivalent(getHotData(app, "in_1"), tibble(i = "", value = "NA"))

app$set_inputs(btImport = "click")
Sys.sleep(0.5)
app$set_inputs(tb_importData = "tb_importData_external")
app$set_inputs(selExternalSource = "JSON import")
Sys.sleep(0.5)
app$upload_file(externalSourceFile_3 = "../data/a.json")
app$set_inputs(cbSelectManuallyExt = TRUE)
app$set_inputs(selInputDataExt = "a")
app$set_inputs(btImportExternal = "click")
Sys.sleep(0.5)
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true", timeout = 10000L)
expect_equivalent(getHotData(app, "in_1"), tibble(i = c("Seattle", "Boston"), value = c(123L, 456L)))
expect_match(app$get_js("$('.shiny-notification-content-text:visible').map(function(){return $(this).text();}).get().join(',')"),
  "1 datasets imported from: JSON import",
  fixed = TRUE
)
app$click(selector = "a[data-value='outputData']")
app$set_inputs(outputTabset = "outputTabset_1")
Sys.sleep(1)
app$set_inputs(outputTableView = "click")
Sys.sleep(1)
app$wait_for_js("$('#table_tab_1_1-noData').is(':visible');", timeout = 5000L)
expect_true(app$get_js("$('#table_tab_1_1-data').is(':hidden');"))
app$stop()
