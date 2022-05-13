app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("miroscenio_test")

app$setInputs(inputTabset = "inputTabset_7")
Sys.sleep(1)
app$snapshot(
  items = list(input = paste0("slider_", c("7", "8"))),
  screenshot = TRUE
)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
scenariosInDb <- getSelectizeAliases(app, "#selLoadScen")
expect_true(any(startsWith(scenariosInDb, "I am a scenario (")))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = paste0("../data/transport.miroscen"))
app$setInputs(btImportLocal = "click")
expect_true(app$waitFor("$('#importDataClearSandbox').is(':visible');", 50))
expect_true(app$waitFor("$('#importDataOverwrite').is(':hidden');", 50))
expect_true(app$waitFor("$('#importDataOverwrite').is(':hidden');", 50))
expect_true(app$waitFor("$('#btReplaceInputData').is(':hidden');", 50))
expect_true(app$waitFor("$('#btMergeInputData').is(':hidden');", 50))
expect_true(app$waitFor("$('#btOverwriteScenLocal').is(':visible');", 50))
app$setInputs(btOverwriteScenLocal = "click")
app$setInputs(inputTabset = "inputTabset_7")
Sys.sleep(1)
app$snapshot(
  items = list(input = paste0("slider_", c("7", "8"))),
  screenshot = TRUE
)
Sys.sleep(0.5)
app$setInputs(btEditMeta = "click")
Sys.sleep(1)
app$findElement('#editMetaUI a[data-value="views"]')$click()
Sys.sleep(1)
expect_identical(length(app$findElements("#currentViewsTable tbody tr")), 1L)
app$findElement('#editMetaUI a[data-value="attachments"]')$click()
Sys.sleep(1)
attachmentList <- app$findElements(".attachment-line")
expect_identical(length(attachmentList), 2L)
expect_identical(attachmentList[[1]]$findElement(".checkbox input")$getAttribute("checked"), "true")
expect_identical(attachmentList[[2]]$findElement(".checkbox input")$getAttribute("checked"), NULL)
app$findElement('#editMetaUI a[data-value="general"]')$click()
Sys.sleep(1)
expect_identical(app$getValue("editMetaName"), "Test Scenario")
expect_identical(app$getValue("editMetaTags"), list("asd", "def"))
app$setInputs(btUpdateMeta = "click")
Sys.sleep(1)
app$findElements(".navbar-custom-menu a.dropdown-toggle")[[1]]$click()
app$findElement(".navbar-custom-menu a[onclick*='btExportScen']")$click()
Sys.sleep(1)
app$setInputs(exportFileType = "miroscen")
Sys.sleep(0.5)
expect_true(app$waitFor("$('#shiny-modal .gmsalert-error').is(':hidden');", 50))
expect_true(app$waitFor("$('#shiny-modal .choose-input').is(':hidden');", 50))
expect_files_in_zip(app, "scenExportHandler", c(
  "data.gdx", "metadata.json", "views.json",
  "attachments/", "attachments/scalars.csv",
  "attachments/bad1.miroscen"
))
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$setInputs(btOverwriteScen = "click")
Sys.sleep(1)
expect_equivalent(
  getHotData(app, "in_1"),
  tibble::tibble(i = "", value = "NA")
)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$uploadFile(localInput = paste0("../data/transport.miroscen"))
app$setInputs(cbSelectManuallyLoc = "click")
app$setInputs(selInputDataLoc = c("a", "d"))
app$setInputs(btImportLocal = "click")
app$setInputs(btReplaceInputData = "click")
Sys.sleep(0.5)
expect_equivalent(
  getHotData(app, "in_1"),
  tibble::tibble(i = c("Seattle", "San-Diego"), value = c(350L, 600L))
)
app$setInputs(inputTabset = "inputTabset_2")
Sys.sleep(0.5)
expect_equivalent(
  getHotData(app, "in_2"),
  tibble::tibble(j = "", value = "NA")
)
app$setInputs(inputTabset = "inputTabset_3")
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
  iconv(as.character(app$getAllValues()[["output"]][["inputDataTitle"]][["html"]])),
  "I am a scenario (*)"
)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$uploadFile(localInput = paste0("../data/transport.miroscen"))
app$setInputs(btImport = "click")
app$setInputs(btOverwriteScenLocal = "click")
Sys.sleep(1)
expect_identical(
  iconv(as.character(app$getAllValues()[["output"]][["inputDataTitle"]][["html"]])),
  "<i>&lt;Test Scenario&gt; (*)</i>"
)
expect_identical(app$waitFor("$('#dirtyFlagIcon').is(':visible')===false;", timeout = 50L), TRUE)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$uploadFile(localInput = paste0("../data/transport.miroscen"))
app$setInputs(cbSelectManuallyLoc = "click")
app$setInputs(selInputDataLoc = c("a", "d"))
app$setInputs(btImportLocal = "click")
app$setInputs(btReplaceInputData = "click")
Sys.sleep(1)
# when replacing data in scenario with output data, dirty flag should be visible
expect_identical(app$waitFor("$('#dirtyFlagIcon').is(':visible')===true;", timeout = 50L), TRUE)
app$stop()
