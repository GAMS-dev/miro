app <- AppDriver$new("../../", name = "comparison_mode_test", variant = NULL, load_timeout = 20000)

currentUser <- Sys.info()[["user"]]

# test compare mode in tab view
app$click(selector = "a[data-value='scenarios']")
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='tab']")
Sys.sleep(1)
app$expect_values(output = c("cmpScenTitle_4", "cmpScenTitle_5"))
app$click(selector = "#cmpTabNoScenWrapper .action-button")
Sys.sleep(1)
scenToSelect <- paste0(c("1_", "2_"), currentUser)
app$set_inputs(selLoadScen = scenToSelect, wait_ = FALSE, values_ = FALSE)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(3)
app$click(selector = "#btCompareScen")
Sys.sleep(0.2)
app$click(selector = "a[data-value='contentScen_5_2']")
app$click(selector = "a[data-value='scen_4_']")
expect_error(app$wait_for_js("$('.tab-content div[data-value=\"contentScen_4_2\"]').is(':visible')", timeout = 5000L), NA)
app$click(selector = "a[data-value='contentScen_4_5']")
app$click(selector = "a[data-value='scen_5_']")
expect_error(app$wait_for_js("$('.tab-content div[data-value=\"contentScen_5_5\"]').is(':visible')", timeout = 5000L), NA)

# test compare mode in split view
app$click(selector = ".btSplitView button")
app$click(selector = ".btSplitView a[data-view='split']")
app$run_js("$('.scenSplit-button-load').get(1).click();")
Sys.sleep(2)
app$set_inputs(btScenSplit2_open = "click")
Sys.sleep(1)
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$click(selector = "#btCompareScen")
Sys.sleep(0.5)
app$click(selector = "a[data-value='contentScen_2_2']")
Sys.sleep(1)
expect_error(app$wait_for_js("$('.tab-content div[data-value=\"contentScen_3_2\"]').is(':visible')", timeout = 2000), NA)
app$click(selector = "a[data-value='contentScen_3_5']")
Sys.sleep(1)
expect_error(app$wait_for_js("$('.tab-content div[data-value=\"contentScen_2_5\"]').is(':visible')", timeout = 50), NA)

# test download of files
app$run_js("$('.scen-buttons-wrapper button').get(1).click()")
Sys.sleep(1)
app$set_inputs(exportFileType = "xlsx", cbSelectManuallyExp = "true")
Sys.sleep(0.5)
app$set_inputs(selDataToExport = c("_scalars", "abserror"))
expect_sheets_in_xls(app, "scenExportHandler", c(" Info", "abserror (Output)", "_scalars (Input)", "_index"))
Sys.sleep(1)

app$run_js("$('.scen-buttons-wrapper button').get(1).click()")
Sys.sleep(1)
app$set_inputs(exportFileType = "xlsx")
expect_sheets_in_xls(app, "scenExportHandler", c(" Info", "_scalars_out (Output)", "stock_weight (Output)", "dowvsindex (Output)", "abserror (Output)", "pricemerge (Output)", "_scalars (Input)", "price (Input)", "_index"))
Sys.sleep(1)

app$run_js("$('.scen-buttons-wrapper button').get(4).click()")
Sys.sleep(1)
app$set_inputs(exportFileType = "gdx", cbSelectManuallyExp = "true")
Sys.sleep(0.5)
app$set_inputs(selDataToExport = c("_scalars_out", "dowvsindex"))
Sys.sleep(1)
expect_symbols_in_gdx(app, "scenExportHandler", c(
  "dowvsindex", "error_ratio", "error_train", "error_test",
  "firstdaytraining", "lastdaytraining"
))
Sys.sleep(1)
app$run_js("$('.scen-buttons-wrapper button').get(1).click()")
Sys.sleep(1)
app$set_inputs(exportFileType = "gdx")
expect_symbols_in_gdx(app, "scenExportHandler", c(
  "dowvsindex", "error_ratio", "error_train", "error_test",
  "firstdaytraining", "lastdaytraining", "stock_weight",
  "abserror", "pricemerge", "price", "maxstock", "trainingdays"
))
Sys.sleep(1)

app$run_js("$('.scen-buttons-wrapper button').get(1).click()")
Sys.sleep(1)
app$set_inputs(exportFileType = "miroscen")
expect_error(app$wait_for_js("$('#cbSelectManuallyExp').is(':hidden')", timeout = 50), NA)
Sys.sleep(0.5)
expect_download_size(app, "scenExportHandler", "split_comp_download.miroscen")
Sys.sleep(1)

app$stop()
