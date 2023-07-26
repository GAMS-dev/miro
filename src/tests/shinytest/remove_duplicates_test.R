app <- AppDriver$new("../../",
  name = "remove_duplicates_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
app$set_inputs(btRemoveDuplicates = "click")
expect_error(app$wait_for_js("$('.shiny-notification-content').text().includes('None of your input tables contain duplicate records');", timeout = 10000L), NA)

app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)

app$set_inputs(btImport = "click")
Sys.sleep(1)
app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE)
expect_error(app$wait_for_js("$('#localInput.shiny-bound-input').is(':visible');", timeout = 2000L), NA)
app$upload_file(localInput = "../data/transport_duplicates.xlsx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
Sys.sleep(1)

app$set_inputs(btRemoveDuplicates = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true;", timeout = 5000L)
expect_error(app$wait_for_js("$('.modal-body div:visible').text().includes('duplicate records: \\'Capacity\\', \\'Demand\\', \\'Distance\\'.')", timeout = 10000L), NA)
app$set_inputs(btRemoveDuplicatesKeepFirst = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true;", timeout = 5000L)
app$set_inputs(inputTabset = "inputTabset_2")
expect_equivalent(
  getHotData(app, "in_1"),
  tibble(i = c("Seattle", "San-Diego", "Hannover"), value = c(350, 600, 20))
)
app$set_inputs(inputTabset = "inputTabset_3")
expect_equivalent(
  getHotData(app, "in_2"),
  tibble(i = c("New-york", "Chicago", "Topeka"), value = c(325, 300, 275))
)
app$set_inputs(inputTabset = "inputTabset_4")
expect_equivalent(
  getHotData(app, "in_3"),
  tibble(j = c("New-york", "Chicago", "Topeka"), Seattle = c(2.5, 1.7, 1.8), `San-Diego` = c(2.5, 1.8, 1.4))
)

app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)

app$set_inputs(btImport = "click")
Sys.sleep(1)
app$set_inputs(tb_importData = "tb_importData_local", wait_ = FALSE)
expect_error(app$wait_for_js("$('#localInput.shiny-bound-input').is(':visible');", timeout = 2000L), NA)
app$upload_file(localInput = "../data/transport_duplicates.xlsx")
app$set_inputs(btImportLocal = "click")
app$set_inputs(btReplaceInputData = "click")
Sys.sleep(1)

app$set_inputs(btRemoveDuplicates = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown===true;", timeout = 5000L)
expect_error(app$wait_for_js("$('.modal-body div:visible').text().includes('duplicate records: \\'Capacity\\', \\'Demand\\', \\'Distance\\'.')", timeout = 10000L), NA)
app$set_inputs(btRemoveDuplicatesKeepLast = "click")
app$wait_for_js("($('#shiny-modal').data('bs.modal')||{}).isShown!==true;", timeout = 5000L)
app$set_inputs(inputTabset = "inputTabset_2")
expect_equivalent(
  getHotData(app, "in_1"),
  tibble(i = c("Seattle", "San-Diego", "Hannover"), value = c(20, 10, 300))
)
app$set_inputs(inputTabset = "inputTabset_3")
expect_equivalent(
  getHotData(app, "in_2"),
  tibble(i = c("New-york", "Topeka", "Chicago"), value = c(325, 275, 20))
)
app$set_inputs(inputTabset = "inputTabset_4")
expect_equivalent(
  getHotData(app, "in_3"),
  tibble(j = c("New-york", "Chicago", "Topeka"), Seattle = c(0.1, 1.1, 1.8), `San-Diego` = c(2.5, 1.8, 1.4))
)

app$stop()
