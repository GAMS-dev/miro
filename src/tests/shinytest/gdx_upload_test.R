app <- AppDriver$new("../../",
  name = paste0("gdx_upload_test_", Sys.getenv("GMSMODELNAME")), variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

widgetSheetId <- 1L
if (identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  widgetSheetId <- 2L
} else if (identical(Sys.getenv("GMSMODELNAME"), "transport")) {
  widgetSheetId <- 7L
}
app$set_inputs(inputTabset = paste0("inputTabset_", widgetSheetId))
if (!identical(Sys.getenv("GMSMODELNAME"), "pickstock")) {
  Sys.sleep(1)
  app$expect_values(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L)))
}
app$set_inputs(btImport = "click")
app$set_inputs(tb_importData = "tb_importData_local")
app$upload_file(localInput = paste0("../data/", Sys.getenv("GMSMODELNAME"), ".gdx"))
app$set_inputs(btImportLocal = "click")
app$set_inputs(inputTabset = paste0("inputTabset_", widgetSheetId))
Sys.sleep(1)
app$expect_values(input = paste0("slider_", c(widgetSheetId, widgetSheetId + 1L)))
app$stop()
