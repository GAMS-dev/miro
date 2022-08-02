app <- AppDriver$new("../../", name = "skip_scen_import_test", variant = NULL, load_timeout = 20000)

app$set_inputs(btImport = "click")
Sys.sleep(1)
app$expect_values(input = "tb_importData")
app$set_inputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# remove scenario
app$set_inputs(btDelete = "click")
Sys.sleep(0.5)
app$click(selector = "#btDeleteConfirm")
Sys.sleep(1)
app$stop()

# launch again. Import of data should be skipped now
app <- AppDriver$new("../../", variant = NULL, load_timeout = 20000)
app$set_inputs(btImport = "click")
Sys.sleep(1)
expect_identical(app$get_values()$input[["tb_importData"]], if (identical(Sys.getenv("MIRO_FORCE_SCEN_IMPORT"), "true")) {
  "tb_importData_remote"
} else {
  "tb_importData_local"
})
app$stop()
