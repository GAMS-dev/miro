app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("skip_scen_import_test")

app$setInputs(btImport = "click")
Sys.sleep(1)
app$snapshot(
  items = list(input = "tb_importData"),
  screenshot = TRUE
)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
# remove scenario
app$setInputs(btDelete = "click")
Sys.sleep(0.5)
app$findElement("#btDeleteConfirm")$click()
Sys.sleep(1)
app$stop()

# launch again. Import of data should be skipped now
app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$setInputs(btImport = "click")
Sys.sleep(1)
expect_identical(app$getValue("tb_importData"), if (identical(Sys.getenv("MIRO_FORCE_SCEN_IMPORT"), "true")) {
  "tb_importData_remote"
} else {
  "tb_importData_local"
})
app$stop()
