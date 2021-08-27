app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("bigdata_table")

Sys.sleep(2)
app$setInputs(inputTabset = "inputTabset_1")
Sys.sleep(1)
app$setInputs(in_1_rows_selected = 3, allowInputNoBinding_ = TRUE)
app$setInputs(in_1_remove_row = "click")
Sys.sleep(1)
app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)
app$setInputs(in_1_add_row = "click")
Sys.sleep(1)
app$waitFor("$('#newRow_1')[0].selectize.createItem('2000-01-01');", timeout = 50)
app$setInputs(newRow_20 = "12.3456789123")
app$setInputs(in_1_add_row_confirm = "click")
Sys.sleep(1)
app$setInputs(btGraphIn = "click")
Sys.sleep(1)
app$snapshotDownload("in_1-miroPivot-downloadCsv", "data.csv")
app$stop()
