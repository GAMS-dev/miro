app <- AppDriver$new("../../",
  name = "bigdata_table", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

Sys.sleep(2)
app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(1)
app$set_inputs(in_1_rows_selected = 3, allow_no_input_binding_ = TRUE)
app$set_inputs(
  in_1_cell_edit = list(row = 1, col = 10, value = "1.23456789"),
  allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE
)
app$set_inputs(
  in_1_cell_edit = list(row = 2, col = 10, value = "2.3456789"),
  allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE
)
Sys.sleep(1)
app$set_inputs(in_1_remove_row = "click")
Sys.sleep(1)
app$expect_values(output = "inputDataTitle")
app$set_inputs(in_1_add_row = "click")
Sys.sleep(1)
app$run_js("$('#newRow_1')[0].selectize.createItem('2000-01-01');")
app$set_inputs(newRow_20 = "12.3456789123")
app$set_inputs(in_1_add_row_confirm = "click")
Sys.sleep(1)
app$set_inputs(btGraphIn = "click")
Sys.sleep(1)
app$expect_download("in_1-miroPivot-downloadCsv", name = "data.csv")
app$stop()
