app <- AppDriver$new("../../",
  name = "miropivot_inf_test.R", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(inputTabset = "inputTabset_2")
app$wait_for_js("HTMLWidgets.getInstance(document.getElementById('in_1'))?.hot!=null", timeout = 10000L)
app$run_js("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,1,'Inf');")
app$set_inputs(btGraphIn = "click")
app$wait_for_js("$('#in_1-miroPivot-pivotTable td').length===1;", timeout = 10000L)
expect_identical(
  getVisibleDtData(app, "in_1-miroPivot-pivotTable"),
  structure(
    list(
      ...1 = Inf
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -1L)
  )
)
app$stop()
