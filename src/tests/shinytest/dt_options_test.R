app <- AppDriver$new("../../",
  name = "dt_options_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2)

app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)
expect_true(app$get_js("$('#tab_1_1-datatable .DTFC_LeftWrapper').is(':visible')===true && $('#tab_1_1-datatable .dataTables_paginate').is(':visible')===false"))

app$stop()
