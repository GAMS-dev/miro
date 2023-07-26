app <- AppDriver$new("../../",
  name = "input_table_markdown_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

Sys.sleep(2)
app$set_inputs(inputTabset = "inputTabset_1")
Sys.sleep(1)
app$expect_values(output = "inputDataTitle")
expect_true(app$get_js("$('#tableLabel_1').html().includes('<h3>Data:</h3>') && $('#tableLabel_1').html().includes('<p><em>test</em> <strong>asd</strong></p>');", timeout = 50))
app$click(selector = "a[data-value='outputData']")
app$set_inputs(inputTabset = "inputTabset_1")
app$set_inputs(outputTabset = "outputTabset_2")
Sys.sleep(0.5)
expect_true(app$get_js("$('#tableOutLabel_2').is(':visible') && $('#tableOutLabel_2').html().includes('<h1>asd</h1>');"))
expect_true(app$get_js("$('#tab_1_2-data').is(':visible')===true"))
expect_true(app$get_js("$('#scenTable_1_2').is(':visible')===false"))
app$set_inputs(outputTableView = "click")
Sys.sleep(0.5)
expect_true(app$get_js("$('#tableOutLabel_2').is(':visible') && $('#tableOutLabel_2').html().includes('<h1>asd</h1>');"))
expect_true(app$get_js("$('#tab_1_2-data').is(':visible')===false"))
expect_true(app$get_js("$('#scenTable_1_2').is(':visible')===true"))
app$stop()
