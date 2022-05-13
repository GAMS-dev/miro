app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("input_table_markdown_test")

Sys.sleep(2)
app$setInputs(inputTabset = "inputTabset_1")
Sys.sleep(1)
app$snapshot(
  items = list(output = "inputDataTitle"),
  screenshot = TRUE
)
expect_true(app$waitFor("$('#tableLabel_1').html().includes('<h3>Data:</h3>') && $('#tableLabel_1').html().includes('<p><em>test</em> <strong>asd</strong></p>');", timeout = 50))
app$findElement("a[data-value='outputData']")$click()
app$setInputs(inputTabset = "inputTabset_1")
app$setInputs(outputTabset = "outputTabset_2")
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tableOutLabel_2').is(':visible') && $('#tableOutLabel_2').html().includes('<h1>asd</h1>');", timeout = 50))
expect_true(app$waitFor("$('#tab_1_2-data').is(':visible')===true", timeout = 50L))
expect_true(app$waitFor("$('#scenTable_1_2').is(':visible')===false", timeout = 50L))
app$setInputs(outputTableView = "click")
Sys.sleep(0.5)
expect_true(app$waitFor("$('#tableOutLabel_2').is(':visible') && $('#tableOutLabel_2').html().includes('<h1>asd</h1>');", timeout = 50))
expect_true(app$waitFor("$('#tab_1_2-data').is(':visible')===false", timeout = 50L))
expect_true(app$waitFor("$('#scenTable_1_2').is(':visible')===true", timeout = 50L))
app$stop()
