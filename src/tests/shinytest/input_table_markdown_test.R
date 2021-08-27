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
app$stop()
