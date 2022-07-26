app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("readonly_mode_test")
app$snapshot(items = list(output = "outputDataTitle"), screenshot = TRUE)

expect_true(app$waitFor("$('a[data-value=\"gamsinter\"]').is(':visible')===false", timeout = 50))
expect_true(app$waitFor("$('#btSolve').is(':visible')===false", timeout = 50))
expect_true(app$waitFor("Shiny.setInputValue('btDelete',1)==null", timeout = 50))
Sys.sleep(1)
expect_true(app$waitFor("$('.modal-body').is(':visible')===false", timeout = 50))

expect_true(app$waitFor("Shiny.setInputValue('btSaveAs',1)==null", timeout = 50))
Sys.sleep(1)
expect_true(app$waitFor("$('.modal-body').is(':visible')===false", timeout = 50))
app$stop()
