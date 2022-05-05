app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("custom_analysis_test")

app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)

app$findElement("a[data-value='loadResults']")$click()
Sys.sleep(0.5)
app$setInputs(btSendQuery = "click")
app$setInputs(batchLoadAll = "click")
# first custom analysis
app$waitFor("$('#btBatchCompare+.dropdown-toggle').click()&&$('#btBatchCompare~.dropdown-menu a:eq(2)').text()==='My first custom analysis renderer'", timeout = 50L)
app$waitFor("$('#btBatchCompare+.dropdown-toggle').click()&&$('#btBatchCompare~.dropdown-menu a:eq(3)').text()==='My second custom analyzzzer'", timeout = 50L)
app$waitFor("$('#btBatchCompare+.dropdown-toggle').click()&&$('#btBatchCompare~.dropdown-menu a:eq(2)').click();", timeout = 50L)
Sys.sleep(2L)
expect_true(grepl("<tdalign=\"right\">4.00</td><tdalign=\"right\">0.80</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">0.80</td>",
  gsub(" ", "", app$getAllValues()$output[["cmpCustom_test1-maxstockVsErrorTrainTable"]], fixed = TRUE),
  fixed = TRUE
))
expect_true(grepl("<tdalign=\"right\">4.00</td><tdalign=\"right\">9.51</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">9.51</td>",
  gsub(" ", "", app$getAllValues()$output[["cmpCustom_test1-maxstockVsErrorTestTable"]], fixed = TRUE),
  fixed = TRUE
))

app$findElement("a[data-value='inputData']")$click()
Sys.sleep(0.5)
app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$setInputs(tb_importData = "tb_importData_local")
app$uploadFile(localInput = paste0("../data/pickstock_custom_analysis.miroscen"))
app$setInputs(btImportLocal = "click")
app$setInputs(btReplaceInputData = "click")
Sys.sleep(1)
app$findElement("a[data-value='scenarios']")$click()
app$findElement("#btRefreshCustomCmp_1")$click()
Sys.sleep(2)
expect_true(grepl("<tdalign=\"right\">8.00</td><tdalign=\"right\">0.12</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">0.80</td>",
  gsub(" ", "", app$getAllValues()$output[["cmpCustom_test1-maxstockVsErrorTrainTable"]], fixed = TRUE),
  fixed = TRUE
))
expect_true(grepl("<tdalign=\"right\">8.00</td><tdalign=\"right\">1.68</td></tr>\n<tr><tdalign=\"right\">4.00</td><tdalign=\"right\">9.51</td>",
  gsub(" ", "", app$getAllValues()$output[["cmpCustom_test1-maxstockVsErrorTestTable"]], fixed = TRUE),
  fixed = TRUE
))
app$waitFor("$(\"button[onclick*='btCloseScenCmp']:visible\").click();", timeout = 50L)
Sys.sleep(0.5)
expect_true(app$waitFor("$('#cmpCustomNoScenWrapper_1').is(':visible');", timeout = 50L))
app$waitFor("$('#cmpCustomNoScenWrapper_1 button').click();", timeout = 50L)
Sys.sleep(0.5)
expect_true(app$waitFor("$('#btSendQuery').is(':visible');", timeout = 50L))
app$stop()
