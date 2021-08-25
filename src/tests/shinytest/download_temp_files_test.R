app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("download_temp_files_test")

app$snapshot(items = list(output = "outputDataTitle"),
             screenshot = TRUE)

app$findElement("a[data-value='outputData']")$click()
Sys.sleep(1)

app$setInputs(btDownloadTmpFiles = "click")
Sys.sleep(1)
expect_true(app$waitFor("$('#btDownloadTmpConfirm').is(':visible')===false;", timeout = 50))
expect_identical(getSelectizeOptions(app, "#selectDownloadTmp"),
                 c("pickstock_output_tables.gms", "dowjones2016.csv"))
expect_files_in_zip(app, "btDownloadTmpZip", c("pickstock_output_tables.gms", "dowjones2016.csv"))
app$setInputs(selectDownloadTmp = "pickstock_output_tables.gms")
expect_true(app$waitFor("$('#btDownloadTmpConfirm').is(':visible');", timeout = 50))
content <- get_downloaded_file_content(app, "btDownloadTmpConfirm")
expect_true(startsWith(content, "$title Stock Selection"))
app$setInputs(selectDownloadTmp = "dowjones2016.csv")
content <- get_downloaded_file_content(app, "btDownloadTmpConfirm")
expect_true(startsWith(content, paste0("date,symbol,price", if(.Platform$OS.type == 'windows') "\r\n" else "\n")))

# select multiple files should result in zip
app$setInputs(selectDownloadTmp = c("pickstock_output_tables.gms", "dowjones2016.csv"))
expect_files_in_zip(app, "btDownloadTmpConfirm", c("pickstock_output_tables.gms", "dowjones2016.csv"))
# Make sure doing funny stuff is not possible here..
app$setInputs(selectDownloadTmp = "asd.csv")
content <- get_downloaded_file_content(app, "btDownloadTmpConfirm")
expect_true(identical(content, paste0("Invalid  filename", if(.Platform$OS.type == 'windows') "\r\n" else "\n")))
app$stop()
