app <- AppDriver$new("../../", name = "download_temp_files_test", variant = NULL, load_timeout = 20000)

app$click(selector = "a[data-value='outputData']")
Sys.sleep(1)

app$set_inputs(btDownloadTmpFiles = "click")
Sys.sleep(1)
expect_true(app$get_js("$('#btDownloadTmpConfirm').is(':visible')===false;", timeout = 50))
expect_identical(
  getSelectizeOptions(app, "#selectDownloadTmp"),
  list("dowjones2016.csv", "pickstock_output_tables.gms")
)
expect_files_in_zip(app, "btDownloadTmpZip", c("pickstock_output_tables.gms", "dowjones2016.csv"))
app$set_inputs(selectDownloadTmp = "pickstock_output_tables.gms")
expect_true(app$get_js("$('#btDownloadTmpConfirm').is(':visible');", timeout = 50))
content <- get_downloaded_file_content(app, "btDownloadTmpConfirm")
expect_true(startsWith(content, "$title Stock Selection"))
app$set_inputs(selectDownloadTmp = "dowjones2016.csv")
content <- get_downloaded_file_content(app, "btDownloadTmpConfirm")
expect_true(startsWith(content, paste0("date,symbol,price", if (.Platform$OS.type == "windows") "\r\n" else "\n")))

# select multiple files should result in zip
app$set_inputs(selectDownloadTmp = c("pickstock_output_tables.gms", "dowjones2016.csv"))
expect_files_in_zip(app, "btDownloadTmpConfirm", c("pickstock_output_tables.gms", "dowjones2016.csv"))
# Make sure doing funny stuff is not possible here..
app$set_inputs(selectDownloadTmp = "asd.csv")
content <- get_downloaded_file_content(app, "btDownloadTmpConfirm")
expect_true(identical(content, paste0("Invalid  filename", if (.Platform$OS.type == "windows") "\r\n" else "\n")))
app$stop()
