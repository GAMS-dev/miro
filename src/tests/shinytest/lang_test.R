app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("lang_test_", Sys.getenv("MIRO_LANG")))
app$snapshot(items = list(input = paste0("slider_", c(2L, 2L + 1L))),
             screenshot = TRUE)
expect_identical(iconv(as.character(app$getAllValues()[['output']][["inputDataTitle"]][["html"]])),
                 if(Sys.getenv("MIRO_LANG") == "en") "<i>&lt;New Scenario&gt;</i>"
                 else if(Sys.getenv("MIRO_LANG") == "de") "<i>&lt;Neues Szenario&gt;</i>"
                 else if(Sys.getenv("MIRO_LANG") == "cn") "<i>&lt;新建场景&gt;</i>")
app$stop()
