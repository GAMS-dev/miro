app <- AppDriver$new("../../", name = paste0("lang_test_", Sys.getenv("MIRO_LANG")), variant = NULL, load_timeout = 20000)
Sys.sleep(2L)
app$expect_values(input = paste0("slider_", c(2L, 2L + 1L)))

expect_identical(
  iconv(as.character(app$get_values()[["output"]][["inputDataTitle"]][["html"]])),
  if (Sys.getenv("MIRO_LANG") == "en") {
    "<i>&lt;New Scenario&gt;</i>"
  } else if (Sys.getenv("MIRO_LANG") == "de") {
    "<i>&lt;Neues Szenario&gt;</i>"
  } else if (Sys.getenv("MIRO_LANG") == "cn") "<i>&lt;新建场景&gt;</i>"
)
app$stop()
