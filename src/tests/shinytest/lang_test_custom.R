app <- AppDriver$new("../../",
  name = paste0("lang_test_", Sys.getenv("MIRO_LANG")), variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2L)
app$expect_values(input = paste0("slider_", c(2L, 2L + 1L)))

expect_identical(
  iconv(as.character(app$get_values()[["output"]][["inputDataTitle"]][["html"]])),
  "<i>&lt;customScenarioName&gt;</i>"
)
app$stop()
