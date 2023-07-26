app <- AppDriver$new("../../",
  name = "metadata_no_scalars_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
expect_error(app$click(selector = "a[data-value='views']"), NA)
Sys.sleep(0.1)

app$expect_values(output = c("inputDataTitle"))

app$stop()
