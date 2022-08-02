app <- AppDriver$new("../../", name = "metadata_no_scalars_test", variant = NULL, load_timeout = 20000)

app$set_inputs(btEditMeta = "click")
Sys.sleep(1)
expect_error(app$click(selector = "a[data-value='views']"), NA)
Sys.sleep(0.1)

app$expect_values(output = c("inputDataTitle"))

app$stop()
