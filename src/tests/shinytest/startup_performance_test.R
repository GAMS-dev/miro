app <- AppDriver$new("../../", name = "startup_performance_test", variant = NULL, load_timeout = 20000)
app$expect_values(output = "inputDataTitle")
app$stop()
