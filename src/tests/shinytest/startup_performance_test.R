app <- AppDriver$new("../../",
  name = "startup_performance_test",
  variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
app$expect_values(output = "inputDataTitle")
app$stop()
