app <- AppDriver$new("../../",
  name = "symbol_order_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)

# input symbol order
expect_true(app$get_js("$('a[data-value=\"inputTabset_1\"]').text()==='Scalars';", timeout = 50))
expect_true(app$get_js("$('a[data-value=\"inputTabset_2\"]').text()==='Advanced options';", timeout = 50))
expect_true(app$get_js("$('a[data-value=\"inputTabset_3\"]').text()==='Price';", timeout = 50))
expect_true(app$get_js("$('a[data-value=\"inputTabset_4\"]').text()==='Input widgets';", timeout = 50))
app$expect_values(output = "inputDataTitle")
app$click(selector = 'a[data-value="outputData"]')
Sys.sleep(1L)
app$click(selector = 'a[data-value="outputTabset_2"]')
expect_error(app$wait_for_js("$('a[data-value=\"outputTabset_1\"]').text()==='Output Scalars';", timeout = 5000L), NA)
expect_true(app$get_js("$('a[data-value=\"outputTabset_2\"]').text()==='Dow Jones vs. Index Fund';", timeout = 50))
expect_true(app$get_js("$('a[data-value=\"outputTabset_3\"]').text()==='weight';", timeout = 50))
expect_true(app$get_js("$('a[data-value=\"outputTabset_4\"]').text()==='Price (stocks & dow jones)';", timeout = 50))
expect_true(app$get_js("$('a[data-value=\"outputTabset_2_1\"]').text()==='dow jones vs. index fund';", timeout = 50))
expect_true(app$get_js("$('a[data-value=\"outputTabset_2_2\"]').text()==='absolute error';", timeout = 50))
app$stop()
