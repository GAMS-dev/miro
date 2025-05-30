app <- AppDriver$new("../../",
  name = "widget_dependencies_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2L)
allStocks <- c(
  "AAPL", "AXP", "BA", "CAT", "CSCO", "CVX", "DD", "DIS", "GE",
  "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM",
  "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "V",
  "VZ", "WMT", "XOM"
)

# check stock selection dropdown and select JPM stock
app$click(selector = 'a[data-value="inputTabset_2"]')
Sys.sleep(1L)
expect_options(unlist(getSelectizeOptions(app, "#dropdown_4")) %>% sort(), allStocks)
expect_identical(app$get_js("$('#slider_2').parent().find('.irs-max').text()"), "30")
expect_identical(app$get_js("$('#slider_3').parent().find('.irs-max').text()"), "252")
expect_identical(app$get_value(input = "slider_2"), 8L)
expect_identical(app$get_value(input = "slider_3"), 102L)
expect_identical(app$get_value(input = "dropdown_4"), "INTC")

app$click(selector = 'a[data-value="inputTabset_1"]')
app$run_js("HTMLWidgets.getInstance(document.getElementById('in_1')).hot.setDataAtCell(0,1,'YELP');")
app$click(selector = 'a[data-value="inputTabset_2"]')
Sys.sleep(1L)
expect_options(unlist(getSelectizeOptions(app, "#dropdown_4")) %>% sort(), c(allStocks, "YELP"))
expect_identical(app$get_js("$('#slider_2').parent().find('.irs-max').text()"), "31")
expect_identical(app$get_js("$('#slider_3').parent().find('.irs-max').text()"), "252")
expect_identical(app$get_value(input = "slider_2"), 8L)
expect_identical(app$get_value(input = "slider_3"), 102L)
expect_identical(app$get_value(input = "dropdown_4"), "INTC")

app$click(selector = "#btRemove1")
Sys.sleep(0.5)
app$click(selector = ".modal-footer .bt-gms-confirm")
Sys.sleep(0.5)
expect_identical(app$get_value(input = "slider_2"), 3L)
expect_identical(app$get_value(input = "slider_3"), 99L)
expect_identical(app$get_value(input = "dropdown_4"), "AAPL")

app$stop()
