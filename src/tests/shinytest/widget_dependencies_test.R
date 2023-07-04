app <- AppDriver$new("../../", name = "widget_dependencies_test", variant = NULL, load_timeout = 20000)
Sys.sleep(2L)
app$view()
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
