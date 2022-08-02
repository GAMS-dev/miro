app <- AppDriver$new("../../", name = "widget_dependencies_test", variant = NULL, load_timeout = 20000)
Sys.sleep(2L)

allStocks <- c(
  "AAPL", "AXP", "BA", "CAT", "CSCO", "CVX", "DD", "DIS", "GE",
  "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM",
  "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "V",
  "VZ", "WMT", "XOM"
)

# check stock table (already filtered)
app$click(selector = 'a[data-value="inputTabset_1"]')
Sys.sleep(2L)
priceData <- getHotData(app, "in_1")
filteredStock <- priceData[[2]] %>% unique()
expect_identical(filteredStock, "AAPL")

# check stock selection dropdown and select JPM stock
app$click(selector = 'a[data-value="inputTabset_1"]')
Sys.sleep(1L)
expect_options(unlist(getSelectizeOptions(app, "#dropdown_2")) %>% sort(), allStocks)
selectSelectizeOption(app, "#dropdown_2", "JPM")

# check filtered table
app$click(selector = 'a[data-value="inputTabset_2"]')
Sys.sleep(2L)
priceData <- getHotData(app, "in_1")
filteredStock <- priceData[[2]] %>% unique()
expect_identical(filteredStock, "JPM")
app$expect_values(output = "inputDataTitle")

app$stop()
