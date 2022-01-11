app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("widget_dependencies_test")
Sys.sleep(2L)

allStocks <- c(
  "AAPL", "AXP", "BA", "CAT", "CSCO", "CVX", "DD", "DIS", "GE",
  "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM",
  "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "V",
  "VZ", "WMT", "XOM"
)

# check stock table (already filtered)
app$findElement('a[data-value="inputTabset_1"]')$click()
Sys.sleep(2L)
priceData <- getHotData(app, "in_1")
filteredStock <- priceData[[2]] %>% unique()
expect_identical(filteredStock, "AAPL")

# check stock selection dropdown and select JPM stock
app$findElement('a[data-value="inputTabset_1"]')$click()
Sys.sleep(1L)
expect_options(getSelectizeOptions(app, "#dropdown_2") %>% sort(), allStocks)
selectSelectizeOption(app, "#dropdown_2", "JPM")

# check filtered table
app$findElement('a[data-value="inputTabset_2"]')$click()
Sys.sleep(2L)
priceData <- getHotData(app, "in_1")
filteredStock <- priceData[[2]] %>% unique()
expect_identical(filteredStock, "JPM")
app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)

app$stop()
