context("Utility functions")

source("../../global.R")
source("../../components/util.R")

modelIn1 <- list(rdata = list(alias = "Reactor data \U0001f600⻩", symtype = "parameter", 
                              headers = list(rr = list(type = "string", alias = "Reactor"), 
                                             s = list(type = "string", alias = "Scenario"), 
                                             vmin = list(type = "numeric", alias = "vmin"), 
                                             vmax = list(type = "numeric", alias = "vmax")), 
                              type = "hot"), 
                 pdata = list(alias = "Product data", 
                              symtype = "parameter", 
                              headers = list(pp = list(type = "string", alias = "Product"), 
                                             s = list(type = "string", alias = "Scenario"), 
                                             demand = list(type = "numeric", alias = "demand"), 
                                             `production time` = list(
                                               type = "numeric", alias = "production time")), 
                              type = "hot"), 
                 actscen = list(alias = "Active scenario", dropdown = list(
                   label = "Select scenario", choices = c("$s$", "_")), 
                   type = "dropdown"), 
                 whrs = list(alias = "Length of considered period [hours]", 
                             slider = list(label = "Select period length [h]", min = 1L, 
                                           max = 672L, default = 168L, step = 1L), type = "slider"), 
                 csti = list(alias = "Depreciation cost per m^3 reactor and period [kEuro]", 
                             slider = list(label = "Select depreciation cost [k€]", 
                                           min = 0.01, max = 2.5, default = 0.97, step = 0.01), 
                             type = "slider"), 
                 cstf = list(alias = "Fixed cost per week and reactor [kEuro]", 
                             slider = list(label = "Select fixed cost [k€]", min = 1.5, 
                                           max = 5L, default = 2.45, step = 0.05), type = "slider"), 
                 esf = list(alias = "Economies of scale factor", slider = list(
                   label = "Select EOS", min = 0L, max = 1L, default = 0.5, 
                   step = 0.05), type = "slider"))

test_that("getDependenciesDropdown works", {
  expect_identical(getDependenciesDropdown(c("$s$", "_"), modelIn1, "actscen"), 
                   list(fw = list(rdata = list("s"), pdata = list("s")), 
                        bw = list(rdata = list("s"), pdata = list("s")), 
                        strings = "_"))
  expect_identical(getDependenciesDropdown(c("$rdata$s$", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(rdata = list("s")), 
                        bw = list(rdata = list("s")), 
                        strings = c("_", "def")))
  expect_identical(getDependenciesDropdown(c("$rdata$s", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(rdata = list("s")), 
                        bw = list(),
                        strings = c("_", "def")))
  expect_identical(getDependenciesDropdown(c("pdata$s$", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(), 
                        bw = list(pdata = list("s")),
                        strings = c("_", "def")))
  expect_error(getDependenciesDropdown(c("xdata$s$", "_", "def"), modelIn1, "actscen"))
  expect_error(getDependenciesDropdown(c("$xdata", "_", "def"), modelIn1, "actscen"))
  expect_identical(getDependenciesDropdown(c("pdata$$s$$", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(), 
                        bw = list(),
                        strings = c("pdata$s$", "_", "def")))
})

test_that("getDependenciesSlider works", {
  expect_identical(getDependenciesSlider(min = 0L, 
                                         max = "card(rdata$rr)", 
                                         def = 0L, 
                                         step = 1L,
                                         modelIn = modelIn1, 
                                         listOfOperators = listOfOperators),
                   list(min = 0, max = list(rdata = "rr", `$operator` = "count"), 
                        def = 0, step = 1))
  expect_identical(getDependenciesSlider(min = "min(rdata$rr)", 
                                         max = "card(rdata$rr)", 
                                         def = "max(rdata$rr)", 
                                         step = 1L,
                                         modelIn = modelIn1, 
                                         listOfOperators = listOfOperators),
                   list(min = list(rdata = "rr", `$operator` = "min"), 
                        max = list(rdata = "rr", `$operator` = "count"), 
                        def = list(rdata = "rr", `$operator` = "max"), 
                        step = 1))
  expect_identical(getDependenciesSlider(min = "min(rdata$rr)", 
                                         max = "card(rdata$rr)", 
                                         def = "sd(rdata$rr)", 
                                         step = 1L,
                                         modelIn = modelIn1, 
                                         listOfOperators = listOfOperators),
                   list(min = list(rdata = "rr", `$operator` = "min"), 
                        max = list(rdata = "rr", `$operator` = "count"), 
                        def = list(rdata = "rr", `$operator` = "sd"), 
                        step = 1))
  expect_identical(getDependenciesSlider(min = "mean(rdata$rr)", 
                                         max = "median(rdata$rr)", 
                                         def = "var(rdata$rr)", 
                                         step = "1.0",
                                         modelIn = modelIn1, 
                                         listOfOperators = listOfOperators),
                   list(min = list(rdata = "rr", `$operator` = "mean"), 
                        max = list(rdata = "rr", `$operator` = "median"), 
                        def = list(rdata = "rr", `$operator` = "var"), 
                        step = 1))
  expect_error(getDependenciesSlider(min = "asd(rdata$rr)", 
                                     max = "card(rdata$rr)", 
                                     def = "max(rdata$rr)", 
                                     step = 1L,
                                     modelIn = modelIn1, 
                                     listOfOperators = listOfOperators))
  expect_error(getDependenciesSlider(min = "min(xdata$rr)", 
                                     max = "card(rdata$rr)", 
                                     def = "max(rdata$rr)", 
                                     step = 1L,
                                     modelIn = modelIn1, 
                                     listOfOperators = listOfOperators))
  expect_error(getDependenciesSlider(min = "min(rdata$aa)", 
                                     max = "card(rdata$rr)", 
                                     def = "max(rdata$rr)", 
                                     step = 1L,
                                     modelIn = modelIn1, 
                                     listOfOperators = listOfOperators))
})

test_that("getWidgetDependencies works", {
  expect_identical(getWidgetDependencies("dropdown", "$date"),c("0", "", "date"))
  expect_identical(getWidgetDependencies("dropdown", "$price$date"),c("0", "price", "date"))
  expect_identical(getWidgetDependencies("dropdown", "date$"),c("1", "", "date"))
  expect_identical(getWidgetDependencies("dropdown", "price$date$"),c("1", "price", "date"))
  expect_identical(getWidgetDependencies("dropdown", "$date$"),c("2", "", "date"))
  expect_identical(getWidgetDependencies("dropdown", "$price$date$"),c("2", "price", "date"))
  
  expect_identical(getWidgetDependencies("slider", "card(price$date)"),c("card", "price", "date"))
  
  expect_identical(getWidgetDependencies("dropdown", c("1", "2")),character(0))
})
