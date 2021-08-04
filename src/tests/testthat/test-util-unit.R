context("Unit tests - Utility functions")

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
                        hasDep = TRUE,
                        strings = "_"))
  expect_identical(getDependenciesDropdown(c("$rdata$s$", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(rdata = list("s")), 
                        bw = list(rdata = list("s")),
                        hasDep = TRUE,
                        strings = c("_", "def")))
  expect_identical(getDependenciesDropdown(c("$rdata$s", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(rdata = list("s")), 
                        bw = list(),
                        hasDep = TRUE,
                        strings = c("_", "def")))
  expect_identical(getDependenciesDropdown(c("pdata$s$", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(), 
                        bw = list(pdata = list("s")),
                        hasDep = TRUE,
                        strings = c("_", "def")))
  expect_error(getDependenciesDropdown(c("xdata$s$", "_", "def"), modelIn1, "actscen"))
  expect_error(getDependenciesDropdown(c("$xdata", "_", "def"), modelIn1, "actscen"))
  expect_identical(getDependenciesDropdown(c("pdata$$s$$", "_", "def"), modelIn1, "actscen"), 
                   list(fw = list(), 
                        bw = list(),
                        hasDep = FALSE,
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
test_that("genWidgetGroups works", {
  expect_identical(genWidgetGroups(c("a","c","e","f"), list(list(name = "Widgets 1", members = c("a", "c"), sameTab = TRUE)), 
                                   widgetTabName = "Widgets", aggregateWidgets = TRUE),
                   list(list(name = "Widgets", members = c("e", "f"), sameTab = TRUE),
                        list(name = "Widgets 1", members = c("a", "c"), sameTab = TRUE)))
  expect_identical(genWidgetGroups(c("a","c","e","f"), list(list(name = "Widgets 1", members = c("a", "c"))), 
                                   widgetTabName = "Widgets", aggregateWidgets = TRUE),
                   list(list(name = "Widgets", members = c("e", "f"), sameTab = TRUE),
                        list(name = "Widgets 1", members = c("a", "c"))))
  expect_identical(genWidgetGroups(c("a","c","e","f"), list(list(name = "Widgets 1", members = c("a", "c"))), 
                                   widgetTabName = "Widgets", aggregateWidgets = FALSE),
                   list(list(name = "Widgets", members = c("e", "f"), sameTab = FALSE),
                        list(name = "Widgets 1", members = c("a", "c"))))
  expect_identical(genWidgetGroups(c("a","c","e","f"), list(list(name = "Widgets 1", members = c("a", "c"))), 
                                   widgetTabName = "Widgets", aggregateWidgets = FALSE,
                                   inputGroups = list(list(name = "A", members = c("x", "y")),
                                                      list(names = "B", members = c("e", "z")))),
                   list(list(name = "Widgets", members = c("f"), sameTab = FALSE),
                        list(name = "Widgets 1", members = c("a", "c"))))
  expect_identical(genWidgetGroups(c("a","c","e","f"), list(list(name = "Widgets 1", members = c("a", "c"))), 
                                   widgetTabName = "Widgets", aggregateWidgets = FALSE,
                                   inputGroups = list(list(name = "A", members = c("x", "y")),
                                                      list(names = "B", members = c("e", "f")))),
                   list(list(name = "Widgets 1", members = c("a", "c"))))
})
test_that("getTabs works", {
  expect_identical(getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"), 
                           list(list(name = "Group 1", members = c("b", "d"))), 
                           idsToDisplay = NULL, widgetIds = NULL,
                           mergeScalars = FALSE, widgetIdsMultiDim = integer(0L)),
                   list(tabs = list(1L, c(2L, 4L), 3L), tabTitles = list("A", c("Group 1", "B", "D"), "C"), 
                        tabSheetMap = list(1L, 2:1, 3L, c(2L, 2L))))
  expect_identical(getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"), 
                           list(list(name = "Group 1", members = c("a", "b"))), 
                           idsToDisplay = 1:3, widgetIds = NULL,
                           mergeScalars = FALSE, widgetIdsMultiDim = integer(0L)),
                   list(tabs = list(1:2, 3L), tabTitles = list(c("Group 1", "A", "B"), "C"), 
                        tabSheetMap = list(c(1L, 1L), 1:2, 2L, NULL)))
  expect_identical(getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"), 
                           list(list(name = "Group 1", members = c("a", "b"))), 
                           idsToDisplay = 1:3, widgetIds = NULL,
                           mergeScalars = FALSE, widgetIdsMultiDim = integer(0L)),
                   list(tabs = list(1:2, 3L), tabTitles = list(c("Group 1", "A", "B"), "C"), 
                        tabSheetMap = list(c(1L, 1L), 1:2, 2L, NULL)))
  expect_identical(getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
                           list(list(name = "Group 1", members = c("a", "b"))),
                           idsToDisplay = NULL, widgetIds = 3:4, scalarsTabName = "Scalars",
                           mergeScalars = TRUE, widgetIdsMultiDim = integer(0L)),
                   list(tabs = list(0L, 1:2), tabTitles = list("Scalars", c("Group 1", "A", "B")),
                        tabSheetMap = list(c(2L, 1L), c(2L, 2L), NULL, NULL)))
  expect_identical(getTabs(c("a", "_scalars", "c", "d"), c("A", "SCALARS", "C", "D"), 
                           list(list(name = "Group 1", members = c("a", "_scalars"))), 
                           idsToDisplay = NULL, widgetIds = 4, scalarsTabName = "Scalars",
                           mergeScalars = TRUE, widgetIdsMultiDim = integer(0L)),
                   list(tabs = list(0L, 1L, 3L), tabTitles = list("Scalars", c("Group 1", "A"), "C"),
                        tabSheetMap = list(c(2L, 1L), 1L, 3L, NULL)))
  expect_identical(getTabs(c("a", "c", "d", "_scalars"), c("A", "C", "D", "SCALARS"),
                           list(list(name = "Group 1", members = c("a", "_scalars"))),
                           idsToDisplay = NULL, widgetIds = 3, scalarsTabName = "Scalars",
                           mergeScalars = TRUE, widgetIdsMultiDim = integer(0L)),
                   list(tabs = list(0L, 1L, 2L), tabTitles = list("Scalars", c("Group 1", "A"), "C"),
                        tabSheetMap = list(c(2L, 1L), 3L, NULL, 1L)))
  expect_identical(getTabs(c("a", "c", "d", "_scalars"), c("A", "C", "D", "SCALARS"),
                           list(list(name = "Group 1", members = c("a", "_scalars"))),
                           idsToDisplay = NULL, widgetIds = 3, scalarsTabName = "Scalars",
                           mergeScalars = TRUE, widgetIdsMultiDim = 3),
                   list(tabs = list(0L, 1L, 2L, 3L), tabTitles = list("Scalars", c("Group 1", "A"), "C", "D"),
                        tabSheetMap = list(c(2L, 1L), 3L, 4L, 1L)))
  expect_identical(getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
                           list(list(name = "Group 1", members = c("b", "a")),
                                list(name = "Group 2", members = c("c", "d"))),
                           idsToDisplay = NULL, widgetIds = integer(), scalarsTabName = "Scalars",
                           mergeScalars = TRUE, widgetIdsMultiDim = integer()),
                   list(tabs = list(c(2L, 1L), c(3L, 4L)),
                        tabTitles = list(c("Group 1", "B", "A"), c("Group 2", "C", "D")),
                        tabSheetMap = list(c(1L, 2L), c(1L, 1L), c(2L, 1L), c(2L, 2L))))
  expect_identical(getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
                           list(list(name = "Group 1", members = c("b", "a")),
                                list(name = "Group 2", members = c("c", "d"))),
                           idsToDisplay = NULL, widgetIds = c(1L, 3L), scalarsTabName = "Scalars",
                           mergeScalars = TRUE, widgetIdsMultiDim = integer()),
                   list(tabs = list(0L, 2L, 4L),
                        tabTitles = list("Scalars", c("Group 1", "B"), c("Group 2", "D")),
                        tabSheetMap = list(NULL, c(2L, 1L), NULL, c(3L, 1L))))
  expect_identical(getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
                           list(list(name = "Group 1", members = c("b", "a")),
                                list(name = "Group 2", members = c("c", "d"))),
                           idsToDisplay = NULL, widgetIds = c(1L), scalarsTabName = "Scalars",
                           mergeScalars = TRUE, widgetIdsMultiDim = c(3L)),
                   list(tabs = list(0L, 2L, c(3L, 4L)),
                        tabTitles = list("Scalars", c("Group 1", "B"), c("Group 2", "C", "D")),
                        tabSheetMap = list(NULL, c(2L, 1L), c(3L, 1L), c(3L, 2L))))
})
source("../../tools/config/util.R")

test_that("Parsing function bodies with parseFunctionBody works", {
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){}\n \n renderMirorenderer_t <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){\n    # asd\n}\n", "mirorenderer_tOutput"),
                   character())
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){}\n \n renderMirorenderer_t <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){   \t  \n    # asd\n}\n", "renderMirorenderer_t"),
                   "    # asd")
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\n    # { { { \n    test123  \n    }\n \n    ", "mirorenderer_tOutput"), c("    # { { { ", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    # { { { \r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    # { { { ", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    a <- \"# { { { \r\n    \"\r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    a <- \"# { { { ", "    \"", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    a <- ' { { { \r\n    '\r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    a <- ' { { { ", "    '", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    a <- `# \"'{ { { \r\n    `\r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    a <- `# \"'{ { { ", "    `", "    test123"))
})
