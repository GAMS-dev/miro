context("Unit tests - Utility functions")

source("../../components/util.R")

modelIn1 <- list(
  rdata = list(
    alias = "Reactor data \U0001f600⻩", symtype = "parameter",
    headers = list(
      rr = list(type = "string", alias = "Reactor"),
      s = list(type = "string", alias = "Scenario"),
      vmin = list(type = "numeric", alias = "vmin"),
      vmax = list(type = "numeric", alias = "vmax")
    ),
    type = "hot"
  ),
  pdata = list(
    alias = "Product data",
    symtype = "parameter",
    headers = list(
      pp = list(type = "string", alias = "Product"),
      s = list(type = "string", alias = "Scenario"),
      demand = list(type = "numeric", alias = "demand"),
      `production time` = list(
        type = "numeric", alias = "production time"
      )
    ),
    type = "hot"
  ),
  actscen = list(
    alias = "Active scenario", dropdown = list(
      label = "Select scenario", choices = c("$s$", "_")
    ),
    type = "dropdown"
  ),
  whrs = list(
    alias = "Length of considered period [hours]",
    slider = list(
      label = "Select period length [h]", min = 1L,
      max = 672L, default = 168L, step = 1L
    ), type = "slider"
  ),
  csti = list(
    alias = "Depreciation cost per m^3 reactor and period [kEuro]",
    slider = list(
      label = "Select depreciation cost [k€]",
      min = 0.01, max = 2.5, default = 0.97, step = 0.01
    ),
    type = "slider"
  ),
  cstf = list(
    alias = "Fixed cost per week and reactor [kEuro]",
    slider = list(
      label = "Select fixed cost [k€]", min = 1.5,
      max = 5L, default = 2.45, step = 0.05
    ), type = "slider"
  ),
  esf = list(alias = "Economies of scale factor", slider = list(
    label = "Select EOS", min = 0L, max = 1L, default = 0.5,
    step = 0.05
  ), type = "slider")
)

test_that("getDependenciesDropdown works", {
  expect_identical(
    getDependenciesDropdown(c("$s$", "_"), modelIn1, "actscen"),
    list(
      fw = list(rdata = list("s"), pdata = list("s")),
      bw = list(rdata = list("s"), pdata = list("s")),
      hasDep = TRUE,
      strings = "_"
    )
  )
  expect_identical(
    getDependenciesDropdown(c("$rdata$s$", "_", "def"), modelIn1, "actscen"),
    list(
      fw = list(rdata = list("s")),
      bw = list(rdata = list("s")),
      hasDep = TRUE,
      strings = c("_", "def")
    )
  )
  expect_identical(
    getDependenciesDropdown(c("$rdata$s", "_", "def"), modelIn1, "actscen"),
    list(
      fw = list(rdata = list("s")),
      bw = list(),
      hasDep = TRUE,
      strings = c("_", "def")
    )
  )
  expect_identical(
    getDependenciesDropdown(c("pdata$s$", "_", "def"), modelIn1, "actscen"),
    list(
      fw = list(),
      bw = list(pdata = list("s")),
      hasDep = TRUE,
      strings = c("_", "def")
    )
  )
  expect_error(getDependenciesDropdown(c("xdata$s$", "_", "def"), modelIn1, "actscen"))
  expect_error(getDependenciesDropdown(c("$xdata", "_", "def"), modelIn1, "actscen"))
  expect_identical(
    getDependenciesDropdown(c("pdata$$s$$", "_", "def"), modelIn1, "actscen"),
    list(
      fw = list(),
      bw = list(),
      hasDep = FALSE,
      strings = c("pdata$s$", "_", "def")
    )
  )
})

test_that("getDependenciesSlider works", {
  expect_identical(
    getDependenciesSlider(
      min = 0L,
      max = "card(rdata$rr)",
      def = 0L,
      step = 1L,
      modelIn = modelIn1,
      listOfOperators = listOfOperators
    ),
    list(
      min = 0, max = list(rdata = "rr", `$operator` = "count"),
      def = 0, step = 1
    )
  )
  expect_identical(
    getDependenciesSlider(
      min = "min(rdata$rr)",
      max = "card(rdata$rr)",
      def = "max(rdata$rr)",
      step = 1L,
      modelIn = modelIn1,
      listOfOperators = listOfOperators
    ),
    list(
      min = list(rdata = "rr", `$operator` = "min"),
      max = list(rdata = "rr", `$operator` = "count"),
      def = list(rdata = "rr", `$operator` = "max"),
      step = 1
    )
  )
  expect_identical(
    getDependenciesSlider(
      min = "min(rdata$rr)",
      max = "card(rdata$rr)",
      def = "sd(rdata$rr)",
      step = 1L,
      modelIn = modelIn1,
      listOfOperators = listOfOperators
    ),
    list(
      min = list(rdata = "rr", `$operator` = "min"),
      max = list(rdata = "rr", `$operator` = "count"),
      def = list(rdata = "rr", `$operator` = "sd"),
      step = 1
    )
  )
  expect_identical(
    getDependenciesSlider(
      min = "mean(rdata$rr)",
      max = "median(rdata$rr)",
      def = "var(rdata$rr)",
      step = "1.0",
      modelIn = modelIn1,
      listOfOperators = listOfOperators
    ),
    list(
      min = list(rdata = "rr", `$operator` = "mean"),
      max = list(rdata = "rr", `$operator` = "median"),
      def = list(rdata = "rr", `$operator` = "var"),
      step = 1
    )
  )
  expect_error(getDependenciesSlider(
    min = "asd(rdata$rr)",
    max = "card(rdata$rr)",
    def = "max(rdata$rr)",
    step = 1L,
    modelIn = modelIn1,
    listOfOperators = listOfOperators
  ))
  expect_error(getDependenciesSlider(
    min = "min(xdata$rr)",
    max = "card(rdata$rr)",
    def = "max(rdata$rr)",
    step = 1L,
    modelIn = modelIn1,
    listOfOperators = listOfOperators
  ))
  expect_error(getDependenciesSlider(
    min = "min(rdata$aa)",
    max = "card(rdata$rr)",
    def = "max(rdata$rr)",
    step = 1L,
    modelIn = modelIn1,
    listOfOperators = listOfOperators
  ))
})

test_that("getWidgetDependencies works", {
  expect_identical(getWidgetDependencies("dropdown", "$date"), c("0", "", "date"))
  expect_identical(getWidgetDependencies("dropdown", "$price$date"), c("0", "price", "date"))
  expect_identical(getWidgetDependencies("dropdown", "date$"), c("1", "", "date"))
  expect_identical(getWidgetDependencies("dropdown", "price$date$"), c("1", "price", "date"))
  expect_identical(getWidgetDependencies("dropdown", "$date$"), c("2", "", "date"))
  expect_identical(getWidgetDependencies("dropdown", "$price$date$"), c("2", "price", "date"))

  expect_identical(getWidgetDependencies("slider", "card(price$date)"), c("card", "price", "date"))

  expect_identical(getWidgetDependencies("dropdown", c("1", "2")), character(0))
})
test_that("genWidgetGroups works", {
  expect_identical(
    genWidgetGroups(c("a", "c", "e", "f"), list(list(name = "Widgets 1", members = c("a", "c"), sameTab = TRUE)),
      widgetTabName = "Widgets", aggregateWidgets = TRUE
    ),
    list(
      list(name = "Widgets", members = c("e", "f"), sameTab = TRUE),
      list(name = "Widgets 1", members = c("a", "c"), sameTab = TRUE)
    )
  )
  expect_identical(
    genWidgetGroups(c("a", "c", "e", "f"), list(list(name = "Widgets 1", members = c("a", "c"))),
      widgetTabName = "Widgets", aggregateWidgets = TRUE
    ),
    list(
      list(name = "Widgets", members = c("e", "f"), sameTab = TRUE),
      list(name = "Widgets 1", members = c("a", "c"))
    )
  )
  expect_identical(
    genWidgetGroups(c("a", "c", "e", "f"), list(list(name = "Widgets 1", members = c("a", "c"))),
      widgetTabName = "Widgets", aggregateWidgets = FALSE
    ),
    list(
      list(name = "Widgets", members = c("e", "f"), sameTab = FALSE),
      list(name = "Widgets 1", members = c("a", "c"))
    )
  )
  expect_identical(
    genWidgetGroups(c("a", "c", "e", "f"), list(list(name = "Widgets 1", members = c("a", "c"))),
      widgetTabName = "Widgets", aggregateWidgets = FALSE,
      inputGroups = list(
        list(name = "A", members = c("x", "y")),
        list(names = "B", members = c("e", "z"))
      )
    ),
    list(
      list(name = "Widgets", members = c("f"), sameTab = FALSE),
      list(name = "Widgets 1", members = c("a", "c"))
    )
  )
  expect_identical(
    genWidgetGroups(c("a", "c", "e", "f"), list(list(name = "Widgets 1", members = c("a", "c"))),
      widgetTabName = "Widgets", aggregateWidgets = FALSE,
      inputGroups = list(
        list(name = "A", members = c("x", "y")),
        list(names = "B", members = c("e", "f"))
      )
    ),
    list(list(name = "Widgets 1", members = c("a", "c")))
  )
})
test_that("getTabs works", {
  expect_identical(
    getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
      list(list(name = "Group 1", members = c("b", "d"))),
      idsToDisplay = NULL, widgetIds = NULL,
      mergeScalars = FALSE, widgetIdsMultiDim = integer(0L)
    ),
    list(
      tabs = list(1L, c(2L, 4L), 3L), tabSettings = list(list(), list(), list()),
      tabTitles = list("A", c("Group 1", "B", "D"), "C"),
      tabSheetMap = list(1L, 2:1, 3L, c(2L, 2L))
    )
  )
  expect_identical(
    getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
      list(list(name = "Group 1", members = c("a", "b"))),
      idsToDisplay = 1:3, widgetIds = NULL,
      mergeScalars = FALSE, widgetIdsMultiDim = integer(0L)
    ),
    list(
      tabs = list(1:2, 3L), tabSettings = list(list(), list()),
      tabTitles = list(c("Group 1", "A", "B"), "C"),
      tabSheetMap = list(c(1L, 1L), 1:2, 2L, NULL)
    )
  )
  expect_identical(
    getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
      list(list(name = "Group 1", members = c("a", "b"))),
      idsToDisplay = 1:3, widgetIds = NULL,
      mergeScalars = FALSE, widgetIdsMultiDim = integer(0L)
    ),
    list(
      tabs = list(1:2, 3L), tabSettings = list(list(), list()),
      tabTitles = list(c("Group 1", "A", "B"), "C"),
      tabSheetMap = list(c(1L, 1L), 1:2, 2L, NULL)
    )
  )
  expect_identical(
    getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
      list(list(name = "Group 1", members = c("a", "b"))),
      idsToDisplay = NULL, widgetIds = 3:4, scalarsTabName = "Scalars",
      mergeScalars = TRUE, widgetIdsMultiDim = integer(0L)
    ),
    list(
      tabs = list(0L, 1:2), tabSettings = list(list(), list()),
      tabTitles = list("Scalars", c("Group 1", "A", "B")),
      tabSheetMap = list(c(2L, 1L), c(2L, 2L), NULL, NULL)
    )
  )
  expect_identical(
    getTabs(c("a", "_scalars", "c", "d"), c("A", "SCALARS", "C", "D"),
      list(list(name = "Group 1", members = c("a", "_scalars"))),
      idsToDisplay = NULL, widgetIds = 4, scalarsTabName = "Scalars",
      mergeScalars = TRUE, widgetIdsMultiDim = integer(0L)
    ),
    list(
      tabs = list(0L, 1L, 3L), tabSettings = list(list(), list(), list()),
      tabTitles = list("Scalars", c("Group 1", "A"), "C"),
      tabSheetMap = list(c(2L, 1L), 1L, 3L, NULL)
    )
  )
  expect_identical(
    getTabs(c("a", "c", "d", "_scalars"), c("A", "C", "D", "SCALARS"),
      list(list(name = "Group 1", members = c("a", "_scalars"))),
      idsToDisplay = NULL, widgetIds = 3, scalarsTabName = "Scalars",
      mergeScalars = TRUE, widgetIdsMultiDim = integer(0L)
    ),
    list(
      tabs = list(0L, 1L, 2L), tabSettings = list(list(), list(), list()),
      tabTitles = list("Scalars", c("Group 1", "A"), "C"),
      tabSheetMap = list(c(2L, 1L), 3L, NULL, 1L)
    )
  )
  expect_identical(
    getTabs(c("a", "c", "d", "_scalars"), c("A", "C", "D", "SCALARS"),
      list(list(name = "Group 1", members = c("a", "_scalars"))),
      idsToDisplay = NULL, widgetIds = 3, scalarsTabName = "Scalars",
      mergeScalars = TRUE, widgetIdsMultiDim = 3
    ),
    list(
      tabs = list(0L, 1L, 2L, 3L), tabSettings = list(list(), list(), list(), list()),
      tabTitles = list("Scalars", c("Group 1", "A"), "C", "D"),
      tabSheetMap = list(c(2L, 1L), 3L, 4L, 1L)
    )
  )
  expect_identical(
    getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
      list(
        list(name = "Group 1", members = c("b", "a")),
        list(name = "Group 2", members = c("c", "d"))
      ),
      idsToDisplay = NULL, widgetIds = integer(), scalarsTabName = "Scalars",
      mergeScalars = TRUE, widgetIdsMultiDim = integer()
    ),
    list(
      tabs = list(c(2L, 1L), c(3L, 4L)), tabSettings = list(list(), list()),
      tabTitles = list(c("Group 1", "B", "A"), c("Group 2", "C", "D")),
      tabSheetMap = list(c(1L, 2L), c(1L, 1L), c(2L, 1L), c(2L, 2L))
    )
  )
  expect_identical(
    getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
      list(
        list(name = "Group 1", members = c("b", "a")),
        list(name = "Group 2", members = c("c", "d"))
      ),
      idsToDisplay = NULL, widgetIds = c(1L, 3L), scalarsTabName = "Scalars",
      mergeScalars = TRUE, widgetIdsMultiDim = integer()
    ),
    list(
      tabs = list(0L, 2L, 4L), tabSettings = list(list(), list(), list()),
      tabTitles = list("Scalars", c("Group 1", "B"), c("Group 2", "D")),
      tabSheetMap = list(NULL, c(2L, 1L), NULL, c(3L, 1L))
    )
  )
  expect_identical(
    getTabs(c("a", "b", "c", "d"), c("A", "B", "C", "D"),
      list(
        list(name = "Group 1", members = c("b", "a")),
        list(name = "Group 2", members = c("c", "d"))
      ),
      idsToDisplay = NULL, widgetIds = c(1L), scalarsTabName = "Scalars",
      mergeScalars = TRUE, widgetIdsMultiDim = c(3L)
    ),
    list(
      tabs = list(0L, 2L, c(3L, 4L)), tabSettings = list(list(), list(), list()),
      tabTitles = list("Scalars", c("Group 1", "B"), c("Group 2", "C", "D")),
      tabSheetMap = list(NULL, c(2L, 1L), c(3L, 1L), c(3L, 2L))
    )
  )
})
source("../../tools/config/util.R")

test_that("Parsing function bodies with parseFunctionBody works", {
  expect_identical(
    parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){}\n \n renderMirorenderer_t <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){\n    # asd\n}\n", "mirorenderer_tOutput"),
    character()
  )
  expect_identical(
    parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){}\n \n renderMirorenderer_t <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){   \t  \n    # asd\n}\n", "renderMirorenderer_t"),
    "    # asd"
  )
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\n    # { { { \n    test123  \n    }\n \n    ", "mirorenderer_tOutput"), c("    # { { { ", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    # { { { \r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    # { { { ", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    a <- \"# { { { \r\n    \"\r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    a <- \"# { { { ", "    \"", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    a <- ' { { { \r\n    '\r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    a <- ' { { { ", "    '", "    test123"))
  expect_identical(parseFunctionBody("mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL){\r\n    a <- `# \"'{ { { \r\n    `\r\n    test123  \r\n    }\r\n \r\n    ", "mirorenderer_tOutput"), c("    a <- `# \"'{ { { ", "    `", "    test123"))
})

source("../../modules/renderers/miro-pivot.R")

setIndices <- c("hdr1", "hdr2", "hdr3", "hdr4")
test_that("Getting index lists work (miroPivot)", {
  expect_identical(
    getIndexLists(setIndices, list(
      cols = list(), filter = list(), aggregations = list(),
      rows = list("hdr2", "hdr3")
    )),
    list(
      rows = c("hdr2", "hdr3", "hdr1", "hdr4"), cols = character(),
      filter = character(), aggregations = character()
    )
  )
  expect_identical(
    getIndexLists(setIndices, list(
      cols = list("hdr4" = c()), filter = list(), aggregations = list(),
      rows = list("hdr2", "hdr3")
    )),
    list(rows = c("hdr2", "hdr3", "hdr1"), cols = "hdr4", filter = character(), aggregations = character())
  )
  expect_identical(
    getIndexLists(setIndices, list(
      cols = list("hdr4" = c(), "hdr1" = c()),
      filter = list(), aggregations = list(),
      rows = list("hdr2", "hdr3")
    )),
    list(
      rows = c("hdr2", "hdr3"), cols = c("hdr4", "hdr1"),
      filter = character(), aggregations = character()
    )
  )
  expect_identical(
    getIndexLists(setIndices, list(
      cols = list("hdr4" = c(), "asd" = c()),
      filter = list(), aggregations = list(),
      rows = list("hdr2", "hdr3")
    )),
    list(
      rows = c("hdr2", "hdr3", "hdr1"), cols = c("hdr4"),
      filter = character(), aggregations = character()
    )
  )
  expect_identical(
    getIndexLists(setIndices, list(
      cols = list("hdr4" = c(), "hdr1" = c()),
      filter = list(), aggregations = list(),
      rows = list("hdr2", "hdr3", "hdr1")
    )),
    list(
      rows = c("hdr2", "hdr3"), cols = c("hdr4", "hdr1"),
      filter = character(), aggregations = character()
    )
  )
  expect_identical(
    getIndexLists(setIndices, list(
      cols = list("hdr4" = c(), "hdr1" = c()),
      filter = list(), aggregations = list(),
      rows = list("asd", "hdr3", "hdr1")
    )),
    list(
      rows = c("hdr3", "hdr2"), cols = c("hdr4", "hdr1"),
      filter = character(), aggregations = character()
    )
  )
  expect_identical(
    getIndexLists(setIndices, list(
      cols = list(),
      filter = list(), aggregations = list(),
      rows = list()
    )),
    list(
      rows = c("hdr1", "hdr2", "hdr3", "hdr4"), cols = character(),
      filter = character(), aggregations = character()
    )
  )
})

test_that("Merging dataframes works", {
  expect_identical(
    mergeDf(
      tibble(i = c("i1", "i2"), j = c("j1", "j2"), val = c(1, 2)),
      tibble(i = c("i1", "i2"), j = c("j2", "j2"), val = c(3, 4))
    ),
    tibble(i = c("i1", "i2", "i1"), j = c("j1", "j2", "j2"), val = c(1, 4, 3))
  )
  expect_identical(
    mergeDf(
      tibble(i = c("i1", "i2"), j = c("j1", "j2"), hdr2 = c(1, 2), hdr1 = c(3, NA)),
      tibble(i = c("i1", "i2"), j = c("j2", "j2"), hdr2 = c(3, NA), hdr1 = c(NA, 6))
    ),
    tibble(i = c("i1", "i2", "i1"), j = c("j1", "j2", "j2"), hdr2 = c(1, 2, 3), hdr1 = c(3, 6, NA))
  )
  expect_identical(
    mergeDf(
      tibble(i = c("i1", "i2"), j = c("j1", "j2"), text = c("a", "b")),
      tibble(i = c("i1", "i2"), j = c("j2", "j2"), text = c("c", "d"))
    ),
    tibble(i = c("i1", "i2", "i1"), j = c("j1", "j2", "j2"), text = c("a", "d", "c"))
  )
  expect_identical(
    mergeDf(
      tibble(i = c("i1", "i2"), j = c("j1", "j2"), val = c(1, NA)),
      tibble(i = character(), j = character(), val = numeric())
    ),
    tibble(i = c("i1", "i2"), j = c("j1", "j2"), val = c(1, NA))
  )
  expect_identical(
    mergeDf(
      tibble(i = character(), j = character(), val = numeric()),
      tibble(i = c("i1", "i2"), j = c("j1", "j2"), val = c(NA, 4))
    ),
    tibble(i = c("i1", "i2"), j = c("j1", "j2"), val = c(NA, 4))
  )
  expect_identical(
    mergeDf(tibble(scalar = c("scalar1", "scalar2"), description = c("desc1", "desc2"), value = c("1.123456", "test")),
      tibble(scalar = c("scalar3", "scalar2"), description = c("", ""), value = c("2.345678", "test2")),
      isScalarsTable = TRUE
    ),
    tibble(scalar = c("scalar1", "scalar2", "scalar3"), description = c("desc1", "desc2", ""), value = c("1.123456", "test2", "2.345678"))
  )
  expect_identical(
    mergeDf(tibble(scalar = c("scalar1", "scalar2"), description = c("desc1", "desc2"), value = c("1.123456", "test")),
      tibble(scalar = c("scalar3", "scalar2"), description = c("", ""), value = c("2.345678", NA)),
      isScalarsTable = TRUE
    ),
    tibble(scalar = c("scalar1", "scalar2", "scalar3"), description = c("desc1", "desc2", ""), value = c("1.123456", "test", "2.345678"))
  )
  expect_identical(
    mergeDf(tibble(scalar = c("scalar1", "scalar2"), description = c("desc1", "desc2"), value = c("1.123456", "test")),
      tibble(scalar = character(), description = character(), value = character()),
      isScalarsTable = TRUE
    ),
    tibble(scalar = c("scalar1", "scalar2"), description = c("desc1", "desc2"), value = c("1.123456", "test"))
  )
  expect_identical(
    mergeDf(tibble(scalar = character(), description = character(), value = character()),
      tibble(scalar = c("scalar1", "scalar2"), description = c("desc1", "desc2"), value = c("1.123456", "test")),
      isScalarsTable = TRUE
    ),
    tibble(scalar = c("scalar1", "scalar2"), description = c("desc1", "desc2"), value = c("1.123456", "test"))
  )
})

test_that("hotToR works", {
  expect_equivalent(
    hotToR(
      list(
        data = list(list("a1", "b1"), list("a2", "b2")), changes = list(
          event = "afterChange",
          changes = list(list(0L, 0L, "aa", "")), source = "edit"
        ),
        params = list(
          data = list(list("", "")), rClass = list(
            "tbl_df",
            "tbl", "data.frame"
          ), rColClasses = list(
            uni = "character",
            text = "character"
          ), rColnames = list("uni", "text"),
          rColHeaders = list("uni", "text"), rRowHeaders = NULL,
          rDataDim = list(1L, 2L), selectCallback = TRUE, colHeaders = list(
            "Universal set", "Set text "
          ), rowHeaders = "1",
          columns = list(list(type = "text", default = NULL), list(
            type = "text", default = NULL
          )), width = NULL, height = 700L,
          debug = 0L, search = TRUE, stretchH = "none", comments = FALSE,
          contextMenu = list(items = list(
            row_above = list(), row_below = list(),
            remove_row = list(), hsep4 = list(
              name = "---------",
              key = "hsep4"
            ), undo = list(), redo = list(),
            hsep5 = list(name = "---------", key = "hsep5"),
            alignment = list()
          )), ishighlight = TRUE, currentRowClassName = "currentRow",
          currentColClassName = "currentCol", colWidths = 200L,
          columnSorting = TRUE, manualColumnMove = FALSE, manualColumnResize = TRUE
        )
      ),
      list(
        alias = "asd", symtype = "set", headers = list(uni = list(
          type = "string", alias = "Universal set"
        ), text = list(
          type = "string",
          alias = "Set text "
        )),
        type = "hot", colTypes = "cc"
      )
    ),
    tibble(uni = c("a1", "a2"), text = c("b1", "b2"))
  )


  # empty table should result in empty df
  expect_equivalent(
    hotToR(
      list(
        data = list(list("", "")), changes = list(
          event = "afterChange",
          changes = list(list(0L, 0L, "aa", "")), source = "edit"
        ),
        params = list(
          data = list(list("", "")), rClass = list(
            "tbl_df",
            "tbl", "data.frame"
          ), rColClasses = list(
            uni = "character",
            text = "character"
          ), rColnames = list("uni", "text"),
          rColHeaders = list("uni", "text"), rRowHeaders = NULL,
          rDataDim = list(1L, 2L), selectCallback = TRUE, colHeaders = list(
            "Universal set", "Set text "
          ), rowHeaders = "1",
          columns = list(list(type = "text", default = NULL), list(
            type = "text", default = NULL
          )), width = NULL, height = 700L,
          debug = 0L, search = TRUE, stretchH = "none", comments = FALSE,
          contextMenu = list(items = list(
            row_above = list(), row_below = list(),
            remove_row = list(), hsep4 = list(
              name = "---------",
              key = "hsep4"
            ), undo = list(), redo = list(),
            hsep5 = list(name = "---------", key = "hsep5"),
            alignment = list()
          )), ishighlight = TRUE, currentRowClassName = "currentRow",
          currentColClassName = "currentCol", colWidths = 200L,
          columnSorting = TRUE, manualColumnMove = FALSE, manualColumnResize = TRUE
        )
      ),
      list(
        alias = "asd", symtype = "set", headers = list(uni = list(
          type = "string", alias = "Universal set"
        ), text = list(
          type = "string",
          alias = "Set text "
        )),
        type = "hot", colTypes = "cc"
      )
    ),
    tibble(uni = character(), text = character())
  )

  expect_equivalent(
    hotToR(
      list(
        data = list(), changes = list(
          event = "afterChange",
          changes = list(list(0L, 0L, "aa", "")), source = "edit"
        ),
        params = list(
          data = list(list("", "")), rClass = list(
            "tbl_df",
            "tbl", "data.frame"
          ), rColClasses = list(
            uni = "character",
            text = "character"
          ), rColnames = list("uni", "text"),
          rColHeaders = list("uni", "text"), rRowHeaders = NULL,
          rDataDim = list(1L, 2L), selectCallback = TRUE, colHeaders = list(
            "Universal set", "Set text "
          ), rowHeaders = "1",
          columns = list(list(type = "text", default = NULL), list(
            type = "text", default = NULL
          )), width = NULL, height = 700L,
          debug = 0L, search = TRUE, stretchH = "none", comments = FALSE,
          contextMenu = list(items = list(
            row_above = list(), row_below = list(),
            remove_row = list(), hsep4 = list(
              name = "---------",
              key = "hsep4"
            ), undo = list(), redo = list(),
            hsep5 = list(name = "---------", key = "hsep5"),
            alignment = list()
          )), ishighlight = TRUE, currentRowClassName = "currentRow",
          currentColClassName = "currentCol", colWidths = 200L,
          columnSorting = TRUE, manualColumnMove = FALSE, manualColumnResize = TRUE
        )
      ),
      list(
        alias = "asd", symtype = "set", headers = list(uni = list(
          type = "string", alias = "Universal set"
        ), text = list(
          type = "string",
          alias = "Set text "
        )),
        type = "hot", colTypes = "cc"
      )
    ),
    tibble(uni = character(), text = character())
  )


  expect_equivalent(
    hotToR(
      list(
        data = list(list("", NA_real_)), changes = list(
          event = "afterChange",
          changes = list(list(0L, 0L, "aa", "")), source = "edit"
        ),
        params = list(
          data = list(list("", "")), rClass = list(
            "tbl_df",
            "tbl", "data.frame"
          ), rColClasses = list(
            uni = "character",
            text = "character"
          ), rColnames = list("uni", "text"),
          rColHeaders = list("uni", "text"), rRowHeaders = NULL,
          rDataDim = list(1L, 2L), selectCallback = TRUE, colHeaders = list(
            "Universal set", "value"
          ), rowHeaders = "1",
          columns = list(list(type = "text", default = NULL), list(
            type = "text", default = NULL
          )), width = NULL, height = 700L,
          debug = 0L, search = TRUE, stretchH = "none", comments = FALSE,
          contextMenu = list(items = list(
            row_above = list(), row_below = list(),
            remove_row = list(), hsep4 = list(
              name = "---------",
              key = "hsep4"
            ), undo = list(), redo = list(),
            hsep5 = list(name = "---------", key = "hsep5"),
            alignment = list()
          )), ishighlight = TRUE, currentRowClassName = "currentRow",
          currentColClassName = "currentCol", colWidths = 200L,
          columnSorting = TRUE, manualColumnMove = FALSE, manualColumnResize = TRUE
        )
      ),
      list(
        alias = "asd", symtype = "parameter", headers = list(uni = list(
          type = "string", alias = "Universal set"
        ), value = list(
          type = "string",
          alias = "value"
        )),
        type = "hot", colTypes = "cd"
      )
    ),
    tibble(uni = character(), value = numeric())
  )
})
