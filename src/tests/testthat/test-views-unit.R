context("Unit tests - views class")

library(jsonlite)
library(futile.logger)

source("../../components/scenario_extensions.R")
source("../../components/views.R")

FakeSession <- R6Class("FakeSession", public = list(
  initialize = function(id) {
    private$id <- id
    return(invisible(self))
  },
  ns = function(id) {
    return(paste0(private$id, "-", id))
  }
), private = list(
  id = NULL
))

lang <<- list(nav = list(scen = list(pivot = list(viewPrefix = "Pivot Comparison:"))))

ioConfig <<- list(scenTableNamesToDisplay = c("out1", "out2", "out3", "in1", "in2"))

views <- Views$new(c("in1", "widget1", "widget2", "in2"), c("out1", "out2", "out4", "out3"), c("in1", "in2"),
  customCompareModeConfig = list(bla123 = list(label = "First custom compare"), def467 = list(label = "Second cool analyzer"))
)

test_that("Loading/Unloading configuration works", {
  testViewData <- tibble(
    "_sid" = c(1, 1, 1, 1, 5, 6, 7, 7),
    symName = c("in1", "in1", "in3", "_pivotcomp_out2", "in2", "out2", "out2", "out2"),
    id = c("view1", "view2", "view3", "pivot1", "view4", "view5", "view5", "view6"),
    data = c(
      '{"rows":["bla","blubb"]}', '{"cols":["bla","blubb"]}',
      '{"rows":["ayaya","dudud"]}', '{"filter":["bla","blubb"]}',
      '{"filter":["bla","blubb"]}',
      '{"aggregations":["gagaga","blubb"]}',
      '{"aggregations":["gagaga","blubb"]}',
      '{"filter":["bla","blubb"]}'
    )
  )
  expect_error(views$loadConf(testViewData))
  expect_error(views$loadConf(testViewData,
    sandbox = FALSE, scenIds = c(1, 2, 3, 4),
    sidsToLoad = c(1, 5, 6, 7)
  ), NA)
  expect_error(views$loadConf(testViewData[1:4, ]), NA)
  expect_output(views$loadConf(testViewData[1:4, ]))
  expect_identical(views$getConf(), testViewData[1:4, -1])
})

test_that("Getting configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  expect_identical(
    views$get(fakeSessionIn1),
    list(view1 = list(rows = list("bla", "blubb")), view2 = list(
      cols = list("bla", "blubb")
    ))
  )
  expect_identical(
    views$getIds(fakeSessionIn1),
    c("view1", "view2")
  )
  expect_identical(
    views$get(fakeSessionScen3Out2),
    list(view5 = list(aggregations = list("gagaga", "blubb")))
  )
  expect_identical(
    views$getIds(fakeSessionScen3Out2),
    "view5"
  )
  expect_identical(
    views$get(fakeSessionIn1, "view2"),
    list(cols = list("bla", "blubb"))
  )
  expect_identical(
    views$get(fakeSessionScen3Out2, "view5"),
    list(aggregations = list("gagaga", "blubb"))
  )
  expect_error(views$get(fakeSessionIn1, "view4"))
  expect_error(views$get(fakeSessionScen3Out2, "view6"))
})

test_that("Adding configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  fakeSessionScen0Out2 <- FakeSession$new("tab_0_2")

  expect_error(views$add(fakeSessionIn1, "view1", list(cols = list("a", "b"))), NA)
  expect_identical(
    views$get(fakeSessionIn1, "view1"),
    list(cols = list("a", "b"))
  )
  expect_error(views$add(fakeSessionScen3Out2, "view6", list(cols = list("a", "b"))))
  expect_error(views$add(fakeSessionScen0Out2, "pivot123", list(cols = list("a", "b"))), NA)
})

test_that("Removing configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen4Out2 <- FakeSession$new("tab_4_2")

  expect_error(views$removeConf(list(c("in1", "view1"))), NA)
  expect_error(views$removeConf(list(c("out2", "view1")), scenId = "4"))
  expect_error(views$get(fakeSessionScen4Out2, "view5"), NA)
  expect_error(views$removeConf(list(c("out2", "view5")), scenId = "4"), NA)
  expect_error(views$get(fakeSessionIn1, "view1"))
  expect_error(views$get(fakeSessionScen4Out2, "view5"))
  expect_identical(
    views$get(fakeSessionIn1, "view2"),
    list(cols = list("bla", "blubb"))
  )
})

test_that("Adding configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")

  expect_identical(
    views$getConf(),
    tibble(
      symName = c("in1", "in3", "_pivotcomp_out2", "_pivotcomp_out2"),
      id = c("view2", "view3", "pivot1", "pivot123"),
      data = c(
        '{"cols":["bla","blubb"]}',
        '{"rows":["ayaya","dudud"]}',
        '{"filter":["bla","blubb"]}',
        '{"cols":["a","b"]}'
      )
    )
  )
  expect_identical(
    views$getConf(scenId = "4"),
    tibble(
      symName = c("out2"),
      id = c("view6"),
      data = c('{"filter":["bla","blubb"]}')
    )
  )
  expect_output(views$addConf(list(
    in10 = list(
      new1 = list(a = "b"),
      view2 = list(b = "c")
    ),
    `_pivotcomp_in10` = list(bla = list(u = "i")),
    `_customcomp_bla1234` = list(a = "c")
  )))
  expect_identical(views$getInvalidViews(), c("in10", "_customcomp_bla1234"))
  expect_error(views$addConf(list(
    in1 = list(
      new1 = list(a = "b"),
      view2 = list(b = "c")
    ),
    out3 = list(bla = list(cols = list("bla", "bli", "blubb")))
  )), NA)
  expect_identical(views$getInvalidViews(), NULL)
  expect_identical(
    views$getDuplicatedViews(),
    list(in1 = "view2")
  )
  expect_error(views$addConf(list(in1 = list(
    new3 = list(a = "b"),
    new4 = list(b = "c")
  )), scenId = "4"), NA)
  expect_identical(
    views$getConf(),
    tibble(
      symName = c("in1", "in1", "in3", "_pivotcomp_out2", "_pivotcomp_out2", "out3"),
      id = c("view2", "new1", "view3", "pivot1", "pivot123", "bla"),
      data = c(
        '{"cols":["bla","blubb"]}',
        '{"a":"b"}',
        '{"rows":["ayaya","dudud"]}',
        '{"filter":["bla","blubb"]}',
        '{"cols":["a","b"]}',
        '{"cols":["bla","bli","blubb"]}'
      )
    )
  )
  expect_identical(
    views$getConf(scenId = "4"),
    tibble(
      symName = c("out2", "in1", "in1"),
      id = c("view6", "new3", "new4"),
      data = c(
        '{"filter":["bla","blubb"]}',
        '{"a":"b"}',
        '{"b":"c"}'
      )
    )
  )
})

test_that("Get Configuration JSON works", {
  expect_identical(
    views$getJSON(),
    structure('{"in1":{"view2":{"cols":["bla","blubb"]},"new1":{"a":"b"}},"in3":{"view3":{"rows":["ayaya","dudud"]}},"_pivotcomp_out2":{"pivot1":{"filter":["bla","blubb"]},"pivot123":{"cols":["a","b"]}},"out3":{"bla":{"cols":["bla","bli","blubb"]}}}',
      class = "json"
    )
  )
  expect_identical(
    views$getJSON(list(c("in1", "view2"))),
    structure('{"in1":{"view2":{"cols":["bla","blubb"]}}}',
      class = "json"
    )
  )
  expect_identical(
    views$getJSON(list(c("in1", "view2"), c("in1", "new1"))),
    structure('{"in1":{"view2":{"cols":["bla","blubb"]},"new1":{"a":"b"}}}',
      class = "json"
    )
  )
  expect_identical(
    views$getJSON(list(c("in1", "new1"), c("in3", "view3"), c("in4", "nono"))),
    structure('{"in1":{"new1":{"a":"b"}},"in3":{"view3":{"rows":["ayaya","dudud"]}}}',
      class = "json"
    )
  )
  expect_identical(
    views$getJSON(scenId = "4"),
    structure('{"out2":{"view6":{"filter":["bla","blubb"]}},"in1":{"new3":{"a":"b"},"new4":{"b":"c"}}}',
      class = "json"
    )
  )
})

test_that("Removing configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  fakeSessionScen0Out2 <- FakeSession$new("tab_0_2")

  expect_error(views$remove(fakeSessionIn1, "new1"), NA)
  expect_error(views$remove(fakeSessionIn1, "new1"))
  expect_error(views$remove(fakeSessionScen3Out2, "new1"))
  expect_identical(
    views$getConf(),
    tibble(
      symName = c("in1", "in3", "_pivotcomp_out2", "_pivotcomp_out2", "out3"),
      id = c("view2", "view3", "pivot1", "pivot123", "bla"),
      data = c(
        '{"cols":["bla","blubb"]}',
        '{"rows":["ayaya","dudud"]}',
        '{"filter":["bla","blubb"]}',
        '{"cols":["a","b"]}',
        '{"cols":["bla","bli","blubb"]}'
      )
    )
  )
  expect_error(views$remove(fakeSessionScen0Out2, "pivot1"), NA)
  expect_error(views$remove(fakeSessionScen0Out2, "pivot123"), NA)
  expect_identical(
    views$getConf(),
    tibble(
      symName = c("in1", "in3", "out3"),
      id = c("view2", "view3", "bla"),
      data = c(
        '{"cols":["bla","blubb"]}',
        '{"rows":["ayaya","dudud"]}',
        '{"cols":["bla","bli","blubb"]}'
      )
    )
  )
})

test_that("Callback functions work", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen0In1 <- FakeSession$new("tab_0_4")

  testEnv <- new.env(parent = emptyenv())
  testEnv0 <- new.env(parent = emptyenv())
  testEnv$a <- 1L
  testEnv0$a <- 4L

  testModule <- function(env, session) {
    a <- 1L
    updateEnv <- function() {
      env$a <- a
    }
    testCallback <- function() {
      a <<- a + 1L
      updateEnv()
    }
    expect_error(views$registerUpdateCallback(session, testCallback), NA)
  }
  testModule(testEnv, fakeSessionIn1)
  testModule(testEnv0, fakeSessionScen0In1)

  expect_error(views$addConf(list(in1 = list(
    new1 = list(a = "b"),
    new2 = list(a = "c")
  ))), NA)
  expect_identical(testEnv$a, 2L)
  expect_error(views$removeConf(list(c("in1", "new1"), c("in1", "new2"))), NA)
  expect_identical(testEnv$a, 3L)

  expect_identical(testEnv0$a, 4L)
  expect_error(views$addConf(list(`_pivotcomp_in1` = list(
    new1 = list(a = "b"),
    new2 = list(a = "c")
  ), `_customcomp_def467` = list(
    view = list(a = "38")
  ))), NA)
  expect_identical(testEnv0$a, 2L)
  expect_error(views$removeConf(list(
    c("_pivotcomp_in1", "new1"),
    c("_pivotcomp_in1", "new2")
  )), NA)
  expect_identical(testEnv0$a, 3L)
})

test_that("Getting views summary works", {
  expect_output(views$getSummary(
    list(
      in1 = list(alias = "input 1"),
      in2 = list(alias = "input 2")
    ),
    list(
      out1 = list(alias = "output 1"),
      out2 = list(alias = "output 2"),
      out4 = list(alias = "output 4"),
      out3 = list(alias = "output 3")
    )
  ))
  expect_identical(
    views$getSummary(
      list(
        in1 = list(alias = "input 1"),
        in2 = list(alias = "input 2")
      ),
      list(
        out1 = list(alias = "output 1"),
        out2 = list(alias = "output 2"),
        out4 = list(alias = "output 4"),
        out3 = list(alias = "output 3")
      )
    ),
    list(
      symName = c("out3", "in1", "in3", "_customcomp_def467"),
      symAlias = c(
        "output 3",
        "input 1", "in3", "Second cool analyzer"
      ),
      id = c("bla", "view2", "view3", "view")
    )
  )
  expect_error(views$removeConf(list(
    c("_customcomp_def467", "view")
  )), NA)
  expect_error(views$addConf(list(
    out2 = list(
      new1 = list(a = "b"),
      new2 = list(a = "c")
    ),
    `_pivotcomp_out2` = list(pivot3 = list(b = "c")),
    `_pivotcomp_out1` = list(
      pivot1 = list(b = "c"),
      pivot2 = list(def = "bla")
    ),
    out1 = list(new1 = list(bla = "blablabla")),
    in2 = list(burriko = list(asd = "def")),
    in1 = list(view1 = list(
      mama = "best",
      cookie = "jar"
    ))
  )), NA)
  expect_identical(
    views$getSummary(
      list(
        in1 = list(alias = "input 1"),
        in2 = list(alias = "input 2")
      ),
      list(
        out1 = list(alias = "output 1"),
        out2 = list(alias = "output 2"),
        out4 = list(alias = "output 4"),
        out3 = list(alias = "output 3")
      )
    ),
    list(
      symName = c(
        "out1", "out2", "out2", "out3", "in1", "in1",
        "in2", "in3", "_pivotcomp_out1", "_pivotcomp_out1", "_pivotcomp_out2"
      ),
      symAlias = c(
        "output 1", "output 2", "output 2",
        "output 3", "input 1", "input 1", "input 2", "in3", "Pivot Comparison: output 1",
        "Pivot Comparison: output 1", "Pivot Comparison: output 2"
      ),
      id = c(
        "new1", "new1", "new2", "bla", "view2", "view1", "burriko", "view3", "pivot1", "pivot2",
        "pivot3"
      )
    )
  )
  expect_identical(
    views$getSummary(
      list(
        in1 = list(alias = "input 1"),
        in2 = list(alias = "input 2")
      ),
      list(
        out1 = list(alias = "output 1"),
        out2 = list(alias = "output 2"),
        out3 = list(alias = "output 3")
      ),
      scenId = "4"
    ),
    list(
      symName = c("out2", "in1", "in1"),
      symAlias = c("output 2", "input 1", "input 1"),
      id = c("view6", "new3", "new4")
    )
  )
  expect_identical(
    views$getSummary(
      list(
        in1 = list(alias = "input 1"),
        in2 = list(alias = "input 2")
      ),
      list(
        out1 = list(alias = "output 1"),
        out2 = list(alias = "output 2"),
        out3 = list(alias = "output 3")
      ),
      scenId = "10"
    ),
    list(symName = NULL, symAlias = NULL, id = NULL)
  )
})

test_that("Duplicating sandbox configuration works", {
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")

  expect_identical(views$getIds(fakeSessionScen3Out2), c("view5"))
  views$duplicateSandboxConf("3")
  expect_identical(views$getIds(fakeSessionScen3Out2), c("new1", "new2"))
  expect_identical(
    views$get(fakeSessionScen3Out2),
    list(
      new1 = list(a = "b"),
      new2 = list(a = "c")
    )
  )
})

test_that("Clearing configuration works", {
  expect_error(views$clearConf(), NA)
  expect_identical(
    views$getConf(),
    tibble()
  )
  expect_error(views$clearConf(scenId = "4"), NA)
  expect_identical(
    views$getConf(scenId = "4"),
    tibble()
  )
})

test_that("Loading scenario with no view config and one that does have views works", {
  testViewData <- tibble(
    "_sid" = c(1, 1),
    symName = c("in1", "in1"),
    id = c("view1", "view2"),
    data = c('{"rows":["bla","blubb"]}', '{"cols":["bla","blubb"]}')
  )
  expect_error(views$loadConf(testViewData, sandbox = FALSE, scenIds = c(4, 5), sidsToLoad = c(2, 1)), NA)
  expect_identical(views$getConf("4"), tibble())
  expect_identical(views$getConf("5"), testViewData[, -1])
})

test_that("View IDs should be stripped of spaces", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  expect_error(views$add(fakeSessionIn1, "\n view1 \t", list(cols = list("a", "b"))), NA)
  expect_identical(
    views$getConf(),
    tibble(
      symName = "in1", id = "view1",
      data = "{\"cols\":[\"a\",\"b\"]}"
    )
  )
  expect_error(views$remove(fakeSessionIn1, " view1 "), NA)
  expect_error(views$addConf(list(out2 = list(
    ` new1  \t` = list(a = "b"),
    `  new2` = list(a = "c")
  ))), NA)
  expect_identical(
    views$getConf(),
    tibble(
      symName = c("out2", "out2"),
      id = c("new1", "new2"),
      data = c(
        "{\"a\":\"b\"}",
        "{\"a\":\"c\"}"
      )
    )
  )
  testViewData <- tibble(
    "_sid" = c(1, 1),
    symName = c("in1", "out2"),
    id = c("view1 ", " view6\t"),
    data = c(
      '{"rows":["bla","blubb"]}',
      '{"filter":["bla","blubb"]}'
    )
  )
  expect_error(views$loadConf(testViewData), NA)
  expect_identical(
    views$getConf(),
    tibble(
      symName = c("in1", "out2"),
      id = c("view1", "view6"),
      data = c(
        '{"rows":["bla","blubb"]}',
        '{"filter":["bla","blubb"]}'
      )
    )
  )
})

test_that("Global views work", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  views$setGlobalViews(list(in1 = list(view1 = list(asd = "def"), view3 = list(def = "ghi"))))
  expect_identical(views$getIds(fakeSessionIn1), c("view1", "view3"))
  expect_identical(views$getIds(fakeSessionIn1, "global"), c("view1", "view3"))
  expect_identical(views$getIds(fakeSessionIn1, "local"), c("view1"))
  expect_identical(views$get(fakeSessionIn1), list(view1 = list(rows = list("bla", "blubb")), view3 = list(def = "ghi")))
  expect_identical(views$get(fakeSessionIn1, "view1"), list(rows = list("bla", "blubb")))
  expect_identical(views$get(fakeSessionIn1, "view1", "local"), list(rows = list("bla", "blubb")))
  expect_identical(views$get(fakeSessionIn1, "view1", "global"), list(asd = "def"))
  expect_identical(views$get(fakeSessionIn1, "view3"), list(def = "ghi"))
  expect_error(views$get(fakeSessionIn1, "view3", "local"), regexp = "could not be found")
  expect_identical(views$get(fakeSessionIn1, "view3"), list(def = "ghi"))
  expect_identical(views$get(fakeSessionIn1, filter = "global"), list(view1 = list(asd = "def"), view3 = list(def = "ghi")))
  expect_identical(views$get(fakeSessionIn1, filter = "local"), list(view1 = list(rows = list("bla", "blubb"))))
  expect_error(views$remove(fakeSessionIn1, "view3"), regexp = "does not exist")
  expect_error(views$remove(fakeSessionIn1, "view1"), NA)
  expect_identical(views$get(fakeSessionIn1), list(view1 = list(asd = "def"), view3 = list(def = "ghi")))
  expect_error(views$get(fakeSessionIn1, "view1", "local"), "could not be found")
})
