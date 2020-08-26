context("Unit tests - views class")

library(jsonlite)
library(futile.logger)

source("../../components/views.R")

FakeSession <- R6Class("FakeSession", public = list(
  initialize = function(id){
    private$id <- id
    return(invisible(self))
  },
  ns = function(id){
    return(paste0(private$id, "-", id))
  }
), private = list(
  id = NULL
))

views <- Views$new(c("in1", "widget1", "widget2", "in2"), c("out1", "out2", "out3"), c("in1", "in2"))

test_that("Loading/Unloading configuration works", {
  testViewData <- tibble("_sid" = c(1, 1, 1, 2, 3), 
                         symName = c("in1", "in1", "in3", "in2", "out2"),
                         id = c("view1", "view2", "view3", "view4", "view5"),
                         data = c('{"rows":["bla","blubb"]}', '{"cols":["bla","blubb"]}',
                                  '{"rows":["ayaya","dudud"]}', '{"filter":["bla","blubb"]}',
                                  '{"aggregations":["gagaga","blubb"]}'))
  expect_error(views$loadConf(testViewData))
  expect_error(views$loadConf(testViewData, sandbox = FALSE), NA)
  expect_error(views$loadConf(testViewData[1:3, ]), NA)
  expect_output(views$loadConf(testViewData[1:3, ]))
  expect_identical(views$getConf(), testViewData[1:3, -1])
})

test_that("Getting configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  expect_identical(views$get(fakeSessionIn1),
                   list(view1 = list(rows = list("bla", "blubb")), view2 = list(
                     cols = list("bla", "blubb"))))
  expect_identical(views$getIds(fakeSessionIn1),
                   c("view1", "view2"))
  expect_identical(views$get(fakeSessionScen3Out2),
                   list(view5 = list(aggregations = list("gagaga", "blubb"))))
  expect_identical(views$getIds(fakeSessionScen3Out2),
                   "view5")
  expect_identical(views$get(fakeSessionIn1, "view2"),
                   list(cols = list("bla", "blubb")))
  expect_identical(views$get(fakeSessionScen3Out2, "view5"),
                   list(aggregations = list("gagaga", "blubb")))
  expect_error(views$get(fakeSessionIn1, "view4"))
  expect_error(views$get(fakeSessionScen3Out2, "view6"))
})

test_that("Adding configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  expect_error(views$add(fakeSessionIn1, "view1", list(cols = list("a", "b"))), NA)
  expect_identical(views$get(fakeSessionIn1, "view1"),
                   list(cols = list("a", "b")))
  expect_error(views$add(fakeSessionScen3Out2, "view6", list(cols = list("a", "b"))))
})

test_that("Removing configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  
  expect_error(views$removeConf(list(c("in1", "view1"))), NA)
  expect_error(views$get(fakeSessionIn1, "view1"))
  expect_identical(views$get(fakeSessionIn1, "view2"),
                   list(cols = list("bla","blubb")))
})

test_that("Adding configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  
  expect_identical(views$getConf(),
                   tibble(symName = c("in1", "in3"),
                          id = c("view2", "view3"),
                          data = c('{"cols":["bla","blubb"]}',
                                   '{"rows":["ayaya","dudud"]}')))
  expect_output(views$addConf(list(in10 = list(new1 = list(a = "b"),
                                               view2 = list(b = "c")))))
  expect_identical(views$getInvalidViews(), "in10")
  expect_error(views$addConf(list(in1 = list(new1 = list(a = "b"),
                                             view2 = list(b = "c")),
                                  out3 = list(bla = list(cols = list("bla", "bli", "blubb"))))), NA)
  expect_identical(views$getInvalidViews(), NULL)
  expect_identical(views$getDuplicatedViews(),
                   list(in1 = "view2"))
  expect_identical(views$getConf(),
                   tibble(symName = c("in1", "in1", "in3", "out3"),
                          id = c("view2", "new1", "view3", "bla"),
                          data = c('{"cols":["bla","blubb"]}',
                                   '{"a":"b"}',
                                   '{"rows":["ayaya","dudud"]}',
                                   '{"cols":["bla","bli","blubb"]}')))
})

test_that("Get Configuration JSON works", {
  expect_identical(views$getJSON(),
                   structure('{"in1":{"view2":{"cols":["bla","blubb"]},"new1":{"a":"b"}},"in3":{"view3":{"rows":["ayaya","dudud"]}},"out3":{"bla":{"cols":["bla","bli","blubb"]}}}',
                             class = "json"))
  expect_identical(views$getJSON(list(c("in1", "view2"))),
                   structure('{"in1":{"view2":{"cols":["bla","blubb"]}}}',
                             class = "json"))
  expect_identical(views$getJSON(list(c("in1", "view2"), c("in1", "new1"))),
                   structure('{"in1":{"view2":{"cols":["bla","blubb"]},"new1":{"a":"b"}}}',
                             class = "json"))
  expect_identical(views$getJSON(list(c("in1", "new1"), c("in3", "view3"), c("in4", "nono"))),
                   structure('{"in1":{"new1":{"a":"b"}},"in3":{"view3":{"rows":["ayaya","dudud"]}}}',
                             class = "json"))
})

test_that("Removing configuration works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  
  expect_error(views$remove(fakeSessionIn1, "new1"), NA)
  expect_error(views$remove(fakeSessionIn1, "new1"))
  expect_error(views$remove(fakeSessionScen3Out2, "new1"))
  expect_identical(views$getConf(),
                   tibble(symName = c("in1", "in3", "out3"),
                          id = c("view2", "view3", "bla"),
                          data = c('{"cols":["bla","blubb"]}',
                                   '{"rows":["ayaya","dudud"]}',
                                   '{"cols":["bla","bli","blubb"]}')))
})

test_that("Callback functions work", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  
  testEnv <- new.env(parent = emptyenv())
  testEnv$a <- 1L
  
  testModule <- function(env){
    a <- 1L
    updateEnv <- function(){
      env$a <- a
    }
    testCallback <- function(){
      a <<- a + 1L
      updateEnv()
    }
    expect_error(views$registerUpdateCallback(fakeSessionIn1, testCallback), NA)
  }
  testModule(testEnv)
  
  expect_error(views$addConf(list(in1 = list(new1 = list(a = "b"),
                                             new2 = list(a = "c")))), NA)
  expect_identical(testEnv$a, 2L)
  expect_error(views$removeConf(list(c("in1", "new1"), c("in1", "new2"))), NA)
  expect_identical(testEnv$a, 3L)
})

test_that("Getting views summary works", {
  expect_output(views$getSummary(list(in1 = list(alias = "input 1"),
                                      in2 = list(alias = "input 2")),
                                 list(out1 = list(alias = "output 1"),
                                      out2 = list(alias = "output 2"),
                                      out3 = list(alias = "output 3"))))
  expect_identical(views$getSummary(list(in1 = list(alias = "input 1"),
                                         in2 = list(alias = "input 2")),
                                    list(out1 = list(alias = "output 1"),
                                         out2 = list(alias = "output 2"),
                                         out3 = list(alias = "output 3"))),
                   list(symName = c("out3", "in1", "in3"),
                        symAlias = c("output 3", 
                                     "input 1", "in3"),
                        id = c("bla", "view2", "view3")))
  expect_error(views$addConf(list(out2 = list(new1 = list(a = "b"),
                                              new2 = list(a = "c")),
                                  out1 = list(new1 = list(bla = "blablabla")),
                                  in2 = list(burriko = list(asd = "def")),
                                  in1 = list(view1 = list(mama = "best",
                                                          cookie = "jar")))), NA)
  expect_identical(views$getSummary(list(in1 = list(alias = "input 1"),
                                         in2 = list(alias = "input 2")),
                                    list(out1 = list(alias = "output 1"),
                                         out2 = list(alias = "output 2"),
                                         out3 = list(alias = "output 3"))),
                   list(symName = c("out1", "out2", "out2", "out3", "in1", "in1",
                                    "in2", "in3"),
                        symAlias = c("output 1", "output 2", "output 2",
                                     "output 3", "input 1", "input 1", "input 2", "in3"),
                        id = c("new1", "new1", "new2", "bla", "view2", "view1", "burriko", "view3")))
})

test_that("Duplicating sandbox configuration works", {
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  
  expect_identical(views$getIds(fakeSessionScen3Out2), c("view5"))
  views$duplicateSandboxConf("3")
  expect_identical(views$getIds(fakeSessionScen3Out2), c("new1", "new2"))
  expect_identical(views$get(fakeSessionScen3Out2),
                   list(new1 = list(a = "b"),
                        new2 = list(a = "c")))
})

test_that("Clearing configuration works", {
  expect_error(views$clearConf(), NA)
  expect_identical(views$getConf(),
                   tibble())
})
