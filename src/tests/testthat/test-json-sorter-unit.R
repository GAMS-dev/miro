context("Unit tests - JSONSorter class")
library(V8)
library(jsonlite)

source("../../components/json_sorter.R")

test_that("sort() orders object keys alphabetically, case-insensitively, recursively", {
  sorter <- JSONSorter$new("../../JS")
  input <- toJSON(list(Zebra = 1, apple = list(Banana = 2, aardvark = 3), mango = 4), auto_unbox = TRUE)
  sorted <- sorter$sort(input)
  parsed <- fromJSON(sorted, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_identical(names(parsed), c("apple", "mango", "Zebra"))
  expect_identical(names(parsed$apple), c("aardvark", "Banana"))
})

test_that("sort() preserves array element order", {
  sorter <- JSONSorter$new("../../JS")
  sorted <- sorter$sort("[3,1,2]")
  parsed <- fromJSON(sorted, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_equal(parsed, c(3, 1, 2))
})

test_that("write() preserves null values inside a JSON array through a read-write round trip", {
  data <- fromJSON("[null,1,2]", simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  tf <- tempfile(fileext = ".json")
  on.exit(unlink(tf))
  sorter <- JSONSorter$new("../../JS", filePath = tf)
  sorter$write(data)
  writtenText <- paste(readLines(tf), collapse = "")
  expect_identical(gsub("\\s", "", writtenText), "[null,1,2]")
  reparsed <- fromJSON(tf, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_equal(reparsed, c(NA, 1, 2))
})

test_that("write() uses the filePath argument over the constructor default", {
  tf1 <- tempfile(fileext = ".json")
  tf2 <- tempfile(fileext = ".json")
  on.exit(unlink(c(tf1, tf2)))
  sorter <- JSONSorter$new("../../JS", filePath = tf1)
  sorter$write(list(a = 1), filePath = tf2)
  expect_false(file.exists(tf1))
  expect_true(file.exists(tf2))
})
