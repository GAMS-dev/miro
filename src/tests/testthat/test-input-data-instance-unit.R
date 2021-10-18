context("Unit tests - Input Data instance class")
library(tidyr)
library(dplyr)

source("../../components/input_data_instance.R")

ioConfig <<- list(
  modelInRaw = list(`_scalars` = list(
    symnames = c("maxstock", "trainingdays", "solver", "clearvalueset"),
    symtypes = c("parameter", "parameter", "set", "set")
  )),
  modelIn = list(solver = list(type = "dropdown", dropdown = list(clearValue = FALSE))),
  DDPar = c(
    "__cl__gmspar_daterange_lo",
    "__cl__gmspar_daterange_up",
    "_gmspar_date",
    "_gmspar_sliderrange_lo",
    "_gmspar_sliderrange_up"
  ),
  GMSOpt = c("_gmsopt_lsttitleleftaligned")
)
scalarDf <- tibble(
  scalar = c("maxstock", "trainingdays", "solver", "clearvalueset"),
  description = "",
  value = c("3", "7", "CPLEX", "element text")
)
clArgsDf <- tibble(
  scalar = c(
    "_gmsopt_lsttitleleftaligned",
    "_gmspar_date",
    "_gmspar_daterange_lo",
    "_gmspar_daterange_up",
    "_gmspar_numericinput",
    "_gmspar_sliderrange_lo",
    "_gmspar_sliderrange_up",
    "_gmspar_textinput"
  ),
  description = "",
  value = c(
    "1", "2020-07-15", NA_character_, NA_character_,
    "4000.56", "7", "22", NA_character_
  )
)
priceDf <- tibble(
  date = structure(16801, class = "Date"),
  symbol = "ABC", value = 300.1234
)

test_that("Generating hashes works", {
  inDataInst <- InputDataInstance$new()
  inDataInst$push("maptest", tibble(i = "i1", j = "j1", value = 1.2345))
  inDataInst$push("price", priceDf)
  inDataInst$push("_scalars", scalarDf)
  inDataInst$pushClArgs(clArgsDf)
  inDataInst$addFilePaths(file.path(getwd(), "..", "data", c("bad-views.json", "pickstock_negative_price.gdx")))
  expectedHash <- "7546dec8b27546a2a1fd48ea58f8ca2f314c3b70ba9b6bb8acbdf79da82352d4"
  expect_identical(inDataInst$generateScenHash(), expectedHash)

  # order of cl args should not matter
  shuffledRows <- sample(nrow(clArgsDf))

  inDataInst <- InputDataInstance$new()
  inDataInst$push("maptest", tibble(i = "i1", j = "j1", value = 1.2345))
  inDataInst$push("price", priceDf)
  inDataInst$push("_scalars", scalarDf)
  inDataInst$addFilePaths(file.path(getwd(), "..", "data", c("pickstock_negative_price.gdx", "bad-views.json")))
  inDataInst$pushClArgs(clArgsDf[shuffledRows, ])
  expect_identical(inDataInst$generateScenHash(), expectedHash)

  # order of non-cl args SHOULD matter
  inDataInst <- InputDataInstance$new()
  inDataInst$push("price", priceDf)
  inDataInst$push("maptest", tibble(i = "i1", j = "j1", value = 1.2345))
  inDataInst$push("_scalars", scalarDf)
  inDataInst$pushClArgs(clArgsDf)
  inDataInst$addFilePaths(file.path(getwd(), "..", "data", c("bad-views.json", "pickstock_negative_price.gdx")))
  expect_true(!identical(inDataInst$generateScenHash(), expectedHash))

  expect_identical(inDataInst$get("price"), priceDf)
})
