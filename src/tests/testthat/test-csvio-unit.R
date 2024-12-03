context("Unit tests - CsvIO class")
library(readr)

lang <<- jsonlite::fromJSON("../../conf/en.json", simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
source("../../components/localfileio.R")
source("../../components/csvio.R")

ioConfig <<- list(
  modelOut = list(
    "_scalars_out" = list(
      symnames = c("cowf", "explimitgr", "big", "pawat", "pafod", "tolcnl", "tolpr", "tolnwfp", "betaf"),
      symtypes = c("parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter"),
      symtext = c("", "", "", "", "", "", "", "", ""),
      colTypes = "ccc",
      alias = "Output scalars",
      headers = list(
        scalar = list(),
        description = list(),
        value = list()
      )
    ),
    "_scalarsve_out" = list(symnames = c("z"), symtext = c("objective function value"), symtypes = c("variable"), colTypes = "ccddddd", headers = list(scalar = list(), description = list(), level = list(), marginal = list(), lower = list(), upper = list(), scale = list())),
    var1 = list(symtype = "variable", colTypes = "cddddd", headers = list(uni = list(), level = list(), marginal = list(), lower = list(), upper = list(), scale = list())),
    var1a = list(symtype = "variable", colTypes = "cddddd", headers = list(uni = list(), level = list(), marginal = list(), lower = list(), upper = list(), scale = list())),
    var1b = list(symtype = "variable", colTypes = "cddddd", headers = list(uni = list(), level = list(), marginal = list(), lower = list(), upper = list(), scale = list())),
    eq1 = list(symtype = "equation", colTypes = "ccddddd", headers = list(uni1 = list(), uni2 = list(), level = list(), marginal = list(), lower = list(), upper = list(), scale = list())),
    i1 = list(alias = "Set i1", symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i1a = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i2 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i3 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i4 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i4a = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i5 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i6 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i6a = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i6b = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    j4 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i7 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i8 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i9 = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
    i10 = list(symtype = "set", colTypes = "ccc", headers = list(uni1 = list(), uni2 = list(), text = list())),
    i11 = list(symtype = "set", colTypes = "cccc", headers = list(uni1 = list(), uni2 = list(), uni3 = list(), text = list())),
    i11sparse = list(symtype = "set", colTypes = "cccc", headers = list(uni1 = list(), uni2 = list(), uni3 = list(), text = list())),
    i11dense = list(symtype = "set", colTypes = "cccc", headers = list(uni1 = list(), uni2 = list(), uni3 = list(), text = list())),
    i11yn = list(symtype = "set", colTypes = "cccc", headers = list(uni1 = list(), uni2 = list(), uni3 = list(), text = list())),
    i11nodata = list(symtype = "set", colTypes = "cccc", headers = list(uni1 = list(), uni2 = list(), uni3 = list(), text = list()))
  ),
  modelInRaw = list(
    "_scalars" = list(
      symnames = c("baseyear", "repco", "gr", "growthq", "drc", "the1", "lstd", "trcap", "twcap", "twefac", "labfac"),
      symtext = c("", "", "", "", "", "", "", "", "", "", ""),
      symtypes = c("set", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter", "parameter"),
      colTypes = "ccc",
      alias = "Input scalars",
      headers = list(
        scalar = list(),
        description = list(),
        value = list()
      )
    ),
    "_gmsopt_solver" = list(),
    distance = list(alias = "Distance", symtype = "parameter", colTypes = "ccd", headers = list(uni1 = list(), uni2 = list(), value = list())),
    distance2 = list(symtype = "parameter", colTypes = "ccd", headers = list(uni1 = list(), uni2 = list(), value = list())),
    distance3 = list(alias = "Distance 3", symtype = "parameter", colTypes = "cdd", headers = list(uni = list(), dallas = list(), chicago = list())),
    distance3a = list(symtype = "parameter", colTypes = "cddd", headers = list(uni = list(), dallas = list(), chicago = list(), clevelaND = list())),
    distance4 = list(symtype = "parameter", colTypes = "ccdd", headers = list(uni1 = list(), uni2 = list(), chicago = list(), clevelaND = list())),
    modedistance = list(
      symtype = "parameter", colTypes = "cccd",
      headers = list(
        uni1 = list(), uni2 = list(), uni3 = list(),
        value = list()
      )
    ),
    modedistanceset = list(
      symtype = "set", colTypes = "cccc",
      headers = list(
        uni1 = list(), uni2 = list(), uni3 = list(),
        text = list()
      )
    ),
    modedistancesetnodata = list(
      symtype = "set", colTypes = "cccc",
      headers = list(
        uni1 = list(), uni2 = list(), uni3 = list(),
        text = list()
      )
    ),
    modedistancesetsparse = list(
      symtype = "set", colTypes = "cccc",
      headers = list(
        uni1 = list(), uni2 = list(), uni3 = list(),
        text = list()
      )
    ),
    modedistancesetdense = list(
      symtype = "set", colTypes = "cccc",
      headers = list(
        uni1 = list(), uni2 = list(), uni3 = list(),
        text = list()
      )
    ),
    specialVal = list(
      symtype = "parameter", colTypes = "cd",
      headers = list(
        uni = list(),
        value = list()
      )
    )
  )
)

csvio <- CsvIO$new()

csvioPrivate <- csvio$.__enclos_env__$private

test_that("Initialising CSV file works", {
  expect_error(csvio$rInitFile("../data/csvtest-ambig.csv"), class = "error_ambiguous_delim")
  expect_identical(csvio$getRDelim(), c(",", ";"))
  expect_identical(csvio$getHeaders(), character(0L))
  expect_error(csvio$rInitFile("../data/csvtest-2.tsv"), NA)
  expect_identical(csvio$getRDelim(), "\t")
  expect_identical(csvio$getHeaders(), c("header 1", "header 2", "Value", "header 4"))
  expect_error(csvio$rInitFile("../data/csvtest-1.csv"), NA)
  expect_identical(csvio$getRDelim(), ";")
  expect_identical(csvio$getHeaders(), c("header 1", "header 2", "Value", "header 4"))
})

test_that("Reading CSV file works", {
  expect_error(csvio$setColsToRead(c("header 2", "header 1", "Value"), "distance"), NA)
  expect_identical(
    csvio$read("../data/csvtest-1.csv", "distance", decimalSep = ","),
    tibble(
      uni1 = paste0("i", 1:7),
      uni2 = paste0("j", 1:7),
      value = c(12.34, 13.470, 16.471, 11, 13.477, 7, 13.4791)
    )
  )
  expect_identical(
    csvio$read("../data/csvtest-3.sv", "distance4", colsToRead = c("header 2", "header 1", "Value 2", "Value 1")),
    tibble(
      uni1 = paste0("i", 1:7),
      uni2 = paste0("j", 1:7),
      chicago = as.numeric(1:7),
      clevelaND = c(12.34, 13.470, 16.471, 11, 13.477, 7, 13.4791)
    )
  )
  expect_identical(
    csvio$read("../data/csvtest-3.sv", "distance4", colsToRead = c("header 2", "header 1", "-", "Value 2")),
    tibble(
      uni1 = paste0("i", 1:7),
      uni2 = paste0("j", 1:7),
      chicago = NA_character_,
      clevelaND = as.numeric(1:7)
    )
  )
  expect_error(csvio$rInitFile("../data/csvtest-ambig.csv"), class = "error_ambiguous_delim")
  expect_error(csvio$read("../data/csvtest-ambig.csv", "specialVal"), class = "error_ambiguous_delim")
  expect_error(csvio$setRDelim(";"), NA)
  expect_equal(csvio$read("../data/csvtest-ambig.csv", "i1")[], tibble(uni = "j1", text = "12,34"))
  expect_equal(csvio$read("../data/csvtest-ambig.csv", "i1", delim = ",")[], tibble(uni = "j1;12", text = "34"))
})

test_that("Setting symbol name should prevent reading symbol with different name", {
  expect_error(csvio$rInitFile("../data/csvtest-1.csv"), NA)
  expect_error(csvio$setRSymName("distance"), NA)
  expect_error(csvio$read("../data/csvtest-1.csv", "distance2"), class = "error_notfound")
  expect_identical(
    csvio$read("../data/csvtest-1.csv", "distance",
      decimalSep = ",",
      colsToRead = c("header 2", "header 1", "Value")
    ),
    tibble(
      uni1 = paste0("i", 1:7),
      uni2 = paste0("j", 1:7),
      value = c(12.34, 13.470, 16.471, 11, 13.477, 7, 13.4791)
    )
  )
})

test_that("Guessing delimiter works", {
  expect_identical(csvioPrivate$guessDelim(c("uni,text", "fCapital,")), ",")
  expect_identical(csvioPrivate$guessDelim(c("i,j,value", "\"San,Diego\",New-York,2.5")), ",")
})
