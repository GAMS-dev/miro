context("Unit tests - XlsIO class")
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)

lang <<- jsonlite::fromJSON("../../conf/en.json", simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
source("../../components/localfileio.R")
source("../../components/xlsio.R")

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
    i9a = list(symtype = "set", colTypes = "cc", headers = list(uni = list(), text = list())),
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
    distance3 = list(alias = "Distance 😈 3", symtype = "parameter", colTypes = "cdd", headers = list(uni = list(), dallas = list(), chicago = list())),
    distance3a = list(symtype = "parameter", colTypes = "cddd", headers = list(uni = list(), dallas = list(), chicago = list(), clevelaND = list())),
    distance4 = list(symtype = "parameter", colTypes = "ccdd", headers = list(uni1 = list(), uni2 = list(), chicago = list(), clevelaND = list())),
    modedistance = list(
      symtype = "parameter", colTypes = "cccd",
      headers = list(
        uni1 = list(), uni2 = list(), uni3 = list(),
        value = list()
      )
    ),
    modedistanceb = list(
      symtype = "parameter", colTypes = "ccccd",
      headers = list(
        uni1 = list(), uni2 = list(), uni3 = list(), uni4 = list(),
        value = list()
      )
    ),
    modedistancea = list(
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
    specialval = list(
      symtype = "parameter", colTypes = "cd",
      headers = list(
        uni = list(),
        value = list()
      )
    )
  )
)

xlsio <- XlsIO$new()

xlsioPrivate <- xlsio$.__enclos_env__$private

test_that("Converting Excel column spec to index works", {
  expect_identical(xlsioPrivate$excelColToIndex("A"), 1L)
  expect_identical(xlsioPrivate$excelColToIndex(c("A", "L")), c(1L, 12L))
  expect_identical(xlsioPrivate$excelColToIndex("CD"), 82L)
  expect_identical(xlsioPrivate$excelColToIndex(c("IV", "LH", "XFD", "287")), c(256L, 320L, 16384L, 287L))
  expect_error(xlsioPrivate$excelColToIndex(c("AAAA", "LH")), class = "error_parse_config")
  expect_error(xlsioPrivate$excelColToIndex(c("0", "LH")), class = "error_parse_config")
  expect_error(xlsioPrivate$excelColToIndex(c("A1", "LH")), class = "error_parse_config")
  expect_error(xlsioPrivate$excelColToIndex(c("Aa", "LH")), class = "error_parse_config")
})

test_that("Getting sheet names to cache works", {
  xlsioPrivate$getSheetsToCache(c(
    "\" asd efg!!bla!A1\"", " test1!A1:B12", "\" asd efg!!bla!B12\"",
    "\" asd!A12\"", "test1!A1:B12", " test1!X23 ", "asd!X191:Z10000"
  ))
  expect_identical(xlsioPrivate$cache, list(` asd efg!!bla` = NULL, test1 = NULL))
  expect_identical(xlsioPrivate$sheetRefCount, structure(c(2L, 3L),
    .Dim = 2L,
    .Dimnames = structure(list(c(" asd efg!!bla", "test1")),
      .Names = "sheetNames"
    ), class = "table"
  ))
})

test_that("Converting column ranges to index works", {
  expect_identical(xlsioPrivate$rangeToIndex("1,A:2, 5"), c(1L, 2L, 5L))
  expect_identical(xlsioPrivate$rangeToIndex("D, E"), c(4L, 5L))
  expect_error(xlsioPrivate$rangeToIndex("1,B:1,5"), class = "error_parse_config")
  expect_error(xlsioPrivate$rangeToIndex("1,AAAA:AAAB,5"), class = "error_parse_config")
  expect_error(xlsioPrivate$rangeToIndex("1,A:B:C,5"), class = "error_parse_config")
  expect_error(xlsioPrivate$rangeToIndex("1,A:B,0"), class = "error_parse_config")
})

test_that("Parsing cell range works", {
  rng <- xlsioPrivate$parseCellRange("idontexist", "sheet 1!B12:C14")
  expect_identical(rng$ul, c(12L, 2L))
  expect_identical(rng$sheet, "sheet 1")
  expect_identical(rng$lr, c(14L, 3L))
  rng <- xlsioPrivate$parseCellRange("idontexist", "sheet 2!A1")
  expect_identical(rng$ul, c(1L, 1L))
  expect_identical(rng$sheet, "sheet 2")
  expect_identical(rng$lr, c(NA_integer_, NA_integer_))
  rng <- xlsioPrivate$parseCellRange("baseyear", "sheet 3!C4")
  expect_identical(rng$ul, c(4L, 3L))
  expect_identical(rng$sheet, "sheet 3")
  expect_identical(rng$lr, c(4L, 3L))
  xlsioPrivate$rSheets <- paste0("sheet ", 1:3)
  rng <- xlsioPrivate$parseCellRange("idontexist", "sheet 2")
  expect_identical(rng$ul, c(1L, 1L))
  expect_identical(rng$sheet, "sheet 2")
  expect_identical(rng$lr, c(NA_integer_, NA_integer_))
  rng <- xlsioPrivate$parseCellRange("idontexist", "sheet 2!")
  expect_identical(rng$ul, c(1L, 1L))
  expect_identical(rng$sheet, "sheet 2")
  expect_identical(rng$lr, c(NA_integer_, NA_integer_))
  rng <- xlsioPrivate$parseCellRange("idontexist", "")
  expect_identical(rng$ul, c(1L, 1L))
  expect_identical(rng$sheet, "sheet 1")
  expect_identical(rng$lr, c(NA_integer_, NA_integer_))
  rng <- xlsioPrivate$parseCellRange("idontexist", "!")
  expect_identical(rng$ul, c(1L, 1L))
  expect_identical(rng$sheet, "sheet 1")
  expect_identical(rng$lr, c(NA_integer_, NA_integer_))
  rng <- xlsioPrivate$parseCellRange("idontexist", "!B3")
  expect_identical(rng$ul, c(3L, 2L))
  expect_identical(rng$sheet, "sheet 1")
  expect_identical(rng$lr, c(NA_integer_, NA_integer_))
  rng <- xlsioPrivate$parseCellRange("idontexist", "!B3:D4")
  expect_identical(rng$ul, c(3L, 2L))
  expect_identical(rng$sheet, "sheet 1")
  expect_identical(rng$lr, c(4L, 4L))
  expect_error(xlsioPrivate$parseCellRange("idontexist", "sheet 4"))
  expect_error(xlsioPrivate$parseCellRange("idontexist", "!sheet 1"))
})

test_that("Test that initialising index works", {
  xlsio$readIndex("../data/exampleData.xlsx", "index", forceInit = TRUE)
  rWarnings <- xlsio$getWarnings()
  expect_length(rWarnings, 4L)
  expect_true(any(grepl("'bla'", rWarnings, fixed = TRUE)))
  expect_true(any(grepl("non-empty rows", rWarnings, fixed = TRUE)))
  expect_true(any(grepl("Key=value", rWarnings, fixed = TRUE)))
  expect_true(any(grepl("Data type: 'dSet'.*'i10'", rWarnings)))
  expect_equal(xlsioPrivate$rIndex[["i10"]], tibble(
    symbol = "i10", range = "testIndex!B26:E27",
    rdim = 0L, cdim = 2L, dim = 2L, values = "noData", ignorerows = NA_character_, ignorecols = NA_character_, se = "1", squeeze = "1", dset = FALSE
  ))
  expect_equal(xlsioPrivate$rIndex[["modedistance"]], tibble(
    symbol = "modedistance", range = "testIndex!A26:E31",
    rdim = 1L, cdim = 2L, dim = 3L, values = "auto", ignorerows = NA_character_,
    ignorecols = NA_character_,
    se = "1", squeeze = "1", dset = FALSE
  ))
  expect_equal(xlsioPrivate$rIndex[["distance2"]], tibble(
    symbol = "distance2", range = "testIndex!A26:E31",
    rdim = 1L, cdim = 1L, dim = 2L, values = "auto", ignorerows = "27",
    ignorecols = "C, E", se = "1", squeeze = "1", dset = FALSE
  ))
})

test_that("Index errors work", {
  xlsio$readIndex("../data/exampleData.xlsx", "indexErrors", forceInit = TRUE)
  rWarnings <- xlsio$getWarnings()
  expect_true(any(grepl("'ignoreRovs'", rWarnings, fixed = TRUE)))
  expect_error(xlsio$readIndex("../data/exampleData.xlsx", "indexErrors!F9", forceInit = TRUE),
    class = "error_parse_config", regexp = "invalid column dimension"
  )
  expect_error(xlsio$readIndex("../data/exampleData.xlsx", "indexErrors!H14", forceInit = TRUE),
    class = "error_parse_config", regexp = "i1.+multiple times"
  )
})

test_that("Reading parameters works", {
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "distance", indexRange = "index!A1", forceInit = TRUE),
    tibble(
      uni1 = c("brussels", "brussels", "san francisco", "san francisco", "san francisco", "boston", "boston", "boston"),
      uni2 = c("cleveland", "chicago", "cleveland", "chicago", "dallas", "cleveland", "chicago", "dallas"),
      value = c(5000, 6000, 2200, 2000, 1800, 700, 900, 1500)
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "modedistance", indexRange = "index!A1"),
    tibble(
      uni1 = c("ship", "ship", "truck", "truck", "rail", "rail", "barge", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco", "san francisco", "san francisco", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago"),
      value = c(5000, 6000, 2200, 2000, 2200, 2000, 2800, 2800)
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "modedistancea", indexRange = "index!A1"),
    tibble(
      uni1 = c("ship", "truck"),
      uni2 = c("brussels", "brussels"),
      uni3 = c("cleveland", "cleveland"),
      value = c(100, 200)
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "modedistanceb", indexRange = "index!A1"),
    tibble(
      uni1 = c("a", "a"),
      uni2 = c("ship", "truck"),
      uni3 = c("brussels", "brussels"),
      uni4 = c("cleveland", "cleveland"),
      value = c(100, 200)
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "distance2", indexRange = "index!A1"),
    tibble(
      uni1 = c("ship", "truck", "rail", "barge"),
      uni2 = c("brussels", "san francisco", "san francisco", "san francisco"),
      value = c(5000, 2200, 2200, 2800)
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "specialval"),
    tibble(
      uni = c("v2", "v3", "v4", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18"),
      value = c(Inf, -Inf, Inf, NA, 770, NA, 3, NA, NA, NA, NA)
    )
  )
})
test_that("Reading tables works", {
  expect_error(xlsio$read("../data/exampleData.xlsx", "distance3", indexRange = "index!A1", forceInit = TRUE),
    class = "error_data"
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "distance3a"),
    tibble(
      uni = c("brussels", "san francisco", "boston"),
      dallas = c(0, 1800, 1500),
      chicago = c(6000, 2000, 900),
      cleveland = c(5000, 2200, 700)
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "distance4"),
    tibble(
      uni1 = c("ship", "truck", "rail", "barge"),
      uni2 = c("brussels", "san francisco", "san francisco", "san francisco"),
      chicago = c(6000, 2000, 2000, 2800),
      cleveland = c(5000, 2200, 2200, 2800)
    )
  )
})

test_that("Reading sets works", {
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i10", indexRange = "index", forceInit = TRUE),
    tibble(
      uni1 = c("brussels", "brussels", "san francisco", "san francisco"),
      uni2 = c("cleveland", "chicago", "cleveland", "chicago"),
      text = c("", "", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i9a"),
    tibble(
      uni = c("ship", "ship", "truck", "truck"),
      text = c("brussels", "cleveland", "brussels", "cleveland")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "j4"),
    tibble(
      uni = c("brussels", "san francisco", "boston"),
      text = c("", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i6"),
    tibble(
      uni = c("cleveland", "chicago", "dallas"),
      text = c("", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i6a"),
    tibble(
      uni = c("cleveland", "chicago", "dallas"),
      text = c("", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i6b"),
    tibble(
      uni = c("cleveland", "chicago", "dallas"),
      text = c("5000", "6000", "0")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i7"),
    tibble(
      uni = c("brussels", "san francisco"),
      text = c("", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i8"),
    tibble(
      uni = c("cleveland", "chicago"),
      text = c("5000", "6000")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i2"),
    tibble(
      uni = c("boats", "watercraft"),
      text = c("", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i3"),
    tibble(
      uni = c("new york", "chicago", "boston"),
      text = c("city1", "city2", "city3")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i4"),
    tibble(
      uni = c("new york", "chicago", "boston", "san francisco", "dallas", "houston"),
      text = c("", "", "", "", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i4a"),
    tibble(
      uni = c("new york", "chicago", "boston", "san francisco", "dallas"),
      text = c("", "", "", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "modedistanceset", indexRange = "index", forceInit = TRUE),
    tibble(
      uni1 = c("ship", "ship", "truck", "truck", "rail", "rail", "barge", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco", "san francisco", "san francisco", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago"),
      text = c("", "", "", "", "", "", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "modedistancesetnodata", indexRange = "index"),
    tibble(
      uni1 = c("ship", "ship", "ship", "ship", "truck", "truck", "truck", "truck", "rail", "rail", "rail", "rail", "barge", "barge", "barge", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco", "brussels", "brussels", "san francisco", "san francisco", "brussels", "brussels", "san francisco", "san francisco", "brussels", "brussels", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago"),
      text = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "modedistancesetdense", indexRange = "index"),
    tibble(
      uni1 = c("ship", "ship", "ship", "ship", "truck", "truck", "truck", "truck", "rail", "rail", "rail", "rail", "barge", "barge", "barge", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco", "brussels", "brussels", "san francisco", "san francisco", "brussels", "brussels", "san francisco", "san francisco", "brussels", "brussels", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago", "cleveland", "chicago"),
      text = c(
        "5000", "6000", "0", "0", "0", "0", "2200", "2000", "0", "0",
        "2200", "2000", "0", "0", "2800", "2800"
      )
    )
  )
  expect_error(xlsio$read("../data/exampleData.xlsx", "modedistancesetsparse", indexRange = "index"), class = "error_notfound")
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i11"),
    tibble(
      uni1 = c("ship", "truck", "rail", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago"),
      text = c("text 1", "", "text 2", "no")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i11dense"),
    tibble(
      uni1 = c("ship", "truck", "rail", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago"),
      text = c("text 1", "", "text 2", "no")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i11sparse"),
    tibble(
      uni1 = c("ship", "rail", "barge"),
      uni2 = c("brussels", "san francisco", "san francisco"),
      uni3 = c("cleveland", "cleveland", "chicago"),
      text = c("text 1", "text 2", "no")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i11yn"),
    tibble(
      uni1 = c("ship", "rail"),
      uni2 = c("brussels", "san francisco"),
      uni3 = c("cleveland", "cleveland"),
      text = c("", "")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i11nodata"),
    tibble(
      uni1 = c("ship", "truck", "rail", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago"),
      text = c("", "", "", "")
    )
  )
})

test_that("Reading scalars works", {
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "labfac", indexRange = "scalarIndex!C12:H17", forceInit = TRUE),
    "0.5"
  )
  rWarnings <- xlsio$getWarnings()
  expect_length(rWarnings, 1L)
  expect_true(any(grepl("scalar", rWarnings, fixed = TRUE)))
  expect_identical(xlsio$read("../data/exampleData.xlsx", "growthq"), "1988")
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "_scalars"),
    tibble(
      scalar = c("baseyear", "repco", "gr", "growthq", "drc", "the1", "lstd", "trcap", "twcap", "twefac", "labfac", "_gmsopt_solver"),
      description = c("", "", "", "", "", "", "", "", "", "", "", ""),
      value = c("1988", "2.5", "0.3", "2.5", "0.15", NA_character_, "200", "250", "59.504100000000001", "0.5", "0.5", "CBC")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "_scalars_out"),
    tibble(
      scalar = c("cowf", "explimitgr", "big", "pawat", "pafod", "tolcnl", "tolpr", "tolnwfp", "betaf"),
      description = "",
      value = c("0.5", "5", "4000", "999999", "1000", "0", "0", "0", "0.5")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "_scalars", indexRange = "scalarIndex!I21", forceInit = TRUE),
    tibble(
      scalar = c("baseyear", "repco", "gr", "growthq", "drc", "the1", "lstd", "trcap", "twcap", "twefac", "labfac", "_gmsopt_solver"),
      description = c("", "", "", "", "", "", "", "", "", "", "", ""),
      value = c("1988", "2.5", "0.3", "2.5", "0.15", "0.6", NA_character_, NA_character_, NA_character_, "0.5", "0.5", "CPLEX")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "_scalars", indexRange = "scalarIndex!C12:H13", forceInit = TRUE),
    tibble(
      scalar = c("baseyear", "repco", "gr", "growthq", "drc", "the1", "lstd", "trcap", "twcap", "twefac", "labfac", "_gmsopt_solver"),
      description = c("", "", "", "", "", "", "", "", "", "", "", ""),
      value = c(NA_character_, NA_character_, NA_character_, "1988", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
    )
  )
})

test_that("Reading variables/equations works", {
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "var1", indexRange = "vareqIndex", forceInit = TRUE),
    tibble(
      uni = c("x1", "x2", "x3"),
      level = c(1, 2, NA_real_),
      marginal = c(2.56778, NA_real_, 1.23456),
      lower = c(NA_real_, NA_real_, NA_real_),
      upper = c(100, 200.23, -1.234),
      scale = c(NA_real_, NA_real_, NA_real_)
    )
  )
  expect_identical(xlsioPrivate$sheetRefCount[["vareq"]], 2L)
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "var1a"),
    tibble(
      uni = c("x1", "x2", "x3"),
      level = c(1, 2, 0),
      marginal = c(2.56778, 0, 1.23456),
      lower = c(NA_real_, NA_real_, NA_real_),
      upper = c(100, 200.23, -1.234),
      scale = c(NA_real_, NA_real_, NA_real_)
    )
  )
  expect_identical(xlsioPrivate$sheetRefCount[["vareq"]], 1L)
  expect_true(length(xlsioPrivate$cache[["vareq"]]) > 0)
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "var1b"),
    tibble(
      uni = c("x1", "x2", "x3"),
      level = c(1, 2, NA_real_),
      marginal = c(2.56778, NA_real_, 1.23456),
      lower = c(NA_real_, NA_real_, NA_real_),
      upper = c(100, 200.23, -1.234),
      scale = c(NA_real_, NA_real_, NA_real_)
    )
  )
  expect_identical(xlsioPrivate$sheetRefCount[["vareq"]], 0L)
  expect_false(length(xlsioPrivate$cache[["vareq"]]) > 0)
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "eq1"),
    tibble(
      uni1 = c("x1", "x1", "x2", "x2", "x3", "x3"),
      uni2 = c("y1", "y2", "y1", "y2", "y1", "y2"),
      level = c(1, -12.34, 2, -Inf, NA_real_, 14.12),
      marginal = c(2.56778, NA_real_, NA_real_, NA_real_, 1.23456, NA_real_),
      lower = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      upper = c(100, NA_real_, 200.23, NA_real_, -1.234, NA_real_),
      scale = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "_scalarsve_out"),
    tibble(
      scalar = c("z"),
      description = c("objective function value"),
      level = c(123),
      marginal = c(NA_real_),
      lower = c(-Inf),
      upper = c(Inf),
      scale = c(NA_real_)
    )
  )
})

test_that("Reading symbols without data works", {
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i1", indexRange = "emptySymIndex", forceInit = TRUE),
    tibble(
      uni = character(),
      text = character()
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i10"),
    tibble(uni1 = character(), uni2 = character(), text = character())
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "modedistance"),
    tibble(uni1 = character(), uni2 = character(), uni3 = character(), value = numeric())
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i1a"),
    tibble(
      uni = character(),
      text = character()
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i2"),
    tibble(
      uni = character(),
      text = character()
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i9"),
    tibble(
      uni = character(),
      text = character()
    )
  )
})

test_that("Reading Excel without index works", {
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "i11", forceInit = TRUE),
    tibble(
      uni1 = c("ship", "truck", "rail", "barge"),
      uni2 = c("brussels", "brussels", "san francisco", "san francisco"),
      uni3 = c("cleveland", "chicago", "cleveland", "chicago"),
      text = c("text 1", NA, "text 2", "no")
    )
  )
  expect_identical(
    xlsio$read("../data/exampleData.xlsx", "_scalars"),
    tibble(
      scalar = c("baseyear", "repco", "gr", "growthq", "drc", "the1", "lstd", "trcap", "twcap", "twefac", "labfac", "_gmsopt_solver"),
      description = c("", "", "", "", "", "", "", "", "", "", "", ""),
      value = c(NA_character_, "1e+07", NA_character_, NA_character_, "2.5", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
    )
  )
  expect_error(xlsio$read("../data/exampleData.xlsx", "i10"), class = "error_notfound")
})

test_that("Writing Excel files works", {
  tmpdir <- tempdir(TRUE)
  xlsOutFileName <- file.path(tmpdir, "test.xlsx")
  testData <- list(
    "_scalars_out" = tibble(
      scalar = c("cowf", "explimitgr", "big"),
      description = c("a", "b", "c"),
      value = c(1, 2, 3)
    ),
    "_scalars" = tibble(
      scalar = c("_gmsopt_solver"),
      description = c(""),
      value = c("CBC")
    ),
    i1 = tibble(
      uni = c("uni1", "uni2"),
      text = c("text1", "text2")
    ),
    distance = tibble(uni1 = character(), uni2 = character(), value = numeric()),
    distance3 = tibble(uni = c("bla1"), dallas = 1.1, chicago = 1.2)
  )
  expect_error(xlsio$write(xlsOutFileName, testData, includeEmptySheets = TRUE), NA)
  expect_identical(excel_sheets(xlsOutFileName), c("_scalars_out (Output)", "_scalars (Input)", "i1 (Output)", "distance (Input)", "distance3 (Input)", "_index"))
  expect_identical(
    suppressMessages(read_excel(xlsOutFileName, "_index")),
    tibble(
      type = c("par", "par", "par", "set", "set", "par"),
      symbol = c("cowf", "explimitgr", "big", "_gmsopt_solver", "i1", "distance3"),
      range = c(
        '"_scalars_out (Output)!C2"', '"_scalars_out (Output)!C3"', '"_scalars_out (Output)!C4"',
        '"_scalars (Input)!C2"', '"i1 (Output)!A2"', '"distance3 (Input)!A1"'
      ),
      cDim = c(0, 0, 0, 0, 0, 1),
      dim = c(0, 0, 0, 0, 1, 2)
    )
  )
  expect_error(xlsio$write(xlsOutFileName, testData, includeEmptySheets = FALSE), NA)
  expect_identical(excel_sheets(xlsOutFileName), c("_scalars_out (Output)", "_scalars (Input)", "i1 (Output)", "distance3 (Input)", "_index"))
  expect_identical(
    suppressMessages(read_excel(xlsOutFileName, "_index")),
    tibble(
      type = c("par", "par", "par", "set", "set", "par"),
      symbol = c("cowf", "explimitgr", "big", "_gmsopt_solver", "i1", "distance3"),
      range = c(
        '"_scalars_out (Output)!C2"', '"_scalars_out (Output)!C3"', '"_scalars_out (Output)!C4"',
        '"_scalars (Input)!C2"', '"i1 (Output)!A2"', '"distance3 (Input)!A1"'
      ),
      cDim = c(0, 0, 0, 0, 0, 1),
      dim = c(0, 0, 0, 0, 1, 2)
    )
  )
  expect_error(xlsio$write(xlsOutFileName, testData, tibble(`_sid` = 1, `_uid` = "fproske", `_sname` = "asd", `_stime` = "def"),
    includeMetadataSheet = TRUE, includeEmptySheets = FALSE
  ), NA)
  expect_identical(excel_sheets(xlsOutFileName), c(" Info", "_scalars_out (Output)", "_scalars (Input)", "i1 (Output)", "distance3 (Input)", "_index"))
  expect_identical(
    suppressMessages(read_excel(xlsOutFileName, " Info")),
    tibble(
      User = c("fproske", NA, NA, NA),
      `Scenario name` = c("asd", NA, NA, NA),
      `Time created` = c("def", NA, NA, NA),
      `...4` = NA,
      `...5` = NA,
      `Symbol name` = c(0, 0, 0, 0),
      Description = c("Input scalars", "Output scalars", "Distance 😈 3", "Set i1"),
      `...8` = c("(Input)", "(Output)", "(Input)", "(Output)")
    )
  )

  expect_identical(
    xlsio$read(xlsOutFileName, "_scalars_out", forceInit = TRUE),
    tibble(
      scalar = c("cowf", "explimitgr", "big", "pawat", "pafod", "tolcnl", "tolpr", "tolnwfp", "betaf"),
      description = "",
      value = c("1", "2", "3", NA, NA, NA, NA, NA, NA)
    )
  )
})

# test example from documentation
ioConfig <<- list(
  modelOut = list(
    "_scalars_out" = list(
      symnames = c("total_cost"),
      symtypes = c("parameter"),
      symtext = c("Total cost"),
      colTypes = "ccc",
      alias = "Output scalars",
      headers = list(
        scalar = list(),
        description = list(),
        value = list()
      )
    ),
    schedule = list(
      alias = "Schedule (results)", colTypes = "ccddddddd",
      headers = list(
        i = list(), j = list(),
        lngp = list(), latp = list(), lngm = list(),
        latm = list(), demand = list(), cap = list(),
        quantities = list()
      )
    )
  ),
  modelInRaw = list(
    "_scalars" = list(
      symnames = c("f", "mins", "beta", "type"),
      symtext = c("", "", "", ""),
      symtypes = c("parameter", "parameter", "parameter", "set"),
      colTypes = "ccc",
      headers = list(
        scalar = list(),
        description = list(),
        value = list()
      )
    ),
    d = list(alias = "Distance", colTypes = "ccd", headers = list(i = list(), j = list(), value = list())),
    j = list(symtype = "set", alias = "Set j", colTypes = "cc", headers = list(uni = list(), text = list()))
  )
)
xlsio <- XlsIO$new()

test_that("Example from documentation works", {
  expect_identical(
    xlsio$read("../data/doc_example.xlsx", "schedule", forceInit = TRUE),
    tibble(
      i = c("Seattle", "Seattle"), j = c("New-york", "Chicago"),
      lngp = NA_real_, latp = NA_real_, lngm = NA_real_, latm = NA_real_,
      demand = c(325, 300), cap = c(350, 350), quantities = c(50, 300)
    )
  )
  expect_identical(
    xlsio$read("../data/doc_example.xlsx", "d"),
    tibble(
      i = c("Seattle", "Seattle", "Seattle", "San-Diego", "San-Diego", "San-Diego"),
      j = c("New-york", "Chicago", "Topeka", "New-york", "Chicago", "Topeka"),
      value = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
    )
  )
  expect_identical(
    xlsio$read("../data/doc_example.xlsx", "j"),
    tibble(
      uni = c("New-york"),
      text = c("")
    )
  )
  expect_identical(
    xlsio$read("../data/doc_example.xlsx", "_scalars"),
    tibble(
      scalar = c("f", "mins", "beta", "type"),
      description = c("", "", "", ""),
      value = c("90", "100", "0.95", "lp")
    )
  )
  expect_identical(
    xlsio$read("../data/doc_example.xlsx", "_scalars_out"),
    tibble(
      scalar = c("total_cost"),
      description = c("Total cost"),
      value = c("153.675")
    )
  )
})

test_that("Getting symbol names woks", {
  expect_identical(xlsio$getSymbolNames(), c("j", "d", "_scalars"))
})
