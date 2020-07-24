context("gdxio class")

load(file.path(getwd(), "data/test_gdxio.miroconf"), .GlobalEnv)
source("../../global.R")
source("../../components/util.R")
source("../../components/gdxio.R")

filePathEnc <- nativeFileEnc(file.path(getwd(), "dätä/tests_gdxio.gdx"))
filePathEnc2 <- nativeFileEnc(file.path(getwd(), "dätä/test_gdxio.gdx"))
filePathEnc3 <- nativeFileEnc(file.path(getwd(), "dätä/tests_gdxioeq.gdx"))
filePathEnc4 <- nativeFileEnc(file.path(getwd(), "dätä/tests_gdxiouni.gdx"))

gdxio <- GdxIO$new(file.path(.libPaths()[1], "gdxrrwMIRO", "bin"), c(modelInRaw, modelOut), 
                   scalarsFileName, scalarsOutName, 
                   scalarEquationsName, 
                   scalarEquationsOutName,
                   list())

test_that("Reading of set works", {
  expect_equal(gdxio$rgdx(filePathEnc2, "i"), 
               tibble::tibble('1' = c("seattle", "san-diego"), '2' = rep.int(NA_character_, 2L)))
})
test_that("Reading of parameter works", {
  expect_identical(gdxio$rgdx(filePathEnc2, "a"), 
                   tibble::tibble('1' = c("seattle", "san-diego"), '2' = c(350, 600)))
})
test_that("Reading of table with squeezed out column works", {
  expect_identical(gdxio$rgdx(filePathEnc2, "squeezed_out"), 
                   tibble::tibble('1' = c("seattle", "san-diego"), 'asd' = c(1, 1),
                                  'def' = c(NA_real_, NA_real_)))
  data <- list(tibble::tibble('1' = c("seattle", "san-diego"), 'asd' = c(0, 0),
                              'def' = c(1, 2)))
  names(data) <- "squeezed_out"
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_identical(gdxio$rgdx(filePathEnc, "squeezed_out"), 
                   tibble::tibble('1' = c("seattle", "san-diego"), 'asd' = c(0, 0),
                                  'def' = c(1, 2)))
})
test_that("Reading of (single) scalar works", {
  expect_identical(gdxio$rgdx(filePathEnc2, "f"), 
                  90)
})
test_that("Reading of (single) singleton set works", {
  expect_identical(gdxio$rgdx(filePathEnc2, "sub_i"), 
                   tibble::tibble(`1` = "seattle", `2` = 'test'))
  ssData <- tibble::tibble(scalar = "sub_i", description = "test", value = "seattle")
  data <- list(ssData)
  names(data) <- scalarsFileName
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(filePathEnc, "sub_i"), 
               tibble::tibble(`1` = "seattle", `2` = NA_character_))
  unlink(filePath)
  ssData <- tibble::tibble(scalar = "sub_i", description = "test", value = "topeka||asd")
  data <- list(ssData)
  names(data) <- scalarsFileName
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(filePathEnc, "sub_i"), 
               tibble::tibble(`1` = "topeka", `2` = "asd"))
})
test_that("Reading of input scalars works", {
  expect_equal(gdxio$rgdx(filePathEnc2, scalarsFileName), 
                   tibble::tibble(`scalarSymbols$symnames` = c('sub_i', 'f', 'mins', 'beta'),
                                  `scalarSymbols$symtext` = c('sub_i',
                                                              'freight in dollars per case per thousand miles',
                                                              'minimum shipment (MIP- and MINLP-only)',
                                                              'beta (MINLP-only)'),
                                  `vapply(...)` = c('seattle||test', 90, NA_character_, NA_character_)))
})
test_that("Reading of output scalars works", {
  expect_identical(gdxio$rgdx(filePathEnc2, scalarsOutName), 
                   tibble::tibble('scalarSymbols$symnames' = c('total_cost'), 
                          'scalarSymbols$symtext' = c('total transportation costs in thousands of dollars'),
                          'vapply(...)' = c(NA_character_)))
})
test_that("Reading of (single) scalar equation works", {
  expect_identical(gdxio$rgdx(filePathEnc2, "cost"), 
                   c(0, 1, 0, 0, 1))
})
test_that("Reading of (single) scalar variable works", {
  expect_identical(gdxio$rgdx(filePathEnc2, "z"), 
                   c(153.675, 0, -Inf, Inf, 1))
})
test_that("Reading of scalar variables and equations works", {
  expect_identical(gdxio$rgdx(filePathEnc2, scalarEquationsOutName), 
                   tibble::tibble('scalarSymbols$symnames[salarVeFound]' = c('z', 'cost'), 
                          'scalarSymbols$symtext[salarVeFound]' = c('total transportation costs in thousands of dollars',
                                                      'define objective function'),
                          '1' = c(153.675, 0), '2' = c(0, 1), '3' = c(-Inf, 0), 
                          '4' = c(Inf, 0), '5' = c(1, 1)))
})
test_that("Reading of equations works", {
  expect_identical(gdxio$rgdx(filePathEnc2, 'supply'), 
                   tibble::tibble('1' = c("seattle", "san-diego"), l = c(350, 550), m = c(0, 0),
                          lo = c(-Inf, -Inf), up = c(350, 600), s = c(1, 1)))
})
test_that("Reading of variables works", {
  expect_equal(as.data.frame(gdxio$rgdx(filePathEnc2, 'x')), 
               as.data.frame(tibble::tibble('1' = c("seattle", "seattle", "seattle",
                                            "san-diego", "san-diego", "san-diego"),
                                    '2' = c("new-york", "chicago", "topeka",
                                            "new-york", "chicago", "topeka"), 
                                    l = c(50, 300, 0, 275, 0, 275),
                                    m = c(0, 0, 0.036, 0, 0.009, 0),
                                    lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L))))
})

test_that("Writing of scalars works", {
  scalarData <- tibble::tibble(`scalarSymbols$symnames` = c('sub_i', 'f', 'mins', 'beta'),
                               `scalarSymbols$symtext` = c('sub_i',
                                                           'freight in dollars per case per thousand miles',
                                                           'minimum shipment (MIP- and MINLP-only)',
                                                           'beta (MINLP-only)'),
                               `vapply(...)` = c('seattle||test', 60, NA_character_, NA_character_))
  data <- list(scalarData)
  names(data) <- scalarsFileName
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(filePath, scalarsFileName), 
               scalarData)
})

test_that("Writing of scalar variables and equations works", {
  scalarVe <- tibble::tibble('scalarSymbols$symnames[salarVeFound]' = c('z', 'cost'), 
                             'scalarSymbols$symtext[salarVeFound]' = c('total transportation costs in thousands of dollars',
                                                         'define objective function'),
                             '1' = c(153.675, 0), '2' = c(0, 1), '3' = c(-Inf, 0), 
                             '4' = c(Inf, 0), '5' = c(1, 1))
  data <- list(scalarVe)
  names(data) <- scalarEquationsOutName
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_identical(gdxio$rgdx(filePathEnc2, scalarEquationsOutName), 
                   scalarVe)
})

test_that("Writing of variables works", {
  varData <- tibble::tibble('1' = c("seattle", "seattle", "seattle",
                                    "san-diego", "san-diego", "san-diego"),
                            '2' = c("new-york", "chicago", "topeka",
                                    "new-york", "chicago", "topeka"), 
                            l = c(50, 300, 0, 275, 0, 275),
                            m = c(0, 0, 0.036, 0, 0.009, 0),
                            lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L))
  data <- list(varData)
  names(data) <- "x"
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(filePathEnc, 'x'), 
               varData)
})

test_that("Writing of equations works", {
  eqData <- tibble::tibble(`1` = c("seattle", "san-diego"), l = c(350, 550), m = c(0, 0),
                           lo = c(-Inf, -Inf), up = c(350, 600), s = c(1, 1))
  data <- list(eqData)
  names(data) <- "supply"
  filePath <- filePathEnc3
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_identical(gdxio$rgdx(filePathEnc3, "supply"), 
                   eqData)
})

test_that("Writing of singleton set with data from dropdown works", {
  gdxio <- GdxIO$new(file.path(.libPaths()[1], "gdxrrwMIRO", "bin"), 
                     c(modelInRaw, modelOut), 
                     scalarsFileName, scalarsOutName, 
                     scalarEquationsName, 
                     scalarEquationsOutName,
                     list(sub_i = list(aliases = c("test", "bla"), 
                                       choices = c("seattle", "san-diego"),
                                       clearValue = FALSE)))
  scalarData <- tibble::tibble(`scalarSymbols$symnames` = 'sub_i',
                               `scalarSymbols$symtext` = 'sub_i',
                               `vapply(...)` = 'seattle')
  data <- list(scalarData)
  names(data) <- scalarsFileName
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(filePath, scalarsFileName), 
               tibble::tibble(`scalarSymbols$symnames` = c('sub_i', 'f', 'mins', 'beta'),
                              `scalarSymbols$symtext` = c('sub_i', 'freight in dollars per case per thousand miles',
                                                         'minimum shipment (MIP- and MINLP-only)',
                                                         'beta (MINLP-only)'),
                              `vapply(...)` = c('seattle||test', NA_character_, NA_character_,
                                                NA_character_)))
})

test_that("Writing of singleton set with data from dropdown and clearValue=TRUE works", {
  gdxio <- GdxIO$new(file.path(.libPaths()[1], "gdxrrwMIRO", "bin"), 
                     c(modelInRaw, modelOut), 
                     scalarsFileName, scalarsOutName, 
                     scalarEquationsName, 
                     scalarEquationsOutName,
                     list(sub_i = list(aliases = c("test", "bla"), 
                                       choices = c("seattle", "san-diego"),
                                       clearValue = TRUE)))
  scalarData <- tibble::tibble(`scalarSymbols$symnames` = 'sub_i',
                               `scalarSymbols$symtext` = 'sub_i',
                               `vapply(...)` = 'seattle')
  data <- list(scalarData)
  names(data) <- scalarsFileName
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(filePath, scalarsFileName), 
               tibble::tibble(`scalarSymbols$symnames` = c('sub_i', 'f', 'mins', 'beta'),
                              `scalarSymbols$symtext` = c('sub_i', 'freight in dollars per case per thousand miles',
                                                          'minimum shipment (MIP- and MINLP-only)',
                                                          'beta (MINLP-only)'),
                              `vapply(...)` = c('||test', NA_character_, NA_character_,
                                                NA_character_)))
})

test_that("Reading / writing of textOnlySymbols works", {
  gdxio <- GdxIO$new(file.path(.libPaths()[1], "gdxrrwMIRO", "bin"), 
                     c(modelInRaw, modelOut), 
                     scalarsFileName, scalarsOutName, 
                     scalarEquationsName, 
                     scalarEquationsOutName,
                     list(),
                     textOnlySymbols = c("sub_i"))
  scalarData <- tibble::tibble(`scalarSymbols$symnames` = 'sub_i',
                               `scalarSymbols$symtext` = 'sub_i',
                               `vapply(...)` = 'test')
  data <- list(scalarData)
  names(data) <- scalarsFileName
  filePath <- filePathEnc
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(filePath, scalarsFileName), 
               tibble::tibble(`scalarSymbols$symnames` = c('sub_i', 'f', 'mins', 'beta'),
                              `scalarSymbols$symtext` = c('sub_i', 'freight in dollars per case per thousand miles',
                                                          'minimum shipment (MIP- and MINLP-only)',
                                                          'beta (MINLP-only)'),
                              `vapply(...)` = c('test', NA_character_, NA_character_,
                                                NA_character_)))
  expect_identical(gdxrrwMIRO::rgdx(filePath, list(name = "sub_i", te = TRUE))$te, "test")
})

test_that("Reading/writing unicode characters work", {
  setData <- tibble::tibble('1' = c("seattle䤉䤉", "😀😀😀😀😀😀😀"), '2' = c("😈", "ધધધધ"))
  data <- list(setData)
  names(data) <- "i"
  filePath <- filePathEnc4
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_identical(gdxio$rgdx(filePathEnc4, "i"), 
                   setData)
})
