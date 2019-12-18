context("gdxio read")

load(file.path(getwd(), "data/test_gdxio.miroconf"), .GlobalEnv)
source("../../global.R")
source("../../components/gdxio.R")

gdxio <- GdxIO$new(file.path(.libPaths()[1], "gdxrrwMIRO", "bin"), c(modelInRaw, modelOut), 
                   scalarsFileName, scalarsOutName, 
                   scalarEquationsName, 
                   scalarEquationsOutName)

test_that("Reading of set works", {
  expect_equal(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), "i"), 
               tibble::tibble('1' = c("seattle", "san-diego"), '2' = rep.int(NA_character_, 2L)))
})
test_that("Reading of parameter works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), "a"), 
                   tibble::tibble('1' = c("seattle", "san-diego"), '2' = c(350, 600)))
})
test_that("Reading of table with squeezed out column works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), "squeezed_out"), 
                   tibble::tibble('1' = c("seattle", "san-diego"), 'asd' = c(1, 1),
                                  'def' = c(0, 0)))
  data <- list(tibble::tibble('1' = c("seattle", "san-diego"), 'asd' = c(0, 0),
                              'def' = c(1, 2)))
  names(data) <- "squeezed_out"
  filePath <- file.path(getwd(), "data/tests_gdxio.gdx")
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/tests_gdxio.gdx"), "squeezed_out"), 
                   tibble::tibble('1' = c("seattle", "san-diego"), 'asd' = c(0, 0),
                                  'def' = c(1, 2)))
})
test_that("Reading of (single) scalar works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), "f"), 
                  90)
})
test_that("Reading of (single) singleton set works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), "sub_i"), 
                   tibble::tibble(`1` = "seattle", `2` = 'test'))
  ssData <- tibble::tibble(scalar = "sub_i", description = "test", value = "seattle")
  data <- list(ssData)
  names(data) <- scalarsFileName
  filePath <- file.path(getwd(), "data/tests_gdxio.gdx")
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(file.path(getwd(), "data/tests_gdxio.gdx"), "sub_i"), 
               tibble::tibble(`1` = "seattle", `2` = NA_character_))
  unlink(filePath)
  ssData <- tibble::tibble(scalar = "sub_i", description = "test", value = "topeka||asd")
  data <- list(ssData)
  names(data) <- scalarsFileName
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(file.path(getwd(), "data/tests_gdxio.gdx"), "sub_i"), 
               tibble::tibble(`1` = "topeka", `2` = "asd"))
})
test_that("Reading of input scalars works", {
  expect_equal(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), scalarsFileName), 
                   tibble::tibble(`scalarSymbols$symnames` = c('mins', 'beta', 'sub_i', 'f'),
                                  `scalarSymbols$symtext` = c('minimum shipment (MIP- and MINLP-only)',
                                                              'beta (MINLP-only)',
                                                              'sub_i',
                                                              'freight in dollars per case per thousand miles'),
                                  `vapply(...)` = c(NA_character_, NA_character_, 'seattle||test', 90)))
})
test_that("Reading of output scalars works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), scalarsOutName), 
                   tibble::tibble('scalarSymbols$symnames' = c('total_cost'), 
                          'scalarSymbols$symtext' = c('total transportation costs in thousands of dollars'),
                          'vapply(...)' = c(NA_character_)))
})
test_that("Reading of (single) scalar equation works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), "cost"), 
                   c(0, 1, 0, 0, 1))
})
test_that("Reading of (single) scalar variable works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), "z"), 
                   c(153.675, 0, -Inf, Inf, 1))
})
test_that("Reading of scalar variables and equations works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), scalarEquationsOutName), 
                   tibble::tibble('scalarSymbols$symnames[salarVeFound]' = c('z', 'cost'), 
                          'scalarSymbols$symtext[salarVeFound]' = c('total transportation costs in thousands of dollars',
                                                      'define objective function'),
                          '1' = c(153.675, 0), '2' = c(0, 1), '3' = c(-Inf, 0), 
                          '4' = c(Inf, 0), '5' = c(1, 1)))
})
test_that("Reading of equations works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), 'supply'), 
                   tibble::tibble('1' = c("seattle", "san-diego"), l = c(350, 550), m = c(0, 0),
                          lo = c(-Inf, -Inf), up = c(350, 600), s = c(1, 1)))
})
test_that("Reading of variables works", {
  expect_equal(as.data.frame(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), 'x')), 
               as.data.frame(tibble::tibble('1' = c("seattle", "seattle", "seattle",
                                            "san-diego", "san-diego", "san-diego"),
                                    '2' = c("new-york", "chicago", "topeka",
                                            "new-york", "chicago", "topeka"), 
                                    l = c(50, 300, 0, 275, 0, 275),
                                    m = c(0, 0, 0.036, 0, 0.009, 0),
                                    lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L))))
})

test_that("Writing of scalars works", {
  scalarData <- tibble::tibble(`scalarSymbols$symnames` = c('mins', 'beta', 'sub_i', 'f'),
                               `scalarSymbols$symtext` = c('minimum shipment (MIP- and MINLP-only)',
                                                           'beta (MINLP-only)',
                                                           'sub_i',
                                                           'freight in dollars per case per thousand miles'),
                               `vapply(...)` = c(NA_character_, NA_character_, 'seattle||test', 60))
  data <- list(scalarData)
  names(data) <- scalarsFileName
  filePath <- file.path(getwd(), "data/tests_gdxio.gdx")
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
  filePath <- file.path(getwd(), "data/tests_gdxio.gdx")
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/test_gdxio.gdx"), scalarEquationsOutName), 
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
  filePath <- file.path(getwd(), "data/tests_gdxio.gdx")
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_equal(gdxio$rgdx(file.path(getwd(), "data/tests_gdxio.gdx"), 'x'), 
               varData)
})

test_that("Writing of equations works", {
  eqData <- tibble::tibble(`1` = c("seattle", "san-diego"), l = c(350, 550), m = c(0, 0),
                           lo = c(-Inf, -Inf), up = c(350, 600), s = c(1, 1))
  data <- list(eqData)
  names(data) <- "supply"
  filePath <- file.path(getwd(), "data/tests_gdxioeq.gdx")
  on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/tests_gdxioeq.gdx"), "supply"), 
                   eqData)
})
