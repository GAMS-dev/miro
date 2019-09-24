context("gdxio read")

load(file.path(getwd(), "data/trnsport.miroconf"), .GlobalEnv)
source("../../global.R")
source("../../R/gdxio.R")

gdxio <- GdxIO$new('', c(modelInRaw, modelOut))

test_that("Reading of set works", {
  expect_equal(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "i"), 
               tibble::tibble('1' = c("seattle", "san-diego"), '2' = rep.int(NA, 2L)))
})
test_that("Reading of parameter works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "a"), 
                   tibble::tibble('1' = c("seattle", "san-diego"), '2' = c(350, 600)))
})
test_that("Reading of (single) scalar works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "f"), 
                  90)
})
test_that("Reading of (single) singleton set works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "sub_i"), 
                   tibble(`1` = "seattle", `2` = 'test'))
})
test_that("Reading of input scalars works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), scalarsFileName), 
                   tibble::tibble(`scalarSymbols$symnames` = c('mins', 'beta', 'sub_i'),
                                  `scalarSymbols$symtext` = c('minimum shipment (MIP- and MINLP-only)',
                                                              'beta (MINLP-only)',
                                                              'sub_i'),
                                  `vapply(...)` = c(NA_character_, NA_character_, 'seattle||test')))
})
test_that("Reading of output scalars works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), scalarsOutName), 
                   tibble::tibble('scalarSymbols$symnames' = c('f', 'total_cost'), 
                          'scalarSymbols$symtext' = c('freight in dollars per case per thousand miles',
                                                      'total transportation costs in thousands of dollars'),
                          'vapply(...)' = c('90', NA_character_)))
})
test_that("Reading of (single) scalar equation works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "cost"), 
                   c(0, 1, 0, 0, 1))
})
test_that("Reading of (single) scalar variable works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "z"), 
                   c(153.675, 0, -Inf, Inf, 1))
})
test_that("Reading of scalar variables and equations works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), scalarEquationsOutName), 
                   tibble::tibble('scalarSymbols$symtypes' = c('variable', 'equation'), 
                          'scalarSymbols$symnames' = c('z', 'cost'), 
                          'scalarSymbols$symtext' = c('total transportation costs in thousands of dollars',
                                                      'define objective function'),
                          '1' = c(153.675, 0), '2' = c(0, 1), '3' = c(-Inf, 0), 
                          '4' = c(Inf, 0), '5' = c(1, 1)))
})
test_that("Reading of equations works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), 'supply'), 
                   tibble::tibble('1' = c("seattle", "san-diego"), l = c(350, 550), m = c(0, 0),
                          lo = c(-Inf, -Inf), up = c(350, 600), s = c(1, 1)))
})
test_that("Reading of variables works", {
  expect_equal(as.data.frame(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), 'x')), 
               as.data.frame(tibble('1' = c("seattle", "seattle", "seattle",
                                            "san-diego", "san-diego", "san-diego"),
                                    '2' = c("new-york", "chicago", "topeka",
                                            "new-york", "chicago", "topeka"), 
                                    l = c(50, 300, 0, 275, 0, 275),
                                    m = c(0, 0, 0.036, 0, 0.009, 0),
                                    lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L))))
})

test_that("Writing of scalars works", {
  scalarData <- tibble::tibble(`scalarSymbols$symnames` = c('mins', 'beta', 'sub_i'),
                               `scalarSymbols$symtext` = c('minimum shipment (MIP- and MINLP-only)',
                                                           'beta (MINLP-only)',
                                                           'sub_i'),
                               `vapply(...)` = c(NA_character_, NA_character_, 'seattle||test'))
  data <- list(scalarData)
  names(data) <- scalarsFileName
  filePath <- file.path(getwd(), "data/tests_gdxio.gdx")
  #on.exit(unlink(filePath), add = TRUE)
  gdxio$wgdx(filePath, data)
  
  expect_equal(gdxio$rgdx(filePath, scalarsFileName), 
               scalarData)
})
