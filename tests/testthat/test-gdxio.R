context("gdxio read")

load(file.path(getwd(), "data/trnsport.miroconf"), .GlobalEnv)
source("../../global.R")
source("../../R/gdxio.R")

gdxio <- GdxIO$new('', c(modelInRaw, modelOut))

test_that("Reading of set works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "i"), 
               tibble::tibble('...1' = c("seattle", "san-diego"), '...2' = rep.int(NA_character_, 2L)))
})
test_that("Reading of parameter works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "a"), 
                   tibble::tibble('...1' = c("seattle", "san-diego"), '...2' = c(350, 600)))
})
test_that("Reading of (single) scalar works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), "f"), 
                  90)
})
test_that("Reading of input scalars works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), scalarsFileName), 
                   tibble::tibble())
})
test_that("Reading of output scalars works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), scalarsOutName), 
                   tibble::tibble('scalarSymbols$symnames' = 'f', 
                          'scalarSymbols$symtext' = 'freight in dollars per case per thousand miles',
                          'vapply(...)' = '90'))
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
                          '...1' = c(153.675, 0), '...2' = c(0, 1), '...3' = c(-Inf, 0), 
                          '...4' = c(Inf, 0), '...5' = c(1, 1)))
})
test_that("Reading of equations works", {
  expect_identical(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), 'supply'), 
                   tibble::tibble('...1' = c("san-diego", "seattle"), l = c(550, 350), m = c(0, 0),
                          lo = c(-Inf, -Inf), up = c(600, 350), s = c(1, 1)))
})
test_that("Reading of variables works", {
  expect_equal(as.data.frame(gdxio$rgdx(file.path(getwd(), "data/trnsport.gdx"), 'x')), 
                   data.frame('...1' = c("san-diego", "san-diego", "san-diego",
                                  "seattle", "seattle", "seattle"),
                          '...2' = c("chicago", "new-york", "topeka",
                                  "chicago", "new-york", "topeka"), 
                          l = c(0, 275, 275, 300, 50, 0),
                          m = c(0.009, 0, 0, 0, 0, 0.036),
                          lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L), 
                          stringsAsFactors = FALSE, check.names = FALSE))
})