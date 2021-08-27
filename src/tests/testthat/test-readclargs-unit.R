context("Unit tests - pf file read/write")

test_that("Reading of pf file works with no DDpar/GMSOpt", {
  expect_identical(
    loadPfFileContent(
      c(
        "--asd=def", "MIP=CPLEX",
        "IDXGenerateGDX=bla.gdx", "huhu=asd"
      ),
      c(), c()
    ),
    tibble::tibble()
  )
})

test_that("Reading of pf file works with only DDpar", {
  expect_identical(
    loadPfFileContent(
      c(
        "--asd=def", "MIP=CPLEX",
        "IDXGenerateGDX=bla.gdx", "huhu=asd"
      ),
      c(), c("asd")
    ),
    tibble::tibble(scalar = "asd", description = "", value = "def")
  )
})

test_that("Reading of pf file works with only GMSOpt", {
  expect_identical(
    loadPfFileContent(
      c(
        "--asd=def", "MIP=CPLEX",
        "IDXGenerateGDX=bla.gdx", "huhu=asd"
      ),
      c("mip")
    ),
    tibble::tibble(scalar = "MIP", description = "", value = "CPLEX")
  )
})

test_that("Reading of pf file works with both GMSOpt and DDPar", {
  expect_identical(
    loadPfFileContent(
      c(
        "--asd=def", "MIP=CPLEX",
        "IDXGenerateGDX=bla.gdx", "huhu=asd"
      ),
      c("mip"), c("asd")
    ),
    tibble::tibble(
      scalar = c("asd", "MIP"), description = character(2L),
      value = c("def", "CPLEX")
    )
  )
})

test_that("Reading of malformed pf file works", {
  expect_identical(
    loadPfFileContent(
      c(
        "--asd=def", "MIP=CPLEX",
        "IDXGenerateGDX=bla.gdx", "huhu"
      ),
      c("mip"), c("asd")
    ),
    tibble::tibble()
  )
})

test_that("Reading of pf file with mixed space and = works", {
  expect_identical(
    loadPfFileContent(
      c(
        "--asd def", "MIP=CPLEX",
        "IDXGenerateGDX=bla.gdx", "huhu=asd"
      ),
      c("mip"), c("asd")
    ),
    tibble::tibble(
      scalar = c("asd", "MIP"), description = character(2L),
      value = c("def", "CPLEX")
    )
  )
})

test_that("Reading of pf file works with both GMSOpt and DDPar range", {
  expect_identical(
    loadPfFileContent(
      c(
        "--asd=def", "MIP=CPLEX", "--def_lo=1",
        "--def_up=99",
        "IDXGenerateGDX=bla.gdx", "huhu=asd"
      ),
      c("mip"), c("asd", "def_lo", "def_up")
    ),
    tibble::tibble(
      scalar = c("asd", "MIP", "def_lo", "def_up"),
      description = character(4L),
      value = c("def", "CPLEX", "1", "99")
    )
  )
  expect_identical(
    loadPfFileContent(
      c(
        "--asd=def", "MIP=CPLEX", "--def_lo 1",
        "--def_up 99",
        "IDXGenerateGDX=bla.gdx", "huhu=asd"
      ),
      c("mip"), c("asd", "def_lo", "def_up")
    ),
    tibble::tibble(
      scalar = c("asd", "MIP", "def_lo", "def_up"),
      description = character(4L),
      value = c("def", "CPLEX", "1", "99")
    )
  )
})
