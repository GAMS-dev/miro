context("Unit tests - miroscenio")

library(jsonlite)
library(readr)
library(dplyr)
library(zip)
library(futile.logger)

load(file.path(getwd(), "data/test_gdxio.miroconf"), .GlobalEnv)
source("../../components/gdxio.R")
source("../../components/miroscenio.R")
modelNameRaw <<- "Pickstock"

gdxio <<- GdxIO$new(
  file.path(
    .libPaths()[1], "gdxrrwMIRO",
    if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
      file.path("bin", "x64")
    } else {
      "bin"
    }
  ),
  c(modelInRaw, modelOut),
  scalarsFileName, scalarsOutName,
  scalarEquationsName,
  scalarEquationsOutName,
  list()
)

viewsDummy <- list(
  addConf = function(conf) {
    expect_identical(conf$bla[[1]], "asd")
  },
  getJSON = function(views = NULL, scenId = NULL) {
    if (identical(scenId, "1")) {
      return('{"bla":["asd"]}')
    }
    return('{"bla":["def"]}')
  }
)
attachmentsDummy <- list(add = function(session, filePaths, fileNames = NULL, overwrite = FALSE, execPerm = NULL) {
  expect_true(is.null(session))
  expect_identical(fileNames, c("a.txt", "b.txt"))
  expect_identical(execPerm, c(FALSE, TRUE))
  expect_identical(basename(filePaths), c("a.txt", "b.txt"))
}, getMetadata = function(scenId) {
  return(tibble::tibble(name = c("a.txt", "b.txt"), execPerm = c(FALSE, TRUE)))
}, download = function(filePath, fileNames = NULL,
                       fullPath = FALSE, allExecPerm = FALSE, scenId = NULL) {
  for (fileName in fileNames) {
    writeLines(fileName, file.path(filePath, fileName))
  }
  return(file.path(filePath, fileNames))
})
scenDummy <- list(updateMetadata = function(newName, newTags) {
  expect_identical(newName, "test")
  expect_identical(newTags, letters)
})

test_that("Writing miroscen files works", {
  tmpd <- tempdir(check = TRUE)
  on.exit(unlink(file.path(tmpd, "test.miroscen")))
  generateMiroScen(file.path(tmpd, "test.miroscen"),
    tibble::tibble(
      sid = 1, uid = "conner", name = "test", stime = Sys.time(),
      tags = vector2Csv(letters)
    ),
    list(
      i = tibble::tibble("1" = c("seattle", "san-diego"), "2" = rep.int(NA_character_, 2L)),
      a = tibble::tibble("1" = c("seattle", "san-diego"), "2" = c(350, 600)),
      x = tibble::tibble(
        "1" = c(
          "seattle", "seattle", "seattle",
          "san-diego", "san-diego", "san-diego"
        ),
        "2" = c(
          "new-york", "chicago", "topeka",
          "new-york", "chicago", "topeka"
        ),
        l = c(50, 300, 0, 275, 0, 275),
        m = c(0, 0, 0.036, 0, 0.009, 0),
        lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L)
      ),
      `_scalars` = tibble::tibble(
        scalar = c("_gmsopt_a", "_gmspar_b"),
        description = c("bla", "blubb"),
        value = c("1", "2")
      )
    ),
    attachmentsDummy, viewsDummy,
    tabsetId = NULL
  )
  expect_true(file.exists(file.path(tmpd, "test.miroscen")))
  expect_identical(
    zip::zip_list(file.path(tmpd, "test.miroscen"))[[1]],
    c(
      "metadata.json", "data.gdx", "views.json", "attachments/",
      "attachments/a.txt", "attachments/b.txt"
    )
  )
  generateMiroScen(file.path(tmpd, "test.miroscen"),
    tibble::tibble(
      sid = 1, uid = "conner", name = "test", stime = Sys.time(),
      tags = vector2Csv(letters)
    ),
    list(
      i = tibble::tibble("1" = c("seattle", "san-diego"), "2" = rep.int(NA_character_, 2L)),
      a = tibble::tibble("1" = c("seattle", "san-diego"), "2" = c(350, 600)),
      x = tibble::tibble(
        "1" = c(
          "seattle", "seattle", "seattle",
          "san-diego", "san-diego", "san-diego"
        ),
        "2" = c(
          "new-york", "chicago", "topeka",
          "new-york", "chicago", "topeka"
        ),
        l = c(50, 300, 0, 275, 0, 275),
        m = c(0, 0, 0.036, 0, 0.009, 0),
        lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L)
      ),
      `_scalars` = tibble::tibble(
        scalar = c("_gmsopt_a", "_gmspar_b"),
        description = c("bla", "blubb"),
        value = c("1", "2")
      )
    ),
    attachmentsDummy, viewsDummy,
    tabsetId = 2L
  )
  on.exit(unlink(file.path(tmpd, "views.json")), add = TRUE)
  unzip(file.path(tmpd, "test.miroscen"), "views.json", exdir = tmpd)
  expect_identical(read_file(file.path(tmpd, "views.json")), '{"bla":["def"]}')
})
test_that("Validating miroscen files works", {
  expect_error(validateMiroScen(file.path("..", "data", "bad1.miroscen")),
    regexp = "missing",
    class = "error_badformat"
  )
  expect_error(validateMiroScen(file.path("..", "data", "bad2.miroscen")))
  expect_error(validateMiroScen(file.path("..", "data", "bad3.miroscen")),
    regexp = "metadata\\.json",
    class = "error_badformat"
  )
})
test_that("Reading miroscen files works", {
  tmpd <- tempdir(check = TRUE)
  on.exit(unlink(file.path(tmpd, c("test.miroscen", "data.gdx"))))
  generateMiroScen(file.path(tmpd, "test.miroscen"),
    tibble::tibble(
      sid = 1, uid = "conner", name = "test", stime = Sys.time(),
      tags = vector2Csv(letters)
    ),
    list(
      i = tibble::tibble("1" = c("seattle", "san-diego"), "2" = rep.int(NA_character_, 2L)),
      a = tibble::tibble("1" = c("seattle", "san-diego"), "2" = c(350, 600)),
      x = tibble::tibble(
        "1" = c(
          "seattle", "seattle", "seattle",
          "san-diego", "san-diego", "san-diego"
        ),
        "2" = c(
          "new-york", "chicago", "topeka",
          "new-york", "chicago", "topeka"
        ),
        l = c(50, 300, 0, 275, 0, 275),
        m = c(0, 0, 0.036, 0, 0.009, 0),
        lo = rep.int(0, 6L), up = rep.int(Inf, 6L), s = rep.int(1, 6L)
      ),
      `_scalars` = tibble::tibble(
        scalar = c("_gmsopt_a", "_gmspar_b"),
        description = c("bla", "blubb"),
        value = c("1", "2")
      )
    ),
    attachmentsDummy, viewsDummy,
    tabsetId = NULL
  )
  expect_output(f <- loadMiroScen(
    file.path(tmpd, "test.miroscen"), scenDummy, attachmentsDummy, viewsDummy,
    names(modelIn)
  ),
  regexp = "ignored"
  )
  expect_identical(f, tibble::tibble(scalar = character(), description = character(), value = character()))
  expect_true(file.exists(file.path(tmpd, "data.gdx")))
})
test_that("Loading MIRO scenario into specified directory works", {
  tmpd <- tempdir(check = TRUE)
  if (!dir.create(file.path(tmpd, "bla"))) {
    stop(sprintf("Could not create directory: %s", file.path(tmpd, "bla")))
  }
  if (!file.copy(file.path(getwd(), "data/test.miroscen"), file.path(tmpd, "test.miroscen"))) {
    stop("Could not copy miroscen file")
  }
  on.exit(unlink(file.path(tmpd, c("test.miroscen", "bla")), recursive = TRUE))
  expect_output(f <- loadMiroScen(file.path(tmpd, "test.miroscen"), scenDummy, attachmentsDummy, viewsDummy,
    c(names(modelIn), "_gmsopt_a"),
    exdir = file.path(tmpd, "bla")
  ),
  regexp = "ignored"
  )
  expect_identical(f, tibble::tibble(scalar = "_gmsopt_a", description = "", value = "1"))
  expect_true(file.exists(file.path(tmpd, "bla", "data.gdx")))
})
