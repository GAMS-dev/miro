context("Unit tests - attachment class")

library(jsonlite)
library(futile.logger)
library(DBI)

source("../../components/scenario_metadata.R")
source("../../components/db.R")
source("../../components/attachments.R")

FakeSession <- R6Class("FakeSession", public = list(
  initialize = function(id){
    private$id <- id
    return(invisible(self))
  },
  ns = function(id){
    return(paste0(private$id, "-", id))
  }
), private = list(
  id = NULL
))

testDir <- file.path(getwd(), "..")

createTestDb()

db <- Db$new(uid = "te_de\\%d", 
             dbConf = list(type = "sqlite",
                           name = file.path(testDir, 
                                            "miro.sqlite3")), 
             dbSchema = list(colNames = list("_scenMeta" = c(sid = "_sid")),
                             tabName = c("_scenAttach" = "_attach",
                                         "_scenMeta" = "asd",
                                         "_scenLock" = "def"),
                             colTypes = list()),
             slocktimeLimit = slocktimeLimit, modelName = modelName,
             hcubeActive = FALSE, ugroups = c("bla_blubb", "test123"))

workDir <- tempdir()

attachments <- Attachments$new(db, list(maxSize = 200, maxNo = 3,
                                        forbiddenFNames = c(MIROGdxInName, MIROGdxOutName,
                                                            paste0(modelName, c(".log", ".lst")))),
                               workDir,
                               c("in1", "widget1", "widget2", "in2"),
                               c("out1", "out2", "out3"), c("in1", "in2"))
callbackCounter <- 1L
callbackFunction <- function(){
  callbackCounter <<- callbackCounter + 1L
}

test_that("Adding attachments work", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  
  attachments$registerUpdateCallback(fakeSessionIn1, callbackFunction)
  
  file.copy(c(file.path(testDir, "data", "_scalars.csv"),
              file.path(testDir, "data", "bad-views2.json")),
            c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json")))
  expect_error(attachments$add(session = fakeSessionIn1, c(file.path(testDir, "data", "_scalars.csv"),
                                                           file.path(testDir, "data", "bad-views2.json")),
                               fileNames = NULL, overwrite = FALSE, execPerm = NULL), NA)
  file.move(c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json")),
            c(file.path(testDir, "data", "_scalars.csv"),
              file.path(testDir, "data", "bad-views2.json")))
  expect_identical(attachments$getIds(), c("_scalars.csv", "bad-views2.json"))
  expect_identical(callbackCounter, 2L)
  
  expect_error(attachments$add(session = NULL, file.path(testDir, "data", "bad-views.json"),
                               fileNames = NULL, overwrite = FALSE, execPerm = NULL), class = "error_max_size")
  expect_error(attachments$add(session = fakeSessionScen3Out2, file.path(testDir, "data", "_scalars.csv"),
                               fileNames = NULL, overwrite = FALSE, execPerm = NULL), class = "error_readonly")
  expect_error(attachments$add(session = fakeSessionIn1, file.path(testDir, "data", "_scalars.csv"),
                               fileNames = NULL, overwrite = FALSE, execPerm = NULL), class = "error_duplicate_files")
  expect_identical(callbackCounter, 2L)
  
  file.copy(file.path(testDir, "data", "_scalars.csv"),
            file.path("data", "_scalars.csv"))
  expect_error(attachments$add(session = NULL, file.path(testDir, "data", "_scalars.csv"),
                               fileNames = "scalars.csv", overwrite = FALSE, execPerm = FALSE), NA)
  expect_identical(callbackCounter, 3L)
  file.move(file.path("data", "_scalars.csv"),
            file.path(testDir, "data", "_scalars.csv"))
  expect_identical(attachments$getMetadata(),
                   tibble(name = c("_scalars.csv", "bad-views2.json", "scalars.csv"),
                          execPerm = c(TRUE, TRUE, FALSE)))
  expect_true(all(file.exists(file.path(workDir, c("_scalars.csv", "bad-views2.json",
                                                   file.path("_miro_attach_", "scalars.csv"))))))
  expect_error(attachments$add(session = NULL, file.path(testDir, "data", "bad-views3.json"),
                               fileNames = "def.json", overwrite = FALSE, execPerm = NULL), class = "error_file_not_found")
  expect_error(attachments$add(session = NULL, file.path(testDir, "data", "bad-views2.json"),
                               fileNames = "def.json", overwrite = FALSE, execPerm = NULL), class = "error_max_no")
})

test_that("Flushing opQueue works", {
  data <- attachments$flushOpQueue()$save
  expect_error(db$exportScenDataset(dplyr::bind_cols("_sid" = rep.int(1, nrow(data)),
                                                     data),
                                    "_attach", addForeignKey = FALSE), NA)
})

test_that("Initializing scen data works", {
  expect_error(attachments$initScenData(1), NA)
})

test_that("Saving/downloading attachments work", {
  unlink(file.path(workDir, c("_scalars.csv", "bad-views2.json",
                              file.path("_miro_attach_", "scalars.csv"))))
  expect_error(attachments$save(workDir, "scalars.csv"), NA)
  expect_true(file.exists(file.path(workDir, "scalars.csv")))
  expect_identical(readBin(file.path(workDir, "scalars.csv"), "raw"), as.raw(0x73))
  
  expect_error(attachments$save(file.path(workDir, "asd.csv"), "scalars.csv"), NA)
  expect_true(file.exists(file.path(workDir, "asd.csv")))
  expect_identical(readBin(file.path(workDir, "asd.csv"), "raw"), as.raw(0x73))
  
  unlink(file.path(workDir, c("_scalars.csv", "bad-views2.json", "scalars.csv")))
  
  expect_error(attachments$download(workDir, allExecPerm = TRUE), NA)
  expect_true(all(file.exists(file.path(workDir, c("_scalars.csv", "bad-views2.json")))))
  expect_false(file.exists(file.path(workDir, c("scalars.csv"))))
  unlink(file.path(workDir, c("_scalars.csv", "bad-views2.json", "scalars.csv")))
})

test_that("Overwriting attachments work", {
  file.copy(file.path(testDir, "data", "bad-views2.json"),
            file.path("data", "bad-views2.json"))
  expect_error(attachments$add(session = NULL, file.path(testDir, "data", "bad-views2.json"),
                               fileNames = "scalars.csv", overwrite = TRUE, execPerm = TRUE), NA)
  file.move(file.path("data", "bad-views2.json"),
            file.path(testDir, "data", "bad-views2.json"))
  expect_error(attachments$save(file.path(workDir, "asd.csv"), "scalars.csv"), NA)
  expect_true(file.exists(file.path(workDir, "asd.csv")))
  expect_identical(readBin(file.path(workDir, "asd.csv"), "raw"), as.raw(0x2e))
  expect_identical(callbackCounter, 4L)
  unlink(file.path(workDir, c("scalars.csv", "asd.csv")))
})

test_that("Updating attachments works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")
  
  expect_error(attachments$setExecPerm(session = fakeSessionIn1,
                                       c("_scalars.csv", "bad-views2.json"), FALSE), NA)
  expect_error(attachments$setExecPerm(session = fakeSessionScen3Out2,
                                       c("_scalars.csv", "bad-views2.json"), FALSE), class = "error_readonly")
  expect_error(attachments$setExecPerm(session = NULL, c("_scalars.csv", "bad-views2.json", "scalars.csv"),
                                       c(FALSE, FALSE, TRUE)), NA)
  expect_error(attachments$remove(session = NULL, "_scalars.csv"), NA)
  expect_error(attachments$remove(session = NULL, "_scalars.csv"), class = "error_file_not_found")
  expect_error(attachments$download(workDir, allExecPerm = TRUE), NA)
  
  expect_false(any(file.exists(file.path(workDir, c("_scalars.csv", "bad-views2.json")))))
  expect_true(file.exists(file.path(workDir, c("scalars.csv"))))
  unlink(file.path(workDir, c("_scalars.csv", "bad-views2.json", "scalars.csv")))
})

test_that("Download multiple attachments works", {
  expect_identical(length(attachments$download(workDir, fileNames = c("_scalars.csv", "scalars.csv"))), 2L)
  expect_true(file.exists(file.path(workDir, c("_scalars.csv"))))
  unlink(file.path(workDir, c("_scalars.csv", "scalars.csv")))
})