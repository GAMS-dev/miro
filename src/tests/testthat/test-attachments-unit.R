context("Unit tests - attachment class")

library(jsonlite)
library(futile.logger)
library(DBI)

source("../../components/scenario_extensions.R")
source("../../components/db_schema.R")
source("../../components/db.R")
source("../../components/attachments.R")

FakeSession <- R6Class("FakeSession", public = list(
  initialize = function(id) {
    private$id <- id
    return(invisible(self))
  },
  ns = function(id) {
    return(paste0(private$id, "-", id))
  }
), private = list(
  id = NULL
))
createTestDb()

ioConfig <<- list()

dbSchema <<- DbSchema$new()
if (identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) {
  dbConfig <- list(
    type = "postgres",
    username = Sys.getenv("MIRO_DB_USERNAME", "postgres"),
    password = Sys.getenv("MIRO_DB_PASSWORD", ""),
    name = Sys.getenv("MIRO_DB_NAME", "postgres"),
    host = Sys.getenv("MIRO_DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("MIRO_DB_PORT", "5432")),
    schema = "mirotests"
  )
} else {
  dbConfig <- list(
    type = "sqlite",
    name = file.path(
      testDir, "testdb",
      "miro.sqlite3"
    )
  )
}
db <- Db$new(
  uid = "te_de\\%d",
  dbConf = dbConfig,
  slocktimeLimit = slocktimeLimit, modelName = modelName,
  ugroups = c("bla_blubb", "test123")
)
dbSchema$setConn(db$getConn())

workDir <- tempdir()

attachments <- Attachments$new(
  db, list(
    maxSize = 200, maxNo = 3,
    forbiddenFNames = c(
      MIROGdxInName, MIROGdxOutName,
      paste0(modelName, c(".log", ".lst"))
    )
  ),
  workDir,
  c("in1", "widget1", "widget2", "in2"),
  c("out1", "out2", "out3"), c("in1", "in2")
)
callbackCounter <- 1L
callbackFunction <- function() {
  callbackCounter <<- callbackCounter + 1L
}

writeToDb <- function(attachmentOpQueue, sid = 1L) {
  if (length(attachmentOpQueue$remove)) {
    db$deleteRows("_scenAttach", "fileName",
      attachmentOpQueue$remove,
      conditionSep = "OR", subsetSids = sid
    )
  }
  if (length(attachmentOpQueue$updateExec$name)) {
    fnHasExecPerm <- attachmentOpQueue$updateExec$
      name[attachmentOpQueue$updateExec$execPerm]
    fnHasNoExecPerm <- attachmentOpQueue$updateExec$
      name[!attachmentOpQueue$updateExec$execPerm]
    if (length(fnHasExecPerm)) {
      db$updateRows("_scenAttach",
        tibble::tibble("fileName", fnHasExecPerm),
        colNames = "execPerm",
        values = TRUE,
        subsetSids = sid, innerSepAND = FALSE
      )
    }
    if (length(fnHasNoExecPerm)) {
      db$updateRows("_scenAttach",
        tibble::tibble("fileName", fnHasNoExecPerm),
        colNames = "execPerm",
        values = FALSE,
        subsetSids = sid, innerSepAND = FALSE
      )
    }
  }
  if (length(attachmentOpQueue$save)) {
    db$exportScenDataset(
      dplyr::bind_cols(
        "_sid" = rep.int(sid, nrow(attachmentOpQueue$save)),
        attachmentOpQueue$save
      ),
      "_scenAttach"
    )
  }
}

test_that("Adding attachments work", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")

  attachments$registerUpdateCallback(fakeSessionIn1, callbackFunction)

  file.copy(
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json"))
  )
  expect_error(attachments$add(
    session = fakeSessionIn1, c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    fileNames = NULL, overwrite = FALSE, execPerm = NULL
  ), NA)
  file.move(
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json")),
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    )
  )
  expect_true(all(attachments$getIds() %in% c("_scalars.csv", "bad-views2.json")))
  expect_identical(length(attachments$getIds()), 2L)
  expect_identical(callbackCounter, 2L)

  expect_error(attachments$add(
    session = NULL, file.path(testDir, "data", "bad-views.json"),
    fileNames = NULL, overwrite = FALSE, execPerm = NULL
  ), class = "error_max_size")
  expect_error(attachments$add(
    session = fakeSessionScen3Out2, file.path(testDir, "data", "_scalars.csv"),
    fileNames = NULL, overwrite = FALSE, execPerm = NULL
  ), class = "error_readonly")
  expect_error(attachments$add(
    session = fakeSessionIn1, file.path(testDir, "data", "_scalars.csv"),
    fileNames = NULL, overwrite = FALSE, execPerm = NULL
  ), class = "error_duplicate_files")
  expect_identical(callbackCounter, 2L)

  file.copy(
    file.path(testDir, "data", "_scalars.csv"),
    file.path("data", "_scalars.csv")
  )
  expect_error(attachments$add(
    session = NULL, file.path(testDir, "data", "_scalars.csv"),
    fileNames = "scalars.csv", overwrite = FALSE, execPerm = FALSE
  ), NA)
  expect_identical(callbackCounter, 3L)
  file.move(
    file.path("data", "_scalars.csv"),
    file.path(testDir, "data", "_scalars.csv")
  )
  expect_equal(
    attachments$getMetadata(),
    tibble(
      name = c("_scalars.csv", "bad-views2.json", "scalars.csv"),
      execPerm = c(TRUE, TRUE, FALSE)
    )
  )
  expect_true(all(file.exists(file.path(workDir, c(
    "_scalars.csv", "bad-views2.json",
    file.path("_miro_attach_", "scalars.csv")
  )))))
  expect_error(attachments$add(
    session = NULL, file.path(testDir, "data", "bad-views3.json"),
    fileNames = "def.json", overwrite = FALSE, execPerm = NULL
  ), class = "error_not_found")
  expect_error(attachments$add(
    session = NULL, file.path(testDir, "data", c("_scalars.csv", "bad-views2.json")),
    fileNames = c("_scalars.csv", "def.json"), overwrite = TRUE, execPerm = NULL
  ), class = "error_max_no")
  expect_true("_scalars.csv" %in% attachments$getMetadata()[[1]])
})

test_that("Flushing opQueue works", {
  expect_error(writeToDb(attachments$flushOpQueue()), NA)
})

test_that("Initializing scen data works", {
  expect_error(attachments$initScenData(1), NA)
})

test_that("Adding attachments works after storing in db", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  file.copy(
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json"))
  )
  expect_error(attachments$add(
    session = fakeSessionIn1, c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    fileNames = NULL, overwrite = TRUE, execPerm = NULL
  ), NA)
  file.copy(
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json")),
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    )
  )
  expect_error(attachments$add(
    session = fakeSessionIn1, c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    fileNames = NULL, overwrite = TRUE, execPerm = c(TRUE, FALSE)
  ), NA)
  expect_equal(
    attachments$getMetadata(),
    tibble(
      name = c("_scalars.csv", "bad-views2.json", "scalars.csv"),
      execPerm = if (identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) c(TRUE, FALSE, FALSE) else c(1L, 0L, 0L)
    )
  )
  expect_error(attachments$setExecPerm(fakeSessionIn1, "bad-views2.json", TRUE), NA)
  expect_equal(
    attachments$getMetadata(),
    tibble(
      name = c("_scalars.csv", "bad-views2.json", "scalars.csv"),
      execPerm = if (identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) c(TRUE, TRUE, FALSE) else c(1L, 1L, 0L)
    )
  )
  expect_error(writeToDb(attachments$flushOpQueue()), NA)
  file.move(
    file.path("data", c("_scalars.csv", "bad-views2.json")),
    file.path(testDir, "data", c("_scalars.csv", "bad-views2.json"))
  )
})

test_that("Saving/downloading attachments work", {
  expect_false(file.exists(file.path(workDir, "scalars.csv")))
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
})

test_that("Overwriting attachments work", {
  file.copy(
    file.path(testDir, "data", "bad-views2.json"),
    file.path("data", "bad-views2.json")
  )
  expect_error(attachments$add(
    session = NULL, file.path(testDir, "data", "bad-views2.json"),
    fileNames = "scalars.csv", overwrite = TRUE, execPerm = TRUE
  ), NA)
  file.move(
    file.path("data", "bad-views2.json"),
    file.path(testDir, "data", "bad-views2.json")
  )
  expect_error(attachments$save(file.path(workDir, "asd.csv"), "scalars.csv"), NA)
  expect_true(file.exists(file.path(workDir, "asd.csv")))
  expect_identical(readBin(file.path(workDir, "asd.csv"), "raw"), as.raw(0x2e))
  expect_identical(callbackCounter, 7L)
})

test_that("Updating attachments works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  fakeSessionScen3Out2 <- FakeSession$new("tab_3_2")

  expect_error(attachments$setExecPerm(
    session = fakeSessionIn1,
    c("_scalars.csv", "bad-views2.json"), FALSE
  ), NA)
  expect_error(attachments$setExecPerm(
    session = fakeSessionScen3Out2,
    c("_scalars.csv", "bad-views2.json"), FALSE
  ), class = "error_readonly")
  expect_error(attachments$setExecPerm(
    session = NULL, c("_scalars.csv", "bad-views2.json", "scalars.csv"),
    c(FALSE, FALSE, TRUE)
  ), NA)
  expect_error(attachments$remove(session = NULL, "_scalars.csv"), NA)
  expect_error(attachments$remove(session = NULL, "_scalars.csv"), class = "error_not_found")

  unlink(file.path(workDir, "bad-views2.json"))
  expect_error(attachments$download(workDir, allExecPerm = TRUE), NA)

  expect_false(any(file.exists(file.path(workDir, c("_scalars.csv", "bad-views2.json")))))
  expect_true(file.exists(file.path(workDir, c("scalars.csv"))))
  file.copy(
    file.path(testDir, "data", "bad-views2.json"),
    file.path(workDir, "bad-views2.json")
  )
  expect_error(attachments$add(
    session = NULL, file.path(testDir, "data", c("_scalars.csv")),
    fileNames = c("_scalars.csv"), overwrite = TRUE, execPerm = NULL
  ), NA)
  file.copy(
    file.path(workDir, "_scalars.csv"),
    file.path(testDir, "data", "_scalars.csv")
  )
})

test_that("Download multiple attachments works", {
  expect_identical(length(attachments$download(workDir, fileNames = c("_scalars.csv", "scalars.csv"))), 2L)
  expect_true(all(file.exists(file.path(workDir, c("_scalars.csv", "scalars.csv")))))
  unlink(file.path(workDir, c("scalars.csv")))
})

test_that("Saving already deleted attachment should result in warning in log", {
  expect_output(opQueue <- attachments$flushOpQueue(), regexp = "Problems reading file:.* No such file or directory")
  expect_identical(opQueue$save$fileName, c("scalars.csv", "_scalars.csv"))
  expect_identical(opQueue$save$fileContent, blob::new_blob(list(
    raw(0),
    charToRaw("scalar,description,value\nmaxstock,maximum number of stocks to select,4\ntrainingdays,number of days for training,100\nsolver,MIP-Solver,CPLEX\n")
  )))
})

attachments <- Attachments$new(
  db, list(
    maxSize = 200, maxNo = 3,
    forbiddenFNames = c(
      MIROGdxInName, MIROGdxOutName,
      paste0(modelName, c(".log", ".lst"))
    )
  ),
  workDir,
  c("in1", "widget1", "widget2", "in2"),
  c("out1", "out2", "out3"), c("in1", "in2")
)

db$deleteRows("_scenAttach", force = TRUE)

test_that("getData method works", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  expect_identical(
    attachments$getData(),
    tibble::tibble(`_sid` = integer(), fileName = character(), execPerm = logical())
  )
  expect_identical(
    attachments$getData(includeContent = TRUE),
    tibble::tibble(`_sid` = integer(), fileName = character(), execPerm = logical(), fileContent = blob::blob())
  )
  file.copy(
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json"))
  )
  attachments$add(
    session = fakeSessionIn1, c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    fileNames = NULL, overwrite = FALSE, execPerm = NULL
  )
  file.move(
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json")),
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    )
  )
  expect_identical(
    attachments$getData(),
    tibble::tibble(
      `_sid` = c(0L, 0L), fileName = c("_scalars.csv", "bad-views2.json"),
      execPerm = c(TRUE, TRUE)
    )
  )
  attachments$setExecPerm(fakeSessionIn1, "_scalars.csv", FALSE)
  expect_identical(
    attachments$getData(),
    tibble::tibble(
      `_sid` = c(0L, 0L), fileName = c("_scalars.csv", "bad-views2.json"),
      execPerm = c(FALSE, TRUE)
    )
  )
  attachments$remove(fakeSessionIn1, "_scalars.csv")
  expect_identical(
    attachments$getData(),
    tibble::tibble(
      `_sid` = c(0L), fileName = c("bad-views2.json"),
      execPerm = c(TRUE)
    )
  )
  expect_identical(
    attachments$getData(includeContent = TRUE)$fileContent,
    blob::blob(charToRaw("./tests/data/good-views.json"))
  )
  expect_identical(
    attachments$getData(includeSandboxScen = FALSE),
    tibble::tibble(`_sid` = integer(), fileName = character(), execPerm = logical())
  )
  writeToDb(attachments$flushOpQueue(), sid = 1L)
  attachments$setSid(1L)
  expect_identical(
    attachments$getData(includeSandboxScen = FALSE),
    tibble::tibble(
      `_sid` = c(1L), fileName = c("bad-views2.json"),
      execPerm = c(TRUE)
    )
  )
  file.copy(
    file.path(testDir, "data", "_scalars.csv"),
    file.path("data", "_scalars.csv")
  )
  attachments$add(
    session = fakeSessionIn1, c(
      file.path(testDir, "data", "_scalars.csv")
    ),
    fileNames = "test.csv", overwrite = FALSE, execPerm = FALSE
  )
  file.move(
    file.path("data", "_scalars.csv"),
    file.path(testDir, "data", "_scalars.csv")
  )
  attachments$setExecPerm(fakeSessionIn1, "bad-views2.json", FALSE)
  expect_identical(
    attachments$getData(scenIds = 1L),
    tibble::tibble(
      `_sid` = c(0L, 0L, 1L), fileName = c("test.csv", "bad-views2.json", "bad-views2.json"),
      execPerm = c(FALSE, FALSE, TRUE)
    )
  )
  expect_identical(
    attachments$getData(scenIds = 1L, fileNames = c("bad-views2.json")),
    tibble::tibble(
      `_sid` = c(0L, 1L), fileName = c("bad-views2.json", "bad-views2.json"),
      execPerm = c(FALSE, TRUE)
    )
  )
  attachments$clear()
  expect_identical(
    attachments$getData(scenIds = 1L),
    tibble::tibble(
      `_sid` = c(1L), fileName = c("bad-views2.json"),
      execPerm = c(TRUE)
    )
  )
})

test_that("download method works with modified exec perm of remote files", {
  fakeSessionIn1 <- FakeSession$new("in_1")
  file.copy(
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json"))
  )
  attachments$add(
    session = fakeSessionIn1, c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    ),
    fileNames = NULL, overwrite = FALSE, execPerm = NULL
  )
  writeToDb(attachments$flushOpQueue(), sid = 1L)
  attachments$clear(cleanLocal = TRUE)
  expect_false(any(file.exists(file.path(workDir, c("_scalars.csv", "bad-views2.json")))))
  attachments$setSid(1L)
  attachments$remove(fakeSessionIn1, "_scalars.csv")
  expect_error(attachments$download(workDir, allExecPerm = TRUE), NA)
  expect_false(file.exists(file.path(workDir, c("_scalars.csv"))))
  expect_true(file.exists(file.path(workDir, c("bad-views2.json"))))

  file.move(
    c(file.path("data", "_scalars.csv"), file.path("data", "bad-views2.json")),
    c(
      file.path(testDir, "data", "_scalars.csv"),
      file.path(testDir, "data", "bad-views2.json")
    )
  )
})

db$finalize()
