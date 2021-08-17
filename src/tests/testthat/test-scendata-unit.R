context("Unit tests - ScenData class")

library(jsonlite)
library(futile.logger)
library(DBI)
library(dplyr)
library(tidyr)

source("../../components/db_schema.R")
source("../../components/db.R")
source("../../components/scen_data.R")


inScalarNames <- c("maxstock", "trainingdays", "solver")
outScalarNames <- c("error_train", "error_test", "error_ratio",
                    "firstdaytraining", "lastdaytraining")
inScalarDesc <- c("maximum number of stocks to select",
                  "number of days for training",
                  "solver")
outScalarDesc <- c("Absolute error in entire training phase",
                   "Absolute error in entire testing phase",
                   "Ratio between error test and error train",
                   "first date of training period",
                   "last date of training period")

ioConfig <<- list(modelOut = list(stock_weight = list(),
                                  dowvsindex = list(),
                                  abserror = list(),
                                  pricemerge = list(),
                                  `_scalars_out` = list(symnames = outScalarNames, symtext = outScalarDesc)),
                  modelInRaw = list(`_scalars` = list(symnames = inScalarNames, symtext = inScalarDesc)),
                  inputDsNames = c("price", "_scalars"))

outScalarDataVisible <- tibble(scalar = outScalarNames[1:3],
                               description = outScalarDesc[1:3],
                               value = c("79.6135736013082", "951.166205306451",
                                         "11.9472869044886"))
outScalarDataFull <- tibble(scalar = outScalarNames,
                            description = outScalarDesc,
                            value = c("79.6135736013082", "951.166205306451",
                                      "11.9472869044886", "2016-01-04", "2016-05-24"))

dbSchema <<- DbSchema$new(list(schema = list(stock_weight = list(tabName = "stock_weight", 
                                                                 colNames = c("symbol", "value"),
                                                                 colTypes = "cd"),
                                             dowvsindex = list(tabName = "dowvsindex",
                                                               colNames = c("date", "dj", "index fund"),
                                                               colTypes = "cdd"),
                                             abserror = list(tabName = "abserror",
                                                             colNames = c("date", "absolute error train", "absolute error test"),
                                                             colTypes = "cdd"),
                                             pricemerge = list(tabName = "pricemerge", 
                                                               colNames = c("date", "uni", "value"),
                                                               colTypes = "ccd"), 
                                             price = list(tabName = "price",
                                                          colNames = c("date", "symbol", "value"),
                                                          colTypes = "ccd"),
                                             error_train = list(tabName = "error_train",
                                                                colNames = "error_train",
                                                                colTypes = "d"),
                                             error_test = list(tabName = "error_test",
                                                               colNames = "error_test",
                                                               colTypes = "d"), 
                                             error_ratio = list(tabName = "error_ratio",
                                                                colNames = "error_ratio", 
                                                                colTypes = "d"),
                                             firstdaytraining = list(tabName = "firstdaytraining",
                                                                     colNames = "firstdaytraining",
                                                                     colTypes = "c"),
                                             lastdaytraining = list(tabName = "lastdaytraining",
                                                                    colNames = "lastdaytraining",
                                                                    colTypes = "c"),
                                             maxstock = list(tabName = "maxstock",
                                                             colNames = "maxstock",
                                                             colTypes = "d"),
                                             trainingdays = list(tabName = "trainingdays",
                                                                 colNames = "trainingdays",
                                                                 colTypes = "d"),
                                             solver = list(tabName = "solver",
                                                           colNames = "solver",
                                                           colTypes = "c")),
                               views = list(`_scalars_out` = c("error_train", "error_test", "error_ratio",
                                                               "firstdaytraining", "lastdaytraining"),
                                            `_scalars` = c("maxstock", "trainingdays", "solver"))))
procEnv <- list(R_LIBS_USER = Sys.getenv("LIB_PATH"),
                GAMS_SYS_DIR = Sys.getenv("GAMS_SYS_DIR"))
if(identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
  procEnv$MIRO_DB_TYPE <- "postgres"
  procEnv$MIRO_DB_SCHEMA <- "mirotests"
  procEnv$MIRO_DB_USERNAME <- Sys.getenv("MIRO_DB_USERNAME", "postgres")
  procEnv$MIRO_DB_PASSWORD <-Sys.getenv("MIRO_DB_PASSWORD", "")
  procEnv$MIRO_DB_NAME <- Sys.getenv("MIRO_DB_NAME", "postgres")
  procEnv$MIRO_DB_HOST <- Sys.getenv("MIRO_DB_HOST", "localhost")
  procEnv$MIRO_DB_PORT <- as.integer(Sys.getenv("MIRO_DB_PORT", "5432"))
  
  dbConfig <- list(type = "postgres",
                   username = procEnv$MIRO_DB_USERNAME,
                   password = procEnv$MIRO_DB_PASSWORD,
                   name = procEnv$MIRO_DB_NAME,
                   host = procEnv$MIRO_DB_HOST,
                   port = procEnv$MIRO_DB_PORT,
                   schema = "mirotests")
}else{
  dbPath <- file.path(testDir, "testdb",
                      "pickstock.sqlite3")
  procEnv$MIRO_DB_PATH <- dirname(dbPath)
  dbConfig <- list(type = "sqlite",
                   name = dbPath)
}
createTestDb()
db <- Db$new(uid = "te_de\\%d", 
             dbConf = dbConfig,
             slocktimeLimit = slocktimeLimit, modelName = "pickstock",
             hcubeActive = FALSE, ugroups = c("bla_blubb", "test123"))
dbSchema$setConn(db$getConn())

scenData <<- ScenData$new(db = db,
                          scenDataTemplate = list(tibble(scalar = c("error_train", "error_test", "error_ratio"),
                                                         description = "",
                                                         value = NA_character_),
                                                  tibble(symbol = character(), value = numeric()),
                                                  tibble(date = character(), dj = numeric(), `index fund` = numeric()),
                                                  tibble(date = character(), `absolute error train` = numeric(),
                                                         `absolute error test` = numeric()),
                                                  tibble(date = character(), uni = character(), value = numeric()),
                                                  tibble(date = character(), symbol = character(), value = numeric()),
                                                  tibble(scalar = character(), description = character(), value = character())),
                          hiddenOutputScalars = c("firstdaytraining", "lastdaytraining"))

test_that("Getting all scalars works without data", {
  expect_error(scenData$load(1, refId = "sb", showProgress = FALSE), NA)
  expect_identical(scenData$getScalars("sb"), tibble(scalar = character(), description = character(), value = character()))
})

populateDb(procEnv, "pickstock")

test_that("Getting all scalars works with data", {
  expect_error(scenData$load(1, refId = "sb", showProgress = FALSE), NA)
  expect_identical(scenData$getScalars("sb"),
                   tibble(scalar = c(inScalarNames, outScalarNames),
                          description = c(inScalarDesc, outScalarDesc),
                          value = c("2", "99", "CPLEX", "79.6135736013082", "951.166205306451",
                                    "11.9472869044886", "2016-01-04", "2016-05-24")))
})

test_that("includeHiddenScalars argument works when calling ScenData$get", {
  dataTmp <- scenData$get("sb")
  expect_identical(names(dataTmp), c(names(ioConfig$modelOut), ioConfig$inputDsNames))
  expect_identical(dataTmp[[scalarsOutName]], outScalarDataVisible)
  
  dataTmp <- scenData$get("sb", includeHiddenScalars = TRUE)
  expect_identical(names(dataTmp), c(names(ioConfig$modelOut), ioConfig$inputDsNames))
  expect_identical(dataTmp[[scalarsOutName]], outScalarDataFull)
  
  dataTmp <- scenData$get("sb", c(ioConfig$inputDsNames, scalarsOutName))
  expect_identical(names(dataTmp), c(ioConfig$inputDsNames, scalarsOutName))
  expect_identical(dataTmp[[scalarsOutName]], outScalarDataVisible)
  
  dataTmp <- scenData$get("sb", c(ioConfig$inputDsNames, scalarsOutName),
                          includeHiddenScalars = TRUE)
  expect_identical(names(dataTmp), c(ioConfig$inputDsNames, scalarsOutName))
  expect_identical(dataTmp[[scalarsOutName]], outScalarDataFull)
  
  expect_identical(scenData$get("sb", scalarsOutName, includeHiddenScalars = TRUE)[[1]],
                   outScalarDataFull)
  expect_identical(scenData$get("sb", scalarsOutName)[[1]], outScalarDataVisible)
  expect_identical(scenData$get("sb", scalarsOutName, includeHiddenScalars = TRUE, drop = TRUE),
                   outScalarDataFull)
  expect_identical(scenData$get("sb", scalarsOutName, drop = TRUE), outScalarDataVisible)
})

if(!identical(procEnv$MIRO_DB_TYPE, "postgres")){
  dbPath <- file.path(testDir, "testdb",
                      "indus89.sqlite3")
  procEnv$MIRO_DB_PATH <- dirname(dbPath)
  dbConfig <- list(type = "sqlite",
                   name = dbPath)
}
db$finalize()
createTestDb()
db <- Db$new(uid = "te_de\\%d", 
             dbConf = dbConfig,
             slocktimeLimit = slocktimeLimit, modelName = "pickstock",
             hcubeActive = FALSE, ugroups = c("bla_blubb", "test123"))
populateDb(procEnv, "indus89", modelPath = file.path(getwd(), "..", "model", "indus89"))

ioConfig <<- list(modelOut = list(x = list()),
                  inputDsNames = c())
dbSchema <<- DbSchema$new(list(schema = list(x = list(tabName = "x",
                                                      colNames = c("z", "g", "c", "t", "s",
                                                                   "w", "level", "marginal",
                                                                   "lower", "upper", "scale"),
                                                      colTypes = "ccccccddddd")), views = list()))
dbSchema$setConn(db$getConn())

scenData <- ScenData$new(db = db,
                         scenDataTemplate = list(tibble(z = character(),
                                                        g = character(),
                                                        c = character(),
                                                        t = character(),
                                                        s = character(),
                                                        w = character(),
                                                        level = numeric(),
                                                        marginal = numeric(),
                                                        lower = numeric(),
                                                        upper = numeric(),
                                                        scale = numeric())),
                         hiddenOutputScalars = character())

test_that("Getting all scalars works without scalars defined in app", {
  expect_error(scenData$load(1, refId = "sb", showProgress = FALSE), NA)
  expect_identical(scenData$getScalars("sb"), tibble(scalar = character(), description = character(), value = character()))
})

db$finalize()
