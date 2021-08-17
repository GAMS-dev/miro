context("Unit tests - DbMigrator class")
library(futile.logger)
library(DBI)
library(RSQLite)

source("../../components/db_schema.R")
source("../../components/db.R")
source("../../components/db_migrator.R")

modelName <- "transport"

skipPostgres <- TRUE
if(identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
  skipPostgres <- FALSE
  createTestDb()
}

ioConfig <<- list(modelOut = list(results = NULL, `_scalars_out` = NULL),
                  inputDsNames = c("a", "b", "d", "ilocdata", "jlocdata"),
                  hcubeScalars = character())

dbSchema <<- DbSchema$new(list(schema = list(results = list(tabName = "results",
                                                            colNames = c("i", "j", "cap", "demand", "quantities", "bla"),
                                                            colTypes = "ccdddd"), 
                                             total_cost = list(tabName = "total_cost",
                                                               colNames = "total_cost",
                                                               colTypes = "c"),
                                             a = list(tabName = "a",
                                                      colNames = c("i", "j", "value"),
                                                      colTypes = "ccd"),
                                             b = list(tabName = "b",
                                                      colNames = c("k", "value"),
                                                      colTypes = "cd"),
                                             d = list(tabName = "d",
                                                      colNames = c("i", 
                                                                   "j", "k", "value"),
                                                      colTypes = "cccd"),
                                             ilocdata = list(tabName = "ilocdata",
                                                             colNames = c("i", "lng", "lat"),
                                                             colTypes = "cdd"), 
                                             jlocdata = list(tabName = "jlocdata",
                                                             colNames = c("j", "lng", "lat"),
                                                             colTypes = "cdd")),
                               views = list("_scalars_out" = "total_cost")))

for(dbType in c("sqlite", "postgres")){
  if(dbType == "postgres" && skipPostgres){
    next
  }
  procEnv <- list(R_LIBS_USER = Sys.getenv("LIB_PATH"),
                  GAMS_SYS_DIR = Sys.getenv("GAMS_SYS_DIR"))
  
  if(identical(dbType, "sqlite")){
    dbPath <- file.path(testDir, "transport.sqlite3")
    procEnv$MIRO_DB_PATH <- dirname(dbPath)
    
    unlink(dbPath)
    
    dbConfig <- list(type = "sqlite",
                     name = dbPath)
  }else{
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
  }
  populateDb(procEnv, "transport")
  db <- Db$new(uid = "user", dbConf = dbConfig,
               slocktimeLimit = 20, modelName = modelName,
               hcubeActive = FALSE, ugroups = "users", forceNew = TRUE)
  conn <- db$getConn()
  dbSchema$setConn(conn)
  
  dbMigrator <- DbMigrator$new(db)
  
  test_that(paste0("Validating migration config works (", dbType, ")"), {
    migrationConfig <- list(results = list(oldTableName  = "schedule1",
                                           colNames = c("i", "j", "cap", 
                                                        "demand", "quantities", "-")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_config", regex = "schedule1")
    migrationConfig <- list(results = list(oldTableName  = "schedule",
                                           colNames = c("i", "j", "cap", 
                                                        "demand", "quantities", "-")),
                            a = list(oldTableName = "schedule", colNames = c("i", "-", "demand")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_config", regex = "duplicate")
    migrationConfig <- list(a = list(oldTableName = "a", colNames = c("i", "-", "demand", "-")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_config", regex = "Length of column names")
    migrationConfig <- list(aa = list(oldTableName = "a", colNames = c("i", "-", "demand", "-")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_config", regex = "aa")
  })
  
  test_that(paste0("Migrating tables works (", dbType, ")"), {
    migrationConfig <- list(results = list(oldTableName  = "schedule",
                                           colNames = c("i", "j", "cap", 
                                                        "demand", "quantities", "-")),
                            a = list(oldTableName = "a", colNames = c("i", "-", "value")),
                            b = list(oldTableName = "b", colNames = c("j", "value")),
                            total_cost = list(oldTableName = "total_cost", colNames = "total_cost"))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_data_loss", regex = "forceRemove")
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = TRUE), NA)
    expect_identical(tolower(dbMigrator$.__enclos_env__$private$getTableInfo("total_cost")$colTypes),
                     "text")
    expect_identical(dbReadTable(conn, "results")[-1],
                     data.frame(i = c("Seattle", "Seattle", "Seattle", "San-Diego", "San-Diego", "San-Diego"),
                                j = c("New-york", "Chicago", "Topeka", "New-york", "Chicago", "Topeka"),
                                cap = c(350, 350, 350, 600, 600, 600),
                                demand = c(325, 300, 275, 325, 300, 275),
                                quantities = c(50, 300, NA, 275, NA, 275),
                                bla = NA_real_))
    expect_identical(dbReadTable(conn, "a")[-1],
                     data.frame(i = c("Seattle", "San-Diego"),
                                j = NA_character_,
                                value = c(350, 600)))
    expect_identical(dbReadTable(conn, "b")[-1],
                     data.frame(k = c("New-york", "Chicago", "Topeka"),
                                value = c(325, 300, 275)))
    migrationConfig <- list(a = list(oldTableName = "a", colNames = c("-", "-", "value")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_data_loss", regex = "forceRemove")
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = TRUE), NA)
    expect_identical(dbReadTable(conn, "a")[-1],
                     data.frame(i = NA_character_,
                                j = NA_character_,
                                value = c(350, 600)))
    # swap columns
    migrationConfig <- list(ilocdata = list(oldTableName = "ilocdata",
                                            colNames = c("i", "lat", "lng")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), NA)
    expect_identical(dbReadTable(conn, "ilocdata")[-1],
                     data.frame(i = c("Seattle", "San-Diego"),
                                lng = c(47.608013, 32.715736),
                                lat = c(-122.335167, -117.161087)))
    migrationConfig <- list(ilocdata = list(oldTableName = "ilocdata",
                                            colNames = c("i", "lat", "lng")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), NA)
    expect_identical(dbReadTable(conn, "ilocdata")[-1],
                     data.frame(i = c("Seattle", "San-Diego"),
                                lng = c(-122.335167, -117.161087),
                                lat = c(47.608013, 32.715736)))
    migrationConfig <- list(d = list(oldTableName = "d",
                                     colNames = c("i", "j", "j", "value")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), NA)
    expect_identical(dbReadTable(conn, "d")[-1],
                     data.frame(i = c("Seattle", "Seattle", "Seattle",
                                      "San-Diego", "San-Diego", "San-Diego"),
                                j = c("New-york", "Chicago", "Topeka",
                                      "New-york", "Chicago", "Topeka"),
                                k = c("New-york", "Chicago", "Topeka",
                                      "New-york", "Chicago", "Topeka"),
                                value = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)))
    migrationConfig <- list(ilocdata = list(oldTableName = "jlocdata",
                                            colNames = c("j", "lng", "lat")),
                            jlocdata = list(oldTableName = "ilocdata",
                                            colNames = c("i", "lng", "lat")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), NA)
    expect_identical(dbReadTable(conn, "jlocdata")[-1],
                     data.frame(j = c("Seattle", "San-Diego"),
                                lng = c(-122.335167, -117.161087),
                                lat = c(47.608013, 32.715736)))
    expect_identical(dbReadTable(conn, "ilocdata")[-1],
                     data.frame(i = c("New-york", "Chicago", "Topeka"),
                                lng = c(-73.935242, -87.623177, -95.695312),
                                lat = c(40.73061, 41.881832, 39.056198)))
  })
}

modelName <- "pickstock"

skipPostgres <- TRUE
if(identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
  skipPostgres <- FALSE
  createTestDb()
}

ioConfig <<- list(modelOut = list(dowvsindex = NULL, stock_weight = NULL, `_scalars_out` = NULL),
                  inputDsNames = character(),
                  hcubeScalars = character())

dbSchema <<- DbSchema$new(list(schema = list(dowvsindex = list(tabName = "dowvsindex",
                                                            colNames = c("date", "index fund"),
                                                            colTypes = "cd"),
                                             stock_weight = list(tabName = "stock_weight",
                                                                 colNames = c("symbol", "value"),
                                                                 colTypes = "cd"),
                                             error_train2 = list(tabName = "error_train2",
                                                                 colNames = "error_train2",
                                                                 colTypes = "d")),
                               views = list(`_scalars_out` = "error_train2")))

for(dbType in c("sqlite", "postgres")){
  if(dbType == "postgres" && skipPostgres){
    skip("Skipping Postgres tests as MIRO_DB_TYPE is not set to 'postgres'")
  }
  procEnv <- list(R_LIBS_USER = Sys.getenv("LIB_PATH"),
                  GAMS_SYS_DIR = Sys.getenv("GAMS_SYS_DIR"))
  
  if(identical(dbType, "sqlite")){
    dbPath <- file.path(testDir, "pickstock.sqlite3")
    procEnv$MIRO_DB_PATH <- dirname(dbPath)
    
    unlink(dbPath)
    
    dbConfig <- list(type = "sqlite",
                     name = dbPath)
  }else{
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
  }
  populateDb(procEnv, "pickstock")
  db$finalize()
  db <- Db$new(uid = "user", dbConf = dbConfig,
               slocktimeLimit = 20, modelName = modelName,
               hcubeActive = FALSE, ugroups = "users", forceNew = TRUE)
  conn <- db$getConn()
  dbSchema$setConn(conn)
  
  dbMigrator <- DbMigrator$new(db)
  
  test_that(paste0("Migrating tables works (", dbType, "), pickstock"), {
    migrationConfig <- list(dowvsindex = list(oldTableName  = "dowvsindex",
                                              colNames = c("date", "index fund")),
                            stock_weight = list(oldTableName = "stock_weight",
                                                colNames = c("-", "-")),
                            error_train2 = list(oldTableName = "error_train",
                                                colNames = "-"))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = TRUE), NA)
    expect_equal(dbReadTable(conn, "dowvsindex")[-1][1:2, ],
                 data.frame(date = c("2016-01-04", "2016-01-05"),
                            "index fund" = c(98.3977098527647, 
                                             99.9596599723713)))
    expect_false(dbExistsTable(conn, "stock_weight"))
    expect_equal(dbReadTable(conn, "_scalars_out")[-1],
                 data.frame(error_train2 = NA_real_))
  })
}

db$finalize()
