context("Unit tests - DbMigrator class")
library(futile.logger)
library(DBI)
library(RSQLite)

source("../../components/db_schema.R")
source("../../components/db.R")
source("../../components/db_migrator.R")

ioConfig <<- list(modelOut = list(results = NULL, `_scalars_out` = NULL),
                  inputDsNames = c("a", "b", "d", "ilocdata", "jlocdata"),
                  hcubeScalars = character())

miroAppPath <- file.path(getwd(), "..", "..")

populateDb <- function(procEnv){
  procEnv$MIRO_POPULATE_DB <- "true"
  modelPath <- file.path(miroAppPath, "model", "transport")
  procEnv$MIRO_MODEL_PATH <- file.path(modelPath, paste0(modelName, ".gms"))
  procEnv$MIRO_DATA_DIR <- file.path(modelPath, "data_transport",
                                     "default.gdx")
  procEnv$MIRO_OVERWRITE_SCEN_IMPORT <- "true"
  
  miroProc <- processx::process$new(file.path(R.home("bin"), "R"),
                                    c("-e", 
                                      paste0("shiny::runApp('", miroAppPath, "',port=3839,host='0.0.0.0')")),
                                    env = unlist(procEnv), wd = miroAppPath, stderr = "|", stdout = "|")
  miroProc$wait()
  if(miroProc$get_exit_status() != 0L){
    print(miroProc$read_all_output())
    print(miroProc$read_all_error())
  }
}

modelName <- "transport"

skipPostgres <- TRUE
if(identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
  skipPostgres <- FALSE
  createTestDb()
}

dbSchema <<- DbSchema$new(list(results = list(tabName = "results",
                                              colNames = c("i", "j", "lngp", "latp", "lngm", "latm", "cap", 
                                                           "demand", "quantities", "bla"),
                                              colTypes = "ccdddddddd"), 
                               `_scalars_out` = list(tabName = "_scalars_out",
                                                     colNames = c("scalar", "description", 
                                                                  "value"),
                                                     colTypes = "ccc"),
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
                                               colTypes = "cdd")))

for(dbType in c("sqlite", "postgres")){
  if(dbType == "postgres" && skipPostgres){
    skip("Skipping Postgres tests as MIRO_DB_TYPE is not set to 'postgres'")
  }
  procEnv <- list()
  
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
  
  db <- Db$new(uid = "user", dbConf = dbConfig,
               slocktimeLimit = 20, modelName = modelName,
               hcubeActive = FALSE, ugroups = "users", forceNew = TRUE)
  conn <- db$getConn()
  dbSchema$setConn(conn)
  
  populateDb(procEnv)
  dbMigrator <- DbMigrator$new(db)
  
  test_that(paste0("Validating migration config works (", dbType, ")"), {
    migrationConfig <- list(results = list(oldTableName  = "schedule1",
                                           colNames = c("i", "j", "lngp", "latp", "lngm", "latm", "cap", 
                                                        "demand", "quantities", "-")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_config", regex = "schedule1")
    migrationConfig <- list(results = list(oldTableName  = "schedule",
                                           colNames = c("i", "j", "lngp", "latp", "lngm", "latm", "cap", 
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
                                           colNames = c("i", "j", "lngp", "latp", "lngm", "latm", "cap", 
                                                        "demand", "quantities", "-")),
                            a = list(oldTableName = "a", colNames = c("i", "-", "value")),
                            b = list(oldTableName = "b", colNames = c("j", "value")))
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = FALSE), class = "error_data_loss", regex = "forceRemove")
    expect_error(dbMigrator$migrateDb(migrationConfig,
                                      forceRemove = TRUE), NA)
    expect_identical(dbReadTable(conn, "results")[-1],
                     data.frame(i = c("Seattle", "Seattle", "Seattle", "San-Diego", "San-Diego", "San-Diego"),
                                j = c("New-york", "Chicago", "Topeka", "New-york", "Chicago", "Topeka"),
                                lngp = c(-122.335167, -122.335167, -122.335167, -117.161087, -117.161087, -117.161087),
                                latp = c(47.608013, 47.608013, 47.608013, 32.715736, 32.715736, 32.715736),
                                lngm = c(-73.935242, -87.623177, -95.695312, -73.935242, -87.623177, -95.695312),
                                latm = c(40.73061, 41.881832, 39.056198, 40.73061, 41.881832, 39.056198), cap = c(350, 350, 350, 600, 600, 600),
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


