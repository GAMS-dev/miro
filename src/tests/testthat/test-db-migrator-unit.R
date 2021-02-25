context("Unit tests - DbMigrator class")
library(futile.logger)
library(DBI)
library(RSQLite)

source("../../components/db.R")
source("../../components/db_migrator.R")

ioConfig <<- list(hcubeScalars = character())

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


testDir <- file.path(getwd(), "..")
dbSchema <- list(tabName = c(`_scenMeta` = "_sys_metadata_transport", `_scenLock` = "_sys_scenlocks_transport", 
                             `_scenTrc` = "_sys_trace_transport", `_scenAttach` = "_sys_attach_transport", 
                             `_scenScripts` = "_sys_scripts_transport", `_jobMeta` = "_sys_jobs_transport", 
                             "transport_results", "transport__scalars_out", "transport_a", 
                             "transport_b", "transport_d", "transport_ilocdata", 
                             "transport__scalars"),
                 colNames = list(`_scenMeta` = c(sid = "_sid", 
                                                 uid = "_uid", sname = "_sname", stime = "_stime", stag = "_stag", 
                                                 accessR = "_accessr", accessW = "_accessw", accessX = "_accessx", 
                                                 scode = "_scode"), `_scenLock` = c(uid = "_uid", sid = "_sid", 
                                                                                    lock = "_slocktime"),
                                 `_scenTrc` = c("InputFileName", "ModelType", 
                                                "SolverName", "NLP", "MIP", "JulianDate", "Direction", "NumberOfEquations", 
                                                "NumberOfVariables", "NumberOfDiscreteVariables", "NumberOfNonZeros", 
                                                "NumberOfNonlinearNonZeros", "OptionFile", "ModelStatus", "SolverStatus", 
                                                "ObjectiveValue", "ObjectiveValueEstimate", "SolverTime", "NumberOfIterations", 
                                                "NumberOfDomainViolations", "NumberOfNodes", "#User1"),
                                 `_scenAttach` = c(sid = "_sid", 
                                                   fn = "fileName", fExt = "fileExt", execPerm = "execPerm", content = "fileContent", 
                                                   time = "timestamp"), `_scenScripts` = c(sid = "_sid", id = "id", 
                                                                                           content = "scriptContent"),
                                 `_jobMeta` = c(jid = "_jid", uid = "_uid", 
                                                status = "_status", time = "_jtime", tag = "_stag", pid = "_pid", 
                                                sid = "_sid", gamsret = "_gamsret", scode = "_scode", sname = "_sname"
                                 ),
                                 results = c("i", "j", "lngp", "latp", "lngm", "latm", "cap", 
                                              "demand", "quantities", "bla"),
                                 `_scalars_out` = c("scalar", "description", 
                                                    "value"),
                                 a = c("i", "j", "value"),
                                 b = c("k", "value"),
                                 d = c("i", 
                                       "j", "value"),
                                 ilocdata = c("i", "lat", "lng"),
                                 `_scalars` = c("scalar", "description", "value"
                                 )),
                 colTypes = c(`_scenMeta` = "iccTcccci", `_scenLock` = "ciT", 
                              `_scenTrc` = "cccccdidddddiiiddddddc", `_scenAttach` = "icclbT", 
                              `_scenScripts` = "icc", `_jobMeta` = "iciTcciiic", results = "ccdddddddd", 
                              `_scalars_out` = "ccc", a = "ccd", b = "cd", d = "ccd", ilocdata = "cdd", 
                              `_scalars` = "ccc"))

if(identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
  dbTypes <- c("sqlite", "postgres")
}else{
  dbTypes <- "sqlite"
}

for(dbType in dbTypes){
  procEnv <- list()
  
  if(identical(dbType, "sqlite")){
    dbPath <- file.path(testDir, "miro.sqlite3")
    procEnv$MIRO_DB_PATH <- dirname(dbPath)
    
    unlink(dbPath)
    
    dbConfig <- list(type = "sqlite",
                     name = dbPath)
  }else{
    procEnv$MIRO_DB_TYPE <- "postgres"
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
                     port = procEnv$MIRO_DB_PORT)
  }
  db <- Db$new(uid = "user", dbConf = dbConfig, dbSchema = dbSchema,
               slocktimeLimit = 20, modelName = modelName,
               hcubeActive = FALSE, ugroups = "users", forceNew = TRUE)
  conn <- db$getConn()
  
  populateDb(procEnv)
  dbMigrator <- DbMigrator$new(db)
  
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
    expect_identical(dbReadTable(conn, "transport_results")[-1],
                     data.frame(i = c("Seattle", "Seattle", "Seattle", "San-Diego", "San-Diego", "San-Diego"),
                                j = c("New-york", "Chicago", "Topeka", "New-york", "Chicago", "Topeka"),
                                lngp = c(-122.335167, -122.335167, -122.335167, -117.161087, -117.161087, -117.161087),
                                latp = c(47.608013, 47.608013, 47.608013, 32.715736, 32.715736, 32.715736),
                                lngm = c(-73.935242, -87.623177, -95.695312, -73.935242, -87.623177, -95.695312), 
                                latm = c(40.73061, 41.881832, 39.056198, 40.73061, 41.881832, 39.056198), cap = c(350, 350, 350, 600, 600, 600),
                                demand = c(325, 300, 275, 325, 300, 275),
                                quantities = c(50, 300, NA, 275, NA, 275),
                                bla = NA_real_))
    expect_identical(dbReadTable(conn, "transport_a")[-1],
                     data.frame(i = c("Seattle", "San-Diego"),
                                j = NA_character_,
                                value = c(350, 600)))
    expect_identical(dbReadTable(conn, "transport_b")[-1],
                     data.frame(k = c("New-york", "Chicago", "Topeka"),
                                value = c(325, 300, 275)))
    migrationConfig <- list(results = list(oldTableName  = "schedule",
                                           colNames = c("i", "j", "lngp", "latp", "lngm", "latm", "cap", 
                                                        "demand", "quantities", "-")),
                            a = list(oldTableName = "a", colNames = c("i", "-", "value")),
                            b = list(oldTableName = "b", colNames = c("j", "value")))
  })
}


