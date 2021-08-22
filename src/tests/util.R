getSelectizeOptions <- function(app, selector){
  app$getDebugLog("browser")
  app$waitFor(paste0("var options=$('", selector, "')[0].selectize.options;options=Object.keys(options);for(i in options){console.log(options[i])};true"))
  options <- app$getDebugLog("browser")$message
  return(rev(substr(options, 1, nchar(options) -4)))
}
getSelectizeAliases <- function(app, selector){
  app$getDebugLog("browser")
  app$waitFor(paste0("var options=$('", selector, "')[0].selectize.options;for(key in options){console.log(options[key].label)};true"))
  options <- app$getDebugLog("browser")$message
  return(rev(substr(options, 1, nchar(options) -4)))
}
getVisibleDtData <- function(app, id){
  app$waitFor(paste0("console.log(JSON.stringify($('#", id, "').data('datatable').data().toArray()))"), timeout = 50L)
  dtData <- app$getDebugLog("browser")$message
  return(tibble::as_tibble(jsonlite::fromJSON(substr(dtData, 1, nchar(dtData) -4)),
                           .name_repair = "universal"))
}
getHotData <- function(app, id){
  hotToR <- function(data){
    return(suppressWarnings(as_tibble(
      data.table::rbindlist(data$data, use.names = FALSE))))
  }
  return(hotToR(jsonlite::fromJSON(app$getAllValues()$output[[id]], simplifyDataFrame = FALSE, simplifyMatrix = FALSE)$x))
}
expect_options <- function(options, optionsExpected){
  expect_true(all(options %in% optionsExpected) &&
                identical(length(optionsExpected), length(options)))
}
addSelectizeOption <- function(app, selector, value, alias = value){
  return(app$waitFor(paste0("$('", selector, "')[0].selectize.addOption({value:'", value, "',label: '", alias, "'});true;"), timeout = 50))
}
selectSelectizeOption <- function(app, selector, value){
  return(app$waitFor(paste0("$('", selector, "')[0].selectize.addItem('", value, "');true;"), timeout = 50))
}

expect_download_size <- function(app, id, filename, tolerance = 100){
  url <- app$findElement(paste0("#", id))$getAttribute("href")
  req <- httr::GET(url)
  filePath <- file.path(getwd(), "data", "downloads-expected", basename(app$getSnapshotDir()))
  if(!file.exists(filePath)){
    if(!dir.create(filePath, recursive = TRUE)){
      stop("Could not create file downloads test directory", call. = FALSE)
    }
  }
  if(file.exists(file.path(filePath, filename))){
    tempFiles <- file.path(tempdir(), "shinytest-download")
    writeBin(req$content, tempFiles)
    expect_equal(file.info(tempFiles)$size, file.info(file.path(filePath, filename))$size,
                 tolerance = tolerance)
  }else{
    writeBin(req$content, file.path(filePath, filename))
  }
}

expect_files_in_zip <- function(app, id, files){
  url <- app$findElement(paste0("#", id))$getAttribute("href")
  req <- httr::GET(url)
  tempFiles <- file.path(tempdir(check = TRUE), "shinytest-download")
  on.exit(unlink(tempFiles))
  writeBin(req$content, tempFiles)
  filesInZip <- zip::zip_list(tempFiles)$filename
  expect_identical(length(filesInZip), length(files))
  expect_true(all(files %in% filesInZip))
}

expect_symbols_in_gdx <- function(app, id, symNames){
  url <- app$findElement(paste0("#", id))$getAttribute("href")
  req <- httr::GET(url)
  tempFiles <- file.path(tempdir(check = TRUE), "shinytest-download.gdx")
  on.exit(unlink(tempFiles))
  writeBin(req$content, tempFiles)
  if(length(names(symNames))){
    filesInZip <- zip::zip_list(tempFiles)$filename
    expect_identical(length(filesInZip), length(symNames))
    gdxFileNames <- paste0(names(symNames), ".gdx")
    expect_true(all(gdxFileNames %in% filesInZip))
    tmpPath <- file.path(tempdir(check = TRUE), as.character(as.numeric(Sys.time())))
    if(!dir.create(tmpPath)){
      stop(sprintf("Could not create tmpdir: %s", tmpPath))
    }
    on.exit(unlink(tmpPath, recursive = TRUE), add = TRUE)
    zip::unzip(tempFiles, exdir = tmpPath, files = gdxFileNames)
    tempFiles <- file.path(tmpPath, gdxFileNames)
  }
  gdxrrwMIRO::igdx(file.path(.libPaths()[1], "gdxrrwMIRO",
                             if(identical(tolower(Sys.info()[["sysname"]]), "windows"))
                               file.path("bin", "x64") else "bin"), silent = TRUE, returnStr = FALSE)
  for(tempFile in tempFiles){
    symInGdx <- gdxrrwMIRO::gdxInfo(tempFile, returnList = TRUE, dump = FALSE)
    symInGdx <- c(symInGdx$sets, symInGdx$parameters, symInGdx$variables, symInGdx$equations)
    if(length(tempFiles) > 1L){
      scenNameTmp <- gsub("\\.gdx$", "", basename(tempFile))
      expect_identical(length(symInGdx), length(symNames[[scenNameTmp]]))
      expect_true(all(symNames[[scenNameTmp]] %in% symInGdx))
    }else{
      expect_identical(length(symInGdx), length(symNames))
      expect_true(all(symNames %in% symInGdx))
    }
  }
}

expect_sheets_in_xls <- function(app, id, sheetNames){
  url <- app$findElement(paste0("#", id))$getAttribute("href")
  req <- httr::GET(url)
  tempFiles <- file.path(tempdir(check = TRUE), "shinytest-download")
  on.exit(unlink(tempFiles))
  writeBin(req$content, tempFiles)
  sheetsInXls <- readxl::excel_sheets(tempFiles)
  expect_identical(length(sheetsInXls), length(sheetNames))
  expect_true(all(sheetNames %in% sheetsInXls))
}

get_downloaded_file_content <- function(app, id, raw = FALSE){
  url <- app$findElement(paste0("#", id))$getAttribute("href")
  req <- httr::GET(url)
  if(raw)
    return(req$content)
  return(rawToChar(req$content))
}

connectDb <- function(modelName = NULL, dbPath = file.path(getwd(), "..", "testdb")){
  if(identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
    conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
                           dbname = Sys.getenv("MIRO_DB_NAME"),
                           host = Sys.getenv("MIRO_DB_HOST"),
                           port = 5432,
                           user = Sys.getenv("MIRO_DB_USERNAME"),
                           password = Sys.getenv("MIRO_DB_PASSWORD"),
                           bigint = "integer")
    DBI::dbExecute(conn, paste0("SET search_path TO ",
                                Sys.getenv("MIRO_DB_SCHEMA"),
                                ";"))
    return(conn)
  }
  return(DBI::dbConnect(drv = RSQLite::SQLite(),
                        dbname = file.path(dbPath, paste0(modelName, ".sqlite3")), bigint = "integer"))
}

createTestDb <- function(dbPath = file.path(getwd(), "..", "testdb")){
  if(identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")){
    # need to clean db tables
    conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
                           dbname = Sys.getenv("MIRO_DB_NAME"),
                           host = Sys.getenv("MIRO_DB_HOST"),
                           port = 5432,
                           user = Sys.getenv("MIRO_DB_USERNAME"),
                           password = Sys.getenv("MIRO_DB_PASSWORD"),
                           bigint = "integer")
    on.exit(DBI::dbDisconnect(conn))
    DBI::dbExecute(conn, DBI::SQL("SET client_min_messages TO WARNING;"))
    DBI::dbExecute(conn, DBI::SQL("DROP SCHEMA IF EXISTS mirotests CASCADE;"))
    DBI::dbExecute(conn, DBI::SQL("CREATE SCHEMA mirotests;"))
    DBI::dbExecute(conn, DBI::SQL(paste0("GRANT ALL ON SCHEMA mirotests TO ",
                                         DBI::dbQuoteIdentifier(conn, Sys.getenv("MIRO_DB_USERNAME")), ";")))
    Sys.setenv(MIRO_DB_SCHEMA = "mirotests")
  }else{
    if(file.exists(dbPath)){
      if(unlink(dbPath, force = TRUE, recursive = TRUE)){
        gc()
        if(unlink(dbPath, force = TRUE, recursive = TRUE)){
          stop("Could not remove old database SQLite file for tests")
        }
      }
      if(!dir.create(dbPath)){
        stop(sprintf("Could not create test database path: '%s'.", dbPath))
      }
    }
    Sys.setenv(MIRO_DB_PATH = dbPath)
  }
}

saveAdditionalGamsClArgs <- function(miroModelDir, modelToTest, additionalGamsClArgs){
  if(!length(additionalGamsClArgs)){
    return()
  }
  configJSONFileName <- file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
  file.copy(configJSONFileName,
            file.path(dirname(configJSONFileName), paste0(modelToTest, "_tmp.json")), overwrite = TRUE)
  configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
                                                    simplifyDataFrame = FALSE,
                                                    simplifyMatrix = FALSE))
  configJSON$extraClArgs <- c(configJSON$extraClArgs, additionalGamsClArgs)
  jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  return(invisible())
}
PerformanceReporter <- R6::R6Class("PerformanceReporter", public = list(
  initialize = function(){
    private$url <- Sys.getenv("MIRO_REPORTER_URL", unset = NA)
    if(!is.na(private$url)){
      private$user <- Sys.getenv("MIRO_REPORTER_USER", unset = NA)
      private$pass <- Sys.getenv("MIRO_REPORTER_PASS", unset = NA)
    }
  },
  measure = function(id, expression){
    expression_call <- substitute(expression)
    time <- Sys.time()
    eval(expression_call, parent.frame())
    private$data[[id]] <- as.numeric(difftime(Sys.time(), time, units = "secs"))
    return(invisible(self))
  },
  getData = function(){
    return(private$data)
  },
  publish = function(){
    context <- eval(substitute(testthat::get_reporter()$.context),
                    envir = parent.frame())
    dataToPublish <- list(context = context,
                          data = private$data)
    private$data <- list()
    if(is.na(private$pass)){
      warning("No reporter password set. Skipping publishing performance results.")
      print(dataToPublish)
      return(return(invisible(self)))
    }
    tryCatch({
      req <- httr::POST(private$url, body = dataToPublish,
                        httr::authenticate(private$user, private$pass),
                        encode = "json",
                        httr::timeout(3L))
      if(httr::status_code(req) != 200L){
        stop(sprintf("Bad status code: %s", httr::status_code(req)), call. = FALSE)
      }
    }, error = function(e){
      warning(sprintf("Problems publishing performance results. Error message: %s",
                      conditionMessage(e)))
    })
    return(return(invisible(self)))
  }
), private = list(
  data = list(),
  url = NA,
  user = NA,
  pass = NA
))
populateDb <- function(procEnv, modelName, modelPath = NULL){
  procEnv$MIRO_POPULATE_DB <- "true"
  miroAppPath <- file.path(getwd(), "..", "..")
  if(is.null(modelPath)){
    modelPath <- file.path(miroAppPath, "model", modelName)
  }
  procEnv$MIRO_MODEL_PATH <- file.path(modelPath, paste0(modelName, ".gms"))
  procEnv$MIRO_DATA_DIR <- file.path(modelPath, paste0("data_", modelName),
                                     "default.gdx")
  procEnv$MIRO_OVERWRITE_SCEN_IMPORT <- "true"

  miroProc <- processx::run(file.path(R.home("bin"), "R"),
                            c("-e",
                              paste0("shiny::runApp('", miroAppPath, "',port=3839,host='0.0.0.0')")),
                            env = unlist(procEnv), wd = miroAppPath, error_on_status = FALSE, timeout = 30)
  if(miroProc$status != 0L){
    print(miroProc$stdout)
    print(miroProc$stderr)
  }
}
