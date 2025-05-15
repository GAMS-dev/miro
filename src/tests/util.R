getSelectizeOptions <- function(app, selector) {
  options <- app$get_js(paste0("Object.keys($('", selector, "')[0].selectize.options);"))
  return(options)
}
getSelectizeAliases <- function(app, selector) {
  options <- app$get_js(paste0("Object.values($('", selector, "')[0].selectize.options).map(option => option.label)"))
  return(unlist(options))
}
getVisibleDtData <- function(app, id) {
  dtData <- app$get_js(paste0("JSON.stringify($('#", id, "').data('datatable').data().toArray())"))
  return(tibble::as_tibble(jsonlite::fromJSON(dtData),
    .name_repair = "universal"
  ))
}
getHotData <- function(app, id) {
  hotToR <- function(data) {
    return(suppressWarnings(as_tibble(
      data.table::rbindlist(data$data, use.names = FALSE)
    )))
  }
  return(hotToR(jsonlite::fromJSON(app$get_values()$output[[id]], simplifyDataFrame = FALSE, simplifyMatrix = FALSE)$x))
}
expect_chartjs <- function(app, id, data, labels, tolerance = 1e-6) {
  chartjsData <- jsonlite::fromJSON(app$get_values()$output[[id]])$x$data
  if (is.list(data)) {
    expect_equal(chartjsData$datasets$data, data, tolerance = tolerance)
  } else {
    expect_equal(chartjsData$datasets$data[[1]], data, tolerance = tolerance)
  }
  expect_identical(chartjsData$labels, labels)
}
expect_options <- function(options, optionsExpected) {
  expect_true(all(options %in% optionsExpected) &&
    identical(length(optionsExpected), length(options)))
}
addSelectizeOption <- function(app, selector, value, alias = value) {
  return(app$run_js(paste0("$('", selector, "')[0].selectize.addOption({value:'", value, "',label: '", alias, "'});")))
}
selectSelectizeOption <- function(app, selector, value) {
  return(app$run_js(paste0("$('", selector, "')[0].selectize.addItem('", value, "');")))
}

expect_download <- function(app, id, filename) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  filePath <- file.path(getwd(), "testthat", "_snaps")
  if (!file.exists(filePath)) {
    if (!dir.create(filePath, recursive = TRUE)) {
      stop("Could not create file downloads test directory", call. = FALSE)
    }
  }
  if (file.exists(file.path(filePath, filename))) {
    if (is.raw(req$content)) {
      expect_identical(req$content, readr::read_file_raw(file.path(filePath, filename)))
    } else {
      expect_identical(req$content, readr::read_file(file.path(filePath, filename)))
    }
  } else {
    writeBin(req$content, file.path(filePath, filename))
  }
}

expect_download_size <- function(app, id, filename, tolerance = 100) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  filePath <- file.path(getwd(), "testthat", "_snaps")
  if (!file.exists(filePath)) {
    if (!dir.create(filePath, recursive = TRUE)) {
      stop("Could not create file downloads test directory", call. = FALSE)
    }
  }
  if (file.exists(file.path(filePath, filename))) {
    tempFiles <- file.path(tempdir(), "shinytest-download")
    writeBin(req$content, tempFiles)
    expect_equal(file.info(tempFiles)$size, file.info(file.path(filePath, filename))$size,
      tolerance = tolerance
    )
  } else {
    writeBin(req$content, file.path(filePath, filename))
  }
}

get_file_content <- function(app, id) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  return(req$content)
}

expect_files_in_zip <- function(app, id, files) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  tempFiles <- file.path(tempdir(check = TRUE), "shinytest-download")
  on.exit(unlink(tempFiles))
  writeBin(req$content, tempFiles)
  filesInZip <- zip::zip_list(tempFiles)$filename
  expect_identical(length(filesInZip), length(files))
  expect_true(all(files %in% filesInZip))
}

expect_symbols_in_gdx <- function(app, id, symNames) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  tempFiles <- file.path(tempdir(check = TRUE), "shinytest-download.gdx")
  on.exit(unlink(tempFiles))
  writeBin(req$content, tempFiles)
  if (length(names(symNames))) {
    filesInZip <- zip::zip_list(tempFiles)$filename
    expect_identical(length(filesInZip), length(symNames))
    gdxFileNames <- paste0(names(symNames), ".gdx")
    expect_true(all(gdxFileNames %in% filesInZip))
    tmpPath <- file.path(tempdir(check = TRUE), as.character(as.numeric(Sys.time())))
    if (!dir.create(tmpPath)) {
      stop(sprintf("Could not create tmpdir: %s", tmpPath))
    }
    on.exit(unlink(tmpPath, recursive = TRUE), add = TRUE)
    zip::unzip(tempFiles, exdir = tmpPath, files = gdxFileNames)
    tempFiles <- file.path(tmpPath, gdxFileNames)
  }
  gdxrrwMIRO::igdx(file.path(
    .libPaths()[1], "gdxrrwMIRO",
    if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
      file.path("bin", "x64")
    } else {
      "bin"
    }
  ), silent = TRUE, returnStr = FALSE)
  for (tempFile in tempFiles) {
    symInGdx <- gdxrrwMIRO::gdxInfo(tempFile, returnList = TRUE, dump = FALSE)
    symInGdx <- c(symInGdx$sets, symInGdx$parameters, symInGdx$variables, symInGdx$equations)
    if (length(tempFiles) > 1L) {
      scenNameTmp <- gsub("\\.gdx$", "", basename(tempFile))
      expect_identical(length(symInGdx), length(symNames[[scenNameTmp]]))
      expect_true(all(symNames[[scenNameTmp]] %in% symInGdx))
    } else {
      expect_identical(length(symInGdx), length(symNames))
      expect_true(all(symNames %in% symInGdx))
    }
  }
}

expect_symbols_in_miroscen <- function(app, id, symNames) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  tempFiles <- file.path(tempdir(check = TRUE), "shinytest-download.miroscen")
  on.exit(unlink(tempFiles))
  writeBin(req$content, tempFiles)
  tmpPath <- file.path(tempdir(check = TRUE), as.character(as.numeric(Sys.time())))
  if (!dir.create(tmpPath)) {
    stop(sprintf("Could not create tmpdir: %s", tmpPath))
  }
  on.exit(unlink(tmpPath, recursive = TRUE), add = TRUE)
  zip::unzip(tempFiles, exdir = tmpPath, files = c("data.gdx", "metadata.json"))
  gdxrrwMIRO::igdx(file.path(
    .libPaths()[1], "gdxrrwMIRO",
    if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
      file.path("bin", "x64")
    } else {
      "bin"
    }
  ), silent = TRUE, returnStr = FALSE)
  symInGdx <- gdxrrwMIRO::gdxInfo(file.path(tmpPath, "data.gdx"), returnList = TRUE, dump = FALSE)
  symInGdx <- c(symInGdx$sets, symInGdx$parameters, symInGdx$variables, symInGdx$equations)
  isClArg <- startsWith(symNames, "_gmspar_") | startsWith(symNames, "_gmsopt_")
  expectedSymInGdx <- symNames[!isClArg]
  expectedClArgs <- symNames[isClArg]
  expect_identical(length(symInGdx), length(expectedSymInGdx))
  expect_true(all(expectedSymInGdx %in% symInGdx))

  clArgsInMiroscen <- vapply(
    jsonlite::read_json(file.path(tmpPath, "metadata.json"))$cl_args, "[[",
    character(1L), "scalar"
  )
  expect_identical(length(clArgsInMiroscen), length(expectedClArgs))
  expect_true(all(expectedClArgs %in% clArgsInMiroscen))
}

expect_sheets_in_xls <- function(app, id, sheetNames) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  tempFiles <- file.path(tempdir(check = TRUE), "shinytest-download")
  on.exit(unlink(tempFiles))
  writeBin(req$content, tempFiles)
  sheetsInXls <- readxl::excel_sheets(tempFiles)
  expect_identical(length(sheetsInXls), length(sheetNames))
  expect_true(all(sheetNames %in% sheetsInXls))
}

get_downloaded_file_content <- function(app, id, raw = FALSE) {
  i <- 1L
  repeat {
    url <- paste0(app$get_url(), app$get_js(paste0("$('#", id, "').attr('href')")))
    if (grepl("/session/", url, fixed = TRUE) || i > 5) {
      break
    }
    i <- i + 1L
    Sys.sleep(0.2)
  }
  req <- httr::GET(url)
  if (raw) {
    return(req$content)
  }
  return(rawToChar(req$content))
}

connectDb <- function(modelName = NULL, dbPath = Sys.getenv("MIRO_DB_PATH")) {
  if (identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) {
    conn <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = Sys.getenv("MIRO_DB_NAME"),
      host = Sys.getenv("MIRO_DB_HOST"),
      port = 5432,
      user = Sys.getenv("MIRO_DB_USERNAME"),
      password = Sys.getenv("MIRO_DB_PASSWORD"),
      bigint = "integer"
    )
    DBI::dbExecute(conn, paste0(
      "SET search_path TO ",
      Sys.getenv("MIRO_DB_SCHEMA"),
      ";"
    ))
    return(conn)
  }
  return(DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = file.path(dbPath, paste0(modelName, ".sqlite3")), bigint = "integer"
  ))
}

createTestDb <- function(dbPath = file.path(getwd(), "..", "testdb")) {
  if (identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) {
    # need to clean db tables
    conn <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = Sys.getenv("MIRO_DB_NAME"),
      host = Sys.getenv("MIRO_DB_HOST"),
      port = 5432,
      user = Sys.getenv("MIRO_DB_USERNAME"),
      password = Sys.getenv("MIRO_DB_PASSWORD"),
      bigint = "integer"
    )
    on.exit(DBI::dbDisconnect(conn))
    DBI::dbExecute(conn, DBI::SQL("SET client_min_messages TO WARNING;"))
    DBI::dbExecute(conn, DBI::SQL("DROP SCHEMA IF EXISTS mirotests CASCADE;"))
    DBI::dbExecute(conn, DBI::SQL("CREATE SCHEMA mirotests;"))
    DBI::dbExecute(conn, DBI::SQL(paste0(
      "GRANT ALL ON SCHEMA mirotests TO ",
      DBI::dbQuoteIdentifier(conn, Sys.getenv("MIRO_DB_USERNAME")), ";"
    )))
    Sys.setenv(MIRO_DB_SCHEMA = "mirotests")
  } else {
    if (file.exists(dbPath)) {
      if (unlink(dbPath, force = TRUE, recursive = TRUE)) {
        gc()
        if (unlink(dbPath, force = TRUE, recursive = TRUE)) {
          # try create new subfolder with random name
          dbPath <- file.path(dbPath, stringi::stri_rand_strings(1L, 20L)[[1L]])
        }
      }
      if (!dir.create(dbPath, recursive = TRUE)) {
        stop(sprintf("Could not create test database path: '%s'.", dbPath))
      }
    }
    Sys.setenv(MIRO_DB_PATH = dbPath)
  }
}

saveAdditionalGamsClArgs <- function(miroModelDir, modelToTest, additionalGamsClArgs) {
  if (!length(additionalGamsClArgs)) {
    return()
  }
  configJSONFileName <- file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
  file.copy(configJSONFileName,
    file.path(dirname(configJSONFileName), paste0(modelToTest, "_tmp.json")),
    overwrite = TRUE
  )
  configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  ))
  configJSON$extraClArgs <- c(configJSON$extraClArgs, additionalGamsClArgs)
  jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  return(invisible())
}
PerformanceReporter <- R6::R6Class("PerformanceReporter", public = list(
  initialize = function() {
    private$url <- Sys.getenv("MIRO_REPORTER_URL", unset = NA)
    if (!is.na(private$url)) {
      private$user <- Sys.getenv("MIRO_REPORTER_USER", unset = NA)
      private$pass <- Sys.getenv("MIRO_REPORTER_PASS", unset = NA)
    }
  },
  measure = function(id, expression) {
    expression_call <- substitute(expression)
    time <- Sys.time()
    eval(expression_call, parent.frame())
    private$data[[id]] <- as.numeric(difftime(Sys.time(), time, units = "secs"))
    return(invisible(self))
  },
  getData = function() {
    return(private$data)
  },
  publish = function() {
    context <- eval(substitute(testthat::get_reporter()$.context),
      envir = parent.frame()
    )
    dataToPublish <- list(
      context = context,
      data = private$data
    )
    private$data <- list()
    if (is.na(private$pass)) {
      warning("No reporter password set. Skipping publishing performance results.")
      print(dataToPublish)
      return(return(invisible(self)))
    }
    tryCatch(
      {
        req <- httr::POST(private$url,
          body = dataToPublish,
          httr::authenticate(private$user, private$pass),
          encode = "json",
          httr::timeout(10L)
        )
        if (httr::status_code(req) != 200L) {
          stop(sprintf("Bad status code: %s", httr::status_code(req)), call. = FALSE)
        }
      },
      error = function(e) {
        warning(sprintf(
          "Problems publishing performance results. Error message: %s",
          conditionMessage(e)
        ))
      }
    )
    return(return(invisible(self)))
  }
), private = list(
  data = list(),
  url = NA,
  user = NA,
  pass = NA
))
populateDb <- function(procEnv, modelName, modelPath = NULL) {
  procEnv$MIRO_POPULATE_DB <- "true"
  miroAppPath <- file.path(getwd(), "..", "..")
  if (is.null(modelPath)) {
    modelPath <- file.path(miroAppPath, "model", modelName)
  }
  procEnv$MIRO_MODEL_PATH <- file.path(modelPath, paste0(modelName, ".gms"))
  procEnv$MIRO_DATA_DIR <- file.path(
    modelPath, paste0("data_", modelName),
    "default.gdx"
  )
  procEnv$MIRO_OVERWRITE_SCEN_IMPORT <- "true"

  miroProc <- processx::run(file.path(R.home("bin"), "R"),
    c(
      "-e",
      paste0("shiny::runApp('", miroAppPath, "',port=3839,host='0.0.0.0')")
    ),
    env = unlist(procEnv), wd = miroAppPath, error_on_status = FALSE, timeout = 30
  )
  if (miroProc$status != 0L) {
    print(miroProc$stdout)
    print(miroProc$stderr)
  }
}
getEngineToken <- function(apiURL, username, password) {
  resp <- httr::POST(paste0(apiURL, "/auth/login"),
    body = list(username = username, password = password, expires_in = 3600), httr::timeout(10L)
  )
  stopifnot(identical(httr::status_code(resp), 200L))
  return(httr::content(resp)$token)
}
createUser <- function(apiURL, inviterUser, inviterPass, namespace,
                       username, password, roles = NULL, volumeQuota = NULL, diskQuota = NULL, deleteIfExists = TRUE) {
  requestBody <- list(namespace_permissions = paste0("7@", namespace))
  if (length(roles)) {
    requestBody$roles <- roles
  }
  if (length(volumeQuota)) {
    requestBody$volume_quota <- volumeQuota
  }
  if (length(diskQuota)) {
    requestBody$disk_quota <- diskQuota
  }
  resp <- httr::POST(paste0(apiURL, "/users/invitation"),
    body = requestBody, httr::timeout(10L),
    httr::authenticate(inviterUser, inviterPass)
  )
  stopifnot(identical(httr::status_code(resp), 201L))
  token <- httr::content(resp)$invitation_token
  resp <- httr::POST(paste0(apiURL, "/users/"), body = list(
    username = username,
    password = password,
    invitation_code = token
  ), httr::timeout(10L), httr::authenticate(inviterUser, inviterPass))
  if (deleteIfExists && identical(httr::status_code(resp), 400L)) {
    removeUser(apiURL, inviterUser, inviterPass, username)
    resp <- httr::POST(paste0(apiURL, "/users/"), body = list(
      username = username,
      password = password,
      invitation_code = token
    ), httr::timeout(10L))
  }
  stopifnot(identical(httr::status_code(resp), 201L))
}
removeUser <- function(apiURL, inviterUser, inviterPass, username) {
  resp <- httr::DELETE(
    paste0(
      apiURL, "/users/?username=", URLencode(username, reserved = TRUE),
      "&delete_results=true&delete_children=true"
    ), httr::timeout(10L),
    httr::authenticate(inviterUser, inviterPass)
  )
  stopifnot(identical(httr::status_code(resp), 200L))
}
