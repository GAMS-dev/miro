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

createTestDb <- function(dbPath = file.path(getwd(), "..", "miro.sqlite3")){
  if(file.exists(dbPath)){
    if(unlink(dbPath, force = TRUE)){
      gc()
      if(unlink(dbPath, force = TRUE)){
        stop("Could not remove old database SQLite file for tests")
      }
    }
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
    if(is.na(private$pass)){
      warning("No reporter password set. Skipping publishing performance results.")
      private$data <- list()
      return(return(invisible(self)))
    }
    dataToPublish <- list(context = context,
                          data = private$data)
    private$data <- list()
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
