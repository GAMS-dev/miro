test_that(
  "Symbol order works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()
    modelToTest <- "pickstock"
    testModelDir <- file.path(testDir, "model", modelToTest)
    modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
    configJSONFileName <- file.path(
      testModelDir, paste0("conf_", modelToTest),
      paste0(modelToTest, ".json")
    )
    ioJSONFileName <- file.path(
      testModelDir, paste0("conf_", modelToTest),
      paste0(modelToTest, "_io.json")
    )

    Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
    Sys.setenv(MIRO_MODE = "base")

    # add scalar symbol to _io.json (test model currently uses --parameter) for order test
    file.copy(ioJSONFileName, file.path(
      dirname(ioJSONFileName),
      paste0(tolower(modelToTest), "_io_tmp.json")
    ), overwrite = TRUE)
    ioJSON <- suppressWarnings(jsonlite::fromJSON(ioJSONFileName,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))
    ioJSON$inputSymbols$`_scalars`$symnames <- c("maxstock", "trainingdays", "solver")
    ioJSON$inputSymbols$`_scalars`$symtext <- c("maximum number of stocks to select", "number of days for training", "solver")
    ioJSON$inputSymbols$`_scalars`$symtypes <- c("parameter", "parameter", "set")
    jsonlite::write_json(ioJSON, ioJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")


    # use --widget for new 'solver' symbol, set input/output symbol order
    file.copy(configJSONFileName, file.path(
      dirname(configJSONFileName),
      paste0(tolower(modelToTest), "_tmp.json")
    ), overwrite = TRUE)
    configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))

    configJSON$inputWidgets$trainingdays <- NULL
    configJSON$inputWidgets$solver <- configJSON$inputWidgets$`_gmsopt_mip`
    configJSON$inputWidgets$`_gmsopt_mip` <- NULL
    configJSON$inputWidgetGroups[[1]]$name <- "Advanced options"
    configJSON$inputWidgetGroups[[1]]$members <- "solver"
    configJSON$outputGroups[[1]]$name <- "Dow Jones vs. Index Fund"
    configJSON$outputGroups[[1]]$members <- c("dowvsindex", "abserror")
    configJSON$overwriteSheetOrder$input <- c("_scalars", "_widgets1", "price", "_widgets")
    configJSON$overwriteSheetOrder$output <- c("_scalars_out", "dowvsindex", "stock_weight", "pricemerge", "abserror")

    jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
    file.copy2(file.path(testDir, "data", "pickstock.gdx"), file.path(modelDataPath, "default.gdx"))

    source(file.path(testDir, "shinytest", "symbol_order_test.R"), local = TRUE)

    file.rename(
      file.path(dirname(ioJSONFileName), paste0(tolower(modelToTest), "_io_tmp.json")),
      file.path(dirname(ioJSONFileName), paste0(tolower(modelToTest), "_io.json"))
    )
    file.rename(
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json"))
    )

    unlink(modelDataPath, recursive = TRUE, force = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
