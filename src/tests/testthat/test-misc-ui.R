test_that(
  "Miscellaneous works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    testModelPath <- file.path(testDir, "model", "transport")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "transport.gms"
    ))
    Sys.setenv(MIRO_MODE = "base")

    modelDataPath <- file.path(testModelPath, "data_transport")
    modelConfPath <- file.path(testModelPath, "conf_transport")

    file.copy2(
      file.path(testDir, "data", "transport.gdx"),
      file.path(modelDataPath, "default1.gdx")
    )
    file.copy2(
      file.path(testDir, "data", "transport2.gdx"),
      file.path(modelDataPath, "default2.gdx")
    )
    source(file.path(testDir, "shinytest", "load_input_graph_test.R"), local = TRUE)
    unlink(modelDataPath, recursive = TRUE, force = TRUE)


    # test that metadata dialog opens when having no scalars in
    # data contract but double dash parameters defined (#398)
    file.move(
      file.path(modelConfPath, "transport_io.json"),
      file.path(modelConfPath, "transport_io_tmp.json")
    )
    file.move(
      file.path(modelConfPath, "transport.json"),
      file.path(modelConfPath, "transport_tmp.json")
    )
    jsonlite::write_json(
      list(
        modelTitle = "trnsport",
        inputSymbols = list(a = list(
          alias = "capacity of plant i in cases",
          symtype = "parameter", headers = list(i = list(
            type = "string",
            alias = "canning plants"
          ), value = list(
            type = "numeric",
            alias = "capacity of plant i in cases"
          ))
        )), outputSymbols = list(
          schedule = list(
            alias = "shipment quantities in cases",
            symtype = "parameter", headers = list(i = list(
              type = "string",
              alias = "canning plants"
            ), j = list(
              type = "string",
              alias = "markets"
            ), lngp = list(
              type = "numeric",
              alias = "lngp"
            ), latp = list(
              type = "numeric",
              alias = "latp"
            ), lngm = list(
              type = "numeric",
              alias = "lngm"
            ), latm = list(
              type = "numeric",
              alias = "latm"
            ), cap = list(
              type = "numeric",
              alias = "cap"
            ), demand = list(
              type = "numeric",
              alias = "demand"
            ), quantities = list(
              type = "numeric",
              alias = "quantities"
            ))
          )
        )
      ),
      file.path(modelConfPath, "transport_io.json"),
      auto_unbox = TRUE
    )
    jsonlite::write_json(
      list(inputWidgets = list(`_gmspar_type` = list(
        widgetType = "dropdown", alias = "model type",
        selected = "lp", noHcube = FALSE, multiple = FALSE, label = "Select the model type",
        choices = c("lp", "mip", "minlp"), aliases = c("LP", "MIP", "MINLP")
      ))),
      file.path(modelConfPath, "transport.json"),
      auto_unbox = TRUE
    )

    createTestDb()
    source(file.path(testDir, "shinytest", "metadata_no_scalars_test.R"), local = TRUE)

    file.move(
      file.path(modelConfPath, "transport_tmp.json"),
      file.path(modelConfPath, "transport.json")
    )
    file.move(
      file.path(modelConfPath, "transport_io_tmp.json"),
      file.path(modelConfPath, "transport_io.json")
    )

    testModelPath <- file.path(testDir, "model", "transport_numericHeaders")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "transport_numericHeaders.gms"
    ))
    skip_if(
      identical(Sys.getenv("GAMS_SYS_DIR"), ""),
      "GAMS_SYS_DIR environment variable not set. Skipping tests."
    )
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      saveAdditionalGamsClArgs(
        testModelPath, "transport_numericheaders",
        paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
      )
    }
    createTestDb()
    source(file.path(testDir, "shinytest", "integer_headers_test.R"), local = TRUE)
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      file.rename(
        file.path(testModelPath, "conf_transport_numericheaders", "transport_numericheaders_tmp.json"),
        file.path(testModelPath, "conf_transport_numericheaders", "transport_numericheaders.json")
      )
    }

    testModelPath <- file.path(testDir, "model", "transport_latin1")
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      saveAdditionalGamsClArgs(
        testModelPath, "transport",
        paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
      )
    }
    createTestDb()
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "Transport.gms"
    ))

    source(file.path(testDir, "shinytest", "lst_file_bad_encoding_test.R"), local = TRUE)

    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      file.rename(
        file.path(testModelPath, "conf_transport", "transport_tmp.json"),
        file.path(testModelPath, "conf_transport", "transport.json")
      )
    }

    testModelPath <- file.path(testDir, "model", "pickstock_gamspy")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "pickstock.py"
    ))
    skip_if(
      identical(Sys.getenv("PYTHON_EXEC_PATH", NA_character_), NA_character_),
      "No GAMSPy installation found"
    )
    createTestDb()

    source(file.path(testDir, "shinytest", "extra_cl_args_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)
