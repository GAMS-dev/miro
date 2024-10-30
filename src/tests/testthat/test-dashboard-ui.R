test_that(
  "Dashboard renderer works",
  {
    createTestDb()

    modelDir <- file.path(
      getwd(), "..", "model", "pickstock_dashboard"
    )

    Sys.setenv(MIRO_MODEL_PATH = file.path(
      modelDir,
      "pickstock.gms"
    ))
    Sys.setenv(MIRO_MODE = "base")
    Sys.setenv(DASHBOARD_RENDERER_NAME = "dashboard")

    # source(file.path(testDir, "shinytest", "dashboard_test.R"), local = TRUE)

    # using dashboard as custom renderer also works
    configJSONFilePath <- file.path(modelDir, "conf_pickstock", "pickstock.json")

    configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFilePath,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))

    file.move(
      configJSONFilePath,
      file.path(dirname(configJSONFilePath), "bk_pickstock.json")
    )

    configJSON$dataRendering$stock_weight$outType <- "mirorenderer_stock_weight"

    jsonlite::write_json(configJSON, configJSONFilePath,
      pretty = TRUE, auto_unbox = TRUE, null = "null"
    )

    # copy and modify dashboard renderer file
    dashboardRenderer <- readLines(file.path(
      getwd(), "..", "..",
      "modules", "renderers", "dashboard.R"
    ))
    dashboardRenderer <- gsub(
      pattern = "^dashboardOutput <- function\\(",
      replacement = "mirorenderer_stock_weightOutput <- function(",
      x = dashboardRenderer
    )
    dashboardRenderer <- gsub(
      pattern = "^renderDashboard <- function\\(id, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, \\.\\.\\.\\)",
      replacement = "renderMirorenderer_stock_weight <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, attachments = NULL, outputScalarsFull = NULL, ...)",
      x = dashboardRenderer
    )
    dashboardRenderer <- paste(dashboardRenderer, collapse = "\n")
    dashboardRenderer <- gsub(
      pattern = "moduleServer\\(\\s*id,\\s*function\\(input, output, session\\) \\{",
      replacement = "",
      x = dashboardRenderer
    )
    dashboardRenderer <- strsplit(dashboardRenderer, "\n")[[1]]
    closingIndices <- grep("\\}", dashboardRenderer)
    dashboardRenderer <- dashboardRenderer[-seq(
      tail(closingIndices, 2)[1],
      tail(closingIndices, 2)[2] - 1L
    )]
    dir.create(file.path(modelDir, "renderer_pickstock"), showWarnings = FALSE)
    writeLines(dashboardRenderer, file.path(modelDir, "renderer_pickstock", "mirorenderer_stock_weight.R"))

    Sys.setenv(DASHBOARD_RENDERER_NAME = "custom")
    # source(file.path(testDir, "shinytest", "dashboard_test.R"), local = TRUE)

    unlink(file.path(modelDir, "renderer_pickstock", "mirorenderer_stock_weight.R"))
    file.move(
      file.path(dirname(configJSONFilePath), "bk_pickstock.json"),
      configJSONFilePath
    )

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "DASHBOARD_RENDERER_NAME"))
  }
)
