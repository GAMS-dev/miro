renderDataUI <- function(id, type, height = NULL, graphConfig = NULL,
                         createdDynamically = FALSE, showNoDataTxt = TRUE) {
  ns <- NS(id)
  # make output type case insensitive
  typeCustom <- type
  type <- tolower(type)
  if (!length(type)) {
    type <- "datatable"
  }

  renderconfig <- getRendererConfig(graphConfig)

  graphOptions <- renderconfig$graphOptions
  rendererOptions <- renderconfig$rendererOptions
  graphTool <- renderconfig$graphTool

  if (identical(type, "datatable")) {
    data <- DTOutput(ns("datatable"))
  } else if (type %in% c("graph", "dtgraph")) {
    if (identical(graphTool, "plotly")) {
      if (identical(type, "graph")) {
        dataGraph <- miroPlotlyOutput(ns("miroPlotly"), height = "100%", options = graphOptions)
      } else {
        dataGraph <- tags$div(
          class = "renderer-wrapper",
          genSpinner(externalStyle = character(0L)),
          miroPlotlyOutput(ns("miroPlotly"), height = "100%", options = graphOptions)
        )
      }
    } else if (identical(graphTool, "dygraphs")) {
      dataGraph <- miroDygraphsOutput(ns("miroDygraphs"), height = "70vh", options = graphOptions)
    } else if (identical(graphTool, "leaflet")) {
      dataGraph <- miroLeafletOutput(ns("miroLeaflet"), height = height, options = graphOptions)
    } else if (identical(graphTool, "timevis")) {
      dataGraph <- miroTimevisOutput(ns("miroTimevis"), height = "70vh", options = graphOptions)
    } else {
      stop(paste0("The tool you selected for: '", id, "' is not supported by the current version of GAMS MIRO."))
    }

    if (!graphTool %in% c("leaflet", "plotly", "timevis", "dygraphs")) {
      filterOptions <- graphOptions$filter
      if (length(filterOptions$col)) {
        data <- tags$div(
          class = "data-filter-wrapper",
          if (isTRUE(filterOptions$date)) {
            dateRangeInput(
              ns("data_filter"),
              filterOptions$label
            )
          } else {
            selectInput(ns("data_filter"),
              filterOptions$label,
              choices = c(), multiple = isTRUE(filterOptions$multiple)
            )
          },
          dataGraph
        )
      } else {
        data <- dataGraph
      }
    } else {
      data <- dataGraph
    }

    if (identical(type, "dtgraph")) {
      data <- tagList(
        tags$div(
          class = "dtgraph-wrapper",
          tags$div(class = "col-md-6 col-md-push-6 dtgraph-graph", data, style = "overflow-x:auto;"),
          tags$div(class = "col-md-6 col-md-pull-6", DTOutput(ns("datatable")), style = "overflow-x:auto;")
        )
      )
    }
  } else if (identical(type, "valuebox")) {
    data <- uiOutput(ns("scalarBoxes"))
  } else if (identical(type, "miropivot")) {
    data <- miroPivotOutput(ns("miroPivot"), height = height, options = rendererOptions)
  } else if (identical(type, "dashboard")) {
    data <- dashboardOutput(ns("dashboard"), height = height, options = rendererOptions)
  } else {
    tryCatch(
      {
        customOutput <- match.fun(typeCustom %+% "Output")
      },
      error = function(e) {
        stop(sprintf("An output function for the custom renderer: '%s' was not found.
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
      }
    )
    data <- customOutput(ns("custom"),
      height = height, options = graphConfig$options,
      path = customRendererDir
    )
  }
  return(tagList(
    if (showNoDataTxt) {
      tags$div(
        id = ns("noData"), class = "out-no-data",
        if (!createdDynamically) lang$nav$outputScreen$boxResults$noData
      )
    },
    tags$div(id = ns("data"), style = if (createdDynamically) "" else "display:none", data)
  ))
}

renderData <- function(input, output, session, data, type, graphConfig = NULL,
                       configData = NULL, dtOptions = NULL,
                       roundPrecision = 2, rendererEnv = NULL,
                       views = NULL, attachments = NULL) {
  renderconfig <- getRendererConfig(graphConfig)

  graphOptions <- renderconfig$graphOptions
  rendererOptions <- renderconfig$rendererOptions
  graphTool <- renderconfig$graphTool

  if (!length(type)) {
    type <- "datatable"
  }
  if (inherits(data, "data.frame")) {
    if (!length(data) || identical(nrow(data), 0L)) {
      showEl(session, "#" %+% session$ns("noData"))
      hideEl(session, "#" %+% session$ns("data"))
      return()
    } else {
      showEl(session, "#" %+% session$ns("data"))
      hideEl(session, "#" %+% session$ns("noData"))
    }
  } else {
    if (!length(data) || !length(data[[1]]) || identical(nrow(data[[1]]), 0L)) {
      showEl(session, "#" %+% session$ns("noData"))
      hideEl(session, "#" %+% session$ns("data"))
      return()
    } else {
      showEl(session, "#" %+% session$ns("data"))
      hideEl(session, "#" %+% session$ns("noData"))
    }
  }

  # make output type case insensitive
  typeCustom <- type
  type <- tolower(type)
  if (type %in% c("graph", "dtgraph")) {
    filterCol <- NULL
    if (!graphTool %in% c("leaflet", "plotly")) {
      if (length(graphOptions$filter) && graphOptions$filter$col %in% names(data)) {
        showEl(session, "#" %+% session$ns("data_filter_wrapper"))
        filterCol <- as.name(graphOptions$filter$col)
        if (isTRUE(graphOptions$filter$date)) {
          choices <- data[[graphOptions$filter$col]]
          updateDateRangeInput(session, "data_filter",
            min = choices[1],
            max = choices[length(choices)],
            start = choices[1], end = choices[length(choices)]
          )
        } else {
          choices <- data[[graphOptions$filter$col]]
          updateSelectInput(session, "data_filter",
            choices = choices,
            selected = choices[1]
          )
        }
      }
    }
    if (identical(graphTool, "leaflet")) {
      renderMiroLeaflet("miroLeaflet", data,
        options = graphOptions,
        roundPrecision = roundPrecision,
        rendererEnv = rendererEnv,
        views = views,
        outputScalarsFull = configData
      )
    } else if (identical(graphTool, "plotly")) {
      renderMiroPlotly("miroPlotly", data,
        options = graphOptions,
        roundPrecision = roundPrecision,
        rendererEnv = rendererEnv,
        views = views,
        outputScalarsFull = configData
      )
    } else if (identical(graphTool, "timevis")) {
      renderMiroTimevis("miroTimevis", data,
        options = graphOptions,
        roundPrecision = roundPrecision,
        rendererEnv = rendererEnv,
        views = views,
        outputScalarsFull = configData
      )
    } else if (identical(graphTool, "dygraphs")) {
      renderMiroDygraphs("miroDygraphs", data,
        options = graphOptions,
        roundPrecision = roundPrecision,
        rendererEnv = rendererEnv,
        views = views,
        outputScalarsFull = configData
      )
    } else {
      stop(
        sprintf("The graph tool '%s' is not currently supported.", graphTool),
        call. = FALSE
      )
    }

    if (type == "dtgraph") {
      output$datatable <- renderDTable(data,
        options = dtOptions, roundPrecision = roundPrecision,
        metadata = if (length(rendererOptions)) rendererOptions[["_metadata_"]]
      )
    }
  } else if (type == "datatable") {
    output$datatable <- renderDTable(data,
      options = if (!is.null(dtOptions)) dtOptions else graphOptions,
      roundPrecision = roundPrecision,
      metadata = if (length(rendererOptions)) rendererOptions[["_metadata_"]]
    )
  } else if (type == "valuebox") {
    force(rendererOptions)
    force(roundPrecision)
    output$scalarBoxes <- renderUI({
      if (!length(rendererOptions) || !length(names(rendererOptions[[1]]))) {
        boxWidth <- if (length(rendererOptions$width)) rendererOptions$width else 4L
        noBoxesRow <- 12 / boxWidth
        numberRows <- ceiling(boxWidth * length(data[[1]]) / 12)
        oldConfig <- TRUE
      } else {
        oldConfig <- FALSE
        if (length(names(rendererOptions))) {
          rendererOptions <- rendererOptions[!names(rendererOptions) %in% c("_metadata_", "count")]
        }
        configuredScalars <- unlist(lapply(rendererOptions, names), use.names = FALSE)
        unconfiguredScalars <- !tolower(data[[1]]) %in% tolower(configuredScalars)
        if (any(unconfiguredScalars)) {
          unconfiguredScalars <- data[[1]][unconfiguredScalars]
          additionalOptions <- lapply(seq_len(ceiling(length(unconfiguredScalars) / 3L)) - 1L, function(rowId) {
            scalarNames <- unconfiguredScalars[seq(rowId * 3L + 1L, min(
              length(unconfiguredScalars),
              rowId * 3L + 3L
            ))]
            return(setNames(
              vector("list", length(scalarNames)),
              scalarNames
            ))
          })
          rendererOptions <- c(rendererOptions, additionalOptions)
        }
        numberRows <- length(rendererOptions)
      }
      lapply(seq_len(numberRows), function(rowId) {
        if (oldConfig) {
          rowConfig <- vector("list", noBoxesRow)
        } else {
          rowConfig <- rendererOptions[[rowId]]
          boxWidth <- 12 / length(rowConfig)
        }
        tags$div(
          class = "container-fluid",
          fluidRow(lapply(seq_along(rowConfig), function(scalarId) {
            if (oldConfig) {
              scalarId <- scalarId + noBoxesRow * (rowId - 1L)
              if (scalarId > length(data[[1]])) {
                return()
              }
              scalarConfig <- list(
                icon = rendererOptions$icon,
                color = rendererOptions$color
              )
            } else {
              scalarConfig <- rowConfig[[scalarId]]
              if (is.na(names(rowConfig)[scalarId])) {
                return()
              }
              scalarId <- match(names(rowConfig)[scalarId], data[[1]])
              if (is.na(scalarId)) {
                flog.warn(
                  "Value box was configured for nonexistent scalar: %s",
                  names(rowConfig)[scalarId]
                )
                return()
              }
            }
            valueBox(
              if (!is.na(suppressWarnings(as.numeric(data[[3]][scalarId])))) {
                round(as.numeric(data[[3]][scalarId]),
                  digits = if (length(scalarConfig$round)) {
                    scalarConfig$round
                  } else {
                    roundPrecision
                  }
                )
              } else {
                data[[3]][scalarId]
              },
              subtitle = if (length(scalarConfig$description)) scalarConfig$description else data[[2]][scalarId],
              width = boxWidth,
              # object
              icon = if (length(scalarConfig$icon)) icon(scalarConfig$icon$name, lib = scalarConfig$icon$lib),
              color = if (length(scalarConfig$color)) scalarConfig$color else "aqua"
            )
          }))
        )
      })
    })
  } else if (type == "miropivot") {
    renderMiroPivot("miroPivot", data,
      options = rendererOptions,
      roundPrecision = roundPrecision,
      rendererEnv = rendererEnv, views = views
    )
  } else if (type == "dashboard") {
    renderDashboard("dashboard", data,
      options = rendererOptions,
      rendererEnv = rendererEnv, views = views,
      outputScalarsFull = configData,
      roundPrecision = roundPrecision
    )
  } else {
    tryCatch(
      {
        customRenderer <- match.fun(paste0(
          "render", toupper(substr(typeCustom, 1, 1)),
          substr(typeCustom, 2, nchar(typeCustom))
        ))
      },
      error = function(e) {
        stop(sprintf("A custom renderer function: '%s' was not found.
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
      }
    )
    tryCatch(
      {
        callModule(customRenderer, "custom", data,
          options = rendererOptions,
          path = customRendererDir, rendererEnv = rendererEnv, views = views,
          attachments = attachments, outputScalarsFull = configData,
          roundPrecision = roundPrecision
        )
      },
      error = function(e) {
        stop(sprintf(
          "An error occured in the custom renderer function: '%s'. Error message: %s.", typeCustom,
          conditionMessage(e)
        ), call. = FALSE)
      }
    )
  }
}
