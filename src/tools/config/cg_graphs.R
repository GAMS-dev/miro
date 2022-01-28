activeSymbol <- list(
  id = integer(1L), name = character(1L),
  alias = character(1L), indices = c(), isInput = FALSE
)
activeSymbolName <- character(0L)
customRendererEvalEnv <- new.env(parent = parent.frame())
# Redefine observe function so that custom renderers don't crash when error
# occurs in observer (see also: https://github.com/rstudio/shiny/issues/2904#issuecomment-634776972)
customRendererEvalEnv$observe <- function(x, ...) {
  x <- substitute(x)
  env <- parent.frame()
  shiny::observe(
    {
      tryCatch(
        eval(x, env),
        error = function(e) {
          showElReplaceTxt(
            session, "#preview-error",
            sprintf(lang$adminMode$graphs$ui$previewErrorObs, conditionMessage(e))
          )
        }
      )
    },
    ...
  )
}
customRendererEnv <- new.env(parent = emptyenv())
skipLoadRenderer <- TRUE
invalidCustomRender <- FALSE
rendererFromExternalSymbol <- FALSE

existingRendererFiles <- tools::file_path_sans_ext(list.files(
  path = customRendererDir,
  pattern = "\\.R$"
))

newChartTool <- character(0L)
isInJSON <- FALSE
configuredWithThisTool <- FALSE
tableSymbol <- FALSE
plotlyChartTools <- c("pie", "bar", "scatter", "line", "bubble", "hist")
noTitle <- c("leaflet", "timevis", "miropivot", "valuebox")
modelInputData <- vector("list", length(modelIn))
modelOutputData <- vector("list", length(modelOut))
configScalars <- tibble()
hotInit <- vector("logical", length(modelIn))
widgetModifiedSkipCount <- vector("integer", length = length(modelIn))
isEmptyInput <- vector(mode = "logical", length = length(modelIn))
isEmptyOutput <- vector(mode = "logical", length = length(modelOut))
isEmptyOutput[] <- TRUE
inputInitialized <- vector(mode = "logical", length = length(modelInWithDep))
currentSelection <- "pie"
idLabelMap <- list(
  chart_ydata = list(), animation_options = list(), hist_xdata = list(), dy_dyEvent = list(),
  dy_dyLimit = list(), leaflet_markers = list(), leaflet_flows = list(), leaflet_minicharts = list(),
  timevis_series = list(), timevis_custom = list()
)
currentConfig <- list()
optionsInserted <- c()
noOutputData <- TRUE
allDataAvailable <- FALSE
leafletGroups <- CharArray$new()
axisOptionsGlobal <- list(y = 1, y2 = 0)
noDygraphHighlight <- list(
  hideOnMouseOut = TRUE,
  highlightCircleSize = 0L,
  highlightSeriesBackgroundAlpha = 1L,
  highlightSeriesOpts = list(
    strokeWidth = 0L
  )
)

langSpecificGraphs <- list()
langSpecificGraphs$barmode <- c("group" = "group", "stack" = "stack")
names(langSpecificGraphs$barmode) <- lang$adminMode$graphs$barOptions$choicesMode
langSpecificGraphs$barOrientation <- c("vertical" = "v", "horizontal" = "h")
names(langSpecificGraphs$barOrientation) <- lang$adminMode$graphs$barOptions$choicesOrientation
langSpecificGraphs$valueboxColor <- c(
  "red" = "red", "yellow" = "yellow", "aqua" = "aqua", "blue" = "blue",
  "light-blue" = "light-blue", "green" = "green", "navy" = "navy", "teal" = "teal",
  "olive" = "olive", "lime" = "lime", "orange" = "orange", "fuchsia" = "fuchsia",
  "purple" = "purple", "maroon" = "maroon", "black" = "black"
)
names(langSpecificGraphs$valueboxColor) <- lang$adminMode$graphs$valueboxOptions$colorChoices
langSpecificGraphs$valueboxIconChoices <- c(
  "_" = "_", "Coins" = "coins", "Wallet" = "wallet", "Dollar" = "dollar-sign",
  "Euro" = "euro-sign", "Yen" = "yen-sign", "Won" = "won-sign",
  "Shekel" = "shekel-sign", "Rupee" = "rupee-sign",
  "Ruble" = "ruble-sign", "Pound" = "pound-sign", "Lira" = "lira-sign",
  "User" = "user-circle", "Users" = "users", "Plus" = "plus-circle",
  "Minus" = "minus-circle", "Exclamation mark" = "exclamation-circle",
  "Question mark" = "question-circle", "Play" = "play-circle",
  "Check" = "check-circle", "Home" = "home", "Cog" = "cog", "Asterisk" = "asterisk",
  "Ban" = "ban", "Heart" = "heart", "Leaf" = "leaf", "Lightbulb" = "lightbulb",
  "Smile" = "smile", "Star" = "star"
)
names(langSpecificGraphs$valueboxIconChoices) <- lang$adminMode$graphs$valueboxOptions$valueboxIconChoices
langSpecificGraphs$easingChoices <- c(
  "linear" = "linear", "quad" = "quad", "cubic" = "cubic", "sin" = "sin",
  "exp" = "exp", "circle" = "circle", "elastic" = "elastic", "back" = "back", "bounce" = "bounce",
  "linear-in" = "linear-in", "quad-in" = "quad-in", "cubic-in" = "cubic-in",
  "sin-in" = "sin-in", "exp-in" = "exp-in", "circle-in" = "circle-in",
  "elastic-in" = "elastic-in", "back-in" = "back-in", "bounce-in" = "bounce-in",
  "linear-out" = "linear-out", "quad-out" = "quad-out", "cubic-out" = "cubic-out",
  "sin-out" = "sin-out", "exp-out" = "exp-out", "circle-out" = "circle-out",
  "elastic-out" = "elastic-out", "back-out" = "back-out", "bounce-out" = "bounce-out",
  "linear-in-out" = "linear-in-out", "quad-in-out" = "quad-in-out",
  "cubic-in-out" = "cubic-in-out", "sin-in-out" = "sin-in-out", "exp-in-out" = "exp-in-out",
  "circle-in-out" = "circle-in-out", "elastic-in-out" = "elastic-in-out",
  "back-in-out" = "back-in-out", "bounce-in-out" = "bounce-in-out"
)
names(langSpecificGraphs$easingChoices) <- lang$adminMode$graphs$animationOptions$easingChoices
langSpecificGraphs$modeChoices <- c("immediate" = "immediate", "next" = "next", "afterall" = "afterall")
names(langSpecificGraphs$modeChoices) <- lang$adminMode$graphs$animationOptions$modeChoices
langSpecificGraphs$normChoices <- c(
  "Number of occurances" = " ", "Percentage of occurances" = "percent",
  "Number of occurances/ Bin interval size" = "density"
)
names(langSpecificGraphs$normChoices) <- lang$adminMode$graphs$histOptions$normChoices
langSpecificGraphs$barmodeChoices <- c(
  "overlay" = "overlay", "stack" = "stack", "group" = "group",
  "relative" = "relative"
)
names(langSpecificGraphs$barmodeChoices) <- lang$adminMode$graphs$histOptions$barmodeChoices
langSpecificGraphs$orientationChoices <- c("vertical" = "vertical", "horizontal" = "horizontal")
names(langSpecificGraphs$orientationChoices) <- lang$adminMode$graphs$histOptions$orientationChoices
langSpecificGraphs$pointShapeChoices <- c(
  "dot" = "dot", "triangle" = "triangle", "square" = "square",
  "diamond" = "diamond", "pentagon" = "pentagon", "hexagon" = "hexagon",
  "circle" = "circle", "star" = "star", "plus" = "plus", "ex" = "ex"
)
names(langSpecificGraphs$pointShapeChoices) <- lang$adminMode$graphs$dygraphsOptions$generalOptions$pointShapeChoices
langSpecificGraphs$positionChoices <- c(
  "Top right" = "topright", "Bottom right" = "bottomright",
  "Bottom left" = "bottomleft", "Top left" = "topleft"
)
names(langSpecificGraphs$positionChoices) <- lang$adminMode$graphs$leafletOptions$positionChoices
langSpecificGraphs$aggregatorChoices <- c(
  "Count" = "Count", "Count Unique Values" = "Count Unique Values",
  "List Unique Values" = "List Unique Values", "Sum" = "Sum",
  "Integer Sum" = "Integer Sum", "Average" = "Average", "Median" = "Median",
  "Sample Variance" = "Sample Variance", "Sample Standard Deviation" = "Sample Standard Deviation",
  "Minimum" = "Minimum", "Maximum" = "Maximum", "First" = "First", "Last" = "Last",
  "Sum over Sum" = "Sum over Sum", "80% Upper Bound" = "80% Upper Bound",
  "80% Lower Bound" = "80% Lower Bound", "Sum as Fraction of Total" = "Sum as Fraction of Total",
  "Sum as Fraction of Rows" = "Sum as Fraction of Rows",
  "Sum as Fraction of Columns" = "Sum as Fraction of Columns",
  "Count as Fraction of Total" = "Count as Fraction of Total",
  "Count as Fraction of Rows" = "Count as Fraction of Rows",
  "Count as Fraction of Columns" = "Count as Fraction of Columns"
)
names(langSpecificGraphs$aggregatorChoices) <- lang$adminMode$graphs$pivotOptions$aggregatorChoices
langSpecificGraphs$rendererChoices <- c(
  "Table" = "Table", "Table Barchart" = "Table Barchart", "Heatmap" = "Heatmap",
  "Row Heatmap" = "Row Heatmap", "Col Heatmap" = "Col Heatmap", "Treemap" = "Treemap",
  "Horizontal Bar Chart" = "Horizontal Bar Chart",
  "Horizontal Stacked Bar Chart" = "Horizontal Stacked Bar Chart", "Bar Chart" = "Bar Chart",
  "Stacked Bar Chart" = "Stacked Bar Chart", "Line Chart" = "Line Chart",
  "Area Chart" = "Area Chart", "Scatter Chart" = "Scatter Chart"
)
names(langSpecificGraphs$rendererChoices) <- lang$adminMode$graphs$pivotOptions$rendererChoices
langSpecificGraphs$localeChoices <- c(
  "cs" = "cs", "da" = "da", "de" = "de", "en" = "en", "es" = "es", "fr" = "fr",
  "it" = "it", "nl" = "nl", "pl" = "pl", "pt" = "pt", "ru" = "ru", "sq" = "sq",
  "tr" = "tr", "zh" = "zh"
)
names(langSpecificGraphs$localeChoices) <- lang$adminMode$graphs$pivotOptions$options$localeChoices
langSpecificGraphs$categoryorderChoices <- c(
  "trace" = "trace", "category ascending" = "category ascending", "category descending" = "category descending",
  "total ascending" = "total ascending", "total descending" = "total descending",
  "min ascending" = "min ascending", "min descending" = "min descending", "max ascending" = "max ascending",
  "max descending" = "max descending", "sum ascending" = "sum ascending", "sum descending" = "sum descending",
  "mean ascending" = "mean ascending", "mean descending" = "mean descending", "median ascending" = "median ascending",
  "median descending" = "median descending"
)
names(langSpecificGraphs$categoryorderChoices) <- lang$adminMode$graphs$axisOptions$categoryorderChoices
langSpecificGraphs$dyLegendOptions <- c("auto" = "auto", "always" = "always", "onmouseover" = "onmouseover", "follow" = "follow")
names(langSpecificGraphs$dyLegendOptions) <- lang$adminMode$graphs$dygraphsOptions$legend$showChoices
langSpecificGraphs$graphOptions <- setNames(
  c("pie", "bar", "scatter", "line", "bubble", "hist", "dygraphs", "leaflet", "timevis", "miropivot", "valuebox", "custom"),
  c(
    lang$adminMode$graphs$graphOptions$pie,
    lang$adminMode$graphs$graphOptions$bar,
    lang$adminMode$graphs$graphOptions$scatter,
    lang$adminMode$graphs$graphOptions$line,
    lang$adminMode$graphs$graphOptions$bubble,
    lang$adminMode$graphs$graphOptions$hist,
    lang$adminMode$graphs$graphOptions$dygraphs,
    lang$adminMode$graphs$graphOptions$leaflet,
    lang$adminMode$graphs$graphOptions$timevis,
    lang$adminMode$graphs$graphOptions$miropivot,
    lang$adminMode$graphs$graphOptions$valuebox,
    lang$adminMode$graphs$graphOptions$custom
  )
)
langSpecificGraphs$graphOptionsNoScalars <- setNames(
  c("pie", "bar", "scatter", "line", "bubble", "hist", "dygraphs", "leaflet", "timevis", "miropivot", "custom"),
  c(
    lang$adminMode$graphs$graphOptions$pie,
    lang$adminMode$graphs$graphOptions$bar,
    lang$adminMode$graphs$graphOptions$scatter,
    lang$adminMode$graphs$graphOptions$line,
    lang$adminMode$graphs$graphOptions$bubble,
    lang$adminMode$graphs$graphOptions$hist,
    lang$adminMode$graphs$graphOptions$dygraphs,
    lang$adminMode$graphs$graphOptions$leaflet,
    lang$adminMode$graphs$graphOptions$timevis,
    lang$adminMode$graphs$graphOptions$miropivot,
    lang$adminMode$graphs$graphOptions$custom
  )
)
langSpecificGraphs$graphOptionsSet <- setNames(
  c("timevis", "miropivot", "custom", "leaflet"),
  c(
    lang$adminMode$graphs$graphOptions$timevis,
    lang$adminMode$graphs$graphOptions$miropivot,
    lang$adminMode$graphs$graphOptions$custom,
    lang$adminMode$graphs$graphOptions$leaflet
  )
)

setViews <- function(viewData = list(), sidToLoad = NULL) {
  globalViewsFilePath <- file.path(dirname(configJSONFileName), "views.json")
  if (!file.exists(globalViewsFilePath)) {
    return()
  }
  viewsTmp <- tryCatch(
    {
      fromJSON(globalViewsFilePath, simplifyDataFrame = FALSE, simplifyVector = FALSE)
    },
    error = function(e) {
      flog.error("Could not read views file: %s. Error message: %s.", globalViewsFilePath, conditionMessage(e))
      showErrorMsg(
        lang$errMsg$fileRead$title,
        sprintf(lang$errMsg$fileRead$desc, globalViewsFilePath)
      )
      return(list())
    }
  )
  views$.__enclos_env__$private$scenViewConf[["1"]] <- viewsTmp
}
saveGlobalViews <- function() {
  globalViews <- views$getScenViewConf(scenId = "1", symName = activeSymbol$name)
  globalViewsFilePath <- file.path(dirname(configJSONFileName), "views.json")
  globalViewsFileExists <- file.exists(globalViewsFilePath)
  if (globalViewsFileExists) {
    globalViewsTmp <- tryCatch(
      {
        fromJSON(globalViewsFilePath, simplifyDataFrame = FALSE, simplifyVector = FALSE)
      },
      error = function(e) {
        flog.error("Could not read views file: %s. Error message: %s.", globalViewsFilePath, conditionMessage(e))
        showErrorMsg(
          lang$errMsg$fileRead$title,
          sprintf(lang$errMsg$fileRead$desc, globalViewsFilePath)
        )
        return(NA)
      }
    )
  } else {
    globalViewsTmp <- list()
  }
  if (is.list(globalViewsTmp)) {
    if (length(globalViews)) {
      globalViewsTmp[[activeSymbol$name]] <- globalViews
    } else {
      globalViewsTmp[[activeSymbol$name]] <- NULL
    }
    if (length(globalViewsTmp)) {
      tryCatch(
        {
          write_json(globalViewsTmp, globalViewsFilePath, pretty = TRUE, auto_unbox = TRUE, null = "null")
        },
        error = function(e) {
          flog.error("Could not write views file: %s. Error message: %s.", globalViewsFilePath, conditionMessage(e))
          showErrorMsg(
            lang$errMsg$fileWrite$title,
            sprintf(lang$errMsg$fileWrite$desc, globalViewsFilePath)
          )
        }
      )
    } else if (globalViewsFileExists) {
      # remove file if no more views
      if (!identical(unlink(globalViewsFilePath), 0L)) {
        flog.error("Could not remove file: %s. Do you lack write permissions?", globalViewsFilePath)
        showErrorMsg(
          lang$errMsg$fileWrite$title,
          sprintf(lang$errMsg$fileWrite$desc, globalViewsFilePath)
        )
      }
      flog.debug("Global views file removed successfully.")
    }
  }
}
removeGlobalViews <- function() {
  globalViewsFilePath <- file.path(dirname(configJSONFileName), "views.json")
  if (!file.exists(globalViewsFilePath)) {
    return()
  }
  globalViewsTmp <- tryCatch(
    {
      fromJSON(globalViewsFilePath, simplifyDataFrame = FALSE, simplifyVector = FALSE)
    },
    error = function(e) {
      flog.error("Could not read views file: %s. Error message: %s.", globalViewsFilePath, conditionMessage(e))
      showErrorMsg(
        lang$errMsg$fileRead$title,
        sprintf(lang$errMsg$fileRead$desc, globalViewsFilePath)
      )
      return(NA)
    }
  )
  if (!is.list(globalViewsTmp)) {
    return()
  }
  globalViewsTmp[[activeSymbol$name]] <- NULL
  if (!length(globalViewsTmp)) {
    # remove file if no more views
    if (!identical(unlink(globalViewsFilePath), 0L)) {
      flog.error("Could not remove file: %s. Do you lack write permissions?", globalViewsFilePath)
      showErrorMsg(
        lang$errMsg$fileWrite$title,
        sprintf(lang$errMsg$fileWrite$desc, globalViewsFilePath)
      )
    }
    flog.debug("Global views file removed successfully.")
    return()
  }
  tryCatch(
    {
      write_json(globalViewsTmp, globalViewsFilePath, pretty = TRUE, auto_unbox = TRUE, null = "null")
      flog.debug("Global views successfully removed from global views file.")
    },
    error = function(e) {
      flog.error("Could not write views file: %s. Error message: %s.", globalViewsFilePath, conditionMessage(e))
      showErrorMsg(
        lang$errMsg$fileWrite$title,
        sprintf(lang$errMsg$fileWrite$desc, globalViewsFilePath)
      )
    }
  )
}
setViews()

hideFilter <- function() {
  hideEl(session, "#preview_output_plotly-data_filter")
  hideEl(session, "#preview_output_dygraphs-data_filter")
  hideEl(session, "#preview_output_leaflet-data_filter")
  hideEl(session, "#preview_output_timevis-data_filter")
}
hideFilter()
# hideEl(session, any(startsWith("#preview_output_") && endsWith("-data_filter")))
scenMetaDb <- NULL
tryCatch(
  {
    scenMetaDb <- db$fetchScenList(scode = SCODEMAP[["scen"]])
  },
  error = function(e) {
    flog.error(
      "Problems fetching list of scenarios from database. Error message: %s.",
      conditionMessage(e)
    )
    errMsg <<- lang$errMsg$fetchScenData$desc
  }
)
showErrorMsg(lang$errMsg$fetchScenData$title, errMsg)
if (!is.null(scenMetaDb) && nrow(scenMetaDb)) {
  # by default, put most recently saved scenario first
  scenList <- formatScenList(scenMetaDb, uid, "_stime", desc = TRUE)
  updateSelectInput(session, "scenList", choices = scenList)
  hideEl(session, "#noDbScen")
  showEl(session, "#dbScen")
} else {
  hideEl(session, "#dbScen")
  showEl(session, "#noDbScen")
}
updatePreviewData <- function(tabularInputWithData, tabularOutputWithData, configScalars) {
  if (any(!isEmptyInput)) {
    changeActiveSymbol(which(!isEmptyInput)[1])
  } else if (any(!isEmptyOutput)) {
    changeActiveSymbol(which(!isEmptyOutput)[1] + length(modelIn))
  } else {
    showErrorMsg(
      lang$adminMode$graphs$errMsg$errTitle1,
      lang$adminMode$graphs$errMsg$errContent1
    )
    return()
  }
  showEl(session, "#preview_wrapper")
  gamsSymbolsTmp <- setNames(
    list(
      c(tabularInputWithData),
      c(tabularOutputWithData)
    ),
    c(
      lang$adminMode$graphs$ui$input,
      lang$adminMode$graphs$ui$output
    )
  )
  updateSelectInput(session, "gams_symbols",
    choices = gamsSymbolsTmp
  )
  if (identical(gamsSymbolsTmp[[1]], scalarsOutName)) {
    currentSelection <<- "valuebox"
  }
  if (identical(length(configScalars), 3L) && nrow(configScalars)) {
    session$sendCustomMessage(
      "gms-setScalarOutputs",
      list(
        indices = configScalars[[1]],
        aliases = configScalars[[2]]
      )
    )
  }
  slideToggleEl(session, "#previewDataInputWrapper",
    toggleIconDiv = "#previewDataInputToggle"
  )
  rv$initData <<- TRUE
}
validateGraphConfig <- function(graphJSON) {
  if (identical(is.na(graphJSON$graph$xaxis$rangefrom), TRUE) || identical(is.na(graphJSON$graph$xaxis$rangeto), TRUE)) {
    return(lang$adminMode$graphs$validate$val1)
  }
  if (identical(input$chart_tool, "custom")) {
    if (identical(nchar(trimws(graphJSON$outType)), 0L)) {
      return("Please specify a name!")
    }
    if (invalidCustomRender) {
      return(lang$adminMode$graphs$validate$custom1)
    }
  }
  return("")
}

saveAndReload <- function(selected) {
  currentConfig[[currentSelection]] <<- isolate(rv$graphConfig$graph)
  currentSelection <<- selected
  if (length(currentConfig[[currentSelection]])) {
    isolate({
      rv$graphConfig$graph <- currentConfig[[currentSelection]]
      if (!selected %in% noTitle) {
        rv$graphConfig$graph$title <- activeSymbol$alias
      }
    })
    allDataAvailable <<- TRUE
  } else {
    isolate({
      rv$graphConfig$graph <- list()
    })
  }
}
changeActiveSymbol <- function(id) {
  if (id <= length(modelIn)) {
    headers <- modelIn[[id]]$headers
    headerAliases <- vapply(headers, "[[", character(1L),
      "alias",
      USE.NAMES = FALSE
    )
    activeSymbol <<- list(
      id = id, name = names(modelIn)[id],
      alias = modelInAlias[id],
      isInput = TRUE,
      indices = setNames(
        names(headers),
        headerAliases
      ),
      indexTypes = vapply(headers, "[[", character(1L),
        "type",
        USE.NAMES = FALSE
      )
    )
    scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "numeric"]
    session$sendCustomMessage("gms-setIndices", list(
      indices = names(headers),
      aliases = headerAliases,
      scalarIndices = unname(scalarIndices),
      scalarAliases = names(scalarIndices)
    ))
  } else {
    id_out <- id - length(modelIn)
    headers <- modelOut[[id_out]]$headers
    headerAliases <- vapply(headers, "[[", character(1L),
      "alias",
      USE.NAMES = FALSE
    )
    activeSymbol <<- list(
      id = id, name = names(modelOut)[id_out],
      alias = modelOutAlias[id_out],
      isInput = FALSE,
      indices = setNames(
        names(headers),
        headerAliases
      ),
      indexTypes = vapply(headers, "[[", character(1L),
        "type",
        USE.NAMES = FALSE
      )
    )
    scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "numeric"]
    session$sendCustomMessage("gms-setIndices", list(
      indices = names(headers),
      aliases = headerAliases,
      scalarIndices = unname(scalarIndices),
      scalarAliases = names(scalarIndices)
    ))
  }
  activeSymbolName <<- activeSymbol$name
  indices <- activeSymbol$indices
  # rv$graphConfig$graph$title <- activeSymbol$alias
  # if(isFALSE(rv$initData) || identical(input$chart_tool, "pie")){
  #   rv$refreshOptions <- rv$refreshOptions + 1L
  # }
  getCurrentGraphConfig()
  rv$initData <- FALSE
  rv$initData <- TRUE
}
updateYAxes <- function(dyReset = NULL) {
  if (input$chart_tool %in% plotlyChartTools) {
    if (axisOptionsGlobal[["y"]] > 0L) {
      rv$graphConfig$graph$yaxis <<- list(
        title = if (length(input$y_title)) input$y_title else "",
        showgrid = isTRUE(input$y_showgrid),
        zeroline = isTRUE(input$y_zeroline),
        showticklabels = isTRUE(input$y_showticklabels),
        categoryorder = if (length(input$y_categoryorder)) input$y_categoryorder else "trace",
        rangefrom = if (length(input$y_rangefrom)) input$y_rangefrom else NULL,
        rangeto = if (length(input$y_rangeto)) input$y_rangeto else NULL
      )
      showEl(session, "#left_yaxis")
    } else {
      rv$graphConfig$graph$yaxis <<- list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      )
      hideEl(session, "#left_yaxis")
    }
    if (axisOptionsGlobal[["y2"]] > 0L) {
      rv$graphConfig$graph$y2axis <<- list(
        title = if (length(input$y2_title)) input$y2_title else "",
        showgrid = isTRUE(input$y2_showgrid),
        zeroline = isTRUE(input$y2_zeroline),
        showticklabels = isTRUE(input$y2_showticklabels),
        categoryorder = if (!is.null(input$y2_categoryorder) && length(input$y2_categoryorder)) input$y2_categoryorder else "trace",
        rangefrom = if (length(input$y2_rangefrom)) input$y2_rangefrom else NULL,
        rangeto = if (length(input$y2_rangeto)) input$y2_rangeto else NULL
      )
      showEl(session, "#right_yaxis")
    } else {
      rv$graphConfig$graph$y2axis <<- NULL
      hideEl(session, "#right_yaxis")
    }
  } else if (identical(input$chart_tool, "dygraphs")) {
    if (axisOptionsGlobal[["y"]] > 0L) {
      rv$graphConfig$graph$yaxis <<- list(
        name = "y",
        label = if (length(input$dyAxis_label)) input$dyAxis_label else NULL,
        valueRange = list(
          if (length(input$dyAxis_valueRangeFrom) && nchar(input$dyAxis_valueRangeFrom)) input$dyAxis_valueRangeFrom else NULL,
          if (length(input$dyAxis_valueRangeTo) && nchar(input$dyAxis_valueRangeTo)) input$dyAxis_valueRangeTo else NULL
        ),
        axisLineColor = if (length(input$dyAxis_axisLineColor)) input$dyAxis_axisLineColor else NULL,
        axisLineWidth = if (length(input$dyAxis_axisLineWidth)) input$dyAxis_axisLineWidth else 0.3,
        axisLabelFontSize = if (!isTRUE(dyReset) && length(input$dyAxis_axisLabelFontSize)) input$dyAxis_axisLabelFontSize else 14L,
        drawGrid = if (!isTRUE(dyReset) && length(input$dyAxis_drawGrid)) input$dyAxis_drawGrid else TRUE,
        gridLineWidth = if (length(input$dyAxis_gridLineWidth)) input$dyAxis_gridLineWidth else 0.3,
        # if one axis is removed, the other one should have independet ticks
        independentTicks = if (axisOptionsGlobal[["y2"]] > 0L) {
          !isFALSE(input$dyAxis_independentTicks)
        } else {
          TRUE
        }
      )
      if (isTRUE(dyReset)) {
        # this is necessary because when axisOptionsGlobal[["y"]] = 0L, special
        # options are set for y axis in order to be invisible.
        # These options should not be reloaded when axisOptionsGlobal[["y"]] > 0L
        rv$graphConfig$graph$yaxis$gridLineColor <<- "gray"
        updateCheckboxInput(session, "dyAxis_drawGrid", value = TRUE)
        updateNumericInput(session, "dyAxis_axisLabelFontSize", value = 14L)
      }
      if (!(axisOptionsGlobal[["y2"]] > 0L)) {
        updateCheckboxInput(session, "dyAxis_independentTicks", value = TRUE)
      }
      showEl(session, "#left_dyAxis")
    } else {
      rv$graphConfig$graph$yaxis <<- list(
        name = "y",
        label = "",
        valueRange = list(NULL, NULL),
        axisLineColor = NULL,
        axisLineWidth = NULL,
        axisLabelFontSize = 0L,
        drawGrid = TRUE,
        gridLineColor = "#00000000",
        gridLineWidth = NULL,
        independentTicks = TRUE
      )
      hideEl(session, "#left_dyAxis")
    }
    if (axisOptionsGlobal[["y2"]] > 0L) {
      rv$graphConfig$graph$yaxis2 <<- list(
        name = "y2",
        label = if (length(input$dyAxis2_label)) input$dyAxis2_label else NULL,
        valueRange = list(
          if (length(input$dyAxis2_valueRangeFrom) && nchar(input$dyAxis2_valueRangeFrom)) input$dyAxis2_valueRangeFrom else NULL,
          if (length(input$dyAxis2_valueRangeTo) && nchar(input$dyAxis2_valueRangeTo)) input$dyAxis2_valueRangeTo else NULL
        ),
        axisLineColor = if (length(input$dyAxis2_axisLineColor)) input$dyAxis2_axisLineColor else NULL,
        axisLineWidth = if (length(input$dyAxis2_axisLineWidth)) input$dyAxis2_axisLineWidth else 0.3,
        axisLabelFontSize = if (length(input$dyAxis2_axisLabelFontSize)) input$dyAxis2_axisLabelFontSize else 14L,
        drawGrid = !isFALSE(input$dyAxis2_drawGrid),
        gridLineWidth = if (length(input$dyAxis2_gridLineWidth)) input$dyAxis2_gridLineWidth else 0.3,
        # if one axis is removed, the other one should have independet ticks
        independentTicks = if (axisOptionsGlobal[["y"]] > 0L) {
          if (!is.null(input$dyAxis2_independentTicks) && length(input$dyAxis2_independentTicks)) input$dyAxis2_independentTicks else NULL
        } else {
          TRUE
        }
      )
      if (!(axisOptionsGlobal[["y"]] > 0L)) {
        updateCheckboxInput(session, "dyAxis2_independentTicks", value = TRUE)
      }
      showEl(session, "#right_dyAxis")
    } else {
      rv$graphConfig$graph$yaxis2 <<- NULL
      hideEl(session, "#right_dyAxis")
    }
  }
}

output$rendererLabelWrapper <- renderUI({
  if (length(rv$graphConfig$label) && !identical(trimws(rv$graphConfigg$label), "")) {
    tags$div(
      id = "rendererLabel",
      class = "readme-wrapper label-wrapper",
      markdown(rv$graphConfig$label)
    )
  }
})

observe({
  if (length(input$renderer_label) && !identical(trimws(input$renderer_label), "")) {
    rv$graphConfig$label <- input$renderer_label
  } else {
    rv$graphConfig$label <- NULL
  }
})

observeEvent(input$dbInput, {
  req(identical(length(input$scenList), 1L))

  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  rv$initData <- FALSE

  scenSelected <- regmatches(input$scenList,
    regexpr("_", input$scenList),
    invert = TRUE
  )
  sidToLoad <- suppressWarnings(as.integer(lapply(scenSelected, "[[", 1L)[[1L]]))
  if (is.na(sidToLoad)) {
    flog.error(
      "Bad scenario ID selected: '%s'. This seems like the user tried to tamper with the app!",
      lapply(scenSelected, "[[", 1L)[[1L]]
    )
    return(NULL)
  }
  tryCatch(
    {
      scenData$load(sidToLoad, refId = "sb")
      scenDataTmp <- scenData$get("sb")
    },
    error = function(e) {
      flog.error(
        "Some error occurred loading scenario: '%s' from database. Error message: %s.",
        sidToLoad, conditionMessage(e)
      )
      errMsg <<- lang$errMsg$loadScen$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))) {
    return(NULL)
  }
  if (length(modelOut)) {
    scenInputData <- scenDataTmp[-seq_along(modelOut)]
    modelOutputData <<- scenDataTmp[seq_along(modelOut)]
    names(modelOutputData) <<- names(modelOut)
  } else {
    scenInputData <- scenDataTmp
    scenOutputData <- NULL
  }
  names(scenInputData) <- inputDsNames
  newInputCount <- 0L

  configScalars <<- scenData$getScalars("sb")

  errMsg <- NULL
  loadMode <- "scen"
  datasetsToFetch <- modelInTabularData
  overwriteInput <- 1L
  dfClArgs <- NULL

  source("./modules/input_load.R", local = TRUE)
  if (!is.null(errMsg)) {
    return(NULL)
  }
  isEmptyInput <<- isNonemptyDataset(modelInputData)
  tabularInputWithData <- inputSymMultiDimChoices[!isEmptyInput]
  isEmptyOutput <<- isNonemptyDataset(modelOutputData)
  tabularOutputWithData <- outputSymMultiDimChoices[!isEmptyOutput]

  tryCatch(
    {
      attachments$clear(cleanLocal = TRUE)
      attachments$initScenData(sidToLoad)
    },
    error = function(e) {
      flog.error(
        "Some error occurred loading attachment data for scenario: '%s' from database. Error message: %s.",
        sidToLoad, conditionMessage(e)
      )
      errMsg <<- lang$errMsg$loadScen$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))) {
    return(NULL)
  }

  updatePreviewData(
    tabularInputWithData,
    tabularOutputWithData,
    configScalars
  )
})
observeEvent(input$localInput, {
  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL
  rv$initData <- FALSE

  fileType <- tolower(tools::file_ext(isolate(input$localInput$datapath)))
  errMsg <- NULL
  if (identical(fileType, "gdx") && useGdx) {
    loadMode <- "gdx"
    datasetsToFetch <- modelInTabularData
  } else if (fileType %in% xlsio$getValidExtensions()) {
    loadMode <- "xls"
    datasetsToFetch <- modelInTabularData
  } else {
    errMsg <- lang$errMsg$GAMSInput$desc
  }
  if (is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))) {
    return(NULL)
  }
  # load input data
  overwriteInput <- 1L
  loadModeWorkDir <- dirname(isolate(input$localInput$datapath))
  loadModeFileName <- basename(isolate(input$localInput$datapath))
  dfClArgs <- NULL
  source("./modules/input_load.R", local = TRUE)
  if (!is.null(errMsg)) {
    return(NULL)
  }
  isEmptyInput <<- isNonemptyDataset(modelInputData)
  tabularInputWithData <- inputSymMultiDimChoices[!isEmptyInput]

  scalarIdTmp <- match(scalarsFileName, tolower(names(scenInputData)))[[1L]]
  if (!is.na(scalarIdTmp)) {
    configScalars <<- scenInputData[[scalarIdTmp]]
  } else {
    configScalars <<- tibble()
  }
  tabularOutputWithData <- NULL
  if (identical(loadMode, "gdx")) {
    tryCatch(
      {
        outputDataTmp <- loadScenData(
          metaData = modelOut,
          workDir = dirname(isolate(input$localInput$datapath)),
          templates = modelOutTemplate,
          method = loadMode,
          fileName = basename(isolate(input$localInput$datapath)),
          xlsio = xlsio
        )
      },
      error = function(e) {
        flog.error(
          "Problems loading output data. Error message: %s.",
          conditionMessage(e)
        )
        errMsg <<- lang$errMsg$readOutput$desc
      }
    )
    if (is.null(showErrorMsg(lang$errMsg$readOutput$title, errMsg))) {
      return()
    }
    if (!is.null(outputDataTmp$tabular)) {
      modelOutputData <<- outputDataTmp$tabular
      names(modelOutputData) <<- names(modelOut)
      if (scalarsOutName %in% names(modelOutputData)) {
        configScalars <<- bind_rows(
          configScalars,
          modelOutputData[[scalarsOutName]]
        )
        if (length(config$hiddenOutputScalars)) {
          rowsToFiler <- !modelOutputData[[scalarsOutName]][[1]] %in% config$hiddenOutputScalars
          modelOutputData[[scalarsOutName]] <<- modelOutputData[[scalarsOutName]][rowsToFiler, ]
        }
      }
    }
    isEmptyOutput <<- isNonemptyDataset(modelOutputData)
    tabularOutputWithData <- outputSymMultiDimChoices[!isEmptyOutput]
  }
  updatePreviewData(
    tabularInputWithData,
    tabularOutputWithData,
    configScalars
  )
})
observeEvent(input$chart_title, {
  rv$graphConfig$graph$title <<- input$chart_title
})
observeEvent(input$leafFlow_lng, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_lng[1])]]]]$lng0 <<- input$leafFlow_lng[2]
})
observeEvent(input$leafFlow_lat1, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_lat1[1])]]]]$lat1 <<- input$leafFlow_lat1[2]
})
observeEvent(input$leafFlow_lng1, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_lng1[1])]]]]$lng1 <<- input$leafFlow_lng1[2]
})
observeEvent(input$leafFlow_flow, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_flow[1])]]]]$flow <<- input$leafFlow_flow[2]
})
observeEvent(input$leafFlow_time, {
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_time[1])]]]]$time <<- input$leafFlow_time[2]
})
observeEvent(input$leafFlow_label, {
  if (nchar(input$leafFlow_label[2])) {
    label <- input$leafFlow_label[2]
  } else {
    label <- NULL
  }
  rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_label[1])]]]]$layerId <<- label
})
observeEvent(input$leafFlow_color, {
  if (nchar(input$leafFlow_color[2])) {
    rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_color[1])]]]]$color <<- input$leafFlow_color[2]
  } else {
    rv$graphConfig$graph$flows[[idLabelMap$leaflet_flows[[as.integer(input$leafFlow_color[1])]]]]$color <<- NULL
  }
})
observeEvent(input$leafFlow_minThickness, {
  minThickness <- as.numeric(input$leafFlow_minThickness[2])
  if (is.na(minThickness)) {
    flog.error(
      "Minimum thickness: '%s' could not be converted to numeric.",
      input$leafFlow_minThickness[2]
    )
    return()
  }
  rv$graphConfig$graph$flows[[idLabelMap$
    leaflet_flows[[as.integer(input$
    leafFlow_minThickness[1])]]]]$minThickness <<- minThickness
})
observeEvent(input$leafFlow_maxThickness, {
  maxThickness <- as.numeric(input$leafFlow_maxThickness[2])
  if (is.na(maxThickness)) {
    flog.error(
      "Minimum thickness: '%s' could not be converted to numeric.",
      input$leafFlow_maxThickness[2]
    )
    return()
  }
  rv$graphConfig$graph$flows[[idLabelMap$
    leaflet_flows[[as.integer(input$
    leafFlow_maxThickness[1])]]]]$maxThickness <<- maxThickness
})


observeEvent(input$leafChart_lng, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_lng[1])]]]]$lng <<- input$leafChart_lng[2]
})
observeEvent(input$leafChart_lat, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_lat[1])]]]]$lat <<- input$leafChart_lat[2]
})
observeEvent(input$leafChart_chartdata, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_chartdata[1])]]]]$chartdata <<- input$leafChart_chartdata[2:length(input$leafChart_chartdata)]
})
observeEvent(input$leafChart_time, {
  if (!identical(input$leafChart_time, "_")) {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_time[1])]]]]$time <<- input$leafChart_time[2]
  } else {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_time[1])]]]]$time <<- NULL
  }
})
observeEvent(input$leafChart_type, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_type[1])]]]]$type <<- input$leafChart_type[2]
})
observeEvent(input$leafChart_width, {
  if (nchar(input$leafChart_width[2])) {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_width[1])]]]]$width <<- as.numeric(input$leafChart_width[2])
  } else {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_width[1])]]]]$width <<- NULL
  }
})
observeEvent(input$leafChart_height, {
  if (nchar(input$leafChart_height[2])) {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_height[1])]]]]$height <<- as.numeric(input$leafChart_height[2])
  } else {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_height[1])]]]]$height <<- NULL
  }
})
observeEvent(input$leafChart_opacity, {
  if (nchar(input$leafChart_opacity[2]) && as.numeric(input$leafChart_opacity[2]) >= 0 && as.numeric(input$leafChart_opacity[2]) <= 1) {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_opacity[1])]]]]$opacity <<- as.numeric(input$leafChart_opacity[2])
  } else {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_opacity[1])]]]]$opacity <<- NULL
  }
})
observeEvent(input$leafChart_showlabels, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_showlabels[1])]]]]$showLabels <<- as.logical(input$leafChart_showlabels[2])
})
observeEvent(input$leafChart_transitionTime, {
  if (!identical(input$leafChart_time, "_") && nchar(input$leafChart_transitionTime[2])) {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_transitionTime[1])]]]]$transitionTime <<- as.numeric(input$leafChart_transitionTime[2])
  } else {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_transitionTime[1])]]]]$transitionTime <<- NULL
  }
})
observeEvent(input$leafChart_layerId, {
  if (!identical(input$leafChart_time, "_")) {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_layerId[1])]]]]$layerId <<- input$leafChart_layerId[2]
  } else {
    rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_layerId[1])]]]]$layerId <<- NULL
  }
})
observeEvent(input$leafChart_legend, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_legend[1])]]]]$legend <<- as.logical(input$leafChart_legend[2])
})
observeEvent(input$leafChart_legendPosition, {
  rv$graphConfig$graph$minicharts[[idLabelMap$leaflet_minicharts[[as.integer(input$leafChart_legendPosition[1])]]]]$legendPosition <<- input$leafChart_legendPosition[2]
})

observeEvent(input$leafMark_lng, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_lng[1])]]]]$lng <<- input$leafMark_lng[2]
})
observeEvent(input$leafMark_label, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_label[1])]]]]$label <<- input$leafMark_label[2]
})
observeEvent(input$leafMark_groupName, {
  id <- idLabelMap$leaflet_markers[[as.integer(input$leafMark_groupName[1])]]
  if (nchar(input$leafMark_groupName[2])) {
    groupName <- input$leafMark_groupName[2]
  } else {
    groupName <- NULL
  }
  oldName <- rv$graphConfig$graph$markers[[id]]$group
  if (length(oldName) || length(groupName)) {
    leafletGroups$update(old = oldName, new = groupName)
  } else {
    return()
  }
  rv$updateLeafletGroups <- rv$updateLeafletGroups + 1L

  rv$graphConfig$graph$markers[[id]]$group <<- groupName
})
observeEvent(input$leafMark_labelcolor, {
  if (nchar(input$leafMark_labelcolor[2])) {
    color <- input$leafMark_labelcolor[2]
  } else {
    color <- NULL
  }
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelcolor[1])]]]]$labelOptions$style$color <<- color
})
observeEvent(input$leafMark_labelbgcolor, {
  if (nchar(input$leafMark_labelbgcolor[2])) {
    color <- input$leafMark_labelbgcolor[2]
  } else {
    color <- NULL
  }
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelbgcolor[1])]]]]$labelOptions$style[["background-color"]] <<- color
})
observeEvent(input$leafMark_labelsize, {
  if (nchar(input$leafMark_labelsize[2])) {
    textsize <- paste0(input$leafMark_labelsize[2], "px")
  } else {
    textsize <- NULL
  }
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelsize[1])]]]]$labelOptions$textsize <<- textsize
})
observeEvent(input$leafMark_icon, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_icon[1])]]]]$iconOptions$icon <<- input$leafMark_icon[2]
})
observeEvent(input$leafMark_iconColor, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_iconColor[1])]]]]$iconOptions$iconColor <<- input$leafMark_iconColor[2]
})
observeEvent(input$leafMark_markerColor, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_markerColor[1])]]]]$iconOptions$markerColor <<- input$leafMark_markerColor[2]
})
observeEvent(input$leafMark_labelPermanent, {
  rv$graphConfig$graph$markers[[idLabelMap$leaflet_markers[[as.integer(input$leafMark_labelPermanent[1])]]]]$labelOptions$permanent <<- identical(input$leafMark_labelPermanent[2], 0L)
})
observeEvent(rv$updateLeafletGroups, {
  choicesTmp <- leafletGroups$get()
  updateSelectInput(session, "leaflc_baseGroups",
    choices = if (!is.null(choicesTmp)) choicesTmp else character(0),
    selected = intersect(rv$graphConfig$graph$layersControl$baseGroups, choicesTmp)
  )
  updateSelectInput(session, "leaflc_overlayGroups",
    choices = if (!is.null(choicesTmp)) choicesTmp else character(0),
    selected = intersect(rv$graphConfig$graph$layersControl$overlayGroups, choicesTmp)
  )
  updateSelectInput(session, "leaflet_hideGroups",
    choices = if (!is.null(choicesTmp)) choicesTmp else character(0),
    selected = intersect(rv$graphConfig$graph$hideGroups, choicesTmp)
  )
})
observe(rv$graphConfig$graph$hideGroups <<- input$leaflet_hideGroups)
observe(rv$graphConfig$graph$layersControl$baseGroups <<- input$leaflc_baseGroups)
observe(rv$graphConfig$graph$layersControl$overlayGroups <<- input$leaflc_overlayGroups)
observe(rv$graphConfig$graph$layersControl$position <<- input$leaflc_position)
observe(rv$graphConfig$graph$layersControl$options$collapsed <<- input$leaflc_collapsed)

observeEvent(input$pivot_rows, ignoreNULL = FALSE, {
  if (length(input$pivot_rows) > 0) {
    rv$graphConfig$pivottable$rows <<- input$pivot_rows
  } else {
    rv$graphConfig$pivottable$rows <<- NULL
  }
})
observeEvent(input$pivot_cols, ignoreNULL = FALSE, {
  if (length(input$pivot_cols) > 0) {
    rv$graphConfig$pivottable$cols <<- input$pivot_cols
  } else {
    rv$graphConfig$pivottable$cols <<- NULL
  }
})
observeEvent(input$pivot_aggregatorName, {
  rv$graphConfig$pivottable$aggregatorName <<- input$pivot_aggregatorName
})
observeEvent(c(input$pivot_vals, input$pivot_vals2), {
  if (identical(input$pivot_vals, "_") || identical(input$pivot_vals, NULL)) {
    valstmp1 <<- NULL
  } else {
    valstmp1 <<- input$pivot_vals
  }
  if (identical(input$pivot_vals2, "_") || identical(input$pivot_vals2, NULL)) {
    valstmp2 <<- NULL
  } else {
    valstmp2 <<- input$pivot_vals2
  }
  if (identical(valstmp2, NULL)) {
    rv$graphConfig$pivottable$vals <<- valstmp1
  } else if (identical(valstmp1, NULL)) {
    rv$graphConfig$pivottable$vals <<- c("", valstmp2)
  } else {
    rv$graphConfig$pivottable$vals <<- c(valstmp1, valstmp2)
  }
})
observeEvent(input$pivot_rendererName, {
  rv$graphConfig$pivottable$rendererName <<- input$pivot_rendererName
})
observeEvent(input$pivot_locale, {
  rv$graphConfig$pivottable$locale <<- input$pivot_locale
})
observeEvent(input$pivot_subtotals, {
  rv$graphConfig$pivottable$subtotals <<- input$pivot_subtotals
})

# observeEvent(input$timevis_series, {
#  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]] <<- input$timevis_series
# })

observeEvent(input$timedata_start,
  {
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_start[1])]]]]$start <<- input$timedata_start[2]
  },
  priority = -500
)
observeEvent(input$timedata_end,
  {
    if (!identical(input$timedata_end[2], "_")) {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_end[1])]]]]$end <<- input$timedata_end[2]
    } else {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_end[1])]]]]$end <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$timedata_type,
  {
    rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_type[1])]]]]$type <<- input$timedata_type[2]
  },
  priority = -500
)
observeEvent(input$timedata_title,
  {
    if (!identical(input$timedata_title[2], "_")) {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_title[1])]]]]$title <<- input$timedata_title[2]
    } else {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_title[1])]]]]$title <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$timedata_group,
  {
    if (!identical(input$timedata_group[2], "_")) {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_group[1])]]]]$group <<- input$timedata_group[2]
    } else {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_group[1])]]]]$group <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$timedata_subgroup,
  {
    if (!identical(input$timedata_subgroup[2], "_")) {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgroup[1])]]]]$subgroup <<- input$timedata_subgroup[2]
    } else {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgroup[1])]]]]$subgroup <<- NULL
    }
  },
  priority = -500
)


observeEvent(input$timedata_grouptitle,
  {
    if (!identical(input$timedata_grouptitle[2], "_")) {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_grouptitle[1])]]]]$groupTitle <<- input$timedata_grouptitle[2]
    } else {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_grouptitle[1])]]]]$groupTitle <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$timedata_subgrouporder,
  {
    if (!identical(input$timedata_subgrouporder[2], "_")) {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgrouporder[1])]]]]$subgroupOrder <<- input$timedata_subgrouporder[2]
    } else {
      rv$graphConfig$graph$series[[idLabelMap$timevis_series[[as.integer(input$timedata_subgrouporder[1])]]]]$subgroupOrder <<- NULL
    }
  },
  priority = -500
)





observeEvent(input$timevis_showZoom, {
  rv$graphConfig$graph$showZoom <<- input$timevis_showZoom
})
observeEvent(input$timevis_zoomFactor, {
  rv$graphConfig$graph$zoomFactor <<- input$timevis_zoomFactor
})
observeEvent(input$timevis_fit, {
  rv$graphConfig$graph$fit <<- input$timevis_fit
})
observeEvent(input$timevis_editable, {
  rv$graphConfig$graph$editable <<- input$timevis_editable
})
observeEvent(input$timevis_multiselect, {
  rv$graphConfig$graph$multiselect <<- input$timevis_multiselect
})
observeEvent(input$timevis_showCurrentTime, {
  rv$graphConfig$graph$showCurrentTime <<- input$timevis_showCurrentTime
})

observeEvent(input$hist_norm, {
  if (identical(input$hist_norm, " ")) {
    value <- ""
  } else {
    value <- input$hist_norm
  }
  rv$graphConfig$graph$histnorm <<- value
})
observeEvent(input$hist_nbins, {
  rv$graphConfig$graph$nbins <<- input$hist_nbins
})
observeEvent(input$hist_barmode, {
  rv$graphConfig$graph$barmode <<- input$hist_barmode
})
# useful when color is specified automatically based on a domain!
observeEvent(input$hist_alpha, {
  rv$graphConfig$graph$alpha <<- input$hist_alpha
})
observeEvent(input$hist_cumulative, {
  rv$graphConfig$graph$cumulative <<- input$hist_cumulative
})
observeEvent(input$hist_horizontal, {
  if (identical(input$hist_horizontal, "horizontal")) {
    rv$graphConfig$graph$horizontal <<- TRUE
  } else {
    rv$graphConfig$graph$horizontal <<- FALSE
  }
})
observeEvent(input$add_piedata, {
  if (length(input$add_piedata) < 3L) {
    return()
  }
  arrayId <- input$add_piedata[1]
  if (!length(arrayId) || !nchar(arrayId)) {
    return()
  }
  if (!arrayId %in% names(rv$graphConfig$graph$traces)) {
    rv$graphConfig$graph$traces[[arrayId]] <<- list(
      labels = activeSymbol$indices[[1]],
      values = input$add_piedata[2],
      name = names(activeSymbol$indices)[1],
      hole = 0
    )
  } else {
    rv$graphConfig$graph$traces[[arrayId]]$values <<- input$add_piedata[2]
    rv$graphConfig$graph$traces[[arrayId]]$name <<- names(activeSymbol$indices)[match(
      input$add_piedata[2],
      activeSymbol$indices
    )[1]]
  }
})
observeEvent(input$remove_piedata, {
  if (length(input$remove_piedata) < 3L) {
    return()
  }
  arrayId <- input$remove_piedata[3]
  if (!length(arrayId) ||
    !arrayId %in% names(rv$graphConfig$graph$traces)) {
    return()
  }
  rv$graphConfig$graph$traces[[arrayId]] <<- NULL
})
observeEvent(input$chart_pielabel, {
  if (length(input$chart_pielabel) < 2L) {
    return()
  }
  arrayId <- input$chart_pielabel[1]
  if (!length(arrayId) ||
    !arrayId %in% names(rv$graphConfig$graph$traces)) {
    return()
  }
  rv$graphConfig$graph$traces[[arrayId]]$labels <- input$chart_pielabel[2]
})
observeEvent(input$chart_piehole, {
  if (length(input$chart_piehole) < 2L) {
    return()
  }
  arrayId <- input$chart_piehole[1]
  if (!length(arrayId) ||
    !arrayId %in% names(rv$graphConfig$graph$traces)) {
    return()
  }
  holeTmp <- suppressWarnings(as.numeric(input$chart_piehole[2]))
  if (is.na(holeTmp)) {
    return()
  }
  rv$graphConfig$graph$traces[[arrayId]]$hole <- holeTmp
})
observeEvent(input$chart_piename, {
  if (length(input$chart_piename) < 2L) {
    return()
  }
  arrayId <- input$chart_piename[1]
  if (!length(arrayId) ||
    !arrayId %in% names(rv$graphConfig$graph$traces)) {
    return()
  }
  if (nchar(input$chart_piename[2])) {
    rv$graphConfig$graph$traces[[arrayId]]$name <- input$chart_piename[2]
  } else {
    rv$graphConfig$graph$traces[[arrayId]]$name <- NULL
  }
})
observeEvent(input$marker_symbol,
  {
    if (identical(input$marker_symbol[2], "_")) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]]$marker$symbol <<- NULL
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_symbol[1])]]]]$marker$symbol <<- input$marker_symbol[2]
    }
  },
  priority = -500
)
observeEvent(input$marker_color,
  {
    if (nchar(input$marker_color[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_color[1])]]]]$marker$color <<- input$marker_color[2]
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_color[1])]]]]$marker$color <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$marker_colorDep,
  {
    if (nchar(input$marker_colorDep[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_colorDep[1])]]]]$marker$color <<- input$marker_colorDep[2]
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_colorDep[1])]]]]$marker$color <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$marker_size,
  {
    if (nchar(input$marker_size[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_size[1])]]]]$marker$size <<- as.numeric(input$marker_size[2])
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_size[1])]]]]$marker$size <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$marker_sizemode,
  {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_sizemode[1])]]]]$marker$sizemode <<- input$marker_sizemode[2]
  },
  priority = -500
)
observeEvent(input$marker_maxsize,
  {
    if (nchar(input$marker_maxsize[2]) && !identical(as.numeric(input$marker_maxsize[2]), 0)) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_maxsize[1])]]]]$marker$maxsize <<- as.numeric(input$marker_maxsize[2])
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_maxsize[1])]]]]$marker$maxsize <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$marker_line_width,
  {
    if (nchar(input$marker_line_width[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_width[1])]]]]$marker$line$width <<- as.numeric(input$marker_line_width[2])
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_width[1])]]]]$marker$line$width <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$marker_line_color,
  {
    if (nchar(input$marker_line_color[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_color[1])]]]]$marker$line$color <<- input$marker_line_color[2]
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$marker_line_color[1])]]]]$marker$line$color <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$trace_yaxis,
  {
    req(length(input$trace_yaxis) >= 2L)
    if (identical(input$trace_yaxis[2], "y2")) {
      axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] - 1L
      axisOptionsGlobal[["y2"]] <<- axisOptionsGlobal[["y2"]] + 1L
    } else {
      axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] + 1L
      axisOptionsGlobal[["y2"]] <<- axisOptionsGlobal[["y2"]] - 1L
    }
    updateYAxes()
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$trace_yaxis[1])]]]]$yaxis <<- input$trace_yaxis[2]
  },
  priority = -500
)
observeEvent(input$line_fill,
  {
    if (nchar(input$line_fill[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_fill[1])]]]]$fill <<- input$line_fill[2]
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_fill[1])]]]]$fill <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$line_color,
  {
    if (nchar(input$line_color[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_color[1])]]]]$line$color <<- input$line_color[2]
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_color[1])]]]]$line$color <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$line_width,
  {
    if (nchar(input$line_width[2])) {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_width[1])]]]]$line$width <<- as.numeric(input$line_width[2])
    } else {
      rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_width[1])]]]]$line$width <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$line_shape, {
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_shape[1])]]]]$line$shape <<- input$line_shape[2]
})
observeEvent(input$line_dash,
  {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$line_dash[1])]]]]$line$dash <<- input$line_dash[2]
  },
  priority = -500
)
observeEvent(input$trace_legend,
  {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$trace_legend[1])]]]]$showlegend <<- as.logical(input$trace_legend[2])
  },
  priority = -500
)
observeEvent(input$trace_frame,
  {
    if (!is.null(input$trace_frame[2]) && (!identical(input$trace_frame[2], "_"))) {
      frameTmp <- if (length(input$animation_frame)) {
        1000 / input$animation_frame
      } else {
        500
      }
      rv$graphConfig$graph$animation <- list(
        easing = if (length(input$animation_easing)) input$animation_easing else "linear",
        mode = if (length(input$animation_mode)) input$animation_mode else "immediate",
        redraw = if (length(input$animation_redraw)) input$animation_redraw else TRUE,
        frame = frameTmp,
        transition = frameTmp,
        slider = list(
          fontcolor = if (length(input$animation_slider_font_color)) input$animation_slider_font_color else "#000000",
          hide = if (length(input$animation_slider_hide)) input$animation_slider_hide else FALSE
        )
      )
      if (length(input$animation_slider_prefix) && !identical(input$animation_slider_prefix, "")) {
        rv$graphConfig$graph$animation$slider$prefix <- input$animation_slider_prefix
      }
      hideEl(session, "#no_plotly_animation_options")
      showEl(session, "#plotly_animation_options")
      traceframetmp <<- input$trace_frame[2]
    } else {
      hideEl(session, "#plotly_animation_options")
      showEl(session, "#no_plotly_animation_options")
      traceframetmp <<- NULL
      rv$graphConfig$graph$animation <<- NULL
    }
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$trace_frame[1])]]]]$frame <<- traceframetmp
  },
  priority = -450
)
observeEvent(input$animation_frame,
  {
    req(rv$graphConfig$graph$animation)
    # frame = amount of time between frames (in milliseconds)
    frameTmp <- 1000 / input$animation_frame
    rv$graphConfig$graph$animation$frame <<- frameTmp
    rv$graphConfig$graph$animation$transition <<- frameTmp
  },
  priority = -500
)
observeEvent(input$animation_easing,
  {
    req(rv$graphConfig$graph$animation)
    rv$graphConfig$graph$animation$easing <<- input$animation_easing
  },
  priority = -500
)
observeEvent(input$animation_redraw,
  {
    req(rv$graphConfig$graph$animation)
    rv$graphConfig$graph$animation$redraw <<- as.logical(input$animation_redraw)
  },
  priority = -500
)
observeEvent(input$animation_mode, {
  req(rv$graphConfig$graph$animation)
  rv$graphConfig$graph$animation$mode <<- input$animation_mode
})
observeEvent(input$animation_slider_hide,
  {
    req(rv$graphConfig$graph$animation)
    rv$graphConfig$graph$animation$slider$hide <<- as.logical(input$animation_slider_hide)
  },
  priority = -500
)
observeEvent(input$animation_slider_prefix,
  {
    req(rv$graphConfig$graph$animation)
    if (length(input$animation_slider_prefix) && !identical(input$animation_slider_prefix, "")) {
      rv$graphConfig$graph$animation$slider$prefix <<- input$animation_slider_prefix
    } else {
      rv$graphConfig$graph$animation$slider$prefix <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$animation_slider_font_color,
  {
    req(rv$graphConfig$graph$animation)
    if (nchar(input$animation_slider_font_color)) {
      rv$graphConfig$graph$animation$slider$fontcolor <<- input$animation_slider_font_color
    } else {
      rv$graphConfig$graph$animation$slider$fontcolor <<- NULL
    }
  },
  priority = -500
)
observeEvent(input$dyrange_activate, {
  if (identical(input$dyrange_activate, TRUE)) {
    rv$graphConfig$graph$dyRangeSelector <<- list(
      height = input$dyrange_height, strokeColor = input$dyrange_strokeColor,
      fillColor = input$dyrange_fillColor, retainDateWindow = input$dyrange_retainDateWindow,
      keepMouseZoom = input$dyrange_keepMouseZoom
    )
  } else {
    rv$graphConfig$graph$dyRangeSelector <<- NULL
  }
})
observeEvent(input$dyrange_height, {
  if (identical(input$dyrange_activate, TRUE)) {
    rv$graphConfig$graph$dyRangeSelector$height <<- input$dyrange_height
  }
})
observeEvent(input$dyrange_strokeColor, {
  if (identical(input$dyrange_activate, TRUE)) {
    rv$graphConfig$graph$dyRangeSelector$strokeColor <<- input$dyrange_strokeColor
  }
})
observeEvent(input$dyrange_fillColor, {
  if (identical(input$dyrange_activate, TRUE)) {
    rv$graphConfig$graph$dyRangeSelector$fillColor <<- input$dyrange_fillColor
  }
})
observeEvent(input$dyrange_keepMouseZoom, {
  if (identical(input$dyrange_activate, TRUE)) {
    rv$graphConfig$graph$dyRangeSelector$keepMouseZoom <<- input$dyrange_keepMouseZoom
  }
})
observeEvent(input$dyrange_retainDateWindow, {
  if (identical(input$dyrange_activate, TRUE)) {
    rv$graphConfig$graph$dyRangeSelector$retainDateWindow <<- input$dyrange_retainDateWindow
  }
})
observeEvent(input$dyrange_height, {
  if (identical(input$dyrange_activate, TRUE)) {
    rv$graphConfig$graph$dyRangeSelector$height <<- input$dyrange_height
  }
})
observeEvent(input$dyopt_logscale, {
  rv$graphConfig$graph$dyOptions$logscale <<- input$dyopt_logscale
})
# observeEvent(input$dyopt_drawGrid, {
#   rv$graphConfig$graph$dyOptions$drawGrid <<- input$dyopt_drawGrid
# })
observeEvent(input$dyser_yaxis, {
  req(length(input$dyser_yaxis) >= 2L)
  yTmp <- axisOptionsGlobal[["y"]]
  y2Tmp <- axisOptionsGlobal[["y2"]]
  if (identical(input$dyser_yaxis[2], "y2")) {
    axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] - 1L
    axisOptionsGlobal[["y2"]] <<- axisOptionsGlobal[["y2"]] + 1L
  } else {
    axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] + 1L
    axisOptionsGlobal[["y2"]] <<- axisOptionsGlobal[["y2"]] - 1L
  }
  if (identical(y2Tmp, 1L) && identical(axisOptionsGlobal[["y2"]], 0L) &&
    identical(yTmp, 0L) && identical(axisOptionsGlobal[["y"]], 1L)) {
    updateYAxes(dyReset = TRUE)
  } else {
    updateYAxes()
  }
  rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyser_yaxis[1])]]]]$yaxis <<- input$dyser_yaxis[2]
})
observeEvent(input$dyser_color, {
  req(length(input$dyser_color) >= 2L)
  if (nchar(input$dyser_color[2])) {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyser_color[1])]]]]$color <<- input$dyser_color[2]
  } else {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyser_color[1])]]]]$color <<- NULL
  }
})
observeEvent(input$dyopt_fillGraph, {
  if (length(input$dyopt_fillGraph) > 1) {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_fillGraph[1])]]]]$fillGraph <<- as.logical(input$dyopt_fillGraph[2])
  } else {
    rv$graphConfig$graph$dyOptions$fillGraph <<- input$dyopt_fillGraph
  }
})
observeEvent(input$dyopt_stepPlot, {
  if (length(input$dyopt_stepPlot) > 1) {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_stepPlot[1])]]]]$stepPlot <<- as.logical(input$dyopt_stepPlot[2])
  } else {
    rv$graphConfig$graph$dyOptions$stepPlot <<- input$dyopt_stepPlot
  }
})
observeEvent(input$dyopt_stemPlot, {
  if (length(input$dyopt_stemPlot) > 1) {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_stemPlot[1])]]]]$stemPlot <<- as.logical(input$dyopt_stemPlot[2])
  } else {
    rv$graphConfig$graph$dyOptions$stemPlot <<- input$dyopt_stemPlot
  }
})
observeEvent(input$dyopt_fillAlpha, {
  rv$graphConfig$graph$dyOptions$fillAlpha <<- input$dyopt_fillAlpha
})
observeEvent(input$dyopt_drawPoints, {
  if (length(input$dyopt_drawPoints) > 1) {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_drawPoints[1])]]]]$drawPoints <<- as.logical(input$dyopt_drawPoints[2])
  } else {
    rv$graphConfig$graph$dyOptions$drawPoints <<- input$dyopt_drawPoints
  }
})
observeEvent(input$dyopt_pointShape, {
  if (length(input$dyopt_pointShape) > 1) {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_pointShape[1])]]]]$pointShape <<- input$dyopt_pointShape[2]
  } else {
    rv$graphConfig$graph$dyOptions$pointShape <<- input$dyopt_pointShape
  }
})
observeEvent(input$dyopt_pointSize, {
  if (length(input$dyopt_pointSize) > 1) {
    rv$graphConfig$graph$ydata[[idLabelMap$chart_ydata[[as.integer(input$dyopt_pointSize[1])]]]]$pointSize <<- as.numeric(input$dyopt_pointSize[2])
  } else {
    rv$graphConfig$graph$dyOptions$pointSize <<- input$dyopt_pointSize
  }
})
observeEvent(input$dxAxis_label, {
  rv$graphConfig$graph$xaxis$label <<- input$dxAxis_label
})
observeEvent(input$dxAxis_drawGrid, {
  rv$graphConfig$graph$xaxis$drawGrid <<- input$dxAxis_drawGrid
})
observeEvent(input$dxAxis_gridLineWidth, {
  if (!is.numeric(input$dxAxis_gridLineWidth)) {
    rv$graphConfig$graph$xaxis$gridLineWidth <<- NULL
  } else {
    rv$graphConfig$graph$xaxis$gridLineWidth <<- input$dxAxis_gridLineWidth
  }
})
observeEvent(input$dxAxis_axisLineColor, {
  if (length(input$dxAxis_axisLineColor) && nchar(input$dxAxis_axisLineColor)) {
    rv$graphConfig$graph$xaxis$axisLineColor <<- input$dxAxis_axisLineColor
  } else {
    rv$graphConfig$graph$xaxis$axisLineColor <<- NULL
  }
})
observeEvent(input$dxAxis_axisLineWidth, {
  if (!is.numeric(input$dxAxis_axisLineWidth)) {
    rv$graphConfig$graph$xaxis$axisLineWidth <<- NULL
  } else {
    rv$graphConfig$graph$xaxis$axisLineWidth <<- input$dxAxis_axisLineWidth
  }
})
observeEvent(input$dxAxis_axisLabelFontSize, {
  if (!is.numeric(input$dxAxis_axisLabelFontSize)) {
    rv$graphConfig$graph$xaxis$axisLabelFontSize <<- NULL
  } else {
    rv$graphConfig$graph$xaxis$axisLabelFontSize <<- input$dxAxis_axisLabelFontSize
  }
})
observeEvent(input$dyAxis_label, {
  rv$graphConfig$graph$yaxis$label <<- input$dyAxis_label
})
observeEvent(input$dyAxis_independentTicks, {
  rv$graphConfig$graph$yaxis$independentTicks <<- input$dyAxis_independentTicks
})
observeEvent(input$dyAxis_drawGrid, {
  # when a second yaxis is used, its grid can only be drawn if drawGrid for first yaxis is also set to TRUE. Setting y1 grid color to 'transparent' therefore.
  if (isFALSE(input$dyAxis_drawGrid)) {
    rv$graphConfig$graph$yaxis$gridLineColor <<- "transparent"
  } else {
    rv$graphConfig$graph$yaxis$gridLineColor <<- "gray"
  }
})
observeEvent(input$dyAxis_gridLineWidth, {
  if (!is.numeric(input$dyAxis_gridLineWidth)) {
    rv$graphConfig$graph$yaxis$gridLineWidth <<- NULL
  } else {
    rv$graphConfig$graph$yaxis$gridLineWidth <<- input$dyAxis_gridLineWidth
  }
})
observeEvent(input$dyAxis_axisLineColor, {
  if (length(input$dyAxis_axisLineColor) && nchar(input$dyAxis_axisLineColor)) {
    rv$graphConfig$graph$yaxis$axisLineColor <<- input$dyAxis_axisLineColor
  } else {
    rv$graphConfig$graph$yaxis$axisLineColor <<- NULL
  }
})
observeEvent(input$dyAxis_axisLineWidth, {
  if (!is.numeric(input$dyAxis_axisLineWidth)) {
    rv$graphConfig$graph$yaxis$axisLineWidth <<- NULL
  } else {
    rv$graphConfig$graph$yaxis$axisLineWidth <<- input$dyAxis_axisLineWidth
  }
})
observeEvent(input$dyAxis_axisLabelFontSize, {
  if (!is.numeric(input$dyAxis_axisLabelFontSize)) {
    rv$graphConfig$graph$yaxis$axisLabelFontSize <<- NULL
  } else {
    rv$graphConfig$graph$yaxis$axisLabelFontSize <<- input$dyAxis_axisLabelFontSize
  }
})
observeEvent(input$dyAxis_valueRangeFrom, {
  req(length(input$dyAxis_valueRangeFrom) == 1L)
  val <- suppressWarnings(as.numeric(input$dyAxis_valueRangeFrom))
  if (is.na(val)) {
    val <- NULL
  }
  val2 <- NULL
  if (length(rv$graphConfig$graph$yaxis$valueRange[[2]]) && !is.na(rv$graphConfig$graph$yaxis$valueRange[[2]])) {
    val2 <- rv$graphConfig$graph$yaxis$valueRange[[2]]
  }
  rv$graphConfig$graph$yaxis$valueRange <<- list(val, val2)
})
observeEvent(input$dyAxis_valueRangeTo, {
  req(length(input$dyAxis_valueRangeTo) == 1L)
  val <- suppressWarnings(as.numeric(input$dyAxis_valueRangeTo))
  if (is.na(val)) {
    val <- NULL
  }
  val2 <- NULL
  if (length(rv$graphConfig$graph$yaxis$valueRange[[1]]) && !is.na(rv$graphConfig$graph$yaxis$valueRange[[1]])) {
    val2 <- rv$graphConfig$graph$yaxis$valueRange[[1]]
  }
  rv$graphConfig$graph$yaxis$valueRange <<- list(val2, val)
})

observeEvent(input$dyAxis2_label, {
  rv$graphConfig$graph$yaxis2$label <<- input$dyAxis2_label
})
observeEvent(input$dyAxis2_independentTicks, {
  rv$graphConfig$graph$yaxis2$independentTicks <<- input$dyAxis2_independentTicks
})
observeEvent(input$dyAxis2_drawGrid, {
  rv$graphConfig$graph$yaxis2$drawGrid <<- input$dyAxis2_drawGrid
})
observeEvent(input$dyAxis2_gridLineWidth, {
  if (!is.numeric(input$dyAxis2_gridLineWidth)) {
    rv$graphConfig$graph$yaxis2$gridLineWidth <<- NULL
  } else {
    rv$graphConfig$graph$yaxis2$gridLineWidth <<- input$dyAxis2_gridLineWidth
  }
})
observeEvent(input$dyAxis2_axisLineColor, {
  if (nchar(input$dyAxis2_axisLineColor)) {
    rv$graphConfig$graph$yaxis2$axisLineColor <<- input$dyAxis2_axisLineColor
  } else {
    rv$graphConfig$graph$yaxis2$axisLineColor <<- NULL
  }
})
observeEvent(input$dyAxis2_axisLineWidth, {
  if (!is.numeric(input$dyAxis2_axisLineWidth)) {
    rv$graphConfig$graph$yaxis2$axisLineWidth <<- NULL
  } else {
    rv$graphConfig$graph$yaxis2$axisLineWidth <<- input$dyAxis2_axisLineWidth
  }
})
observeEvent(input$dyAxis2_axisLabelFontSize, {
  if (!is.numeric(input$dyAxis2_axisLabelFontSize)) {
    rv$graphConfig$graph$yaxis2$axisLabelFontSize <<- NULL
  } else {
    rv$graphConfig$graph$yaxis2$axisLabelFontSize <<- input$dyAxis2_axisLabelFontSize
  }
})
observeEvent(input$dyAxis2_valueRangeFrom, {
  req(length(input$dyAxis2_valueRangeFrom) == 1L)
  val <- suppressWarnings(as.numeric(input$dyAxis2_valueRangeFrom))
  if (is.na(val)) {
    val <- NULL
  }
  val2 <- NULL
  if (length(rv$graphConfig$graph$yaxis2$valueRange[[2]]) && !is.na(rv$graphConfig$graph$yaxis2$valueRange[[2]])) {
    val2 <- rv$graphConfig$graph$yaxis2$valueRange[[2]]
  }
  rv$graphConfig$graph$yaxis2$valueRange <<- list(val, val2)
})
observeEvent(input$dyAxis2_valueRangeTo, {
  req(length(input$dyAxis2_valueRangeTo) == 1L)
  val <- suppressWarnings(as.numeric(input$dyAxis2_valueRangeTo))
  if (is.na(val)) {
    val <- NULL
  }
  val2 <- NULL
  if (length(rv$graphConfig$graph$yaxis2$valueRange[[1]]) && !is.na(rv$graphConfig$graph$yaxis2$valueRange[[1]])) {
    val2 <- rv$graphConfig$graph$yaxis2$valueRange[[1]]
  }
  rv$graphConfig$graph$yaxis2$valueRange <<- list(val2, val)
})
observeEvent(input$dyhighlight_activate, {
  if (isTRUE(input$dyhighlight_activate)) {
    rv$graphConfig$graph$dyHighlight <<- list(
      highlightCircleSize = input$dyhigh_circleSize,
      highlightSeriesBackgroundAlpha = input$dyhigh_seriesBackgroundAlpha,
      hideOnMouseOut = input$dyhigh_hideOnMouseOut,
      highlightSeriesOpts = list(
        strokeWidth = input$dyhigh_strokeWidth,
        strokeBorderWidth = input$dyhigh_strokeBorderWidth,
        strokeBorderColor = input$dyhigh_strokeBorderColor
      )
    )
  } else {
    rv$graphConfig$graph$dyHighlight <<- noDygraphHighlight
  }
})
observeEvent(input$dyhigh_circleSize, {
  if (isTRUE(input$dyhighlight_activate)) {
    rv$graphConfig$graph$dyHighlight$highlightCircleSize <<- input$dyhigh_circleSize
  }
})
observeEvent(input$dyhigh_seriesBackgroundAlpha, {
  if (isTRUE(input$dyhighlight_activate)) {
    rv$graphConfig$graph$dyHighlight$highlightSeriesBackgroundAlpha <<- input$dyhigh_seriesBackgroundAlpha
  }
})
observeEvent(input$dyhigh_hideOnMouseOut, {
  if (isTRUE(input$dyhighlight_activate)) {
    rv$graphConfig$graph$dyHighlight$hideOnMouseOut <<- input$dyhigh_hideOnMouseOut
  }
})
observeEvent(input$dyhigh_strokeWidth, {
  if (isTRUE(input$dyhighlight_activate)) {
    rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeWidth <<- input$dyhigh_strokeWidth
  }
})
observeEvent(input$dyhigh_strokeBorderWidth, {
  if (isTRUE(input$dyhighlight_activate)) {
    rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeBorderWidth <<- input$dyhigh_strokeBorderWidth
  }
})
observeEvent(input$dyhigh_strokeBorderColor, {
  if (isTRUE(input$dyhighlight_activate)) {
    if (nchar(input$dyhigh_strokeBorderColor)) {
      rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeBorderColor <<- input$dyhigh_strokeBorderColor
    } else {
      rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeBorderColor <<- "#ffffff"
    }
  }
})
observeEvent(input$dyLegend_activate, {
  if (isTRUE(input$dyLegend_activate)) {
    rv$graphConfig$graph$dyLegend <<- list(
      show = input$dyLegend_show,
      width = input$dyLegend_width,
      showZeroValues = input$dyLegend_showZeroValues,
      labelsSeparateLines = input$dyLegend_labelsSeparateLines
    )
  } else {
    rv$graphConfig$graph$dyLegend <<- list(show = "never")
  }
})
observeEvent(input$dyLegend_show, {
  if (isTRUE(input$dyLegend_activate)) {
    rv$graphConfig$graph$dyLegend$show <<- input$dyLegend_show
  }
})
observeEvent(input$dyLegend_width, {
  if (isTRUE(input$dyLegend_activate)) {
    rv$graphConfig$graph$dyLegend$width <<- input$dyLegend_width
  }
})
observeEvent(input$dyLegend_showZeroValues, {
  if (isTRUE(input$dyLegend_activate)) {
    rv$graphConfig$graph$dyLegend$showZeroValues <<- input$dyLegend_showZeroValues
  }
})
observeEvent(input$dyLegend_labelsSeparateLines, {
  if (isTRUE(input$dyLegend_activate)) {
    rv$graphConfig$graph$dyLegend$labelsSeparateLines <<- input$dyLegend_labelsSeparateLines
  }
})

observeEvent(input$dyEvent_label, {
  if (identical(input$dyEvent_label[2], "")) {
    eventLabel <- NULL
  } else {
    eventLabel <- input$dyEvent_label[2]
  }
  rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_label[1])]]]]$label <<- eventLabel
})
observeEvent(input$dyEvent_labelLoc, {
  rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_labelLoc[1])]]]]$labelLoc <<- input$dyEvent_labelLoc[2]
})
observeEvent(input$dyEvent_color, {
  if (nchar(input$dyEvent_color[2])) {
    rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_color[1])]]]]$color <<- input$dyEvent_color[2]
  } else {
    rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_color[1])]]]]$color <<- NULL
  }
})
observeEvent(input$dyEvent_strokePattern, {
  rv$graphConfig$graph$dyEvent[[idLabelMap$dy_dyEvent[[as.integer(input$dyEvent_strokePattern[1])]]]]$strokePattern <<- input$dyEvent_strokePattern[2]
})

observeEvent(input$dyLimit_label, {
  if (identical(input$dyLimit_label[2], "")) {
    limitLabel <- NULL
  } else {
    limitLabel <- input$dyLimit_label[2]
  }
  rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_label[1])]]]]$label <<- limitLabel
})
observeEvent(input$dyLimit_labelLoc, {
  rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_labelLoc[1])]]]]$labelLoc <<- input$dyLimit_labelLoc[2]
})
observeEvent(input$dyLimit_color, {
  if (nchar(input$dyLimit_color[2])) {
    rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_color[1])]]]]$color <<- input$dyLimit_color[2]
  } else {
    rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_color[1])]]]]$color <<- NULL
  }
})
observeEvent(input$dyLimit_strokePattern, {
  rv$graphConfig$graph$dyLimit[[idLabelMap$dy_dyLimit[[as.integer(input$dyLimit_strokePattern[1])]]]]$strokePattern <<- input$dyLimit_strokePattern[2]
})
observeEvent(input$dyAnnotation_text, {
  if (nchar(input$dyAnnotation_text[2])) {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_text[1])]]]]$text <<- input$dyAnnotation_text[2]
  } else {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_text[1])]]]]$text <<- NULL
  }
})
observeEvent(input$dyAnnotation_tooltip, {
  if (nchar(input$dyAnnotation_tooltip[2])) {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_tooltip[1])]]]]$tooltip <<- input$dyAnnotation_tooltip[2]
  } else {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_tooltip[1])]]]]$tooltip <<- NULL
  }
})
observeEvent(input$dyAnnotation_width, {
  if (input$dyAnnotation_width[2] == 0L) {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_width[1])]]]]$width <<- NULL
  } else {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_width[1])]]]]$width <<- as.numeric(input$dyAnnotation_width[2])
  }
})
observeEvent(input$dyAnnotation_height, {
  if (input$dyAnnotation_width[2] == 0L) {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_height[1])]]]]$height <<- NULL
  } else {
    rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_height[1])]]]]$height <<- as.numeric(input$dyAnnotation_height[2])
  }
})
observeEvent(input$dyAnnotation_attachAtBottom, {
  rv$graphConfig$graph$dyAnnotation[[idLabelMap$dy_dyAnnotation[[as.integer(input$dyAnnotation_attachAtBottom[1])]]]]$attachAtBottom <<- identical(input$dyAnnotation_attachAtBottom[2], 1L)
})
observeEvent(input$dyShading_up, {
  chart_id <- as.integer(input$dyShading_up[1])
  if (is.na(chart_id)) {
    return()
  }
  rv$graphConfig$graph$dyShading[[idLabelMap$dy_dyShading[[chart_id]]]]$to <<- input$dyShading_up[2]
})
observeEvent(input$dyShading_color, {
  chart_id <- as.integer(input$dyShading_color[1])
  if (is.na(chart_id)) {
    return()
  }
  if (nchar(input$dyShading_color[2])) {
    rv$graphConfig$graph$dyShading[[idLabelMap$dy_dyShading[[chart_id]]]]$color <<- input$dyShading_color[2]
  } else {
    rv$graphConfig$graph$dyShading[[idLabelMap$dy_dyShading[[chart_id]]]]$color <<- NULL
  }
})
observeEvent(input$dyShading_axis, {
  chart_id <- as.integer(input$dyShading_axis[1])
  if (is.na(chart_id)) {
    return()
  }
  rv$graphConfig$graph$dyShading[[idLabelMap$dy_dyShading[[chart_id]]]]$axis <<- input$dyShading_axis[2]
})
observeEvent(input$staticPlot, {
  if (isTRUE(input$staticPlot)) {
    rv$graphConfig$graph$staticPlot <<- TRUE
  } else {
    rv$graphConfig$graph$staticPlot <<- FALSE
  }
})
observeEvent(input$paper_bgcolor, {
  if (nchar(input$paper_bgcolor)) {
    rv$graphConfig$graph$paper_bgcolor <<- input$paper_bgcolor
  } else {
    rv$graphConfig$graph$paper_bgcolor <<- NULL
  }
})
observeEvent(input$plot_bgcolor, {
  if (nchar(input$plot_bgcolor)) {
    rv$graphConfig$graph$plot_bgcolor <<- input$plot_bgcolor
  } else {
    rv$graphConfig$graph$plot_bgcolor <<- NULL
  }
})
observeEvent(input$showlegend, {
  rv$graphConfig$graph$showlegend <<- input$showlegend
})
observeEvent(input$fixedHeight, {
  if (!is.numeric(input$fixedHeight) || identical(input$fixedHeight, 0L)) {
    rv$graphConfig$graph$fixedHeight <<- NULL
    return()
  }
  rv$graphConfig$graph$fixedHeight <<- input$fixedHeight
})
observeEvent(input$fixedWidth, {
  if (!is.numeric(input$fixedWidth) || identical(input$fixedWidth, 0L)) {
    rv$graphConfig$graph$fixedWidth <<- NULL
    return()
  }
  rv$graphConfig$graph$fixedWidth <<- input$fixedWidth
})
observeEvent(input$x_title, {
  rv$graphConfig$graph$xaxis$title <<- input$x_title
})
observeEvent(input$x_categoryorder, {
  rv$graphConfig$graph$xaxis$categoryorder <<- input$x_categoryorder
})
observe({
  req(input$chart_tool %in% plotlyChartTools, !identical(input$chart_tool, "pie"))
  if (isFALSE(input$scaleratio_check) ||
    !is.numeric(input$scaleratio) ||
    input$scaleratio < 0.1) {
    rv$graphConfig$graph$yaxis$scaleratio <<- NULL
    rv$graphConfig$graph$yaxis$scaleanchor <<- NULL
    return()
  }
  rv$graphConfig$graph$yaxis$scaleratio <<- input$scaleratio
  rv$graphConfig$graph$yaxis$scaleanchor <<- "x"
})
observeEvent(input$x_showgrid, {
  rv$graphConfig$graph$xaxis$showgrid <<- input$x_showgrid
})
observeEvent(input$x_zeroline, {
  rv$graphConfig$graph$xaxis$zeroline <<- input$x_zeroline
})
observeEvent(input$x_showticklabels, {
  rv$graphConfig$graph$xaxis$showticklabels <<- input$x_showticklabels
})
observeEvent(input$x_rangefrom, {
  val <- NULL
  if (nchar(input$x_rangefrom)) {
    val <- suppressWarnings(as.numeric(input$x_rangefrom))
    if (is.na(val)) {
      val <- input$x_rangefrom
    }
  }
  rv$graphConfig$graph$xaxis$rangefrom <<- val
})
observeEvent(input$x_rangeto, {
  val <- NULL
  if (nchar(input$x_rangeto)) {
    val <- suppressWarnings(as.numeric(input$x_rangeto))
    if (is.na(val)) {
      val <- input$x_rangeto
    }
  }
  rv$graphConfig$graph$xaxis$rangeto <<- val
})
observeEvent(input$y_title, {
  rv$graphConfig$graph$yaxis$title <<- input$y_title
})
observeEvent(input$y_categoryorder, {
  rv$graphConfig$graph$yaxis$categoryorder <<- input$y_categoryorder
})
observeEvent(input$y_showgrid, {
  rv$graphConfig$graph$yaxis$showgrid <<- input$y_showgrid
})
observeEvent(input$y_zeroline, {
  rv$graphConfig$graph$yaxis$zeroline <<- input$y_zeroline
})
observeEvent(input$y_showticklabels, {
  rv$graphConfig$graph$yaxis$showticklabels <<- input$y_showticklabels
})
observeEvent(input$y_rangefrom, {
  val <- NULL
  if (nchar(input$y_rangefrom)) {
    val <- suppressWarnings(as.numeric(input$y_rangefrom))
    if (is.na(val)) {
      val <- input$y_rangefrom
    }
  }
  rv$graphConfig$graph$yaxis$rangefrom <<- val
})
observeEvent(input$y_rangeto, {
  val <- NULL
  if (nchar(input$y_rangeto)) {
    val <- suppressWarnings(as.numeric(input$y_rangeto))
    if (is.na(val)) {
      val <- input$y_rangeto
    }
  }
  rv$graphConfig$graph$yaxis$rangeto <<- val
})
observeEvent(input$y2_title, {
  rv$graphConfig$graph$y2axis$title <<- input$y2_title
})
observeEvent(input$y2_categoryorder, {
  rv$graphConfig$graph$y2axis$categoryorder <<- input$y2_categoryorder
})
observeEvent(input$y2_showgrid, {
  rv$graphConfig$graph$y2axis$showgrid <<- input$y2_showgrid
})
observeEvent(input$y2_zeroline, {
  rv$graphConfig$graph$y2axis$zeroline <<- input$y2_zeroline
})
observeEvent(input$y2_showticklabels, {
  rv$graphConfig$graph$y2axis$showticklabels <<- input$y2_showticklabels
})
observeEvent(input$y2_rangefrom, {
  val <- NULL
  if (length(input$y2_rangefrom) && nchar(input$y2_rangefrom)) {
    val <- suppressWarnings(as.numeric(input$y2_rangefrom))
    if (is.na(val)) {
      val <- input$y2_rangefrom
    }
  }
  rv$graphConfig$graph$y2axis$rangefrom <<- val
})
observeEvent(input$y2_rangeto, {
  val <- NULL
  if (length(input$y2_rangeto) && nchar(input$y2_rangeto)) {
    val <- suppressWarnings(as.numeric(input$y2_rangeto))
    if (is.na(val)) {
      val <- input$y2_rangeto
    }
  }
  rv$graphConfig$graph$y2axis$rangeto <<- val
})
observeEvent(input$hist_label, {
  if (nchar(input$hist_label[[2]])) {
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_label[1])]]]]$labels <<- input$hist_label[2]
  } else {
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_label[1])]]]]$labels <<- NULL
  }
})
observeEvent(input$hist_color, {
  if (nchar(input$hist_color[[2]])) {
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_color[1])]]]]$color <<- input$hist_color[2]
  } else {
    rv$graphConfig$graph$xdata[[idLabelMap$hist_xdata[[as.integer(input$hist_color[1])]]]]$color <<- NULL
  }
})
observeEvent(input$chart_color, {
  if (identical(input$chart_color, "_")) {
    rv$graphConfig$graph$color <<- NULL
    if (input$chart_tool %in% plotlyChartTools) {
      setInputValue(session,
        id = "#chart_ylabel1",
        value = if (isTRUE(configuredWithThisTool) && length(currentGraphConfig[["ydata"]]$value$label)) {
          currentGraphConfig[["ydata"]]$value$label
        } else {
          names(activeSymbol$indices[activeSymbol$indexTypes == "numeric"])[1]
        }
      )
    }
  } else {
    rv$graphConfig$graph$color <<- input$chart_color
    if (input$chart_tool %in% plotlyChartTools) {
      setInputValue(session, id = "#chart_ylabel1", value = "")
    }
  }
})
observeEvent(input$chart_symbol, {
  if (identical(input$chart_symbol, "_")) {
    rv$graphConfig$graph$symbol <<- NULL
  } else {
    rv$graphConfig$graph$symbol <<- input$chart_symbol
  }
})
observeEvent(input$bar_width, {
  if (identical(input$bar_width, "_")) {
    rv$graphConfig$graph$width <<- NULL
  } else {
    rv$graphConfig$graph$width <<- input$bar_width
  }
})
observeEvent(input$bar_orientation, {
  if (identical(input$bar_orientation, "h")) {
    rv$graphConfig$graph$orientation <<- "h"
  } else {
    rv$graphConfig$graph$orientation <<- NULL
  }
})
observeEvent(input$chart_xdata, {
  if (!length(input$x_title)) {
    updateTextInput(session, "x_title", value = names(activeSymbol$indices)[match(
      input$chart_xdata,
      activeSymbol$indices
    )][1])
  }
  rv$graphConfig$graph$xdata <<- input$chart_xdata
})
observeEvent(input$miropivot_enableHideEmptyCols, {
  if (isTRUE(input$miropivot_enableHideEmptyCols)) {
    showEl(session, "#preview_output_miropivot-miroPivot-hideEmptyCols")
  } else {
    updateCheckboxInput(session, "preview_output_miropivot-miroPivot-hideEmptyCols", value = FALSE)
    hideEl(session, "#preview_output_miropivot-miroPivot-hideEmptyCols")
  }
})
observeEvent(input$miropivot_emptyUEL, {
  if (identical(input$miropivot_emptyUEL, "")) {
    rv$graphConfig$graph$options$emptyUEL <- NULL
  } else {
    rv$graphConfig$graph$options$emptyUEL <- input$miropivot_emptyUEL
  }
})
observe({
  if (!isTRUE(input$miropivot_useExternalDefaultView) || !length(input$miropivot_externalDefaultView)) {
    rv$graphConfig$graph$options$externalDefaultView <- NULL
    return()
  }
  externalViewTmp <- trimws(input$miropivot_externalDefaultView)
  if (identical(externalViewTmp, "")) {
    rv$graphConfig$graph$options$externalDefaultView <- NULL
  } else {
    rv$graphConfig$graph$options$externalDefaultView <- externalViewTmp
  }
})
observeEvent(input$miropivot_hidePivotControls, {
  if (isTRUE(input$miropivot_hidePivotControls)) {
    rv$graphConfig$graph$options$hidePivotControls <- TRUE
  } else {
    rv$graphConfig$graph$options$hidePivotControls <- NULL
  }
})
observeEvent(input$miropivot_fixedColumns, {
  if (isFALSE(input$miropivot_fixedColumns)) {
    rv$graphConfig$graph$options$fixedColumns <- FALSE
  } else {
    rv$graphConfig$graph$options$fixedColumns <- NULL
  }
})
observeEvent(input$add_array_el, {
  chart_id <- input$add_array_el[1]
  chart_label <- input$add_array_el[2]
  el_id <- input$add_array_el[3]
  if (el_id %in% c("dy_dyShading", "dy_dyLimit", "leaflet_markers", "leaflet_flows", "leaflet_minicharts", "timevis_series", "timevis_custom")) {
    # ID is number instead of string as string is not unique
    chart_label <- chart_id
  }
  chart_id <- as.integer(chart_id)
  JSON_id <- gsub("^[^_]*_", "", el_id)
  if (is.na(chart_id) || !nchar(chart_label)) {
    return()
  }
  # label didnt change
  if (length(idLabelMap[[el_id]]) >= chart_id &&
    identical(idLabelMap[[el_id]][[chart_id]], chart_label)) {
    if (identical(el_id, "dy_dyShading")) {
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$from <- input$add_array_el[2]
    } else if (identical(el_id, "dy_dyLimit")) {
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$limit <- input$add_array_el[2]
    } else if (identical(el_id, "leaflet_markers")) {
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$lat <- input$add_array_el[2]
    } else if (identical(el_id, "leaflet_flows")) {
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$lat0 <- input$add_array_el[2]
    } else if (identical(el_id, "leaflet_minicharts")) {
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$lat <- input$add_array_el[2]
    } else if (identical(el_id, "timevis_series")) {
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$content <- input$add_array_el[2]
    } else if (identical(el_id, "timevis_custom")) {
      rv$graphConfig$graph[[JSON_id]][[chart_label]]$time <- input$add_array_el[2]
    } else {
      return()
    }
  }

  # if same label already exists, remove it
  currentContent <- NULL
  if (chart_id <= length(idLabelMap[[el_id]])) {
    labelID <- idLabelMap[[el_id]][[chart_id]]
    if (sum(labelID == idLabelMap[[el_id]]) < 1.5) {
      if (length(input$add_array_el) > 3L) {
        # when label is merely changed not added, we must preserve the previous config
        currentContent <- rv$graphConfig$graph[[JSON_id]][[labelID]]
        # change labels
        if (identical(el_id, "chart_ydata")) {
          currentContent$label <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
        } else if (identical(el_id, "hist_xdata")) {
          currentContent$labels <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
        } else if (identical(el_id, "dy_dyAnnotation")) {
          currentContent$text <- configScalars[[2]][match(chart_label, configScalars[[1]])][1]
        }
      }
      rv$graphConfig$graph[[JSON_id]][labelID] <<- NULL
    }
  }
  idLabelMap[[el_id]][[chart_id]] <<- chart_label
  if (length(currentContent)) {
    rv$graphConfig$graph[[JSON_id]][[chart_label]] <<- currentContent
    return()
  }
  if (identical(el_id, "chart_ydata")) {
    label <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
    if (input$chart_tool %in% plotlyChartTools) {
      if (identical(input$chart_tool, "scatter")) {
        axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] + 1L
        updateYAxes()
        newContent <- list(
          label = label,
          mode = "markers",
          fill = "none",
          marker = list(
            opacity = 1L,
            size = 6L,
            line = list(width = 0L)
          ),
          showlegend = TRUE,
          yaxis = "y"
        )
      } else if (identical(input$chart_tool, "bubble")) {
        newContent <- list(
          label = label,
          mode = "markers",
          marker = list(
            symbol = "circle",
            opacity = 1L,
            size = label,
            sizemode = "area",
            color = label,
            line = list(width = 0L)
          ),
          showlegend = TRUE
        )
      } else if (identical(input$chart_tool, "line")) {
        axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] + 1L
        updateYAxes()
        newContent <- list(
          label = label,
          mode = "lines",
          line = list(
            width = 2L,
            shape = "linear",
            dash = "solid"
          ),
          showlegend = TRUE,
          yaxis = "y"
        )
      } else {
        newContent <- list(
          label = label,
          mode = "lines",
          marker = list(line = list(width = 0L))
        )
      }
    } else {
      axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] + 1L
      updateYAxes()
      newContent <- list(
        label = label,
        stemPlot = FALSE, stepPlot = FALSE,
        fillGraph = FALSE, drawPoints = FALSE,
        pointShape = "dot",
        pointSize = 2L,
        yaxis = "y"
      )
    }
  } else if (identical(el_id, "hist_xdata")) {
    label <- names(activeSymbol$indices)[match(chart_label, activeSymbol$indices)][1]
    newContent <- list(labels = label, color = "#000000")
  } else if (identical(el_id, "dy_dyEvent")) {
    newContent <- list(labelLoc = "top", color = "rgb(0,0,0)", strokePattern = "dashed")
  } else if (identical(el_id, "dy_dyLimit")) {
    newContent <- list(
      limit = input$add_array_el[2],
      labelLoc = "left",
      color = "rgb(0,0,0)",
      strokePattern = "dashed"
    )
  } else if (identical(el_id, "dy_dyAnnotation")) {
    newContent <- list(text = configScalars[[2]][1], attachAtBottom = FALSE)
  } else if (identical(el_id, "dy_dyShading")) {
    newContent <- list(
      from = input$add_array_el[2],
      to = input$add_array_el[2],
      color = "#efefef",
      axis = "x"
    )
  } else if (identical(el_id, "leaflet_markers")) {
    newContent <- list(
      lng = input$add_array_el[2],
      lat = input$add_array_el[2],
      iconOptions = list(
        icon = "circle",
        iconColor = "#000000",
        markerColor = "blue"
      ),
      labelOptions = list(textsize = "12px", permanent = FALSE)
    )
  } else if (identical(el_id, "leaflet_flows")) {
    newContent <- list(
      lng0 = input$add_array_el[2],
      lat0 = input$add_array_el[2],
      lng1 = input$add_array_el[2],
      lat1 = input$add_array_el[2],
      flow = input$add_array_el[2],
      color = "#0000ff",
      minThickness = 1,
      maxThickness = 20
    )
  } else if (identical(el_id, "leaflet_minicharts")) {
    newContent <- list(
      lng = input$add_array_el[2],
      lat = input$add_array_el[2],
      chartdata = input$add_array_el[2][1],
      type = "auto",
      width = 30,
      height = 30,
      opacity = 1,
      showLabels = FALSE,
      transitionTime = 750,
      legend = TRUE,
      legendPosition = "topright"
    )
  } else if (identical(el_id, "timevis_series")) {
    newContent <- list(
      content = input$add_array_el[2],
      start = input$add_array_el[2],
      type = "box"
    )
  } else if (identical(el_id, "timevis_custom")) {
    newContent <- list(time = input$add_array_el[2])
  } else {
    newContent <- NULL
  }
  rv$graphConfig$graph[[JSON_id]][[chart_label]] <<- newContent
})
observeEvent(input$remove_array_el, {
  array_id <- input$remove_array_el[1]
  el_id <- as.integer(input$remove_array_el[3])
  JSON_id <- gsub("^[^_]*_", "", array_id)
  if (startsWith(JSON_id, "ydata")) {
    array_id <- "chart_ydata"
    JSON_id <- "ydata"
  }
  chart_label <- idLabelMap[[array_id]][[el_id]]
  if (length(rv$graphConfig$graph[[JSON_id]][[chart_label]]$yaxis) ||
    length(rv$graphConfig$graph[[JSON_id]][[chart_label]]$axis)) {
    if (identical(rv$graphConfig$graph[[JSON_id]][[chart_label]]$yaxis, "y")) {
      axisOptionsGlobal[["y"]] <<- axisOptionsGlobal[["y"]] - 1L
    }
    if (identical(rv$graphConfig$graph[[JSON_id]][[chart_label]]$yaxis, "y2")) {
      axisOptionsGlobal[["y2"]] <<- axisOptionsGlobal[["y2"]] - 1L
    }
    updateYAxes()
  }
  if (identical(array_id, "leaflet_markers") && length(rv$graphConfig$graph[[JSON_id]][[chart_label]]$group)) {
    leafletGroups$update(old = rv$graphConfig$graph[[JSON_id]][[chart_label]]$group, new = NULL)
    rv$updateLeafletGroups <- rv$updateLeafletGroups + 1L
  }
  if (sum(input$remove_array_el[2] == chart_label) < 1.5) {
    rv$graphConfig$graph[[JSON_id]][chart_label] <- NULL
    if (!length(rv$graphConfig$graph[[JSON_id]])) {
      rv$graphConfig$graph[[JSON_id]] <- NULL
    }
  }
  idLabelMap[[array_id]][el_id] <<- "_NULL"
})
observeEvent(input$chart_ylabel, {
  tryCatch(
    {
      labelID <- idLabelMap$chart_ydata[[as.integer(input$chart_ylabel[1])]]
      if (nchar(input$chart_ylabel[2])) {
        label <- input$chart_ylabel[2]
      } else {
        label <- NULL
      }
      rv$graphConfig$graph$ydata[[labelID]]$label <<- label
    },
    error = function(e) {
      flog.info(
        "Could not change label for y-data. Error message: '%s'.",
        conditionMessage(e)
      )
    }
  )
})
observeEvent(input$bar_mode, {
  rv$graphConfig$graph$barmode <<- input$bar_mode
})

observeEvent(input$customExternalSymbol, {
  if (skipLoadRenderer) {
    skipLoadRenderer <<- FALSE
    return()
  }
  rendererFromExternalSymbol <<- FALSE
  if (identical(input$customExternalSymbol, "-")) {
    rendererFunctionName <- paste0("mirorenderer_", activeSymbolName)
    rendererToLoad <- rendererFunctionName
  } else {
    rendererFunctionName <- input$customExternalSymbol
    currentRendererSymname <- gsub("^mirorenderer\\_", "", rendererFunctionName)
    if (currentRendererSymname %in% c(
      unname(inputSymMultiDimChoices),
      unname(outputSymMultiDimChoices)
    )) {
      # renderer uses code from other (existent) symbol
      rendererFromExternalSymbol <<- TRUE
      rendererToLoad <- rendererFunctionName
    } else {
      # renderer from nonexistent symbol
      rendererToLoad <- rendererFunctionName
      rendererFunctionName <- paste0("mirorenderer_", tolower(activeSymbol$name))
    }
  }
  rv$graphConfig$outType <- rendererFunctionName
  rendererFunctions <- list(output = "", renderer = "")
  tryCatch(
    {
      rendererFunctions <- getRendererFunctions(rendererToLoad)
      updateAceEditor(session, "customOutputFunction", value = rendererFunctions$output)
      updateAceEditor(session, "customRenderFunction", value = rendererFunctions$renderer)
      session$sendCustomMessage("gms-shinySetInputValue", list(
        id = "btUpdateCustomRendererOutput",
        val = 1L,
        timeout = 500L
      ))
    },
    error = function(e) {
      flog.warn(
        "Problems loading custom renderer. Error message: %s",
        conditionMessage(e)
      )
    }
  )
})
observeEvent(input$customPackages,
  {
    if (identical(input$customPackages, rv$graphConfig$packages)) {
      return()
    }
    rv$graphConfig$packages <<- input$customPackages
    showEl(session, "#customPackagesModified")
  },
  ignoreNULL = FALSE
)
observeEvent(input$customAdditionalData,
  {
    if (activeSymbol$id > length(modelIn)) {
      rv$graphConfig$additionalData <<- input$customAdditionalData
    } else {
      rv$graphConfig$additionalData <<- NULL
    }
  },
  ignoreNULL = FALSE
)
observeEvent(input$cutomOptions, {
  tryCatch(
    {
      rv$graphConfig$options <- safeFromJSON(input$cutomOptions,
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE
      )
      invalidCustomRender <<- FALSE
    },
    error = function(e) {
      flog.debug("Invalid JSON (custom renderer options). Error message: %s", conditionMessage(e))
      invalidCustomRender <<- TRUE
    }
  )
})

observeEvent(input$filter_dim, {
  if (input$chart_tool %in% plotlyChartTools) {
    chartToolTmp <- "plotly"
  } else {
    chartToolTmp <- input$chart_tool
  }
  if (isFALSE(input$filter_dim)) {
    rv$graphConfig$graph$filter <<- NULL
    hideEl(session, paste0("#preview_output_", chartToolTmp, "-data_filter"))
  } else {
    rv$graphConfig$graph$filter <<- list(
      col = input$filter_col,
      label = input$filter_label,
      multiple = input$filter_multiple,
      date = FALSE
    )
    showEl(session, paste0("#preview_output_", chartToolTmp, "-data_filter"))
  }
})
observeEvent(input$filter_col, {
  req(isTRUE(input$filter_dim))
  rv$graphConfig$graph$filter$col <<- input$filter_col
})
observeEvent(input$filter_label, {
  req(isTRUE(input$filter_dim))
  newLabel <- ""
  if (nchar(input$filter_label)) {
    rv$graphConfig$graph$filter$label <<- input$filter_label
    newLabel <- input$filter_label
  } else {
    rv$graphConfig$graph$filter$label <<- NULL
  }
  updateSelectInput(session, paste0("#preview_output_", input$chart_tool, "-data_filter"),
    label = newLabel
  )
})
observeEvent(input$filter_multiple, {
  req(isTRUE(input$filter_dim))
  rv$graphConfig$graph$filter$multiple <<- input$filter_multiple
})
observeEvent(input$filter_date, {
  req(isTRUE(input$filter_dim))
  rv$graphConfig$graph$filter$date <<- input$filter_date
})
observe({
  req(identical(rv$graphConfig$graph$tool, "valuebox"), input$valueBoxes)

  scalarNames <- modelOut[[scalarsOutName]]$symnames[!tolower(modelOut[[scalarsOutName]]$symnames)
  %in% tolower(configJSON$hiddenOutputScalars)]
  boxOptionsTmp <- lapply(seq_along(input$valueBoxes), function(rowId) {
    if (!length(input$valueBoxes[[rowId]])) {
      return(NA)
    }
    scalarIds <- vapply(strsplit(input$valueBoxes[[rowId]], "_", fixed = TRUE), function(boxId) {
      as.integer(boxId[2])
    }, integer(1L), USE.NAMES = FALSE)
    ret <- lapply(scalarIds, function(i) {
      list(
        description = input[[paste0("valueBoxDesc_", i)]],
        color = input[[paste0("valueBoxColor_", i)]],
        icon = list(name = input[[paste0("valueBoxIcon_", i)]], lib = "font-awesome"),
        round = if (!is.na(input[[paste0("valueBoxRound_", i)]])) input[[paste0("valueBoxRound_", i)]] else 0L
      )
    })
    names(ret) <- scalarNames[scalarIds]
    return(ret)
  })
  rv$graphConfig$options <- boxOptionsTmp[!is.na(boxOptionsTmp)]
})

getCurrentGraphConfig <- function() {
  currentGraphConfig <<- NULL
  chartId <- match(activeSymbolName, tolower(names(configJSON$dataRendering)))[1]
  if (!is.na(chartId) && length(configJSON$dataRendering[[chartId]]$label) &&
    !identical(trimws(configJSON$dataRendering[[chartId]]$label), "")) {
    currentGraphLabel <<- configJSON$dataRendering[[chartId]]$label
  } else {
    currentGraphLabel <<- ""
  }
  if (identical(input$gams_symbols, scalarsOutName)) {
    currentGraphConfig <<- configJSON$dataRendering[[chartId]][["options"]]
  } else {
    if (!is.na(chartId)) {
      if (length(configJSON$dataRendering[[chartId]][["pivottable"]])) {
        currentGraphConfig <<- configJSON$dataRendering[[chartId]][["pivottable"]]
      } else if (length(configJSON$dataRendering[[chartId]][["graph"]])) {
        currentGraphConfig <<- configJSON$dataRendering[[chartId]][["graph"]]
      } else {
        currentGraphConfig <<- configJSON$dataRendering[[chartId]]
      }
    }
  }
  if (length(currentGraphConfig)) {
    isInJSON <<- TRUE
  } else {
    isInJSON <<- FALSE
  }
}
observeEvent(input$gams_symbols, {
  req(input$gams_symbols)
  symbolID <- match(isolate(input$gams_symbols), names(modelIn))
  if (is.na(symbolID)) {
    symbolID <- match(isolate(input$gams_symbols), names(modelOut)) + length(modelIn)
  }
  # destroy old observers
  for (el in ls(envir = customRendererEnv)) {
    if ("Observer" %in% class(customRendererEnv[[el]])) {
      customRendererEnv[[el]]$destroy()
    }
  }
  updateTextInput(session, "preview_output_miropivot-miroPivot-symbol_name", value = input$gams_symbols)
  updateTextInput(session, "preview_output_custom-symbol_name", value = input$gams_symbols)
  changeActiveSymbol(symbolID)
  newChartTool <<- character(0L)
  if (identical(input$gams_symbols, scalarsFileName)) {
    removeUI(selector = "#tool_options div", multiple = TRUE)
    hideEl(session, ".preview-outer-wrapper")
    insertUI(
      selector = "#tool_options", tags$div(
        class = "config-no-hide",
        paste0(
          lang$adminMode$graphs$errMsg$scalarConfig,
          lang$adminMode$uiR$widgets, "'!"
        )
      ),
      where = "afterBegin"
    )

    disableEl(session, "#saveGraph")
    disableEl(session, "#deleteGraph")
    return()
  } else {
    showEl(session, ".preview-outer-wrapper")
    enableEl(session, "#saveGraph")
    enableEl(session, "#deleteGraph")
  }
  if (identical(input$gams_symbols, scalarsOutName)) {
    updateSelectInput(session, "chart_tool", choices = setNames(
      c("valuebox"),
      lang$adminMode$graphs$updateToolScalars
    ))
    newChartTool <<- "valuebox"
  } else {
    graphType <- NULL
    chartId <- match(activeSymbolName, tolower(names(configJSON$dataRendering)))[1]
    if (!is.na(chartId)) {
      if (length(configJSON$dataRendering[[chartId]][["pivottable"]])) {
        graphType <- "pivot"
      } else if (identical(configJSON$dataRendering[[chartId]]$outType, "miroPivot")) {
        graphType <- "miropivot"
      } else if (identical(configJSON$dataRendering[[chartId]]$outType, "datatable")) {
        graphType <- "datatable"
      } else if (!length(configJSON$dataRendering[[chartId]][["graph"]])) {
        graphType <- "custom"
      }
    }
    indices <- activeSymbol$indices
    scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
    # distinguish between plotly chart or other
    if (length(currentGraphConfig[["type"]])) {
      if (identical(currentGraphConfig[["type"]], "scatter")) {
        # distinguish between line chart and scatter plot
        lineTraces <- vapply(scalarIndices, function(scalarIndex) {
          return(identical(currentGraphConfig$ydata[[scalarIndex]][["mode"]], "lines"))
        }, logical(1L), USE.NAMES = FALSE)
        if (any(lineTraces)) {
          newChartTool <<- "line"
        } else {
          newChartTool <<- "scatter"
        }
      } else {
        newChartTool <<- currentGraphConfig[["type"]]
      }
    } else if (length(currentGraphConfig[["tool"]])) {
      newChartTool <<- currentGraphConfig[["tool"]]
    } else if (identical(graphType, "miropivot")) {
      newChartTool <<- "miropivot"
    } else if (identical(graphType, "pivot")) {
      newChartTool <<- "pivot"
    } else if (identical(graphType, "custom")) {
      newChartTool <<- "custom"
    } else if (identical(graphType, "datatable")) {
      newChartTool <<- "datatable"
    } else {
      if (identical(modelInRaw[[activeSymbolName]]$symtype, "set")) {
        newChartTool <<- "miropivot"
      } else {
        newChartTool <<- "pie"
      }
    }
    if (identical(modelInRaw[[activeSymbolName]]$symtype, "set")) {
      graphOptions <- langSpecificGraphs$graphOptionsSet
    } else {
      graphOptions <- langSpecificGraphs$graphOptionsNoScalars
    }
    updateSelectInput(session, "chart_tool", choices = graphOptions, selected = newChartTool)
    if (identical(newChartTool, "datatable")) {
      # tableSymbol identifier needed when configuration is deleted
      tableSymbol <<- TRUE
      # manual refresh options since updateSelectinput will not trigger an event when selecting (not available choice) 'datatable'
      rv$refreshOptions <- rv$refreshOptions + 1L
      disableEl(session, "#saveGraph")
      showEl(session, "#deleteGraph")
      return()
    } else {
      tableSymbol <<- FALSE
    }
    if (identical(newChartTool, input$chart_tool)) {
      rv$refreshOptions <- rv$refreshOptions + 1L
    }
  }
  if (tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))) {
    showEl(session, "#deleteGraph")
  } else {
    hideEl(session, "#deleteGraph")
  }
})
observeEvent(
  {
    input$chart_tool
    rv$refreshOptions
  },
  {
    req(rv$initData)
    allDataAvailable <<- FALSE
    if (identical(newChartTool, "datatable")) {
      chartTool <- "datatable"
      newChartTool <<- character(0L)
    } else {
      chartTool <- input$chart_tool
      enableEl(session, "#saveGraph")
    }
    # check whether symbol is already configured (in JSON file). Check for identical(newChartTool, chartTool)
    # not sufficient since newChartTool = pie is the default for non-configured symbols
    if (isTRUE(isInJSON) && identical(newChartTool, chartTool)) {
      configuredWithThisTool <<- TRUE
    } else {
      configuredWithThisTool <<- FALSE
    }
    if (!identical(chartTool, "pivot")) {
      rv$graphConfig$pivottable <<- NULL
    }
    if (!identical(chartTool, "leaflet")) {
      rv$graphConfig$graph$layersControl <<- NULL
    }
    if (!identical(chartTool, "valuebox")) {
      rv$graphConfig$options <<- NULL
    }
    if (!identical(chartTool, "custom")) {
      rv$graphConfig$additionalData <<- NULL
      rv$graphConfig$packages <<- NULL
    }
    saveAndReload(chartTool)
    hideFilter()
    removeUI(selector = "#tool_options div", multiple = TRUE)
    if (chartTool %in% plotlyChartTools) {
      rv$graphConfig$graph$tool <<- "plotly"
    } else {
      rv$graphConfig$graph$tool <<- chartTool
    }
    removeClassEl(session, ".category-btn", "category-btn-active")
    hideEl(session, ".category-btn")
    ydataTmp <- currentGraphConfig[["ydata"]]
    axisOptionsGlobal <<- list(y = 1, y2 = 0)
    if (identical(chartTool, "pie")) {
      rv$graphConfig$graph$type <<- "pie"
      showEl(session, ".category-btn-pie")
      addClassEl(session, id = "#categoryPie1", "category-btn-active")
      if (!isTRUE(configuredWithThisTool)) {
        idLabelMap[["chart_piedata"]] <<- list()
      }
      insertUI(selector = "#tool_options", tagList(
        if (!activeSymbol$isInput) {
          textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
            value = currentGraphLabel
          )
        }, getPieOptions()
      ), where = "beforeEnd")
      # check whether symbol is configured as pie (in JSON)
      tracesTmp <- currentGraphConfig[["traces"]]
      if (isTRUE(configuredWithThisTool) && length(tracesTmp)) {
        if (is.null(names(tracesTmp))) {
          names(tracesTmp) <- seq_along(tracesTmp)
        }
        addArrayEl(session, "chart_piedata",
          defaults = tracesTmp
        )
        idLabelMap[["chart_piedata"]] <<- as.list(names(tracesTmp))
        rv$graphConfig$graph$traces <- tracesTmp
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "bar")) {
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "bar"
      showEl(session, ".category-btn-bar")
      addClassEl(session, id = "#categoryBar1", "category-btn-active")
      if (!isTRUE(configuredWithThisTool)) {
        idLabelMap[["chart_ydata"]] <<- list()
      }
      insertUI(selector = "#tool_options", tagList(
        if (!activeSymbol$isInput) {
          textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
            value = currentGraphLabel
          )
        }, getBarOptions()
      ), where = "beforeEnd")
      # check whether symbol is configured as bar chart (in JSON)
      if (isTRUE(configuredWithThisTool) && length(ydataTmp)) {
        addArrayEl(session, "chart_ydatabar",
          defaults = ydataTmp
        )
        idLabelMap[["chart_ydata"]] <<- as.list(names(ydataTmp))
        rv$graphConfig$graph$ydata <<- ydataTmp
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "scatter")) {
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "scatter"
      showEl(session, ".category-btn-scatter")
      addClassEl(session, id = "#categoryScatter1", "category-btn-active")
      # check whether symbol is configured as scatter plot (in JSON) and if so count y & y2 axes
      indices <- activeSymbol$indices
      scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
      if (isTRUE(configuredWithThisTool) && length(ydataTmp)) {
        noY2axis <- vapply(ydataTmp, function(el) {
          identical(el$yaxis, "y2")
        }, logical(1L), USE.NAMES = FALSE)
        noY2axis <- sum(noY2axis)
        noYaxis <- length(ydataTmp) - noY2axis
        axisOptionsGlobal <<- list(y = noYaxis, y2 = noY2axis)
      } else {
        idLabelMap[["chart_ydata"]] <<- list()
      }
      insertUI(selector = "#tool_options", tagList(
        if (!activeSymbol$isInput) {
          textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
            value = currentGraphLabel
          )
        }, getScatterOptions()
      ), where = "beforeEnd")
      # set configured defaults
      if (isTRUE(configuredWithThisTool) && length(ydataTmp)) {
        addArrayEl(session, "chart_ydatascatter",
          defaults = ydataTmp
        )
        idLabelMap[["chart_ydata"]] <<- as.list(names(ydataTmp))
        rv$graphConfig$graph$ydata <<- ydataTmp
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "line")) {
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "scatter"
      showEl(session, ".category-btn-line")
      addClassEl(session, id = "#categoryLine1", "category-btn-active")
      # check whether symbol is configured as scatter plot (in JSON) and if so count y & y2 axes
      indices <- activeSymbol$indices
      scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
      if (isTRUE(configuredWithThisTool) && length(ydataTmp)) {
        noY2axis <- vapply(ydataTmp, function(el) {
          identical(el$yaxis, "y2")
        }, logical(1L), USE.NAMES = FALSE)
        noY2axis <- sum(noY2axis)
        noYaxis <- length(ydataTmp) - noY2axis
        axisOptionsGlobal <<- list(y = noYaxis, y2 = noY2axis)
      } else {
        idLabelMap[["chart_ydata"]] <<- list()
      }
      insertUI(selector = "#tool_options", tagList(
        if (!activeSymbol$isInput) {
          textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
            value = currentGraphLabel
          )
        }, getLineOptions()
      ), where = "beforeEnd")
      # set configured defaults
      if (isTRUE(configuredWithThisTool) && length(ydataTmp)) {
        addArrayEl(session, "chart_ydataline",
          defaults = ydataTmp
        )
        idLabelMap[["chart_ydata"]] <<- as.list(names(ydataTmp))
        rv$graphConfig$graph$ydata <<- ydataTmp
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "bubble")) {
      rv$resetRE <- rv$resetRE + 1L
      rv$graphConfig$graph$type <<- "bubble"
      showEl(session, ".category-btn-bubble")
      addClassEl(session, id = "#categoryBubble1", "category-btn-active")
      if (!isTRUE(configuredWithThisTool)) {
        idLabelMap[["chart_ydata"]] <<- list()
      }
      insertUI(selector = "#tool_options", tagList(
        if (!activeSymbol$isInput) {
          textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
            value = currentGraphLabel
          )
        }, getBubbleOptions()
      ), where = "beforeEnd")
      # set configured defaults
      indices <- activeSymbol$indices
      scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
      if (isTRUE(configuredWithThisTool) && length(ydataTmp)) {
        # in JSON, element 'color' is used both for dynamic and static values
        if (length(configScalars)) {
          ydataJS <- lapply(ydataTmp, function(ydataEl) {
            ydataEl$staticColor <- !tolower(ydataEl$marker$color) %in% tolower(scalarIndices)
            return(ydataEl)
          })
        } else {
          ydataJS <- ydataTmp
        }
        addArrayEl(session, "chart_ydatabubble",
          defaults = ydataJS
        )
        idLabelMap[["chart_ydata"]] <<- as.list(names(ydataTmp))
        rv$graphConfig$graph$ydata <<- ydataTmp
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "hist")) {
      rv$graphConfig$graph$type <<- "hist"
      showEl(session, ".category-btn-hist")
      addClassEl(session, id = "#categoryHist1", "category-btn-active")
      if (!isTRUE(configuredWithThisTool)) {
        idLabelMap[["hist_xdata"]] <<- list()
      }
      insertUI(selector = "#tool_options", tagList(
        if (!activeSymbol$isInput) {
          textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
            value = currentGraphLabel
          )
        }, getHistOptions()
      ), where = "beforeEnd")
      # set configured defaults
      xdataTmp <- currentGraphConfig[["xdata"]]
      if (isTRUE(configuredWithThisTool) && length(xdataTmp)) {
        addArrayEl(session, "hist_xdata",
          defaults = xdataTmp
        )
        idLabelMap[["hist_xdata"]] <<- as.list(names(xdataTmp))
        rv$graphConfig$graph$xdata <<- xdataTmp
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "dygraphs")) {
      showEl(session, ".category-btn-dygraphs")
      addClassEl(session, id = "#categoryDygraphs1", "category-btn-active")
      if (isTRUE(configuredWithThisTool) && length(ydataTmp)) {
        noY2axis <- vapply(ydataTmp, function(el) {
          identical(el$yaxis, "y2")
        }, logical(1L), USE.NAMES = FALSE)
        noY2axis <- sum(noY2axis)
        noYaxis <- length(ydataTmp) - noY2axis
        axisOptionsGlobal <<- list(y = noYaxis, y2 = noY2axis)
      } else {
        idLabelMap[["chart_ydata"]] <<- list()
        idLabelMap[["dy_dyEvent"]] <<- list()
        idLabelMap[["dy_dyLimit"]] <<- list()
        idLabelMap[["dy_dyAnnotation"]] <<- list()
        idLabelMap[["dy_dyShading"]] <<- list()
      }
      insertUI(
        selector = "#tool_options",
        tags$div(id = "dygraph_options", tagList(
          if (!activeSymbol$isInput) {
            textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
              value = currentGraphLabel
            )
          }, getDygraphsOptions()
        )), where = "beforeEnd"
      )
      if (isTRUE(configuredWithThisTool)) {
        dyEventTmp <- currentGraphConfig[["dyEvent"]]
        dyLimitTmp <- currentGraphConfig[["dyLimit"]]
        dyAnnotationTmp <- currentGraphConfig[["dyAnnotation"]]
        dyShadingTmp <- currentGraphConfig[["dyShading"]]
        if (length(ydataTmp)) {
          addArrayEl(session, "dy_ydata",
            defaults = ydataTmp
          )
          idLabelMap[["chart_ydata"]] <<- as.list(names(ydataTmp))
          rv$graphConfig$graph$ydata <<- ydataTmp
        }
        if (length(dyEventTmp)) {
          # in JSON, object key for event is used both for dynamic (gams scalar) and static (textinput) values
          if (length(configScalars)) {
            dyEventJS <- dyEventTmp
            lapply(seq_along(dyEventTmp), function(eventEl) {
              dyEventJS[[eventEl]]$staticEvent <<- !tolower(names(dyEventTmp)[[eventEl]]) %in% tolower(configScalars[[1]])
            })
          } else {
            dyEventJS <- dyLimitTmp
          }
          addArrayEl(session, "dy_dyEvent",
            defaults = dyEventJS
          )
          idLabelMap[["dy_dyEvent"]] <<- as.list(names(dyEventTmp))
          rv$graphConfig$graph$dyEvent <- dyEventTmp
        }
        if (length(dyLimitTmp)) {
          # in JSON, element 'limit' is used both for dynamic (gams scalar) and static (textinput) values
          if (length(configScalars)) {
            dyLimitJS <- lapply(dyLimitTmp, function(limitEl) {
              limitEl$staticLimit <- !tolower(limitEl$limit) %in% tolower(configScalars[[1]])
              return(limitEl)
            })
          } else {
            dyLimitJS <- dyLimitTmp
          }
          addArrayEl(session, "dy_dyLimit",
            defaults = dyLimitJS
          )
          idLabelMap[["dy_dyLimit"]] <<- as.list(names(dyLimitTmp))
          rv$graphConfig$graph$dyLimit <- dyLimitTmp
        }
        if (length(dyAnnotationTmp)) {
          addArrayEl(session, "dy_dyAnnotation",
            defaults = dyAnnotationTmp
          )
          idLabelMap[["dy_dyAnnotation"]] <<- as.list(names(dyAnnotationTmp))
          rv$graphConfig$graph$dyAnnotation <- dyAnnotationTmp
        }
        if (length(dyShadingTmp)) {
          # in JSON, elements 'from' and 'to' are used both for dynamic (gams scalar) and static (textinput) values
          if (length(configScalars)) {
            dyShadingJS <- lapply(dyShadingTmp, function(shadingEl) {
              shadingEl$staticFrom <- !tolower(shadingEl$from) %in% tolower(configScalars[[1]])
              shadingEl$staticTo <- !tolower(shadingEl$to) %in% tolower(configScalars[[1]])
              return(shadingEl)
            })
          } else {
            dyShadingJS <- dyShadingTmp
          }
          addArrayEl(session, "dy_dyShading",
            defaults = dyShadingJS
          )
          idLabelMap[["dy_dyShading"]] <<- as.list(names(dyShadingTmp))
          rv$graphConfig$graph$dyShading <- dyShadingTmp
        }
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "leaflet")) {
      leafletGroups$reset()
      showEl(session, ".category-btn-leaflet")
      addClassEl(session, id = "#categoryLeaflet1", "category-btn-active")
      if (isTRUE(configuredWithThisTool)) {
        markersTmp <- currentGraphConfig[["markers"]]
        if (length(markersTmp)) {
          for (markerTmp in markersTmp) {
            if (length(markerTmp$group)) {
              leafletGroups$update(old = NULL, new = markerTmp$group)
            }
          }
          rv$updateLeafletGroups <- rv$updateLeafletGroups + 1L
        }
      } else {
        idLabelMap[["leaflet_markers"]] <<- list()
        idLabelMap[["leaflet_flows"]] <<- list()
        idLabelMap[["leaflet_minicharts"]] <<- list()
      }
      insertUI(
        selector = "#tool_options",
        tags$div(id = "leaflet_options", tagList(
          if (!activeSymbol$isInput) {
            textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
              value = currentGraphLabel
            )
          }, getLeafletOptions()
        )), where = "beforeEnd"
      )
      if (isTRUE(configuredWithThisTool)) {
        markersTmp <- currentGraphConfig[["markers"]]
        flowsTmp <- currentGraphConfig[["flows"]]
        minichartsTmp <- currentGraphConfig[["minicharts"]]
        if (length(markersTmp)) {
          addArrayEl(session, "leaflet_markers", defaults = markersTmp)
          idLabelMap[["leaflet_markers"]] <<- as.list(names(markersTmp))
          rv$graphConfig$graph$markers <- markersTmp
        }
        if (length(flowsTmp)) {
          addArrayEl(session, "leaflet_flows", defaults = flowsTmp)
          idLabelMap[["leaflet_flows"]] <<- as.list(names(flowsTmp))
          rv$graphConfig$graph$flows <- flowsTmp
        }
        if (length(minichartsTmp)) {
          addArrayEl(session, "leaflet_minicharts", defaults = minichartsTmp)
          idLabelMap[["leaflet_minicharts"]] <<- as.list(names(minichartsTmp))
          rv$graphConfig$graph$minicharts <- minichartsTmp
        }
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "timevis")) {
      showEl(session, ".category-btn-timevis")
      addClassEl(session, id = "#categoryTimevis1", "category-btn-active")
      if (!isTRUE(configuredWithThisTool)) {
        idLabelMap[["timevis_series"]] <<- list()
        idLabelMap[["timevis_custom"]] <<- list()
      }
      insertUI(
        selector = "#tool_options",
        tags$div(id = "timevis_options", tagList(
          if (!activeSymbol$isInput) {
            textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
              value = currentGraphLabel
            )
          }, getTimevisOptions()
        )), where = "beforeEnd"
      )
      if (isTRUE(configuredWithThisTool)) {
        seriesDataTmp <- currentGraphConfig[["series"]]
        customTmp <- currentGraphConfig[["custom"]]
        if (length(seriesDataTmp)) {
          addArrayEl(session, "timevis_series", defaults = seriesDataTmp)
          idLabelMap[["timevis_series"]] <<- as.list(names(seriesDataTmp))
          rv$graphConfig$graph$series <- seriesDataTmp
        }
        if (length(customTmp)) {
          addArrayEl(session, "timevis_custom", defaults = customTmp)
          idLabelMap[["timevis_custom"]] <<- as.list(names(customTmp))
          rv$graphConfig$graph$custom <- customTmp
        }
      }
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "pivot")) {
      showEl(session, ".category-btn-pivot")
      addClassEl(session, id = "#categoryPivot1", "category-btn-active")
      insertUI(
        selector = "#tool_options",
        tags$div(id = "pivot_options", tagList(
          if (!activeSymbol$isInput) {
            textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
              value = currentGraphLabel
            )
          }, getPivotOptions()
        )), where = "beforeEnd"
      )
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "miropivot")) {
      # make sure pivot table is refreshed when changing symbol
      insertUI(
        selector = "#tool_options",
        tagList(
          if (!activeSymbol$isInput) {
            textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
              value = currentGraphLabel
            )
          },
          getMIROPivotOptions(currentGraphConfig$options, prefix = "miropivot_")
        ),
        where = "beforeEnd"
      )
      rv$graphConfig$graph$symname <- activeSymbol$name
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "custom")) {
      showEl(session, ".category-btn-custom")
      addClassEl(session, id = "#categoryCustom1", "category-btn-active")
      invalidCustomRender <<- FALSE
      insertUI(
        selector = "#tool_options",
        tagList(
          if (!activeSymbol$isInput) {
            textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
              value = currentGraphLabel
            )
          },
          tags$div(id = "custom_options", getCustomOptions())
        ), where = "beforeEnd"
      )
      allDataAvailable <<- TRUE
      session$sendCustomMessage("gms-shinySetInputValue", list(
        id = "btUpdateCustomRendererOutput",
        val = 1L,
        timeout = 500L
      ))
    } else if (identical(chartTool, "datatable")) {
      # only show info message for datatable configuration
      insertUI(
        selector = "#tool_options",
        tags$div(
          id = "datatableInfoMsg", class = "config-message",
          style = "display:block;",
          paste0(lang$adminMode$graphs$datatableOptions$infoMsg, lang$adminMode$uiR$table, "'!")
        ),
        where = "beforeEnd"
      )
      rv$graphConfig$graph$symname <- activeSymbol$name
      allDataAvailable <<- TRUE
    } else if (identical(chartTool, "valuebox")) {
      showEl(session, ".category-btn-valuebox")
      addClassEl(session, id = "#categoryValuebox1", "category-btn-active")
      insertUI(
        selector = "#tool_options",
        tagList(
          if (!activeSymbol$isInput) {
            textAreaInput("renderer_label", lang$adminMode$graphs$ui$label,
              value = currentGraphLabel
            )
          },
          tags$div(id = "valuebox_options", getValueboxOptions())
        ), where = "beforeEnd"
      )
      allDataAvailable <<- TRUE
    }
    rv$refreshContent <- rv$refreshContent + 1L
  }
)
getPieOptions <- reactive({
  req(rv$initData)
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    valuesTmp <- if (length(scalarIndices)) scalarIndices[[1]] else ""
    rv$graphConfig$graph$traces <<- list()
    rv$graphConfig$graph$traces[["1"]] <<- list(
      labels = indices[[1]],
      values = valuesTmp,
      hole = 0,
      name = valuesTmp
    )
  })
  # We have to provide the symbol name with create array el, because else Javascript
  # will insert the new array element inside the wrapper of the previous selected symbol
  # due to the delay between R sending the new HTML content and the javascript call to be executed
  tagList(
    tags$div(
      class = "cat-body cat-body-1",
      createArray(session, "chart_piedata", lang$adminMode$graphs$chartOptions$ydata,
        class_outer = "array-wrapper-outer-graph", hr = FALSE,
        autoCreate = !isTRUE(configuredWithThisTool),
        symbolName = activeSymbol$name
      )
    ),
    tags$div(
      class = "cat-body cat-body-2", style = "display:none;",
      getOptionSection()
    )
  )
})
getChartOptions <- reactive({
  req(rv$resetRE > 0L)
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$xdata <<- checkLength(configuredWithThisTool, currentGraphConfig[["xdata"]], indices[[1]])
  })
  graphHasAnimation <- FALSE
  if (isTRUE(configuredWithThisTool) && length(currentGraphConfig[["animation"]])) {
    graphHasAnimation <- TRUE
  }
  tagList(
    tags$div(
      class = "cat-body cat-body-3 cat-body-8 cat-body-13 cat-body-18",
      selectInput("chart_xdata", lang$adminMode$graphs$chartOptions$xdata,
        choices = indices,
        selected = rv$graphConfig$graph$xdata
      ),
      getAxisOptions("x", names(indices)[1])
    ),
    tags$div(
      class = "cat-body cat-body-50 cat-body-51 cat-body-52 cat-body-53", style = "display:none;",
      getColorPivotOptions(),
      createArray(session, "chart_ydata", lang$adminMode$graphs$chartOptions$ydata, isolate(input$chart_tool),
        autoCreate = !isTRUE(configuredWithThisTool),
        class_outer = "array-wrapper-outer-graph", hr = FALSE
      )
    ),
    tags$div(
      class = "cat-body cat-body-4 cat-body-9 cat-body-14 cat-body-19", style = "display:none;",
      tags$div(
        id = "left_yaxis", class = "shiny-input-container",
        style = if (!axisOptionsGlobal[["y"]] > 0L) {
          "display: none;"
        },
        optionSection(
          lang$adminMode$graphs$axisOptions$leftAxis,
          getAxisOptions("y", names(scalarIndices)[1])
        )
      ),
      if (!identical(rv$graphConfig$graph$type, "bar")) {
        tags$div(
          id = "right_yaxis", class = "shiny-input-container",
          style = if (!axisOptionsGlobal[["y2"]] > 0L) {
            "display: none;"
          },
          optionSection(lang$adminMode$graphs$axisOptions$rightAxis, getAxisOptions("y2", names(scalarIndices)[1]))
        )
      }
    ),
    tags$div(
      class = "cat-body cat-body-5 cat-body-10 cat-body-15 cat-body-20", style = "display:none;",
      getFilterOptions()
    ),
    tags$div(
      class = "cat-body cat-body-6 cat-body-12 cat-body-17 cat-body-22", style = "display:none;",
      getOptionSection()
    ),
    tags$div(
      class = "cat-body cat-body-11 cat-body-16 cat-body-21", style = "display:none;",
      tags$div(
        id = "no_plotly_animation_options", class = "shiny-input-container config-message",
        style = if (graphHasAnimation) "display: none;" else "display:block;", lang$adminMode$graphs$animationOptions$noAnimation
      ),
      tags$div(
        id = "plotly_animation_options", class = "shiny-input-container",
        style = if (!graphHasAnimation) "display: none;", getAnimationOptions()
      )
    )
  )
})
getBarOptions <- reactive({
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$barmode <<- checkLength(configuredWithThisTool, currentGraphConfig[["barmode"]], "group")
    rv$graphConfig$graph$orientation <<- checkLength(configuredWithThisTool, currentGraphConfig[["orientation"]], NULL)
    rv$graphConfig$graph$width <<- checkLength(configuredWithThisTool, currentGraphConfig[["width"]], NULL)
    rv$graphConfig$graph$ydata <<- list()
    rv$graphConfig$graph$ydata[[indices[[1]]]] <<- list(
      label = names(indices)[1],
      mode = "lines",
      marker = list(line = list(width = 0L))
    )
    idLabelMap$chart_ydata[[1]] <<- indices[[1]]
  })
  tagList(
    tags$div(
      class = "cat-body cat-body-7", style = "display:none;",
      selectInput("bar_mode", lang$adminMode$graphs$barOptions$mode,
        choices = langSpecificGraphs$barmode,
        selected = rv$graphConfig$graph$barmode
      ),
      selectInput("bar_orientation", lang$adminMode$graphs$barOptions$orientation,
        choices = langSpecificGraphs$barOrientation,
        selected = rv$graphConfig$graph$orientation
      ),
      selectInput("bar_width", lang$adminMode$graphs$barOptions$width,
        choices = c("_", scalarIndices),
        selected = rv$graphConfig$graph$width
      )
    ),
    getChartOptions()
  )
})
getAxisOptions <- function(id, title, labelOnly = FALSE) {
  isolate({
    rv$graphConfig$graph[[id %+% "axis"]]$title <<- checkLength(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$title, title)
    if (!labelOnly) {
      rv$graphConfig$graph[[id %+% "axis"]]$showgrid <<- checkTRUE(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$showgrid)
      rv$graphConfig$graph[[id %+% "axis"]]$zeroline <<- checkTRUE(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$zeroline)
      rv$graphConfig$graph[[id %+% "axis"]]$showticklabels <<- checkLength(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$showticklabels, TRUE)
      rv$graphConfig$graph[[id %+% "axis"]]$categoryorder <<- checkLength(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$categoryorder, "trace")
      rv$graphConfig$graph[[id %+% "axis"]]$rangefrom <<- checkLength(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$rangefrom, NULL)
      rv$graphConfig$graph[[id %+% "axis"]]$rangeto <<- checkLength(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$rangeto, NULL)
    }
    if (!identical(rv$graphConfig$graph$type, "pie") && (identical(id, "y") || identical(id, "y2"))) {
      rv$graphConfig$graph[[id %+% "axis"]]$scaleratio <<- checkLength(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$scaleratio, NULL)
      rv$graphConfig$graph[[id %+% "axis"]]$scaleanchor <<- checkLength(configuredWithThisTool, currentGraphConfig[[id %+% "axis"]]$scaleanchor, NULL)
    }
  })
  if (labelOnly) {
    return(tagList(
      textInput(id %+% "_title", sprintf(lang$adminMode$graphs$axisOptions$labelOnlyTitle, id),
        value = rv$graphConfig$graph[[id %+% "axis"]]$title
      )
    ))
  }
  tagList(
    textInput(id %+% "_title", lang$adminMode$graphs$axisOptions$title,
      value = rv$graphConfig$graph[[id %+% "axis"]]$title
    ),
    selectInput(id %+% "_categoryorder", lang$adminMode$graphs$axisOptions$categoryorder,
      choices = langSpecificGraphs$categoryorderChoices,
      selected = rv$graphConfig$graph[[id %+% "axis"]]$categoryorder
    ),
    if (!identical(rv$graphConfig$graph$type, "pie") && (identical(id, "y") || identical(id, "y2"))) {
      tags$div(
        class = "shiny-input-container", style = "display:inline-block;",
        tags$div(
          tags$div(
            style = "max-width:400px;",
            tags$div(
              style = "display:inline-block",
              checkboxInput_MIRO("scaleratio_check",
                lang$adminMode$graphs$axisOptions$scaleRatioCheck,
                value = isTRUE(length(rv$graphConfig$graph[[id %+% "axis"]]$scaleratio) > 0L)
              )
            ),
            conditionalPanel(
              condition = "input.scaleratio_check===true",
              style = "display:inline-block;padding-left:35px;",
              tags$div(
                numericInput("scaleratio",
                  lang$adminMode$graphs$axisOptions$scaleRatio,
                  min = 0.1, value =
                    if (length(rv$graphConfig$graph[[id %+% "axis"]]$scaleratio)) {
                      rv$graphConfig$graph[[id %+% "axis"]]$scaleratio
                    } else {
                      1L
                    }, step = 0.1
                )
              )
            )
          )
        )
      )
    },
    checkboxInput_MIRO(id %+% "_showgrid", lang$adminMode$graphs$axisOptions$showgrid, rv$graphConfig$graph[[id %+% "axis"]]$showgrid),
    checkboxInput_MIRO(id %+% "_zeroline", lang$adminMode$graphs$axisOptions$zeroline, rv$graphConfig$graph[[id %+% "axis"]]$zeroline),
    checkboxInput_MIRO(id %+% "_showticklabels", lang$adminMode$graphs$axisOptions$showticklabels, rv$graphConfig$graph[[id %+% "axis"]]$showticklabels),
    if (identical(input$chart_tool, "scatter") || identical(input$chart_tool, "line") || identical(input$chart_tool, "bubble")) {
      tags$div(
        class = "shiny-input-container", style = "display:inline-block;",
        tags$label(class = "cb-label shiny-input-container", "for" = "range-wrapper", lang$adminMode$graphs$axisOptions$range),
        tags$div(
          style = "padding-top: 10px;",
          tags$div(
            id = "range-wrapper",
            tags$div(
              style = "max-width:400px;",
              tags$div(style = "display:inline-block", textInput(id %+% "_rangefrom", lang$adminMode$graphs$axisOptions$rangeFrom,
                value = rv$graphConfig$graph[[id %+% "axis"]]$rangefrom
              )),
              tags$div(style = "display:inline-block", textInput(id %+% "_rangeto", lang$adminMode$graphs$axisOptions$rangeTo,
                value = rv$graphConfig$graph[[id %+% "axis"]]$rangeto
              ))
            )
          )
        )
      )
    }
  )
}
getOptionSection <- reactive({
  req(rv$initData)
  isolate({
    rv$graphConfig$graph$title <<- checkLength(configuredWithThisTool, currentGraphConfig[["title"]], activeSymbol$alias)
    rv$graphConfig$graph$showlegend <<- checkLength(configuredWithThisTool, currentGraphConfig[["showlegend"]], TRUE)
    rv$graphConfig$graph$fixedHeight <<- checkLength(configuredWithThisTool, currentGraphConfig[["fixedHeight"]], NULL)
    rv$graphConfig$graph$fixedWidth <<- checkLength(configuredWithThisTool, currentGraphConfig[["fixedWidth"]], NULL)
    rv$graphConfig$graph$paper_bgcolor <<- checkLength(configuredWithThisTool, currentGraphConfig[["paper_bgcolor"]], NULL)
    rv$graphConfig$graph$plot_bgcolor <<- checkLength(configuredWithThisTool, currentGraphConfig[["plot_bgcolor"]], NULL)
    rv$graphConfig$graph$staticPlot <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["staticPlot"]])
  })
  tagList(
    textInput("chart_title", lang$adminMode$graphs$ui$chartTitle, value = rv$graphConfig$graph$title),
    checkboxInput_MIRO("showlegend", lang$adminMode$graphs$chartOptions$options$showlegend, value = TRUE),
    numericInput("fixedHeight",
      lang$adminMode$graphs$chartOptions$options$fixedHeight,
      min = 0L, value = rv$graphConfig$graph$fixedHeight, step = 1L
    ),
    numericInput("fixedWidth",
      lang$adminMode$graphs$chartOptions$options$fixedWidth,
      min = 0L, value = rv$graphConfig$graph$fixedWidth, step = 1L
    ),
    colorPickerInput("paper_bgcolor", lang$adminMode$graphs$chartOptions$options$paperBgColor, value = rv$graphConfig$graph$paper_bgcolor),
    colorPickerInput("plot_bgcolor", lang$adminMode$graphs$chartOptions$options$plotBgColor, value = rv$graphConfig$graph$plot_bgcolor),
    checkboxInput_MIRO("staticPlot", lang$adminMode$graphs$chartOptions$options$staticPlot, value = rv$graphConfig$graph$staticPlot),
    getOuttype()
  )
})
getOuttype <- reactive({
  isolate({
    rv$graphConfig$outType <<- checkLength(configuredWithThisTool, configJSON$dataRendering[[activeSymbolName]][["outType"]], "graph")
  })
  tagList(
    checkboxInput_MIRO("outType", tags$div(
      lang$adminMode$graphs$chartOptions$options$outType,
      tags$a("",
        class = "info-wrapper",
        href = "https://gams.com/miro/charts.html#table-graph-split-screen",
        tags$span(
          class = "fas fa-info-circle", class = "info-icon",
          role = "presentation",
          `aria-label` = "More information"
        ), target = "_blank"
      )
    ),
    value = identical(rv$graphConfig$outType, "dtGraph")
    )
  )
})
getScatterOptions <- reactive({
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$symbol <<- checkLength(configuredWithThisTool, currentGraphConfig[["symbol"]], NULL)
    rv$graphConfig$graph$ydata <- list()
    if (length(scalarIndices)) {
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(
        label = names(scalarIndices)[1],
        mode = "markers",
        fill = "none",
        marker = list(
          opacity = 1L,
          size = 6L,
          line = list(width = 0L)
        ),
        showlegend = TRUE,
        yaxis = "y"
      )
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    } else {
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  tagList(
    getChartOptions(),
    tags$div(
      class = "cat-body cat-body-10", style = "display:none;",
      selectInput("chart_symbol", lang$adminMode$graphs$chartOptions$symbol,
        choices = c("_", indices), selected = rv$graphConfig$graph$symbol
      )
    )
  )
})
getBubbleOptions <- reactive({
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$ydata <- list()
    if (length(scalarIndices)) {
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(
        label = names(scalarIndices)[1],
        mode = "markers",
        marker = list(
          symbol = "circle",
          opacity = 1L,
          size = scalarIndices[1],
          sizemode = "area",
          color = scalarIndices[1],
          line = list(width = 0L)
        ),
        showlegend = TRUE
      )
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    } else {
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  getChartOptions()
})
getLineOptions <- reactive({
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$ydata <- list()
    if (length(scalarIndices)) {
      rv$graphConfig$graph$ydata[[scalarIndices[[1]]]] <<- list(
        label = names(scalarIndices)[1],
        mode = "lines",
        line = list(
          width = 2L,
          shape = "linear",
          dash = "solid"
        ),
        showlegend = TRUE,
        yaxis = "y"
      )
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    } else {
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  getChartOptions()
})
getAnimationOptions <- reactive({
  req(rv$resetRE > 0L)
  isolate({
    animationDefaults <- list(
      easing     = checkLength(configuredWithThisTool, currentGraphConfig[["animation"]][["easing"]], "linear"),
      mode       = checkLength(configuredWithThisTool, currentGraphConfig[["animation"]][["mode"]], "immediate"),
      redraw     = checkLength(configuredWithThisTool, currentGraphConfig[["animation"]][["redraw"]], TRUE),
      frame      = checkLength(configuredWithThisTool, currentGraphConfig[["animation"]][["frame"]], 1000L)
    )
    if (isTRUE(configuredWithThisTool) && length(currentGraphConfig[["animation"]])) {
      rv$graphConfig$graph$animation$easing <<- animationDefaults$easing
      rv$graphConfig$graph$animation$mode <<- animationDefaults$mode
      rv$graphConfig$graph$animation$redraw <<- animationDefaults$redraw
      rv$graphConfig$graph$animation$frame <<- animationDefaults$frame
      rv$graphConfig$graph$animation$transition <<- checkLength(
        configuredWithThisTool,
        currentGraphConfig[["animation"]][["transition"]],
        animationDefaults$frame
      )
    } else {
      rv$graphConfig$graph$animation <- NULL
    }
  })
  tagList(
    numericInput("animation_frame", lang$adminMode$graphs$animationOptions$frame,
      min = 0L,
      value = 1000L / animationDefaults$frame
    ),
    selectInput("animation_easing", lang$adminMode$graphs$animationOptions$easing,
      choices = langSpecificGraphs$easingChoices,
      selected = animationDefaults$easing
    ),
    checkboxInput_MIRO("animation_redraw", lang$adminMode$graphs$animationOptions$redraw,
      value = animationDefaults$redraw
    ),
    selectInput("animation_mode", lang$adminMode$graphs$animationOptions$mode,
      choices = langSpecificGraphs$modeChoices,
      selected = animationDefaults$mode
    ),
    getAnimationSliderOptions()
  )
})
getAnimationSliderOptions <- reactive({
  req(rv$resetRE > 0L)
  isolate({
    animationDefaults <- list(
      hide      = checkTRUE(configuredWithThisTool, currentGraphConfig[["animation"]][["slider"]][["hide"]]),
      prefix    = checkLength(configuredWithThisTool, currentGraphConfig[["animation"]][["slider"]][["prefix"]], ""),
      fontcolor = checkLength(configuredWithThisTool, currentGraphConfig[["animation"]][["slider"]][["fontcolor"]], "#000000")
    )
    if (isTRUE(configuredWithThisTool) && length(currentGraphConfig[["animation"]][["slider"]])) {
      rv$graphConfig$graph$animation$slider$hide <<- animationDefaults$hide
      rv$graphConfig$graph$animation$slider$prefix <<- animationDefaults$prefix
      rv$graphConfig$graph$animation$slider$fontcolor <<- animationDefaults$fontcolor
    } else {
      rv$graphConfig$graph$animation$slider <- NULL
    }
  })
  tagList(
    checkboxInput_MIRO("animation_slider_hide", lang$adminMode$graphs$animationSliderOptions$hide,
      value = animationDefaults$hide
    ),
    textInput("animation_slider_prefix", lang$adminMode$graphs$animationSliderOptions$prefix,
      value = animationDefaults$prefix
    ),
    colorPickerInput("animation_slider_font_color", lang$adminMode$graphs$animationSliderOptions$fontColor,
      value = animationDefaults$fontcolor
    )
  )
})
getHistOptions <- reactive({
  scalarIndices <- activeSymbol$indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    label <- if (isTRUE(configuredWithThisTool) && length(currentGraphConfig$xaxis[["title"]])) {
      currentGraphConfig$xaxis[["title"]]
    } else {
      names(activeSymbol$indices)[match(scalarIndices[1], activeSymbol$indices)][1]
    }
    rv$graphConfig$graph$xdata <<- list()
    rv$graphConfig$graph$xdata[[scalarIndices[1]]] <<- list(labels = label, color = "#000000")
    rv$graphConfig$graph$histnorm <<- checkLength(configuredWithThisTool, currentGraphConfig[["histnorm"]], "")
    rv$graphConfig$graph$nbins <<- checkLength(configuredWithThisTool, currentGraphConfig[["nbins"]], 2L)
    rv$graphConfig$graph$barmode <<- checkLength(configuredWithThisTool, currentGraphConfig[["barmode"]], "overlay")
    rv$graphConfig$graph$alpha <<- checkLength(configuredWithThisTool, currentGraphConfig[["alpha"]], 0.6)
    rv$graphConfig$graph$cumulative <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["cumulative"]])
    rv$graphConfig$graph$horizontal <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["horizontal"]])
    if (length(scalarIndices)) {
      idLabelMap$hist_xdata[[1]] <<- scalarIndices[[1]]
    } else {
      idLabelMap$hist_xdata[[1]] <<- "1"
    }
  })
  tagList(
    tags$div(
      class = "cat-body cat-body-23",
      getColorPivotOptions(),
      createArray(session, "hist_xdata", lang$adminMode$graphs$histOptions$xdata,
        autoCreate = !isTRUE(configuredWithThisTool),
        class_outer = "array-wrapper-outer-graph", hr = FALSE
      )
    ),
    tags$div(
      class = "cat-body cat-body-24", style = "display:none;",
      getAxisOptions("x", label, labelOnly = TRUE),
      getAxisOptions("y", "", labelOnly = TRUE),
      selectInput("hist_norm", lang$adminMode$graphs$histOptions$norm,
        choices = langSpecificGraphs$normChoices,
        selected = rv$graphConfig$graph$histnorm
      ),
      numericInput("hist_nbins", lang$adminMode$graphs$histOptions$nbins,
        min = 0L, value = rv$graphConfig$graph$nbins
      ),
      selectInput("hist_barmode", lang$adminMode$graphs$histOptions$barmode,
        choices = langSpecificGraphs$barmodeChoices,
        selected = rv$graphConfig$graph$barmode
      ),
      numericInput("hist_alpha", lang$adminMode$graphs$histOptions$alpha,
        min = 0L, max = 1L, step = 0.1, value = rv$graphConfig$graph$alpha
      ),
      checkboxInput_MIRO("hist_cumulative", lang$adminMode$graphs$histOptions$cumulative,
        value = rv$graphConfig$graph$cumulative
      ),
      selectInput("hist_horizontal", lang$adminMode$graphs$histOptions$horizontal,
        choices = langSpecificGraphs$orientationChoices,
        selected = if (isTRUE(rv$graphConfig$graph$horizontal)) {
          "horizontal"
        } else {
          "vertical"
        }
      )
    ),
    tags$div(
      class = "cat-body cat-body-25", style = "display:none;",
      getFilterOptions()
    ),
    tags$div(
      class = "cat-body cat-body-26", style = "display:none;",
      getOptionSection()
    )
  )
})
getValueboxOptions <- reactive({
  rv$initData
  rv$refreshContent
  visibleScalars <- !tolower(modelOut[[scalarsOutName]]$symnames) %in% tolower(configJSON$hiddenOutputScalars)
  scalarNames <- modelOut[[scalarsOutName]]$symnames[visibleScalars]

  noScalars <- length(scalarNames)
  if (!length(currentGraphConfig) || !length(names(currentGraphConfig[[1]]))) {
    boxWidth <- checkLength(configuredWithThisTool, currentGraphConfig[["width"]], 4L)
    noBoxesRow <- 12 / boxWidth
    numberRows <- ceiling(boxWidth * noScalars / 12)
    oldConfig <- TRUE
  } else {
    configuredScalars <- unlist(lapply(currentGraphConfig, names), use.names = FALSE)
    unconfiguredScalars <- !tolower(scalarNames) %in% tolower(configuredScalars)
    if (any(unconfiguredScalars)) {
      unconfiguredScalars <- scalarNames[unconfiguredScalars]
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
      currentGraphConfig <- c(currentGraphConfig, additionalOptions)
    }
    numberRows <- length(currentGraphConfig)
    oldConfig <- FALSE
  }

  tags$div(
    class = "cat-body cat-body-49",
    lapply(seq_len(noScalars), function(rowId) {
      if (rowId > numberRows) {
        rowConfig <- vector("list", 1L)
      } else if (oldConfig) {
        rowConfig <- vector("list", noBoxesRow)
      } else {
        rowConfig <- currentGraphConfig[[rowId]]
        boxWidth <- 12 / length(rowConfig)
      }
      tagList(
        tags$div(
          id = paste0("valueboxRow_", rowId), class = "drop-index-list valuebox-config-row",
          style = "margin-bottom:20px;display:flex;",
          if (rowId <= numberRows) {
            lapply(seq_along(rowConfig), function(i) {
              if (oldConfig) {
                i <- i + noBoxesRow * (rowId - 1L)
                if (i > noScalars) {
                  return()
                }
                scalarConfig <- list(
                  icon = currentGraphConfig[["icon"]],
                  color = currentGraphConfig[["color"]]
                )
              } else {
                scalarConfig <- rowConfig[[i]]
                i <- match(names(rowConfig)[i], scalarNames)
                if (is.na(i)) {
                  return()
                }
              }
              tags$div(
                "data-rank-id" = paste0("valueBox_", i),
                class = "valuebox-config-el",
                tags$div(
                  title = lang$adminMode$graphs$valueboxOptions$desc,
                  textInput(paste0("valueBoxDesc_", i),
                    label = NULL,
                    value = if (length(scalarConfig$description)) {
                      scalarConfig$description
                    } else {
                      modelOut[[scalarsOutName]]$symtext[[i]]
                    }
                  )
                ),
                tags$div(
                  title = lang$adminMode$graphs$valueboxOptions$color,
                  selectInput(paste0("valueBoxColor_", i),
                    label = NULL,
                    choices = langSpecificGraphs$valueboxColor,
                    selected = if (length(scalarConfig$color)) scalarConfig$color else "aqua"
                  )
                ),
                fluidRow(
                  column(5,
                    class = "valuebox-left-col",
                    tags$div(
                      title = lang$adminMode$graphs$valueboxOptions$round,
                      numericInput(paste0("valueBoxRound_", i),
                        label = NULL, min = 0L,
                        value = if (length(scalarConfig$round)) scalarConfig$round else config$roundingDecimals
                      )
                    )
                  ),
                  column(7,
                    class = "valuebox-right-col",
                    tags$div(
                      title = lang$adminMode$graphs$valueboxOptions$icon,
                      selectizeInput(paste0("valueBoxIcon_", i),
                        label = NULL,
                        choices = unique(c(
                          langSpecificGraphs$valueboxIconChoices,
                          scalarConfig$icon$name
                        )),
                        selected = scalarConfig$icon$name,
                        options = list(create = TRUE)
                      )
                    )
                  )
                )
              )
            })
          }
        ),
        sortable_js(paste0("valueboxRow_", rowId),
          options = sortable_options(
            group = "valueBoxes", supportPointer = FALSE,
            put = htmlwidgets::JS("
                                                      function(to) {
                                                        return (to.el.children.length < 12 && from.el.children.length > 1);
                                                      }
                                                    "),
            onLoad = sortable_js_capture_bucket_input(
              "valueBoxes",
              paste0("valueboxRow_", seq_len(noScalars)),
              paste0("valueboxRow_", seq_len(noScalars))
            ),
            onSort = sortable_js_capture_bucket_input(
              "valueBoxes",
              paste0("valueboxRow_", seq_len(noScalars)),
              paste0("valueboxRow_", seq_len(noScalars))
            )
          )
        )
      )
    })
  )
})
getDygraphsOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  if (!length(scalarIndices)) {
    showElReplaceTxt(session, "#preview-error", lang$adminMode$graphs$dygraphsOptions$previewError)
    return()
  }
  isolate({
    rv$graphConfig$graph$xdata <<- checkLength(configuredWithThisTool, currentGraphConfig[["xdata"]], unname(indices[1]))
    rv$graphConfig$graph$ydata <<- NULL
    rv$graphConfig$graph$ydata[[scalarIndices[1]]] <<- list(
      label = names(scalarIndices)[[1]],
      stemPlot = FALSE, stepPlot = FALSE,
      fillGraph = FALSE, drawPoints = FALSE,
      pointShape = "dot",
      pointSize = 2L,
      yaxis = "y"
    )

    rv$graphConfig$graph$dyEvent <<- NULL
    rv$graphConfig$graph$dyLimit <<- NULL
    rv$graphConfig$graph$dyAnnotation <<- NULL
    rv$graphConfig$graph$dyShading <<- NULL


    rv$graphConfig$graph$title <<- checkLength(configuredWithThisTool, currentGraphConfig[["title"]], activeSymbol$alias)
    rv$graphConfig$graph$dyOptions$logscale <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["logscale"]])
    rv$graphConfig$graph$dyOptions$stepPlot <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["stepPlot"]])
    rv$graphConfig$graph$dyOptions$stemPlot <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["stemPlot"]])
    rv$graphConfig$graph$dyOptions$fillGraph <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["fillGraph"]])
    rv$graphConfig$graph$dyOptions$fillAlpha <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["fillAlpha"]], 0.15)
    rv$graphConfig$graph$dyOptions$drawPoints <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["drawPoints"]])
    rv$graphConfig$graph$dyOptions$pointShape <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["pointShape"]], "dot")
    rv$graphConfig$graph$dyOptions$pointSize <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyOptions"]][["pointSize"]], 2L)

    rv$graphConfig$graph$dyHighlight <<- NULL
    rv$graphConfig$graph$dyHighlight$highlightSeriesBackgroundAlpha <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyHighlight"]][["highlightSeriesBackgroundAlpha"]], NULL)
    rv$graphConfig$graph$dyHighlight$hideOnMouseOut <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyHighlight"]][["hideOnMouseOut"]], NULL)
    rv$graphConfig$graph$dyHighlight$highlightCircleSize <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyHighlight"]][["highlightCircleSize"]], NULL)
    rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeWidth <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyHighlight"]][["highlightSeriesOpts"]][["strokeWidth"]], NULL)
    rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeBorderWidth <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyHighlight"]][["highlightSeriesOpts"]][["strokeBorderWidth"]], NULL)
    rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeBorderColor <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyHighlight"]][["highlightSeriesOpts"]][["strokeBorderColor"]], NULL)

    rv$graphConfig$graph$dyLegend$show <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyLegend"]][["show"]], "auto")
    rv$graphConfig$graph$dyLegend$width <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyLegend"]][["width"]], 250L)
    rv$graphConfig$graph$dyLegend$showZeroValues <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["dyLegend"]][["showZeroValues"]])
    rv$graphConfig$graph$dyLegend$labelsSeparateLines <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyLegend"]][["labelsSeparateLines"]], FALSE)

    rv$graphConfig$graph$dyRangeSelector <<- NULL
    rv$graphConfig$graph$dyRangeSelector$height <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyRangeSelector"]][["height"]], NULL)
    rv$graphConfig$graph$dyRangeSelector$strokeColor <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyRangeSelector"]][["strokeColor"]], NULL)
    rv$graphConfig$graph$dyRangeSelector$fillColor <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyRangeSelector"]][["fillColor"]], NULL)
    rv$graphConfig$graph$dyRangeSelector$retainDateWindow <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyRangeSelector"]][["retainDateWindow"]], NULL)
    rv$graphConfig$graph$dyRangeSelector$keepMouseZoom <<- checkLength(configuredWithThisTool, currentGraphConfig[["dyRangeSelector"]][["keepMouseZoom"]], NULL)

    rv$graphConfig$graph$color <<- checkLength(configuredWithThisTool, currentGraphConfig[["color"]], NULL)
    if (length(scalarIndices)) {
      idLabelMap$chart_ydata[[1]] <<- scalarIndices[[1]]
    } else {
      idLabelMap$chart_ydata[[1]] <<- "1"
    }
  })
  tagList(
    tags$div(
      class = "cat-body cat-body-27",
      selectInput("chart_xdata", lang$adminMode$graphs$dygraphsOptions$xdata,
        choices = indices,
        selected = rv$graphConfig$graph$xdata
      ),
      getDyaxisOptions("dxAxis", names(indices[1]))
    ),
    tags$div(
      class = "cat-body cat-body-28", style = "display:none;",
      if (!length(indices[activeSymbol$indexTypes == "numeric"]) > 1L) {
        selectInput("chart_color", tags$div(
          lang$adminMode$graphs$dygraphsOptions$color,
          tags$a("",
            class = "info-wrapper", href = "https://gams.com/miro/charts.html#group-domain",
            tags$span(
              class = "fas fa-info-circle", class = "info-icon",
              role = "presentation",
              `aria-label` = "More information"
            ), target = "_blank"
          )
        ),
        choices = c("_", indices[activeSymbol$indexTypes == "string"]), selected = rv$graphConfig$graph$color
        )
      },
      conditionalPanel(
        condition = "input.chart_color == null || input.chart_color==='_'",
        createArray(session, "dy_ydata", lang$adminMode$graphs$dygraphsOptions$ydata,
          autoCreate = !isTRUE(configuredWithThisTool),
          class_outer = "array-wrapper-outer-graph", hr = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.chart_color != null && input.chart_color !== '_'",
        tags$div(class = "shiny-input-container config-message", style = "display:block;", lang$adminMode$graphs$dygraphsOptions$ydataGeneral)
      )
    ),
    tags$div(
      class = "cat-body cat-body-54", style = "display:none;",
      tags$div(
        id = "left_dyAxis", class = "shiny-input-container",
        style = if (!axisOptionsGlobal[["y"]] > 0L) {
          "display: none;"
        },
        optionSection(
          lang$adminMode$graphs$axisOptions$leftAxis,
          getDyaxisOptions("dyAxis", names(scalarIndices)[1])
        )
      ),
      tags$div(
        id = "right_dyAxis", class = "shiny-input-container",
        style = if (!axisOptionsGlobal[["y2"]] > 0L) {
          "display: none;"
        },
        optionSection(
          lang$adminMode$graphs$axisOptions$rightAxis,
          getDyaxisOptions("dyAxis2", names(scalarIndices)[1])
        )
      )
    ),
    tags$div(
      class = "cat-body cat-body-29", style = "display:none;",
      getFilterOptions()
    ),
    if (length(configScalars) && nrow(configScalars)) {
      tagList(
        tags$div(
          class = "cat-body cat-body-30", style = "display:none;",
          createArray(session, "dy_dyEvent", lang$adminMode$graphs$dygraphsOptions$dyEvent,
            autoCreate = FALSE,
            class_outer = "array-wrapper-outer-graph", hr = FALSE
          )
        ),
        tags$div(
          class = "cat-body cat-body-31", style = "display:none;",
          createArray(session, "dy_dyLimit", lang$adminMode$graphs$dygraphsOptions$dyLimit,
            autoCreate = FALSE,
            class_outer = "array-wrapper-outer-graph", hr = FALSE
          )
        ),
        tags$div(
          class = "cat-body cat-body-32", style = "display:none;",
          createArray(session, "dy_dyAnnotation", lang$adminMode$graphs$dygraphsOptions$dyAnnotation,
            autoCreate = FALSE,
            class_outer = "array-wrapper-outer-graph", hr = FALSE
          )
        ),
        tags$div(
          class = "cat-body cat-body-33", style = "display:none;",
          createArray(session, "dy_dyShading", lang$adminMode$graphs$dygraphsOptions$dyShading,
            autoCreate = FALSE,
            class_outer = "array-wrapper-outer-graph", hr = FALSE
          )
        )
      )
    },
    tags$div(
      class = "cat-body cat-body-34", style = "display:none;",
      title = lang$adminMode$graphs$dygraphsOptions$rngSelOpts$title, collapsed = TRUE,
      checkboxInput_MIRO("dyrange_activate", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$activate,
        value = if (length(rv$graphConfig$graph$dyRangeSelector)) TRUE else FALSE
      ),
      conditionalPanel(
        condition = "input.dyrange_activate == true",
        numericInput("dyrange_height", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$height,
          min = 0L,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyRangeSelector$height, 40L)
        ),
        checkboxInput_MIRO("dyrange_retainDateWindow", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$retainDateWindow,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyRangeSelector$retainDateWindow, FALSE)
        ),
        checkboxInput_MIRO("dyrange_keepMouseZoom", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$keepMouseZoom,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyRangeSelector$keepMouseZoom, TRUE)
        ),
        colorPickerInput("dyrange_fillColor", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$fillColor,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyRangeSelector$fillColor, "#A7B1C4")
        ),
        colorPickerInput("dyrange_strokeColor", lang$adminMode$graphs$dygraphsOptions$rngSelOpts$strokeColor,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyRangeSelector$strokeColor, "#808FAB")
        )
      )
    ),
    tags$div(
      class = "cat-body cat-body-36", style = "display:none;",
      textInput("chart_title", lang$adminMode$graphs$ui$chartTitle,
        value = rv$graphConfig$graph$title
      ),
      checkboxInput_MIRO("dyopt_logscale", lang$adminMode$graphs$dygraphsOptions$generalOpts$logscale,
        value = rv$graphConfig$graph$dyOptions$logscale
      ),
      checkboxInput_MIRO("dyopt_stepPlot", lang$adminMode$graphs$dygraphsOptions$generalOpts$stepPlot,
        value = rv$graphConfig$graph$dyOptions$stepPlot
      ),
      checkboxInput_MIRO("dyopt_stemPlot", lang$adminMode$graphs$dygraphsOptions$generalOpts$stemPlot,
        value = rv$graphConfig$graph$dyOptions$stemPlot
      ),
      checkboxInput_MIRO("dyopt_fillGraph", lang$adminMode$graphs$dygraphsOptions$generalOpts$fillGraph,
        value = rv$graphConfig$graph$dyOptions$fillGraph
      ),
      numericInput("dyopt_fillAlpha", lang$adminMode$graphs$dygraphsOptions$generalOpts$fillAlpha,
        min = 0L,
        max = 1L, step = 0.1, value = rv$graphConfig$graph$dyOptions$fillAlpha
      ),
      checkboxInput_MIRO("dyopt_drawPoints", lang$adminMode$graphs$dygraphsOptions$generalOpts$drawPoints,
        value = rv$graphConfig$graph$dyOptions$drawPoints
      ),
      selectInput("dyopt_pointShape", lang$adminMode$graphs$dygraphsOptions$generalOpts$pointShape,
        choices = langSpecificGraphs$pointShapeChoices,
        selected = rv$graphConfig$graph$dyOptions$pointShape
      ),
      numericInput("dyopt_pointSize", lang$adminMode$graphs$dygraphsOptions$generalOpts$pointSize,
        min = 0L,
        value = rv$graphConfig$graph$dyOptions$pointSize
      ),
      getOuttype()
    ),
    tags$div(
      class = "cat-body cat-body-35", style = "display:none;",
      checkboxInput_MIRO("dyhighlight_activate", lang$adminMode$graphs$dygraphsOptions$highOpts$activate,
        # check if highlighting is configured. A check for length(dyHighlight) is not sufficient
        # since 'no highlight' also sets highlighting options (noDygraphHighlight)
        value = if (!length(rv$graphConfig$graph[["dyHighlight"]]) ||
          identical(
            rv$graphConfig$graph[["dyHighlight"]][order(names(rv$graphConfig$graph[["dyHighlight"]]))],
            noDygraphHighlight
          )) {
          FALSE
        } else {
          TRUE
        }
      ),
      conditionalPanel(
        condition = "input.dyhighlight_activate == true",
        numericInput("dyhigh_circleSize", lang$adminMode$graphs$dygraphsOptions$highOpts$circleSize,
          min = 0L,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyHighlight$highlightCircleSize, 3L)
        ),
        sliderInput("dyhigh_seriesBackgroundAlpha", lang$adminMode$graphs$dygraphsOptions$highOpts$seriesBackgroundAlpha,
          min = 0L, max = 1L, step = 0.1,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyHighlight$highlightSeriesBackgroundAlpha, 0.5)
        ),
        numericInput("dyhigh_strokeWidth", lang$adminMode$graphs$dygraphsOptions$highOpts$strokeWidth,
          min = 0L,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeWidth, 1L)
        ),
        numericInput("dyhigh_strokeBorderWidth", lang$adminMode$graphs$dygraphsOptions$highOpts$strokeBorderWidth,
          min = 0L,
          step = 0.1, value = rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeBorderWidth
        ),
        colorPickerInput("dyhigh_strokeBorderColor", lang$adminMode$graphs$dygraphsOptions$highOpts$strokeBorderColor,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyHighlight$highlightSeriesOpts$strokeBorderColor, "#ffffff")
        ),
        checkboxInput_MIRO("dyhigh_hideOnMouseOut", lang$adminMode$graphs$dygraphsOptions$highOpts$hideOnMouseOut,
          value = checkLength(configuredWithThisTool, rv$graphConfig$graph$dyHighlight$hideOnMouseOut, TRUE)
        )
      )
    ),
    tags$div(
      class = "cat-body cat-body-55", style = "display:none;",
      checkboxInput_MIRO("dyLegend_activate", lang$adminMode$graphs$dygraphsOptions$legend$activate,
        value = if (!length(rv$graphConfig$graph[["dyLegend"]]) ||
          identical(rv$graphConfig$graph[["dyLegend"]][["show"]], "never")) {
          FALSE
        } else {
          TRUE
        }
      ),
      conditionalPanel(
        condition = "input.dyLegend_activate == true",
        selectInput("dyLegend_show", lang$adminMode$graphs$dygraphsOptions$legend$show,
          choices = langSpecificGraphs$dyLegendOptions,
          selected = rv$graphConfig$graph$dyLegend$show
        ),
        numericInput("dyLegend_width", lang$adminMode$graphs$dygraphsOptions$legend$width,
          min = 1L, step = 1, value = rv$graphConfig$graph$dyLegend$width
        ),
        checkboxInput_MIRO("dyLegend_showZeroValues", lang$adminMode$graphs$dygraphsOptions$legend$showZeroValues,
          value = rv$graphConfig$graph$dyLegend$showZeroValues
        ),
        checkboxInput_MIRO("dyLegend_labelsSeparateLines", lang$adminMode$graphs$dygraphsOptions$legend$labelsSeparateLines,
          value = rv$graphConfig$graph$dyLegend$labelsSeparateLines
        )
      )
    )
  )
})
getDyaxisOptions <- function(id, title, labelOnly = FALSE) {
  if (identical(id, "dyAxis2")) {
    idJSON <- "yaxis2"
  } else if (identical(id, "dyAxis")) {
    idJSON <- "yaxis"
  } else {
    idJSON <- "xaxis"
  }
  isolate({
    rv$graphConfig$graph[[idJSON]]$name <<- if (identical(id, "dyAxis2")) "y2" else if (identical(id, "dyAxis")) "y" else "x"
    rv$graphConfig$graph[[idJSON]]$label <<- checkLength(configuredWithThisTool, currentGraphConfig[[idJSON]][["label"]], title)
    rv$graphConfig$graph[[idJSON]]$axisLineColor <<- checkLength(configuredWithThisTool, currentGraphConfig[[idJSON]][["axisLineColor"]], NULL)
    rv$graphConfig$graph[[idJSON]]$axisLineWidth <<- checkLength(configuredWithThisTool, currentGraphConfig[[idJSON]][["axisLineWidth"]], 0.3)
    rv$graphConfig$graph[[idJSON]]$axisLabelFontSize <<- checkLength(configuredWithThisTool, currentGraphConfig[[idJSON]][["axisLabelFontSize"]], 14L)
    rv$graphConfig$graph[[idJSON]]$gridLineWidth <<- checkLength(configuredWithThisTool, currentGraphConfig[[idJSON]][["gridLineWidth"]], 0.3)
    # when a second yaxis is used, its grid can only be drawn if drawGrid for first yaxis is also set to TRUE. Using gridinecolor for y1 axis therefore ('transparent', 'gray') therefore.
    if (identical(id, "dyAxis")) {
      rv$graphConfig$graph[[idJSON]]$gridLineColor <<- checkLength(configuredWithThisTool, currentGraphConfig[[idJSON]][["gridLineColor"]], "gray")
    } else {
      rv$graphConfig$graph[[idJSON]]$drawGrid <<- checkNotFALSE(configuredWithThisTool, currentGraphConfig[[idJSON]][["drawGrid"]])
    }
    if (!identical(id, "dxAxis")) {
      valFrom <- if (length(currentGraphConfig[[idJSON]][["valueRange"]][[1]]) &&
        !is.na(currentGraphConfig[[idJSON]][["valueRange"]][[1]])) {
        currentGraphConfig[[idJSON]][["valueRange"]][[1]]
      } else {
        NULL
      }
      valTo <- if (length(currentGraphConfig[[idJSON]][["valueRange"]][[2]]) &&
        !is.na(currentGraphConfig[[idJSON]][["valueRange"]][[2]])) {
        currentGraphConfig[[idJSON]][["valueRange"]][[2]]
      } else {
        NULL
      }
      rv$graphConfig$graph[[idJSON]]$valueRange <<- list(valFrom, valTo)
      rv$graphConfig$graph[[idJSON]]$independentTicks <<- checkNotFALSE(configuredWithThisTool, currentGraphConfig[[idJSON]][["independentTicks"]])
    }
  })
  tagList(
    textInput(id %+% "_label", lang$adminMode$graphs$dygraphsOptions$axisOptions$title,
      value = rv$graphConfig$graph[[idJSON]]$label
    ),
    if (!identical(id, "dxAxis")) {
      checkboxInput_MIRO(
        id %+% "_independentTicks", lang$adminMode$graphs$dygraphsOptions$axisOptions$independentTicks,
        rv$graphConfig$graph[[idJSON]]$independentTicks
      )
    },
    if (identical(id, "dyAxis")) {
      checkboxInput_MIRO(
        id %+% "_drawGrid", lang$adminMode$graphs$dygraphsOptions$axisOptions$drawGrid,
        if (identical(rv$graphConfig$graph[[idJSON]]$gridLineColor, "gray")) TRUE else FALSE
      )
    } else {
      checkboxInput_MIRO(id %+% "_drawGrid", lang$adminMode$graphs$dygraphsOptions$axisOptions$drawGrid, rv$graphConfig$graph[[idJSON]]$drawGrid)
    },
    conditionalPanel(
      condition = "input." %+% id %+% "_drawGrid===true",
      style = "display:inline-block;padding-left:35px;",
      tags$div(
        numericInput(id %+% "_gridLineWidth", lang$adminMode$graphs$dygraphsOptions$axisOptions$gridLineWidth,
          min = 0.1,
          value = rv$graphConfig$graph[[idJSON]]$gridLineWidth, step = 0.1
        )
      )
    ),
    colorPickerInput(id %+% "_axisLineColor", lang$adminMode$graphs$dygraphsOptions$axisOptions$axisLineColor,
      value = rv$graphConfig$graph[[idJSON]]$axisLineColor
    ),
    numericInput(id %+% "_axisLineWidth", lang$adminMode$graphs$dygraphsOptions$axisOptions$axisLineWidth,
      min = 0.1,
      value = rv$graphConfig$graph[[idJSON]]$axisLineWidth, step = 0.1
    ),
    numericInput(id %+% "_axisLabelFontSize", lang$adminMode$graphs$dygraphsOptions$axisOptions$axisLabelFontSize,
      min = 0L,
      value = rv$graphConfig$graph[[idJSON]]$axisLabelFontSize
    ),
    if (!identical(id, "dxAxis")) {
      tags$div(
        class = "shiny-input-container", style = "display:inline-block;",
        tags$label(class = "cb-label shiny-input-container", "for" = "valueRangeWrapper", lang$adminMode$graphs$dygraphsOptions$axisOptions$valueRange),
        tags$div(
          style = "padding-top: 10px;",
          tags$div(
            id = "valueRangeWrapper",
            tags$div(
              style = "max-width:400px;",
              tags$div(
                style = "display:inline-block",
                textInput(id %+% "_valueRangeFrom",
                  lang$adminMode$graphs$dygraphsOptions$axisOptions$valueRangeFrom,
                  value = rv$graphConfig$graph[[idJSON]]$valueRange[[1]]
                )
              ),
              tags$div(
                style = "display:inline-block",
                textInput(id %+% "_valueRangeTo",
                  lang$adminMode$graphs$dygraphsOptions$axisOptions$valueRangeTo,
                  value = rv$graphConfig$graph[[idJSON]]$valueRange[[2]]
                )
              )
            )
          )
        )
      )
    }
  )
}
getLeafletOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$markers <<- NULL
    rv$graphConfig$graph$flows <<- NULL
    rv$graphConfig$graph$minicharts <<- NULL
    rv$graphConfig$graph$hideGroups <<- checkLength(configuredWithThisTool, currentGraphConfig[["hideGroups"]], NULL)
    rv$graphConfig$graph$layersControl$baseGroups <<- checkLength(configuredWithThisTool, currentGraphConfig[["layersControl"]][["baseGroups"]], NULL)
    rv$graphConfig$graph$layersControl$overlayGroups <<- checkLength(configuredWithThisTool, currentGraphConfig[["layersControl"]][["overlayGroups"]], NULL)
    rv$graphConfig$graph$layersControl$position <<- checkLength(configuredWithThisTool, currentGraphConfig[["layersControl"]][["position"]], "topright")
    rv$graphConfig$graph$layersControl$options$collapsed <<- checkNotFALSE(configuredWithThisTool, currentGraphConfig[["layersControl"]][["options"]][["collapsed"]])
  })
  tagList(
    tagList(
      tags$div(
        class = "cat-body cat-body-37",
        createArray(session, "leaflet_markers", lang$adminMode$graphs$leafletOptions$markers,
          autoCreate = FALSE,
          class_outer = "array-wrapper-outer-graph", hr = FALSE
        )
      ),
      tags$div(
        class = "cat-body cat-body-38", style = "display:none;",
        createArray(session, "leaflet_flows", lang$adminMode$graphs$leafletOptions$flows,
          autoCreate = FALSE,
          class_outer = "array-wrapper-outer-graph", hr = FALSE
        )
      ),
      tags$div(
        class = "cat-body cat-body-39", style = "display:none;",
        createArray(session, "leaflet_minicharts", lang$adminMode$graphs$leafletOptions$minicharts,
          autoCreate = FALSE,
          class_outer = "array-wrapper-outer-graph", hr = FALSE
        )
      ),
      tags$div(
        class = "cat-body cat-body-40", style = "display:none;",
        selectInput("leaflet_hideGroups", lang$adminMode$graphs$leafletOptions$hideGroups,
          choices = leafletGroups$get(),
          selected = rv$graphConfig$graph$hideGroups,
          multiple = TRUE
        ),
        selectInput("leaflc_baseGroups", lang$adminMode$graphs$leafletOptions$layer$baseGroups,
          choices = leafletGroups$get(),
          selected = rv$graphConfig$graph$layersControl$baseGroups,
          multiple = TRUE
        ),
        selectInput("leaflc_overlayGroups", lang$adminMode$graphs$leafletOptions$layer$overlayGroups,
          choices = leafletGroups$get(),
          selected = rv$graphConfig$graph$layersControl$overlayGroups,
          multiple = TRUE
        ),
        selectInput("leaflc_position", lang$adminMode$graphs$leafletOptions$layer$position,
          choices = langSpecificGraphs$positionChoices,
          selected = rv$graphConfig$graph$layersControl$position
        ),
        checkboxInput_MIRO("leaflc_collapsed", lang$adminMode$graphs$leafletOptions$layer$collapsed,
          value = rv$graphConfig$graph$layersControl$options$collapsed
        )
      ),
      tags$div(
        class = "cat-body cat-body-41", style = "display:none;",
        getFilterOptions()
      ),
      tags$div(
        class = "cat-body cat-body-42", style = "display:none;",
        getOuttype()
      )
    )
  )
})
getTimevisOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$graph$showZoom <<- checkNotFALSE(configuredWithThisTool, currentGraphConfig[["showZoom"]])
    rv$graphConfig$graph$fit <<- checkNotFALSE(configuredWithThisTool, currentGraphConfig[["fit"]])
    rv$graphConfig$graph$zoomFactor <<- checkLength(configuredWithThisTool, currentGraphConfig[["zoomFactor"]], 0.5)
    rv$graphConfig$graph$editable <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["editable"]])
    rv$graphConfig$graph$multiselect <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["multiselect"]])
    rv$graphConfig$graph$showCurrentTime <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["showCurrentTime"]])
    rv$graphConfig$graph$series <<- list()
    rv$graphConfig$graph$series[["1"]] <<- list(
      content = indices[[1]],
      start = indices[[1]],
      type = "box"
    )
  })
  tagList(
    tagList(
      tags$div(
        class = "cat-body cat-body-43",
        createArray(session, "timevis_series", lang$adminMode$graphs$timevisOptions$series,
          autoCreate = !isTRUE(configuredWithThisTool),
          class_outer = "array-wrapper-outer-graph", hr = FALSE
        ),
        createArray(session, "timevis_custom", lang$adminMode$graphs$timevisOptions$custom,
          autoCreate = FALSE,
          class_outer = "array-wrapper-outer-graph", hr = FALSE
        )
      ),
      tags$div(
        class = "cat-body cat-body-44", style = "display:none;",
        getFilterOptions()
      ),
      tags$div(
        class = "cat-body cat-body-45", style = "display:none;",
        checkboxInput_MIRO(
          "timevis_showZoom", lang$adminMode$graphs$timevisOptions$options$showZoom,
          rv$graphConfig$graph$showZoom
        ),
        numericInput("timevis_zoomFactor", lang$adminMode$graphs$timevisOptions$options$zoomFactor,
          min = 0, max = 1,
          value = rv$graphConfig$graph$zoomFactor, step = 0.1
        ),
        checkboxInput_MIRO(
          "timevis_fit", lang$adminMode$graphs$timevisOptions$options$fit,
          rv$graphConfig$graph$fit
        ),
        checkboxInput_MIRO(
          "timevis_editable", lang$adminMode$graphs$timevisOptions$options$editable,
          rv$graphConfig$graph$editable
        ),
        checkboxInput_MIRO(
          "timevis_multiselect", lang$adminMode$graphs$timevisOptions$options$multiselect,
          rv$graphConfig$graph$multiselect
        ),
        checkboxInput_MIRO(
          "timevis_showCurrentTime", lang$adminMode$graphs$timevisOptions$options$showCurrentTime,
          rv$graphConfig$graph$showCurrentTime
        ),
        getOuttype()
      )
    )
  )
})
getPivotOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rv$graphConfig$pivottable <<- NULL
    rv$graphConfig$pivottable$rows <<- checkLength(configuredWithThisTool, currentGraphConfig[["rows"]], NULL)
    rv$graphConfig$pivottable$cols <<- checkLength(configuredWithThisTool, currentGraphConfig[["cols"]], NULL)
    rv$graphConfig$pivottable$aggregatorName <<- checkLength(configuredWithThisTool, currentGraphConfig[["aggregatorName"]], "Sum")

    valOne <- if (isTRUE(configuredWithThisTool) &&
      length(currentGraphConfig[["vals"]][1]) &&
      !is.na(currentGraphConfig[["vals"]][1])) {
      currentGraphConfig[["vals"]][1]
    } # allows to configure empty values that would otherwise not be loaded empty at the next startup
    else if (!length(currentGraphConfig) && length(scalarIndices)) scalarIndices[[1]] else "_"
    valTwo <- if (isTRUE(configuredWithThisTool) &&
      length(currentGraphConfig[["vals"]][2]) &&
      !is.na(currentGraphConfig[["vals"]][2])) {
      currentGraphConfig[["vals"]][[2]]
    } else {
      NULL
    }
    if (identical(valTwo, NULL)) {
      rv$graphConfig$pivottable$vals <<- valOne
    } else {
      rv$graphConfig$pivottable$vals <<- list(valOne, valTwo)
    }
    rv$graphConfig$pivottable$rendererName <<- checkLength(configuredWithThisTool, currentGraphConfig[["rendererName"]], "Table")
    rv$graphConfig$pivottable$locale <<- checkLength(configuredWithThisTool, currentGraphConfig[["locale"]], "en")
    rv$graphConfig$pivottable$subtotals <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["subtotals"]])
  })
  tagList(
    tags$div(
      class = "cat-body cat-body-46",
      selectInput("pivot_rows", lang$adminMode$graphs$pivotOptions$rows,
        choices = indices, multiple = TRUE,
        selected = rv$graphConfig$pivottable$rows
      ),
      selectInput("pivot_cols", lang$adminMode$graphs$pivotOptions$cols,
        choices = indices, multiple = TRUE,
        selected = rv$graphConfig$pivottable$cols
      ),
      selectInput("pivot_aggregatorName", lang$adminMode$graphs$pivotOptions$aggregator,
        choices = langSpecificGraphs$aggregatorChoices,
        selected = rv$graphConfig$pivottable$aggregatorName
      ),
      tags$div(
        class = "shiny-input-container",
        style = "max-height:800px;max-height: 80vh;padding-right:30px;padding-left:40px;",
        conditionalPanel(
          condition = "input.pivot_aggregatorName && !input.pivot_aggregatorName.startsWith('Count')",
          selectInput("pivot_vals", lang$adminMode$graphs$pivotOptions$vals,
            choices = c("_", indices),
            selected = rv$graphConfig$pivottable$vals[1]
          )
        ),
        conditionalPanel(
          condition = "input.pivot_aggregatorName === 'Sum over Sum' || input.pivot_aggregatorName === '80% Upper Bound' ||
                     input.pivot_aggregatorName === '80% Lower Bound'",
          selectInput("pivot_vals2", lang$adminMode$graphs$pivotOptions$vals,
            choices = c("_", indices),
            selected = rv$graphConfig$pivottable$vals[2]
          )
        )
      ),
      selectInput("pivot_rendererName", lang$adminMode$graphs$pivotOptions$renderer,
        choices = langSpecificGraphs$rendererChoices,
        selected = rv$graphConfig$pivottable$rendererName
      )
    ),
    tags$div(
      class = "cat-body cat-body-47", style = "display:none;",
      selectInput("pivot_locale", lang$adminMode$graphs$pivotOptions$options$locale,
        choices = langSpecificGraphs$localeChoices,
        selected = rv$graphConfig$pivottable$locale
      ),
      checkboxInput_MIRO("pivot_subtotals", span(lang$adminMode$graphs$pivotOptions$options$subtotals, tags$a(href = "http://nagarajanchinnasamy.com/subtotal/", target = "_blank", "http://nagarajanchinnasamy.com/subtotal/")),
        value = rv$graphConfig$pivottable$subtotals
      )
    )
  )
})
getCustomOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices <- activeSymbol$indices
  scalarIndices <- indices[activeSymbol$indexTypes == "numeric"]
  isolate({
    rendererNameTmp <- checkLength(configuredWithThisTool, currentGraphConfig[["outType"]], NULL)
    rv$graphConfig$options <<- checkLength(configuredWithThisTool, currentGraphConfig[["options"]], list())

    externalRendererFiles <- existingRendererFiles[existingRendererFiles != paste0("mirorenderer_", tolower(activeSymbol$name))]

    if (length(rendererNameTmp) &&
      rendererNameTmp %in% externalRendererFiles) {
      externalRendererSelected <- rendererNameTmp
    } else {
      externalRendererSelected <- "-"
    }

    if (identical(input$customExternalSymbol, externalRendererSelected)) {
      skipLoadRenderer <<- FALSE
    } else {
      skipLoadRenderer <<- TRUE
    }

    externalRendererSymbols <- gsub("^mirorenderer\\_", "", externalRendererFiles)

    names(externalRendererFiles) <- externalRendererSymbols

    externalRendererFilesInvalid <- !externalRendererSymbols %in%
      c(
        unname(inputSymMultiDimChoices),
        unname(outputSymMultiDimChoices)
      )

    names(externalRendererFiles)[externalRendererFilesInvalid] <- paste(externalRendererFiles[externalRendererFilesInvalid], lang$adminMode$graphs$customOptions$symNotFoundSuffix)

    rendererFromExternalSymbol <<- FALSE

    if (length(rendererNameTmp)) {
      rv$graphConfig$outType <- rendererNameTmp
    } else {
      rv$graphConfig$outType <- paste0("mirorenderer_", tolower(activeSymbol$name))
    }

    if (length(rendererNameTmp) &&
      rendererNameTmp %in% existingRendererFiles) {
      if (rendererNameTmp %in% externalRendererFiles) {
        if (rendererNameTmp %in% externalRendererFiles[externalRendererFilesInvalid]) {
          # renderer from nonexistent symbol
          rv$graphConfig$outType <- paste0("mirorenderer_", tolower(activeSymbol$name))
        } else {
          # renderer uses code from other (existent) symbol
          rendererFromExternalSymbol <<- TRUE
        }
      }
      rendererFunctions <- list(output = "", renderer = "")
      tryCatch(
        {
          rendererFunctions <- getRendererFunctions(rendererNameTmp)
        },
        error = function(e) {
          flog.warn(
            "Problems loading custom renderer. Error message: %s",
            conditionMessage(e)
          )
        }
      )
    } else {
      rendererFunctions <- list(output = "", renderer = "")
    }

    rv$graphConfig$packages <<- checkLength(configuredWithThisTool, currentGraphConfig[["packages"]], NULL)
    rv$graphConfig$additionalData <<- checkLength(configuredWithThisTool, currentGraphConfig[["additionalData"]], NULL)
  })
  tagList(
    tags$div(
      class = "cat-body cat-body-48",
      selectInput(
        input = "customExternalSymbol",
        label = lang$adminMode$graphs$customOptions$externalRenderer,
        choices = c("-", externalRendererFiles),
        selected = externalRendererSelected
      ),
      tags$div(id = "customOutputBoilerplate", class = "shiny-text-output custom-renderer-boilerplate "),
      aceEditorFullscreenButton(),
      aceEditor(
        outputId = "customOutputFunction",
        value = rendererFunctions$output,
        debounce = 100,
        mode = "r",
        minLines = 1,
        maxLines = 100,
        autoScrollEditorIntoView = TRUE,
        theme = if (isInDarkMode) "ambiance" else "chrome",
        autoComplete = "live",
        autoCompleters = c("rlang", "snippet", "text", "keyword")
      ),
      tags$div(class = "custom-renderer-boilerplate", "}"),
      tags$div(id = "customRenderBoilerplate", class = "custom-renderer-boilerplate shiny-text-output"),
      aceEditorFullscreenButton(),
      aceEditor(
        outputId = "customRenderFunction",
        value = rendererFunctions$renderer,
        debounce = 100,
        mode = "r",
        minLines = 1,
        maxLines = 100,
        autoScrollEditorIntoView = TRUE,
        theme = if (isInDarkMode) "ambiance" else "chrome",
        autoComplete = "live",
        autoCompleters = c("rlang")
      ),
      tags$div(class = "custom-renderer-boilerplate", "}")
    ),
    tags$div(
      class = "cat-body cat-body-48a", style = "display:none;",
      if (activeSymbol$id > length(modelIn)) {
        # active symbol is an output symbol
        selectizeInput("customAdditionalData", lang$adminMode$graphs$customOptions$additionalData,
          choices = setNames(
            list(
              c(inputSymMultiDimChoices),
              c(outputSymMultiDimChoices)[-(activeSymbol$id - length(modelIn))]
            ),
            c(
              lang$adminMode$graphs$ui$input,
              lang$adminMode$graphs$ui$output
            )
          ),
          selected = rv$graphConfig$additionalData,
          multiple = TRUE, options = list("dropdownParent" = "body")
        )
      },
      tags$div(
        id = "customPackagesModified", class = "err-msg",
        lang$adminMode$uiR$restartRequired,
        style = "display:none"
      ),
      selectizeInput("customPackages", tags$div(
        lang$adminMode$graphs$customOptions$packages,
        tags$a("",
          title = lang$adminMode$general$ui$tooltipDocs, class = "info-wrapper", href = "https://gams.com/miro/customize.html#custom-renderers",
          tags$span(
            class = "fas fa-info-circle", class = "info-icon",
            role = "presentation",
            `aria-label` = "More information"
          ), target = "_blank"
        )
      ),
      choices = if (length(rv$graphConfig$packages)) rv$graphConfig$packages else c(),
      selected = rv$graphConfig$packages,
      multiple = TRUE, options = list("create" = TRUE, "persist" = FALSE)
      ),
      tags$label(lang$adminMode$graphs$customOptions$options, `for` = "cutomOptions"),
      aceEditor("cutomOptions",
        mode = "json",
        value = if (length(rv$graphConfig$options)) {
          toJSON(rv$graphConfig$options, pretty = TRUE, auto_unbox = TRUE, null = "null")
        } else {
          "{}"
        },
        debounce = 1000,
        minLines = 1,
        maxLines = 30,
        autoScrollEditorIntoView = TRUE,
        theme = if (isInDarkMode) "ambiance" else "chrome",
        autoComplete = "live"
      )
    )
  )
})
getColorPivotOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices <- activeSymbol$indices
  isolate({
    rv$graphConfig$graph$color <<- checkLength(configuredWithThisTool, currentGraphConfig[["color"]], NULL)
  })
  tagList(
    selectInput("chart_color", tags$div(
      lang$adminMode$graphs$chartOptions$color,
      tags$a("",
        class = "info-wrapper", href = "https://gams.com/miro/charts.html#group-domain",
        tags$span(
          class = "fas fa-info-circle", class = "info-icon",
          role = "presentation",
          `aria-label` = "More information"
        ), target = "_blank"
      )
    ),
    choices = c("_", indices),
    selected = rv$graphConfig$graph$color
    )
  )
})
getFilterOptions <- reactive({
  rv$initData
  rv$refreshContent
  indices <- activeSymbol$indices
  setIndices <- indices[activeSymbol$indexTypes == "string"]
  isolate({
    if (isTRUE(configuredWithThisTool) && length(currentGraphConfig[["filter"]][["col"]])) {
      rv$graphConfig$graph$filter$col <<- checkLength(configuredWithThisTool, currentGraphConfig[["filter"]][["col"]], setIndices[[1]])
      rv$graphConfig$graph$filter$label <<- checkLength(configuredWithThisTool, currentGraphConfig[["filter"]][["label"]], "")
      rv$graphConfig$graph$filter$multiple <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["filter"]][["multiple"]])
      rv$graphConfig$graph$filter$date <<- checkTRUE(configuredWithThisTool, currentGraphConfig[["filter"]][["date"]])
    } else {
      rv$graphConfig$graph$filter <- NULL
    }
  })
  if (input$chart_tool %in% plotlyChartTools) {
    chartToolTmp <- "plotly"
  } else {
    chartToolTmp <- input$chart_tool
  }
  if (isFALSE(input$filter_dim)) {
    hideEl(session, paste0("#preview_output_", chartToolTmp, "-data_filter"))
  } else {
    showEl(session, paste0("#preview_output_", chartToolTmp, "-data_filter"))
  }
  tagList(
    tags$label(
      class = "cb-label info-position", "for" = "filter_dim",
      tags$div(lang$adminMode$graphs$filterOptions$filter, tags$a("",
        class = "info-wrapper", href = "https://gams.com/miro/charts.html#filter-option",
        tags$span(
          class = "fas fa-info-circle", class = "info-icon",
          role = "presentation",
          `aria-label` = "More information"
        ), target = "_blank"
      ))
    ),
    tags$div(
      tags$label(
        class = "checkbox-material",
        checkboxInput("filter_dim", value = isTRUE(length(rv$graphConfig$graph$filter$col) > 0L), label = NULL)
      )
    ),
    tags$div(
      style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;padding-left:40px;",
      conditionalPanel(
        condition = "input.filter_dim == true",
        selectInput("filter_col", lang$adminMode$graphs$filterOptions$dimension,
          choices = setIndices,
          selected = rv$graphConfig$graph$filter$col
        ),
        textInput("filter_label", lang$adminMode$graphs$filterOptions$label,
          placeholder = lang$adminMode$graphs$filterOptions$placeholder,
          value = rv$graphConfig$graph$filter$label
        ),
        checkboxInput_MIRO("filter_multiple", lang$adminMode$graphs$filterOptions$multiple,
          value = rv$graphConfig$graph$filter$multiple
        ),
        checkboxInput_MIRO("filter_date", lang$adminMode$graphs$filterOptions$date,
          value = rv$graphConfig$graph$filter$date
        )
      )
    )
  )
})
# custom renderer
aceAutocomplete(inputId = "customOutputFunction")
aceAutocomplete(inputId = "customRenderFunction")

customOutputFunctionName <- reactive(paste0(tolower(rv$graphConfig$outType), "Output"))
customRendererFunctionName <- reactive(paste0("render", toupper(substr(rv$graphConfig$outType, 1, 1)), substr(tolower(rv$graphConfig$outType), 2, nchar(rv$graphConfig$outType))))

getRendererFunctions <- function(rendererName) {
  rendererPath <- file.path(customRendererDir, paste0(rendererName, ".R"))
  if (!file.exists(rendererPath)) {
    return(list(output = "", renderer = ""))
  }
  rendererContentRaw <- read_file(rendererPath, locale = locale(encoding = "UTF-8"))

  # Need to detect the functions by looking for ^*Output <- * and ^render* <-
  funOutputStr <- paste0(tolower(rendererName), "Output")
  funOutputBody <- parseFunctionBody(rendererContentRaw, funOutputStr)
  if (length(funOutputBody) && startsWith(trimws(funOutputBody[1]), "ns")) {
    funOutputBody <- funOutputBody[-1]
  }

  funRenderStr <- paste0(
    "render", toupper(substr(rendererName, 1, 1)),
    substr(tolower(rendererName), 2, nchar(rendererName))
  )
  funRenderBody <- parseFunctionBody(rendererContentRaw, funRenderStr)
  return(list(output = funOutputBody, renderer = funRenderBody))
}
output$customOutputBoilerplate <- renderText({
  paste0(
    customOutputFunctionName(),
    " <- function(id, height = NULL, options = NULL, path = NULL){\n   ns <- NS(id)"
  )
})
output$customRenderBoilerplate <- renderText({
  paste0(
    customRendererFunctionName(),
    " <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){"
  )
})
output[["preview_custom_renderer"]] <- renderUI({
  req(input$btUpdateCustomRendererOutput)
  outputFunction <- isolate(paste0(
    customOutputFunctionName(),
    " <- function(id, height = NULL, options = NULL, path = NULL){\n    ns <- NS(id)\n",
    isolate(input$customOutputFunction),
    "}"
  ))
  eval(parse(text = outputFunction, encoding = "UTF-8"),
    envir = customRendererEvalEnv
  )("preview_output_custom", height = 400,
    options = rv$graphConfig$options,
    path = customRendererDir)
})


observe(
  {
    req(rv$graphConfig$graph$tool, activeSymbol$id > 0L, allDataAvailable)
    if (identical(rv$graphConfig$graph$tool, "custom")) {
      force(input$btUpdateCustomRendererPreview)
    }
    if (identical(rv$graphConfig$graph$tool, "plotly") && identical(length(rv$graphConfig$graph$type), 0L)) {
      return()
    }
    if (activeSymbol$id > length(modelIn)) {
      symId <- activeSymbol$id - length(modelIn)
      metadata <- list(
        headers = modelOut[[symId]]$headers,
        symtype = modelOut[[symId]]$symtype,
        symname = names(modelOut)[symId]
      )
      data <- modelOutputData[[symId]]
    } else {
      symId <- activeSymbol$id
      metadata <- list(
        headers = modelIn[[symId]]$headers,
        symtype = modelIn[[symId]]$symtype,
        symname = names(modelIn)[symId]
      )
      data <- modelInputData[[symId]]
    }
    tryCatch(
      {
        if (isolate(rv$graphConfig$graph$tool) == "plotly") {
          if (identical(rv$graphConfig$graph$type, "pie") &&
            nrow(data) > 100) {
            showEl(session, "#pieValues")
            hideEl(session, "#preview-content-plotly")
          } else {
            callModule(renderData, "preview_output_plotly",
              type = "graph",
              data = data, configData = configScalars,
              graphOptions = rv$graphConfig$graph,
              roundPrecision = roundPrecision, modelDir = modelDir
            )
            showEl(session, "#preview-content-plotly")
            hideEl(session, "#pieValues")
          }
          hideEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-valuebox")
          hideEl(session, "#preview-content-custom")
        } else if (isolate(rv$graphConfig$graph$tool) == "dygraphs") {
          callModule(renderData, "preview_output_dygraphs",
            type = "graph",
            data = data, configData = configScalars,
            graphOptions = rv$graphConfig$graph,
            roundPrecision = roundPrecision, modelDir = modelDir
          )
          showEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-plotly")
          hideEl(session, "#pieValues")
          hideEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-valuebox")
          hideEl(session, "#preview-content-custom")
        } else if (isolate(rv$graphConfig$graph$tool) == "pivot") {
          callModule(renderData, "preview_output_pivot",
            type = "pivot",
            data = data, configData = configScalars,
            pivotOptions = rv$graphConfig$pivottable,
            roundPrecision = 2, modelDir = modelDir
          )
          showEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-plotly")
          hideEl(session, "#pieValues")
          hideEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-valuebox")
          hideEl(session, "#preview-content-custom")
        } else if (isolate(rv$graphConfig$graph$tool) == "miropivot") {
          for (el in ls(envir = customRendererEnv)) {
            if ("Observer" %in% class(customRendererEnv[[el]])) {
              customRendererEnv[[el]]$destroy()
            }
          }
          aggregationFunctions <- if (identical(metadata$symtype, "set")) {
            setNames(
              c("count", "min"),
              c(
                lang$renderers$miroPivot$aggregationFunctions$count,
                lang$renderers$miroPivot$aggregationFunctions$min
              )
            )
          } else {
            setNames(
              c("sum", "count", "mean", "median", "min", "max"),
              c(
                lang$renderers$miroPivot$aggregationFunctions$sum,
                lang$renderers$miroPivot$aggregationFunctions$count,
                lang$renderers$miroPivot$aggregationFunctions$mean,
                lang$renderers$miroPivot$aggregationFunctions$median,
                lang$renderers$miroPivot$aggregationFunctions$min,
                lang$renderers$miroPivot$aggregationFunctions$max
              )
            )
          }
          selectedAggregationFuction <- currentGraphConfig$options[["aggregationFunction"]]
          if (!length(selectedAggregationFuction) ||
            !selectedAggregationFuction %in% aggregationFunctions) {
            selectedAggregationFuction <- aggregationFunctions[1]
          }
          updateSelectInput(session, "preview_output_miropivot-miroPivot-aggregationFunction",
            choices = aggregationFunctions,
            selected = selectedAggregationFuction
          )
          miropivotOptions <- currentGraphConfig$options
          miropivotOptions$emptyUEL <- rv$graphConfig$graph$options$emptyUEL
          miropivotOptions$enableHideEmptyCols <- TRUE
          miropivotOptions$hidepivotcontrols <- rv$graphConfig$graph$options$hidepivotcontrols
          miropivotOptions$fixedColumns <- rv$graphConfig$graph$options$fixedColumns
          miropivotOptions$externalDefaultView <- rv$graphConfig$graph$options$externalDefaultView
          if (!identical(input[["preview_output_miropivot-miroPivot-symbol_name"]], activeSymbol$name)) {
            return()
          }
          isolate(callModule(renderData, "preview_output_miropivot",
            type = "miropivot",
            data = data, rendererEnv = customRendererEnv,
            customOptions = c(
              list(
                "_metadata_" = metadata,
                resetOnInit = TRUE
              ),
              miropivotOptions
            ),
            roundPrecision = 2, modelDir = modelDir, views = views
          ))
          showEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-plotly")
          hideEl(session, "#pieValues")
          hideEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-valuebox")
          hideEl(session, "#preview-content-custom")
        } else if (isolate(rv$graphConfig$graph$tool) == "timevis") {
          callModule(renderData, "preview_output_timevis",
            type = "graph",
            data = data, configData = configScalars,
            graphOptions = rv$graphConfig$graph,
            roundPrecision = roundPrecision, modelDir = modelDir
          )
          showEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-plotly")
          hideEl(session, "#pieValues")
          hideEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-valuebox")
          hideEl(session, "#preview-content-custom")
        } else if (isolate(rv$graphConfig$graph$tool) == "valuebox") {
          customOptionstmp <- as.vector(rv$graphConfig$options, mode = "list")
          customOptionstmp$count <- length(modelOut[[scalarsOutName]]$symnames)
          dataVisible <- data[!tolower(data[[1]]) %in% tolower(configJSON$hiddenOutputScalars), ]
          callModule(renderData, "preview_output_valuebox",
            type = "valuebox",
            data = dataVisible, configData = configScalars,
            customOptions = customOptionstmp,
            modelDir = modelDir
          )
          showEl(session, "#preview-content-valuebox")
          hideEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-plotly")
          hideEl(session, "#pieValues")
          hideEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-custom")
        } else if (isolate(rv$graphConfig$graph$tool) == "custom") {
          showEl(session, "#preview-content-custom")
          hideEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-plotly")
          hideEl(session, "#pieValues")
          hideEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-valuebox")
          for (el in ls(envir = customRendererEnv)) {
            if ("Observer" %in% class(customRendererEnv[[el]])) {
              customRendererEnv[[el]]$destroy()
            }
          }
          if (length(rv$graphConfig$additionalData)) {
            additionalDataIds <- match(
              rv$graphConfig$additionalData,
              names(modelOut)
            )
            additionalDataIds[is.na(additionalDataIds)] <- match(
              rv$graphConfig$additionalData[is.na(additionalDataIds)],
              names(modelIn)
            ) + length(modelOut)
            additionalDataIds <- c(activeSymbol$id - length(modelIn), additionalDataIds)
            data <- c(modelOutputData, modelInputData)[additionalDataIds]
            names(data) <- c(names(modelOut), names(modelIn))[additionalDataIds]
          }
          if (!identical(input[["preview_output_custom-symbol_name"]], activeSymbol$name)) {
            return()
          }
          local({
            customRendererFunction <- eval(parse(text = isolate(paste0(
              customRendererFunctionName(),
              " <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){\n",
              input$customRenderFunction,
              "\n}"
            )), encoding = "UTF-8"), envir = customRendererEvalEnv)
            callModule(customRendererFunction, "preview_output_custom",
              data,
              options = rv$graphConfig$options,
              path = customRendererDir, rendererEnv = customRendererEnv,
              views = views, attachments = attachments,
              outputScalarsFull = configScalars
            )
          })
        } else {
          callModule(renderData, "preview_output_leaflet",
            type = "graph",
            data = data, configData = configScalars,
            graphOptions = rv$graphConfig$graph,
            roundPrecision = roundPrecision, modelDir = modelDir
          )
          showEl(session, "#preview-content-leaflet")
          hideEl(session, "#preview-content-plotly")
          hideEl(session, "#pieValues")
          hideEl(session, "#preview-content-dygraphs")
          hideEl(session, "#preview-content-pivot")
          hideEl(session, "#preview-content-miropivot")
          hideEl(session, "#preview-content-timevis")
          hideEl(session, "#preview-content-valuebox")
          hideEl(session, "#preview-content-custom")
        }
        hideEl(session, "#preview-error")
      },
      error = function(e) {
        hideEl(session, "#preview-content-dygraphs")
        hideEl(session, "#preview-content-plotly")
        hideEl(session, "#pieValues")
        hideEl(session, "#preview-content-leaflet")
        hideEl(session, "#preview-content-pivot")
        hideEl(session, "#preview-content-miropivot")
        hideEl(session, "#preview-content-timevis")
        hideEl(session, "#preview-content-valuebox")
        showElReplaceTxt(
          session, "#preview-error",
          sprintf(lang$adminMode$graphs$ui$previewError, conditionMessage(e))
        )
      }
    )
  },
  priority = -1000
)

#  ==============================
#          SAVE JSON
#  ==============================
observeEvent(input$saveGraph, {
  req(nchar(activeSymbol$name) > 0L)
  errMsg <- validateGraphConfig(rv$graphConfig)
  if (nchar(errMsg)) {
    showHideEl(session, "#graphValidationErr", 5000L, errMsg)
    return()
  }
  if (tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))) {
    showModal(modalDialog(
      title = lang$adminMode$graphs$saveJson$warnTitle, sprintf(lang$adminMode$graphs$saveJson$warnContent, activeSymbol$name),
      footer = tagList(
        modalButton(lang$adminMode$graphs$saveJson$cancel),
        actionButton("saveGraphConfirm", lang$adminMode$graphs$saveJson$overwrite)
      )
    ))
    return()
  }
  rv$saveGraphConfirm <- rv$saveGraphConfirm + 1L
})
observeEvent(input$saveGraphConfirm, rv$saveGraphConfirm <- rv$saveGraphConfirm + 1L)
observeEvent(rv$saveGraphConfirm, {
  req(rv$saveGraphConfirm > 0L)
  # remove all symbols with same name to avoid case sensitivity issues
  configJSON$dataRendering[activeSymbol$name == tolower(names(configJSON$dataRendering))] <<- NULL
  configJSON$dataRendering[[activeSymbol$name]] <<- rv$graphConfig
  configJSON$dataRendering[[activeSymbol$name]]$height <<- 700
  if (is.null(rv$graphConfig$graph$tool)) {
    flog.error("Problems fetching configuration.")
    return()
  }
  if (rv$graphConfig$graph$tool == "miropivot") {
    configJSON$dataRendering[[activeSymbol$name]]$graph <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$pivottable <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$options <<- list(
      aggregationFunction = input[["preview_output_miropivot-miroPivot-aggregationFunction"]],
      pivotRenderer = input[["preview_output_miropivot-miroPivot-pivotRenderer"]],
      enableHideEmptyCols = isTRUE(input$miropivot_enableHideEmptyCols),
      hideEmptyCols = isTRUE(input[["preview_output_miropivot-miroPivot-hideEmptyCols"]]),
      hidePivotControls = isTRUE(input$miropivot_hidePivotControls),
      fixedColumns = isTRUE(input$miropivot_fixedColumns)
    )
    if (length(rv$graphConfig$graph$options$emptyUEL)) {
      configJSON$dataRendering[[activeSymbol$name]]$options$emptyUEL <<- rv$graphConfig$graph$options$emptyUEL
    }
    if (length(rv$graphConfig$graph$options$externalDefaultView)) {
      configJSON$dataRendering[[activeSymbol$name]]$options$externalDefaultView <<- rv$graphConfig$graph$options$externalDefaultView
    }
    for (indexEl in list(c("rows", "rowIndexList"))) {
      indexVal <- input[[paste0("preview_output_miropivot-miroPivot-", indexEl[[2]])]]
      if (length(indexVal)) {
        configJSON$dataRendering[[activeSymbol$name]]$options[[indexEl[[1]]]] <<- indexVal
      }
    }
    for (indexEl in list(
      c("aggregations", "aggregationIndexList"),
      c("filter", "filterIndexList"),
      c("cols", "colIndexList")
    )) {
      indexVal <- input[[paste0("preview_output_miropivot-miroPivot-", indexEl[[2]])]]
      if (length(indexVal)) {
        filterElList <- lapply(indexVal, function(el) {
          return(input[[paste0("preview_output_miropivot-miroPivot-filter_", el)]])
        })
        names(filterElList) <- indexVal
        configJSON$dataRendering[[activeSymbol$name]]$options[[indexEl[[1]]]] <<- filterElList
      }
    }
    configJSON$dataRendering[[activeSymbol$name]]$outType <<- "miroPivot"
    saveGlobalViews()
  } else if (rv$graphConfig$graph$tool == "pivot") {
    configJSON$dataRendering[[activeSymbol$name]]$graph <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$options <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$outType <<- "pivot"
  } else if (rv$graphConfig$graph$tool == "valuebox") {
    configJSON$dataRendering[[activeSymbol$name]]$graph <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$height <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$pivottable <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$outType <<- "valueBox"
  } else if (rv$graphConfig$graph$tool == "custom") {
    configJSON$dataRendering[[activeSymbol$name]]$graph <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$height <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$pivottable <<- NULL
    if (!length(configJSON$dataRendering[[activeSymbol$name]]$options)) {
      configJSON$dataRendering[[activeSymbol$name]]$options <<- NULL
    }
    customRendererName <- paste0("mirorenderer_", tolower(activeSymbol$name))
    if (rendererFromExternalSymbol) {
      customRendererName <- configJSON$dataRendering[[activeSymbol$name]]$outType
    }
    if (identical(
      configJSON$dataRendering[[activeSymbol$name]]$outType,
      customRendererName
    )) {
      if (!dir.exists(customRendererDir) && !dir.create(customRendererDir)) {
        flog.warn("Could not create custom renderer directory: %s", customRendererDir)
        return(showErrorMsg(
          lang$errMsg$fileWrite$title,
          sprintf(lang$errMsg$fileWrite$desc, customRendererDir)
        ))
      }
      con <- NULL
      rendererFilePathTmp <- file.path(customRendererDir, paste0(customRendererName, ".R"))
      if (tryCatch(
        {
          con <- file(rendererFilePathTmp, open = "wb", encoding = "UTF-8")
          readr::write_file(
            enc2utf8(paste0(
              customOutputFunctionName(),
              " <- function(id, height = NULL, options = NULL, path = NULL){\n    ns <- NS(id)\n",
              isolate(input$customOutputFunction),
              "\n}\n\n",
              customRendererFunctionName(),
              " <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){\n",
              input$customRenderFunction,
              "\n}\n"
            )),
            con
          )
          FALSE
        },
        error = function(e) {
          flog.warn(
            "Problems writing custom renderer file to: %s. Error message: %s",
            rendererFilePathTmp,
            conditionMessage(e)
          )
          showErrorMsg(
            lang$errMsg$fileWrite$title,
            sprintf(lang$errMsg$fileWrite$desc, rendererFilePathTmp)
          )
          return(TRUE)
        },
        finally = {
          if (con) {
            close(con)
          }
        }
      )) {
        return()
      }
      if (!customRendererName %in% existingRendererFiles) {
        existingRendererFiles <<- c(existingRendererFiles, customRendererName)
      }
    }
  } else {
    configJSON$dataRendering[[activeSymbol$name]]$pivottable <<- NULL
    configJSON$dataRendering[[activeSymbol$name]]$options <<- NULL
    if (!identical(rv$graphConfig$graph$tool, "leaflet")) {
      configJSON$dataRendering[[activeSymbol$name]]$graph$layersControl <<- NULL
    }
    configJSON$dataRendering[[activeSymbol$name]]$outType <<- if (isTRUE(input$outType)) "dtGraph" else "graph"
  }
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  removeModal()
  showHideEl(session, "#graphUpdateSuccess", 4000L)
  showEl(session, "#deleteGraph")
  getCurrentGraphConfig()
  # necessary for setting configuredWithThisTool correctly
  newChartTool <<- input$chart_tool
})

observeEvent(input$deleteGraph, {
  req(nchar(activeSymbol$name) > 0L)

  if (!tolower(activeSymbol$name) %in% tolower(names(configJSON$dataRendering))) {
    return()
  }
  showModal(modalDialog(
    title = lang$adminMode$graphs$removeDialog$title, lang$adminMode$graphs$removeDialog$message,
    footer = tagList(
      modalButton(lang$adminMode$graphs$removeDialog$cancel),
      actionButton("deleteGraphConfirm", lang$adminMode$graphs$removeDialog$confirm)
    )
  ))
})
observeEvent(input$deleteGraphConfirm, {
  graphId <- match(tolower(activeSymbol$name), tolower(names(configJSON$dataRendering)))
  if (is.na(graphId)) {
    return()
  }
  customRendererName <- paste0("mirorenderer_", tolower(activeSymbol$name))
  if (identical(
    configJSON$dataRendering[[graphId]]$outType,
    customRendererName
  )) {
    unlink(file.path(
      customRendererDir,
      paste0(customRendererName, ".R")
    ))
    existingRendererFiles <<- existingRendererFiles[existingRendererFiles != customRendererName]
  }
  removeGlobalViews()
  configJSON$dataRendering[[graphId]] <<- NULL
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  removeModal()
  showHideEl(session, "#graphUpdateSuccess", 4000L)
  hideEl(session, "#deleteGraph")
  # reset to default settings and refresh rv's
  axisOptionsGlobal <<- list(y = 1, y2 = 0)
  currentGraphConfig <<- NULL
  currentGraphLabel <- ""
  isInJSON <<- FALSE
  newChartTool <<- "pie"
  if (isTRUE(tableSymbol)) {
    tableSymbol <<- FALSE
    updateSelectInput(session, "chart_tool", selected = newChartTool)
    if (identical(tolower(input$table_symbol), tolower(activeSymbol$name))) {
      updateCheckboxInput(session, "outputTable_noGraph", value = FALSE)
    }
  }
  rv$refreshOptions <- rv$refreshOptions + 1L
})
