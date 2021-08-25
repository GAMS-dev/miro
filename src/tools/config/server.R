source(file.path("tools", "config", "util.R"))
source("./components/db_migrator.R")
dbMigrator <- DbMigrator$new(db)

appDisconnected <- FALSE
configJSONFileName <- paste0(currentModelDir, .Platform$file.sep, "conf_", modelName,
                             .Platform$file.sep, modelName, ".json")
dateFormatChoices <- c("1910-06-22" = "yyyy-mm-dd", "22.06.1910" = "dd.mm.yyyy")

outputSymMultiDimChoices <- character(0L)
if(length(modelOut)){
  outputSymMultiDimChoices <- setNames(names(modelOut),
                                       paste0(names(modelOut), ": ", modelOutAlias))
}

modelInRawTmp <- modelInRaw
if(scalarsFileName %in% names(modelInRaw) &&
   !scalarsFileName %in% names(modelIn)){
  modelInRawTmp <- modelInRaw[names(modelInRaw) != scalarsFileName]
}
inputSymMultiDim <- setNames(names(modelInRawTmp),
                             vapply(modelInRawTmp, "[[",
                                    character(1L), "alias", USE.NAMES = FALSE))
inputSymMultiDimChoices <- setNames(names(modelInRawTmp), vapply(seq_along(modelInRawTmp), function(idx){
  paste0(names(modelInRawTmp)[[idx]], ": ", modelInRawTmp[[idx]][["alias"]])}, character(1L), USE.NAMES = FALSE))

inputSymHeaders <- lapply(inputSymMultiDim, function(el){
  headers <- modelInRaw[[el]]$headers
  return(setNames(names(headers),
                  vapply(headers,
                         "[[", character(1L), "alias", USE.NAMES = FALSE)))
})
inputSymHeaderChoices <- lapply(inputSymMultiDim, function(el){
  headers <- modelInRaw[[el]]$headers
  return(setNames(names(headers), vapply(seq_along(headers), function(idx){
    paste0(names(headers)[idx], ": ", headers[[idx]][["alias"]])}, character(1L),  USE.NAMES = FALSE)))
})

widgetSymbols <- c(unlist(lapply(seq_along(modelIn), function(idx){
  if(identical(modelIn[[idx]][["symtype"]], "set") &&
     length(modelIn[[idx]]$headers) == 2L){
    return(names(modelIn)[idx])
  }else if(length(modelIn[[idx]][["headers"]])){
    return(NULL)
  }
  return(names(modelIn)[idx])
}), use.names = FALSE),
if(length(modelIn[[scalarsFileName]]))
  modelIn[[scalarsFileName]]$symnames)

if(length(widgetSymbols)){
  widgetSymbolIds <- match(widgetSymbols, names(modelIn))
  widgetSymbolIds <- widgetSymbolIds[!is.na(widgetSymbolIds)]
  widgetSymbolsChoices <- setNames(widgetSymbols, paste0(widgetSymbols, ": ", c(
    modelInAlias[widgetSymbolIds],
    if(length(modelIn[[scalarsFileName]]))
      modelIn[[scalarsFileName]]$symtext
  )))
  widgetSymbols <- setNames(widgetSymbols, c(
    modelInAlias[widgetSymbolIds],
    if(length(modelIn[[scalarsFileName]]))
      modelIn[[scalarsFileName]]$symtext
  ))
}else{
  widgetSymbolsChoices <- character(0L)
  widgetSymbols <- character(0L)
}
if(length(inputSymHeaders)){
  names(inputSymHeaders) <- unname(inputSymMultiDim)
}else{
  inputSymHeaders <- character(0L)
}

outputSymHeaders <- lapply(modelOut, function(el){
  vapply(el$headers, "[[",
    character(1L), "alias", USE.NAMES = FALSE)
})
outputSymHeaderChoices <- lapply(modelOut, function(el){
  vapply(seq_along(el$headers),  function(idx){
    paste0(names(el$headers)[[idx]], ": ", el$headers[[idx]][["alias"]])},
    character(1L), USE.NAMES = FALSE)
})
allInputSymHeaders <- setNames(unlist(inputSymHeaders, use.names = FALSE),
                               unlist(lapply(inputSymHeaders, names), use.names = FALSE))
allInputSymHeaders <- allInputSymHeaders[!duplicated(allInputSymHeaders)]

configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
                                                  simplifyDataFrame = FALSE,
                                                  simplifyMatrix = FALSE))
# remove invalid symbols from overwriteAliases and overwriteHeaderAliases
if(length(invalidAliases)){
  configJSON[["overwriteAliases"]][invalidAliases] <- NULL
}
if(length(invalidHeaderAliases)){
  configJSON[["overwriteHeaderAliases"]][invalidHeaderAliases] <- NULL
}
source(file.path("components", "md_parser.R"), local = TRUE)
markdownParser <- MarkdownParser$new()

inputWidgets <- names(modelIn)[vapply(modelIn, function(el){
  if(el$type %in% c("hot", "dt", "custom")){
    return(FALSE)
  }
  return(TRUE)
}, logical(1L), USE.NAMES = FALSE)]
inputWidgetAliases <- vapply(seq_along(configJSON$inputWidgets)[match(inputWidgets, names(configJSON$inputWidgets))],
                             function(widgetId){
                               if(length(configJSON$inputWidgets[[widgetId]]$alias)){
                                 return(configJSON$inputWidgets[[widgetId]]$alias)
                               }
                               return(names(configJSON$inputWidgets)[widgetId])
                             }, character(1L), USE.NAMES = FALSE)

server_admin <- function(input, output, session){
  rv <- reactiveValues(plotly_type = 0L, saveGraphConfirm = 0L, resetRE = 0L,
                       graphConfig = list(outType = "graph", graph = list()),
                       widgetConfig = list(), generalConfig = list(), customLogoChanged = 1L,
                       initData = FALSE, refreshContent = 0L, widget_type = 0L, widget_symbol = 0L,
                       saveWidgetConfirm = 0L, updateLeafletGroups = 0L,
                       saveTableConfirm = 0L, widgetTableConfig = list(), table_symbol = 0L,
                       reset_table_input = 0L, refreshOptions = 0L)

  isInDarkMode <- FALSE

  observe(isInDarkMode <<- isTRUE(input$isInDarkMode))

  xlsio <- XlsIO$new()
  scenData <- ScenData$new(db = db,
                           scenDataTemplate = scenDataTemplate,
                           hiddenOutputScalars = config$hiddenOutputScalars)
  session$sendCustomMessage("gms-setGAMSSymbols",
                            list(gamsSymbols = list(inSym = unname(inputSymMultiDim),
                                                    inAlias = names(inputSymMultiDim),
                                                    inWid = inputWidgets,
                                                    inWidAlias = inputWidgetAliases,
                                                    outSym = names(modelOut),
                                                    outAlias = modelOutAlias),
                                 lang = lang$adminMode$graphs$js))
  # ------------------------------------------------------
  #     General settings
  # ------------------------------------------------------
  source(file.path("tools", "config", "cg_general.R"), local = TRUE)
  # ------------------------------------------------------
  #     Input widgets
  # ------------------------------------------------------
  source(file.path("tools", "config", "cg_widgets.R"), local = TRUE)
  # ------------------------------------------------------
  #     Table settings
  # ------------------------------------------------------
  source(file.path("tools", "config", "cg_tables.R"), local = TRUE)
  # ------------------------------------------------------
  #     CUSTOMIZE GRAPHS
  # ------------------------------------------------------
  source(file.path("tools", "config", "cg_graphs.R"), local = TRUE)
  # ------------------------------------------------------
  #     DB MANAGEMENT
  # ------------------------------------------------------
  source(file.path("tools", "config", "db_management.R"), local = TRUE)

  if(length(invalidWidgetsToRender) || length(invalidGraphsToRender)){
    showModal(modalDialog(
      title = lang$adminMode$invalidSymbolsDialog$title,
      tags$div(class = "gmsalert gmsalert-success center-alert", id = "invalidSymbolsDialogSuccess",
               lang$adminMode$invalidSymbolsDialog$msgSuccess),
      sprintf(lang$adminMode$invalidSymbolsDialog$desc,
              paste0(invalidGraphsToRender, collapse = ", "),
              paste0(invalidWidgetsToRender, collapse = ", ")),
      if(length(invalidWidgetsToRender)){
        tags$div(style = "margin:10px;",
                 actionButton("btRemoveInvalidWidgets",
                              label = lang$adminMode$invalidSymbolsDialog$btRemoveWidgets))
      },
      if(length(invalidGraphsToRender)){
        tags$div(style = "margin:10px;",
                 actionButton("btRemoveInvalidGraphs",
                              label = lang$adminMode$invalidSymbolsDialog$btRemoveGraphs))
      },
      footer = tagList(
        modalButton(lang$adminMode$invalidSymbolsDialog$btCancel)),
      fade = TRUE, easyClose = FALSE
    ))
    observeEvent(input$btRemoveInvalidWidgets, {
      if(length(invalidWidgetsToRender) == 0L){
        flog.info("No invalid widgets. Nothing was removed.")
        return()
      }
      configJSON$inputWidgets <<- configJSON$inputWidgets[!vapply(names(configJSON$inputWidgets),
                                                                  function(el){
                                                                    el %in% invalidWidgetsToRender
                                                                  }, logical(1L), USE.NAMES = FALSE)]
      write_json(configJSON, configJSONFileName, pretty = TRUE,
                 auto_unbox = TRUE, null = "null")
      invalidWidgetsToRender <<- character(0L)
      if(length(invalidGraphsToRender)){
        hideEl(session, "#btRemoveInvalidWidgets")
        showHideEl(session, "#invalidSymbolsDialogSuccess")
      }else{
        removeModal()
      }
    })
    observeEvent(input$btRemoveInvalidGraphs, {
      if(length(invalidGraphsToRender) == 0L){
        flog.info("No invalid graphs Nothing was removed.")
        return()
      }
      configJSON$dataRendering <<- configJSON$dataRendering[!vapply(names(configJSON$dataRendering),
                                                                    function(el){
                                                                      el %in% invalidGraphsToRender
                                                                    }, logical(1L), USE.NAMES = FALSE)]
      write_json(configJSON, configJSONFileName, pretty = TRUE,
                 auto_unbox = TRUE, null = "null")
      invalidGraphsToRender <<- character(0L)
      if(length(invalidWidgetsToRender)){
        hideEl(session, "#btRemoveInvalidGraphs")
        showHideEl(session, "#invalidSymbolsDialogSuccess")
      }else{
        removeModal()
      }
    })
  }

  hideEl(session, "#loading-screen")
  session$onSessionEnded(function() {
    appDisconnected <<- TRUE
    if(!interactive()){
      stopApp()
    }
  })
}
