rowtmp <- list()
isolate({
  indexMap <- IdIdxMap$new(list(
    inputGroups = seq_along(configJSON$inputGroups),
    inputGroups_full = seq_along(configJSON$inputGroups),
    inputWidgetGroups = seq_along(configJSON$inputWidgetGroups),
    inputWidgetGroups_full = seq_along(configJSON$inputWidgetGroups),
    outputGroups = seq_along(configJSON$outputGroups),
    outputGroups_full = seq_along(configJSON$outputGroups),
    symlink = seq_along(configJSON$symbolLinks),
    outputAttachments = seq_along(configJSON$outputAttachments)
  ))

  groupTemp <- list(
    inputGroups = configJSON$inputGroups,
    inputWidgetGroups = configJSON$inputWidgetGroups,
    outputGroups = configJSON$outputGroups
  )
  rv$generalConfig$inputGroups <- configJSON$inputGroups
  rv$generalConfig$inputWidgetGroups <- configJSON$inputWidgetGroups
  rv$generalConfig$outputGroups <- configJSON$outputGroups
  rv$generalConfig$outputAttachments <- configJSON$outputAttachments
  rv$generalConfig$symbolLinks <- configJSON$symbolLinks
  rv$generalConfig$UILogo <- configJSON$UILogo
})
scalarSymbols <- setNames(
  c(
    names(modelIn),
    if (length(modelIn[[scalarsFileName]])) {
      modelIn[[scalarsFileName]]$symnames
    }
  ),
  c(
    modelInAlias,
    if (length(modelIn[[scalarsFileName]])) {
      modelIn[[scalarsFileName]]$symtext
    }
  )
)
scalarSymbols <- scalarSymbols[scalarSymbols %in% scalarInputSym]

outputAttachmentsValidator <- Validator$new(c("filename", "execPerm", "throwError"),
  configJSON$outputAttachments,
  requiredKeys = c("filename")
)

# set default values for array elements
if (length(configJSON$inputGroups)) {
  addArrayEl(session, "symbol_inputGroups", defaults = configJSON$inputGroups)
}
if (length(configJSON$inputWidgetGroups)) {
  addArrayEl(session, "symbol_inputWidgetGroups", defaults = configJSON$inputWidgetGroups)
}
if (length(configJSON$outputGroups)) {
  addArrayEl(session, "symbol_outputGroups", defaults = configJSON$outputGroups)
}
if (length(configJSON$symbolLinks)) {
  addArrayEl(session, "symbol_links", defaults = configJSON$symbolLinks)
}
if (length(configJSON$outputAttachments)) {
  addArrayEl(session, "general_output_attach", defaults = configJSON$outputAttachments)
}

output$general_logo_preview <- renderImage(
  {
    rv$customLogoChanged
    isolate({
      if (identical(rv$generalConfig$UILogo, "gams_logo.png") ||
        !length(rv$generalConfig$UILogo) || !file.exists(paste0(
        currentModelDir, .Platform$file.sep,
        "static_", modelName, .Platform$file.sep,
        rv$generalConfig$UILogo
      ))) {
        filename <- normalizePath(file.path(getwd(), "www", "gams_logo.png"))
      } else {
        filename <- normalizePath(paste0(
          currentModelDir, .Platform$file.sep,
          "static_", modelName, .Platform$file.sep,
          rv$generalConfig$UILogo
        ))
      }
    })
    list(src = filename, height = "50px", alt = "custom logo")
  },
  deleteFile = FALSE
)

observeEvent(input$general_pageTitle, {
  if (length(input$general_pageTitle) && nchar(input$general_pageTitle)) {
    rv$generalConfig$pageTitle <<- input$general_pageTitle
  }
})

output$themeColorsUI <- renderUI({
  tagList(
    tags$h4(lang$adminMode$colors$themeColors$light),
    fluidRow(
      tags$div(
        class = "col-sm-6 themeColors-colorpicker",
        colorPickerInput(
          "primary_color",
          labelTooltip(
            lang$adminMode$colors$themeColors$primary,
            lang$adminMode$colors$themeColors$primaryTooltip
          ),
          value = if (length(configJSON$themeColors$primary_color)) {
            configJSON$themeColors$primary_color
          } else {
            baseColors$primary_color
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "secondary_color",
          labelTooltip(
            lang$adminMode$colors$themeColors$secondary,
            lang$adminMode$colors$themeColors$secondaryTooltip
          ),
          value = if (length(configJSON$themeColors$secondary_color)) {
            configJSON$themeColors$secondary_color
          } else {
            baseColors$secondary_color
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "sidebar_color",
          lang$adminMode$colors$themeColors$sidebar,
          value = if (length(configJSON$themeColors$sidebar_color)) {
            configJSON$themeColors$sidebar_color
          } else {
            baseColors$sidebar_color
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "alert_color",
          labelTooltip(
            lang$adminMode$colors$themeColors$alert,
            lang$adminMode$colors$themeColors$alertTooltip
          ),
          value = if (length(configJSON$themeColors$alert_color)) {
            configJSON$themeColors$alert_color
          } else {
            baseColors$alert_color
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "console_text_color",
          labelTooltip(
            lang$adminMode$colors$themeColors$console,
            lang$adminMode$colors$themeColors$consoleTooltip
          ),
          value = if (length(configJSON$themeColors$console_text_color)) {
            configJSON$themeColors$console_text_color
          } else {
            baseColors$console_text_color
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      )
    ),
    tags$h4(lang$adminMode$colors$themeColors$dark),
    fluidRow(
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "primary_color_dark",
          labelTooltip(
            lang$adminMode$colors$themeColors$primaryDark,
            lang$adminMode$colors$themeColors$primaryTooltip
          ),
          value = if (length(configJSON$themeColors$primary_color_dark)) {
            configJSON$themeColors$primary_color_dark
          } else {
            baseColors$primary_color_dark
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "secondary_color_dark",
          labelTooltip(
            lang$adminMode$colors$themeColors$secondaryDark,
            lang$adminMode$colors$themeColors$secondaryTooltip
          ),
          value = if (length(configJSON$themeColors$secondary_color_dark)) {
            configJSON$themeColors$secondary_color_dark
          } else {
            baseColors$secondary_color_dark
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "sidebar_color_dark",
          lang$adminMode$colors$themeColors$sidebarDark,
          value = if (length(configJSON$themeColors$sidebar_color_dark)) {
            configJSON$themeColors$sidebar_color_dark
          } else {
            baseColors$sidebar_color_dark
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "alert_color_dark",
          labelTooltip(
            lang$adminMode$colors$themeColors$alertDark,
            lang$adminMode$colors$themeColors$alertTooltip
          ),
          value = if (length(configJSON$themeColors$alert_color_dark)) {
            configJSON$themeColors$alert_color_dark
          } else {
            baseColors$alert_color_dark
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "main_bg_dark",
          lang$adminMode$colors$themeColors$mainBgDark,
          value = if (length(configJSON$themeColors$main_bg_dark)) {
            configJSON$themeColors$main_bg_dark
          } else {
            baseColors$main_bg_dark
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "console_text_color_dark",
          labelTooltip(
            lang$adminMode$colors$themeColors$consoleDark,
            lang$adminMode$colors$themeColors$consoleTooltip
          ),
          value = if (length(configJSON$themeColors$console_text_color_dark)) {
            configJSON$themeColors$console_text_color_dark
          } else {
            baseColors$console_text_color_dark
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      ),
      tags$div(
        class = "col-sm-6",
        colorPickerInput(
          "widget_bg_dark",
          labelTooltip(
            lang$adminMode$colors$themeColors$widgetBgDark,
            lang$adminMode$colors$themeColors$widgetBgDarkTooltip
          ),
          value = if (length(configJSON$themeColors$widget_bg_dark)) {
            configJSON$themeColors$widget_bg_dark
          } else {
            baseColors$widget_bg_dark
          },
          colorPreview = TRUE,
          disableAlphaChannel = TRUE
        )
      )
    ),
    tags$h4(lang$adminMode$colors$themeButtons$buttonsTitle),
    tags$div(
      class = "theme-btn-row",
      actionButton("saveLocal", lang$adminMode$colors$themeButtons$saveLocal, icon("floppy-disk"), class = "bt-highlight-1 theme-btn"),
      actionButton("saveGlobal", lang$adminMode$colors$themeButtons$saveGlobal, icon("floppy-disk"), class = "bt-highlight-1 theme-btn"),
      actionButton("removeColors", lang$adminMode$colors$themeButtons$removeColors, icon("trash-can"), class = "bt-remove theme-btn")
    ),
    tags$div(
      class = "theme-btn-row",
      actionButton("downloadTheme", lang$adminMode$colors$themeButtons$downloadTheme, icon("download"), class = "theme-btn")
    ),
    tags$div(class = "space"),
    tags$ul(
      tags$li(
        tags$b(paste0(lang$adminMode$colors$themeButtons$saveLocal, ": ")),
        lang$adminMode$colors$themeButtons$saveLocalDesc
      ),
      tags$li(
        tags$b(paste0(lang$adminMode$colors$themeButtons$saveGlobal, ": ")),
        lang$adminMode$colors$themeButtons$saveGlobalDesc
      ),
      tags$li(
        tags$b(paste0(lang$adminMode$colors$themeButtons$downloadTheme, ": ")),
        lang$adminMode$colors$themeButtons$downloadThemeDesc
      ),
      tags$li(
        tags$b(paste0(lang$adminMode$colors$themeButtons$removeColors, ": ")),
        lang$adminMode$colors$themeButtons$removeColorsDesc
      )
    ),
    tags$div(class = "space")
  )
})

customBaseColors <- reactive({
  list(
    primary_color           = resolveColor(input$primary_color, baseColors$primary_color),
    secondary_color         = resolveColor(input$secondary_color, baseColors$secondary_color),
    sidebar_color           = resolveColor(input$sidebar_color, baseColors$sidebar_color),
    alert_color             = resolveColor(input$alert_color, baseColors$alert_color),
    main_bg                 = "#ffffff",
    console_text_color      = resolveColor(input$console_text_color, baseColors$console_text_color),
    primary_color_dark      = resolveColor(input$primary_color_dark, baseColors$primary_color_dark),
    secondary_color_dark    = resolveColor(input$secondary_color_dark, baseColors$secondary_color_dark),
    sidebar_color_dark      = resolveColor(input$sidebar_color_dark, baseColors$sidebar_color_dark),
    alert_color_dark        = resolveColor(input$alert_color_dark, baseColors$alert_color_dark),
    main_bg_dark            = resolveColor(input$main_bg_dark, baseColors$main_bg_dark),
    console_text_color_dark = resolveColor(input$console_text_color_dark, baseColors$console_text_color_dark),
    widget_bg_dark          = resolveColor(input$widget_bg_dark, baseColors$widget_bg_dark),
    text_color              = "#eeeeee",
    text_color_dark         = "#eeeeee"
  )
})

palette <- reactive(derive_palette(customBaseColors()))

isDefaultPalette <- reactive({
  cb <- customBaseColors()
  def <- baseColors[names(cb)]
  all(mapply(identical, cb, def))
})

observeEvent(palette(),
  {
    vars <- palette()
    msg <- setNames(
      lapply(vars, serialise),
      vapply(names(vars), css_name, "")
    )
    session$sendCustomMessage("update-css", msg)
  },
  ignoreInit = TRUE
)

trigger <- reactiveVal(0)
observeEvent(input$removeColors, {
  if (length(configJSON$themeColors)) {
    showModal(modalDialog(
      title = lang$adminMode$colors$themeButtons$removeColorsTitle,
      lang$adminMode$colors$themeButtons$removeColorsText,
      footer = tagList(
        modalButton(lang$adminMode$colors$themeButtons$removeColorsCancel),
        actionButton("removeColorsConfirm", lang$adminMode$colors$themeButtons$removeColorsConfirm, class = "bt-remove")
      ),
      fade = TRUE, easyClose = TRUE
    ))
  } else {
    showNotification(lang$adminMode$colors$themeButtons$removeColorsNote2, type = "message")
  }
})
observeEvent(input$removeColorsConfirm, {
  rv$generalConfig$themeColors <<- NULL
  configJSON$themeColors <<- NULL
  trigger(trigger() + 1)
  removeModal()
  showNotification(lang$adminMode$colors$themeButtons$removeColorsNote1, type = "message")
})
observeEvent(input$saveLocal, {
  rv$generalConfig$themeColors <- palette()
  showNotification(lang$adminMode$colors$themeButtons$saveLocalNote, type = "message")
})
observeEvent(input$saveGlobal, {
  vars <- palette()
  msg <- setNames(
    lapply(vars, serialise),
    vapply(names(vars), css_name, "")
  )
  properties <- paste0(names(msg), ':"', msg, '"', collapse = ";\n  ")
  css <- paste0(":root{\n  ", properties, ";\n}")
  outFile <- file.path(miroWorkspace, "themecolors.css")

  if (file.exists(outFile)) {
    showModal(modalDialog(
      title = lang$adminMode$colors$themeButtons$saveGlobalModalTitle,
      sprintf(
        lang$adminMode$colors$themeButtons$saveGlobalModalText,
        basename(outFile)
      ),
      footer = tagList(
        modalButton(lang$adminMode$colors$themeButtons$saveGlobalModalCancel),
        actionButton("overwriteGlobal", lang$adminMode$colors$themeButtons$saveGlobalModalOverwrite, class = "bt-highlight-1")
      ),
      fade = TRUE, easyClose = TRUE
    ))

    observeEvent(input$overwriteGlobal, {
      writeLines(css, con = outFile)
      removeModal()
      showNotification(paste0(lang$adminMode$colors$themeButtons$saveGlobalModalNote1, " ", lang$adminMode$colors$themeButtons$saveGlobalModalNoteDesc), duration = 8, type = "message")
    })
  } else {
    writeLines(css, con = outFile)
    showNotification(paste0(lang$adminMode$colors$themeButtons$saveGlobalModalNote2, " ", lang$adminMode$colors$themeButtons$saveGlobalModalNoteDesc), duration = 8, type = "message")
  }
})

observeEvent(input$downloadTheme, {
  outFile <- "themecolors.css"

  showModal(modalDialog(
    title = lang$adminMode$colors$themeButtons$downloadThemeModalTitle,
    sprintf(
      lang$adminMode$colors$themeButtons$downloadThemeModalText,
      outFile
    ),
    footer = tagList(
      modalButton(lang$adminMode$colors$themeButtons$downloadThemeModalCancel),
      downloadButton("downloadThemeConfirm", lang$adminMode$colors$themeButtons$downloadThemeModalDownload, class = "bt-highlight-1")
    ),
    fade = TRUE, easyClose = TRUE
  ))
})
output$downloadThemeConfirm <- downloadHandler(
  filename = function() {
    "themecolors.css"
  },
  content = function(file) {
    vars <- palette()
    msg <- setNames(
      lapply(vars, serialise),
      vapply(names(vars), css_name, "")
    )
    properties <- paste0(names(msg), ':"', msg, '"', collapse = ";\n  ")
    css <- paste0(":root{\n  ", properties, ";\n}")
    writeLines(css, con = file)
  },
  contentType = "text/css"
)

observeEvent(input$general_theme, {
  rv$generalConfig$theme <<- input$general_theme
})
observeEvent(input$general_customCss, {
  rv$generalConfig$customCss <<- input$general_customCss
})
observeEvent(input$general_maxTabsExpandedInput, {
  valTmp <- as.integer(input$general_maxTabsExpandedInput)
  if (length(valTmp) != 1L || is.na(valTmp) || valTmp < 0L) {
    valTmp <- 5L
  }
  if (!length(rv$generalConfig$layoutSettings)) {
    rv$generalConfig$layoutSettings <- list()
  }
  rv$generalConfig$layoutSettings$maxTabsExpandedInput <- valTmp
})
observeEvent(input$general_maxTabsExpandedOutput, {
  valTmp <- as.integer(input$general_maxTabsExpandedOutput)
  if (length(valTmp) != 1L || is.na(valTmp) || valTmp < 1L) {
    valTmp <- 5L
  }
  if (!length(rv$generalConfig$layoutSettings)) {
    rv$generalConfig$layoutSettings <- list()
  }
  rv$generalConfig$layoutSettings$maxTabsExpandedOutput <- valTmp
})
observeEvent(input$general_maxTabsExpandedPivotComp, {
  valTmp <- as.integer(input$general_maxTabsExpandedPivotComp)
  if (length(valTmp) != 1L || is.na(valTmp) || valTmp < 1L) {
    valTmp <- 5L
  }
  if (!length(rv$generalConfig$layoutSettings)) {
    rv$generalConfig$layoutSettings <- list()
  }
  rv$generalConfig$layoutSettings$maxTabsExpandedPivotComp <- valTmp
})
observeEvent(input$general_maxTabsExpandedSplitComp, {
  valTmp <- as.integer(input$general_maxTabsExpandedSplitComp)
  if (length(valTmp) != 1L || is.na(valTmp) || valTmp < 1L) {
    valTmp <- 5L
  }
  if (!length(rv$generalConfig$layoutSettings)) {
    rv$generalConfig$layoutSettings <- list()
  }
  rv$generalConfig$layoutSettings$maxTabsExpandedSplitComp <- valTmp
})
observeEvent(input$general_maxTabsExpandedTabComp, {
  valTmp <- as.integer(input$general_maxTabsExpandedTabComp)
  if (length(valTmp) != 1L || is.na(valTmp) || valTmp < 1L) {
    valTmp <- 5L
  }
  if (!length(rv$generalConfig$layoutSettings)) {
    rv$generalConfig$layoutSettings <- list()
  }
  rv$generalConfig$layoutSettings$maxTabsExpandedTabComp <- valTmp
})
observeEvent(input$general_meta, {
  rv$generalConfig$excelIncludeMeta <<- input$general_meta
})
observeEvent(input$general_empty, {
  rv$generalConfig$excelIncludeEmptySheets <<- input$general_empty
})
observeEvent(input$default_scen_check, {
  if (identical(input$default_scen_check, TRUE) && nchar(input$general_default_scen_name)) {
    rv$generalConfig$defaultScenName <<- input$general_default_scen_name
  } else {
    rv$generalConfig$defaultScenName <<- NULL
    configJSON$defaultScenName <<- NULL
  }
})
observeEvent(input$general_default_scen_name, {
  if (!identical(input$default_scen_check, TRUE)) {
    return()
  }
  if (nchar(input$general_default_scen_name)) {
    rv$generalConfig$defaultScenName <<- input$general_default_scen_name
  } else {
    rv$generalConfig$defaultScenName <<- NULL
    configJSON$defaultScenName <<- NULL
  }
})
observeEvent(input$general_defaultRendererOutput, {
  if (length(input$general_defaultRendererOutput) &&
    input$general_defaultRendererOutput %in% c("miroPivot", "datatable")) {
    rv$generalConfig$defaultRendererOutput <<- input$general_defaultRendererOutput
  } else {
    rv$generalConfig$defaultRendererOutput <<- "miroPivot"
  }
})
observeEvent(input$widget_general_logo_upload, {
  inFile <- input$widget_general_logo_upload
  filePath <- inFile$datapath
  fileName <- inFile$name
  if (!dir.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
    if (!dir.create(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
      flog.error(
        "A problem occurred creating directory: %s. Maybe you have insufficient permissions?",
        paste0(currentModelDir, .Platform$file.sep, "static_", modelName)
      )
      showModal(modalDialog(
        lang$adminMode$general$modalDialog$title,
        lang$adminMode$general$modalDialog$content
      ))
      return()
    }
  } else {
    filesToDelete <- file.path(
      currentModelDir, paste0("static_", modelName),
      rv$generalConfig$UILogo
    )
    filesFailedToDelete <- !suppressWarnings(file.remove(filesToDelete))
    if (any(filesFailedToDelete)) {
      flog.warn(
        "Problems removing files: '%s'. Do you lack the necessary permissions?",
        paste(filesToDelete[filesFailedToDelete], collapse = "', '")
      )
    }
  }
  if (!file.copy(filePath, file.path(currentModelDir, paste0("static_", modelName), fileName), overwrite = TRUE)) {
    flog.error(
      "A problem occurred copying image (%s) to folder: %s. Maybe you have insufficient permissions?",
      filePath, paste0(currentModelDir, .Platform$file.sep, "static_", modelName)
    )
    showModal(modalDialog(
      lang$adminMode$general$modalDialog$title,
      lang$adminMode$general$modalDialog$content
    ))
    return()
  }
  rv$generalConfig$UILogo <<- fileName
  rv$customLogoChanged <<- rv$customLogoChanged + 1L
})
observe({
  input$general_readmeEnableMath
  if (isFALSE(input$general_useReadme) ||
    !length(input$general_readmeTabtitle) ||
    !nchar(trimws(input$general_readmeTabtitle)) ||
    !length(input$general_readmeFileName) ||
    nchar(trimws(input$general_readmeFileName)) < 3L) {
    configJSON$readme <<- NULL
    rv$generalConfig$readme <<- NULL
    disableEl(session, "#btEditReadme")
    return()
  }
  isolate(rv$generalConfig$readme <<- list(
    tabTitle = input$general_readmeTabtitle,
    filename = input$general_readmeFileName,
    enableMath = input$general_readmeEnableMath
  ))
  enableEl(session, "#btEditReadme")
})
observeEvent(input$general_auto, {
  rv$generalConfig$autoGenInputGraphs <<- input$general_auto
})
observeEvent(input$general_save_duration, {
  rv$generalConfig$storeLogFilesDuration <<- input$general_save_duration
})

observeEvent(input$general_args, ignoreNULL = FALSE, {
  if (!length(input$general_args)) {
    hideEl(session, "#invalidClArgsError")
    rv$generalConfig$extraClArgs <<- NULL
    configJSON$extraClArgs <<- NULL
    return()
  }
  invalidClArgs <- trimws(tolower(vapply(strsplit(input$general_args, " |="),
    "[[", character(1L), 1L,
    USE.NAMES = FALSE
  ))) %in% reservedGMSOpt
  if (any(invalidClArgs)) {
    showEl(session, "#invalidClArgsError")
    newClArgs <- input$general_args[!invalidClArgs]
    if (!length(newClArgs)) {
      rv$generalConfig$extraClArgs <<- NULL
      configJSON$extraClArgs <<- NULL
    } else {
      rv$generalConfig$extraClArgs <- I(newClArgs)
    }
  } else {
    hideEl(session, "#invalidClArgsError")
    rv$generalConfig$extraClArgs <- I(input$general_args)
  }
})

observeEvent(input$general_scen, {
  rv$generalConfig$defCompMode <<- input$general_scen
})
observeEvent(input$pivotcomp_emptyUEL, {
  emptyUEL <- input$pivotcomp_emptyUEL
  if (!is.null(emptyUEL) && emptyUEL != "") {
    rv$generalConfig$pivotCompSettings$emptyUEL <- emptyUEL
  } else {
    configJSON$pivotCompSettings$emptyUEL <<- NULL
    rv$generalConfig$pivotCompSettings$emptyUEL <<- NULL
  }
})
observeEvent(input$pivotcomp_hidePivotControls, {
  rv$generalConfig$pivotCompSettings$hidePivotControls <<- input$pivotcomp_hidePivotControls
})
observeEvent(input$pivotcomp_hideEmptyCols, {
  rv$generalConfig$pivotCompSettings$hideEmptyCols <<- input$pivotcomp_hideEmptyCols
})
observeEvent(input$pivotcomp_chartFontSize,
  {
    if (length(input$pivotcomp_chartFontSize) && !is.na(input$pivotcomp_chartFontSize)) {
      rv$generalConfig$pivotCompSettings$chartFontSize <<- input$pivotcomp_chartFontSize
    } else {
      configJSON$pivotCompSettings$chartFontSize <<- NULL
      rv$generalConfig$pivotCompSettings$chartFontSize <<- NULL
    }
  },
  ignoreNULL = FALSE
)
observeEvent(
  {
    input$pivotcomp_showTableSummaryRow
    input$pivotcomp_rowSummaryFunction
    input$pivotcomp_showTableSummaryCol
    input$pivotcomp_colSummaryFunction
  },
  {
    tableSummarySettings <- list(
      rowEnabled = identical(input$pivotcomp_showTableSummaryRow, TRUE),
      rowSummaryFunction = if (length(input$pivotcomp_rowSummaryFunction)) {
        input$pivotcomp_rowSummaryFunction
      } else {
        "sum"
      },
      colEnabled = identical(input$pivotcomp_showTableSummaryCol, TRUE),
      colSummaryFunction = if (length(input$pivotcomp_colSummaryFunction)) {
        input$pivotcomp_colSummaryFunction
      } else {
        "sum"
      }
    )
    rv$generalConfig$pivotCompSettings$tableSummarySettings <<- tableSummarySettings
  }
)
observeEvent(input$pivotcomp_fixedColumns, {
  rv$generalConfig$pivotCompSettings$fixedColumns <<- input$pivotcomp_fixedColumns
})
observeEvent(input$general_act_upload, {
  rv$generalConfig$activateModules$loadLocal <<- input$general_act_upload
})
observeEvent(input$general_act_log, {
  rv$generalConfig$activateModules$logFile <<- input$general_act_log
})
observeEvent(input$general_act_lst, {
  rv$generalConfig$activateModules$lstFile <<- input$general_act_lst
})
observeEvent(input$general_act_attach, {
  rv$generalConfig$activateModules$attachments <<- input$general_act_attach
})
observeEvent(input$general_act_hcube, {
  rv$generalConfig$activateModules$hcube <<- input$general_act_hcube
})
observeEvent(input$general_aggregate, {
  rv$generalConfig$aggregateWidgets <<- input$general_aggregate
})
observeEvent(input$general_overwriteSheetOrderInput, {
  if (!identical(input$general_overwriteSheetOrderInput, unname(inputSymMultiDim))) {
    # adjust widget ids
    sheetOrderTmp <- input$general_overwriteSheetOrderInput
    widgetGrps <- strsplit(sheetOrderTmp, "_widgets", fixed = TRUE)
    i <- 0L
    sheetOrderTmp <- vapply(widgetGrps, function(grpId) {
      if (length(grpId) < 2L) {
        if (identical(grpId, "")) {
          return("_widgets")
        }
        return(grpId)
      }
      i <<- i + 1L
      return(paste0("_widgets", i))
    }, character(1L), USE.NAMES = FALSE)
    rv$generalConfig$overwriteSheetOrder$input <<- sheetOrderTmp
  }
})
observeEvent(input$general_overwriteSheetOrderOutput, {
  if (!identical(input$general_overwriteSheetOrderOutput, names(modelOut))) {
    rv$generalConfig$overwriteSheetOrder$output <<- input$general_overwriteSheetOrderOutput
  }
})
lapply(c(
  modelInRaw[[scalarsFileName]]$symnames,
  modelOut[[scalarsOutName]]$symnames
), function(name) {
  observeEvent(input[[paste0("general_overwriteSymAlias_", name)]], {
    if (length(input[[paste0("general_overwriteSymAlias_", name)]]) &&
      nchar(input[[paste0("general_overwriteSymAlias_", name)]]) > 0L) {
      newAlias <- input[[paste0("general_overwriteSymAlias_", name)]]
      if (!length(rv$generalConfig$overwriteAliases)) {
        rv$generalConfig$overwriteAliases <- list()
      }
      rv$generalConfig$overwriteAliases[[name]] <<- list(newAlias = newAlias)
    } else {
      rv$generalConfig$overwriteAliases[[name]] <<- NULL
      configJSON$overwriteAliases[[name]] <<- NULL
      if (!length(rv$generalConfig$overwriteAliases)) {
        rv$generalConfig$overwriteAliases <<- NULL
        configJSON$overwriteAliases <<- NULL
      }
      return()
    }
  })
})
lapply(c(names(modelInRaw), names(modelOut)), function(name) {
  observeEvent(input[[paste0("general_overwriteSymAlias_", name)]], {
    if (length(input[[paste0("general_overwriteSymAlias_", name)]]) &&
      nchar(input[[paste0("general_overwriteSymAlias_", name)]]) > 0L) {
      newAlias <- input[[paste0("general_overwriteSymAlias_", name)]]
      if (!length(rv$generalConfig$overwriteAliases)) {
        rv$generalConfig$overwriteAliases <- list()
      }
      rv$generalConfig$overwriteAliases[[name]] <<- list(newAlias = newAlias)
    } else {
      rv$generalConfig$overwriteAliases[[name]] <<- NULL
      configJSON$overwriteAliases[[name]] <<- NULL
      if (!length(rv$generalConfig$overwriteAliases)) {
        rv$generalConfig$overwriteAliases <<- NULL
        configJSON$overwriteAliases <<- NULL
      }
      return()
    }
  })
  observe({
    defaultAlias <- FALSE
    if (name %in% names(modelOut)) {
      newHeaders <- unlist(lapply(seq_along(dataContract$outputSymbols[[name]]$headers), function(hdrIdx) {
        input[[paste0("general_overwriteSymHeaders_", name, "_", hdrIdx)]]
      }))
      if (length(newHeaders) != length(dataContract$outputSymbols[[name]]$headers)) {
        return()
      }
      if (any(newHeaders == "")) {
        addClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      } else if (identical(newHeaders, vapply(dataContract$outputSymbols[[name]]$headers, "[[",
        character(1L), "alias",
        USE.NAMES = FALSE
      ))) {
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      } else {
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
      }
    } else {
      newHeaders <- unlist(lapply(seq_along(dataContract$inputSymbols[[name]]$headers), function(hdrIdx) {
        input[[paste0("general_overwriteSymHeaders_", name, "_", hdrIdx)]]
      }))
      if (length(newHeaders) != length(dataContract$inputSymbols[[name]]$headers)) {
        return()
      }
      if (any(newHeaders == "")) {
        addClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      } else if (identical(newHeaders, vapply(dataContract$inputSymbols[[name]]$headers, "[[",
        character(1L), "alias",
        USE.NAMES = FALSE
      ))) {
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      } else {
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
      }
    }
    if (defaultAlias) {
      rv$generalConfig$overwriteHeaderAliases[[name]] <<- NULL
      configJSON$overwriteHeaderAliases[[name]] <<- NULL
      if (!length(rv$generalConfig$overwriteHeaderAliases)) {
        rv$generalConfig$overwriteHeaderAliases <<- NULL
        configJSON$overwriteHeaderAliases <<- NULL
      }
      return()
    }
    if (!length(rv$generalConfig$overwriteHeaderAliases)) {
      rv$generalConfig$overwriteHeaderAliases <- list()
    }
    rv$generalConfig$overwriteHeaderAliases[[name]] <<- list(newHeaders = newHeaders)
  })
})
observeEvent(input$general_downloadTempFiles, {
  rv$generalConfig$activateModules$downloadTempFiles <<- input$general_downloadTempFiles
})

observeEvent(input$general_hidden, ignoreNULL = FALSE, {
  if (!length(input$general_hidden)) {
    configJSON$hiddenOutputScalars <<- NULL
  }
  rv$generalConfig$hiddenOutputScalars <<- input$general_hidden
})
observeEvent(input$general_hiddenOutputSymbols, ignoreNULL = FALSE, {
  if (!length(input$general_hiddenOutputSymbols)) {
    configJSON$hiddenOutputSymbols <<- NULL
  }
  rv$generalConfig$hiddenOutputSymbols <<- input$general_hiddenOutputSymbols
})

observeEvent(input$general_decimal, {
  rv$generalConfig$roundingDecimals <<- input$general_decimal
})
observeEvent(input$general_mirologfile, {
  if (!nchar(input$general_mirologfile)) {
    configJSON$miroLogFile <<- NULL
  }
  if (length(input$general_mirologfile) &&
    nchar(trimws(input$general_mirologfile))) {
    rv$generalConfig$miroLogFile <<- input$general_mirologfile
    return()
  }
  rv$generalConfig$miroLogFile <<- NULL
})
observeEvent(input$general_mirologParsingStdout, {
  rv$generalConfig$parseLogForMiroLogSyntax <<- isTRUE(input$general_mirologParsingStdout)
})
observeEvent(input$add_general, {
  if (length(input$add_general) < 3L) {
    return()
  }
  arrayID <- strsplit(input$add_general[3], "_")[[1]][2]
  arrayIdx <- indexMap$push(arrayID, input$add_general[1])
  arrayIdxAll <- indexMap$push(paste0(arrayID, "_full"), input$add_general[1])

  minMembers <- if (identical(arrayID, "inputWidgetGroups")) 0L else 1L

  if (length(input$add_general) < 3L || nchar(trimws(input$add_general[2])) < 1L) {
    # name has no characters
    if (arrayIdx <= length(rv$generalConfig[[arrayID]])) {
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if (!length(rv$generalConfig[[arrayID]])) {
        rv$generalConfig[[arrayID]] <<- NULL
      }
      if (identical(arrayID, "inputWidgetGroups")) {
        updateSheetOrderInput(arrayIdxAll)
      }
      indexMap$pop(arrayID, input$add_general[1])
    }
    newName <- NULL
    showElReplaceTxt(
      session, paste0("#", input$add_general[3], input$add_general[1], "_err"),
      lang$adminMode$widgets$validate$val36
    )
  } else {
    newName <- input$add_general[2]
    if (arrayIdx <= length(rv$generalConfig[[arrayID]]) && length(rv$generalConfig[[arrayID]][[arrayIdx]])) {
      rv$generalConfig[[arrayID]][[arrayIdx]]$name <- newName
      if (identical(arrayID, "inputWidgetGroups")) {
        updateSheetOrderInput(arrayIdxAll, newName)
      }
    } else if (arrayIdxAll <= length(groupTemp[[arrayID]]) &&
      length(groupTemp[[arrayID]][[arrayIdxAll]]$members) > minMembers &&
      !any(groupTemp[[arrayID]][[arrayIdxAll]]$members %in%
        unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))) {
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(
        name = newName,
        members = groupTemp[[arrayID]][[arrayIdxAll]]$members,
        sameTab = isTRUE(groupTemp[[arrayID]][[arrayIdxAll]]$sameTab),
        colsPerRow = if (length(groupTemp[[arrayID]][[arrayIdxAll]]$colsPerRow)) {
          groupTemp[[arrayID]][[arrayIdxAll]]$colsPerRow
        } else {
          1L
        }
      )
      if (identical(arrayID, "inputWidgetGroups")) {
        updateSheetOrderInput(arrayIdxAll, newName)
      }
    } else {
      showElReplaceTxt(
        session, paste0(
          "#group_member",
          if (identical(arrayID, "inputGroups")) {
            "In"
          } else if (identical(arrayID, "inputWidgetGroups")) "Widget" else "Out",
          input$add_general[1], "_err"
        ),
        lang$adminMode$widgets$validate[[if (minMembers == 0L) "val37a" else "val37"]]
      )
    }
    hideEl(session, paste0("#", input$add_general[3], input$add_general[1], "_err"))
  }
  if (arrayIdxAll > length(groupTemp[[arrayID]])) {
    groupTemp[[arrayID]][[arrayIdxAll]] <<- list(name = newName)
  } else {
    groupTemp[[arrayID]][[arrayIdxAll]]$name <<- newName
  }
})
validateSymbolLink <- function(sourceSym, targetSym, arrayId) {
  if (!identical(
    vapply(modelInRaw[[targetSym]]$headers,
      "[[", character(1L), "type",
      USE.NAMES = FALSE
    ),
    vapply(modelOut[[sourceSym]]$headers,
      "[[", character(1L), "type",
      USE.NAMES = FALSE
    )
  )) {
    showElReplaceTxt(
      session, paste0(
        "#symlink_target",
        arrayId, "_err"
      ),
      lang$adminMode$widgets$validate$val47
    )
  } else {
    hideEl(session, paste0(
      "#symlink_target",
      arrayId, "_err"
    ))
  }
}
observeEvent(input$add_symlink, {
  if (!length(inputSymMultiDim) || length(input$add_symlink) < 3L) {
    return()
  }
  arrayIdx <- indexMap$push("symlink", input$add_symlink[1])

  if (!length(arrayIdx)) {
    flog.error("Bad index for symbolLink detected: '%s'. Please report to GAMS!", arrayIdx)
    return()
  }
  if (arrayIdx <= length(rv$generalConfig$symbolLinks)) {
    rv$generalConfig$symbolLinks[[arrayIdx]]$source <<- input$add_symlink[2]
    validateSymbolLink(
      input$add_symlink[2],
      rv$generalConfig$symbolLinks[[arrayIdx]]$target,
      input$add_symlink[1]
    )
  } else {
    rv$generalConfig$symbolLinks[[arrayIdx]] <<- list(
      source = input$add_symlink[2],
      target = inputSymMultiDim[[1L]]
    )
    validateSymbolLink(
      input$add_symlink[2],
      inputSymMultiDim[[1L]],
      input$add_symlink[1]
    )
  }
})
observeEvent(input$symlink_target, {
  if (length(input$symlink_target) < 2L) {
    return()
  }
  arrayIdx <- indexMap$push(
    "symlink",
    input$symlink_target[1]
  )

  if (length(arrayIdx) && arrayIdx <= length(rv$generalConfig$symbolLinks)) {
    targetSym <- input$symlink_target[2]
    rv$generalConfig$symbolLinks[[arrayIdx]]$target <<- targetSym
    sourceSym <- rv$generalConfig$symbolLinks[[arrayIdx]]$source
    validateSymbolLink(sourceSym, targetSym, input$symlink_target[1])
  }
})
observeEvent(input$remove_symlink, {
  if (length(input$remove_symlink) < 3L) {
    return()
  }
  arrayIdx <- indexMap$pop(
    "symlink",
    input$remove_symlink[3]
  )
  if (length(arrayIdx) && arrayIdx <= length(rv$generalConfig$symbolLinks)) {
    rv$generalConfig$symbolLinks[[arrayIdx]] <<- NULL
    if (!length(rv$generalConfig$symbolLinks)) {
      rv$generalConfig$symbolLinks <<- NULL
    }
  }
})
observeEvent(input$add_attach, {
  arrayID <- as.integer(input$add_attach[1])
  if (is.na(arrayID)) {
    return()
  }
  if (!identical(sanitizeFn(input$add_attach[2]), input$add_attach[2])) {
    showElReplaceTxt(
      session, paste0("#", input$add_attach[3], arrayID, "_err"),
      lang$adminMode$widgets$validate$val57
    )
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "filename")$
      getValidData()
    return()
  }
  if (input$add_attach[2] %in% outputAttachmentsValidator$getValid("filename")) {
    showElReplaceTxt(
      session, paste0("#", input$add_attach[3], arrayID, "_err"),
      lang$adminMode$widgets$validate$val56
    )
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "filename")$
      getValidData()
    return()
  }
  hideEl(session, paste0("#", input$add_attach[3], arrayID, "_err"))
  if (length(input$add_attach) > 1 && nchar(input$add_attach[2]) > 0L) {
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      setVal(arrayID, "filename", input$add_attach[2])$
      getValidData()
  }
})
observeEvent(input$outAttach_exec, {
  if (length(input$outAttach_exec) < 2L) {
    return()
  }
  arrayID <- as.integer(input$outAttach_exec[1])
  if (is.na(arrayID)) {
    return()
  }
  val <- identical(input$outAttach_exec[2], 1L)
  if (!length(val)) {
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "execPerm")$
      getValidData()
    return()
  }
  rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
    setVal(arrayID, "execPerm", val)$
    getValidData()
})
observeEvent(input$outAttach_error, {
  if (length(input$outAttach_error) < 2L) {
    return()
  }
  arrayID <- as.integer(input$outAttach_error[1])
  if (is.na(arrayID)) {
    return()
  }
  val <- !identical(input$outAttach_error[2], 0L)
  if (!length(val)) {
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "throwError")$
      getValidData()
    return()
  }
  rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
    setVal(arrayID, "throwError", val)$
    getValidData()
})
observeEvent(input$remove_attach, {
  if (length(input$remove_attach) < 3L) {
    return()
  }
  arrayID <- as.integer(input$remove_attach[3])
  if (is.na(arrayID)) {
    return()
  }
  rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
    removeEl(arrayID)$
    getValidData()
  if (!length(rv$generalConfig$outputAttachments)) {
    rv$generalConfig$outputAttachments <- NULL
  }
})
changeAndValidateGroupMembers <- function(arrayID, groupMembers, HTMLarrayID, minNoMembers = 2L) {
  arrayIdx <- indexMap$push(arrayID, groupMembers[1])
  arrayIdxAll <- indexMap$push(paste0(arrayID, "_full"), groupMembers[1])

  if (length(groupMembers) > minNoMembers &&
    !any(groupMembers[-1] %in% unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))) {
    newMembers <- groupMembers[2:length(groupMembers)]
    if (arrayIdx <= length(rv$generalConfig[[arrayID]]) && length(rv$generalConfig[[arrayID]][[arrayIdx]])) {
      rv$generalConfig[[arrayID]][[arrayIdx]]$members <- newMembers
    } else if (arrayIdxAll <= length(groupTemp[[arrayID]]) &&
      length(groupTemp[[arrayID]][[arrayIdxAll]]$name)) {
      newName <- groupTemp[[arrayID]][[arrayIdxAll]]$name
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(
        name = newName,
        members = newMembers,
        sameTab = isTRUE(groupTemp[[arrayID]][[arrayIdxAll]]$sameTab),
        colsPerRow = if (length(groupTemp[[arrayID]][[arrayIdxAll]]$colsPerRow)) {
          groupTemp[[arrayID]][[arrayIdxAll]]$colsPerRow
        } else {
          1L
        }
      )
      if (identical(arrayID, "inputWidgetGroups")) {
        updateSheetOrderInput(arrayIdxAll, newName)
      }
    } else {
      showElReplaceTxt(
        session, paste0("#symbol_", arrayID, input$add_general[1], "_err"),
        lang$adminMode$widgets$validate$val36
      )
    }
    hideEl(session, paste0("#", HTMLarrayID, groupMembers[1], "_err"))
  } else {
    if (arrayIdx <= length(rv$generalConfig[[arrayID]])) {
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if (!length(rv$generalConfig[[arrayID]])) {
        rv$generalConfig[[arrayID]] <<- NULL
      }
      if (identical(arrayID, "inputWidgetGroups")) {
        updateSheetOrderInput(arrayIdxAll)
      }
      indexMap$pop(arrayID, groupMembers[1])
    }
    newMembers <- NULL
    showElReplaceTxt(
      session, paste0("#", HTMLarrayID, groupMembers[1], "_err"),
      if (identical(minNoMembers, 2L)) {
        lang$adminMode$widgets$validate[[if (minNoMembers == 0L) "val37a" else "val37"]]
      } else {
        lang$adminMode$widgets$validate$val60
      }
    )
  }
  if (arrayIdxAll > length(groupTemp[[arrayID]])) {
    groupTemp[[arrayID]][[arrayIdxAll]] <<- list(members = newMembers)
  } else {
    groupTemp[[arrayID]][[arrayIdxAll]]$members <<- newMembers
  }
}
updateSheetOrderInput <- function(arrayIdx, newName = NULL) {
  orderItemId <- paste0("_widgets", arrayIdx)
  orderItemIdx <- match(orderItemId, inputTabs)
  if (is.na(orderItemIdx)) {
    if (is.null(newName)) {
      return()
    }
    inputTabs <<- c(
      inputTabs,
      setNames(
        orderItemId,
        newName
      )
    )
  } else if (is.null(newName)) {
    inputTabs <<- inputTabs[-orderItemIdx]
  } else {
    names(inputTabs)[orderItemIdx] <<- newName
  }
  newSheetOrder <- inputTabs
  if (length(input$general_overwriteSheetOrderInput)) {
    newSheetOrder <- inputTabs[order(match(inputTabs, input$general_overwriteSheetOrderInput))]
  }
  updateSelectInput(session, "general_overwriteSheetOrderInput",
    choices = newSheetOrder, selected = newSheetOrder
  )
}
observeEvent(input$group_memberIn, {
  changeAndValidateGroupMembers(
    "inputGroups", input$group_memberIn,
    "group_memberIn"
  )
})
observeEvent(input$group_memberWidget, {
  changeAndValidateGroupMembers(
    "inputWidgetGroups", input$group_memberWidget,
    "group_memberWidget", 1L
  )
})
observeEvent(input$group_memberOut, {
  changeAndValidateGroupMembers(
    "outputGroups", input$group_memberOut,
    "group_memberOut"
  )
})
observeEvent(input$group_sameTabIn, {
  if (length(input$group_sameTabIn) < 2L) {
    return()
  }

  arrayIdx <- indexMap$push("inputGroups", input$group_sameTabIn[1])
  arrayIdxAll <- indexMap$push("inputGroups_full", input$group_sameTabIn[1])
  newVal <- input$group_sameTabIn[2]
  sameTabNewVal <- !identical(newVal, "_")
  if (sameTabNewVal) {
    newVal <- as.integer(newVal)
  } else {
    newVal <- 1L
  }

  if (arrayIdx <= length(rv$generalConfig[["inputGroups"]]) &&
    length(rv$generalConfig[["inputGroups"]][[arrayIdx]])) {
    rv$generalConfig[["inputGroups"]][[arrayIdx]]$sameTab <<- sameTabNewVal
    rv$generalConfig[["inputGroups"]][[arrayIdx]]$colsPerRow <<- newVal
  } else if (arrayIdxAll <= length(groupTemp[["inputGroups"]]) &&
    length(groupTemp[["inputGroups"]][[arrayIdxAll]])) {
    groupTemp[["inputGroups"]][[arrayIdxAll]]$sameTab <<- sameTabNewVal
    groupTemp[["inputGroups"]][[arrayIdxAll]]$colsPerRow <<- newVal
  } else {
    groupTemp[["inputGroups"]][[arrayIdxAll]] <<- list(
      sameTab = sameTabNewVal,
      colsPerRow = newVal
    )
  }
})
observeEvent(input$group_sameTabWidget, {
  if (length(input$group_sameTabWidget) < 2L) {
    return()
  }

  arrayIdx <- indexMap$push("inputWidgetGroups", input$group_sameTabWidget[1])
  arrayIdxAll <- indexMap$push("inputWidgetGroups_full", input$group_sameTabWidget[1])
  newVal <- isTRUE(as.logical(input$group_sameTabWidget[2]))
  if (arrayIdx <= length(rv$generalConfig[["inputWidgetGroups"]]) &&
    length(rv$generalConfig[["inputWidgetGroups"]][[arrayIdx]])) {
    rv$generalConfig[["inputWidgetGroups"]][[arrayIdx]]$sameTab <<- newVal
  } else if (arrayIdxAll <= length(groupTemp[["inputWidgetGroups"]]) &&
    length(groupTemp[["inputWidgetGroups"]][[arrayIdxAll]])) {
    groupTemp[["inputWidgetGroups"]][[arrayIdxAll]]$sameTab <<- newVal
  } else {
    groupTemp[["inputWidgetGroups"]][[arrayIdxAll]] <<- list(sameTab = newVal)
  }
})
observeEvent(input$group_sameTabOut, {
  if (length(input$group_sameTabOut) < 2L) {
    return()
  }

  arrayIdx <- indexMap$push("outputGroups", input$group_sameTabOut[1])
  arrayIdxAll <- indexMap$push("outputGroups_full", input$group_sameTabOut[1])
  newVal <- input$group_sameTabOut[2]
  sameTabNewVal <- !identical(newVal, "_")
  if (sameTabNewVal) {
    newVal <- as.integer(newVal)
  } else {
    newVal <- 1L
  }

  if (arrayIdx <= length(rv$generalConfig[["outputGroups"]]) &&
    length(rv$generalConfig[["outputGroups"]][[arrayIdx]])) {
    rv$generalConfig[["outputGroups"]][[arrayIdx]]$sameTab <<- sameTabNewVal
    rv$generalConfig[["outputGroups"]][[arrayIdx]]$colsPerRow <<- newVal
  } else if (arrayIdxAll <= length(groupTemp[["outputGroups"]]) &&
    length(groupTemp[["outputGroups"]][[arrayIdxAll]])) {
    groupTemp[["outputGroups"]][[arrayIdxAll]]$sameTab <<- sameTabNewVal
    groupTemp[["outputGroups"]][[arrayIdxAll]]$colsPerRow <<- newVal
  } else {
    groupTemp[["outputGroups"]][[arrayIdxAll]] <<- list(sameTab = sameTabNewVal, colsPerRow = newVal)
  }
})
observeEvent(input$remove_general, {
  arrayID <- strsplit(input$remove_general[1], "_")[[1]][2]
  arrayIdx <- indexMap$pop(
    arrayID,
    input$remove_general[2]
  )
  arrayIdxAll <- indexMap$pop(
    paste0(arrayID, "_full"),
    input$remove_general[2]
  )
  if (length(arrayIdx) &&
    arrayIdx <= length(rv$generalConfig[[arrayID]])) {
    rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
    if (!length(rv$generalConfig[[arrayID]])) {
      rv$generalConfig[[arrayID]] <<- NULL
    }
    if (identical(arrayID, "inputWidgetGroups")) {
      updateSheetOrderInput(arrayIdxAll)
    }
  }
  if (length(arrayIdxAll) &&
    arrayIdxAll <= length(groupTemp[[arrayID]])) {
    groupTemp[[arrayID]][[arrayIdxAll]] <<- NULL
  }
})
observeEvent(input$btEditReadme, {
  req(
    length(rv$generalConfig$readme$filename) > 0L,
    nchar(trimws(rv$generalConfig$readme$filename)) > 0L
  )
  readmeContent <- character(0L)
  readmeContentParsed <- character(0L)
  if (length(rv$generalConfig$readme$filename) &&
    file.exists(file.path(
      currentModelDir,
      rv$generalConfig$readme$filename
    ))) {
    readmeContent <- read_file(file.path(
      currentModelDir,
      rv$generalConfig$readme$filename
    ))
    readmeContentParsed <- HTML(markdownParser$
      parseFile(file.path(
      currentModelDir,
      rv$generalConfig$
        readme$filename
    )))
  }

  showModal(modalDialog(
    title = lang$adminMode$general$readme$dialogEdit$title,
    tags$div(
      class = "gmsalert gmsalert-error center-alert", id = "mdSaveError",
      lang$adminMode$general$readme$dialogEdit$msgErrSave
    ),
    fluidRow(
      column(
        6L,
        tags$div(class = "readme-preview-header", lang$adminMode$general$readme$dialogEdit$reamdeHeader),
        tags$textarea(
          id = "mdContent", class = "readme-wrapper readme-preview-markdown",
          oninput = paste0(
            "Miro.mdToHTML(this.value,'mdConvertedContent',",
            if (isTRUE(input$general_readmeEnableMath)) "true" else "false", ")"
          ),
          readmeContent
        )
      ),
      column(
        6L,
        tags$div(class = "readme-preview-header", lang$adminMode$general$readme$dialogEdit$markdownHeader),
        tags$div(
          id = "mdConvertedContent",
          class = "readme-wrapper readme-preview-output", readmeContentParsed
        )
      ),
      tags$script(paste0(
        "setTimeout(function(){Miro.mdToHTML(document.getElementById('mdContent').value,'mdConvertedContent',",
        if (isTRUE(input$general_readmeEnableMath)) "true" else "false", ")},500)"
      ))
    ),
    footer = tagList(
      modalButton(lang$adminMode$general$readme$dialogEdit$btCancel),
      tags$button(
        id = "btMdSave", lang$adminMode$general$readme$dialogEdit$btSave, onclick = "Miro.mdSave('#mdContent')",
        class = "btn btn-default bt-highlight-1"
      )
    ),
    fade = TRUE, easyClose = FALSE, size = "l"
  ))
})
observeEvent(input$btMdSave, {
  req(length(rv$generalConfig$readme$filename))
  tryCatch(
    {
      write_file(input$btMdSave, file.path(
        currentModelDir,
        rv$generalConfig$readme$filename
      ))
      removeModal()
    },
    error = function(e) {
      showHideEl(session, "mdSaveError", 4000L)
    }
  )
})

#  =======================================
#          SAVE JSON (automatically)
#  =======================================
observeEvent(list(rv$generalConfig, trigger()), {
  req(length(rv$generalConfig))
  configJSON$inputGroups <<- NULL
  configJSON$outputGroups <<- NULL
  configJSON$inputWidgetGroups <<- NULL
  if (length(rv$generalConfig$inputGroups) ||
    length(rv$generalConfig$outputGroups) ||
    length(rv$generalConfig$inputWidgetGroups)) {
    newGeneralJSON <- rv$generalConfig
    newGeneralJSON$inputGroups[vapply(newGeneralJSON$inputGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    newGeneralJSON$inputWidgetGroups[vapply(newGeneralJSON$inputWidgetGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    newGeneralJSON$outputGroups[vapply(newGeneralJSON$outputGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    configJSON <<- modifyList(configJSON, newGeneralJSON)
  } else {
    configJSON <<- modifyList(configJSON, rv$generalConfig)
  }
  configJSON$symbolLinks <<- rv$generalConfig$symbolLinks
  configJSON$outputAttachments <<- rv$generalConfig$outputAttachments
  jsonConfig$write(configJSON)
})

outputOptions(output, "themeColorsUI", suspendWhenHidden = FALSE)
