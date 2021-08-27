getIndexLists <- function(setIndices, options = list()) {
  unassignedSetIndices <- setIndices
  indices <- list(
    rows = character(0L),
    cols = character(0L),
    filter = character(0L),
    aggregations = character(0L)
  )

  for (id in c("cols", "filter", "aggregations")) {
    indexIds <- match(names(options[[id]]), unassignedSetIndices)
    indexIds <- indexIds[!is.na(indexIds)]
    indices[[id]] <- unassignedSetIndices[indexIds]
    if (length(indexIds)) {
      unassignedSetIndices <- unassignedSetIndices[-indexIds]
    }
  }
  if (length(options[["rows"]])) {
    rowIndices <- unlist(options[["rows"]])
    missingIndices <- unassignedSetIndices[!unassignedSetIndices %in% rowIndices]
    indexIds <- match(rowIndices, unassignedSetIndices)
    indices[["rows"]] <- c(
      unassignedSetIndices[indexIds[!is.na(indexIds)]],
      missingIndices
    )
  } else {
    indices[["rows"]] <- unassignedSetIndices
  }
  return(indices)
}
genIndexList <- function(indexList) {
  return(lapply(seq_along(indexList), function(idx) {
    tags$li(
      class = "drop-index-item", "data-rank-id" = indexList[idx],
      names(indexList)[idx]
    )
  }))
}
createBootstrapDropdownChoices <- function(el, eventId, editEventId = NULL, deleteEventId = NULL) {
  tags$li(
    id = paste0(eventId, "_", el$id), class = "dropdown-item-wrapper",
    tags$a(
      class = "dropdown-item view-dropdown-item", role = "button", el$alias,
      onClick = paste0(
        "Shiny.setInputValue('", eventId, "','",
        el$id, "',{priority:\'event\'});"
      )
    ),
    if (!is.null(editEventId)) {
      tags$a(
        role = "button", icon("pen"), class = "miro-pivot-view-button",
        onClick = paste0(
          "Shiny.setInputValue('", editEventId, "','",
          el$id, "',{priority:\'event\'});"
        )
      )
    },
    if (!is.null(deleteEventId)) {
      tags$a(
        role = "button", icon("times"), class = "miro-pivot-view-button",
        onClick = paste0(
          "Shiny.setInputValue('", deleteEventId, "','",
          el$id, "',{priority:\'event\'});"
        )
      )
    }
  )
}

miroPivotOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)

  if (is.null(options[["_metadata_"]]$headers)) {
    unassignedSetIndices <- c()
  } else {
    unassignedSetIndices <- setNames(
      names(options[["_metadata_"]]$headers),
      vapply(options[["_metadata_"]]$headers, "[[", character(1L),
        "alias",
        USE.NAMES = FALSE
      )
    )
  }

  noNumericCol <- sum(vapply(options[["_metadata_"]]$headers, function(header) {
    return(identical(header$type, "numeric"))
  }, logical(1L)))

  if (noNumericCol > 1L) {
    # symbol is table and needs to be pivoted
    unassignedSetIndices <- c(unassignedSetIndices[seq_len(length(unassignedSetIndices) - noNumericCol)],
      Header = "Hdr"
    )
  } else {
    unassignedSetIndices <- unassignedSetIndices[-length(unassignedSetIndices)]
  }

  indices <- getIndexLists(unassignedSetIndices, options)
  aggregationFunctions <- if (identical(options[["_metadata_"]]$symtype, "set")) {
    setNames(
      c("count", "min"),
      c(
        lang$renderers$miroPivot$aggregationFunctions$count,
        lang$renderers$miroPivot$aggregationFunctions$min
      )
    )
  } else {
    setNames(
      c("sum", "count", "mean", "median", "min", "max", "sd"),
      c(
        lang$renderers$miroPivot$aggregationFunctions$sum,
        lang$renderers$miroPivot$aggregationFunctions$count,
        lang$renderers$miroPivot$aggregationFunctions$mean,
        lang$renderers$miroPivot$aggregationFunctions$median,
        lang$renderers$miroPivot$aggregationFunctions$min,
        lang$renderers$miroPivot$aggregationFunctions$max,
        lang$renderers$miroPivot$aggregationFunctions$sd
      )
    )
  }

  tags$div(
    id = ns("container"), style = "overflow:hidden;",
    tags$div(id = ns("customError"), class = "gmsalert gmsalert-error"),
    if (length(options$domainFilter$domains)) {
      fluidRow(
        class = "domain-filter",
        style = "margin:0",
        do.call(
          tabsetPanel,
          c(
            id = ns("domainFilter"),
            selected = options$domainFilter$default,
            lapply(options$domainFilter$domains, function(domain) {
              domainId <- match(domain, unassignedSetIndices)
              return(tabPanel(
                title = names(unassignedSetIndices)[domainId],
                value = domain
              ))
            })
          )
        ),
        tags$div(class = "small-space")
      )
    },
    fluidRow(
      class = "row-agg-filter",
      style = "margin:0;padding-top: 5pt;",
      column(
        width = 2L, style = "padding: 1em;",
        style = if (isTRUE(options$hidePivotControls)) {
          "padding: 0;"
        } else {
          "padding: 1em;"
        },
        tags$div(
          class = "drop-index-header presentation-hide",
          style = if (isTRUE(options$hidePivotControls)) "display:none;",
          lang$renderers$miroPivot$filterLabel
        ),
        tags$ul(
          id = ns("filterIndexList"),
          class = "drop-index-list filter-index-list presentation-hide",
          style = if (isTRUE(options$hidePivotControls)) "display:none;",
          genIndexList(indices$filter)
        ),
        if (!isFALSE(options$enablePersistentViews)) {
          tagList(
            tags$div(
              class = "presentation-hide", style = if (isTRUE(options$hidePivotControls)) "display:none;",
              actionButton(ns("saveView"),
                label = NULL, icon = icon("plus-square"),
                title = lang$renderers$miroPivot$btNewView,
                class = "btn-custom",
                style = "margin-bottom: 5px;width:38.25px;"
              ),
              tags$a(
                id = ns("downloadCsv"),
                class = "btn btn-default shiny-download-link btn-custom",
                style = "margin-bottom: 5px;width:38.25px;",
                href = "",
                target = "_blank",
                download = NA,
                title = lang$renderers$miroPivot$btDownloadCsv,
                tags$i(class = "fa fa-file-csv")
              ),
              tags$a(
                id = ns("downloadPng"),
                class = "btn btn-default bt-export-canvas btn-custom",
                style = "margin-bottom: 5px;width:38.25px;",
                download = "chart.png",
                href = "#",
                `data-canvasid` = ns("pivotChart"),
                tags$i(class = "fa fa-file-image"),
                title = lang$renderers$miroPivot$btDownloadPng,
                style = "display:none;"
              ),
              tags$div(
                id = ns("hidePivotControls"), class = "btn btn-default btn-custom activate-pivot-controls",
                style = "margin-bottom: 5px;width:38.25px;",
                icon("table"), title = lang$renderers$miroPivot$btHidePivotControls, `data-id` = ns("")
              )
            ),
            tags$div(
              class = "dropdown presentation",
              style = if (isTRUE(options$hidePivotControls)) "margin-top: 0;",
              tags$button(
                class = "btn btn-default dropdown-toggle btn-dropdown",
                style = "width:100%",
                type = "button", id = ns("toggleViewButton"),
                onclick = "Miro.resetDropdownFilter(this)",
                `data-toggle` = "dropdown", `aria-haspopup` = "true",
                `aria-expanded` = "false",
                lang$renderers$miroPivot$btLoadView, tags$span(
                  class = "caret btn-dropdown-caret"
                )
              ),
              tags$ul(
                id = ns("savedViewsDD"), class = "dropdown-menu btn-dropdown-menu view-dropdown-wrapper",
                `aria-labelledby` = ns("toggleViewButton"),
              )
            )
          )
        } else {
          tags$div(
            class = "presentation-hide",
            tags$a(
              id = ns("downloadCsv"),
              class = "btn btn-default shiny-download-link btn-custom",
              style = if (isTRUE(options$hidePivotControls)) "display:none;",
              href = "",
              target = "_blank",
              download = NA,
              title = lang$renderers$miroPivot$btDownloadCsv,
              tags$i(class = "fa fa-file-csv")
            ),
            tags$a(
              id = ns("downloadPng"),
              class = "btn btn-default bt-export-canvas btn-custom",
              download = "chart.png",
              href = "#",
              `data-canvasid` = ns("pivotChart"),
              tags$i(class = "fa fa-file-image"),
              title = lang$renderers$miroPivot$btDownloadPng,
              style = "display:none;"
            ),
            tags$div(
              id = ns("hidePivotControls"), class = "btn btn-default btn-custom activate-pivot-controls",
              style = if (isTRUE(options$hidePivotControls)) "display:none;",
              icon("table"), title = "Hide pivot Controls", `data-id` = ns("")
            )
          )
        }
      ),
      column(
        width = 10L, class = "presentation-show",
        style = if (!isTRUE(options$hidePivotControls)) "display:none;",
        tags$a(
          `data-proxy-id` = ns("downloadCsv"),
          class = "btn btn-default shiny-download-link btn-custom btn-proxy",
          href = "#", tags$i(class = "fa fa-file-csv"),
          title = lang$renderers$miroPivot$btDownloadCsv
        ),
        tags$a(
          class = "btn btn-default bt-export-canvas btn-custom",
          style = "display:none;",
          download = "chart.png",
          href = "#",
          `data-canvasid` = ns("pivotChart"),
          tags$i(class = "fa fa-file-image"),
          title = lang$renderers$miroPivot$btDownloadPng
        ),
        tags$div(
          id = ns("showPivotControls"),
          class = "btn btn-default btn-custom deactivate-pivot-controls presentation-show",
          icon("table"), title = "Show pivot Controls", `data-id` = ns("")
        )
      ),
      column(
        width = 4L, class = "presentation-hide",
        style = if (isTRUE(options$hidePivotControls)) "padding: 1em;display:none;" else "padding: 1em;",
        tags$div(id = ns("filterDropdowns"), class = "miro-pivot-filter")
      ),
      column(
        width = 4L, class = "presentation-hide",
        style = if (isTRUE(options$hidePivotControls)) "padding: 1em;display:none;" else "padding: 1em;",
        tags$div(id = ns("aggregateDropdowns"), class = "miro-pivot-filter")
      ),
      column(
        width = 2L,
        style = if (isTRUE(options$hidePivotControls)) "padding: 1em;display:none;" else "padding: 1em;",
        class = "presentation-hide",
        tags$div(class = "drop-index-header", lang$renderers$miroPivot$aggregateLabel),
        tags$ul(
          id = ns("aggregationIndexList"), class = "drop-index-list aggregation-index-list",
          genIndexList(indices$aggregations)
        ),
        selectInput(ns("aggregationFunction"),
          label = NULL,
          choices = c()
        )
      )
    ),
    fluidRow(
      class = "col-filter",
      style = if (isTRUE(options$hidePivotControls)) "margin:0;display:none;" else "margin:0;",
      column(
        width = 2L,
        selectInput(ns("pivotRenderer"), "",
          setNames(
            c("table", "heatmap", "bar", "stackedbar", "line", "scatter", "area", "stackedarea", "radar"),
            c(
              lang$renderers$miroPivot$renderer$table,
              lang$renderers$miroPivot$renderer$heatmap,
              lang$renderers$miroPivot$renderer$bar,
              lang$renderers$miroPivot$renderer$stackedbar,
              lang$renderers$miroPivot$renderer$line,
              lang$renderers$miroPivot$renderer$scatter,
              lang$renderers$miroPivot$renderer$area,
              lang$renderers$miroPivot$renderer$stackedarea,
              lang$renderers$miroPivot$renderer$radar
            )
          ),
          selected = if (length(options$pivotRenderer)) {
            options$pivotRenderer
          } else {
            "table"
          }
        ),
        if (isTRUE(options$enableHideEmptyCols)) {
          checkboxInput_MIRO(ns("hideEmptyCols"), lang$renderers$miroPivot$cbHideEmptyCols,
            value = identical(options$hideEmptyCols, TRUE)
          )
        }
      ),
      column(
        width = 6L, style = "padding: 1em;",
        tags$ul(
          id = ns("colIndexList"), class = "drop-index-list vertical-index-list",
          genIndexList(indices$cols)
        )
      ),
      column(
        width = 4L,
        tags$div(id = ns("colDropdowns"), class = "miro-pivot-filter")
      )
    ),
    fluidRow(
      class = "table-chart", style = "margin:0",
      column(
        width = 2L,
        style = if (isTRUE(options$hidePivotControls)) {
          "padding: 1em;padding-top: 31px;display:none;"
        } else {
          "padding: 1em;padding-top: 31px;"
        },
        tags$ul(
          id = ns("rowIndexList"), class = "drop-index-list",
          genIndexList(indices$rows)
        )
      ),
      column(
        width = if (isTRUE(options$hidePivotControls)) 12L else 10L,
        class = if (isTRUE(options[["_input_"]])) "has-edit-buttons",
        style = if (isTRUE(options[["_input_"]]) && isTRUE(options$hidePivotControls)) {
          "min-height: 400px;margin-top:30px;"
        } else {
          "min-height: 400px;"
        },
        if (isTRUE(options[["_input_"]])) {
          tags$div(
            style = "position: absolute; top: -10px;z-index: 1;",
            actionButton(ns("btAddRow"), lang$renderers$miroPivot$btAddRow),
            actionButton(ns("btRemoveRows"), lang$renderers$miroPivot$btRemoveRows, class = "bt-highlight-1"),
            actionButton(ns("enableEdit"), lang$renderers$miroPivot$btEnableEdit,
              style = "display:none"
            )
          )
        },
        tags$div(
          id = ns("errMsg"), class = "gmsalert gmsalert-error",
          style = "position:static;margin-bottom:5px;"
        ),
        genSpinner(ns("loadPivotTable"), hidden = TRUE),
        DTOutput(ns("pivotTable")),
        chartjsOutput(ns("pivotChart"), height = "40px")
      )
    ),
    fluidRow(
      id = ns("dataView"), class = "data-section",
      style = if (!isTRUE(options$hidePivotControls)) "display:none;",
      fluidRow(
        style = "margin:0;display:flex;",
        column(
          width = 4L, class = "data-section-block",
          tags$ul(
            class = "drop-index-list-presentation",
            tags$li(
              class = "list-presentation",
              fluidRow(
                class = "row-presentation",
                column(
                  width = 4L, class = "column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$rows
                  )
                ),
                column(
                  width = 8L, class = "column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$filterLabel
                  )
                )
              )
            )
          ),
          tags$ul(id = ns("rowsPresentation"), class = "drop-index-list-presentation")
        ),
        column(
          width = 4L, class = "data-section-block",
          tags$ul(
            class = "drop-index-list-presentation",
            tags$li(
              class = "list-presentation",
              fluidRow(
                class = "row-presentation",
                column(
                  width = 4L, class = "column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$columns
                  )
                ),
                column(
                  width = 8L, class = "column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$filterLabel
                  )
                )
              )
            )
          ),
          tags$ul(id = ns("colsPresentation"), class = "drop-index-list-presentation")
        ),
        column(
          width = 4L, class = "data-section-block", style = "margin-right: 0;",
          tags$ul(
            class = "drop-index-list-presentation",
            tags$li(
              class = "list-presentation",
              fluidRow(
                class = "row-presentation",
                column(
                  width = 4L, class = "column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$aggregation
                  )
                ),
                column(
                  width = 8L, class = "column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$filterLabel
                  )
                )
              )
            )
          ),
          tags$ul(id = ns("aggPresentation"), class = "drop-index-list-presentation")
        )
      ),
      if (length(options$domainFilter$domains)) {
        fluidRow(
          style = "margin:0px auto 5px auto;",
          tags$div("Domain filter: Commodities")
        )
      }
    ),
    sortable_js(ns("filterIndexList"),
      options = sortable_options(
        group = ns("indices"), supportPointer = FALSE,
        multiDrag = TRUE,
        selectedClass = "drop-index-item-selected",
        onLoad = sortable_js_capture_input(ns("filterIndexList")),
        onSort = sortable_js_capture_input(ns("filterIndexList"))
      )
    ),
    sortable_js(ns("rowIndexList"),
      options = sortable_options(
        group = ns("indices"), supportPointer = FALSE,
        multiDrag = TRUE,
        selectedClass = "drop-index-item-selected",
        onLoad = sortable_js_capture_input(ns("rowIndexList")),
        onSort = sortable_js_capture_input(ns("rowIndexList"))
      )
    ),
    sortable_js(ns("colIndexList"),
      options = sortable_options(
        group = ns("indices"), supportPointer = FALSE, direction = "vertical",
        multiDrag = TRUE,
        selectedClass = "drop-index-item-selected",
        onLoad = sortable_js_capture_input(ns("colIndexList")),
        onSort = sortable_js_capture_input(ns("colIndexList"))
      )
    ),
    sortable_js(ns("aggregationIndexList"),
      options = sortable_options(
        group = ns("indices"), supportPointer = FALSE,
        multiDrag = TRUE,
        selectedClass = "drop-index-item-selected",
        onLoad = sortable_js_capture_input(ns("aggregationIndexList")),
        onSort = sortable_js_capture_input(ns("aggregationIndexList"))
      )
    )
  )
}

renderMiroPivot <- function(id, data, options = NULL, path = NULL, roundPrecision = 2L, rendererEnv = NULL, views = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      if (is.null(views)) {
        # deactivate persistent views if no view object available
        options$enablePersistentViews <- FALSE
      }

      valueColName <- names(data)[length(data)]
      allPlaceholder <- setNames("", lang$renderers$miroPivot$allPlaceholder)

      initFilter <- TRUE
      initData <- TRUE
      initRenderer <- TRUE
      isScalarTable <- FALSE
      resetFilters <- isTRUE(options$resetOnInit)

      SERIES_DEFAULT_COLOR <- "#666"

      numericCols <- vapply(data, class, character(1L), USE.NAMES = FALSE) %in% c("numeric", "integer")
      if (sum(numericCols) > 1L) {
        # data is already pivoted
        flog.info(
          "%s is already pivoted. MIRO pivot will unpivot the symbol.",
          options[["_metadata_"]]$symname
        )
        data <- pivot_longer(data, names(data)[numericCols],
          names_to = "Hdr",
          values_to = "value", names_repair = "unique"
        )
        valueColName <- "value"
      } else if (sum(numericCols) == 0L) {
        # data is a set -> drop last column and replace with 1
        names(data) <- c(names(data)[-length(data)], "value")
        valueColName <- "value"
        isScalarTable <- length(options[["_metadata_"]]$symname) &&
          options[["_metadata_"]]$symname %in% c(scalarsFileName, scalarsOutName)
        if (isScalarTable) {
          data <- suppressWarnings(mutate(data, value = as.numeric(value)))
        } else {
          data[, length(data)] <- 1L
        }
      }
      data <- mutate_if(data, is.character, as.factor)

      noColDim <- 1L
      setIndices <- names(data)[-length(data)]

      noRowDim <- length(data) - 1L

      updateFilter <- reactiveVal(1L)
      updateRenderer <- reactiveVal(1L)

      isInput <- isTRUE(options[["_input_"]])
      isEditable <- FALSE
      bigData <- FALSE

      hiddenEmptyCols <- NULL

      if (isInput) {
        dataUpdated <- reactiveVal(1L)
        if (nrow(data) < 5e+05) {
          isEditable <- TRUE
          hideEl(session, paste0("#", ns("enableEdit")))
          showEl(session, paste0("#", ns("btAddRow")))
          showEl(session, paste0("#", ns("btRemoveRows")))
          data <- unite(data, "__key__", !!!setIndices,
            remove = FALSE, sep = "\U2024", na.rm = FALSE
          )
        } else {
          bigData <- TRUE
          showEl(session, paste0("#", ns("enableEdit")))
          hideEl(session, paste0("#", ns("btAddRow")))
          hideEl(session, paste0("#", ns("btRemoveRows")))
          rendererEnv[[ns("enableEdit")]] <- observe({
            if (length(input$enableEdit) != 1L || input$enableEdit == 0L) {
              return()
            }
            if (isEditable) {
              # already editable
              return()
            }
            flog.trace("MIRO pivot: enable edit button clicked.")
            showEl(session, "#loading-screen")
            on.exit(hideEl(session, "#loading-screen"))
            data <<- unite(data, "__key__", !!!setIndices,
              remove = FALSE, sep = "\U2024", na.rm = FALSE
            )
            isEditable <<- TRUE
            hideEl(session, paste0("#", ns("enableEdit")))
            showEl(session, paste0("#", ns("btAddRow")))
            showEl(session, paste0("#", ns("btRemoveRows")))
          })
        }
      }

      noUpdateFilterEl <- logical(length(setIndices))
      names(noUpdateFilterEl) <- setIndices

      setIndexAliases <- vapply(options[["_metadata_"]]$headers, "[[", character(1L),
        "alias",
        USE.NAMES = FALSE
      )[seq_along(setIndices)]
      if (sum(numericCols) > 1L) {
        setIndexAliases[length(setIndices)] <- "Header"
      }

      indices <- character(0L)
      # we need to update aggregation functions in case the symbol type is not available when rendering the UI
      # (e.g. in Configuration Mode)
      aggregationFunctions <- if (identical(options[["_metadata_"]]$symtype, "set")) {
        setNames(
          c("count", "min"),
          c(
            lang$renderers$miroPivot$aggregationFunctions$count,
            lang$renderers$miroPivot$aggregationFunctions$min
          )
        )
      } else {
        setNames(
          c("sum", "count", "mean", "median", "min", "max", "sd"),
          c(
            lang$renderers$miroPivot$aggregationFunctions$sum,
            lang$renderers$miroPivot$aggregationFunctions$count,
            lang$renderers$miroPivot$aggregationFunctions$mean,
            lang$renderers$miroPivot$aggregationFunctions$median,
            lang$renderers$miroPivot$aggregationFunctions$min,
            lang$renderers$miroPivot$aggregationFunctions$max,
            lang$renderers$miroPivot$aggregationFunctions$sd
          )
        )
      }

      if (length(options$customChartColors)) {
        customChartColors <- options$customChartColors
      } else {
        customChartColors <- c(
          "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
          "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
          "#cab2d6", "#6a3d9a", "#ffff99", "#b15928",
          "#FCDCDB", "#f9b9b7", "#D5D3DA", "#ada9b7",
          "#e2546b", "#66101F", "#E0ABD7", "#c45ab3",
          "#8bf2fe", "#1BE7FF", "#a1d1b6", "#4C9F70",
          "#F6FAA9", "#f0f757", "#d3b499", "#9E6D42",
          "#50caf3", "#086788", "#eee49d", "#E0CA3C",
          "#dbcac7", "#BA9790", "#f69e84", "#EB4511",
          "#ccadf1", "#9B5DE5", "#A1FB8B", "#47fa1a",
          "#8dadd0", "#38618c", "#fcebea", "#fad8d6",
          "#a6b474", "#373d20", "#a248ce", "#210b2c",
          "#f37ea9", "#d81159", "#68f7f7", "#08bdbd",
          "#98feb1", "#35ff69", "#d27193", "#6d213c",
          "#edfab1", "#dcf763", "#feb46f", "#e06c00",
          "#f3ebaa", "#e9d758", "#c0c7c7", "#829191",
          "#f3cac5", "#E8998D", "#c7dac9", "#91b696",
          "#BE99A4", "#714955", "#7c7ccf", "#2a2a72",
          "#7efee0", "#00ffc5", "#c28eb1", "#6c3a5c",
          "#df7192", "#8b1e3f", "#95D86B", "#3E721D"
        )
      }

      if (isTRUE(options$hidePivotControls)) {
        session$sendCustomMessage("gms-activateMiroPivotPresentationObservers", ns(""))
      }

      resetView <- function(viewOptions, domainFilterDomains, interfaceInitialized = TRUE) {
        unassignedSetIndices <- setNames(
          setIndices,
          setIndexAliases
        )

        indices <<- getIndexLists(unassignedSetIndices, viewOptions)
        for (indexEl in list(
          c("filter", "filterIndexList"),
          c("rows", "rowIndexList"),
          c("cols", "colIndexList"),
          c("aggregations", "aggregationIndexList")
        )) {
          session$sendCustomMessage(
            "gms-updateSortable",
            list(
              id = ns(indexEl[[2]]),
              children = lapply(
                genIndexList(indices[[indexEl[[1]]]]),
                as.character
              )
            )
          )
        }

        if (length(viewOptions[["pivotRenderer"]]) &&
          viewOptions[["pivotRenderer"]] %in% c("table", "heatmap", "line", "scatter", "area", "stackedarea", "bar", "stackedbar", "radar")) {
          updateSelectInput(session, "pivotRenderer", selected = viewOptions[["pivotRenderer"]])
        } else {
          updateSelectInput(session, "pivotRenderer", selected = "table")
        }
        if (isTRUE(options$enableHideEmptyCols)) {
          updateCheckboxInput(session, "hideEmptyCols",
            value = identical(viewOptions[["hideEmptyCols"]], TRUE)
          )
        }
        newView <- list(
          filter = unname(indices$filter),
          aggregations = unname(indices$aggregations),
          cols = unname(indices$cols)
        )
        if (length(domainFilterDomains)) {
          if (length(viewOptions[["domainFilter"]][["default"]]) &&
            viewOptions[["domainFilter"]][["default"]] %in% domainFilterDomains) {
            updateTabsetPanel(session, "domainFilter",
              selected = viewOptions[["domainFilter"]][["default"]]
            )
            newView$domainFilter <- viewOptions[["domainFilter"]][["default"]]
          } else {
            updateTabsetPanel(session, "domainFilter",
              selected = domainFilterDomains[[1]]
            )
            newView$domainFilter <- domainFilterDomains[[1]]
          }
        }
        if (!interfaceInitialized) {
          return()
        }
        if (length(viewOptions[["aggregationFunction"]]) &&
          viewOptions[["aggregationFunction"]] %in% aggregationFunctions) {
          updateSelectInput(session, "aggregationFunction",
            selected = viewOptions[["aggregationFunction"]]
          )
        } else {
          updateSelectInput(session, "aggregationFunction",
            selected = aggregationFunctions[[1]]
          )
        }
        initFilter <<- TRUE
        resetFilters <<- TRUE
        initData <<- TRUE
        initRenderer <<- TRUE
        options$resetOnInit <<- TRUE
        noUpdateFilterEl[] <<- FALSE
        isolate({
          if (identical(currentFilters(), newView)) {
            newVal <- updateFilter() + 1L
            updateFilter(newVal)
          }
        })
      }
      if (isTRUE(options$resetOnInit)) {
        resetView(options, options[["domainFilter"]]$domains, interfaceInitialized = FALSE)
      }
      if (is.null(input$aggregationFunction) || identical(input$aggregationFunction, "")) {
        # interface has not been initialised, do it now
        resetFilters <- TRUE
        if (length(options[["aggregationFunction"]]) &&
          options[["aggregationFunction"]] %in% aggregationFunctions) {
          updateSelectInput(session, "aggregationFunction",
            choices = aggregationFunctions,
            selected = options[["aggregationFunction"]]
          )
        } else {
          updateSelectInput(session, "aggregationFunction",
            choices = aggregationFunctions,
            selected = aggregationFunctions[[1]]
          )
        }
      }

      setIndexAliases <- as.list(setIndexAliases)
      names(setIndexAliases) <- setIndices
      currentView <- options
      if (!is.null(rendererEnv[[ns("chartOptions")]])) {
        if (isTRUE(options$resetOnInit)) {
          rendererEnv[[ns("chartOptions")]] <- NULL
        } else {
          currentView$chartOptions <- rendererEnv[[ns("chartOptions")]]
        }
      }

      miroPivotState <- list(
        currentSeriesLabels = character(),
        triggerEditViewDialog = FALSE,
        editView = FALSE
      )

      if (!isFALSE(options$enablePersistentViews)) {
        updateViewList <- function() {
          removeUI(paste0("#", ns("savedViewsDD"), " .dropdown-item-wrapper"), multiple = TRUE)

          viewChoices <- lapply(sort(views$getIds(session)), function(viewId) {
            createBootstrapDropdownChoices(
              list(
                id = htmlIdEnc(viewId),
                alias = viewId
              ),
              ns("savedViews"), ns("editView"), ns("deleteView")
            )
          })
          insertUI(paste0("#", ns("savedViewsDD")),
            c(
              list(
                tags$input(
                  type = "text",
                  placeholder = lang$renderers$dropdownFilter$placeholder,
                  class = "form-control miro-dropdown-filter dropdown-item-wrapper",
                  onkeyup = "Miro.filterMiroDropdown(this)"
                ),
                createBootstrapDropdownChoices(
                  list(
                    id = "iZGVmYXVsdA--",
                    alias = lang$renderers$miroPivot$defaultViewName
                  ),
                  ns("savedViews")
                )
              ),
              viewChoices
            ),
            where = "beforeEnd"
          )
        }
        updateViewList()
        readonlyViews <- views$isReadonly(session)
        if (readonlyViews) {
          disableEl(session, paste0("#", ns("saveView")))
        } else {
          views$registerUpdateCallback(session, updateViewList)
        }

        showAddViewDialog <- function(pivotRenderer, viewOptions = NULL) {
          miroPivotState$editView <<- length(viewOptions) > 0L
          if (length(pivotRenderer) &&
            pivotRenderer %in% c("line", "scatter", "area", "stackedarea", "area", "stackedarea", "bar", "stackedbar", "radar")) {
            moreOptions <- NULL
            if (pivotRenderer %in% c("bar", "stackedbar", "line", "scatter", "area", "stackedarea")) {
              moreOptions <- tags$div(
                class = "row",
                tags$div(
                  class = "col-sm-12",
                  textInput(ns("advancedTitle"),
                    width = "100%",
                    lang$renderers$miroPivot$newViewChartTitle,
                    value = viewOptions$chartOptions$title
                  )
                ),
                tags$div(
                  class = "col-sm-6",
                  textInput(ns("advancedxTitle"),
                    width = "100%",
                    lang$renderers$miroPivot$newViewxTitle,
                    value = viewOptions$chartOptions$xTitle
                  )
                ),
                tags$div(
                  class = "col-sm-6",
                  textInput(ns("advancedyTitle"),
                    width = "100%",
                    lang$renderers$miroPivot$newViewyTitle,
                    value = viewOptions$chartOptions$yTitle
                  )
                )
              )
            }
            customChartColorsUI <- lapply(seq_along(miroPivotState$currentSeriesLabels), function(labelId) {
              colorLabel <- miroPivotState$currentSeriesLabels[labelId]
              if (length(names(viewOptions$chartOptions$customChartColors))) {
                if (colorLabel %in% names(viewOptions$chartOptions$customChartColors)) {
                  colorVal <- viewOptions$chartOptions$customChartColors[[colorLabel]][1]
                } else {
                  colorVal <- SERIES_DEFAULT_COLOR
                }
              } else {
                colorVal <- customChartColors[(labelId - 1L) * 2L + 1L]
              }
              tags$div(
                class = "col-sm-6",
                colorPickerInput(ns(paste0("customChartColor_", labelId)),
                  colorLabel,
                  colorVal,
                  colorBox = TRUE
                )
              )
            })
            additionalOptionsContent <- tags$div(
              id = ns("newViewOptionsWrapper"), style = "text-align:left;",
              tags$div(
                moreOptions,
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-sm-6",
                    checkboxInput_MIRO(ns("useCustomChartColors"),
                      label = lang$renderers$miroPivot$newViewCbCustomColors,
                      value = length(viewOptions$chartOptions$customChartColors) > 0L
                    )
                  ),
                  tags$div(
                    class = "col-sm-6",
                    `data-display-if` = "input.useCustomChartColors===true",
                    `data-ns-prefix` = ns(""),
                    checkboxInput_MIRO("miroPivotCbCustomColorInputs",
                      label = lang$renderers$miroPivot$newViewCbManualColors,
                      value = FALSE
                    )
                  )
                ),
                conditionalPanel("input.useCustomChartColors===true",
                  ns = ns,
                  tags$div(
                    class = "row miro-pivot-custom-colors-wrapper",
                    customChartColorsUI
                  )
                )
              )
            )
          } else {
            additionalOptionsContent <- NULL
          }
          showModal(modalDialog(tags$div(
            id = ns("errUniqueName"), style = "display:none;",
            lang$renderers$miroPivot$errUniqueViewName
          ),
          textInput(ns("newViewName"),
            width = "100%",
            lang$renderers$miroPivot$newViewLabel,
            value = viewOptions$name
          ),
          additionalOptionsContent,
          footer = tagList(
            tags$div(
              id = ns("saveViewButtonsWrapper"),
              modalButton(lang$renderers$miroPivot$newViewBtCancel),
              actionButton(ns("saveViewConfirm"),
                lang$renderers$miroPivot$newViewBtSave,
                class = "bt-highlight-1 bt-gms-confirm"
              )
            ),
            tags$div(
              id = ns("saveViewOverwriteButtonsWrapper"), style = "display:none",
              actionButton(
                ns("saveViewCancelOverwrite"),
                lang$renderers$miroPivot$newViewBtCancelOverwrite
              ),
              actionButton(ns("saveViewOverwrite"),
                lang$renderers$miroPivot$newViewBtOverwrite,
                class = "bt-highlight-1 bt-gms-confirm"
              )
            )
          ),
          fade = TRUE, easyClose = FALSE, size = "m",
          title = lang$renderers$miroPivot$newViewTitle
          ))
        }

        rendererEnv[[ns("saveView")]] <- observe({
          if (is.null(input$saveView) || initData || input$saveView == 0L || readonlyViews) {
            return()
          }
          showAddViewDialog(isolate(input$pivotRenderer))
        })
        deleteView <- function(viewId) {
          views$remove(session, viewId)
          removeUI(paste0("#", ns("savedViews"), "_", htmlIdEnc(viewId)))
        }
        addNewView <- function(viewName, overwrite = FALSE) {
          isolate({
            newViewConfig <- list(
              aggregationFunction = input$aggregationFunction,
              pivotRenderer = input$pivotRenderer,
              domainFilter = list(default = input$domainFilter)
            )
            if (isTRUE(options$enableHideEmptyCols)) {
              newViewConfig$hideEmptyCols <- identical(input$hideEmptyCols, TRUE)
            }
            for (indexEl in list(c("rows", "rowIndexList"))) {
              indexVal <- input[[indexEl[[2]]]]
              if (length(indexVal)) {
                newViewConfig[[indexEl[[1]]]] <- indexVal
              }
            }
            for (indexEl in list(
              c("aggregations", "aggregationIndexList"),
              c("filter", "filterIndexList"),
              c("cols", "colIndexList")
            )) {
              indexVal <- input[[indexEl[[2]]]]
              if (length(indexVal)) {
                filterElList <- lapply(indexVal, function(el) {
                  return(input[[paste0("filter_", el)]])
                })
                names(filterElList) <- indexVal
                newViewConfig[[indexEl[[1]]]] <- filterElList
              }
            }
            refreshRequired <- FALSE
            if (length(isolate(input$pivotRenderer)) &&
              isolate(input$pivotRenderer) %in% c("line", "scatter", "area", "stackedarea", "bar", "stackedbar", "radar")) {
              for (advancedOption in list(
                list(
                  inputId = "advancedTitle",
                  optionId = "title"
                ),
                list(
                  inputId = "advancedxTitle",
                  optionId = "xTitle"
                ),
                list(
                  inputId = "advancedyTitle",
                  optionId = "yTitle"
                )
              )) {
                optionValTmp <- input[[advancedOption$inputId]]
                if (length(optionValTmp)) {
                  optionValTmp <- trimws(optionValTmp)
                  if (nchar(optionValTmp) > 0L) {
                    refreshRequired <- TRUE
                    newViewConfig$chartOptions[[advancedOption$optionId]] <- optionValTmp
                  }
                }
              }
              if (isTRUE(input[["useCustomChartColors"]])) {
                refreshRequired <- TRUE
                newViewConfig$chartOptions[["customChartColors"]] <- setNames(
                  lapply(seq_along(miroPivotState$currentSeriesLabels), function(labelId) {
                    seriesColor <- input[[paste0("customChartColor_", labelId)]]
                    hoverColor <- tryCatch(colorspace::darken(seriesColor, amount = 0.3),
                      error = function(e) {
                        flog.warn(
                          "MIRO Piovot: Problems darkening color for label: %s.",
                          miroPivotState$currentSeriesLabels[labelId]
                        )
                        return(NA)
                      }
                    )
                    if (is.na(hoverColor)) {
                      return(c(SERIES_DEFAULT_COLOR, SERIES_DEFAULT_COLOR))
                    }
                    return(c(seriesColor, hoverColor))
                  }), miroPivotState$currentSeriesLabels
                )
              }
            }
            if (overwrite) {
              views$add(session, viewName, newViewConfig)
            } else {
              insertUI(paste0("#", ns("savedViewsDD")),
                createBootstrapDropdownChoices(
                  list(
                    id = htmlIdEnc(viewName),
                    alias = viewName
                  ),
                  ns("savedViews"), ns("editView"), ns("deleteView")
                ),
                where = "beforeEnd"
              )
              views$add(session, viewName, newViewConfig)
            }
            if (miroPivotState$editView &&
              !identical(viewName, currentView$name)) {
              # view was renamed
              deleteView(currentView$name)
            }
            if (refreshRequired) {
              currentView <<- newViewConfig
              currentView$name <<- viewName
              newVal <- updateRenderer() + 1L
              updateRenderer(newVal)
            }
          })
        }
        # rendererEnv[[ns("togglePivotControls")]] <- observe({
        #     hideEl(session, paste0("#", ns("dataView")))
        # })
        rendererEnv[[ns("saveViewConfirm")]] <- observe({
          if (is.null(input$saveViewConfirm) || initData ||
            input$saveViewConfirm == 0L || readonlyViews) {
            return()
          }
          newViewNameTrimmed <- trimws(isolate(input$newViewName))
          if (identical(newViewNameTrimmed, "")) {
            return()
          }
          overwriteView <- miroPivotState$editView && identical(newViewNameTrimmed, currentView$name)
          if (newViewNameTrimmed %in% c("default", views$getIds(session)) &&
            !overwriteView) {
            hideEl(session, paste0("#", ns("newViewName")))
            hideEl(session, paste0("#", ns("saveViewButtonsWrapper")))
            hideEl(session, paste0("#", ns("newViewOptionsWrapper")))
            showEl(session, paste0("#", ns("errUniqueName")))
            showEl(session, paste0("#", ns("saveViewOverwriteButtonsWrapper")))
            return()
          }
          addNewView(newViewNameTrimmed, overwrite = overwriteView)
          removeModal(session)
        })
        rendererEnv[[ns("saveViewOverwriteConfirm")]] <- observe({
          if (is.null(input$saveViewOverwrite) || initData ||
            input$saveViewOverwrite == 0L || readonlyViews) {
            return()
          }
          newViewNameTrimmed <- trimws(isolate(input$newViewName))
          if (identical(newViewNameTrimmed, "")) {
            flog.error("New view name is empty. Should never happen!")
            return()
          }
          addNewView(newViewNameTrimmed, overwrite = TRUE)
          removeModal(session)
        })
        rendererEnv[[ns("saveViewCancelOverwrite")]] <- observe({
          if (is.null(input$saveViewCancelOverwrite) || initData ||
            input$saveViewCancelOverwrite == 0L || readonlyViews) {
            return()
          }
          hideEl(session, paste0("#", ns("saveViewOverwriteButtonsWrapper")))
          hideEl(session, paste0("#", ns("errUniqueName")))
          showEl(session, paste0("#", ns("newViewName")))
          showEl(session, paste0("#", ns("newViewOptionsWrapper")))
          showEl(session, paste0("#", ns("saveViewButtonsWrapper")))
        })
        rendererEnv[[ns("editView")]] <- observe({
          if (is.null(input$editView) || initData || readonlyViews) {
            return()
          }
          viewId <- htmlIdDec(input$editView)
          if (length(viewId) != 1L ||
            !viewId %in% views$getIds(session)) {
            flog.error(
              "Invalid view id: '%s' attempted to be edited. This looks like an attempt to tamper with the app!",
              input$editView
            )
            return()
          }
          miroPivotState$triggerEditViewDialog <<- TRUE
          currentView <<- views$get(session, viewId)
          currentView$name <<- viewId
          resetView(currentView, options[["domainFilter"]]$domains)
        })
        rendererEnv[[ns("deleteView")]] <- observe({
          if (is.null(input$deleteView) || initData || readonlyViews) {
            return()
          }
          viewId <- htmlIdDec(input$deleteView)
          if (length(viewId) != 1L ||
            !viewId %in% views$getIds(session)) {
            flog.error(
              "Invalid view id: '%s' attempted to be removed. This looks like an attempt to tamper with the app!",
              input$deleteView
            )
            return()
          }
          deleteView(viewId)
        })
        rendererEnv[[ns("savedViews")]] <- observe({
          if (is.null(input$savedViews) || initData) {
            return()
          }
          viewId <- htmlIdDec(input$savedViews)
          if (length(viewId) != 1L ||
            !viewId %in% c("default", views$getIds(session))) {
            flog.error(
              "Invalid view id: '%s' attempted to be loaded. This looks like an attempt to tamper with the app!",
              input$savedViews
            )
            return()
          }
          currentView <<- if (identical(viewId, "default")) {
            options
          } else {
            views$get(session, viewId)
          }
          currentView$name <<- viewId
          resetView(currentView, options[["domainFilter"]]$domains)
        })
      }

      output$downloadCsv <- downloadHandler(
        filename = paste0(options[["_metadata_"]]$symname, ".csv"),
        content = function(file) {
          if (isInput) {
            dataUpdated()
          }
          if (length(hiddenEmptyCols)) {
            return(write_csv(dataToRender()[-hiddenEmptyCols],
              file,
              na = ""
            ))
          }
          return(write_csv(dataToRender(), file, na = ""))
        }
      )
      rendererEnv[[ns("filterDropdowns")]] <- observe({
        filterElements <- filteredData()$filterElements
        if (is.null(filteredData())) {
          return()
        }
        initData <- initFilter
        if (initFilter && isTRUE(options$resetOnInit)) {
          filterIndexList <- unname(indices[["filter"]])
          aggregationIndexList <- unname(indices[["aggregations"]])
          colIndexList <- unname(indices[["cols"]])
        } else {
          filterIndexList <- isolate(input$filterIndexList)
          aggregationIndexList <- isolate(input$aggregationIndexList)
          colIndexList <- isolate(input$colIndexList)
        }
        invalidFilters <- NULL
        getFilterDropdowns <- function(filterIndex, optionId = "filter") {
          allowEmpty <- optionId %in% c("aggregations", "cols")
          if (initData && resetFilters &&
            (allowEmpty || length(currentView[[optionId]][[filterIndex]]))) {
            currentFilterVal <- currentView[[optionId]][[filterIndex]]
            if (length(currentFilterVal)) {
              availableFilterVal <- currentFilterVal %in% filterElements[[filterIndex]]
              if (all(availableFilterVal)) {
                if (!identical(isolate(input[[paste0("filter_", filterIndex)]]), currentFilterVal)) {
                  noUpdateFilterEl[[filterIndex]] <<- TRUE
                }
              } else {
                invalidFiterValues <- paste(currentFilterVal[!availableFilterVal], collapse = "', '")
                flog.debug(
                  "View with invalid filter value(s): '%s' for domain: '%s' loaded.",
                  invalidFiterValues, filterIndex
                )
                invalidFilters <<- paste(invalidFilters, sprintf(
                  lang$renderers$miroPivot$invalidFilters$message,
                  setIndexAliases[[filterIndex]], invalidFiterValues
                ),
                sep = " "
                )
              }
            } else {
              availableFilterVal <- logical(0L)
            }
          } else {
            currentFilterVal <- isolate(input[[paste0("filter_", filterIndex)]])
            availableFilterVal <- currentFilterVal %in% filterElements[[filterIndex]]
          }
          if (any(availableFilterVal)) {
            selectedFilterVal <- currentFilterVal[availableFilterVal]
          } else {
            if (allowEmpty) {
              selectedFilterVal <- ""
              if (!is.null(isolate(input[[paste0("filter_", filterIndex)]]))) {
                noUpdateFilterEl[[filterIndex]] <<- TRUE
              }
            } else {
              selectedFilterVal <- filterElements[[filterIndex]][1]
              if (identical(isolate(input[[paste0("filter_", filterIndex)]]), selectedFilterVal)) {
                noUpdateFilterEl[[filterIndex]] <<- FALSE
              } else {
                noUpdateFilterEl[[filterIndex]] <<- TRUE
              }
            }
          }
          if (allowEmpty) {
            choices <- as.character(filterElements[[filterIndex]])
            choices <- c(allPlaceholder, setNames(choices, choices))
          } else {
            choices <- as.character(filterElements[[filterIndex]])
          }
          ddHash <- digest::digest(list(filterIndex, choices), algo = "sha1")
          list(
            htmltools::doRenderTags(
              htmltools::tagAppendAttributes(
                serverSelectInput(session, ns(paste0("filter_", filterIndex)), setIndexAliases[[filterIndex]],
                  choices = choices,
                  selected = selectedFilterVal, multiple = TRUE,
                  options = list("plugins" = list("remove_button"))
                ),
                `data-hash` = ddHash
              )
            ),
            ddHash,
            initData
          )
        }
        initFilter <<- FALSE
        session$
          sendCustomMessage(
          "gms-populateMiroPivotFilters",
          list(
            ns = ns(""),
            filter = lapply(filterIndexList, getFilterDropdowns),
            aggregations = lapply(aggregationIndexList, getFilterDropdowns,
              optionId = "aggregations"
            ),
            cols = lapply(colIndexList, getFilterDropdowns, optionId = "cols")
          )
        )
        if (length(invalidFilters)) {
          showHideEl(session, paste0("#", ns("customError")), 7000L,
            msg = paste0(
              lang$renderers$miroPivot$invalidFilters$title,
              invalidFilters
            )
          )
        }
      })


      currentFilters <- reactiveVal(NULL)
      rendererEnv[[ns("currentFilters")]] <- observe({
        newFilters <- list(
          filter = input$filterIndexList,
          aggregations = input$aggregationIndexList,
          cols = input$colIndexList
        )
        if (sum(vapply(newFilters, length, integer(1L), USE.NAMES = FALSE)) +
          length(input$rowIndexList) != length(setIndices) ||
          any(vapply(newFilters, is.null, logical(1L), USE.NAMES = FALSE))) {
          # UI not initialised
          return()
        }
        if (initFilter && isTRUE(options$resetOnInit)) {
          newFilters <- list(
            filter = unname(indices[["filter"]]),
            aggregations = unname(indices[["aggregations"]]),
            cols = unname(indices[["cols"]])
          )
        }
        if (length(options$domainFilter$domains)) {
          newFilters$domainFilter <- input$domainFilter
          if (initFilter && length(currentView$domainFilter[["default"]]) &&
            !identical(input$domainFilter, currentView$domainFilter[["default"]])) {
            return()
          }
        }

        isolate({
          currentFilters(newFilters)
        })
      })
      throttledFilters <- vector("list", length(setIndices))
      lapply(setIndices, function(filterIndex) {
        throttledFilters[[filterIndex]] <<- throttle(reactive({
          input[[paste0("filter_", filterIndex)]]
        }), if (bigData) 2000 else 500)
        rendererEnv[[ns(paste0("filter_", filterIndex))]] <- observe({
          isInitialized <- TRUE
          if (!filterIndex %in% rendererEnv[[ns("filtersInitialized")]] &&
            (!initData || length(throttledFilters[[filterIndex]]()))) {
            isInitialized <- FALSE
            if (length(rendererEnv[[ns("filtersInitialized")]])) {
              rendererEnv[[ns("filtersInitialized")]] <- c(
                filterIndex,
                rendererEnv[[ns("filtersInitialized")]]
              )
            } else {
              rendererEnv[[ns("filtersInitialized")]] <- filterIndex
            }
          }
          if (is.null(throttledFilters[[filterIndex]]())) {
            if (!isInitialized ||
              !filterIndex %in% isolate(c(
                input$aggregationIndexList,
                input$colIndexList
              ))) {
              return()
            }
          }
          if (!isFALSE(noUpdateFilterEl[[filterIndex]])) {
            noUpdateFilterEl[[filterIndex]] <<- FALSE
            return()
          }
          isolate({
            newVal <- updateFilter() + 1L
            updateFilter(newVal)
          })
        })
      })

      filteredData <- reactive({
        updateFilter()
        if (is.null(currentFilters())) {
          return()
        }
        filterIndexList <- currentFilters()$filter
        aggFilterIndexList <- currentFilters()$aggregations
        colFilterIndexList <- currentFilters()$cols
        domainFilter <- currentFilters()$domainFilter
        filterIndexList <- c(filterIndexList, aggFilterIndexList, colFilterIndexList)
        if (length(domainFilter)) {
          if (identical("__key__", names(data)[1])) {
            dataTmp <- data[-1] %>% filter(.data[[domainFilter]] != if (length(options$domainFilter$filterVal)) {
              options$domainFilter$filterVal
            } else {
              "\U00A0"
            })
          } else {
            dataTmp <- data %>% filter(.data[[domainFilter]] != if (length(options$domainFilter$filterVal)) {
              options$domainFilter$filterVal
            } else {
              "\U00A0"
            })
          }
          if (!length(filterIndexList)) {
            return(list(data = dataTmp, filterElements = list()))
          }
        } else {
          if (!length(filterIndexList)) {
            if (identical("__key__", names(data)[1])) {
              return(list(data = data[-1], filterElements = list()))
            }
            return(list(data = data, filterElements = list()))
          }
          if (identical("__key__", names(data)[1])) {
            dataTmp <- data[-1]
          } else {
            dataTmp <- data
          }
        }

        filterElements <- vector("list", length(filterIndexList))
        names(filterElements) <- filterIndexList
        multiFilterIndices <- c()

        for (filterIndex in filterIndexList) {
          filterElements[[filterIndex]] <- sort(unique(dataTmp[[filterIndex]]))
          optionId <- "filter"
          if (filterIndex %in% aggFilterIndexList) {
            optionId <- "aggregations"
          } else if (filterIndex %in% colFilterIndexList) {
            optionId <- "cols"
          }
          if (initFilter && resetFilters &&
            (optionId %in% c("aggregations", "cols") ||
              length(currentView[[optionId]][[filterIndex]]))) {
            filterVal <- currentView[[optionId]][[filterIndex]]
          } else {
            filterVal <- isolate(input[[paste0("filter_", filterIndex)]])
          }
          if (!any(filterVal %in% filterElements[[filterIndex]])) {
            if (filterIndex %in% c(aggFilterIndexList, colFilterIndexList)) {
              # nothing selected = no filter for aggregations/cols
              next
            }
            filterVal <- filterElements[[filterIndex]][1]
          }
          if (any(is.na(match(filterIndex, names(dataTmp))))) {
            flog.warn(
              "Attempt to tamper with the app detected! User entered: '%s' as filter index",
              filterIndex
            )
            stop("Attempt to tamper with the app detected!", call. = FALSE)
          }
          if (length(filterVal) > 1L) {
            multiFilterIndices <- c(multiFilterIndices, filterIndex)
            filterExpression <- paste0(
              ".[[\"", filterIndex, "\"]]%in%c('",
              paste(gsub("'", "\\'", filterVal, fixed = TRUE), collapse = "','"),
              "')"
            )
          } else {
            filterExpression <- paste0(
              ".[[\"", filterIndex, "\"]]=='",
              gsub("'", "\\'", filterVal, fixed = TRUE),
              "'"
            )
          }
          dataTmp <- dataTmp %>% filter(
            !!rlang::parse_expr(filterExpression)
          )
        }
        return(list(
          data = dataTmp, filterElements = filterElements,
          multiFilterIndices = multiFilterIndices
        ))
      })

      dataToRender <- reactive({
        dataTmp <- filteredData()$data
        rowIndexList <- input$rowIndexList
        initData <<- FALSE
        if (!length(dataTmp)) {
          return()
        }
        colIndexList <- isolate(currentFilters()$cols)
        aggIndexList <- isolate(currentFilters()$aggregations)
        multiFilterIndices <- filteredData()$multiFilterIndices
        aggregationFunction <- NULL

        if (is.null(rowIndexList)) {
          rowIndexList <- setIndices
        }
        if (length(rowIndexList) + length(filteredData()$filterElements) != length(setIndices) ||
          any(rowIndexList %in% names(filteredData()$filterElements))) {
          return()
        }
        rowIndexList <- c(
          rowIndexList,
          multiFilterIndices[!multiFilterIndices %in% c(aggIndexList, colIndexList)]
        )
        if (length(aggIndexList)) {
          if (isInput) {
            isEditable <<- FALSE
            disableEl(session, paste0("#", ns("btAddRow")))
            disableEl(session, paste0("#", ns("btRemoveRows")))
          }
          aggregationFunctionTmp <- input$aggregationFunction
          if (is.null(aggregationFunction)) {
            aggregationFunction <- aggregationFunctionTmp
          }
          if (identical(aggregationFunction, "")) {
            aggregationFunction <- aggregationFunctions[[1]]
          } else if (length(aggregationFunction) != 1L ||
            !aggregationFunction %in% c("sum", "count", "min", "max", "mean", "median", "sd")) {
            flog.warn(
              "Attempt to tamper with the app detected! User entered: '%s' as aggregation function.",
              aggregationFunction
            )
            stop("Attempt to tamper with the app detected!", call. = FALSE)
          }
          if (!identical(valueColName, "value")) {
            names(dataTmp)[length(dataTmp)] <- "value"
          }
          dataTmp <- dataTmp %>%
            group_by(!!!rlang::syms(c(rowIndexList, colIndexList))) %>%
            summarise(value = !!rlang::parse_expr(
              if (identical(aggregationFunction, "count")) {
                "sum(!is.na(value))"
              } else {
                paste0(aggregationFunction, "(value, na.rm = TRUE)")
              }
            ), .groups = "drop_last")
          if (!identical(valueColName, "value")) {
            names(dataTmp)[length(dataTmp)] <- valueColName
          }
        } else if (isInput) {
          if (bigData) {
            isEditable <<- identical(names(data)[1], "__key__")
          } else {
            isEditable <<- TRUE
          }
          enableEl(session, paste0("#", ns("btAddRow")))
          enableEl(session, paste0("#", ns("btRemoveRows")))
        }
        if (length(rowIndexList)) {
          dataTmp <- dataTmp %>%
            select(!!!c(rowIndexList, colIndexList, valueColName)) %>%
            arrange(!!!rlang::syms(rowIndexList))
        } else {
          dataTmp <- dataTmp %>% select(!!!c(colIndexList, valueColName))
        }
        if (length(colIndexList)) {
          # note that names_sep is not an ASCII full stop, but UNICODE U+2024
          tryCatch(
            {
              dataTmp <- dataTmp %>%
                pivot_wider(
                  names_from = !!colIndexList, values_from = !!valueColName,
                  names_sep = "\U2024",
                  names_sort = TRUE, names_repair = "unique"
                )
            },
            warning = function(w) {
              if (grepl("list-cols", conditionMessage(w), fixed = TRUE)) {
                flog.trace("MIRO pivot: Data contains duplicated keys and can therefore not be pivoted.")
                showErrorMsg(
                  lang$renderers$miroPivot$errorTitle,
                  lang$renderers$miroPivot$errPivotDuplicate
                )
              } else {
                flog.info(
                  "MIRO pivot: Unexpected warning while pivoting data. Error message: %s",
                  conditionMessage(e)
                )
                showErrorMsg(
                  lang$renderers$miroPivot$errorTitle,
                  lang$renderers$miroPivot$errPivot
                )
              }
            },
            error = function(e) {
              flog.info(
                "MIRO pivot: Unexpected error while pivoting data. Error message: %s",
                conditionMessage(e)
              )
              showErrorMsg(
                lang$renderers$miroPivot$errorTitle,
                lang$renderers$miroPivot$errPivot
              )
            }
          )
        }
        attr(dataTmp, "noRowHeaders") <- length(rowIndexList)
        return(dataTmp)
      })

      # ===================================================
      #        CHART RENDERER
      # ===================================================

      output$pivotChart <- renderChartjs({
        updateRenderer()
        pivotRenderer <- input$pivotRenderer
        if (!is.null(rendererEnv[[ns("chartOptions")]])) {
          # reset chart options
          rendererEnv[[ns("chartOptions")]] <- NULL
        }
        if (initRenderer && isTRUE(options$resetOnInit)) {
          if (length(currentView[["pivotRenderer"]])) {
            initRenderer <<- FALSE
            if (!identical(pivotRenderer, currentView[["pivotRenderer"]])) {
              return()
            }
          } else {
            return()
          }
        }
        if (!pivotRenderer %in% c("line", "scatter", "area", "stackedarea", "bar", "stackedbar", "radar")) {
          return()
        }
        showEl(session, paste0("#", ns("loadPivotTable")))
        dataTmp <- dataToRender()
        if (!length(dataTmp)) {
          if (miroPivotState$triggerEditViewDialog) {
            miroPivotState$triggerEditViewDialog <<- FALSE
            showAddViewDialog(pivotRenderer, viewOptions = currentView)
          }
          return()
        }
        rowHeaderLen <- attr(dataTmp, "noRowHeaders")
        noSeries <- length(dataTmp) - rowHeaderLen
        noError <- TRUE
        if (noSeries > 40L) {
          showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(lang$renderers$miroPivot$colTruncationWarning, "40"))
          noError <- FALSE
        }
        if (nrow(dataTmp) > 500L) {
          showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(lang$renderers$miroPivot$rowTruncationWarning, "500"))
          dataTmp <- slice(dataTmp, 1:500L)
          noError <- FALSE
        }
        setCssEl(
          session, paste0("#", ns("pivotChart")),
          list(position = "", top = "")
        )
        if (noError) {
          hideEl(session, paste0("#", ns("errMsg")))
        }
        if (isTRUE(options$enableHideEmptyCols) && isTRUE(input$hideEmptyCols)) {
          hiddenEmptyColsTmp <- vapply(dataTmp[seq_len(rowHeaderLen)],
            function(x) {
              identical(
                as.character(unique(x)),
                if (length(options$emptyUEL)) {
                  options$emptyUEL
                } else {
                  "-"
                }
              )
            },
            logical(1L),
            USE.NAMES = FALSE
          )
          hiddenEmptyCols <<- which(hiddenEmptyColsTmp)
          hiddenEmptyColsJs <- rep.int("false", rowHeaderLen)
          hiddenEmptyColsJs[hiddenEmptyColsTmp] <- "true"
          setAttributes(
            session, paste0(
              "#", ns("rowIndexList .drop-index-item:nth-child("),
              seq_len(rowHeaderLen), ")"
            ),
            "data-col-hidden", hiddenEmptyColsJs
          )
          labels <- do.call(paste, c(
            dataTmp[which(!hiddenEmptyColsTmp)],
            list(sep = ".")
          ))
        } else {
          hiddenEmptyCols <<- NULL
          if (isTRUE(options$enableHideEmptyCols)) {
            setAttributes(
              session, paste0("#", ns("rowIndexList .drop-index-item")),
              "data-col-hidden", "false"
            )
          }
          labels <- do.call(paste, c(dataTmp[seq_len(rowHeaderLen)], list(sep = ".")))
        }
        if (!length(labels)) {
          labels <- "value"
        }
        miroPivotState$currentSeriesLabels <<- names(dataTmp)[seq(rowHeaderLen + 1L, min(
          noSeries + rowHeaderLen,
          40L + rowHeaderLen
        ))]
        if (miroPivotState$triggerEditViewDialog) {
          miroPivotState$triggerEditViewDialog <<- FALSE
          showAddViewDialog(pivotRenderer, viewOptions = currentView)
        }
        if (length(currentView$chartOptions$customChartColors) &&
          length(names(currentView$chartOptions$customChartColors))) {
          # custom chart colors specified
          chartColorIdx <- match(
            miroPivotState$currentSeriesLabels,
            names(currentView$chartOptions$customChartColors)
          )
          chartColorsToUse <- currentView$chartOptions$customChartColors[chartColorIdx]
          chartColorsToUse[is.na(chartColorIdx)] <- list(c(SERIES_DEFAULT_COLOR, SERIES_DEFAULT_COLOR))
          chartColorsToUse <- unlist(chartColorsToUse, use.names = FALSE)
        } else {
          chartColorsToUse <- customChartColors
        }
        if (pivotRenderer %in% c("line", "scatter", "area", "stackedarea", "area", "stackedarea")) {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsLine(
              labels = labels,
              xTitle = currentView$chartOptions$xTitle,
              yTitle = currentView$chartOptions$yTitle
            )
          if (identical(pivotRenderer, "scatter")) {
            chartJsObj$x$options$showLine <- FALSE
          } else if (identical(pivotRenderer, "stackedarea")) {
            chartJsObj$x$scales$y$stacked <- TRUE
          }
        } else if (identical(pivotRenderer, "stackedbar")) {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsBar(
              labels = labels, stacked = TRUE,
              xTitle = currentView$chartOptions$xTitle,
              yTitle = currentView$chartOptions$yTitle
            )
        } else if (identical(pivotRenderer, "radar")) {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsRadar(labels = labels)
        } else {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsBar(
              labels = labels,
              xTitle = currentView$chartOptions$xTitle,
              yTitle = currentView$chartOptions$yTitle
            )
        }
        if (length(currentView$chartOptions)) {
          # reset chart options
          rendererEnv[[ns("chartOptions")]] <- currentView$chartOptions
          currentView$chartOptions <<- NULL
        }
        for (i in seq_len(min(noSeries, 40L))) {
          chartJsObj <- cjsSeries(chartJsObj, dataTmp[[rowHeaderLen + i]],
            label = names(dataTmp)[rowHeaderLen + i],
            fill = pivotRenderer %in% c("area", "stackedarea"),
            fillOpacity = if (identical(pivotRenderer, "stackedarea")) 1 else 0.15
          )
        }
        hideEl(session, paste0("#", ns("loadPivotTable")))

        return(chartJsObj %>% cjsLegend())
      })

      # ===================================================
      #        TABLE RENDERER
      # ===================================================

      output$pivotTable <- renderDT({
        pivotRenderer <- input$pivotRenderer
        if (initRenderer && isTRUE(options$resetOnInit)) {
          if (length(currentView[["pivotRenderer"]])) {
            pivotRenderer <- currentView[["pivotRenderer"]]
          } else {
            pivotRenderer <- "table"
          }
          initRenderer <<- FALSE
        }
        isEditableTable <- isEditable
        isHeatmap <- FALSE
        if (!identical(pivotRenderer, "table")) {
          if (!identical(pivotRenderer, "heatmap")) {
            showEl(session, paste0("#", ns("downloadPng")))
            return()
          }
          isHeatmap <- TRUE
          isEditableTable <- FALSE
        }
        if (miroPivotState$triggerEditViewDialog) {
          miroPivotState$triggerEditViewDialog <<- FALSE
          showAddViewDialog(pivotRenderer, viewOptions = currentView)
        }
        hideEl(session, paste0("#", ns("downloadPng")))

        setCssEl(
          session, paste0("#", ns("pivotChart")),
          list(position = "absolute", top = "-2000pt")
        )
        showEl(session, paste0("#", ns("loadPivotTable")))
        dataTmp <- dataToRender()
        if (!length(dataTmp)) {
          return()
        }
        noRowHeaders <- attr(dataTmp, "noRowHeaders")
        if (isHeatmap) {
          brks <- quantile(dataTmp[-seq_len(noRowHeaders)],
            probs = seq(.05, .95, .05), na.rm = TRUE
          )
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {
            paste0("rgb(255,", ., ",", ., ")")
          }
        }
        if (length(dataTmp) > 300) {
          showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(lang$renderers$miroPivot$colTruncationWarning, "300"))
          dataTmp <- dataTmp[, 1:300]
        } else {
          hideEl(session, paste0("#", ns("errMsg")))
        }
        hideEl(session, paste0("#", ns("loadPivotTable")))

        if (isTRUE(options$enableHideEmptyCols) && isTRUE(input$hideEmptyCols)) {
          hiddenEmptyCols <<- which(vapply(dataTmp[seq_len(noRowHeaders)],
            function(x) {
              identical(
                as.character(unique(x)),
                if (length(options$emptyUEL)) {
                  options$emptyUEL
                } else {
                  "-"
                }
              )
            },
            logical(1L),
            USE.NAMES = FALSE
          ))
          hiddenEmptyColsJs <- rep.int("false", noRowHeaders)
          hiddenEmptyColsJs[hiddenEmptyCols] <- "true"
          setAttributes(
            session, paste0(
              "#", ns("rowIndexList .drop-index-item:nth-child("),
              seq_len(noRowHeaders), ")"
            ),
            "data-col-hidden", hiddenEmptyColsJs
          )
          columnDefsTmp <- list(list(
            visible = FALSE,
            targets = hiddenEmptyCols - 1L
          ))
        } else {
          hiddenEmptyCols <<- NULL
          if (isTRUE(options$enableHideEmptyCols)) {
            setAttributes(
              session, paste0("#", ns("rowIndexList .drop-index-item")),
              "data-col-hidden", "false"
            )
          }
          columnDefsTmp <- NULL
        }

        ret <- datatable(dataTmp,
          extensions = c("Scroller", "FixedColumns"),
          selection = if (isEditableTable) "multiple" else "none", editable = isEditableTable,
          callback = JS("setTimeout(function() { table.draw(true); }, 500);"),
          container = DTbuildColHeaderContainer(
            names(dataTmp),
            noRowHeaders,
            unlist(setIndexAliases[names(dataTmp)[seq_len(noRowHeaders)]],
              use.names = FALSE
            )
          ),
          options = list(
            fnDrawCallback = if (noRowHeaders > 1) JS(paste0("function ( settings ) {
    var api = this.api();
    var cols = api.columns([", paste(seq_len(noRowHeaders - 1L) - 1L, collapse = ","), "], {page:'current'} ).nodes();
    api.columns([", paste(seq_len(noRowHeaders - 1L) - 1L, collapse = ","), "],
      {page:'current'} ).data().each( function ( colData, i ) {
       var last = null;
       $.each(colData, function ( j, group ) {
          if ( last !== group || (i > 0 && $(cols[i - 1]).eq( j ).text() !== '')) {
              last = group;
              $(cols[i]).eq( j ).css({
                position: '-webkit-sticky',
                position: 'sticky',
                top: 0
              });
              $(cols[i]).eq( j ).addClass('tableScrollCell');
          } else {
            $(cols[i]).eq( j ).css(
                    'border-top', 'none'
                ).text(
                  ''
              );
          }
      } );
    });
}")),
            scrollY = 400, scrollX = TRUE, scrollCollapse = TRUE,
            scroller = list(loadingIndicator = FALSE), dom = "Bfrtip",
            fixedColumns = list(leftColumns = noRowHeaders),
            columnDefs = columnDefsTmp
          ), rownames = FALSE
        ) %>%
          formatRound(seq(noRowHeaders + 1, length(dataTmp)),
            digits = roundPrecision
          )
        if (!isHeatmap) {
          return(ret)
        }
        return(formatStyle(ret, seq(noRowHeaders + 1, length(dataTmp)),
          color = "#000",
          backgroundColor = styleInterval(brks, clrs)
        ))
      })

      if (isInput) {
        dtProxy <- dataTableProxy(ns("pivotTable"))

        rendererEnv[[ns("editTableAdd")]] <- observe({
          if (length(input[["btAddRow"]]) != 1L ||
            input[["btAddRow"]] == 0L || !isEditable ||
            identical(rendererEnv[[ns("editTableAdd")]]$.execCount, 1L)) {
            return()
          }
          isolate({
            if (!length(dataToRender())) {
              flog.warn("MIRO Pivot: Add row button was clicked but dataToRender has no length.")
              return()
            }
            if (identical(nrow(data), 0L)) {
              # pretend we are in list view when no data exists
              colHeaders <- names(data)[-1]
              noRowHeaders <- length(data) - 2L
            } else {
              colHeaders <- names(dataToRender())
              noRowHeaders <- attr(dataToRender(), "noRowHeaders")
            }
            removeUI("body>.selectize-dropdown", multiple = TRUE, immediate = TRUE)
            colHeaders[seq_len(noRowHeaders)] <- unlist(setIndexAliases[colHeaders[seq_len(noRowHeaders)]],
              use.names = FALSE
            )
            showModal(
              modalDialog(
                title = lang$renderers$miroPivot$dialogAddRow$title,
                tags$div(id = ns("newRowError"), class = "gmsalert gmsalert-error"),
                tags$div(
                  class = "table-responsive", style = "margin-top:30px",
                  tags$table(
                    class = "table",
                    tags$tr(
                      lapply(colHeaders, tags$th)
                    ),
                    tags$tr(lapply(seq_along(colHeaders), function(i) {
                      tags$td(
                        style = "min-width:100px;",
                        if (i <= noRowHeaders) {
                          serverSelectInput(session, ns(paste0("newRow_", i)), NULL,
                            if (!identical(nrow(data), 0L)) levels(dataToRender()[[i]]),
                            multiple = TRUE,
                            width = "100%",
                            options = list(
                              create = TRUE, maxItems = 1L,
                              dropdownParent = "body"
                            )
                          )
                        } else {
                          textInput(ns(paste0("newRow_", i)), NULL, NA, width = "100%")
                        }
                      )
                    }))
                  )
                ),
                footer = tagList(
                  modalButton(lang$renderers$miroPivot$dialogAddRow$btCancel),
                  actionButton(ns("btAddRowConfirm"),
                    label = lang$renderers$miroPivot$btAddRow,
                    class = "bt-highlight-1 bt-gms-confirm"
                  )
                ),
                fade = TRUE, easyClose = FALSE, size = "l"
              )
            )
          })
        })
        rendererEnv[[ns("editTableAddConfirm")]] <- observe({
          if (length(input[["btAddRowConfirm"]]) != 1L ||
            input[["btAddRowConfirm"]] == 0L || !isEditable ||
            identical(rendererEnv[[ns("editTableAddConfirm")]]$.execCount, 1L)) {
            return()
          }
          isolate({
            if (!length(dataToRender())) {
              flog.warn("MIRO Pivot: Add row button was clicked but dataToRender has no length.")
              return()
            }
            if (bigData) {
              showEl(session, "#loading-screen")
              on.exit(hideEl(session, "#loading-screen"))
            }
            flog.trace("MIRO pivot: Received request to add new row(s).")
            if (identical(nrow(data), 0L)) {
              # pretend we are in list view when no data exists
              colHeaders <- names(data)[-1]
              noRowHeaders <- length(data) - 2L

              rowIndices <- colHeaders[seq_len(noRowHeaders)]
              colIndices <- character(0L)
              filterIndices <- character(0L)
            } else {
              colHeaders <- names(dataToRender())
              noRowHeaders <- attr(dataToRender(), "noRowHeaders")

              valueColName <- names(data)[length(data)]

              rowIndices <- colHeaders[seq_len(noRowHeaders)]
              colIndices <- currentFilters()$cols
              colIndices <- colIndices[colIndices != valueColName]
              filterIndices <- currentFilters()$filter
              filterIndices <- filterIndices[!filterIndices %in% c(filteredData()$multiFilterIndices, valueColName)]
            }
            newKeys <- vapply(seq_len(noRowHeaders), function(i) {
              editedKey <- tryCatch(trimws(input[[paste0("newRow_", i)]]),
                error = function(e) {
                  NA_character_
                }
              )
              if (isValidUEL(editedKey)) {
                return(editedKey)
              }
              return(NA_character_)
            }, character(1L), USE.NAMES = FALSE)
            if (any(is.na(newKeys))) {
              return(showHideEl(
                session, paste0("#", ns("newRowError")), 5000L,
                lang$renderers$miroPivot$dialogAddRow$invalidKeysError
              ))
            }

            newValues <- vapply(seq(noRowHeaders + 1L, length(colHeaders)), function(i) {
              newVal <- tryCatch(suppressWarnings(as.numeric(input[[paste0("newRow_", i)]])),
                error = function(e) {
                  NA_real_
                }
              )
              if (length(newVal) != 1L) {
                return(NA_real_)
              }
              return(newVal)
            }, numeric(1L), USE.NAMES = FALSE)
            newColIds <- which(!is.na(newValues))
            if (!length(newColIds)) {
              return(showHideEl(
                session, paste0("#", ns("newRowError")), 5000L,
                lang$renderers$miroPivot$dialogAddRow$invalidValuesError
              ))
            }
            newValues <- newValues[newColIds]
            # in case new values are non-integer and value column was integer before,
            # we have to convert it
            mustConvertValCol <- FALSE
            if (is.integer(data[[length(data)]])) {
              if (all(is_wholenumber(newValues))) {
                newValues <- as.integer(newValues)
              } else {
                mustConvertValCol <- TRUE
              }
            }

            colElements <- vector("list", length(newColIds))
            if (length(colIndices)) {
              colElements <- strsplit(colHeaders[noRowHeaders + newColIds], "\U2024", fixed = TRUE)
            } else if (length(newColIds) > 1L) {
              flog.error("MIRO pivot: Column dimensions of current filters and dataToRender don't match. Please contact GAMS!")
              return(showHideEl(
                session, paste0("#", ns("newRowError")), 5000L,
                lang$renderers$miroPivot$dialogAddRow$unknownError
              ))
            }
            indexOrder <- match(setIndices, c(
              rowIndices,
              colIndices,
              filterIndices
            ))
            if (any(is.na(indexOrder)) || length(indexOrder) != length(setIndices)) {
              flog.error("MIRO pivot: Could not determine index order.")
              return(showHideEl(
                session, paste0("#", ns("newRowError")), 5000L,
                lang$renderers$miroPivot$dialogAddRow$unknownError
              ))
            }
            if (tryCatch(
              {
                filterElements <- vapply(filterIndices, function(filterIndex) {
                  return(input[[paste0("filter_", filterIndex)]])
                }, character(1L), USE.NAMES = FALSE)
                FALSE
              },
              error = function(e) {
                flog.warn(
                  "Could not determine filter elements. Error message: %s",
                  conditionMessage(e)
                )
                return(TRUE)
              }
            )) {
              return(showHideEl(
                session, paste0("#", ns("newRowError")), 5000L,
                lang$renderers$miroPivot$dialogAddRow$unknownError
              ))
            }
            newKeysColIds <- match(rowIndices, setIndices) + 1L
            # we might have to extend factor if there are new keys
            hasNewUELs <- FALSE
            for (i in seq_along(newKeys)) {
              if (!newKeys[i] %in% levels(data[[newKeysColIds[i]]])) {
                hasNewUELs <- TRUE
                data[[newKeysColIds[i]]] <<- factor(data[[newKeysColIds[i]]],
                  levels = unique(sort(c(
                    levels(data[[newKeysColIds[i]]]),
                    newKeys[i]
                  )))
                )
              }
            }
            # now that factors are extended, we can safely add new rows
            newRow <- vector("list", length(newValues))
            newKey <- vector("list", length(newValues))
            for (i in seq_along(newValues)) {
              newRowTmp <- c(newKeys, colElements[[i]], filterElements)[indexOrder]
              newKey[[i]] <- paste(newRowTmp, collapse = "\U2024")
              newRow[[i]] <- c(
                list(newKey[[i]]), as.list(newRowTmp),
                list(newValues[i])
              )
              if (!hasNewUELs && newKey[[i]] %in% data[["__key__"]]) {
                flog.debug("Key: '%s' already exists.", newKey[[i]])
                return(showHideEl(
                  session, paste0("#", ns("newRowError")), 5000L,
                  sprintf(lang$renderers$miroPivot$duplicateRecordError, newKey[[i]])
                ))
              }
            }
            if (mustConvertValCol) {
              data[[length(data)]] <<- as.numeric(data[[length(data)]])
            }
            for (i in seq_along(newValues)) {
              flog.debug("MIRO pivot: adding new data record: %s", newKey[[i]])
              data[nrow(data) + 1L, ] <<- newRow[[i]]
            }
            removeModal()
            newVal <- dataUpdated() + 1L
            dataUpdated(newVal)
            newUpdateFilterVal <- updateFilter() + 1L
            updateFilter(newUpdateFilterVal)
          })
        })

        rendererEnv[[ns("editTableRemove")]] <- observe({
          if (length(input[["btRemoveRows"]]) != 1L ||
            input[["btRemoveRows"]] == 0L || !isEditable ||
            identical(rendererEnv[[ns("editTableRemove")]]$.execCount, 1L)) {
            return()
          }
          isolate({
            idsToRemove <- input[["pivotTable_rows_selected"]]
            if (!length(idsToRemove)) {
              return()
            }
            if (!length(dataToRender())) {
              flog.warn("MIRO Pivot: Remove rows button was clicked but dataToRender has no length.")
              return()
            }
            if (bigData) {
              showEl(session, "#loading-screen")
              on.exit(hideEl(session, "#loading-screen"))
            }
            flog.trace("MIRO pivot: Received request to remove row(s).")
            colHeaders <- names(dataToRender())
            noRowHeaders <- attr(dataToRender(), "noRowHeaders")
            rowIndices <- colHeaders[seq_len(noRowHeaders)]
            colIndices <- currentFilters()$cols
            colIndices <- colIndices[colIndices != valueColName]
            filterIndices <- currentFilters()$filter
            filterIndices <- filterIndices[!filterIndices %in% c(filteredData()$multiFilterIndices, valueColName)]

            indexOrder <- match(setIndices, c(
              rowIndices,
              colIndices,
              filterIndices
            ))
            rowsToRemove <- mutate_if(
              slice(dataToRender(), idsToRemove)[seq_len(noRowHeaders)],
              is.factor, as.character
            )
            if (tryCatch(
              {
                filterElements <- vapply(filterIndices, function(filterIndex) {
                  return(input[[paste0("filter_", filterIndex)]])
                }, character(1L), USE.NAMES = FALSE)
                FALSE
              },
              error = function(e) {
                flog.error(
                  "Could not determine filter elements. Error message: %s",
                  conditionMessage(e)
                )
                return(TRUE)
              }
            )) {
              return()
            }
            if (length(colIndices)) {
              if (noRowHeaders > 0L) {
                colHeadersTmp <- colHeaders[-seq_len(noRowHeaders)]
              } else {
                colHeadersTmp <- colHeaders
              }
              colElementsList <- strsplit(colHeadersTmp, "\U2024", fixed = TRUE)
            } else {
              colElementsList <- list(character())
            }
            keysToRemove <- unlist(lapply(seq_along(idsToRemove), function(i) {
              vapply(colElementsList, function(colElements) {
                paste(c(
                  as.character(rowsToRemove[i, ]),
                  colElements,
                  filterElements
                )[indexOrder], collapse = "\U2024")
              }, character(1L), USE.NAMES = FALSE)
            }))
            rowIdsToRemove <- match(keysToRemove, data[["__key__"]])
            rowIdsToRemove <- rowIdsToRemove[!is.na(rowIdsToRemove)]
            if (length(rowIdsToRemove) > 0L) {
              data <<- data[-rowIdsToRemove, ]
              flog.trace("MIRO pivot: %s row(s) removed.", length(rowIdsToRemove))

              newVal <- dataUpdated() + 1L
              dataUpdated(newVal)
              newUpdateFilterVal <- updateFilter() + 1L
              updateFilter(newUpdateFilterVal)
            }
          })
        })

        rendererEnv[[ns("editTable")]] <- observe({
          info <- input[["pivotTable_cell_edit"]]
          if (!isEditable || is.null(info) ||
            identical(rendererEnv[[ns("editTable")]]$.execCount, 1L)) {
            return()
          }
          flog.trace("MIRO pivot: Received request to edit table data.")
          if (bigData) {
            if (!identical("__key__", names(data)[1])) {
              flog.error("User edited input table even though it was readonly. This seems like an attempt to tamper with the app!")
              return()
            }
            showEl(session, "#loading-screen")
            on.exit(hideEl(session, "#loading-screen"))
          }
          isolate({
            editedCol <- suppressWarnings(as.integer(info$col)) + 1L
            newUpdateFilterVal <- updateFilter() + 1L

            if (is.na(editedCol) || length(editedCol) != 1L) {
              flog.error("MIRO pivot: Could not determine edited column.")
              updateFilter(newUpdateFilterVal)
              return()
            }
            if (tryCatch(
              {
                valueColName <- names(data)[length(data)]
                dataHeaders <- names(dataToRender())
                noRowHeaders <- attr(dataToRender(), "noRowHeaders")

                rowIndices <- dataHeaders[seq_len(noRowHeaders)]
                colIndices <- currentFilters()$cols
                colIndices <- colIndices[colIndices != valueColName]
                filterIndices <- currentFilters()$filter
                filterIndices <- filterIndices[!filterIndices %in% c(filteredData()$multiFilterIndices, valueColName)]

                keyToReplace <- NULL
                indexOrder <- match(setIndices, c(
                  rowIndices,
                  colIndices,
                  filterIndices
                ))

                rowToReplace <- slice(dataToRender(), info$row[1])
                rowElements <- vapply(rowIndices, function(rowIndex) {
                  as.character(rowToReplace[[rowIndex]][1])
                }, character(1L), USE.NAMES = FALSE)
                filterElements <- vapply(filterIndices, function(filterIndex) {
                  return(input[[paste0("filter_", filterIndex)]])
                }, character(1L), USE.NAMES = FALSE)

                if (length(colIndices)) {
                  if (noRowHeaders > 0L) {
                    colHeaders <- dataHeaders[-seq_len(noRowHeaders)]
                  } else {
                    colHeaders <- dataHeaders
                  }
                  if (editedCol > noRowHeaders) {
                    colElements <- strsplit(dataHeaders[editedCol], "\U2024", fixed = TRUE)[[1]]
                  } else if (length(colHeaders) == 1L) {
                    colElements <- strsplit(colHeaders, "\U2024", fixed = TRUE)[[1]]
                  } else {
                    # key was edited while columns are pivoted -> need to (potentially) change multiple keys
                    colElementsList <- strsplit(colHeaders, "\U2024", fixed = TRUE)
                    keyToReplace <- vapply(colElementsList, function(colElements) {
                      paste(c(
                        rowElements,
                        colElements,
                        filterElements
                      )[indexOrder],
                      collapse = "\U2024"
                      )
                    }, character(1L), USE.NAMES = FALSE)
                    rowId <- match(keyToReplace, data[["__key__"]])
                  }
                } else {
                  colElements <- character()
                }

                if (is.null(keyToReplace)) {
                  keyVector <- c(
                    rowElements,
                    colElements,
                    filterElements
                  )
                  if (length(keyVector) < length(setIndices)) {
                    stop("MIRO pivot: Could not determine row ID!", call. = FALSE)
                  }
                  keyVector <- keyVector[indexOrder]
                  keyToReplace <- paste(keyVector,
                    collapse = "\U2024"
                  )
                  rowId <- which(keyToReplace == data[["__key__"]])
                }
                if (!length(rowId)) {
                  if (editedCol <= noRowHeaders) {
                    stop(sprintf(
                      "MIRO pivot: Could not find row with ID: '%s' in original data.",
                      paste(keyToReplace, collapse = ", ")
                    ), call. = FALSE)
                  }
                  # value was added in a pivoted column that was empty/NA before
                }
                FALSE
              },
              error = function(e) {
                flog.error(
                  "MIRO pivot: Problems getting row to replace. Error message: %s",
                  conditionMessage(e)
                )
                TRUE
              }
            )) {
              return()
            }
            if (editedCol > noRowHeaders) {
              # edited column is value column
              editedVal <- suppressWarnings(as.numeric(info$value))
              newData <- dataToRender()
              if (is.na(editedVal) || length(editedVal) != 1L) {
                if (identical(info$value, "")) {
                  # delete row
                  if (!length(rowId)) {
                    flog.debug("MIRO pivot: Edited value is not numeric!.")
                    updateFilter(newUpdateFilterVal)
                    return()
                  }
                  data <<- data[-c(rowId), ]
                  editedVal <- NA_real_
                  if (length(colIndices) == 0L) {
                    newVal <- dataUpdated() + 1L
                    dataUpdated(newVal)
                    updateFilter(newUpdateFilterVal)
                    return()
                  }
                } else {
                  flog.debug("MIRO pivot: Edited value is not numeric!.")
                  updateFilter(newUpdateFilterVal)
                  return()
                }
              } else {
                if (length(rowId)) {
                  data[rowId, length(data)] <<- editedVal
                } else {
                  # need to add new row

                  # in case new values are non-integer and value column was integer before,
                  # we have to convert it
                  mustConvertValCol <- FALSE
                  if (is.integer(data[[length(data)]])) {
                    if (all(is_wholenumber(editedVal))) {
                      editedVal <- as.integer(editedVal)
                    } else {
                      data[[length(data)]] <<- as.numeric(data[[length(data)]])
                      newData[[editedCol]] <- as.numeric(newData[[editedCol]])
                    }
                  }
                  data[nrow(data) + 1L, ] <<- c(
                    list(keyToReplace),
                    as.list(keyVector),
                    list(editedVal)
                  )
                }
              }
              newData[info$row, editedCol] <- editedVal
              replaceData(dtProxy, newData, resetPaging = FALSE)

              newVal <- dataUpdated() + 1L
              dataUpdated(newVal)
              return()
            }
            # edited column is key column
            editedKey <- tryCatch(trimws(info$value), error = function(e) {
              ""
            })
            if (!isValidUEL(editedKey)) {
              flog.info("MIRO pivot: Edited key is invalid!.")
              updateFilter(newUpdateFilterVal)
              return()
            }
            colId <- match(dataHeaders[editedCol], setIndices)
            if (length(rowId) == 1L) {
              keyVector[colId] <- editedKey
              newKey <- paste(keyVector,
                collapse = "\U2024"
              )
            } else {
              validRowIds <- !is.na(rowId)
              rowId <- rowId[validRowIds]
              colElementsList <- colElementsList[validRowIds]
              rowElements[editedCol] <- editedKey
              newKey <- vapply(colElementsList, function(colElements) {
                paste(c(
                  rowElements,
                  colElements,
                  filterElements
                )[indexOrder],
                collapse = "\U2024"
                )
              }, character(1L), USE.NAMES = FALSE)
            }
            if (editedKey %in% levels(data[[colId + 1L]])) {
              if (any(newKey %in% data[["__key__"]])) {
                flog.debug("The record: %s already exists.", newKey)
                updateFilter(newUpdateFilterVal)
                return(showErrorMsg(
                  lang$renderers$miroPivot$duplicateRecordTitle,
                  lang$renderers$miroPivot$duplicateRecordDesc
                ))
              }
              data[rowId, colId + 1L] <<- editedKey
            } else {
              levels(data[[colId + 1L]]) <<- c(levels(data[[colId + 1L]]), editedKey)
              data[rowId, colId + 1L] <<- editedKey
              # reorder factor column
              data[[colId + 1L]] <<- factor(data[[colId + 1L]],
                levels = unique(sort(levels(data[[colId + 1L]])))
              )
            }
            data[rowId, 1L] <<- newKey
            newVal <- dataUpdated() + 1L
            dataUpdated(newVal)
            updateFilter(newUpdateFilterVal)
          })
        })
        return(reactive({
          dataUpdated()
          if (sum(numericCols) == 0L) {
            # data is a set -> add empty text column back
            if (identical(names(data)[1], "__key__")) {
              dataRet <- data[-1]
            } else {
              dataRet <- data
            }
            dataRet[length(dataRet)] <- NA_character_

            return(dataRet)
          } else if (identical(names(data)[1], "__key__")) {
            return(data[-1])
          }
          return(data)
        }))
      }
    }
  )
}
