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
        role = "button", icon("xmark"), class = "miro-pivot-view-button",
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
    tags$a(
      id = ns("downloadCsv"),
      class = "shiny-download-link",
      style = "visibility:hidden;",
      href = "",
      target = "_blank",
      download = NA
    ),
    if (length(options$domainFilter$domains)) {
      domainFilterTabs <- lapply(options$domainFilter$domains, function(domain) {
        domainId <- match(domain, unassignedSetIndices)
        return(tabPanel(
          title = names(unassignedSetIndices)[domainId],
          value = domain
        ))
      })
      if (identical(options$domainFilter$showAll, TRUE)) {
        domainFilterTabs <- c(
          domainFilterTabs,
          list(tabPanel(
            title = lang$renderers$miroPivot$domainFilterShowAllTitle,
            value = "_none"
          ))
        )
      }
      fluidRow(
        class = "domain-filter tabs-ul-mobile-helper",
        style = "margin:0",
        do.call(
          tabsetPanel,
          c(
            id = ns("domainFilter"),
            selected = options$domainFilter$default,
            domainFilterTabs
          )
        ),
        tags$div(class = "small-space")
      )
    },
    fluidRow(
      class = "row-agg-filter",
      tags$div(
        class = "col-sm-12 col-md-2 filter-index-wrapper",
        style = "padding: 1em;",
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
              class = "presentation-hide custom-order", style = if (isTRUE(options$hidePivotControls)) "display:none;",
              actionButton(ns("saveView"),
                label = tags$div(
                  icon("square-plus"),
                  tags$div(
                    class = "miro-pivot-btn-text",
                    lang$renderers$miroPivot$btNewView
                  )
                ),
                title = lang$renderers$miroPivot$btNewView,
                class = "btn-custom pivot-btn-custom"
              ),
              tags$a(
                `data-proxy-id` = ns("downloadCsv"),
                class = "btn btn-default shiny-download-link btn-custom btn-proxy pivot-btn-custom",
                href = "#/", tags$div(
                  tags$i(class = "fa fa-file-csv"),
                  tags$div(
                    class = "miro-pivot-btn-text",
                    lang$renderers$miroPivot$btDownloadCsv
                  )
                ),
                title = lang$renderers$miroPivot$btDownloadCsv
              ),
              tags$a(
                id = ns("downloadPng"),
                class = "btn btn-default bt-export-canvas btn-custom pivot-btn-custom",
                download = "chart.png",
                href = "#",
                `data-canvasid` = ns("pivotChart"),
                tags$div(
                  tags$i(class = "fa fa-file-image"),
                  tags$div(
                    class = "miro-pivot-btn-text",
                    lang$renderers$miroPivot$btDownloadPng
                  )
                ),
                title = lang$renderers$miroPivot$btDownloadPng,
                style = "display:none;"
              ),
              actionButton(ns("showSettings"),
                label = tags$div(
                  icon("gear"),
                  tags$div(
                    class = "miro-pivot-btn-text",
                    lang$renderers$miroPivot$btShowSettings
                  )
                ),
                title = lang$renderers$miroPivot$btShowSettings,
                class = "btn-custom pivot-btn-custom"
              ),
              tags$div(
                id = ns("hidePivotControls"), class = "btn btn-default btn-custom pivot-btn-custom activate-pivot-controls",
                tags$div(
                  icon("table"),
                  tags$div(
                    class = "miro-pivot-btn-text",
                    lang$renderers$miroPivot$btHidePivotControlsShort
                  )
                ),
                title = lang$renderers$miroPivot$btHidePivotControls, `data-id` = ns("")
              )
            ),
            tags$div(
              class = "dropdown presentation",
              style = if (isTRUE(options$hidePivotControls)) "margin-top: 0;",
              tags$button(
                class = "btn btn-default dropdown-toggle btn-dropdown btn-view-dropdown",
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
              `data-proxy-id` = ns("downloadCsv"),
              class = "btn btn-default shiny-download-link btn-custom btn-proxy pivot-btn-custom",
              style = if (isTRUE(options$hidePivotControls)) "display:none;",
              href = "#/", tags$div(
                tags$i(class = "fa fa-file-csv"),
                tags$div(
                  class = "miro-pivot-btn-text",
                  lang$renderers$miroPivot$btDownloadCsv
                )
              ),
              title = lang$renderers$miroPivot$btDownloadCsv
            ),
            tags$a(
              id = ns("downloadPng"),
              class = "btn btn-default bt-export-canvas btn-custom pivot-btn-custom",
              download = "chart.png",
              href = "#",
              `data-canvasid` = ns("pivotChart"),
              tags$div(
                tags$i(class = "fa fa-file-image"),
                tags$div(
                  class = "miro-pivot-btn-text",
                  lang$renderers$miroPivot$btDownloadPng
                )
              ),
              title = lang$renderers$miroPivot$btDownloadPng,
              style = "display:none;"
            ),
            tags$div(
              id = ns("hidePivotControls"), class = "btn btn-default btn-custom pivot-btn-custom activate-pivot-controls",
              style = if (isTRUE(options$hidePivotControls)) "display:none;",
              tags$div(
                icon("table"),
                tags$div(
                  class = "miro-pivot-btn-text",
                  lang$renderers$miroPivot$btHidePivotControlsShort
                )
              ),
              title = lang$renderers$miroPivot$btHidePivotControls, `data-id` = ns("")
            )
          )
        }
      ),
      tags$div(
        class = "col-sm-12 col-md-10 presentation-show",
        style = if (!isTRUE(options$hidePivotControls)) "display:none;",
        tags$a(
          `data-proxy-id` = ns("downloadCsv"),
          class = "btn btn-default shiny-download-link btn-custom btn-proxy pivot-btn-custom",
          href = "#/", tags$div(
            tags$i(class = "fa fa-file-csv"),
            tags$div(
              class = "miro-pivot-btn-text",
              lang$renderers$miroPivot$btDownloadCsvShort
            )
          ),
          title = lang$renderers$miroPivot$btDownloadCsv
        ),
        tags$a(
          class = "btn btn-default bt-export-canvas btn-custom pivot-btn-custom",
          style = "display:none;",
          download = "chart.png",
          href = "#",
          `data-canvasid` = ns("pivotChart"),
          tags$div(
            tags$i(class = "fa fa-file-image"),
            tags$div(
              class = "miro-pivot-btn-text",
              lang$renderers$miroPivot$btDownloadPngShort
            )
          ),
          title = lang$renderers$miroPivot$btDownloadPng
        ),
        tags$div(
          id = ns("showPivotControls"),
          class = "btn btn-default btn-custom pivot-btn-custom deactivate-pivot-controls presentation-show",
          tags$div(
            icon("table"),
            tags$div(
              class = "miro-pivot-btn-text",
              lang$renderers$miroPivot$btShowPivotControlsShort
            )
          ),
          title = lang$renderers$miroPivot$btShowPivotControls, `data-id` = ns("")
        )
      ),
      tags$div(
        class = "col-sm-12 col-md-4 filter-dropdowns-wrapper",
        class = "presentation-hide",
        style = if (isTRUE(options$hidePivotControls)) "padding: 1em;display:none;" else "padding: 1em;",
        tags$div(id = ns("filterDropdowns"), class = "miro-pivot-filter")
      ),
      tags$div(
        class = "col-sm-12 col-md-4 aggregate-dropdowns-wrapper",
        class = "presentation-hide",
        style = if (isTRUE(options$hidePivotControls)) "padding: 1em;display:none;" else "padding: 1em;",
        tags$div(id = ns("aggregateDropdowns"), class = "miro-pivot-filter")
      ),
      tags$div(
        class = "col-sm-12 col-md-2 aggregate-index-wrapper",
        style = if (isTRUE(options$hidePivotControls)) "padding: 1em;display:none;" else "padding: 1em;",
        class = "presentation-hide",
        tags$div(class = "drop-index-header", lang$renderers$miroPivot$aggregateLabel),
        tags$ul(
          id = ns("aggregationIndexList"), class = "drop-index-list aggregation-index-list",
          genIndexList(indices$aggregations)
        ),
        tags$div(
          class = "aggregation-function",
          selectInput(ns("aggregationFunction"),
            label = NULL,
            choices = c()
          )
        )
      )
    ),
    fluidRow(
      class = "col-filter",
      style = if (isTRUE(options$hidePivotControls)) "margin:0;display:none;" else "margin:0;",
      tags$div(
        class = "col-sm-12 col-md-2 col-charttype-wrapper",
        selectInput(ns("pivotRenderer"), "",
          setNames(
            c(
              "table", "heatmap", "pie", "doughnut", "bar", "horizontalbar",
              "stackedbar", "horizontalstackedbar", "line", "scatter", "area",
              "stackedarea", "radar", "timeseries"
            ),
            c(
              lang$renderers$miroPivot$renderer$table,
              lang$renderers$miroPivot$renderer$heatmap,
              lang$renderers$miroPivot$renderer$pie,
              lang$renderers$miroPivot$renderer$doughnut,
              lang$renderers$miroPivot$renderer$bar,
              lang$renderers$miroPivot$renderer$horizontalbar,
              lang$renderers$miroPivot$renderer$stackedbar,
              lang$renderers$miroPivot$renderer$horizontalstackedbar,
              lang$renderers$miroPivot$renderer$line,
              lang$renderers$miroPivot$renderer$scatter,
              lang$renderers$miroPivot$renderer$area,
              lang$renderers$miroPivot$renderer$stackedarea,
              lang$renderers$miroPivot$renderer$radar,
              lang$renderers$miroPivot$renderer$timeseries
            )
          ),
          selected = if (length(options$pivotRenderer)) {
            options$pivotRenderer
          } else {
            "table"
          }
        )
      ),
      tags$div(
        class = "col-sm-12 col-md-6 col-index-wrapper",
        style = "padding: 1em;",
        tags$div(
          class = "col-index-header drop-index-header",
          lang$renderers$miroPivot$columns
        ),
        tags$ul(
          id = ns("colIndexList"), class = "drop-index-list vertical-index-list",
          genIndexList(indices$cols)
        )
      ),
      tags$div(
        class = "col-sm-12 col-md-4 col-dropdowns-wrapper",
        tags$div(id = ns("colDropdowns"), class = "miro-pivot-filter")
      )
    ),
    fluidRow(
      class = "table-chart", style = "margin:0",
      tags$div(
        class = "col-sm-12 col-md-2 row-dropdowns-wrapper",
        style = if (isTRUE(options$hidePivotControls)) {
          "padding: 1em;padding-top: 31px;display:none;"
        } else {
          "padding: 1em;padding-top: 31px;"
        },
        tags$div(
          class = "row-index-header drop-index-header",
          lang$renderers$miroPivot$rows
        ),
        tags$ul(
          id = ns("rowIndexList"), class = "drop-index-list",
          genIndexList(indices$rows)
        )
      ),
      tags$div(
        class = if (isTRUE(options[["_input_"]])) "has-edit-buttons",
        class = "col-sm-12 pivot-chart-height",
        class = if (isTRUE(options$hidePivotControls)) "col-md-12" else "col-md-10",
        class = if (identical(options$fixedColumns, FALSE)) "dt-hide-fixed",
        style = if (isTRUE(options[["_input_"]]) && isTRUE(options$hidePivotControls)) {
          "margin-top:30px;"
        },
        tags$div(
          id = ns("errMsg"), class = "gmsalert gmsalert-error",
          style = "position:static;margin-bottom:5px;"
        ),
        tags$div(
          id = ns("noData"), class = "out-no-data",
          style = "margin-top:50px;",
          lang$nav$outputScreen$boxResults$noData
        ),
        genSpinner(ns("loadPivotTable"), hidden = TRUE),
        DTOutput(ns("pivotTable")),
        chartjsOutput(ns("pivotChart"), height = "40px"),
        if (isTRUE(options[["_input_"]])) {
          tags$div(
            class = "pivot-edit-btn-wrapper",
            actionButton(ns("btAddRow"), lang$renderers$miroPivot$btAddRow),
            actionButton(ns("btRemoveRows"), lang$renderers$miroPivot$btRemoveRows, class = "bt-highlight-1"),
            actionButton(ns("enableEdit"), lang$renderers$miroPivot$btEnableEdit,
              style = "display:none"
            )
          )
        }
      )
    ),
    fluidRow(
      id = ns("dataView"), class = "data-section",
      style = if (!isTRUE(options$hidePivotControls)) "display:none;",
      fluidRow(
        style = "margin:0;display:flex;",
        tags$div(
          class = "col-sm-12 col-md-4 data-section-block",
          tags$ul(
            class = "drop-index-list-presentation",
            tags$li(
              class = "list-presentation",
              fluidRow(
                class = "row-presentation",
                tags$div(
                  class = "col-sm-12 col-md-4 column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$rows
                  )
                ),
                tags$div(
                  class = "col-sm-12 col-md-8 column-presentation",
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
        tags$div(
          class = "col-sm-12 col-md-4 data-section-block",
          tags$ul(
            class = "drop-index-list-presentation",
            tags$li(
              class = "list-presentation",
              fluidRow(
                class = "row-presentation",
                tags$div(
                  class = "col-sm-12 col-md-4 column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$columns
                  )
                ),
                tags$div(
                  class = "col-sm-12 col-md-8 column-presentation",
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
        tags$div(
          class = "col-sm-12 col-md-4 data-section-block last-section",
          tags$ul(
            class = "drop-index-list-presentation",
            tags$li(
              class = "list-presentation",
              fluidRow(
                class = "row-presentation",
                tags$div(
                  class = "col-sm-12 col-md-4 column-presentation",
                  tags$div(
                    class = "data-section-header",
                    lang$renderers$miroPivot$aggregation
                  )
                ),
                tags$div(
                  class = "col-sm-12 col-md-8 column-presentation",
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
      data <- mutate(data, across(where(is.character), as.factor))

      noColDim <- 1L
      setIndices <- names(data)[-length(data)]

      noRowDim <- length(data) - 1L

      updateFilter <- reactiveVal(1L)
      updateRenderer <- reactiveVal(1L)

      isInput <- isTRUE(options[["_input_"]])
      isEditable <- FALSE
      bigData <- FALSE

      hiddenEmptyCols <- NULL

      hideEmptyCols <- reactiveVal(identical(options$hideEmptyCols, TRUE))
      singleDropdown <- reactiveVal(character(0))
      tableSummarySettings <- reactiveVal(list(
        rowEnabled = FALSE, rowSummaryFunction = "sum",
        colEnabled = FALSE, colSummaryFunction = "sum"
      ))

      if (isInput) {
        dataUpdated <- reactiveVal(1L)
        changesToApply <- list()
        if (identical(options[["readonly"]], TRUE)) {
          hideEl(session, paste0("#", ns("btAddRow")))
          hideEl(session, paste0("#", ns("btRemoveRows")))
        } else if (nrow(data) < 5e+05) {
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
            isolate({
              newVal <- updateFilter() + 1L
              updateFilter(newVal)
            })
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

      if (length(options$domainFilter$domains)) {
        if (identical(options$domainFilter$showAll, TRUE)) {
          domainFilterDomains <- c(options$domainFilter$domains, "_none")
        } else {
          domainFilterDomains <- options$domainFilter$domains
        }
      } else {
        domainFilterDomains <- NULL
      }

      if (isTRUE(options$hidePivotControls)) {
        session$sendCustomMessage("gms-activateMiroPivotPresentationObservers", ns(""))
      }

      resetView <- function(viewOptions, interfaceInitialized = TRUE) {
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
          viewOptions[["pivotRenderer"]] %in% c(
            "table", "heatmap", "line", "scatter", "area", "stackedarea",
            "bar", "stackedbar", "radar", "timeseries", "doughnut", "pie",
            "horizontalstackedbar", "horizontalbar"
          )) {
          updateSelectInput(session, "pivotRenderer", selected = viewOptions[["pivotRenderer"]])
        } else {
          updateSelectInput(session, "pivotRenderer", selected = "table")
        }
        if (isTRUE(options$enableHideEmptyCols)) {
          isolate(hideEmptyCols(identical(viewOptions[["hideEmptyCols"]], TRUE)))
        }
        if (length(viewOptions[["singleDropdown"]])) {
          isolate(singleDropdown(viewOptions[["singleDropdown"]]))
        } else {
          isolate(singleDropdown(character(0)))
        }
        if (length(viewOptions[["tableSummarySettings"]])) {
          bothEnabled <- identical(viewOptions[["tableSummarySettings"]][["enabled"]], TRUE)
          newTableSummarySettings <- list(
            rowEnabled = bothEnabled || identical(viewOptions[["tableSummarySettings"]][["rowEnabled"]], TRUE),
            rowSummaryFunction = "sum",
            colEnabled = bothEnabled || identical(viewOptions[["tableSummarySettings"]][["colEnabled"]], TRUE),
            colSummaryFunction = "sum"
          )
          if (length(viewOptions[["tableSummarySettings"]][["rowSummaryFunction"]]) == 1L &&
            viewOptions[["tableSummarySettings"]][["rowSummaryFunction"]] %in% aggregationFunctions[aggregationFunctions %in% c("sum", "count", "mean")]) {
            newTableSummarySettings[["rowSummaryFunction"]] <- viewOptions[["tableSummarySettings"]][["rowSummaryFunction"]]
          }
          if (length(viewOptions[["tableSummarySettings"]][["colSummaryFunction"]]) == 1L &&
            viewOptions[["tableSummarySettings"]][["colSummaryFunction"]] %in% aggregationFunctions) {
            newTableSummarySettings[["colSummaryFunction"]] <- viewOptions[["tableSummarySettings"]][["colSummaryFunction"]]
          }
        } else {
          newTableSummarySettings <- list(
            rowEnabled = FALSE, rowSummaryFunction = "sum",
            colEnabled = FALSE, colSummaryFunction = "sum"
          )
        }
        isolate(tableSummarySettings(newTableSummarySettings))
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
          if (identical(newView$domainFilter, "_none")) {
            newView$domainFilter <- NULL
          }
        }
        if (length(viewOptions$name)) {
          setTextContent(session, paste0("#", ns("toggleViewButton")),
            viewOptions$name,
            keepChildNodes = TRUE
          )
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
      currentView <- options
      if (isTRUE(options$resetOnInit) || (length(options$externalDefaultView) && options$externalDefaultView %in% views$getIds(session))) {
        if (length(options$externalDefaultView) && options$externalDefaultView %in% views$getIds(session)) {
          currentView <- views$get(session, options$externalDefaultView)
          options$resetOnInit <- TRUE
          resetFilters <- TRUE
        }
        resetView(currentView, interfaceInitialized = FALSE)
      }
      if (is.null(input$aggregationFunction) || identical(input$aggregationFunction, "")) {
        # interface has not been initialised, do it now
        resetFilters <- TRUE
        if (length(currentView[["aggregationFunction"]]) &&
          currentView[["aggregationFunction"]] %in% aggregationFunctions) {
          updateSelectInput(session, "aggregationFunction",
            choices = aggregationFunctions,
            selected = currentView[["aggregationFunction"]]
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

          localViewIds <- views$getIds(session, "local")

          viewChoices <- lapply(sort(views$getIds(session)), function(viewId) {
            if (viewId %in% localViewIds && !readonlyViews) {
              return(createBootstrapDropdownChoices(
                list(
                  id = htmlIdEnc(viewId),
                  alias = viewId
                ),
                ns("savedViews"), ns("editView"), ns("deleteView")
              ))
            }
            return(createBootstrapDropdownChoices(
              list(
                id = htmlIdEnc(viewId),
                alias = viewId
              ),
              ns("savedViews")
            ))
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
        readonlyViews <- views$isReadonly(session)
        updateViewList()
        if (readonlyViews) {
          disableEl(session, paste0("#", ns("saveView")))
        } else {
          views$registerUpdateCallback(session, updateViewList)
        }

        showAddViewDialog <- function(pivotRenderer, viewOptions = NULL) {
          miroPivotState$editView <<- length(viewOptions) > 0L
          if (length(pivotRenderer) &&
            pivotRenderer %in% c(
              "line", "scatter", "area", "stackedarea", "pie", "doughnut",
              "bar", "stackedbar", "radar", "timeseries", "horizontalbar",
              "horizontalstackedbar"
            )) {
            moreOptions <- NULL
            if (pivotRenderer %in% c(
              "bar", "stackedbar", "line", "scatter", "area", "stackedarea",
              "timeseries", "horizontalbar", "horizontalstackedbar"
            )) {
              moreOptions <- c(
                list(
                  tabPanel(
                    lang$renderers$miroPivot$newView$tabGeneral,
                    tags$div(class = "small-space"),
                    tags$div(
                      class = "row",
                      tags$div(
                        class = "col-sm-12",
                        textInput(ns("advancedTitle"),
                          width = "100%",
                          lang$renderers$miroPivot$newView$chartTitle,
                          value = viewOptions$chartOptions$title
                        )
                      ),
                      tags$div(
                        class = "col-sm-6",
                        textInput(ns("advancedxTitle"),
                          width = "100%",
                          lang$renderers$miroPivot$newView$xTitle,
                          value = viewOptions$chartOptions$xTitle
                        )
                      ),
                      tags$div(
                        class = "col-sm-6",
                        textInput(ns("advancedyTitle"),
                          width = "100%",
                          lang$renderers$miroPivot$newView$yTitle,
                          value = viewOptions$chartOptions$yTitle
                        )
                      )
                    ),
                    tags$div(
                      class = "row",
                      if (pivotRenderer %in% c("line", "area", "stackedarea", "radar")) {
                        tags$div(
                          class = "col-sm-6",
                          checkboxInput_MIRO(ns("showDataMarkers"),
                            label = lang$renderers$miroPivot$newView$showDataMarkers,
                            value = !identical(viewOptions$chartOptions$showDataMarkers, FALSE)
                          )
                        )
                      },
                      if (identical(pivotRenderer, "line")) {
                        tags$div(
                          class = "col-sm-6",
                          checkboxInput_MIRO(ns("stepPlot"),
                            label = lang$renderers$miroPivot$newView$stepPlot,
                            value = identical(viewOptions$chartOptions$stepPlot, TRUE)
                          )
                        )
                      } else if (pivotRenderer %in% c("stackedarea", "horizontalstackedbar", "stackedbar")) {
                        tags$div(
                          class = "col-sm-6",
                          checkboxInput_MIRO(ns("singleStack"),
                            label = lang$renderers$miroPivot$newView$singleStack,
                            value = identical(viewOptions$chartOptions$singleStack, TRUE)
                          )
                        )
                      }
                    )
                  ),
                  tabPanel(
                    lang$renderers$miroPivot$newView$tabScales,
                    tags$div(class = "small-space"),
                    tags$div(
                      class = "row",
                      tags$div(
                        class = "col-sm-6",
                        numericInput(ns("yMin"),
                          width = "100%",
                          label = lang$renderers$miroPivot$newView$minY,
                          value = viewOptions$chartOptions$yMin
                        )
                      ),
                      tags$div(
                        class = "col-sm-6",
                        numericInput(ns("yMax"),
                          width = "100%",
                          label = lang$renderers$miroPivot$newView$maxY,
                          value = viewOptions$chartOptions$yMax
                        )
                      )
                    ),
                    tags$div(
                      class = "row",
                      tags$div(
                        class = "col-sm-6",
                        checkboxInput_MIRO(ns("xGrid"),
                          label = lang$renderers$miroPivot$newView$xGrid,
                          value = !isFALSE(viewOptions$chartOptions$showXGrid)
                        )
                      ),
                      tags$div(
                        class = "col-sm-6",
                        checkboxInput_MIRO(ns("yGrid"),
                          label = lang$renderers$miroPivot$newView$yGrid,
                          value = !isFALSE(viewOptions$chartOptions$showYGrid)
                        )
                      )
                    ),
                    tags$div(
                      class = "row",
                      tags$div(
                        class = "col-sm-6",
                        checkboxInput_MIRO(ns("useLogScaleY"),
                          label = lang$renderers$miroPivot$newView$yLogScale,
                          value = identical(viewOptions$chartOptions$yLogScale, TRUE)
                        )
                      )
                    )
                  ),
                  tabPanel(
                    lang$renderers$miroPivot$newView$tabMultiChart,
                    tags$div(class = "small-space"),
                    tags$div(
                      class = "row",
                      tags$div(
                        class = "col-sm-12",
                        selectInput(ns("addMultiChartSeries"),
                          width = "100%",
                          label = lang$renderers$miroPivot$newView$multiChartSeries,
                          choices = miroPivotState$currentSeriesLabels,
                          selected = viewOptions$chartOptions$multiChartSeries,
                          multiple = TRUE
                        )
                      )
                    ),
                    tags$div(
                      class = "row",
                      tags$div(
                        class = "col-sm-6",
                        `data-display-if` = "input.addMultiChartSeries?.length>0",
                        `data-ns-prefix` = ns(""),
                        selectInput(ns("miroPivotMultiChartRenderer"),
                          width = "100%",
                          label = lang$renderers$miroPivot$newView$multiChartSeriesTypeSelection,
                          choices = setNames(
                            c("line", "bar", "scatter"),
                            c(
                              lang$renderers$miroPivot$renderer$line,
                              lang$renderers$miroPivot$renderer$bar,
                              lang$renderers$miroPivot$renderer$scatter
                            )
                          ),
                          selected = if (length(viewOptions$chartOptions$multiChartOptions$multiChartRenderer)) {
                            viewOptions$chartOptions$multiChartOptions$multiChartRenderer
                          } else if (pivotRenderer %in% c(
                            "line", "area", "stackedarea", "timeseries"
                          )) {
                            "bar"
                          } else {
                            "line"
                          }
                        )
                      ),
                      tags$div(
                        class = "col-sm-6",
                        `data-display-if` = "input.addMultiChartSeries?.length>0 && ['stackedarea', 'horizontalstackedbar', 'stackedbar'].includes(input.pivotRenderer)",
                        `data-ns-prefix` = ns(""),
                        selectInput(ns("stackMultiChartSeries"),
                          width = "100%",
                          label = lang$renderers$miroPivot$newView$stackMultiChartSeries,
                          choices = setNames(
                            c("no", "regularStack", "individualStack"),
                            c(
                              lang$renderers$miroPivot$newView$stackType1,
                              lang$renderers$miroPivot$newView$stackType2,
                              lang$renderers$miroPivot$newView$stackType3
                            )
                          ),
                          selected = if (length(viewOptions$chartOptions$multiChartOptions$stackMultiChartSeries)) viewOptions$chartOptions$multiChartOptions$stackMultiChartSeries else "no"
                        )
                      ),
                      tags$div(
                        class = "col-sm-6",
                        `data-display-if` = "input.addMultiChartSeries?.length>0 && input.miroPivotMultiChartRenderer==='line'",
                        `data-ns-prefix` = ns(""),
                        checkboxInput_MIRO(ns("multiChartStepPlot"),
                          label = lang$renderers$miroPivot$newView$stepPlot,
                          value = identical(viewOptions$chartOptions$multiChartOptions$multiChartStepPlot, TRUE)
                        )
                      ),
                      tags$div(
                        class = "col-sm-6",
                        `data-display-if` = "input.addMultiChartSeries?.length>0 && input.miroPivotMultiChartRenderer==='line'",
                        `data-ns-prefix` = ns(""),
                        checkboxInput_MIRO(ns("showMultiChartDataMarkers"),
                          label = lang$renderers$miroPivot$newView$showMultiChartDataMarkers,
                          value = identical(viewOptions$chartOptions$multiChartOptions$showMultiChartDataMarkers, TRUE)
                        )
                      )
                    )
                  ),
                  tabPanel(
                    lang$renderers$miroPivot$newView$tabSecondAxis,
                    tags$div(class = "small-space"),
                    tags$div(
                      class = "row",
                      tags$div(
                        class = "col-sm-12",
                        checkboxInput_MIRO(ns("y2axis"),
                          label = if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) {
                            lang$renderers$miroPivot$newView$x2axis
                          } else {
                            lang$renderers$miroPivot$newView$y2axis
                          },
                          value = length(viewOptions$chartOptions$y2axis) > 0L
                        )
                      )
                    ),
                    conditionalPanel("input.y2axis===true",
                      ns = ns,
                      tags$div(
                        class = "row",
                        tags$div(
                          class = "col-sm-6",
                          selectInput(ns("y2axisSeries"),
                            width = "100%",
                            label = lang$renderers$miroPivot$newView$y2axisSeries,
                            choices = miroPivotState$currentSeriesLabels,
                            selected = viewOptions$chartOptions$y2axis$series,
                            multiple = TRUE
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          textInput(ns("y2axisTitle"),
                            width = "100%",
                            lang$renderers$miroPivot$newView$y2axisTitle,
                            value = viewOptions$chartOptions$y2axis$title
                          )
                        )
                      ),
                      tags$div(
                        class = "row",
                        tags$div(
                          class = "col-sm-6",
                          checkboxInput_MIRO(ns("y2axisUseLogScaleY"),
                            label = lang$renderers$miroPivot$newView$y2axisLogScale,
                            value = identical(viewOptions$chartOptions$y2axis$logScale, TRUE)
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          checkboxInput_MIRO(ns("y2axisDrawGrid"),
                            label = lang$renderers$miroPivot$newView$y2axisGrid,
                            value = identical(viewOptions$chartOptions$y2axis$showGrid, TRUE)
                          )
                        )
                      ),
                      tags$div(
                        class = "row",
                        tags$div(
                          class = "col-sm-6",
                          numericInput(ns("y2Min"),
                            width = "100%",
                            label = lang$renderers$miroPivot$newView$minY2,
                            value = viewOptions$chartOptions$y2axis$min
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          numericInput(ns("y2Max"),
                            width = "100%",
                            label = lang$renderers$miroPivot$newView$maxY2,
                            value = viewOptions$chartOptions$y2axis$max
                          )
                        )
                      )
                    )
                  )
                )
              )
            } else if (pivotRenderer %in% c("pie", "doughnut", "radar")) {
              moreOptions <- c(
                list(tabPanel(
                  lang$renderers$miroPivot$newView$tabGeneral,
                  tags$div(class = "small-space"),
                  tags$div(
                    class = "row",
                    tags$div(
                      class = "col-sm-12",
                      textInput(ns("advancedTitle"),
                        width = "100%",
                        lang$renderers$miroPivot$newView$chartTitle,
                        value = viewOptions$chartOptions$title
                      )
                    )
                  )
                ))
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
                class = "tabs-ul-mobile-helper view-menu",
                do.call(
                  tabsetPanel,
                  c(
                    id = ns("newViewTabs"),
                    moreOptions,
                    list(tabPanel(
                      lang$renderers$miroPivot$newView$tabColors,
                      tags$div(class = "small-space"),
                      tags$div(
                        class = "row",
                        if (pivotRenderer %in% c("stackedarea", "area")) {
                          tags$div(
                            class = "col-sm-6",
                            `data-ns-prefix` = ns(""),
                            numericInput(ns("fillOpacity"),
                              width = "100%",
                              min = 0,
                              max = 1,
                              step = 0.1,
                              label = lang$renderers$miroPivot$newView$fillOpacity,
                              value = viewOptions$chartOptions$fillOpacity
                            )
                          )
                        }
                      ),
                      tags$div(
                        class = "row",
                        tags$div(
                          class = "col-sm-6",
                          checkboxInput_MIRO(ns("useCustomChartColors"),
                            label = lang$renderers$miroPivot$newView$cbCustomColors,
                            value = length(viewOptions$chartOptions$customChartColors) > 0L
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          `data-display-if` = "input.useCustomChartColors===true",
                          `data-ns-prefix` = ns(""),
                          checkboxInput_MIRO("miroPivotCbCustomColorInputs",
                            label = lang$renderers$miroPivot$newView$cbManualColors,
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
                    ))
                  )
                )
              )
            )
          } else if (length(pivotRenderer) &&
            pivotRenderer %in% c("table", "heatmap")) {
            additionalOptionsContent <- tags$div(
              id = ns("newViewOptionsWrapper"), style = "text-align:left;",
              tags$div(
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-sm-12",
                    textInput(ns("advancedTitle"),
                      width = "100%",
                      lang$renderers$miroPivot$newView$chartTitle,
                      value = viewOptions$chartOptions$title
                    )
                  )
                )
              )
            )
          } else {
            additionalOptionsContent <- NULL
          }
          showModal(modalDialog(
            tags$div(
              id = ns("errUniqueName"), style = "display:none;",
              lang$renderers$miroPivot$errUniqueViewName
            ),
            textInput(ns("newViewName"),
              width = "100%",
              lang$renderers$miroPivot$newView$label,
              value = viewOptions$name
            ),
            additionalOptionsContent,
            footer = tagList(
              if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) {
                tags$div(
                  style = "text-align:left;",
                  tags$span(
                    class = "fas fa-circle-info info-icon"
                  ),
                  lang$renderers$miroPivot$newView$info
                )
              },
              tags$div(
                id = ns("saveViewButtonsWrapper"),
                modalButton(lang$renderers$miroPivot$newView$btCancel),
                actionButton(ns("saveViewConfirm"),
                  lang$renderers$miroPivot$newView$btSave,
                  class = "bt-highlight-1 bt-gms-confirm"
                )
              ),
              tags$div(
                id = ns("saveViewOverwriteButtonsWrapper"), style = "display:none",
                actionButton(
                  ns("saveViewCancelOverwrite"),
                  lang$renderers$miroPivot$newView$btCancelOverwrite
                ),
                actionButton(ns("saveViewOverwrite"),
                  lang$renderers$miroPivot$newView$btOverwrite,
                  class = "bt-highlight-1 bt-gms-confirm"
                )
              )
            ),
            fade = TRUE, easyClose = FALSE, size = "m",
            title = lang$renderers$miroPivot$newView$title
          ))
        }

        rendererEnv[[ns("saveView")]] <- observe({
          if (is.null(input$saveView) || initData || input$saveView == 0L || readonlyViews) {
            return()
          }
          showAddViewDialog(isolate(input$pivotRenderer))
        })
        rendererEnv[[ns("showSettings")]] <- observe({
          if (is.null(input$showSettings) || initData || input$showSettings == 0L) {
            return()
          }
          isolate({
            showModal(modalDialog(
              tagList(
                if (isTRUE(options$enableHideEmptyCols)) {
                  tags$div(
                    class = "row",
                    tags$div(
                      class = "col-sm-12",
                      checkboxInput_MIRO(ns("hideEmptyCols"), lang$renderers$miroPivot$cbHideEmptyCols,
                        value = hideEmptyCols()
                      )
                    )
                  )
                },
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-sm-12",
                    `data-ns-prefix` = ns(""),
                    selectInput(ns("singleDropdown"),
                      label = lang$renderers$miroPivot$singleDropdown,
                      choices = setIndices,
                      selected = singleDropdown(),
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-sm-6",
                    checkboxInput_MIRO(ns("showTableSummaryCol"),
                      label = lang$renderers$miroPivot$cbShowTableSummaryCol,
                      value = tableSummarySettings()$colEnabled
                    )
                  ),
                  tags$div(
                    class = "col-sm-6",
                    `data-display-if` = "input.showTableSummaryCol===true",
                    `data-ns-prefix` = ns(""),
                    selectInput(ns("colSummaryFunction"),
                      label = lang$renderers$miroPivot$colSummaryFunction,
                      choices = aggregationFunctions,
                      selected = tableSummarySettings()$colSummaryFunction,
                      width = "100%"
                    )
                  )
                ),
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-sm-6",
                    checkboxInput_MIRO(ns("showTableSummaryRow"),
                      label = lang$renderers$miroPivot$cbShowTableSummaryRow,
                      value = tableSummarySettings()$rowEnabled
                    )
                  ),
                  tags$div(
                    class = "col-sm-6",
                    `data-display-if` = "input.showTableSummaryRow===true",
                    `data-ns-prefix` = ns(""),
                    selectInput(ns("rowSummaryFunction"),
                      label = lang$renderers$miroPivot$rowSummaryFunction,
                      choices = aggregationFunctions[aggregationFunctions %in% c("sum", "count", "mean")],
                      selected = tableSummarySettings()$rowSummaryFunction,
                      width = "100%"
                    )
                  )
                )
              ),
              footer = tags$div(
                modalButton(lang$renderers$miroPivot$updateSettingsBtCancel),
                actionButton(ns("updateSettings"),
                  lang$renderers$miroPivot$updateSettingsBtSave,
                  class = "bt-highlight-1 bt-gms-confirm"
                )
              ),
              fade = TRUE, easyClose = FALSE, size = "m",
              title = lang$renderers$miroPivot$updateSettingsTitle
            ))
          })
        })
        deleteView <- function(viewId) {
          views$remove(session, viewId)
          updateViewList()
        }
        addNewView <- function(viewName, overwrite = FALSE) {
          isolate({
            refreshRequired <- FALSE
            newViewConfig <- list(
              aggregationFunction = input$aggregationFunction,
              pivotRenderer = input$pivotRenderer,
              domainFilter = list(default = input$domainFilter)
            )
            if (isTRUE(options$enableHideEmptyCols)) {
              newViewConfig$hideEmptyCols <- hideEmptyCols()
            }
            newViewConfig$singleDropdown <- singleDropdown()
            newViewConfig$tableSummarySettings <- tableSummarySettings()
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
            if (length(isolate(input$pivotRenderer)) &&
              isolate(input$pivotRenderer) %in% c(
                "line", "scatter", "area", "stackedarea",
                "bar", "stackedbar", "radar", "timeseries",
                "pie", "doughnut", "horizontalbar", "horizontalstackedbar",
                "table", "heatmap"
              )) {
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
                  refreshRequired <- TRUE
                  if (nchar(optionValTmp) > 0L) {
                    newViewConfig$chartOptions[[advancedOption$optionId]] <- optionValTmp
                  }
                }
              }
              if (length(input[["yMin"]])) {
                refreshRequired <- TRUE
                newViewConfig$chartOptions[["yMin"]] <- if (is.na(input[["yMin"]])) NULL else input[["yMin"]]
              }
              if (length(input[["yMax"]])) {
                refreshRequired <- TRUE
                newViewConfig$chartOptions[["yMax"]] <- if (is.na(input[["yMax"]])) NULL else input[["yMax"]]
              }
              if (length(input[["fillOpacity"]])) {
                refreshRequired <- TRUE
                newViewConfig$chartOptions[["fillOpacity"]] <- if (is.na(input[["fillOpacity"]])) NULL else input[["fillOpacity"]]
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
              for (advancedOption in list(
                list(
                  inputId = "useLogScaleY",
                  optionId = "yLogScale"
                ),
                list(
                  inputId = "showDataMarkers",
                  optionId = "showDataMarkers"
                ),
                list(
                  inputId = "stepPlot",
                  optionId = "stepPlot"
                ),
                list(
                  inputId = "xGrid",
                  optionId = "showXGrid"
                ),
                list(
                  inputId = "yGrid",
                  optionId = "showYGrid"
                ),
                list(
                  inputId = "singleStack",
                  optionId = "singleStack"
                )
              )) {
                if (length(input[[advancedOption$inputId]])) {
                  refreshRequired <- TRUE
                  newViewConfig$chartOptions[[advancedOption$optionId]] <- isTRUE(input[[advancedOption$inputId]])
                }
              }
              if (isTRUE(input[["y2axis"]])) {
                refreshRequired <- TRUE

                y2axisSeries <- input[["y2axisSeries"]]
                y2axisTitle <- trimws(input[["y2axisTitle"]])
                y2axisDrawGrid <- isTRUE(input[["y2axisDrawGrid"]])
                y2axisUseLogScaleY <- isTRUE(input[["y2axisUseLogScaleY"]])
                y2axisy2Min <- input[["y2Min"]]
                y2axisy2Max <- input[["y2Max"]]

                newViewConfig$chartOptions[["y2axis"]] <- Filter(function(x) !is.null(x) && !any(is.na(x)), list(
                  series = if (length(y2axisSeries)) y2axisSeries else NULL,
                  title = if (nchar(y2axisTitle) > 0L) y2axisTitle else NULL,
                  showGrid = y2axisDrawGrid,
                  logScale = y2axisUseLogScaleY,
                  min = y2axisy2Min,
                  max = y2axisy2Max
                ))
              }
              if (length(input[["addMultiChartSeries"]])) {
                refreshRequired <- TRUE
                newViewConfig$chartOptions$multiChartSeries <- input[["addMultiChartSeries"]]

                showMultiChartDataMarkers <- isTRUE(input[["showMultiChartDataMarkers"]])
                multiChartRenderer <- trimws(input[["miroPivotMultiChartRenderer"]])
                stackMultiChartSeries <- input[["stackMultiChartSeries"]]
                multiChartStepPlot <- isTRUE(input[["multiChartStepPlot"]])

                newViewConfig$chartOptions[["multiChartOptions"]] <- Filter(Negate(function(x) is.null(x) || is.na(x)), list(
                  showMultiChartDataMarkers = showMultiChartDataMarkers,
                  multiChartRenderer = multiChartRenderer,
                  stackMultiChartSeries = stackMultiChartSeries,
                  multiChartStepPlot = multiChartStepPlot
                ))
              }
            }
            views$add(session, viewName, newViewConfig)

            if (miroPivotState$editView &&
              !identical(viewName, currentView$name)) {
              # view was renamed
              deleteView(currentView$name)
            } else {
              updateViewList()
            }
            if (length(viewName)) {
              setTextContent(session, paste0("#", ns("toggleViewButton")),
                viewName,
                keepChildNodes = TRUE
              )
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
        rendererEnv[[ns("updateSettings")]] <- observe({
          if (is.null(input$updateSettings) || initData ||
            input$updateSettings == 0L) {
            return()
          }
          isolate({
            newTableSummarySettings <- list(
              rowEnabled = identical(input$showTableSummaryRow, TRUE),
              rowSummaryFunction = "sum",
              colEnabled = identical(input$showTableSummaryCol, TRUE),
              colSummaryFunction = "sum"
            )
            if (length(input$rowSummaryFunction) == 1L &&
              input$rowSummaryFunction %in% aggregationFunctions[aggregationFunctions %in% c("sum", "count", "mean")]) {
              newTableSummarySettings[["rowSummaryFunction"]] <- input$rowSummaryFunction
            }
            if (length(input$colSummaryFunction) == 1L &&
              input$colSummaryFunction %in% aggregationFunctions) {
              newTableSummarySettings[["colSummaryFunction"]] <- input$colSummaryFunction
            }
            if (length(input$singleDropdown) > 0 &&
              all(input$singleDropdown %in% setIndices)) {
              newSingleDropdown <- input$singleDropdown
            } else {
              newSingleDropdown <- character(0)
            }
            if (!identical(singleDropdown(), newSingleDropdown)) {
              singleDropdown(newSingleDropdown)
            }
            tableSummarySettings(newTableSummarySettings)
            hideEmptyCols(identical(input$hideEmptyCols, TRUE))
            removeModal(session)
          })
        })
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
            !viewId %in% views$getIds(session, "local")) {
            flog.error(
              "Invalid view id: '%s' attempted to be edited. This looks like an attempt to tamper with the app!",
              input$editView
            )
            return()
          }
          miroPivotState$triggerEditViewDialog <<- TRUE
          currentView <<- views$get(session, viewId)
          currentView$name <<- viewId
          resetView(currentView)
        })
        rendererEnv[[ns("deleteView")]] <- observe({
          if (is.null(input$deleteView) || initData || readonlyViews) {
            return()
          }
          viewId <- htmlIdDec(input$deleteView)
          if (length(viewId) != 1L ||
            !viewId %in% views$getIds(session, "local")) {
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
          externalViewIds <- views$getIds(session)
          if (length(viewId) != 1L ||
            !viewId %in% c("default", externalViewIds)) {
            flog.error(
              "Invalid view id: '%s' attempted to be loaded. This looks like an attempt to tamper with the app!",
              input$savedViews
            )
            return()
          }
          currentView <<- if (identical(viewId, "default")) {
            if (length(options$externalDefaultView) && options$externalDefaultView %in% externalViewIds) {
              views$get(session, options$externalDefaultView)
            } else {
              options
            }
          } else {
            views$get(session, viewId)
          }
          currentView$name <<- viewId
          resetView(currentView)
        })
      }

      output$downloadCsv <- downloadHandler(
        filename = paste0(options[["_metadata_"]]$symname, ".csv"),
        content = function(file) {
          if (isInput) {
            dataUpdated()
            dataTmp <- dfApplyChanges(dataToRender(), changesToApply)
          } else {
            dataTmp <- dataToRender()
          }
          if (length(hiddenEmptyCols)) {
            return(write_csv(dataTmp[-hiddenEmptyCols],
              file,
              na = ""
            ))
          }
          return(write_csv(dataTmp, file, na = ""))
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
          ddHash <- digest::digest(
            list(
              filterIndex,
              choices,
              multiple = !filterIndex %in% filteredData()$singleFilterIndices
            ),
            algo = "sha1"
          )
          list(
            htmltools::doRenderTags(
              htmltools::tagAppendAttributes(
                serverSelectInput(session, ns(paste0("filter_", filterIndex)), setIndexAliases[[filterIndex]],
                  choices = choices,
                  selected = if (!filterIndex %in% filteredData()$singleFilterIndices) {
                    selectedFilterVal
                  } else if (nchar(selectedFilterVal[1])) {
                    selectedFilterVal[1]
                  } else {
                    filterElements[[filterIndex]][1]
                  },
                  multiple = !filterIndex %in% filteredData()$singleFilterIndices,
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
          if (identical(newFilters$domainFilter, "_none")) {
            newFilters$domainFilter <- NULL
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
          multiFilterIndices = multiFilterIndices,
          singleFilterIndices = singleDropdown()
        ))
      })

      dataToRender <- reactive({
        if (isInput) {
          changesToApply <<- list()
        }
        if (identical(input$aggregationFunction, "")) {
          return()
        }
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
            isEditable <<- !identical(options[["readonly"]], TRUE)
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
        if (!pivotRenderer %in% c(
          "line", "scatter", "area", "stackedarea", "bar",
          "stackedbar", "radar", "timeseries", "pie", "doughnut",
          "horizontalbar", "horizontalstackedbar"
        )) {
          return()
        }
        dataTmp <- dataToRender()
        if (!length(dataTmp)) {
          if (miroPivotState$triggerEditViewDialog) {
            miroPivotState$triggerEditViewDialog <<- FALSE
            showAddViewDialog(pivotRenderer, viewOptions = currentView)
          }
          showEl(session, paste0("#", ns("noData")))
          return()
        }
        hideEl(session, paste0("#", ns("noData")))
        showEl(session, paste0("#", ns("loadPivotTable")))
        rowHeaderLen <- attr(dataTmp, "noRowHeaders")
        noSeries <- length(dataTmp) - rowHeaderLen
        noError <- TRUE
        if (identical(pivotRenderer, "timeseries")) {
          if (nrow(dataTmp) > 2e4) {
            showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(lang$renderers$miroPivot$rowTruncationWarning, "20 000"))
            dataTmp <- slice(dataTmp, 1:20000L)
            noError <- FALSE
          }
          dataTmp <- dataTmp[stri_order(dataTmp[[1]], numeric = TRUE), ]
        } else {
          if (nrow(dataTmp) > 500L) {
            showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(lang$renderers$miroPivot$rowTruncationWarning, "500"))
            dataTmp <- slice(dataTmp, 1:500L)
            noError <- FALSE
          }
        }
        if (noSeries > 40L) {
          showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(lang$renderers$miroPivot$colTruncationWarning, "40"))
          noError <- FALSE
        }
        setCssEl(
          session, paste0("#", ns("pivotChart-container")),
          list(position = "", top = "")
        )
        if (noError) {
          hideEl(session, paste0("#", ns("errMsg")))
        }
        if (isTRUE(options$enableHideEmptyCols) && hideEmptyCols()) {
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
        if (pivotRenderer %in% c(
          "line", "scatter", "area", "stackedarea",
          "timeseries"
        )) {
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
            chartJsObj$x$scales$y$stacked <- if (identical(currentView$chartOptions$singleStack, TRUE)) "single" else TRUE
            chartJsObj$x$options$plugins$tooltip <- list(
              mode = "index",
              position = "nearest"
            )
          } else if (identical(pivotRenderer, "timeseries")) {
            chartJsObj$x$options$normalized <- TRUE
            chartJsObj$x$options$animation <- FALSE
            chartJsObj$x$options$elements <- list(
              point = list(
                radius = 0L,
                hitRadius = 4L
              )
            )
            chartJsObj$x$options$plugins$tooltip <- list(
              mode = "index",
              position = "nearest"
            )
          }
        } else if (pivotRenderer %in% c("stackedbar", "horizontalstackedbar")) {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsBar(
              labels = labels, stacked = TRUE,
              xTitle = currentView$chartOptions$xTitle,
              yTitle = currentView$chartOptions$yTitle
            )
          chartJsObj$x$scales$y$stacked <- if (identical(currentView$chartOptions$singleStack, TRUE)) "single" else TRUE
          chartJsObj$x$options$plugins$tooltip <- list(
            mode = "index",
            position = "nearest"
          )
        } else if (identical(pivotRenderer, "radar")) {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsRadar(labels = labels)
        } else if (identical(pivotRenderer, "pie")) {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsPie(labels = labels)
        } else if (identical(pivotRenderer, "doughnut")) {
          chartJsObj <- chartjs(
            customColors = chartColorsToUse,
            title = currentView$chartOptions$title
          ) %>%
            cjsDoughnut(labels = labels, cutout = 80)
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
        if (length(currentView$chartOptions$y2axis$series)) {
          axisType <- if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) "x2" else "y2"
          position <- if (axisType == "x2") "top" else "right"

          chartJsObj <- cjsAddScale(chartJsObj,
            axis = axisType,
            position = position,
            type = if (identical(currentView$chartOptions$y2axis$logScale, TRUE)) "logarithmic" else "linear",
            display = "auto",
            title = list(
              text = if (length(currentView$chartOptions$y2axis$title)) currentView$chartOptions$y2axis$title else NULL,
              display = if (length(currentView$chartOptions$y2axis$title)) TRUE else FALSE
            )
          )

          if (identical(currentView$chartOptions$y2axis$showGrid, FALSE)) {
            chartJsObj$x$scales[[axisType]]$grid$display <- FALSE
          }
          if (length(currentView$chartOptions$y2axis$min)) {
            chartJsObj$x$scales[[axisType]]$min <- currentView$chartOptions$y2axis$min
          }
          if (length(currentView$chartOptions$y2axis$max)) {
            chartJsObj$x$scales[[axisType]]$max <- currentView$chartOptions$y2axis$max
          }
        }

        if (identical(currentView$chartOptions$yLogScale, TRUE)) {
          if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) {
            chartJsObj$x$scales$x$type <- "logarithmic"
            chartJsObj$x$options$indexAxis <- "y"
            chartJsObj$x$scales$y$type <- "category"
          } else if (identical(chartJsObj$x$scales$y$type, "linear")) {
            chartJsObj$x$scales$y$type <- "logarithmic"
          }
        } else if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) {
          chartJsObj$x$scales$x$type <- "linear"
          chartJsObj$x$options$indexAxis <- "y"
          chartJsObj$x$scales$y$type <- "category"
        }

        axisType <- if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) "x" else "y"
        xGrid <- if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) "y" else "x"
        if (length(currentView$chartOptions$yMin)) {
          chartJsObj$x$scales[[axisType]]$min <- currentView$chartOptions$yMin
        }
        if (length(currentView$chartOptions$yMax)) {
          chartJsObj$x$scales[[axisType]]$max <- currentView$chartOptions$yMax
        }
        if (identical(currentView$chartOptions$showXGrid, FALSE)) {
          chartJsObj$x$scales[[xGrid]]$grid$display <- FALSE
        }
        if (identical(currentView$chartOptions$showYGrid, FALSE)) {
          chartJsObj$x$scales[[axisType]]$grid$display <- FALSE
        }

        if (identical(currentView$chartOptions$showDataMarkers, FALSE) &&
          !identical(pivotRenderer, "scatter")) {
          chartJsObj$x$options$normalized <- TRUE
          chartJsObj$x$options$animation <- FALSE
          chartJsObj$x$options$elements <- list(
            point = list(
              radius = 0L,
              hitRadius = 4L
            )
          )
          chartJsObj$x$options$plugins$tooltip <- list(
            mode = "index",
            position = "nearest"
          )
        }
        chartJsObj$x$options$locale <- "en-US"
        chartJsObj$x$options$plugins$zoom <- list(
          zoom = list(
            wheel = list(
              enabled = TRUE
            ),
            pinch = list(
              enabled = TRUE
            ),
            mode = "xy",
            scaleMode = "y"
          ),
          pan = list(
            enabled = TRUE,
            mode = "xy"
          )
        )
        multiChartRenderer <- character(0)
        if (length(currentView$chartOptions$multiChartOptions$multiChartRenderer)) {
          multiChartRenderer <- currentView$chartOptions$multiChartOptions$multiChartRenderer
        }
        for (i in seq_len(min(noSeries, 40L))) {
          label <- names(dataTmp)[rowHeaderLen + i]
          scaleID <- NULL
          if (length(currentView$chartOptions$y2axis$series) && label %in% currentView$chartOptions$y2axis$series) {
            if (pivotRenderer %in% c("horizontalbar", "horizontalstackedbar")) {
              scaleID <- "x2"
            } else {
              scaleID <- "y2"
            }
          }
          if (label %in% currentView$chartOptions$multiChartSeries) {
            if (pivotRenderer %in% c("line", "area", "stackedarea", "timeseries")) {
              multiChartRenderer <- if (length(multiChartRenderer)) multiChartRenderer else "bar"
            } else {
              multiChartRenderer <- if (length(multiChartRenderer)) multiChartRenderer else "line"
            }
            if ((multiChartRenderer %in% c("line", "scatter") || identical(pivotRenderer, "stackedarea")) &&
              !identical(currentView$chartOptions$multiChartOptions$stackMultiChartSeries, "regularStack")) {
              order <- 0
            } else {
              order <- 2
            }

            chartJsObj <- cjsSeries(chartJsObj, dataTmp[[rowHeaderLen + i]],
              label = label,
              type = multiChartRenderer,
              showLine = multiChartRenderer %in% c("line", "area", "stackedarea", "timeseries"),
              order = order,
              scaleID = scaleID,
              pointHitRadius = if (identical(currentView$chartOptions$multiChartOptions$showMultiChartDataMarkers, TRUE)) 1L else 0,
              pointRadius = if (identical(currentView$chartOptions$multiChartOptions$showMultiChartDataMarkers, TRUE)) 3L else 0,
              stack = if (identical(currentView$chartOptions$multiChartOptions$stackMultiChartSeries, "regularStack")) {
                "stack1"
              } else if (identical(currentView$chartOptions$multiChartOptions$stackMultiChartSeries, "individualStack")) {
                "stack0"
              } else {
                as.character(i)
              },
              stepped = identical(currentView$chartOptions$multiChartOptions$multiChartStepPlot, TRUE)
            )
          } else {
            if (identical(pivotRenderer, "stackedarea")) {
              fillOpacity <- if (length(currentView$chartOptions$fillOpacity)) {
                currentView$chartOptions$fillOpacity
              } else {
                1
              }
            } else if (identical(pivotRenderer, "area")) {
              fillOpacity <- if (length(currentView$chartOptions$fillOpacity)) {
                currentView$chartOptions$fillOpacity
              } else {
                0.15
              }
            } else {
              fillOpacity <- 0.15
            }
            chartJsObj <- cjsSeries(chartJsObj, dataTmp[[rowHeaderLen + i]],
              label = label,
              fill = pivotRenderer %in% c("area", "stackedarea"),
              fillOpacity = fillOpacity,
              order = 1,
              scaleID = scaleID,
              stack = if (pivotRenderer %in% c("stackedarea", "stackedbar", "horizontalstackedbar")) "stack1" else NULL,
              stepped = identical(currentView$chartOptions$stepPlot, TRUE)
            )
          }
        }
        if (length(currentView$chartOptions)) {
          # reset chart options
          rendererEnv[[ns("chartOptions")]] <- currentView$chartOptions
          currentView$chartOptions <<- NULL
        }

        hideEl(session, paste0("#", ns("loadPivotTable")))
        chartJsObj$x$options$maintainAspectRatio <- FALSE
        if (length(currentView[["_didRender_"]])) {
          if (currentView[["_didRender_"]] < 2L) {
            setTextContent(session, paste0("#", ns("toggleViewButton")),
              lang$renderers$miroPivot$btLoadView,
              keepChildNodes = TRUE
            )
            currentView[["_didRender_"]] <<- 2L
          }
        } else {
          currentView[["_didRender_"]] <<- 1L
        }
        return(chartJsObj %>% cjsLegend())
      })

      # ===================================================
      #        TABLE RENDERER
      # ===================================================

      output$pivotTable <- renderDT({
        updateRenderer()
        pivotRenderer <- input$pivotRenderer
        if (!is.null(rendererEnv[[ns("chartOptions")]])) {
          # reset chart options
          rendererEnv[[ns("chartOptions")]] <- NULL
        }
        if (initRenderer && isTRUE(options$resetOnInit)) {
          if (length(currentView[["pivotRenderer"]])) {
            if (!identical(pivotRenderer, currentView[["pivotRenderer"]])) {
              return()
            }
            pivotRenderer <- currentView[["pivotRenderer"]]
          } else {
            pivotRenderer <- "table"
          }
          initRenderer <<- FALSE
        }
        dataTmp <- dataToRender()
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
          session, paste0("#", ns("pivotChart-container")),
          list(position = "absolute", top = "-2000pt")
        )
        if (!length(dataTmp)) {
          showEl(session, paste0("#", ns("noData")))
          return()
        }
        hideEl(session, paste0("#", ns("noData")))
        showEl(session, paste0("#", ns("loadPivotTable")))
        noRowHeaders <- attr(dataTmp, "noRowHeaders")
        if (isHeatmap) {
          brks <- quantile(dataTmp[-seq_len(noRowHeaders)],
            probs = seq(.05, .95, .05), na.rm = TRUE
          )
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
            {
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

        columnDefsTmp <- NULL
        if (isTRUE(options$enableHideEmptyCols) && hideEmptyCols()) {
          if (noRowHeaders > 0L) {
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
          }
        } else {
          hiddenEmptyCols <<- NULL
          if (isTRUE(options$enableHideEmptyCols)) {
            setAttributes(
              session, paste0("#", ns("rowIndexList .drop-index-item")),
              "data-col-hidden", "false"
            )
          }
        }

        fixedColumnsConfig <- list(leftColumns = noRowHeaders)
        colSummarySettings <- NULL

        if (tableSummarySettings()$rowEnabled) {
          if (identical(tableSummarySettings()$rowSummaryFunction, "sum")) {
            dataTmp <- mutate(
              ungroup(dataTmp),
              !!paste0(
                lang$renderers$miroPivot$aggregationFunctions$sum,
                "\U2003\U2003"
              ) := rowSums(dataTmp[vapply(dataTmp, is.numeric,
                logical(1L),
                USE.NAMES = FALSE
              )], na.rm = TRUE)
            )
          } else if (identical(tableSummarySettings()$rowSummaryFunction, "mean")) {
            dataTmp <- mutate(
              ungroup(dataTmp),
              !!paste0(
                lang$renderers$miroPivot$aggregationFunctions$mean,
                "\U2003\U2003"
              ) := rowMeans(dataTmp[vapply(dataTmp, is.numeric,
                logical(1L),
                USE.NAMES = FALSE
              )], na.rm = TRUE)
            )
          } else {
            # count
            dataTmp <- mutate(
              ungroup(dataTmp),
              !!paste0(
                lang$renderers$miroPivot$aggregationFunctions$count,
                "\U2003\U2003"
              ) := rowSums(!is.na(dataTmp[vapply(dataTmp, is.numeric,
                logical(1L),
                USE.NAMES = FALSE
              )]))
            )
          }
          fixedColumnsConfig$rightColumns <- 1
        }
        if (tableSummarySettings()$colEnabled) {
          colSummarySettings <- list(caption = lang$renderers$miroPivot$aggregationFunctions[[tableSummarySettings()$colSummaryFunction]])
          if (identical(tableSummarySettings()$colSummaryFunction, "count")) {
            colSummarySettings$data <- round(colSums(!is.na(dataTmp[vapply(dataTmp, is.numeric,
              logical(1L),
              USE.NAMES = FALSE
            )])), digits = roundPrecision)
          } else {
            colSummarySettings$data <- round(as.numeric(slice(summarise(dataTmp, across(
              where(is.numeric),
              \(x) match.fun(tableSummarySettings()$colSummaryFunction)(x, na.rm = TRUE)
            )), 1L)), digits = roundPrecision)
          }
        }

        ret <- datatable(dataTmp,
          extensions = c("Scroller", "FixedColumns"),
          selection = if (isEditableTable) "multiple" else "none", editable = isEditableTable,
          callback = JS("setTimeout(function() { table.draw(true); }, 500);"),
          title = currentView$chartOptions$title,
          container = DTbuildColHeaderContainer(
            names(dataTmp),
            noRowHeaders,
            unlist(setIndexAliases[names(dataTmp)[seq_len(noRowHeaders)]],
              use.names = FALSE
            ),
            colSummary = colSummarySettings
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
            scroller = list(loadingIndicator = FALSE), dom = "frtip",
            fixedColumns = if (nrow(dataTmp)) fixedColumnsConfig,
            columnDefs = columnDefsTmp
          ), rownames = FALSE
        )
        if (noRowHeaders < length(dataTmp)) {
          ret <- formatRound(ret, seq(noRowHeaders + 1, length(dataTmp)),
            digits = roundPrecision
          )
        }
        if (length(currentView$chartOptions)) {
          # reset chart options
          rendererEnv[[ns("chartOptions")]] <- currentView$chartOptions
          currentView$chartOptions <<- NULL
        }
        if (length(currentView[["_didRender_"]])) {
          if (currentView[["_didRender_"]] < 2L) {
            setTextContent(session, paste0("#", ns("toggleViewButton")),
              lang$renderers$miroPivot$btLoadView,
              keepChildNodes = TRUE
            )
            currentView[["_didRender_"]] <<- 2L
          }
        } else {
          currentView[["_didRender_"]] <<- 1L
        }
        if (!isHeatmap || noRowHeaders >= length(dataTmp)) {
          return(ret)
        }
        return(formatStyle(ret, seq(noRowHeaders + 1, length(dataTmp)),
          color = "#000",
          backgroundColor = styleInterval(brks, clrs)
        ))
      })

      if (isInput) {
        dtProxy <- dataTableProxy("pivotTable")

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
            rowsToRemove <- mutate(
              slice(dataToRender(), idsToRemove)[seq_len(noRowHeaders)],
              across(where(is.factor), as.character)
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
                      paste(
                        c(
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
              newData <- dfApplyChanges(dataToRender(), changesToApply)
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
              changesToApply <<- c(changesToApply, list(list(info$row, editedCol, editedVal)))
              replaceData(dtProxy, newData, resetPaging = FALSE, rownames = FALSE)

              newVal <- dataUpdated() + 1L
              dataUpdated(newVal)
              if (tableSummarySettings()$rowEnabled || tableSummarySettings()$colEnabled) {
                updateFilter(newUpdateFilterVal)
              }
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
                paste(
                  c(
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
