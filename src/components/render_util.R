dashboardMatchSeriesLabel <- function(key, label, exact = FALSE) {
  if (exact && key == label) {
    return(TRUE)
  }
  return(
    grepl(paste0("\u2024", key, "\u2024"), label, fixed = TRUE) ||
      startsWith(label, paste0(key, "\u2024")) ||
      endsWith(label, paste0("\u2024", key))
  )
}
dashboardNormalizeUserFilter <- function(uf, singleDropdown = NULL) {
  if (is.null(uf)) {
    return(NULL)
  }

  isNewFormat <- is.list(uf) && (
    length(uf) == 0 ||
      all(vapply(uf, function(it) is.list(it) && !is.null(it$dimension), logical(1)))
  )

  if (isNewFormat) {
    for (i in seq_along(uf)) {
      if (is.null(uf[[i]]$multiple)) {
        uf[[i]]$multiple <- !(length(singleDropdown) && uf[[i]]$dimension %in% singleDropdown)
      }
    }
    return(uf)
  }

  if (is.character(uf)) {
    return(lapply(uf, function(dim) {
      list(
        dimension = dim,
        multiple = !(length(singleDropdown) && dim %in% singleDropdown),
        selected = NULL,
        placeholder = "All"
      )
    }))
  }

  uf
}
dashboardPreprocessDataViewsConfig <- function(dataViewsConfig) {
  out <- lapply(names(dataViewsConfig), function(viewName) {
    dataViewConfig <- dataViewsConfig[[viewName]]

    if (!is.list(dataViewConfig)) {
      return(dataViewConfig)
    }

    uf <- dataViewConfig$userFilter

    # user filter from external dataView
    if (!is.null(uf) && is.character(uf) && length(uf) == 1 && uf %in% names(dataViewsConfig)) {
      dataViewConfig$.userFilterExternalSymbol <- uf
      uf <- dataViewsConfig[[uf]]$userFilter
    }

    dataViewConfig$userFilter <- dashboardNormalizeUserFilter(
      uf,
      singleDropdown = dataViewConfig$singleDropdown
    )

    dataViewConfig
  })

  names(out) <- names(dataViewsConfig)
  out
}
dashboardGetSelectedValues <- function(choices, selected, multiple) {
  if (is.null(selected)) {
    return(NULL)
  }

  effectiveChoices <- if (!isFALSE(multiple)) choices[unname(choices) != ""] else choices
  if (length(effectiveChoices) == 0) {
    return(NULL)
  }

  toIndex <- function(values, n) {
    if (is.numeric(values)) {
      return(as.integer(values))
    }
    if (is.character(values)) {
      map <- c(first = 1L, last = n)
      suppressWarnings(num <- as.integer(values))
      index <- ifelse(is.na(num) & (values %in% names(map)), map[values], num)
      return(as.integer(index))
    }
    integer(0)
  }

  vals <- NULL
  if (identical(selected$mode, "explicit")) {
    vals <- selected$values
    vals <- vals[vals %in% unname(effectiveChoices)]
  } else if (identical(selected$mode, "position")) {
    idx <- toIndex(selected$values, length(effectiveChoices))
    idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(effectiveChoices)]
    vals <- unname(effectiveChoices)[idx]
  }

  if (!isTRUE(multiple) && length(vals) > 1) vals <- vals[1]
  if (length(vals) == 0) NULL else vals
}
dashboardPrepareData <- function(config, viewData) {
  if (is.null(viewData)) {
    return(NULL)
  }

  dataTmp <- viewData

  filterIndexList <- names(config$filter)
  aggFilterIndexList <- names(config$aggregations)
  colFilterIndexList <- names(config$cols)
  filterIndexList <- c(filterIndexList, aggFilterIndexList, colFilterIndexList)

  filterElements <- vector("list", length(filterIndexList))
  names(filterElements) <- filterIndexList
  multiFilterIndices <- c()

  if (length(config$baselineComparison)) {
    baselineCompConfig <- config$baselineComparison
  } else {
    baselineCompConfig <- list()
  }
  baselineComp <- NULL

  for (filterIndex in filterIndexList) {
    filterElements[[filterIndex]] <- sort(unique(dataTmp[[filterIndex]]))
    optionId <- "filter"
    if (filterIndex %in% aggFilterIndexList) {
      optionId <- "aggregations"
    } else if (filterIndex %in% colFilterIndexList) {
      optionId <- "cols"
    }
    filterVal <- config[[optionId]][[filterIndex]]

    if (!any(filterVal %in% filterElements[[filterIndex]])) {
      if (length(filterVal)) {
        invalidFilters <- filterVal[!is.na(filterVal) & !(filterVal %in% filterElements[[filterIndex]])]
        flog.debug(paste0(
          "Dashboard: Some filters could not be applied because the values are not present in the data: ",
          paste(invalidFilters, collapse = ", ")
        ))
        return(list(data = dataTmp[0, ], warnings = lang$renderers$dashboard$noDataWarning))
      }
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
    }
    if (identical(filterIndex, baselineCompConfig$domain)) {
      baselineCompConfig$filterIndex <- filterIndex
      baselineCompConfig$filterVal <- filterVal
    } else {
      dataTmp <- dataTmp %>%
        filter(.data[[filterIndex]] %in% filterVal)
    }
  }


  rowIndexList <- config$rows
  aggregationFunction <- config$aggregationFunction
  if (is.null(rowIndexList)) {
    rowIndexList <- character(0)
  }
  rowIndexList <- c(
    rowIndexList,
    multiFilterIndices[!multiFilterIndices %in% c(aggFilterIndexList, colFilterIndexList)]
  )
  additionalIndicesToGroupBy <- NULL
  if (length(baselineCompConfig$filterIndex) &&
    !baselineCompConfig$filterIndex %in% rowIndexList) {
    additionalIndicesToGroupBy <- baselineCompConfig$filterIndex
  }
  valueColName <- names(dataTmp)[length(dataTmp)]
  if (length(aggFilterIndexList)) {
    if (identical(aggregationFunction, "")) {
      aggregationFunction <- "count"
    } else if (length(aggregationFunction) != 1L ||
      !aggregationFunction %in% c("sum", "count", "min", "max", "mean", "median", "sd")) {
      flog.warn(
        "Attempt to tamper with the app detected! User entered: '%s' as aggregation function.",
        aggregationFunction
      )
      stop("Attempt to tamper with the app detected!", call. = FALSE)
    }
    valueColName <- names(dataTmp)[length(dataTmp)]
    if (!identical(valueColName, "value")) {
      names(dataTmp)[length(dataTmp)] <- "value"
    }
    dataTmp <- dataTmp %>%
      group_by(!!!rlang::syms(c(rowIndexList, additionalIndicesToGroupBy, colFilterIndexList))) %>%
      summarise(value = !!rlang::parse_expr(
        if (identical(aggregationFunction, "count")) {
          "sum(!is.na(value))"
        } else {
          paste0(aggregationFunction, "(value, na.rm = TRUE)")
        }
      ), .groups = "drop_last") %>%
      ungroup()
    if (!identical(valueColName, "value")) {
      names(dataTmp)[length(dataTmp)] <- valueColName
    }
  }

  # apply custom labels
  if (length(config$chartOptions$customLabels)) {
    labelCols <- dataTmp[, vapply(dataTmp, class, character(1L), USE.NAMES = FALSE) == "character"]
    for (col in seq_len(length(labelCols))) {
      dataTmp[[col]] <- sapply(dataTmp[[col]], function(x) {
        if (x %in% names(config$chartOptions$customLabels)) {
          config$chartOptions$customLabels[[x]]
        } else {
          x
        }
      })
    }
  }

  if (length(baselineCompConfig)) {
    baselineCompRecord <- baselineCompConfig$record
    if (baselineCompRecord %in% names(config$chartOptions$customLabels)) {
      baselineCompRecord <- config$chartOptions$customLabels[[baselineCompRecord]]
    }
    baselineCompConfig$data <- dataTmp %>%
      filter(.data[[baselineCompConfig$domain]] == baselineCompRecord) %>%
      select(any_of(setdiff(
        c(rowIndexList, colFilterIndexList, valueColName),
        baselineCompConfig$domain
      ))) %>%
      rename(.baseline = value)
    if (length(baselineCompConfig$filterIndex)) {
      filterVal <- baselineCompConfig$filterVal
      customFilterValIds <- match(filterVal, names(config$chartOptions$customLabels))
      isCustomFilterVal <- !is.na(customFilterValIds)
      filterVal[isCustomFilterVal] <- config$chartOptions$customLabels[customFilterValIds[isCustomFilterVal]]
      dataTmp <- dataTmp %>%
        filter(.data[[baselineCompConfig$filterIndex]] %in% filterVal)
    }
  }

  dataTmp <- dataTmp %>% select(all_of(c(rowIndexList, colFilterIndexList, valueColName)))

  if (length(rowIndexList)) {
    if (length(config$chartOptions$customLabelsOrder)) {
      # apply custom labels order
      dataTmp <- dashboardApplyCustomLabelsOrder(
        dataTmp,
        length(rowIndexList),
        config$chartOptions$customLabelsOrder
      )
    } else {
      # alphabetical order
      dataTmp <- arrange(dataTmp, !!!rlang::syms(rowIndexList))
    }
  }

  userFilterData <- list()
  dims <- character(0)
  uf <- config$userFilter
  if (length(uf)) {
    dims <- unique(vapply(uf, function(f) f$dimension, character(1)))
    if (is.null(config$.userFilterExternalSymbol)) {
      for (dim in dims) {
        col <- dataTmp[[dim]]
        if (!is.null(col)) {
          userFilterData[[dim]] <- unique(col)
        } else {
          userFilterData[[dim]] <- character(0)
        }
      }
    }
  }

  # apply custom series order
  if (length(colFilterIndexList)) {
    pivotSpec <- build_wider_spec(
      dataTmp,
      names_from  = all_of(colFilterIndexList),
      values_from = all_of(valueColName),
      names_sep   = "\U2024",
      names_sort  = TRUE
    )
    if (length(config$chartOptions$customSeriesOrder)) {
      desiredSeriesOrder <- c(
        config$chartOptions$customSeriesOrder,
        setdiff(pivotSpec$.name, config$chartOptions$customSeriesOrder)
      )
      pivotSpec <- arrange(pivotSpec, factor(.name, levels = desiredSeriesOrder))
    }
  }
  if (length(baselineCompConfig)) {
    if (identical(length(baselineCompConfig$data), 1L)) {
      baselineCompDataTmp <- cross_join(dataTmp, baselineCompConfig$data)
    } else {
      baselineCompDataTmp <- dataTmp %>%
        left_join(baselineCompConfig$data, by = setdiff(names(baselineCompConfig$data), ".baseline"))
    }
    metricSuffix <- vector("character", length(baselineCompConfig$metrics))
    baselineComp <- list()
    for (metricsIdx in seq_along(baselineCompConfig$metrics)) {
      if (identical(metricsIdx, 1L)) {
        colNameTmp <- ".primary"
      } else {
        colNameTmp <- ".secondary"
      }
      metricSuffix[[metricsIdx]] <- ""
      if (identical(baselineCompConfig$metrics[[metricsIdx]], "percentage difference")) {
        baselineCompDataTmp <- mutate(baselineCompDataTmp, !!colNameTmp := (!!sym(valueColName) - .baseline) / .baseline * 100)
        metricSuffix[[metricsIdx]] <- "%"
      } else if (identical(baselineCompConfig$metrics[[metricsIdx]], "absolute difference")) {
        baselineCompDataTmp <- mutate(baselineCompDataTmp, !!colNameTmp := !!sym(valueColName) - .baseline)
      } else if (identical(baselineCompConfig$metrics[[metricsIdx]], "normalization")) {
        baselineCompDataTmp <- mutate(baselineCompDataTmp, !!colNameTmp := !!sym(valueColName) / .baseline)
      } else {
        baselineCompDataTmp <- mutate(baselineCompDataTmp, !!colNameTmp := !!sym(valueColName))
      }
      if (identical(metricsIdx, 1L)) {
        dataTmp <- select(baselineCompDataTmp, -all_of(c(".baseline", valueColName))) %>%
          rename(!!valueColName := .primary)
      } else {
        baselineCompDataTmp <- select(baselineCompDataTmp, -all_of(c(valueColName, ".baseline")))
        if (length(colFilterIndexList)) {
          # we want to avoid pivoting secondary metrics as well as pivoting has a large memory footprint
          # To avoid this, we need to keep track of how the order is affected when pivoting, though
          # The order is affected by two things:
          # 1) `names_sort=TRUE` and `names_sep="\U2024` changes order of pivoted columns
          # 2) missing data will be added when pivoting as the new table is dense
          # we account for those 2 things by 1) computing row and col levels and 2) calling `complete()`
          # to make long table dense as well. This allows us to only communicate array of secondary values
          # with DT and index into array using `rowIdx+(colIdx*noRows)` formula.
          colLevels <- pivotSpec %>%
            mutate(.col = row_number()) %>%
            select(all_of(c(".col", colFilterIndexList)))
          if (length(rowIndexList)) {
            rowLevels <- baselineCompDataTmp %>%
              distinct(across(all_of(rowIndexList))) %>%
              droplevels() %>%
              mutate(.row = row_number())
            baselineComp$secondaryData <- baselineCompDataTmp %>%
              complete(
                !!!rowLevels[rowIndexList],
                nesting(!!!rlang::syms(colFilterIndexList))
              ) %>%
              left_join(rowLevels, by = rowIndexList) %>%
              left_join(colLevels, by = colFilterIndexList) %>%
              mutate(.key = .row + (.col * nrow(rowLevels))) %>%
              arrange(.key) %>%
              select(any_of(c(dims, ".primary", ".secondary")))
          } else {
            baselineComp$secondaryData <- baselineCompDataTmp %>%
              complete(
                nesting(!!!rlang::syms(colFilterIndexList))
              ) %>%
              left_join(colLevels, by = colFilterIndexList) %>%
              arrange(.col) %>%
              select(any_of(c(dims, ".primary", ".secondary")))
          }
        } else {
          baselineComp$secondaryData <- select(
            baselineCompDataTmp,
            any_of(c(dims, ".primary", ".secondary"))
          )
        }
      }
    }
    baselineComp$metricSuffix <- metricSuffix
  }

  if (length(colFilterIndexList)) {
    # note that names_sep is not an ASCII full stop, but UNICODE U+2024
    tryCatch(
      {
        dataTmp <- dataTmp %>%
          pivot_wider_spec(
            pivotSpec,
            names_repair = "unique"
          )
      },
      warning = function(w) {
        if (grepl("list-cols", conditionMessage(w), fixed = TRUE)) {
          flog.trace("Dashboard configuration: Data contains duplicated keys and can therefore not be pivoted.")
          showErrorMsg(
            lang$renderers$miroPivot$errorTitle,
            lang$renderers$miroPivot$errPivotDuplicate
          )
        } else {
          flog.info(
            "Dashboard configuration: Unexpected warning while pivoting data. Error message: %s",
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
          "Dashboard configuration: Unexpected error while pivoting data. Error message: %s",
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
  if (length(baselineComp)) {
    attr(dataTmp, "baselineComp") <- baselineComp
  }
  for (filterName in names(userFilterData)) {
    attr(dataTmp, paste0("userFilterData_", filterName)) <- userFilterData[[filterName]]
  }
  return(list(data = dataTmp, warnings = NULL))
}
dashboardHeatmapColors <- function(symbolData, noRowHeaders, heatmaptype = 1L) {
  if (heatmaptype == 1L) {
    brks <- quantile(symbolData[-seq_len(as.numeric(noRowHeaders))],
      probs = seq(.05, .95, .05), na.rm = TRUE
    )
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {
        paste0("rgb(255,", ., ",", ., ")")
      }
    return(list(brks = brks, clrs = clrs))
  }

  # Exclude row headers
  relevantData <- symbolData[-seq_len(as.numeric(noRowHeaders))]

  # Separate positive and negative values
  positiveValues <- unlist(lapply(relevantData, function(col) col[col >= 0]), use.names = FALSE)
  negativeValues <- unlist(lapply(relevantData, function(col) col[col < 0]), use.names = FALSE)

  # Remove NA and get unique values
  positiveValues <- unique(positiveValues[!is.na(positiveValues)])
  negativeValues <- unique(negativeValues[!is.na(negativeValues)])

  # Determine extremes
  symbolLowestNegative <- if (length(negativeValues)) min(negativeValues) else if (length(positiveValues)) min(positiveValues) else 0
  symbolHighestPositive <- if (length(positiveValues)) max(positiveValues) else if (length(negativeValues)) max(negativeValues) else 0


  symbolAbsMax <- max(abs(symbolLowestNegative), abs(symbolHighestPositive))

  # Generate quantiles for positive and negative values separately
  symbolQuantilesPositive <- seq(0, symbolAbsMax, length.out = 10)
  symbolQuantilesNegative <- seq(-symbolAbsMax, 0, length.out = 10)

  brks <- unique(c(symbolQuantilesNegative, symbolQuantilesPositive))

  # Generate color values for positive values (shades of green)
  symbolPositiveColors <- round(seq(90, 50,
    length.out = length(symbolQuantilesPositive)
  ), 0) %>%
    {
      paste0("hsl(202,52%,", ., "%)")
    }

  # Generate color values for negative values (shades of red)
  symbolNegativeColors <- round(seq(50, 90,
    length.out = length(symbolQuantilesNegative)
  ), 0) %>%
    {
      paste0("hsl(34,90%,", ., "%)")
    }

  # Combine the positive and negative color sets
  clrs <- c(symbolNegativeColors, symbolPositiveColors)

  return(list(brks = brks, clrs = clrs))
}
dashboardApplyCustomLabelsOrder <- function(data, noRowHeaders, customLabelsOrder) {
  if (!is.list(customLabelsOrder)) {
    mergedCols <- paste0("\U2024", "mergedCols")
    orderCol <- paste0(mergedCols, "\U2024")
    orderTmpCol <- paste0(orderCol, "\U2024")
    orderTibble <- tibble(
      !!mergedCols := customLabelsOrder,
      !!orderTmpCol := seq_along(customLabelsOrder)
    )

    colsToUnite <- names(data)[1:noRowHeaders]
    data <- data %>%
      unite(!!mergedCols, all_of(colsToUnite), sep = "\U2024", remove = FALSE) %>%
      left_join(orderTibble, by = mergedCols) %>%
      mutate(!!orderCol := ifelse(is.na(!!sym(orderTmpCol)),
        suppressWarnings(max(!!sym(orderTmpCol), na.rm = TRUE)) + row_number(),
        !!sym(orderTmpCol)
      )) %>%
      arrange(!!sym(orderCol)) %>%
      select(-all_of(c(mergedCols, orderTmpCol, orderCol)))
  } else {
    for (col in names(customLabelsOrder)) {
      if (col %in% names(data)) {
        orderedValues <- customLabelsOrder[[col]]
        allValues <- unique(data[[col]])
        leftoverValues <- allValues[!allValues %in% orderedValues]
        finalLevels <- c(orderedValues, leftoverValues)

        tmpCol <- paste0(".tmp_sort_", col)
        data[[tmpCol]] <- factor(data[[col]], levels = finalLevels, ordered = TRUE)
      }
    }

    tmpCols <- paste0(".tmp_sort_", names(customLabelsOrder))

    data <- data %>%
      arrange(across(all_of(tmpCols)))

    data <- data %>%
      select(-all_of(tmpCols))
  }
  return(data)
}
dashboardDefaultColorPair <- function(i, globalPalette) {
  pairIndex <- 2 * i
  if (pairIndex - 1 <= length(globalPalette)) {
    return(globalPalette[(pairIndex - 1):pairIndex])
  } else {
    # (A) Recycle:
    recycleI <- ((i - 1) %% (length(globalPalette) / 2)) + 1
    return(globalPalette[(2 * recycleI - 1):(2 * recycleI)])
    # (B) Or fallback to a single default (e.g. gray):
    # return(c("#666","#666"))
  }
}
dashboardTransformLabels <- function(originalLabels, customLabels) {
  transformedLabels <- c()
  if (length(customLabels)) {
    transformedLabels <- vapply(originalLabels, function(label) {
      if (label %in% names(customLabels)) {
        return(customLabels[[label]])
      }
      labelsTmp <- strsplit(label, "\u2024")[[1]]
      labelMatch <- which(labelsTmp %in% names(customLabels))
      if (length(labelMatch)) {
        labelsTmp[labelMatch] <- unlist(customLabels[labelsTmp[labelMatch]])
      }
      return(paste(labelsTmp, collapse = "\u2024"))
    }, character(1L), USE.NAMES = FALSE)
  } else {
    transformedLabels <- originalLabels
  }
  return(transformedLabels)
}
dashboardRenderDataView <- function(dataViewsConfig, dataView, dataViews, userFilterChoices, userFilterDefaults, ns, filterWarnings) {
  # Build and return the UI for a dashboard section/data-view
  # (views that are visible when clicking on a value box).
  # For a section/data-view the function:
  #   - looks up every view's config in dataViewsConfig
  #   - for each view the function adds a column with:
  #     title, chart-type selector, download buttons,
  #     user-filter dropdowns, a DT table output and a ChartJS chart output
  # If the data is empty. a corresponding warning is shown

  chartChoices <- setNames(
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
  )
  viewList <- dataViews[[dataView]]
  if (is.null(names(viewList))) {
    viewList <- unlist(viewList, recursive = FALSE)
  }
  viewIds <- as.list(names(viewList))
  titleList <- viewList
  tags$div(
    class = "dashboard-section-wrapper",
    id = ns(paste0(dataView, "View")),
    lapply(seq_along(viewIds), function(i) {
      id <- viewIds[[i]]
      title <- titleList[[i]]

      if (is.list(dataViewsConfig[[id]])) {
        uf <- dataViewsConfig[[id]]$userFilter
        filterWarning <- filterWarnings[[id]]
        filterInputs <- list()
        inlineClass <- "one-inline"
        if (length(uf)) {
          dims <- vapply(uf, `[[`, character(1), "dimension")
          inlineClass <- if (length(uf) %% 2 == 0) "even-inline" else if (length(uf) == 1) "one-inline" else "odd-inline"

          if (is.null(dataViewsConfig[[id]]$.userFilterExternalSymbol)) {
            filterInputs <- lapply(seq_along(uf), function(i) {
              f <- uf[[i]]
              dim <- dims[i]

              choices <- userFilterChoices[[id]][[dim]]
              if (is.null(choices)) choices <- character(0)

              selected <- NULL
              if (!is.null(userFilterDefaults[[id]]) && length(userFilterDefaults[[id]][[dim]])) {
                selected <- userFilterDefaults[[id]][[dim]]
              }
              tags$div(
                class = paste("user-filter-dropdown user-filter", inlineClass),
                selectizeInput(
                  ns(paste0(id, "userFilter_", dim)),
                  label = if (!is.null(f$label)) f$label else NULL,
                  selected = selected,
                  choices = choices,
                  multiple = isTRUE(f$multiple),
                  width = "100%"
                )
              )
            })
          }
        }

        column(
          width = if (length(dataViewsConfig[[id]]$colWidth)) as.numeric(dataViewsConfig[[id]]$colWidth) else 12,
          class = if (!nzchar(title)) "add-margin",
          id = ns(paste0(id, "_wrapper")),
          if (nzchar(title)) {
            tags$h4(title, class = "highlight-block")
          },
          tags$div(
            style = "overflow:auto;",
            if (!(is.null(filterWarning) || !nchar(filterWarning))) {
              tags$div(
                class = "out-no-data",
                filterWarning
              )
            } else {
              tagList(
                tags$div(
                  class = "row table-chart-wide-widgets",
                  tags$div(
                    class = paste("user-filters", inlineClass),
                    do.call(tagList, filterInputs)
                  ),
                  tags$div(
                    class = paste("charttype-and-btn-wrapper", inlineClass),
                    tags$div(
                      class = "chart-type-dropdown",
                      selectizeInput(ns(paste0(id, "ChartType")),
                        label = NULL,
                        choices = chartChoices,
                        selected = dataViewsConfig[[id]]$pivotRenderer
                      )
                    ),
                    tags$div(
                      class = "dashboard-btn-wrapper",
                      tags$a(
                        id = ns(paste0(id, "DownloadCsv")),
                        class = "btn btn-default btn-custom pivot-btn-custom shiny-download-link dashboard-btn dashboard-btn-csv",
                        href = "",
                        target = "_blank",
                        download = NA,
                        tags$div(
                          tags$i(class = "fa fa-file-csv")
                        ),
                        title = lang$renderers$miroPivot$btDownloadCsv
                      ),
                      tags$a(
                        id = ns(paste0(id, "DownloadPng")),
                        class = "btn btn-default bt-export-canvas btn-custom pivot-btn-custom dashboard-btn dashboard-btn-png",
                        style = if (dataViewsConfig[[id]]$pivotRenderer %in% c("table", "heatmap")) "display:none;",
                        download = paste0(id, "Chart", ".png"),
                        href = "#",
                        `data-canvasid` = ns(paste0(id, "Chart")),
                        tags$div(
                          tags$i(class = "fa fa-file-image")
                        ),
                        title = lang$renderers$miroPivot$btDownloadPng
                      )
                    )
                  )
                ),
                tags$div(
                  class = "table-chart-wide-wrapper",
                  style = paste0("min-height: ", if (length(dataViewsConfig[[id]]$height)) dataViewsConfig[[id]]$height else "33vh"),
                  DT::DTOutput(ns(paste0(id, "Table"))),
                  tags$div(
                    id = ns(paste0(id, "ChartWrapper")), class = "dashboard-chart-wrapper",
                    style = paste0("height: ", if (length(dataViewsConfig[[id]]$height)) dataViewsConfig[[id]]$height else "33vh"),
                    chartjs::chartjsOutput(ns(paste0(id, "Chart")),
                      height = if (length(dataViewsConfig[[id]]$height)) dataViewsConfig[[id]]$height else "33vh"
                    )
                  )
                )
              )
            }
          )
        )
      } else {
        uiOutput(ns(id))
      }
    })
  )
}
dashboardGetData <- function(indicator, dashboardChartData, dataViewsConfig, selectedUserFilters) {
  noRowHeaders <- attr(dashboardChartData[[indicator]], "noRowHeaders")
  dataTmp <- dashboardChartData[[indicator]]

  # filter user selection
  if (length(dataViewsConfig[[indicator]]$userFilter)) {
    if (length(attr(dataTmp, "baselineComp"))) {
      secondaryData <- attr(dataTmp, "baselineComp")$secondaryData
    } else {
      secondaryData <- NULL
    }
    for (filterName in names(selectedUserFilters)[names(selectedUserFilters) %in% c(names(dataTmp)[seq_len(noRowHeaders)], names(dataViewsConfig[[indicator]]$cols))]) {
      filterEl <- selectedUserFilters[[filterName]]
      if (length(filterEl)) {
        filterColIdx <- match(filterName, names(dataViewsConfig[[indicator]]$cols))
        if (!is.na(filterColIdx)) {
          cols <- if (noRowHeaders == 0) names(dataTmp) else names(dataTmp)[-seq_len(noRowHeaders)]
          filterDimElements <- vapply(
            strsplit(cols, "\U2024", fixed = TRUE),
            "[[", character(1), filterColIdx
          )
          dataTmp <- dataTmp %>%
            select(
              seq_len(noRowHeaders),
              all_of(which(filterDimElements %in% filterEl) + noRowHeaders)
            )
        } else {
          dataTmp <- dataTmp %>%
            filter(!!rlang::sym(filterName) %in% filterEl)
        }
        if (length(secondaryData)) {
          secondaryData <- secondaryData %>%
            filter(!!rlang::sym(filterName) %in% filterEl)
        }
      }
    }
    if (length(secondaryData)) {
      attr(dataTmp, "baselineComp")$secondaryData <- secondaryData
    }
  }
  return(dataTmp)
}
getRendererConfig <- function(graphConfig) {
  rendererOptions <- graphConfig$options

  if (identical(tolower(graphConfig$outType), "datatable")) {
    graphOptions <- graphConfig$datatable
  } else {
    graphOptions <- graphConfig$graph
  }

  if (!length(graphOptions) &&
    identical(tolower(graphConfig$outType), "graph")) {
    graphOptions <- rendererOptions
  }

  list(
    graphOptions = graphOptions,
    rendererOptions = rendererOptions,
    graphTool = graphOptions$tool
  )
}
rendererUtilGetEvent <- function(configData, eventId) {
  # extracts event from configData
  #
  # args:
  # configData :     configuration dataframe
  # eventId    :     id of the event
  #
  # returns:
  # string with event information extracted from configData
  if (length(configData)) {
    idx <- match(tolower(eventId), tolower(configData[[1]]))
    if (is.na(idx)) {
      # index could not be found so return the string (fixed value)
      return(eventId)
    } else {
      return(configData[[3]][[idx]])
    }
  } else {
    # config data does not exist, so return string
    return(eventId)
  }
}
rendererUtilGetMarkerInfo <- function(data) {
  marker <- list(
    opacity = data$opacity,
    size = data$size,
    line = list(
      color = data$line$color,
      width = data$line$width
    )
  )
  if (length(data$color)) {
    marker$color <- data$color
  }
  if (length(data$symbol)) {
    marker$symbol <- data$symbol
  }
  return(marker)
}
rendererUtilParseLabel <- function(label, colNames) {
  if (!nchar(label)) {
    return(NULL)
  }
  label <- gsub("\\", "\\\\", label, fixed = TRUE)
  label <- gsub('"', '\\"', label, fixed = TRUE)
  for (colName in colNames) {
    label <- gsub(paste0("[", colName, "]"), paste0('",data[[\'', colName, '\']],"'),
      label,
      fixed = TRUE
    )
  }
  return(parse(text = paste0('paste0("', label, '")')))
}
rendererUtilIsColor <- function(x) {
  tryCatch(is.matrix(col2rgb(x)),
    error = function(e) FALSE
  )
}
resolveRendererData <- function(data) {
  dataTmp <- if (is.reactive(data)) {
    data()
  } else {
    data
  }

  if (is.null(dataTmp)) {
    return(NULL)
  }

  if (inherits(dataTmp, "data.frame")) {
    dataTmp <- type_convert(dataTmp, cols())
  }

  dataTmp
}

updateRendererFilterInput <- function(session, input, data, options) {
  if (!length(options$filter$col)) {
    return()
  }

  dataTmp <- resolveRendererData(data)

  if (is.null(dataTmp) || !options$filter$col %in% names(dataTmp)) {
    return()
  }

  choices <- dataTmp[[options$filter$col]]
  choices <- choices[!is.na(choices)]

  if (!length(choices)) {
    return()
  }

  if (isTRUE(options$filter$date)) {
    currentSelection <- isolate(input$data_filter)
    updateDateRangeInput(session, "data_filter",
      min = min(choices, na.rm = TRUE),
      max = max(choices, na.rm = TRUE),
      start = if (length(currentSelection)) currentSelection[1] else min(choices, na.rm = TRUE),
      end = if (length(currentSelection)) currentSelection[2] else max(choices, na.rm = TRUE)
    )
  } else {
    choices <- unique(choices)
    currentSelection <- isolate(input$data_filter)
    selected <- currentSelection[currentSelection %in% choices]

    if (!length(selected) && length(choices)) {
      selected <- choices[1]
    }

    updateSelectInput(session, "data_filter",
      choices = choices,
      selected = selected
    )
  }
}

filterRendererData <- function(dataTmp, input, options) {
  if (is.null(dataTmp) ||
    !length(options$filter$col) ||
    !length(input$data_filter) ||
    !options$filter$col %in% names(dataTmp)) {
    return(dataTmp)
  }

  if (isTRUE(options$filter$date)) {
    filterTmp <- as.POSIXct(input$data_filter)

    if (length(filterTmp) < 2L || any(is.na(filterTmp))) {
      return(dataTmp)
    }

    filterValues <- dataTmp[[options$filter$col]]

    keep <- !is.na(filterValues) &
      filterValues >= filterTmp[1] &
      filterValues <= filterTmp[2]

    dataTmp <- dataTmp[keep, , drop = FALSE]

    return(dataTmp)
  }

  dataTmp[
    dataTmp[[options$filter$col]] %in% input$data_filter, ,
    drop = FALSE
  ]
}
