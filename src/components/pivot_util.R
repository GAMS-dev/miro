matchSeriesLabel <- function(key, label, exact = FALSE) {
  if (exact) {
    return(key == label)
  }
  return(grepl(paste0("\u2024", key, "\u2024"), label, fixed = TRUE) ||
    startsWith(label, paste0(key, "\u2024")) || endsWith(label, paste0("\u2024", key)))
}
matchLabel <- function(key, label, exact = FALSE) {
  if (exact) {
    exact <- (key == label)
  }
  contained <- grepl(paste0("\u2024", key, "\u2024"), label)
  starts <- grepl(paste0("^", key, "\u2024"), label)
  ends <- grepl(paste0("\u2024", key, "$"), label)
  exact || contained || starts || ends
}
prepareData <- function(config, viewData, dataViewsConfigNames) {
  dataTmp <- viewData

  filterIndexList <- names(config$filter)
  aggFilterIndexList <- names(config$aggregations)
  colFilterIndexList <- names(config$cols)
  filterIndexList <- c(filterIndexList, aggFilterIndexList, colFilterIndexList)

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
    filterVal <- config[[optionId]][[filterIndex]]

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


  rowIndexList <- config$rows
  aggregationFunction <- config$aggregationFunction
  if (is.null(rowIndexList)) {
    rowIndexList <- character(0)
  }
  rowIndexList <- c(
    rowIndexList,
    multiFilterIndices[!multiFilterIndices %in% c(aggFilterIndexList, colFilterIndexList)]
  )
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
      group_by(!!!rlang::syms(c(rowIndexList, colFilterIndexList))) %>%
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
  }

  if (length(rowIndexList)) {
    dataTmp <- dataTmp %>%
      select(!!!c(rowIndexList, colFilterIndexList, valueColName)) %>%
      arrange(!!!rlang::syms(rowIndexList))
  } else {
    dataTmp <- dataTmp %>% select(!!!c(colFilterIndexList, valueColName))
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

  userFilterData <- list()

  if (length(config$userFilter) &&
    !(length(config$userFilter) == 1 && config$userFilter %in% dataViewsConfigNames)) {
    for (filter in config$userFilter) {
      userFilterData[[filter]] <- unique(dataTmp[[filter]])
    }
  }

  if (length(colFilterIndexList)) {
    # note that names_sep is not an ASCII full stop, but UNICODE U+2024
    tryCatch(
      {
        dataTmp <- dataTmp %>%
          pivot_wider(
            names_from = !!colFilterIndexList, values_from = !!valueColName,
            names_sep = "\U2024",
            names_sort = TRUE, names_repair = "unique"
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
  for (filterName in names(userFilterData)) {
    attr(dataTmp, paste0("userFilterData_", filterName)) <- userFilterData[[filterName]]
  }
  return(dataTmp)
}
heatmapColors <- function(symbolData, noRowHeaders, heatmaptype = 1L) {
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
applyCustomLabelsOrder <- function(data, noRowHeaders, customLabelsOrder) {
  if (!is.list(customLabelsOrder)) {
    mergedCols <- paste0("\U2024", "mergedCols")
    orderCol <- paste0(mergedCols, "\U2024")
    orderTmpCol <- paste0(orderCol, "\U2024")
    orderTibble <- tibble(
      !!mergedCols := customLabelsOrder,
      !!orderTmpCol := seq_along(customLabelsOrder)
    )

    data <- data %>%
      unite(!!mergedCols, 1:noRowHeaders, sep = "\U2024", remove = FALSE) %>%
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
# change the column order. Will change the order of table columns/plotted series
applyCustomSeriesOrder <- function(data, noRowHeaders, customSeriesOrder) {
  fixedCols <- colnames(data)[1:noRowHeaders]
  valueCols <- colnames(data)[(noRowHeaders + 1):ncol(data)]

  matchedCols <- character(0)
  for (desired in customSeriesOrder) {
    patternMatches <- sapply(valueCols, function(colName) {
      matchSeriesLabel(key = desired, label = colName, exact = TRUE)
    })

    matchedNow <- valueCols[patternMatches]
    matchedCols <- c(matchedCols, matchedNow)
    valueCols <- setdiff(valueCols, matchedNow)
  }

  orderedValueCols <- c(matchedCols, valueCols)

  orderedData <- data %>%
    select(all_of(fixedCols), all_of(orderedValueCols))

  return(orderedData)
}
defaultColorPair <- function(i, globalPalette) {
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
transformLabels <- function(originalLabels, customLabels) {
  transformedLabels <- c()
  if (length(customLabels)) {
    for (label in originalLabels) {
      transformedLabel <- label
      if (label %in% names(customLabels)) {
        transformedLabel <- customLabels[[label]]
      } else {
        labelsTmp <- strsplit(label, "\u2024")[[1]]
        labelMatch <- which(labelsTmp %in% names(customLabels))
        if (length(labelMatch)) {
          labelsTmp[labelMatch] <- unlist(customLabels[labelsTmp[labelMatch]])
        }
        transformedLabel <- paste(labelsTmp, collapse = "\u2024")
      }
      transformedLabels <- c(transformedLabels, transformedLabel)
    }
  } else {
    transformedLabels <- originalLabels
  }
  return(transformedLabels)
}
