#' ==========================================================================
#' Sandbox input data changes
#'
#' The AI assistant runs in a background `mirai` process that only has a
#' database connection, so it can never write to the sandbox itself. Instead it
#' *proposes* a change: `AIToolAdapter$modifySandboxInputData()` validates the
#' request, resolves it into the column/row indices of the symbol it targets and
#' queues it. The Shiny session then renders the proposal with a before/after
#' preview and, once the user confirms it, re-applies the very same resolved
#' change to the live sandbox data.
#'
#' The helpers below are what both sides run, so the preview the user confirms
#' and the data that ends up in the sandbox cannot drift apart.
#' ==========================================================================
AI_INPUT_CHANGE_OPERATIONS <- c("MULTIPLY", "DIVIDE", "ADD", "SUBTRACT", "SET")
AI_INPUT_CHANGE_FILTER_OPERATORS <- c("=", "!=", ">", ">=", "<", "<=", "IN")
AI_INPUT_CHANGE_PREVIEW_ROWS <- 50L
AI_INPUT_CHANGE_MODEL_PREVIEW_ROWS <- 10L
AI_MAX_LISTED_CHOICES <- 50L

deriveAiInputChangeSummary <- function(before, matchedRows, operation, value) {
  if (identical(operation, "SET")) {
    # SET also fills in rows whose value was missing
    return(list(count = matchedRows, sum = matchedRows * value, min = value, max = value))
  }
  if (!before$count) {
    return(list(count = 0L))
  }
  newBounds <- computeAiInputChangeValues(c(before$min, before$max), operation, value)
  return(list(
    count = before$count,
    sum = switch(operation,
      MULTIPLY = before$sum * value,
      DIVIDE = before$sum / value,
      ADD = before$sum + before$count * value,
      SUBTRACT = before$sum - before$count * value
    ),
    # a negative operand flips the order of the bounds
    min = min(newBounds), max = max(newBounds)
  ))
}

formatAiInputValue <- function(values) {
  if (!length(values)) {
    return(character(0L))
  }
  if (is.numeric(values)) {
    return(vapply(values, function(value) {
      if (is.na(value)) {
        return("NA")
      }
      return(format(value, trim = TRUE, digits = 10L))
    }, character(1L), USE.NAMES = FALSE))
  }
  formatted <- as.character(values)
  formatted[is.na(formatted)] <- "NA"
  return(formatted)
}

computeAiInputChangeValues <- function(values, operation, value) {
  return(switch(operation,
    MULTIPLY = values * value,
    DIVIDE = values / value,
    ADD = values + value,
    SUBTRACT = values - value,
    SET = rep(as.numeric(value), length(values)),
    stop(sprintf("Unsupported operation: '%s'.", operation), call. = FALSE)
  ))
}

#' Rows a resolved change applies to. An unevaluable comparison (a missing value,
#' a non-numeric entry in a `>`/`<` filter) never matches, so rows we cannot
#' reason about are left untouched rather than silently overwritten.
matchAiInputChangeRows <- function(data, filters) {
  noRows <- nrow(data)
  if (is.null(noRows)) {
    noRows <- length(data)
  }
  rowMask <- rep(TRUE, noRows)
  for (filterSpec in filters) {
    colValues <- data[[filterSpec$columnIdx]]
    if (identical(filterSpec$operator, "IN")) {
      targets <- trimws(strsplit(filterSpec$value, ",", fixed = TRUE)[[1L]])
      comparison <- as.character(colValues) %in% targets
    } else if (filterSpec$operator %in% c("=", "!=")) {
      numericTarget <- suppressWarnings(as.numeric(filterSpec$value))
      if (is.numeric(colValues) && !is.na(numericTarget)) {
        comparison <- colValues == numericTarget
      } else {
        comparison <- as.character(colValues) == filterSpec$value
      }
      if (identical(filterSpec$operator, "!=")) {
        comparison <- !comparison
      }
    } else {
      numericTarget <- suppressWarnings(as.numeric(filterSpec$value))
      numericValues <- suppressWarnings(as.numeric(colValues))
      comparison <- switch(filterSpec$operator,
        ">" = numericValues > numericTarget,
        ">=" = numericValues >= numericTarget,
        "<" = numericValues < numericTarget,
        "<=" = numericValues <= numericTarget
      )
    }
    comparison[is.na(comparison)] <- FALSE
    rowMask <- rowMask & comparison
  }
  return(rowMask)
}

applyAiInputChange <- function(data, change) {
  if (isTRUE(change$isChoice)) {
    oldValue <- NA_character_
    if (length(data)) {
      oldValue <- as.character(data)[[1L]]
    }
    return(list(
      data = change$value, rowMask = TRUE,
      oldValues = oldValue, newValues = change$value
    ))
  }
  if (isTRUE(change$isScalar)) {
    oldValues <- suppressWarnings(as.numeric(data))
    if (!length(oldValues)) {
      oldValues <- NA_real_
    }
    oldValues <- oldValues[[1L]]
    newValues <- computeAiInputChangeValues(oldValues, change$operation, change$value)
    return(list(
      data = newValues, rowMask = TRUE,
      oldValues = oldValues, newValues = newValues
    ))
  }
  colIdx <- change$targetColumnIdx
  if (is.null(nrow(data)) || colIdx > length(data)) {
    stop(sprintf(
      "Data of symbol '%s' does not have the expected %d columns.",
      change$symbolName, colIdx
    ), call. = FALSE)
  }
  rowMask <- matchAiInputChangeRows(data, change$filters)
  colValues <- suppressWarnings(as.numeric(data[[colIdx]]))
  oldValues <- colValues[rowMask]
  newValues <- computeAiInputChangeValues(oldValues, change$operation, change$value)
  colValues[rowMask] <- newValues
  data[[colIdx]] <- colValues
  return(list(
    data = data, rowMask = rowMask,
    oldValues = oldValues, newValues = newValues
  ))
}

buildAiInputChangePreview <- function(data, change, changeResult,
                                      maxRows = AI_INPUT_CHANGE_PREVIEW_ROWS) {
  if (isTRUE(change$isScalar) || isTRUE(change$isChoice)) {
    return(list(
      labels = NULL,
      before = formatAiInputValue(changeResult$oldValues),
      after = formatAiInputValue(changeResult$newValues),
      noRows = 1L, noRowsShown = 1L
    ))
  }
  affectedRows <- which(changeResult$rowMask)
  shownRows <- utils::head(affectedRows, maxRows)
  labelCols <- setdiff(seq_along(change$columnAliases), change$targetColumnIdx)
  labels <- NULL
  if (length(labelCols)) {
    labels <- as.data.frame(
      lapply(labelCols, function(colIdx) formatAiInputValue(data[[colIdx]][shownRows])),
      stringsAsFactors = FALSE, optional = TRUE
    )
    names(labels) <- change$columnAliases[labelCols]
  }
  return(list(
    labels = labels,
    before = formatAiInputValue(data[[change$targetColumnIdx]][shownRows]),
    after = formatAiInputValue(changeResult$data[[change$targetColumnIdx]][shownRows]),
    noRows = length(affectedRows), noRowsShown = length(shownRows)
  ))
}

AIToolAdapter <- R6Class("AIToolAdapter",
  public = list(
    initialize = function(dbSchema, conn, ioConfig, inputModificationEnabled = FALSE) {
      private$dbSchema <- dbSchema
      private$conn <- conn
      private$ioConfig <- ioConfig
      private$inputModificationEnabled <- isTRUE(inputModificationEnabled)
      # every symbol's description follows from the configuration alone, so it
      # is resolved once here instead of on each request and each lookup
      private$symbols <- private$describeSymbols()
      if (private$inputModificationEnabled) {
        # what may be changed follows from the columns, so it needs them resolved
        for (sym in names(private$symbols)) {
          if (identical(private$symbols[[sym]]$direction, "input")) {
            private$symbols[[sym]]$modifiable <- private$getModifiableInfo(sym)
          }
        }
      }
      return(self)
    },
    getPendingInputChanges = function() {
      return(private$pendingInputChanges)
    },
    setAllowedSids = function(allowedSids) {
      private$allowedSids <- as.integer(allowedSids)[!is.na(as.integer(allowedSids))]
      return(self)
    },
    setScenIdNameMap = function(scenIdNameMap) {
      private$customscenIdNameMap <- scenIdNameMap
      return(invisible(self))
    },
    describeToolCall = function(toolName, argumentsJson, resultJson) {
      if (!identical(toolName, "queryScenarioDatabase")) {
        return(NULL)
      }
      args <- tryCatch(
        {
          parsed <- if (is.character(argumentsJson)) jsonlite::fromJSON(argumentsJson) else argumentsJson
          if (!is.list(parsed)) NULL else parsed
        },
        error = function(e) NULL
      )
      if (is.null(args)) {
        return(NULL)
      }
      parsedResult <- tryCatch(jsonlite::fromJSON(resultJson, simplifyVector = TRUE), error = function(err) NULL)

      symName <- if (length(args$symbolName) == 1L && nzchar(args$symbolName)) as.character(args$symbolName) else NULL
      symbolAlias <- symName
      if (!is.null(symName) && length(private$symbols[[symName]]$alias) == 1L) {
        symbolAlias <- private$symbols[[symName]]$alias
      }

      filters <- args$filters
      if (!is.null(filters) && !is.data.frame(filters) && is.list(filters) &&
        all(c("column", "operator", "value") %in% names(filters))) {
        # A single filter object was sent instead of a one-element array; normalize it.
        filters <- as.data.frame(filters, stringsAsFactors = FALSE)
      }
      filterList <- NULL
      if (is.data.frame(filters) && nrow(filters)) {
        filterList <- lapply(seq_len(nrow(filters)), function(i) {
          list(
            column = as.character(filters$column[[i]]),
            operator = as.character(filters$operator[[i]]),
            value = as.character(filters$value[[i]])
          )
        })
      }

      errorMsg <- NULL
      warningMsg <- NULL
      rowCount <- NULL
      truncated <- FALSE
      if (!is.null(parsedResult)) {
        if (length(parsedResult$error) == 1L) {
          errorMsg <- as.character(parsedResult$error)
        } else if (!is.null(parsedResult$`_warning`)) {
          warningMsg <- as.character(parsedResult$`_warning`)
          truncated <- TRUE
          rowCount <- if (!is.null(parsedResult$data)) nrow(parsedResult$data) else 0L
        } else if (is.data.frame(parsedResult)) {
          rowCount <- nrow(parsedResult)
        } else if (is.list(parsedResult)) {
          # an empty result set parses to an empty (non-data-frame) list
          rowCount <- length(parsedResult)
        }
      }

      aggregation <- if (length(args$aggregation) == 1L) toupper(as.character(args$aggregation)) else NULL
      if (identical(aggregation, "NONE")) {
        aggregation <- NULL
      }
      analysisMode <- if (length(args$analysisMode) == 1L) toupper(as.character(args$analysisMode)) else NULL
      if (identical(analysisMode, "STANDARD")) {
        analysisMode <- NULL
      }
      groupByColumn <- if (length(args$groupByColumn)) as.character(unlist(args$groupByColumn, use.names = FALSE)) else NULL
      scenarioNames <- if (length(args$scenarioNames)) as.character(unlist(args$scenarioNames, use.names = FALSE)) else NULL
      orderBy <- if (length(args$orderBy) == 1L && nzchar(args$orderBy)) as.character(args$orderBy) else NULL
      sortDirection <- if (!is.null(orderBy) && length(args$sortDirection) == 1L) toupper(as.character(args$sortDirection)) else NULL
      limitVal <- if (length(args$limit) == 1L) suppressWarnings(as.integer(args$limit)) else NULL

      entry <- list(
        symbolName = symName,
        symbolAlias = symbolAlias,
        scenarioNames = scenarioNames,
        aggregation = aggregation,
        analysisMode = analysisMode,
        groupByColumn = groupByColumn,
        filters = filterList,
        orderBy = orderBy,
        sortDirection = sortDirection,
        limit = limitVal,
        error = errorMsg,
        warning = warningMsg,
        rowCount = rowCount,
        truncated = truncated
      )
      # drop absent fields entirely rather than keeping them as `NULL` --
      # otherwise a JSON round-trip turns them into `{}` instead of leaving
      # them out, which the renderer would then have to special-case
      return(Filter(Negate(is.null), entry))
    },
    getSystemMetadata = function() {
      scenMetaTable <- private$dbSchema$getDbTableName("_scenMeta")
      scenMetaSchema <- private$dbSchema$getDbSchema("_scenMeta")

      escMetaTable <- DBI::dbQuoteIdentifier(private$conn, scenMetaTable)
      escSidCol <- DBI::dbQuoteIdentifier(private$conn, scenMetaSchema$colNames[["sid"]])
      escSnameCol <- DBI::dbQuoteIdentifier(private$conn, scenMetaSchema$colNames[["sname"]])
      escStagCol <- DBI::dbQuoteIdentifier(private$conn, scenMetaSchema$colNames[["stag"]])

      metaQuery <- sprintf(
        "SELECT %s AS sid, %s AS name, %s AS tags FROM %s WHERE %s IN (%s)",
        escSidCol, escSnameCol, escStagCol, escMetaTable,
        escSidCol, paste(private$allowedSids, collapse = ",")
      )

      activeScenariosDf <- tryCatch(
        {
          DBI::dbGetQuery(private$conn, metaQuery)
        },
        error = function(e) {
          data.frame(sid = integer(0), name = character(0), tags = character(0))
        }
      )

      if (length(private$customscenIdNameMap)) {
        sidNameMapIds <- match(as.character(activeScenariosDf$sid), names(private$customscenIdNameMap))
        namesNeedRemapping <- !is.na(sidNameMapIds)
        if (any(namesNeedRemapping)) {
          activeScenariosDf$name[namesNeedRemapping] <- private$customscenIdNameMap[sidNameMapIds[namesNeedRemapping]]
        }
      }
      private$scenarioMap <- setNames(as.character(activeScenariosDf$sid), activeScenariosDf$name)
      private$scenarioMapById <- setNames(activeScenariosDf$name, as.character(activeScenariosDf$sid))

      databaseSymbols <- setNames(lapply(names(private$symbols), function(sym) {
        symInfo <- private$symbols[[sym]]
        databaseSymbol <- list(
          alias = symInfo$alias,
          isScalar = symInfo$isScalar,
          direction = symInfo$direction,
          columns = setNames(
            lapply(seq_along(symInfo$columns$names), function(colId) {
              column <- list(
                alias = symInfo$columns$aliases[[colId]],
                type = symInfo$columns$types[[colId]]
              )
              if (isTRUE(symInfo$columns$readonly[[colId]])) {
                column$readonly <- TRUE
              }
              return(column)
            }),
            symInfo$columns$names
          )
        )
        if (length(symInfo$widget)) {
          databaseSymbol$widget <- symInfo$widget
          if (length(symInfo$widget$choicesFrom)) {
            databaseSymbol$widget$choices <- private$listDependentChoices(sym)
          }
        }
        if (length(symInfo$modifiable)) {
          databaseSymbol$modifiable <- symInfo$modifiable
        }
        return(databaseSymbol)
      }), names(private$symbols))
      combinedContext <- list(
        activeScenarios = activeScenariosDf[, c("name", "tags"), drop = FALSE],
        databaseSymbols = databaseSymbols
      )

      return(jsonlite::toJSON(combinedContext, auto_unbox = TRUE, pretty = TRUE))
    },
    getToolSchema = function() {
      propertiesList <- list(
        symbolName = list(type = "string", description = "The technical symbol identifier string from the databaseSymbols layout (e.g., table names for parameters or scalars)."),
        selectColumn = list(type = "string", description = "Optional. The specific column to aggregate. CRITICAL: If you select ONLY a numeric value column during a standard query, you will lose the descriptive label columns (e.g., 'mac', 'gov', 'sector'). To retain context, you should OMIT this parameter to return all columns (equivalent to SELECT *)."),
        aggregation = list(type = "string", enum = c("SUM", "AVG", "MAX", "MIN", "COUNT", "NONE"), description = "Math aggregation to perform on the database level. Required (must be SUM, AVG, MAX, MIN, or COUNT -- not NONE) when analysisMode is DIFFERENCE or PERCENT_CHANGE."),
        groupByColumn = list(
          type = "array",
          items = list(type = "string"),
          description = "Optional. Up to 3 index columns used to group calculations, e.g. ['region', 'year']. CRITICAL: If you use groupByColumn, you MUST also provide a valid aggregation (e.g., SUM, MAX, AVG) and a selectColumn. Do not use groupByColumn with aggregation='NONE'. Any dimension of the symbol NOT listed here is aggregated across all of its values -- narrow it first with `filters` if you need one specific value instead of a sum/avg/etc. across all of them."
        ),
        scenarioNames = list(type = "array", items = list(type = "string"), description = "An array of scenario names pulled from the activeScenarios block matching the user's request."),
        filters = list(
          type = "array",
          description = "Optional. An array of filter conditions to apply to the rows.",
          items = list(
            type = "object",
            properties = list(
              column = list(
                type = "string",
                description = "The name of the column to filter on."
              ),
              operator = list(
                type = "string",
                enum = c("=", "!=", ">", "<", "LIKE", "IN"),
                description = "The SQL comparison operator."
              ),
              value = list(
                type = "string",
                description = "The value to filter by."
              )
            ),
            required = c("column", "operator", "value")
          )
        ),
        orderBy = list(
          type = "string",
          description = "Optional. The column to sort results by. CRITICAL: If analysisMode is DIFFERENCE, use '_delta'. If PERCENT_CHANGE, use '_pct_change'. For STANDARD mode aggregation, use the aggregation alias with a leading underscore (e.g., '_sum', '_max', '_count'). Otherwise, use the target column name."
        ),
        sortDirection = list(
          type = "string",
          enum = c("ASC", "DESC"),
          description = "Optional. Pairs with orderBy. Use DESC for finding highest values/bottlenecks, ASC for lowest."
        ),
        limit = list(
          type = "integer",
          description = "Optional. Maximum number of rows to return. Default is 50. Use a small limit (e.g., 5) if you just need to inspect data structure or find unique values."
        )
      )

      requiredList <- c("symbolName", "scenarioNames")

      # Conditionally add comparison features ONLY if >1 scenario exists
      if (length(private$allowedSids) > 1) {
        propertiesList$analysisMode <- list(
          type = "string",
          enum = c("STANDARD", "DIFFERENCE", "PERCENT_CHANGE"),
          description = "Optional. STANDARD returns normal values. DIFFERENCE calculates (Target - Baseline). PERCENT_CHANGE calculates ((Target - Baseline)/Baseline)*100. If scenarioNames contains more than one non-baseline scenario, one row (or one row per distinct combination of groupByColumn values) is returned per target scenario."
        )
        propertiesList$baselineScenarioName <- list(
          type = "string",
          description = "Required if analysisMode is DIFFERENCE or PERCENT_CHANGE. Defines which scenario name in scenarioNames acts as the baseline; every other scenario in scenarioNames is treated as a target scenario and compared against it."
        )
      } else {
        # If only 1 scenario, force the orderBy description to drop delta references
        propertiesList$orderBy$description <- "Optional. The column to sort results by. For aggregation, use the alias with a leading underscore (e.g., '_sum'). Otherwise, use the target column name."
      }

      toolSchema <- list(
        list(
          name = "queryScenarioDatabase",
          type = "function",
          description = "Queries the GAMS MIRO database. Use this to calculate metrics, sum values, compare scenarios, or filter rows. `null` is the actual value -- never substitute a neighboring column's value for it.",
          parameters = list(
            type = "object",
            properties = propertiesList,
            required = requiredList
          )
        )
      )
      if (private$inputModificationEnabled) {
        toolSchema[[length(toolSchema) + 1L]] <- private$getModifyInputToolSchema()
      }
      return(toolSchema)
    },
    modifySandboxInputData = function(argumentsJson) {
      if (!private$inputModificationEnabled) {
        return(private$toolError("Input data cannot be modified in the current context."))
      }
      if (length(private$allowedSids) != 1L) {
        return(private$toolError("Input data can only be modified when a single sandbox scenario is in context."))
      }
      args <- tryCatch(
        {
          parsed <- if (is.character(argumentsJson)) jsonlite::fromJSON(argumentsJson) else argumentsJson
          if (!is.list(parsed)) stop("Parsed arguments are not a JSON object.", call. = FALSE)
          parsed
        },
        error = function(e) NULL
      )
      if (is.null(args)) {
        return(private$toolError("Invalid JSON argument format."))
      }

      symName <- args$symbolName
      if (length(symName) != 1L || !is.character(symName) || !nzchar(symName)) {
        return(private$toolError("`symbolName` is required and must be a non-empty string."))
      }
      if (is.null(private$ioConfig$modelIn[[symName]])) {
        return(private$toolError(sprintf("'%s' is not an input symbol, so it cannot be modified.", symName)))
      }
      modifiableInfo <- private$getModifiableInfo(symName)
      if (is.null(modifiableInfo)) {
        return(private$toolError(sprintf(
          "Input symbol '%s' cannot be modified (it is readonly, its numeric columns are all readonly, it holds no numeric or single-choice value, or it is a widget type that does not support being changed). Only symbols with a `modifiable` entry in the database schema can be changed.",
          symName
        )))
      }
      isChoice <- identical(modifiableInfo$kind, "choice")
      operation <- if (length(args$operation) == 1L) toupper(as.character(args$operation)) else ""
      if (!operation %in% AI_INPUT_CHANGE_OPERATIONS) {
        return(private$toolError(sprintf(
          "`operation` is required and must be one of: %s.",
          paste(AI_INPUT_CHANGE_OPERATIONS, collapse = ", ")
        )))
      }
      symConfig <- private$ioConfig$modelIn[[symName]]
      symColumns <- private$getInputSymbolColumns(symName)
      isScalar <- !length(symConfig$headers)

      if (isChoice) {
        return(private$proposeInputChoiceChange(args, symName, symConfig, symColumns, operation))
      }

      value <- suppressWarnings(as.numeric(args$value))
      if (length(value) != 1L || is.na(value) || !is.finite(value)) {
        return(private$toolError("`value` is required and must be a single finite number."))
      }
      if (identical(operation, "DIVIDE") && identical(value, 0)) {
        return(private$toolError("Cannot divide by zero."))
      }

      if (isScalar) {
        if (length(args$filters)) {
          return(private$toolError(sprintf("'%s' holds a single value, so `filters` cannot be used.", symName)))
        }
        targetColumnIdx <- 1L
      } else {
        modifiableColumns <- paste(modifiableInfo$columns, collapse = ", ")
        targetColumn <- args$targetColumn
        if (length(targetColumn) != 1L || !is.character(targetColumn) || !nzchar(targetColumn)) {
          return(private$toolError(sprintf(
            "`targetColumn` is required for table symbol '%s'. Modifiable columns: %s.",
            symName, modifiableColumns
          )))
        }
        targetColumnIdx <- match(targetColumn, symColumns$names)
        if (is.na(targetColumnIdx)) {
          return(private$toolError(sprintf("Invalid `targetColumn` '%s' for symbol '%s'.", targetColumn, symName)))
        }
        if (isTRUE(symColumns$readonly[[targetColumnIdx]])) {
          return(private$toolError(sprintf(
            "Column '%s' of symbol '%s' is readonly and cannot be changed. Modifiable columns: %s.",
            targetColumn, symName, modifiableColumns
          )))
        }
        if (!identical(symColumns$types[[targetColumnIdx]], "numeric")) {
          return(private$toolError(sprintf(
            "Column '%s' of symbol '%s' is not numeric. Modifiable columns: %s.",
            targetColumn, symName, modifiableColumns
          )))
        }
      }

      filters <- private$resolveInputChangeFilters(args$filters, symColumns)
      if (is.character(filters)) {
        return(private$toolError(filters))
      }

      change <- list(
        changeId = private$generateChangeId(),
        symbolName = symName,
        symbolAlias = if (length(symConfig$alias)) symConfig$alias else symName,
        isScalar = isScalar,
        operation = operation,
        value = value,
        targetColumn = symColumns$names[[targetColumnIdx]],
        targetColumnIdx = targetColumnIdx,
        targetColumnAlias = symColumns$aliases[[targetColumnIdx]],
        columnAliases = symColumns$aliases,
        filters = filters,
        reason = if (length(args$reason) == 1L && is.character(args$reason)) args$reason else NULL
      )

      changeStats <- tryCatch(private$summarizeInputChangeInDb(symName, symColumns$names, change),
        error = function(err) {
          return(conditionMessage(err))
        }
      )
      if (is.character(changeStats)) {
        return(private$toolError(sprintf("Could not read current data of symbol '%s': %s", symName, changeStats)))
      }
      if (changeStats$matchedRows == 0L) {
        return(private$toolError(sprintf(
          "No row of symbol '%s' matches the given filters, so there is nothing to change. Query the symbol first to find out which values its index columns actually take.",
          symName
        )))
      }
      afterSummary <- deriveAiInputChangeSummary(
        changeStats$before, changeStats$matchedRows, operation, value
      )
      boundsError <- private$checkInputChangeBounds(symName, c(afterSummary$min, afterSummary$max))
      if (!is.null(boundsError)) {
        return(private$toolError(boundsError))
      }

      private$pendingInputChanges[[length(private$pendingInputChanges) + 1L]] <- change

      sampleBefore <- suppressWarnings(as.numeric(changeStats$sample[[targetColumnIdx]]))
      modelPreview <- data.frame(
        `_before` = formatAiInputValue(sampleBefore),
        `_after` = formatAiInputValue(computeAiInputChangeValues(sampleBefore, operation, value)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      labelCols <- setdiff(seq_along(symColumns$names), targetColumnIdx)
      if (length(labelCols)) {
        labels <- as.data.frame(
          lapply(labelCols, function(colIdx) formatAiInputValue(changeStats$sample[[colIdx]])),
          stringsAsFactors = FALSE, optional = TRUE
        )
        names(labels) <- symColumns$aliases[labelCols]
        modelPreview <- cbind(labels, modelPreview)
      }
      return(toJSON(list(
        status = "AWAITING_USER_CONFIRMATION",
        symbolName = symName,
        operation = operation,
        value = value,
        targetColumn = change$targetColumn,
        valuesAffected = changeStats$matchedRows,
        before = changeStats$before,
        after = afterSummary,
        preview = modelPreview,
        previewNote = if (changeStats$matchedRows > nrow(modelPreview)) {
          sprintf(
            "Only the first %d of %d affected rows are shown here.",
            nrow(modelPreview), changeStats$matchedRows
          )
        },
        note = "The change has NOT been applied. It is queued as a proposal that the user must confirm in the chat. Do not query the database to verify it and do not state that the data has changed; tell the user in one or two sentences what you proposed and that they need to confirm or discard it."
      ), dataframe = "rows", auto_unbox = TRUE, na = "null"))
    },
    queryScenarioDatabase = function(argumentsJson) {
      args <- tryCatch(
        {
          parsed <- if (is.character(argumentsJson)) jsonlite::fromJSON(argumentsJson) else argumentsJson
          if (!is.list(parsed)) stop("Parsed arguments are not a JSON object.", call. = FALSE)
          parsed
        },
        error = function(e) NULL
      )
      if (is.null(args)) {
        return(jsonlite::toJSON(list(error = "Invalid JSON argument format."), auto_unbox = TRUE))
      }

      scalarArgNames <- c(
        "selectColumn", "aggregation", "analysisMode",
        "baselineScenarioName", "orderBy", "sortDirection"
      )
      argLengths <- vapply(scalarArgNames, function(nm) length(args[[nm]]), integer(1))
      oversizedArgNames <- scalarArgNames[argLengths > 1]
      if (length(oversizedArgNames) > 0) {
        return(jsonlite::toJSON(list(error = sprintf("`%s` must be a single string value, not an array.", oversizedArgNames[[1]])), auto_unbox = TRUE))
      }

      symName <- args$symbolName
      if (is.null(symName) || length(symName) != 1 || !nzchar(symName)) {
        return(jsonlite::toJSON(list(error = "`symbolName` is required and must be a non-empty string."), auto_unbox = TRUE))
      }
      selectCol <- args$selectColumn
      groupByCol <- args$groupByColumn
      if (!is.null(groupByCol)) {
        groupByCol <- unique(as.character(groupByCol))
        groupByCol <- groupByCol[nzchar(groupByCol)]
        if (length(groupByCol) == 0) {
          groupByCol <- NULL
        } else if (length(groupByCol) > 3) {
          return(jsonlite::toJSON(list(error = "You may group by at most 3 columns at a time."), auto_unbox = TRUE))
        }
      }
      filters <- args$filters
      if (!is.null(filters) && !is.data.frame(filters) && is.list(filters) &&
        all(c("column", "operator", "value") %in% names(filters))) {
        # A single filter object was sent instead of a one-element array; normalize it.
        filters <- as.data.frame(filters, stringsAsFactors = FALSE)
      }
      requestedNames <- args$scenarioNames
      if (is.null(requestedNames) || length(requestedNames) == 0) {
        return(jsonlite::toJSON(list(error = "`scenarioNames` is required and must be a non-empty array."), auto_unbox = TRUE))
      }
      scenarioIds <- as.integer(private$scenarioMap[requestedNames])
      if (any(is.na(scenarioIds))) {
        return(jsonlite::toJSON(list(error = "Access Denied: Requested `scenarioNames` are not available or loaded in the current active session context."), auto_unbox = TRUE))
      }
      aggregation <- if (is.null(args$aggregation)) "NONE" else toupper(args$aggregation)
      analysisMode <- if (is.null(args$analysisMode)) "STANDARD" else toupper(args$analysisMode)
      if (!(analysisMode %in% c("STANDARD", "DIFFERENCE", "PERCENT_CHANGE"))) {
        return(jsonlite::toJSON(list(error = sprintf("Invalid analysisMode '%s'. Must be STANDARD, DIFFERENCE, or PERCENT_CHANGE.", analysisMode)), auto_unbox = TRUE))
      }
      baselineName <- args$baselineScenarioName
      baselineSid <- if (!is.null(baselineName) && baselineName != "") as.integer(private$scenarioMap[baselineName]) else NULL
      if (any(is.na(baselineSid))) {
        return(jsonlite::toJSON(list(error = "Access Denied: Requested `baselineScenarioName` is not available or loaded in the current active session context."), auto_unbox = TRUE))
      }
      orderBy <- args$orderBy
      sortDir <- if (is.null(args$sortDirection)) "ASC" else toupper(args$sortDirection)

      # Step A: Validate requested table/symbol exists
      if (!(symName %in% private$dbSchema$getDbTableNames())) {
        return(jsonlite::toJSON(list(error = sprintf("Symbol '%s' does not exist.", symName)), auto_unbox = TRUE))
      }

      # Step B: Secure Session Multi-Tenant Guardrail Intersect
      validatedSids <- intersect(scenarioIds, private$allowedSids)

      if (length(validatedSids) == 0) {
        return(jsonlite::toJSON(list(error = "Access Denied: Requested scenarios are not available or loaded in the current active session context."), auto_unbox = TRUE))
      }

      # Step C: Parse physical DB mapping schema details
      dbTable <- private$dbSchema$getDbTableName(symName)
      dbSchema <- private$dbSchema$getDbSchema(symName)
      validCols <- unname(dbSchema$colNames)
      if (symName %in% private$dbSchema$getDbTableNames()) {
        validCols <- c("_sid", validCols)
      }

      isDiffMode <- analysisMode %in% c("DIFFERENCE", "PERCENT_CHANGE")
      targetSids <- NULL

      # Whitelist validation check for projection column targeting
      if (is.null(selectCol) || selectCol == "" || selectCol == "NONE") {
        if (isDiffMode) {
          return(jsonlite::toJSON(list(error = "You must provide a specific selectColumn and aggregation to use DIFFERENCE or PERCENT_CHANGE."), auto_unbox = TRUE))
        }
        if (length(private$allowedSids) > 1L) {
          colsToSelect <- validCols
        } else {
          colsToSelect <- validCols[validCols != "_sid"]
        }
        selectExprs <- paste(DBI::dbQuoteIdentifier(private$conn, colsToSelect), collapse = ", ")
      } else {
        if (!selectCol %in% validCols) {
          return(jsonlite::toJSON(list(error = sprintf("Invalid column '%s' for symbol '%s'.", selectCol, symName)), auto_unbox = TRUE))
        }
        escSelect <- DBI::dbQuoteIdentifier(private$conn, selectCol)

        if (!isDiffMode) {
          selectExprs <- if (aggregation %in% c("SUM", "AVG", "MAX", "MIN", "COUNT")) {
            sprintf("%s(%s) AS %s", aggregation, escSelect, DBI::dbQuoteIdentifier(private$conn, paste0("_", tolower(aggregation))))
          } else {
            escSelect
          }
          if (length(private$allowedSids) > 1L) {
            selectExprs <- sprintf("%s, %s", DBI::dbQuoteIdentifier(private$conn, "_sid"), selectExprs)
          }
        } else {
          if (length(validatedSids) < 2) {
            return(jsonlite::toJSON(list(error = "Logic Error: You must provide at least two valid scenarioNames in the array to calculate a DIFFERENCE or PERCENT_CHANGE."), auto_unbox = TRUE))
          }
          if (is.null(baselineSid) || !(baselineSid %in% validatedSids)) {
            return(jsonlite::toJSON(list(error = "baselineScenarioName is required and must be in scenarioNames for DIFFERENCE/PERCENT_CHANGE."), auto_unbox = TRUE))
          }
          if (!(aggregation %in% c("SUM", "AVG", "MAX", "MIN", "COUNT"))) {
            return(jsonlite::toJSON(list(error = "You must provide a valid aggregation (SUM, AVG, MAX, MIN, or COUNT) to use DIFFERENCE or PERCENT_CHANGE."), auto_unbox = TRUE))
          }

          targetSids <- setdiff(validatedSids, baselineSid)
          if (length(targetSids) == 0) targetSids <- baselineSid

          aggFunc <- aggregation

          baseExpr <- sprintf("%s(CASE WHEN %s = %d THEN %s ELSE NULL END)", aggFunc, DBI::dbQuoteIdentifier(private$conn, "_sid"), baselineSid, escSelect)

          # One SELECT branch per target scenario; branches are combined with UNION ALL below.
          selectExprs <- vapply(targetSids, function(tSid) {
            targExpr <- sprintf("%s(CASE WHEN %s = %d THEN %s ELSE NULL END)", aggFunc, DBI::dbQuoteIdentifier(private$conn, "_sid"), tSid, escSelect)
            if (analysisMode == "DIFFERENCE") {
              metricExpr <- sprintf("(%s - %s) AS %s", targExpr, baseExpr, DBI::dbQuoteIdentifier(private$conn, "_delta"))
            } else {
              metricExpr <- sprintf("CASE WHEN %s = 0 THEN NULL ELSE (100.0 * (%s - %s) / %s) END AS %s", baseExpr, targExpr, baseExpr, baseExpr, DBI::dbQuoteIdentifier(private$conn, "_pct_change"))
            }
            sprintf("%d AS %s, %s", tSid, DBI::dbQuoteIdentifier(private$conn, "_target_sid"), metricExpr)
          }, character(1))
        }
      }

      # Step D: SQL Compiler Pipeline Initialization
      escTable <- DBI::dbQuoteIdentifier(private$conn, dbTable)

      whereClause <- sprintf("FROM %s WHERE 1=1", escTable)
      # Append our validated, session-scoped context identifiers
      whereClause <- sprintf("%s AND %s IN (%s)", whereClause, DBI::dbQuoteIdentifier(private$conn, "_sid"), paste(validatedSids, collapse = ","))

      # Step E: Parse and sanitize dynamic row-level filters array
      if (!is.null(filters) && length(filters) > 0) {
        if (!is.data.frame(filters)) {
          return(jsonlite::toJSON(list(error = "`filters` must be an array of {column, operator, value} objects."), auto_unbox = TRUE))
        }
        for (i in seq_len(nrow(filters))) {
          fCol <- filters$column[i]
          fOp <- filters$operator[i]
          fVal <- filters$value[i]

          if (!fCol %in% validCols) {
            return(jsonlite::toJSON(list(error = "Forbidden target filter column configuration."), auto_unbox = TRUE))
          }
          if (!fOp %in% c("=", "!=", ">", "<", "LIKE", "IN")) {
            return(jsonlite::toJSON(list(error = "Forbidden SQL evaluation operator usage."), auto_unbox = TRUE))
          }

          if (fOp == "IN") {
            val_vector <- trimws(unlist(strsplit(fVal, ",")))
            quoted_vals <- paste(vapply(val_vector, function(v) DBI::dbQuoteLiteral(private$conn, v), character(1)), collapse = ", ")
            whereClause <- sprintf("%s AND %s IN (%s)", whereClause, DBI::dbQuoteIdentifier(private$conn, fCol), quoted_vals)
          } else {
            whereClause <- sprintf("%s AND %s %s %s", whereClause, DBI::dbQuoteIdentifier(private$conn, fCol), fOp, DBI::dbQuoteLiteral(private$conn, fVal))
          }
        }
      }

      if (!is.null(groupByCol)) {
        if (aggregation == "NONE" && analysisMode == "STANDARD") {
          return(jsonlite::toJSON(list(error = "Logic Error: You provided a groupByColumn but no aggregation method. You must specify an aggregation like SUM, MAX, or COUNT when grouping data."), auto_unbox = TRUE))
        }
        invalidGroupByCols <- setdiff(groupByCol, validCols)
        if (length(invalidGroupByCols) > 0) {
          return(jsonlite::toJSON(list(error = sprintf("Invalid groupBy column(s) targeted: %s.", paste(invalidGroupByCols, collapse = ", "))), auto_unbox = TRUE))
        }
      }

      # Step F: Grouping evaluation closure
      groupingCols <- c()
      if (length(private$allowedSids) > 1L && analysisMode == "STANDARD" && aggregation != "NONE") {
        groupingCols <- c(groupingCols, DBI::dbQuoteIdentifier(private$conn, "_sid"))
      }
      if (!is.null(groupByCol)) {
        groupingCols <- c(groupingCols, DBI::dbQuoteIdentifier(private$conn, groupByCol))
      }
      groupByClause <- if (length(groupingCols) > 0) sprintf(" GROUP BY %s", paste(groupingCols, collapse = ", ")) else ""

      groupBySelect <- if (!is.null(groupByCol)) paste0(paste(DBI::dbQuoteIdentifier(private$conn, groupByCol), collapse = ", "), ", ") else ""

      # Each element of selectExprs becomes its own branch; DIFFERENCE/PERCENT_CHANGE with
      # multiple target scenarios yields one branch per target, combined via UNION ALL.
      branchQueries <- vapply(selectExprs, function(sel) {
        sprintf("SELECT %s%s %s%s", groupBySelect, sel, whereClause, groupByClause)
      }, character(1))

      query <- paste(branchQueries, collapse = " UNION ALL ")

      if (!is.null(orderBy) && orderBy != "") {
        aggAlias <- paste0("_", tolower(aggregation))
        allowedOrderCols <- c(validCols, aggAlias, "_delta", "_pct_change")

        if (!(orderBy %in% allowedOrderCols)) {
          return(jsonlite::toJSON(list(error = "Invalid orderBy column targeted."), auto_unbox = TRUE))
        }
        if (!sortDir %in% c("ASC", "DESC")) sortDir <- "ASC"

        query <- sprintf("%s ORDER BY %s %s", query, DBI::dbQuoteIdentifier(private$conn, orderBy), sortDir)
      }

      safeLimit <- max(1L, min(
        if (!is.null(args$limit) && is.numeric(args$limit)) as.integer(args$limit) else 50L,
        100L
      ))

      query <- sprintf("%s LIMIT %d", query, safeLimit + 1L)

      # Step G: Database evaluation execution step
      resDf <- tryCatch(
        {
          DBI::dbGetQuery(private$conn, query)
        },
        error = function(e) {
          return(data.frame(error = sprintf("Database Engine Interface Error: %s", e$message)))
        }
      )

      # A database failure is reported as a one-column `error` frame. Return it in the
      # same shape as every other error above, before the scenario-name mapping - that
      # mapping selects the query's own columns and would otherwise fail with
      # "undefined columns selected", masking the actual database error.
      if (identical(names(resDf), "error")) {
        return(jsonlite::toJSON(list(error = resDf$error[[1L]]), auto_unbox = TRUE))
      }

      # Map sids to scenario names. Skipped for an empty result.
      if (length(private$allowedSids) > 1L && nrow(resDf) > 0L) {
        if (analysisMode == "STANDARD") {
          if ("_sid" %in% names(resDf)) {
            mappedNames <- private$scenarioMapById[as.character(resDf$`_sid`)]
            resDf$`_scenario_name` <- ifelse(is.na(mappedNames), paste("Scenario", resDf$`_sid`), mappedNames)
            resDf$`_sid` <- NULL
            resDf <- resDf[, c("_scenario_name", setdiff(names(resDf), "_scenario_name")), drop = FALSE]
          }
        } else {
          if ("_target_sid" %in% names(resDf)) {
            targetNames <- private$scenarioMapById[as.character(resDf$`_target_sid`)]
            resDf$`_target_scenario` <- ifelse(is.na(targetNames), paste("Scenario", resDf$`_target_sid`), targetNames)
            resDf$`_target_sid` <- NULL
          }
          baseName <- private$scenarioMapById[as.character(baselineSid)]
          resDf$`_baseline_scenario` <- ifelse(is.na(baseName), paste("Scenario", baselineSid), baseName)
          resDf <- resDf[, c("_target_scenario", "_baseline_scenario", setdiff(names(resDf), c("_target_scenario", "_baseline_scenario"))), drop = FALSE]
        }
      }

      truncated <- FALSE
      if (nrow(resDf) > safeLimit) {
        resDf <- resDf[seq_len(safeLimit), , drop = FALSE]
        truncated <- TRUE
      }

      if (!truncated) {
        return(toJSON(resDf, dataframe = "rows", auto_unbox = TRUE, na = "null"))
      }

      warningMsg <- sprintf("WARNING: Results truncated to %d rows. Use tighter filters or limit.", safeLimit)
      missingTargets <- character(0)

      # With multiple target scenarios the truncated rows can come from only
      # some branches; call out any target scenario left with zero rows so it
      # isn't silently dropped from the response.
      if (isDiffMode && "_target_scenario" %in% names(resDf)) {
        requestedTargetNames <- unname(private$scenarioMapById[as.character(targetSids)])
        missingTargets <- setdiff(requestedTargetNames, unique(resDf$`_target_scenario`))
        if (length(missingTargets) > 0) {
          warningMsg <- sprintf(
            "%s The following target scenarios have no rows left in this truncated result: %s. Narrow your filters, request fewer scenarios per call, or increase `limit`.",
            warningMsg, paste(missingTargets, collapse = ", ")
          )
        }
      }

      responseList <- list(`_warning` = warningMsg, data = resDf)
      if (length(missingTargets) > 0) {
        responseList$`_missing_target_scenarios` <- missingTargets
      }

      return(toJSON(responseList, dataframe = "rows", auto_unbox = TRUE, na = "null"))
    }
  ),
  private = list(
    dbSchema = NULL,
    conn = NULL,
    ioConfig = NULL,
    symbols = NULL,
    inputModificationEnabled = FALSE,
    pendingInputChanges = list(),
    allowedSids = NULL,
    customscenIdNameMap = character(),
    scenarioMap = NULL,
    scenarioMapById = NULL,
    toolError = function(message) {
      return(jsonlite::toJSON(list(error = message), auto_unbox = TRUE))
    },
    generateChangeId = function() {
      return(paste0("aichg_", paste(sample(c(letters, 0:9), 12L, replace = TRUE), collapse = "")))
    },
    describeSymbols = function() {
      outputScalars <- private$ioConfig$modelOut[[scalarsOutName]]
      inputScalars <- private$ioConfig$modelIn[[scalarsFileName]]
      symbols <- list()
      for (sym in private$dbSchema$getDbTableNames()) {
        symConfig <- private$ioConfig$modelIn[[sym]]
        direction <- "input"
        if (is.null(symConfig)) {
          symConfig <- private$ioConfig$modelOut[[sym]]
          direction <- "output"
        }
        if (is.null(symConfig)) {
          # a scalar has no entry of its own; it is described by the scalars sheet
          scalarId <- match(sym, outputScalars$symnames)
          if (!is.na(scalarId)) {
            symConfig <- list(
              alias = outputScalars$symtext[[scalarId]],
              symtype = outputScalars$symtypes[[scalarId]], isScalarDs = TRUE
            )
            direction <- "output"
          } else {
            scalarId <- match(sym, inputScalars$symnames)
            if (is.na(scalarId)) {
              futile.logger::flog.error("Database symbol '%s' not found in either input or output symbols.", sym)
              next
            }
            symConfig <- list(
              alias = inputScalars$symtext[[scalarId]],
              symtype = inputScalars$symtypes[[scalarId]], isScalarDs = TRUE
            )
            direction <- "input"
          }
        }
        symbols[[sym]] <- list(
          alias = symConfig$alias,
          direction = direction,
          isScalar = isTRUE(symConfig$isScalarDs),
          columns = private$describeSymbolColumns(sym, symConfig),
          widget = tryCatch(private$getWidgetConfig(sym), error = function(err) {
            futile.logger::flog.warn(
              "Could not determine widget config for symbol: %s. Error: %s",
              sym, conditionMessage(err)
            )
            return(NULL)
          })
        )
      }
      return(symbols)
    },
    describeSymbolColumns = function(symName, symConfig) {
      symbolType <- function(type) {
        return(tryCatch(private$getSymbolType(type),
          error = function(err) {
            futile.logger::flog.warn("Unknown type: '%s' for symbol: %s.", type, symName)
            return("unknown")
          }
        ))
      }
      if (!length(symConfig$headers)) {
        return(list(
          names = symName,
          aliases = if (length(symConfig$alias)) symConfig$alias else symName,
          types = symbolType(symConfig$symtype %||% symConfig$type %||% "unknown"),
          readonly = isTRUE(symConfig$readonly) || isTRUE(symConfig$options$readonly)
        ))
      }
      colNames <- names(symConfig$headers)
      return(list(
        names = colNames,
        aliases = vapply(colNames, function(colName) {
          alias <- symConfig$headers[[colName]]$alias
          if (length(alias) == 1L && nzchar(alias)) {
            return(alias)
          }
          return(colName)
        }, character(1L), USE.NAMES = FALSE),
        types = vapply(colNames, function(colName) {
          return(symbolType(symConfig$headers[[colName]]$type %||% "string"))
        }, character(1L), USE.NAMES = FALSE),
        # a table's `readonlyCols` are resolved into the headers during init
        readonly = vapply(colNames, function(colName) {
          return(isTRUE(symConfig$headers[[colName]]$readonly))
        }, logical(1L), USE.NAMES = FALSE)
      ))
    },
    getInputSymbolColumns = function(symName) {
      return(private$symbols[[symName]]$columns)
    },
    isModifiableInputSymbol = function(symName) {
      if (identical(symName, scalarsFileName)) {
        # the scalars sheet is exposed as one symbol per scalar widget
        return(FALSE)
      }
      symConfig <- private$ioConfig$modelIn[[symName]]
      if (is.null(symConfig) || length(symConfig$definedByExternalSymbol)) {
        return(FALSE)
      }
      if (isTRUE(symConfig$readonly) || isTRUE(symConfig$options$readonly)) {
        return(FALSE)
      }
      widgetType <- symConfig$type
      if (length(widgetType) != 1L) {
        return(FALSE)
      }
      if (widgetType %in% c("hot", "dt", "custom")) {
        return(length(private$modifiableColumnsOf(symName)) > 0L)
      }
      if (widgetType %in% c("slider", "numericinput")) {
        # a range slider is stored as two separate `<sym>_lo`/`<sym>_up` scalars
        return(length(symConfig[[widgetType]]$default) < 2L)
      }
      if (identical(widgetType, "dropdown")) {
        # a multi-select dropdown holds a list of values rather than one choice.
        # A single-select one is one value, whether it is stored as a scalar or
        # (when it is backed by a GAMS set) as a one-column table.
        return(!isTRUE(symConfig$dropdown$multiple) &&
          length(private$getInputSymbolColumns(symName)$names) == 1L)
      }
      return(FALSE)
    },
    getModifiableInfo = function(symName) {
      if (!private$isModifiableInputSymbol(symName)) {
        return(NULL)
      }
      if (identical(private$ioConfig$modelIn[[symName]]$type, "dropdown")) {
        return(list(kind = "choice"))
      }
      # I() keeps a single column an array rather than a bare string
      return(list(kind = "numeric", columns = I(private$modifiableColumnsOf(symName))))
    },
    modifiableColumnsOf = function(symName) {
      symColumns <- private$getInputSymbolColumns(symName)
      if (!length(symColumns$names)) {
        return(character(0L))
      }
      return(symColumns$names[symColumns$types == "numeric" & !symColumns$readonly])
    },
    listDependentChoices = function(symName) {
      symChoices <- tryCatch(private$getInputSymbolChoices(symName)$choices,
        error = function(err) {
          futile.logger::flog.warn(
            "Could not resolve choices of symbol: '%s'. Error message: %s",
            symName, conditionMessage(err)
          )
          return(character(0L))
        }
      )
      if (!length(symChoices) || length(symChoices) > AI_MAX_LISTED_CHOICES) {
        return(NULL)
      }
      # I() keeps a single choice an array rather than a bare string
      return(I(symChoices))
    },
    getInputSymbolChoices = function(symName) {
      dropdownConfig <- private$ioConfig$modelIn[[symName]]$dropdown
      choices <- as.character(dropdownConfig$choices)
      aliases <- as.character(dropdownConfig$aliases)
      dependencies <- dropdownConfig$dependencyConfig$fw
      if (length(dependencies)) {
        choices <- as.character(dropdownConfig$dependencyConfig$staticChoices)
        aliases <- as.character(dropdownConfig$dependencyConfig$staticAliases)
        for (depSymName in names(dependencies)) {
          depColName <- dependencies[[depSymName]][[1L]]
          depColumns <- private$getInputSymbolColumns(depSymName)
          depColIdx <- match(depColName, depColumns$names)
          if (is.na(depColIdx)) {
            next
          }
          dbColNames <- setdiff(unname(private$dbSchema$getDbSchema(depSymName)$colNames), "_sid")
          if (length(dbColNames) != length(depColumns$names)) {
            next
          }
          depValues <- DBI::dbGetQuery(private$conn, sprintf(
            "SELECT DISTINCT %s FROM %s WHERE %s = %d",
            DBI::dbQuoteIdentifier(private$conn, dbColNames[[depColIdx]]),
            DBI::dbQuoteIdentifier(private$conn, private$dbSchema$getDbTableName(depSymName)),
            DBI::dbQuoteIdentifier(private$conn, "_sid"), private$allowedSids[[1L]]
          ))
          choices <- c(choices, as.character(depValues[[1L]]))
        }
        aliases <- character(0L)
      }
      choices <- unique(choices[!is.na(choices)])
      return(list(choices = choices, aliases = aliases))
    },
    proposeInputChoiceChange = function(args, symName, symConfig, symColumns, operation) {
      if (!identical(operation, "SET")) {
        return(private$toolError(sprintf(
          "'%s' holds a single choice, so only the SET operation is supported (arithmetic does not apply to it).",
          symName
        )))
      }
      newValue <- args$textValue
      if (length(newValue) != 1L || !is.character(newValue) || !nzchar(newValue)) {
        return(private$toolError(sprintf(
          "`textValue` is required for '%s' and must be the new selection as a single string (`value` is only used for numeric symbols).",
          symName
        )))
      }
      symChoices <- tryCatch(private$getInputSymbolChoices(symName),
        error = function(err) {
          return(conditionMessage(err))
        }
      )
      if (is.character(symChoices)) {
        return(private$toolError(sprintf("Could not determine the choices of '%s': %s", symName, symChoices)))
      }
      if (!newValue %in% symChoices$choices) {
        # a model that only saw the labels should not have to guess the values
        aliasIdx <- match(newValue, symChoices$aliases)
        if (!is.na(aliasIdx) && aliasIdx <= length(symChoices$choices)) {
          newValue <- symChoices$choices[[aliasIdx]]
        } else {
          return(private$toolError(sprintf(
            "'%s' is not one of the values '%s' accepts. Valid values: %s.",
            newValue, symName, paste(symChoices$choices, collapse = ", ")
          )))
        }
      }

      change <- list(
        changeId = private$generateChangeId(),
        symbolName = symName,
        symbolAlias = if (length(symConfig$alias)) symConfig$alias else symName,
        isScalar = TRUE, isChoice = TRUE,
        operation = "SET",
        value = newValue,
        targetColumn = symColumns$names[[1L]],
        targetColumnIdx = 1L,
        targetColumnAlias = symColumns$aliases[[1L]],
        columnAliases = symColumns$aliases,
        filters = list(),
        reason = if (length(args$reason) == 1L && is.character(args$reason)) args$reason else NULL
      )

      currentValue <- tryCatch(
        list(value = private$readInputChangeCurrentValue(symName, symColumns$names, change)),
        error = function(err) list(error = conditionMessage(err))
      )
      if (!is.null(currentValue$error)) {
        return(private$toolError(sprintf(
          "Could not read the current value of symbol '%s': %s", symName, currentValue$error
        )))
      }
      currentValue <- currentValue$value
      if (identical(currentValue, newValue)) {
        return(private$toolError(sprintf(
          "'%s' is already set to '%s', so there is nothing to change.", symName, newValue
        )))
      }
      private$pendingInputChanges[[length(private$pendingInputChanges) + 1L]] <- change

      return(toJSON(list(
        status = "AWAITING_USER_CONFIRMATION",
        symbolName = symName,
        operation = "SET",
        currentValue = currentValue,
        newValue = newValue,
        note = "The change has NOT been applied. It is queued as a proposal that the user must confirm in the chat. Do not query the database to verify it and do not state that the data has changed; tell the user in one or two sentences what you proposed and that they need to confirm or discard it."
      ), auto_unbox = TRUE, na = "null"))
    },
    readInputChangeCurrentValue = function(symName, colNames, change) {
      dbColNames <- setdiff(unname(private$dbSchema$getDbSchema(symName)$colNames), "_sid")
      if (length(dbColNames) != length(colNames)) {
        stop(sprintf(
          "database layout of symbol '%s' (%d columns) does not match its configuration (%d columns)",
          symName, length(dbColNames), length(colNames)
        ), call. = FALSE)
      }
      currentValue <- DBI::dbGetQuery(private$conn, sprintf(
        "SELECT %s FROM %s WHERE %s = %d LIMIT 1",
        DBI::dbQuoteIdentifier(private$conn, dbColNames[[change$targetColumnIdx]]),
        DBI::dbQuoteIdentifier(private$conn, private$dbSchema$getDbTableName(symName)),
        DBI::dbQuoteIdentifier(private$conn, "_sid"), private$allowedSids[[1L]]
      ))
      if (!nrow(currentValue)) {
        return(NA_character_)
      }
      return(as.character(currentValue[[1L]][[1L]]))
    },
    #' Keep proposals inside the bounds the widget enforces: a slider would
    #' silently clamp an out-of-range value, so the data the user confirmed and
    #' the data that ends up in the sandbox would differ.
    checkInputChangeBounds = function(symName, newValues) {
      symConfig <- private$ioConfig$modelIn[[symName]]
      widgetConfig <- symConfig[[symConfig$type]]
      newValues <- newValues[!is.na(newValues)]
      if (!length(newValues)) {
        return(NULL)
      }
      minValue <- widgetConfig$min
      maxValue <- widgetConfig$max
      if (is.numeric(minValue) && length(minValue) == 1L && min(newValues) < minValue) {
        return(sprintf(
          "The resulting value (%s) is below the minimum (%s) allowed for '%s'.",
          formatAiInputValue(min(newValues)), formatAiInputValue(minValue), symName
        ))
      }
      if (is.numeric(maxValue) && length(maxValue) == 1L && max(newValues) > maxValue) {
        return(sprintf(
          "The resulting value (%s) exceeds the maximum (%s) allowed for '%s'.",
          formatAiInputValue(max(newValues)), formatAiInputValue(maxValue), symName
        ))
      }
      return(NULL)
    },
    resolveInputChangeFilters = function(filters, symColumns) {
      if (is.null(filters) || length(filters) == 0L) {
        return(list())
      }
      if (!is.data.frame(filters) && is.list(filters) &&
        all(c("column", "operator", "value") %in% names(filters))) {
        # a single filter object was sent instead of a one-element array; normalize it
        filters <- as.data.frame(filters, stringsAsFactors = FALSE)
      }
      if (!is.data.frame(filters)) {
        return("`filters` must be an array of {column, operator, value} objects.")
      }
      resolved <- vector("list", nrow(filters))
      for (i in seq_len(nrow(filters))) {
        colIdx <- match(as.character(filters$column[i]), symColumns$names)
        if (is.na(colIdx)) {
          return(sprintf("Invalid filter column '%s'.", filters$column[i]))
        }
        operator <- as.character(filters$operator[i])
        if (!operator %in% AI_INPUT_CHANGE_FILTER_OPERATORS) {
          return(sprintf(
            "Invalid filter operator '%s'. Must be one of: %s.",
            operator, paste(AI_INPUT_CHANGE_FILTER_OPERATORS, collapse = ", ")
          ))
        }
        filterValue <- as.character(filters$value[i])
        isNumericCol <- identical(symColumns$types[[colIdx]], "numeric")
        if (operator %in% c(">", ">=", "<", "<=") && !isNumericCol) {
          return(sprintf("Operator '%s' requires a numeric filter column, but '%s' is not numeric.", operator, symColumns$names[[colIdx]]))
        }
        if (isNumericCol) {
          # comparing a numeric column against text matches nothing in R and is
          # a type error in some databases, so it is rejected up front
          numericValues <- suppressWarnings(as.numeric(
            if (identical(operator, "IN")) trimws(strsplit(filterValue, ",", fixed = TRUE)[[1L]]) else filterValue
          ))
          if (!length(numericValues) || anyNA(numericValues)) {
            return(sprintf(
              "Filter value '%s' is not a number, but column '%s' is numeric.",
              filterValue, symColumns$names[[colIdx]]
            ))
          }
        }
        resolved[[i]] <- list(
          column = symColumns$names[[colIdx]],
          columnAlias = symColumns$aliases[[colIdx]],
          isNumericColumn = isNumericCol,
          columnIdx = colIdx,
          operator = operator,
          value = filterValue
        )
      }
      return(resolved)
    },
    buildInputChangeWhereClause = function(change, dbColNames) {
      whereClause <- sprintf(
        "%s = %d",
        DBI::dbQuoteIdentifier(private$conn, "_sid"), private$allowedSids[[1L]]
      )
      for (filterSpec in change$filters) {
        escCol <- DBI::dbQuoteIdentifier(private$conn, dbColNames[[filterSpec$columnIdx]])
        quoteValue <- function(value) {
          if (isTRUE(filterSpec$isNumericColumn)) {
            return(DBI::dbQuoteLiteral(private$conn, as.numeric(value)))
          }
          return(DBI::dbQuoteLiteral(private$conn, value))
        }
        if (identical(filterSpec$operator, "IN")) {
          targets <- trimws(strsplit(filterSpec$value, ",", fixed = TRUE)[[1L]])
          whereClause <- sprintf(
            "%s AND %s IN (%s)", whereClause, escCol,
            paste(vapply(targets, quoteValue, character(1L), USE.NAMES = FALSE), collapse = ", ")
          )
        } else {
          whereClause <- sprintf(
            "%s AND %s %s %s", whereClause, escCol,
            filterSpec$operator, quoteValue(filterSpec$value)
          )
        }
      }
      return(whereClause)
    },
    summarizeInputChangeInDb = function(symName, colNames, change) {
      dbColNames <- setdiff(unname(private$dbSchema$getDbSchema(symName)$colNames), "_sid")
      if (length(dbColNames) != length(colNames)) {
        stop(sprintf(
          "database layout of symbol '%s' (%d columns) does not match its configuration (%d columns)",
          symName, length(dbColNames), length(colNames)
        ), call. = FALSE)
      }
      escTable <- DBI::dbQuoteIdentifier(private$conn, private$dbSchema$getDbTableName(symName))
      escTarget <- DBI::dbQuoteIdentifier(private$conn, dbColNames[[change$targetColumnIdx]])
      whereClause <- private$buildInputChangeWhereClause(change, dbColNames)

      stats <- DBI::dbGetQuery(private$conn, sprintf(
        "SELECT COUNT(*) AS matched_rows, COUNT(%1$s) AS value_count, SUM(%1$s) AS value_sum, MIN(%1$s) AS value_min, MAX(%1$s) AS value_max FROM %2$s WHERE %3$s",
        escTarget, escTable, whereClause
      ))
      sample <- DBI::dbGetQuery(private$conn, sprintf(
        "SELECT %s FROM %s WHERE %s LIMIT %d",
        paste(DBI::dbQuoteIdentifier(private$conn, dbColNames), collapse = ", "),
        escTable, whereClause, AI_INPUT_CHANGE_MODEL_PREVIEW_ROWS
      ))
      names(sample) <- colNames

      valueCount <- as.integer(stats$value_count[[1L]])
      before <- list(count = valueCount)
      if (valueCount > 0L) {
        before <- list(
          count = valueCount, sum = as.numeric(stats$value_sum[[1L]]),
          min = as.numeric(stats$value_min[[1L]]), max = as.numeric(stats$value_max[[1L]])
        )
      }
      return(list(
        matchedRows = as.integer(stats$matched_rows[[1L]]),
        before = before, sample = sample
      ))
    },
    getModifyInputToolSchema = function() {
      return(list(
        name = "modifySandboxInputData",
        type = "function",
        description = "Proposes a change to the sandbox scenario's INPUT data: arithmetic on a numeric column, or a new selection for a symbol holding one choice (a dropdown). The change is NOT applied; it is queued as a proposal the user confirms in the chat, seeing exact before/after values first. Only symbols with a `modifiable` entry in the database schema can be changed. One call per column.",
        parameters = list(
          type = "object",
          properties = list(
            symbolName = list(
              type = "string",
              description = "Technical identifier of the input symbol. Must have a `modifiable` entry in the database schema."
            ),
            targetColumn = list(
              type = "string",
              description = "The numeric column to change; must be listed in that symbol's `modifiable.columns`. Omit for symbols holding a single value (sliders, numeric inputs, dropdowns)."
            ),
            operation = list(
              type = "string",
              enum = AI_INPUT_CHANGE_OPERATIONS,
              description = "MULTIPLY/DIVIDE/ADD/SUBTRACT combine `value` with the cell's current value; SET replaces it. Prefer the relative operations to preserve the user's numbers; use SET only when the user asked for an absolute value. `modifiable.kind: \"choice\"` supports SET only."
            ),
            value = list(
              type = "number",
              description = "The numeric operand, e.g. 1.1 with MULTIPLY for a 10% increase. Required for `modifiable.kind: \"numeric\"`."
            ),
            textValue = list(
              type = "string",
              description = "New selection for `modifiable.kind: \"choice\"` symbols (instead of `value`, always with SET). Must be one of that symbol's `widget.choices`. If the schema lists none, `widget.choicesFrom` names the symbol and column the valid values come from -- query that column for them."
            ),
            filters = list(
              type = "array",
              description = "Optional. Restricts the change to matching rows; WITHOUT filters EVERY row of the column is changed. Not allowed for symbols holding a single value. Rows whose filter column is empty never match.",
              items = list(
                type = "object",
                properties = list(
                  column = list(type = "string", description = "Column to filter on."),
                  operator = list(
                    type = "string",
                    enum = AI_INPUT_CHANGE_FILTER_OPERATORS,
                    description = "`IN` takes a comma-separated list of values. The ordering operators require a numeric column."
                  ),
                  value = list(type = "string", description = "Value to compare against.")
                ),
                required = c("column", "operator", "value")
              )
            ),
            reason = list(
              type = "string",
              description = "One short sentence, shown on the confirmation card, saying why the change is proposed."
            )
          ),
          required = c("symbolName", "operation")
        )
      ))
    },
    getSymbolType = function(symType) {
      if (symType %in% c("dropdown", "dropdowne", "set", "date", "daterange", "custom", "hot", "dt", "string", "textinput")) {
        return("string")
      }
      if (symType %in% c("slider", "parameter", "numeric", "checkbox", "variable", "equation", "numericinput")) {
        return("numeric")
      }
      stop(sprintf("Unknown type: %s", symType), call. = FALSE)
    },
    getWidgetConfig = function(sym) {
      if (!sym %in% names(private$ioConfig$modelIn)) {
        return(NULL)
      }
      widgetConfig <- private$ioConfig$modelIn[[sym]]
      widgetType <- widgetConfig$type
      if (!length(widgetType) || widgetType %in% c("custom", "hot", "dt")) {
        return(list(type = "table", readonly = isTRUE(widgetConfig$readonly) || isTRUE(widgetConfig$options$readonly)))
      }
      cleanedWidgetConfig <- list(type = widgetType)
      widgetConfig <- widgetConfig[[widgetType]]
      filters <- c("label", "tooltip", "min", "max", "multiple", "step", "choices", "format", "daysofweekdisabled", "datesdisabled", "placeholder")
      for (key in filters) {
        if (length(widgetConfig[[key]])) {
          cleanedWidgetConfig[[key]] <- widgetConfig[[key]]
        }
      }
      if (identical(widgetType, "dropdown")) {
        cleanedWidgetConfig$multiple <- isTRUE(widgetConfig$multiple)
        dependencies <- widgetConfig$dependencyConfig$fw
        if (length(dependencies)) {
          # the configured choices are only the static ones; the rest follow
          # another symbol's data, so name the symbol and column they come from
          cleanedWidgetConfig$choices <- NULL
          cleanedWidgetConfig$choicesFrom <- unname(lapply(names(dependencies), function(depSymName) {
            return(list(symbol = depSymName, column = dependencies[[depSymName]][[1L]]))
          }))
        }
      } else if (identical(widgetType, "slider")) {
        for (key in c("min", "max", "step")) {
          if (!is.numeric(cleanedWidgetConfig[[key]])) {
            cleanedWidgetConfig[[key]] <- NULL
          }
        }
      }
      return(cleanedWidgetConfig)
    }
  )
)
