AIToolAdapter <- R6Class("AIToolAdapter",
  public = list(
    initialize = function(dbSchema, conn, ioConfig) {
      private$dbSchema <- dbSchema
      private$conn <- conn
      private$ioConfig <- ioConfig
      return(self)
    },
    setAllowedSids = function(allowedSids) {
      private$allowedSids <- as.integer(allowedSids)[!is.na(as.integer(allowedSids))]
      return(self)
    },
    setScenIdNameMap = function(scenIdNameMap) {
      private$customscenIdNameMap <- scenIdNameMap
      return(invisible(self))
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

      allSymbols <- private$dbSchema$getDbTableNames()
      databaseSymbols <- list()
      if (scalarsOutName %in% names(private$ioConfig$modelOut)) {
        outputScalars <- private$ioConfig$modelOut[[scalarsOutName]]
      } else {
        outputScalars <- NULL
      }
      if (scalarsFileName %in% names(private$ioConfig$modelIn)) {
        inputScalars <- private$ioConfig$modelIn[[scalarsFileName]]
      } else {
        inputScalars <- NULL
      }

      for (sym in allSymbols) {
        cfg <- private$ioConfig$modelIn[[sym]]
        direction <- "input"
        if (is.null(cfg)) {
          cfg <- private$ioConfig$modelOut[[sym]]
          direction <- "output"
        }
        if (is.null(cfg)) {
          scalarId <- match(sym, outputScalars$symnames)
          if (!is.na(scalarId)) {
            cfg <- list(
              alias = outputScalars$symtext[[scalarId]],
              symtype = outputScalars$symtypes[[scalarId]],
              isScalarDs = TRUE
            )
            direction <- "output"
          } else {
            scalarId <- match(sym, inputScalars$symnames)
            if (!is.na(scalarId)) {
              cfg <- list(
                alias = inputScalars$symtext[[scalarId]],
                symtype = inputScalars$symtypes[[scalarId]],
                isScalarDs = TRUE
              )
              direction <- "input"
            } else {
              futile.logger::flog.error("Database symbol '%s' not found in either input or output symbols.", sym)
              next
            }
          }
        }

        symType <- private$getSymbolType(cfg$symtype %||% cfg$type %||% "unknown")

        columns <- list()
        if (is.null(cfg$headers)) {
          columns[[sym]] <- list(
            alias = cfg$alias,
            type = symType
          )
        } else {
          for (colName in names(cfg$headers)) {
            columns[[colName]] <- list(
              alias = cfg$headers[[colName]]$alias,
              type = cfg$headers[[colName]]$type
            )
          }
        }

        databaseSymbols[[sym]] <- list(
          alias = cfg$alias,
          isScalar = isTRUE(cfg$isScalarDs),
          direction = direction,
          columns = columns
        )
        widgetConfig <- tryCatch(private$getWidgetConfig(sym), error = function(err) {
          futile.logger::flog.warn("Could not determine widget config for symbol: %s. Error: %s", sym, conditionMessage(err))
          return(NULL)
        })
        if (length(widgetConfig)) {
          databaseSymbols[[sym]]$widget <- widgetConfig
        }
      }
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

      return(list(
        list(
          name = "queryScenarioDatabase",
          type = "function",
          description = "Queries the GAMS MIRO database. Use this to calculate metrics, sum values, compare scenarios, or filter rows.",
          parameters = list(
            type = "object",
            properties = propertiesList,
            required = requiredList
          )
        )
      ))
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
              metricExpr <- sprintf("CASE WHEN %s = 0 THEN NULL ELSE ((%s - %s) / %s * 100.0) END AS %s", baseExpr, targExpr, baseExpr, baseExpr, DBI::dbQuoteIdentifier(private$conn, "_pct_change"))
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

      # Map sids to scenario names
      if (length(private$allowedSids) > 1L) {
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
        return(toJSON(resDf, dataframe = "rows", auto_unbox = TRUE))
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

      return(toJSON(responseList, dataframe = "rows", auto_unbox = TRUE))
    }
  ),
  private = list(
    dbSchema = NULL,
    conn = NULL,
    ioConfig = NULL,
    allowedSids = NULL,
    customscenIdNameMap = character(),
    scenarioMap = NULL,
    scenarioMapById = NULL,
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
        if (!is.null(widgetConfig$dependencyConfig$fw)) {
          cleanedWidgetConfig$choices <- NULL
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
