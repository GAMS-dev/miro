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
      private$scenIdNameMap <- scenIdNameMap
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

      if (length(private$scenIdNameMap)) {
        sidNameMapIds <- match(as.character(activeScenariosDf$sid), names(private$scenIdNameMap))
        namesNeedRemapping <- !is.na(sidNameMapIds)
        if (any(namesNeedRemapping)) {
          activeScenariosDf$name[namesNeedRemapping] <- private$scenIdNameMap[sidNameMapIds[namesNeedRemapping]]
        }
      }

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
              flog.error("Database symbol %s not found in either input or output symbols.", sym)
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
      }
      combinedContext <- list(
        activeScenarios = activeScenariosDf,
        databaseSymbols = databaseSymbols
      )

      return(jsonlite::toJSON(combinedContext, auto_unbox = TRUE, pretty = TRUE))
    },
    getToolSchema = function() {
      return(list(
        list(
          name = "queryScenarioDatabase",
          type = "function",
          description = "Queries the GAMS MIRO database. Use this to calculate metrics, sum values, or filter rows across the selected scenarios.",
          parameters = list(
            type = "object",
            properties = list(
              symbolName = list(type = "string", description = "The technical symbol identifier string from the databaseSymbols layout (e.g., table names for parameters or scalars)."),
              selectColumn = list(type = "string", description = "Optional. The specific column to aggregate or extract. If omitted, ALL columns for this symbol will be returned (equivalent to SELECT *)."),
              aggregation = list(type = "string", enum = c("SUM", "AVG", "MAX", "MIN", "COUNT", "NONE"), description = "Math aggregation to perform on the database level."),
              groupByColumn = list(type = "string", description = "Optional index column used to group calculations. CRITICAL: If you use groupByColumn, you MUST also provide a valid aggregation (e.g., SUM, MAX, AVG) and a selectColumn. Do not use groupByColumn with aggregation='NONE'."),
              scenarioIds = list(type = "array", items = list(type = "integer"), description = "An array of internal numerical database sids pulled from the active_scenarios block matching the user's scenario request context."),
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
                description = "Optional. The column name to sort the results by (e.g., sorting by the value or aggregation column to find top elements)."
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
            ),
            required = c("symbolName", "scenarioIds")
          )
        )
      ))
    },
    queryScenarioDatabase = function(argumentsJson) {
      args <- tryCatch(
        {
          if (is.character(argumentsJson)) jsonlite::fromJSON(argumentsJson) else argumentsJson
        },
        error = function(e) {
          return(jsonlite::toJSON(list(error = "Invalid JSON argument format.")))
        }
      )

      symName <- args$symbolName
      selectCol <- args$selectColumn
      groupByCol <- args$groupByColumn
      filters <- args$filters
      scenarioIds <- args$scenarioIds
      aggregation <- if (is.null(args$aggregation)) "NONE" else toupper(args$aggregation)
      orderBy <- args$orderBy
      sortDir <- if (is.null(args$sortDirection)) "ASC" else toupper(args$sortDirection)

      # Step A: Validate requested table/symbol exists
      if (!(symName %in% private$dbSchema$getDbTableNames())) {
        return(jsonlite::toJSON(list(error = sprintf("Symbol '%s' does not exist.", symName)), auto_unbox = TRUE))
      }

      # Step B: Secure Session Multi-Tenant Guardrail Intersect
      cleanSids <- as.integer(scenarioIds)
      cleanSids <- scenarioIds[!is.na(scenarioIds)]
      validatedSids <- intersect(cleanSids, private$allowedSids)

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

      # Whitelist validation check for projection column targeting
      if (is.null(selectCol) || selectCol == "" || selectCol == "NONE") {
        colsToSelect <- validCols[validCols != "_sid"]
        selectExpr <- paste(DBI::dbQuoteIdentifier(private$conn, colsToSelect), collapse = ", ")
      } else {
        if (!selectCol %in% validCols) {
          return(jsonlite::toJSON(list(error = sprintf("Invalid column '%s' for symbol '%s'.", selectCol, symName)), auto_unbox = TRUE))
        }
        escSelect <- DBI::dbQuoteIdentifier(private$conn, selectCol)
        selectExpr <- if (aggregation %in% c("SUM", "AVG", "MAX", "MIN", "COUNT")) {
          sprintf("%s(%s) AS %s", aggregation, escSelect, DBI::dbQuoteIdentifier(private$conn, tolower(aggregation)))
        } else {
          escSelect
        }
      }

      # Step D: SQL Compiler Pipeline Initialization
      escTable <- DBI::dbQuoteIdentifier(private$conn, dbTable)

      query <- sprintf("SELECT %s", selectExpr)
      if (!is.null(groupByCol) && groupByCol != "") {
        if (aggregation == "NONE") {
          return(jsonlite::toJSON(list(error = "Logic Error: You provided a groupByColumn but no aggregation method. You must specify an aggregation like SUM, MAX, or COUNT when grouping data.")))
        }
        if (!(groupByCol %in% validCols)) {
          return(jsonlite::toJSON(list(error = "Invalid groupBy column targeted.")))
        }
        query <- sprintf("SELECT %s, %s", DBI::dbQuoteIdentifier(private$conn, groupByCol), selectExpr)
      }

      query <- sprintf("%s FROM %s WHERE 1=1", query, escTable)

      # Append our validated, session-scoped context identifiers
      query <- sprintf("%s AND %s IN (%s)", query, DBI::dbQuoteIdentifier(private$conn, "_sid"), paste(validatedSids, collapse = ","))

      # Step E: Parse and sanitize dynamic row-level filters array
      if (!is.null(filters) && length(filters) > 0) {
        for (i in seq_len(nrow(filters))) {
          fCol <- filters$column[i]
          fOp <- filters$operator[i]
          fVal <- filters$value[i]

          if (!fCol %in% validCols) {
            return(jsonlite::toJSON(list(error = "Forbidden target filter column configuration.")))
          }
          if (!fOp %in% c("=", "!=", ">", "<", "LIKE", "IN")) {
            return(jsonlite::toJSON(list(error = "Forbidden SQL evaluation operator usage.")))
          }

          if (fOp == "IN") {
            # Split string by commas, trim whitespace, and quote each element individually
            val_vector <- trimws(unlist(strsplit(fVal, ",")))
            quoted_vals <- paste(vapply(val_vector, function(v) DBI::dbQuoteLiteral(private$conn, v), character(1)), collapse = ", ")
            query <- sprintf("%s AND %s IN (%s)", query, DBI::dbQuoteIdentifier(private$conn, fCol), quoted_vals)
          } else {
            query <- sprintf("%s AND %s %s %s", query, DBI::dbQuoteIdentifier(private$conn, fCol), fOp, DBI::dbQuoteLiteral(private$conn, fVal))
          }
        }
      }

      # Step F: Grouping evaluation closure
      if (!is.null(groupByCol) && groupByCol != "") {
        query <- sprintf("%s GROUP BY %s", query, DBI::dbQuoteIdentifier(private$conn, groupByCol))
      }
      if (!is.null(orderBy) && orderBy != "") {
        aggAlias <- tolower(aggregation)
        if (!(orderBy %in% c(validCols, aggAlias))) {
          return(jsonlite::toJSON(list(error = "Invalid orderBy column targeted.")))
        }
        if (!sortDir %in% c("ASC", "DESC")) sortDir <- "ASC"

        query <- sprintf("%s ORDER BY %s %s", query, DBI::dbQuoteIdentifier(private$conn, orderBy), sortDir)
      }

      requestedLimit <- if (!is.null(args$limit) && is.numeric(args$limit)) as.integer(args$limit) else 50
      safeLimit <- min(requestedLimit, 100)

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

      truncated <- FALSE
      if (nrow(resDf) > safeLimit) {
        resDf <- resDf[seq_len(safeLimit), , drop = FALSE]
        truncated <- TRUE
      }

      resJson <- toJSON(resDf, dataframe = "rows", auto_unbox = TRUE)

      if (truncated) {
        warningMsg <- sprintf("WARNING: Results truncated to %d rows. Use tighter filters or limit.", safeLimit)
        resJson <- paste0('{"_warning": "', warningMsg, '", "data": ', resJson, "}")
      }

      return(resJson)
    }
  ),
  private = list(
    dbSchema = NULL,
    conn = NULL,
    ioConfig = NULL,
    allowedSids = NULL,
    scenIdNameMap = NULL,
    getSymbolType = function(symType) {
      if (symType %in% c("dropdown", "set", "date", "daterange", "custom", "hot", "dt", "string")) {
        return("string")
      }
      if (symType %in% c("slider", "parameter", "numeric", "checkbox")) {
        return("numeric")
      }
      stop(sprintf("Unknown type: %s", symType), call. = FALSE)
    }
  )
)
