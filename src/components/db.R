# R6 class for database related functions
Db <- R6Class("Db",
  public = list(
    initialize = function(uid, dbConf, slocktimeLimit, modelName,
                          hcubeActive = FALSE,
                          ugroups = character(0L), forceNew = FALSE) {
      # Initialize database class
      #
      # Args:
      #   dbConf:              list with database connection configuration
      #                        includes: type, host, port(optional), username,
      #                        password and name elements
      #   modelName:           name of the current model
      #   slocktimeLimit:      maximum duration a lock is allowed to persist
      #   hcubeActive:         boolean that specifies whether Hypercube mode is currently active
      #   ugroups:             user group(s) (optional)
      #   forceNew:            force creating new db object even if one already exists

      # BEGIN error checks
      if (is.null(private$info$isInitialized) || forceNew) {
        private$info$isInitialized <- 1L
      } else {
        flog.error("Db: Tried to create more than one Db object.")
        stop("An Object of class Db has already been initialized. Only one Db object allowed.",
          call. = FALSE
        )
      }
      stopifnot(is.character(uid), length(uid) == 1L)
      if (!identical(dbConf$type, "sqlite")) {
        dbConf$type <- "postgres"
        stopifnot(
          is.character(dbConf$host), length(dbConf$host) == 1L,
          is.character(dbConf$username), length(dbConf$username) == 1L,
          is.character(dbConf$password), length(dbConf$password) == 1L
        )
      }
      stopifnot(
        is.character(dbConf$name), length(dbConf$name) == 1L,
        is.numeric(slocktimeLimit), length(slocktimeLimit) >= 1L
      )
      if (!is.null(dbConf$port)) {
        stopifnot(is.numeric(dbConf$port) && length(dbConf$port) == 1L)
      }
      stopifnot(
        is.character(dbConf$type), length(dbConf$type) == 1L,
        is.character(modelName), length(modelName) == 1L,
        is.logical(hcubeActive), length(hcubeActive) == 1L
      )
      # END error checks

      private$uid <- uid
      if (length(ugroups) >= 1L && is.character(ugroups)) {
        private$userAccessGroups <- paste0("#", ugroups)
      } else {
        private$userAccessGroups <- character(0L)
      }
      private$modelName <- modelName
      private$slocktimeLimit <- slocktimeLimit
      private$hcubeActive <- hcubeActive

      if (identical(dbConf$type, "postgres")) {
        tryCatch(
          {
            private$conn <- DBI::dbConnect(
              drv = RPostgres::Postgres(), dbname = dbConf$name,
              host = dbConf$host, port = dbConf$port,
              user = dbConf$username, password = dbConf$password,
              bigint = "integer"
            )
          },
          error = function(e) {
            stop(sprintf(
              "Db: Database connection could not be established. Error message: %s",
              conditionMessage(e)
            ),
            call. = FALSE
            )
          }
        )
        private$connectionInfo <- list(
          loc = paste0(dbConf$host, ":", dbConf$port),
          name = dbConf$name, schema = dbConf$schema
        )
        if (length(dbConf$schema) &&
          !dbConf$schema %in% c("public", dbConf$username)) {
          # need to add schema to search path
          self$runQuery(paste0(
            "SET search_path TO ",
            dbQuoteIdentifier(private$conn, dbConf$schema),
            ";"
          ))
        }
      } else if (identical(dbConf$type, "sqlite")) {
        if (length(dbConf$dbPathToMigrate)) {
          tryCatch(
            {
              source("./migrations/1-4-0-migrate-db.R")
              migrateMiroDatabase(oldPath = dbConf$dbPathToMigrate, newPath = dirname(dbConf$name))
            },
            error = function(e) {
              stop(sprintf("Db: Database could not be migrated. Error message: %s", conditionMessage(e)),
                call. = FALSE
              )
            }
          )
        }
        tryCatch(
          {
            private$conn <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = dbConf$name, bigint = "integer")
            # turn foreign key usage on
            self$runQuery("PRAGMA foreign_keys = ON;")
          },
          error = function(e) {
            stop(sprintf(
              "Db: Database connection could not be established. Error message: %s",
              conditionMessage(e)
            ),
            call. = FALSE
            )
          }
        )
        private$connectionInfo <- list(
          loc = dbConf$name,
          name = NULL
        )
      } else {
        stop(sprintf(
          "Db: A non supported database type (%s) was specified. Could not establish connection.",
          dbConf$type
        ), call. = FALSE)
      }
      return(invisible(self))
    },
    getConn = function() private$conn,
    getUid = function() private$uid,
    getUserAccessGroups = function() c(private$uid, private$userAccessGroups),
    getRemoteUsers = function() private$remoteUsers,
    setRemoteUsers = function(remoteUsers) {
      private$remoteUsers <- remoteUsers
      return(invisible(self))
    },
    getSlocktimeLimit = function() private$slocktimeLimit,
    getHcubeActive = function() private$hcubeActive,
    getInfo = function() {
      # Returns some connection info
      #
      # Returns:
      #   list with strings: $loc (location info), $name (database name)
      return(private$connectionInfo)
    },
    getMetadata = function(sid, uid, sname, stime, stag = character(0L),
                           readPerm = character(0L), writePerm = character(0L),
                           execPerm = character(0L),
                           uidAlias = "_uid",
                           snameAlias = "_sname",
                           stimeAlias = "_time",
                           stagAlias = "_stag",
                           readPermAlias = "_accessr",
                           writePermAlias = "_accessw",
                           execPermAlias = "_accessx") {
      # Generate dataframe containing scenario metadata
      #
      # Args:
      #   sid:                      scenario ID
      #   uid:                      user ID
      #   sname:                    name of the scenario
      #   stime:                    time the scenario was generated
      #   stag:                     tags of the scenario (optional)
      #   readPerm:                 read permissions of scenario (optional)
      #   writePerm:                write permissions of scenario (optional)
      #   execPerm:                 execute permissions of scenario (optional)
      #   uidAlias:                 User ID Description (optional)
      #   snameAlias:               Scenario name Description (optional)
      #   stimeAlias:               Scenario time description (optional)
      #   stagAlias:                Scenario tag description (optional)
      #   readPermAlias:            Scenario read permissions description (optional)
      #   writePermAlias:           Scenario write permissions description (optional)
      #   execPermAlias:            Scenario execute permissions description (optional)
      #
      # Returns:
      #   dataframe with metadata

      # BEGIN error checks
      if (!length(sid)) {
        sid <- NA_integer_
      }
      stopifnot(
        is.integer(sid),
        is.character(uid),
        is.character(sname)
      )
      stime <- as.POSIXct(stime)
      stopifnot(inherits(stime, "POSIXct"))
      stopifnot(
        is.character(stag), is.character(readPerm),
        is.character(writePerm), is.character(execPerm),
        is.character(uidAlias), length(uidAlias) == 1L,
        is.character(snameAlias), length(snameAlias) == 1L,
        is.character(stimeAlias), length(stimeAlias) == 1L,
        is.character(stagAlias), is.character(readPermAlias),
        is.character(writePermAlias), is.character(execPermAlias)
      )
      # END error checks

      metadata <- tibble(sid, uid, sname, stime)
      names(metadata) <- c("_sid", uidAlias, snameAlias, stimeAlias)
      if (length(stag)) {
        if (length(stag) > 1L) {
          stag <- vector2Csv(stag)
        }
        metadata[[stagAlias]] <- stag
      }
      permList <- list(
        perm = list(readPerm, writePerm, execPerm),
        alias = list(
          readPermAlias, writePermAlias,
          execPermAlias
        )
      )
      for (i in seq_len(3L)) {
        perm <- permList$perm[[i]]
        if (length(perm)) {
          metadata[[permList$alias[[i]]]] <- vector2Csv(perm)
        }
      }

      return(metadata)
    },
    getAccessPermSubQuery = function(accessId) {
      # returns subquery that can be used to restrict results to user permissions
      return(private$buildSQLSubsetString(
        private$getCsvSubsetClause(
          accessId,
          c(
            private$uid,
            private$userAccessGroups
          )
        ),
        " OR "
      ))
    },
    getScenWithSameHash = function(scenHashes, limit = 1L, count = FALSE, distinctHashes = FALSE) {
      if (!DBI::dbExistsTable(private$conn, dbSchema$getDbTableName("_scenHash"))) {
        if (count) {
          return(tibble(count = 0L))
        }
        return(tibble(`_sid` = integer(), hash = character()))
      }
      escapedHashTableName <- DBI::dbQuoteIdentifier(
        private$conn,
        dbSchema$getDbTableName("_scenHash")
      )
      escapedMetaTableName <- DBI::dbQuoteIdentifier(
        private$conn,
        dbSchema$getDbTableName("_scenMeta")
      )
      if (length(scenHashes) > 500L) {
        dbWriteTable(private$conn, "_sys_temp_hashLookup",
          tibble(hash = scenHashes),
          row.names = FALSE, append = FALSE, overwrite = TRUE,
          temporary = TRUE
        )
        inClause <- "SELECT hash FROM _sys_temp_hashLookup"
      } else {
        inClause <- paste(DBI::dbQuoteString(private$conn, scenHashes), collapse = ", ")
      }
      query <- paste0(
        "SELECT ",
        if (count) {
          paste0("COUNT(DISTINCT(", escapedHashTableName, ".hash))")
        } else if (distinctHashes) {
          paste0("DISTINCT(", escapedHashTableName, ".hash)")
        } else {
          paste(paste0(
            escapedMetaTableName, ".",
            c("_sid", "_uid", "_sname", "_stime", "_stag")
          ),
          collapse = ", "
          )
        }, " FROM ",
        escapedHashTableName, " INNER JOIN ",
        escapedMetaTableName,
        " ON ",
        escapedHashTableName, "._sid=",
        escapedMetaTableName, "._sid WHERE ",
        escapedHashTableName, ".hash IN (",
        inClause, ")",
        " AND (", self$getAccessPermSubQuery("_accessr"), ")",
        if (length(limit)) paste0(" LIMIT ", as.integer(limit))
      )
      return(as_tibble(DBI::dbGetQuery(private$conn, query)))
    },
    checkSnameExists = function(sname, uid = NULL, checkNormalScen = FALSE) {
      # test whether scenario with the given name already exists
      # for the user specified
      #
      # Args:
      #   sname:                scenario name
      #   uid:                  user ID (optional)
      #   checkNormalScen:      relevant in HC mode: whether to check HC job config (FALSE)
      #                         or all normal scenarios (TRUE)
      #
      # Returns:
      #   boolean:              TRUE if id exists, FALSE otherwise,
      #   throws exception in case of error

      # BEGIN error checks
      if (!is.null(uid)) {
        stopifnot(is.character(uid), length(uid) == 1)
      } else {
        uid <- private$uid
      }
      stopifnot(is.character(sname), length(sname) == 1)
      # END error checks

      # check whether scenario name already exists
      scenExists <- self$importDataset("_scenMeta",
        tibble(
          c(
            "_uid",
            "_sname",
            "_scode"
          ),
          c(
            uid, sname,
            if (private$hcubeActive && !checkNormalScen) -1L else 0L
          ),
          c(
            "=", "=",
            if (private$hcubeActive && !checkNormalScen) "=" else ">="
          )
        ),
        count = TRUE, limit = 1L
      )
      if (length(scenExists) && scenExists[[1]] >= 1) {
        flog.trace("Db: Scenario with name: '%s' already exists for user: '%s' " %+%
          "(Db.checkScenExists returns TRUE).", sname, uid)
        return(TRUE)
      }
      flog.trace("Db: Scenario with name: '%s' does not yet exist for user: '%s' " %+%
        "(Db.checkScenExists returns FALSE).", sname, uid)
      return(FALSE)
    },
    getSid = function(sname, uid = NULL) {
      # fetch scenario ID for a given scenario name/ user ID
      # combination
      #
      # Args:
      #    sname:           scenario name
      #    uid:             user ID (optional)
      #
      # Returns:
      #    integer: scenario ID for the given scenario name
      #             or 0 in case scenario does not exist

      # BEGIN error checks
      if (is.null(uid)) {
        uid <- private$uid
      } else {
        stopifnot(is.character(uid), length(uid) == 1)
      }
      stopifnot(is.character(sname), length(sname) == 1)
      # END error checks

      metadataRow <- self$importDataset("_scenMeta",
        tibble(
          c(
            "_uid",
            "_sname",
            "_scode"
          ),
          c(
            uid, sname,
            if (private$hcubeActive) {
              SCODEMAP[["hcube_jobconfig"]]
            } else {
              SCODEMAP[["scen"]]
            }
          )
        ),
        limit = 1L
      )

      if (nrow(metadataRow)) {
        return(as.integer(metadataRow[["_sid"]][1]))
      } else {
        return(0L)
      }
    },
    loadScriptResults = function(sids, limit = 1e7, msgProgress = NULL) {
      # Load script results from database
      #
      # Args:
      #   sids:             scenario IDs to load
      #   limit:            maxmimum number of rows to fetch per dataset
      #   msgProgress:      title and progress info for the progress bar
      #
      # Returns:
      #   list of scenario datasets

      # BEGIN error checks
      sids <- suppressWarnings(as.integer(sids))
      stopifnot(!any(is.na(sids)), length(sids) >= 1)
      stopifnot(is.numeric(limit), length(limit) == 1)
      # END error checks

      # initialize progress bar
      if (!is.null(msgProgress)) {
        stopifnot(is.character(msgProgress$title), length(msgProgress$title) == 1)
        stopifnot(is.character(msgProgress$progress), length(msgProgress$progress) == 1)
        prog <- Progress$new()
        on.exit(prog$close())
        prog$set(message = msgProgress$title, value = 0)
        incAmount <- 1 / length(sids)
        updateProgress <- function(detail = NULL) {
          prog$inc(amount = incAmount, detail = detail)
        }
      }
      scriptData <- lapply(seq_along(sids), function(i) {
        dataset <- self$importDataset(
          tableName = "_scenScripts",
          subsetSids = sids[i], limit = limit
        )
        dataset[, "_sid"] <- NULL
        if (!is.null(msgProgress)) {
          updateProgress(detail = paste0(msgProgress$progress, i))
        }
        return(dataset)
      })
      return(scriptData)
    },
    deleteRows = function(tableName, colNames = NULL, values = NULL,
                          conditionSep = c("AND", "OR"),
                          subsetSids = NULL, force = FALSE) {
      # remove rows from table where rows have given values
      #
      # Args:
      #   tableName:        name of the table where entries should be removed from
      #   colNames:         character vector of column names (optional)
      #   values:           character vector of values that should be removed (optional)
      #   conditionSep:     seperator used for concatenating subsetting conditions (AND or OR) (optional)
      #   subsetSids:       vector of scenario IDs that query should be filtered on (optional)
      #   force:            will force remove of all data in table (optional)
      #
      # Returns:
      #   integer: number of rows deleted

      # BEGIN error checks
      stopifnot(is.character(tableName), length(tableName) == 1)
      if (!is.null(colNames)) {
        stopifnot(is.character(colNames), length(colNames) >= 1)
        values <- as.character(values)
        stopifnot(is.character(values), length(values) >= 1)
      } else if (!is.null(subsetSids)) {
        subsetSids <- as.integer(subsetSids)
        stopifnot(!any(is.na(subsetSids)))
      } else if (!force) {
        stop(sprintf(
          "Can't delete entire table: '%s'. Please specify subset.",
          tableName
        ), call. = FALSE)
      }
      # END error checks

      tableNameDb <- dbSchema$getDbTableName(tableName)

      conditionSep <- match.arg(conditionSep)

      affectedRows <- 0

      subsetSidSQL <- NULL

      subsetRows <- NULL
      if (!is.null(colNames)) {
        subsetRows <- paste(paste(DBI::dbQuoteIdentifier(private$conn, colNames),
          DBI::dbQuoteLiteral(private$conn, values),
          sep = " = "
        ),
        collapse = paste0(" ", conditionSep, " ")
        )
      }
      if (!is.null(subsetSids) && length(subsetSids) >= 1L) {
        subsetSidSQL <- paste0(
          DBI::dbQuoteIdentifier(private$conn, "_sid"),
          " IN (",
          paste(DBI::dbQuoteLiteral(private$conn, subsetSids), collapse = ","),
          ") "
        )
        if (length(subsetRows) && nchar(subsetRows)) {
          subsetRows <- DBI::SQL(paste(subsetRows, subsetSidSQL, sep = " AND "))
        } else {
          subsetRows <- DBI::SQL(subsetSidSQL)
        }
      }
      subsetWritePerm <- NULL
      if (identical(tableName, "_scenMeta")) {
        subsetWritePerm <- paste0(
          " AND (",
          self$getAccessPermSubQuery("_accessw"), ")"
        )
      }
      if (!length(subsetRows) || nchar(subsetRows) < 1L) {
        if (force && is.null(subsetWritePerm)) {
          subsetCondition <- ""
        } else {
          flog.error("No condition provided for rows to delete on table: '%s'.", tableNameDb)
          stop(sprintf("No condition provided for rows to delete on table: '%s'.", tableNameDb),
            call. = FALSE
          )
        }
      } else {
        subsetCondition <- paste0(" WHERE ", subsetRows, subsetWritePerm)
      }
      if (DBI::dbExistsTable(private$conn, tableNameDb)) {
        tryCatch(
          {
            query <- paste0(
              "DELETE FROM ", DBI::dbQuoteIdentifier(private$conn, tableNameDb),
              subsetCondition
            )
            affectedRows <- as.integer(self$runQuery(query))
            flog.debug("Db: %s rows in table: '%s' were deleted. (Db.deleteRows)", affectedRows, tableNameDb)
          },
          error = function(e) {
            stop(sprintf("Db: An error occurred while deleting rows from the database (Db.deleteRows, table: '%s'). Error message: %s.", tableNameDb, conditionMessage(e)),
              call. = FALSE
            )
          }
        )
      }
      return(affectedRows)
    },
    updateRows = function(tableName, ..., colNames, values, innerSepAND = TRUE, subsetSids = NULL) {
      # Update records in database table based on subset conditions
      #
      # Args:
      #   tableName:        name of the table where entries should be removed from
      #   ...:              row subsetting: dataframes with 3 columns (column, value, operator)
      #                     these will be handled as a block of AND conditions
      #                     (if innerSep AND is TRUE) different blocks are concatenated with OR
      #   colNames:         column names to update
      #   values:           character vector of new values that colNames should be set to
      #   innerSepAND:      boolean that specifies whether inner seperator in
      #                     substrings should be AND (TRUE) or OR (FALSE)
      #   subsetSids:       vector of scenario IDs that query should be filtered on
      #
      # Returns:
      #   integer with number of affected rows

      dots <- list(...)
      stopifnot(all(vapply(dots, private$isValidSubsetGroup, logical(1L),
        USE.NAMES = FALSE
      )))
      if (innerSepAND) {
        innerSep <- " AND "
        outerSep <- " OR "
      } else {
        innerSep <- " OR "
        outerSep <- " AND "
      }
      if (!is.null(subsetSids)) {
        subsetSids <- as.integer(subsetSids)
        stopifnot(!any(is.na(subsetSids)))
      }
      stopifnot(is.character(tableName), length(tableName) == 1)
      if (!is.null(colNames)) {
        stopifnot(is.character(colNames), length(colNames) >= 1)
        stopifnot(length(values) >= 1)
      }

      tableNameDb <- dbSchema$getDbTableName(tableName)

      subsetRows <- self$buildRowSubsetSubquery(dots, innerSep, outerSep)
      stopifnot(length(subsetRows) >= 0L)
      if (!is.null(subsetSids) && length(subsetSids) >= 1L) {
        subsetSidSQL <- paste0(
          DBI::dbQuoteIdentifier(private$conn, "_sid"),
          " IN (", paste(DBI::dbQuoteLiteral(private$conn, subsetSids),
            collapse = ","
          ), ") "
        )
        if (length(subsetRows)) {
          subsetRows <- DBI::SQL(paste(subsetRows, subsetSidSQL, sep = " AND "))
        } else {
          subsetRows <- DBI::SQL(subsetSidSQL)
        }
      }
      subsetWritePerm <- NULL
      if (identical(tableName, "_scenMeta")) {
        subsetWritePerm <- paste0(
          " AND (",
          self$getAccessPermSubQuery("_accessw"), ")"
        )
      }
      tryCatch(
        {
          query <- paste0(
            "UPDATE ", DBI::dbQuoteIdentifier(private$conn, tableNameDb), " SET ",
            paste(paste(DBI::dbQuoteIdentifier(private$conn, colNames),
              DBI::dbQuoteLiteral(private$conn, values),
              sep = " = "
            ),
            collapse = ", "
            ), " WHERE ", subsetRows, subsetWritePerm, ";"
          )
          affectedRows <- self$runQuery(query)
          flog.debug(
            "Db: %s rows in table: '%s' were updated (Db.updateRows)",
            affectedRows, tableNameDb
          )
        },
        error = function(e) {
          stop(sprintf(
            "Db: An error occurred while querying the database (Db.updateRows, table: '%s'). Error message: %s.",
            tableNameDb, conditionMessage(e)
          ), call. = FALSE)
        }
      )
      return(affectedRows)
    },
    exportDataset = function(tableName, data, checkColNames = FALSE) {
      # Export the dataframe to database
      #
      # Args:
      #   tableName :       name of the table to export dataframe to
      #   data:             dataframe with data to write
      #   checkColNames:    boolean that specifies whether col name should be
      #                     checked. Non existing columns will be appended
      #
      # Returns:
      #   reference to class or throws exception in case of failure

      stopifnot(
        inherits(data, "data.frame"), is.character(tableName),
        identical(length(tableName), 1L),
        is.logical(checkColNames),
        identical(length(checkColNames), 1L)
      )

      tableNameDb <- dbSchema$getDbTableName(tableName)

      if (!dbExistsTable(private$conn, tableNameDb)) {
        dbWriteTable(private$conn, tableNameDb, data, row.names = FALSE)
        return(self)
      }
      if (!checkColNames) {
        dbWriteTable(private$conn, tableNameDb, data, row.names = FALSE, append = TRUE)
        return(self)
      }
      dataValidate <- self$importDataset(tableName, limit = 1L)
      nonMatchingColNames <- is.na(match(
        names(data),
        names(dataValidate)
      ))
      if (any(nonMatchingColNames)) {
        stop(sprintf(
          "Some columns could not be found in the table: '%s'",
          paste(names(data)[nonMatchingColNames],
            collapse = "', '"
          )
        ), call. = FALSE)
      }
      if (identical(length(dataValidate, nonMatchingColNames))) {
        dbWriteTable(private$conn, tableNameDb, data, row.names = FALSE, append = TRUE)
        return(self)
      }
      nrowData <- nrow(data)
      dataList <- lapply(names(dataValidate), function(colName) {
        colId <- match(colName, names(data))
        if (!is.na(colId)) {
          return(data[[colId]])
        }
        col <- dataValidate[[colName]]
        if (any(class(col) == "POSIXt")) {
          return(rep(NA_character_, nrowData))
        }
        switch(class(col),
          logical = {
            return(logical(nrowData))
          },
          integer = {
            return(rep(NA_integer_, nrowData))
          },
          numeric = {
            return(rep(NA_real_, nrowData))
          },
          character = {
            return(rep(NA_character_, nrowData))
          },
          {
            stop(sprintf(
              "Unknown data type: '%s' of column: '%s'",
              class(col), colName
            ), call. = FALSE)
          }
        )
      })
      names(dataList) <- names(dataValidate)
      dbWriteTable(private$conn, tableNameDb, as_tibble(dataList),
        row.names = FALSE, append = TRUE
      )
      return(self)
    },
    importDataset = function(tableName, ..., colNames = NULL, count = FALSE, limit = 1e7,
                             innerSepAND = TRUE, distinct = FALSE, subsetSids = NULL,
                             orderBy = character(0L), orderAsc = TRUE, isAdmin = FALSE) {
      # Import the data corresponding to the table name provided from the database by
      # considering scenario IDs specified.
      #
      # Args:
      #   tableName :       name of the table to import dataframe from
      #   ...:              row subsetting: dataframes with 3 columns (column, value, operator)
      #                     these will be handled as a block of AND conditions
      #                     (if innerSep AND is TRUE) different blocks are concatenated with OR
      #   colNames:         column names to select, if NULL all will be selected (optional)
      #   count :           boolean that specifies whether to merely count number
      #                     of rows or return actual values (optional)
      #   limit:            maxmimum number of rows to fetch (optional)
      #   innerSepAND:      boolean that specifies whether inner seperator in
      #                     substrings should be AND (TRUE) or OR (FALSE)
      #   distinct:         boolean that specifies whether to remove duplicate rows
      #                     (TRUE) or not (FALSE)
      #   subsetSids:       vector of scenario IDs that query should be filtered on
      #   orderBy:          character vector of columns to order by
      #   orderAsc:         boolean that specifies whether to order in ascending (TRUE)
      #                     or descending (FALSE) order
      #   isAdmin:          boolean that specifies whether user accessing data is admin (can read all data)
      #
      # Returns:
      #   tibble: dataset (cleaned of metadata columns) with data coming from the table selected.
      #   In case table does not exist or no rows in dataset, returns NULL

      # BEGIN error checks
      dots <- list(...)
      stopifnot(all(vapply(dots, private$isValidSubsetGroup, logical(1L),
        USE.NAMES = FALSE
      )))
      stopifnot(is.character(tableName), length(tableName) == 1)
      if (!is.null(colNames)) {
        stopifnot(is.character(colNames), length(colNames) >= 1)
      }
      stopifnot(is.logical(count), length(count) == 1)
      stopifnot(is.numeric(limit), length(limit) == 1)
      stopifnot(is.logical(innerSepAND), length(innerSepAND) == 1)
      stopifnot(is.logical(distinct), length(distinct) == 1)
      stopifnot(is.character(orderBy))
      stopifnot(is.logical(orderAsc), length(orderAsc) == 1)
      stopifnot(is.logical(isAdmin), length(isAdmin) == 1)
      if (innerSepAND) {
        innerSep <- " AND "
        outerSep <- " OR "
      } else {
        innerSep <- " OR "
        outerSep <- " AND "
      }
      if (!is.null(subsetSids)) {
        subsetSids <- as.integer(subsetSids)
        stopifnot(!any(is.na(subsetSids)))
      }
      # END error checks

      tableNameDb <- dbSchema$getDbTableName(tableName)

      if (!dbExistsTable(private$conn, tableNameDb)) {
        flog.debug(
          "Db: A table named: '%s' does not exist in the database (Db.importDataset).",
          tableNameDb
        )
        return(tibble())
      } else {
        if (!is.null(colNames)) {
          colNames <- paste(DBI::dbQuoteIdentifier(private$conn, colNames),
            collapse = ","
          )
        } else {
          colNames <- "*"
        }
        if (distinct) {
          colNames <- "DISTINCT " %+% colNames
        }
        if (count) {
          colNames <- "COUNT(" %+% colNames %+% ")"
        }
        innerJoin <- NULL
        subsetRows <- NULL

        if (length(dots)) {
          subsetRows <- self$buildRowSubsetSubquery(dots, innerSep, outerSep)
        }
        if (!is.null(subsetSids) && length(subsetSids) >= 1L) {
          if (inherits(private$conn, "PqConnection")) {
            # POSTGRES subsetting
            innerJoin <- paste0(
              " INNER JOIN (VALUES ", paste("(" %+% subsetSids, collapse = "), "),
              ")) vals(_v) ON ",
              DBI::dbQuoteIdentifier(private$conn, "_sid"), "=_v"
            )
          } else {
            subsetSidSQL <- paste0(
              DBI::dbQuoteIdentifier(private$conn, "_sid"),
              " IN (", paste(DBI::dbQuoteLiteral(private$conn, subsetSids),
                collapse = ","
              ), ") "
            )
            if (length(subsetRows)) {
              subsetRows <- DBI::SQL(paste0(subsetRows, " AND ", subsetSidSQL))
            } else {
              subsetRows <- DBI::SQL(subsetSidSQL)
            }
          }
        }
        orderByQuery <- character(0L)
        if (length(orderBy)) {
          orderByQuery <- paste0(
            " ORDER BY ",
            paste(DBI::dbQuoteIdentifier(
              private$conn,
              orderBy
            ), collapse = ", "),
            if (orderAsc) " ASC" else " DESC"
          )
        }
        subsetReadPerm <- NULL
        if (!isAdmin && identical(tableName, "_scenMeta")) {
          subsetReadPerm <- self$getAccessPermSubQuery("_accessr")
          if (length(subsetRows)) {
            subsetRows <- paste0(subsetRows, " AND (", subsetReadPerm, ")")
          } else {
            subsetRows <- paste0(" (", subsetReadPerm, ")")
          }
        }
        tryCatch(
          {
            sql <- DBI::SQL(paste0(
              "SELECT ", colNames, " FROM ",
              DBI::dbQuoteIdentifier(private$conn, tableNameDb),
              innerJoin,
              if (length(subsetRows)) " WHERE ", subsetRows, orderByQuery,
              " LIMIT ?lim ;"
            ))
            query <- DBI::sqlInterpolate(private$conn, sql, lim = limit)
            dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
            dataset[["_v"]] <- NULL
            flog.debug("Db: Data was imported from table: '%s' (Db.importDataset).", tableNameDb)
          },
          error = function(e) {
            stop(sprintf(
              "Db: An error occurred while querying the database (Db.importDataset, " %+%
                "table: '%s'). Error message: %s.",
              tableNameDb, conditionMessage(e)
            ), call. = FALSE)
          }
        )
      }
      if (tableName %in% c(scalarsFileName, scalarsOutName)) {
        return(private$convertScalarTableFromDb(dataset, tableName, length(subsetSids)))
      }
      return(dataset)
    },
    exportScenDataset = function(dataset, tableName, isHcJobConfig = TRUE) {
      # Saves scenario dataset to database
      #
      # Args:
      #   dataset:             dataframe to save
      #   tableName:           name of the table to export dataframe to
      #   isHcJobConfig:       boolean that indicates whether data to store is
      #                        Hypercube Mode job configuration (optional)
      #
      # Returns:
      #   Db object: invisibly returns reference to object in case of success, throws Exception if error

      # BEGIN error checks
      if (!hasContent(dataset)) {
        flog.debug("Db: Nothing was written to table '%s' as no data was provided.", tableNameDb)
        return(invisible(self))
      }
      stopifnot(inherits(dataset, "data.frame") || is.null(dataset))
      stopifnot(is.character(tableName), length(tableName) == 1)
      # END error checks

      tableNameDb <- dbSchema$getDbTableName(tableName)
      if (tableName %in% c(scalarsFileName, scalarsOutName)) {
        if (private$hcubeActive && isHcJobConfig &&
          identical(tableName, scalarsFileName)) {
          tableName <- "_hc__scalars"
          tableNameDb <- tableName
        } else {
          dataset <- private$convertScalarTableToDb(dataset, tableName)
        }
      } else {
        dataset <- dateColToChar(private$conn, dataset)
      }
      if (!dbExistsTable(private$conn, tableNameDb)) {
        if (tableName %in% c(scalarsFileName, scalarsOutName)) {
          # this should only happen for example apps
          # as for those, we don't run MIRO when importing them
          tryCatch(
            {
              if (!exists("DbMigrator")) {
                source("./components/db_migrator.R")
              }
              dbMigrator <- DbMigrator$new(self)
              dbMigrator$createMissingScalarTables(tableName)
            },
            error = function(e) {
              stop(sprintf(
                "Table: '%s' could not be created (Db.exportScenDataset). Error message: %s.",
                tableNameDb, conditionMessage(e)
              ), call. = FALSE)
            }
          )
        } else {
          tryCatch(
            {
              self$runQuery(dbSchema$getCreateTableQuery(tableName))
            },
            error = function(e) {
              stop(sprintf(
                "Table: '%s' could not be created (Db.exportScenDataset). Error message: %s.",
                tableNameDb, conditionMessage(e)
              ), call. = FALSE)
            }
          )
          tryCatch(
            {
              self$runQuery(dbSchema$getCreateIndexQuery(tableName))
            },
            error = function(e) {
              stop(sprintf(
                "Index on table: '%s' could not be created (Db.exportScenDataset).Error message: %s.",
                tableNameDb, conditionMessage(e)
              ), call. = FALSE)
            }
          )
        }
        flog.debug(
          "Db: A database table named: '%s' did not yet exist. Therefore it was created (Db.exportScenDataset).",
          tableNameDb
        )
      }
      tryCatch(
        {
          DBI::dbWriteTable(private$conn, tableNameDb, dataset,
            row.names = FALSE, append = TRUE
          )
          flog.debug(
            "Db: Data was added to table: '%s' (Db.exportScenDataset).",
            tableNameDb
          )
        },
        error = function(e) {
          stop(sprintf(
            "Db: An error occurred writing to database (Db.exportScenDataset, table: '%s'). Error message: %s",
            tableNameDb, conditionMessage(e)
          ), call. = FALSE)
        }
      )
      invisible(self)
    },
    writeMetadata = function(metadata, update = FALSE) {
      # Write scenario metadata to database
      #
      # Args:
      #   metadata:      dataframe containing metadata for one or
      #                  multiple scenarios
      #   update:        boolean that specifies whether existing metadata
      #                  shall be updated
      #
      # Returns:
      #   reference to itself (Db R6 object)

      # BEGIN error checks
      stopifnot(inherits(metadata, "data.frame"))
      # END error checks
      metaTabName <- dbSchema$getDbTableName("_scenMeta")

      metadata <- dateColToChar(private$conn, metadata)
      if (!DBI::dbExistsTable(private$conn, metaTabName)) {
        tryCatch(
          {
            self$runQuery(dbSchema$getCreateTableQuery("_scenMeta"))
          },
          error = function(e) {
            stop(sprintf("Metadata table could not be created (Db.writeMetadata). " %+%
              "Error message: %s.", conditionMessage(e)), call. = FALSE)
          }
        )
        flog.debug("Db: A table named: '%s' did not yet exist (Db.writeMetadata). " %+%
          "Therefore it was created.", metaTabName)
      }
      if (update) {
        sql <- DBI::SQL(paste0(
          "UPDATE ",
          DBI::dbQuoteIdentifier(private$conn, metaTabName),
          " SET (",
          DBI::dbQuoteIdentifier(private$conn, "_uid"), ", ",
          DBI::dbQuoteIdentifier(private$conn, "_sname"), ", ",
          DBI::dbQuoteIdentifier(private$conn, "_stime"), ", ",
          DBI::dbQuoteIdentifier(private$conn, "_stag"), ", ",
          DBI::dbQuoteIdentifier(private$conn, "_accessr"), ", ",
          DBI::dbQuoteIdentifier(private$conn, "_accessw"), ", ",
          DBI::dbQuoteIdentifier(private$conn, "_accessx"), ") = (",
          "?uid, ?sname, ?stime, ?stag, ?accessR, ?accessW, ?accessX) WHERE ",
          DBI::dbQuoteIdentifier(private$conn, "_sid"), " = ?sid;"
        ))
        query <- DBI::sqlInterpolate(private$conn, sql,
          uid = metadata[["_uid"]],
          sname = metadata[["_sname"]][[1]],
          stime = as.character(metadata[["_stime"]][[1]], usetz = TRUE),
          stag = metadata[["_stag"]][[1]],
          accessR = metadata[["_accessr"]][[1]],
          accessW = metadata[["_accessw"]][[1]],
          accessX = metadata[["_accessx"]][[1]],
          sid = metadata[["_sid"]][[1]]
        )
        tryCatch(
          {
            self$runQuery(query)
            flog.debug(
              "Db: Metadata (table: '%s') was written to database. (Db.writeMetadata).",
              metaTabName
            )
          },
          error = function(e) {
            stop(sprintf(
              "Db: Metadata (table: '%s') could not " %+% "
        be written to database (Db.writeMetadata). Error message: %s.",
              metaTabName, conditionMessage(e)
            ), call. = FALSE)
          }
        )
      } else {
        # write new metadata
        tryCatch(
          {
            DBI::dbWriteTable(private$conn, metaTabName,
              metadata,
              row.names = FALSE, append = TRUE
            )
            flog.debug(
              "Db: Metadata (table: '%s') was written to database. (Db.writeMetadata).",
              metaTabName
            )
          },
          error = function(e) {
            stop(sprintf(
              "Db: Metadata (table: '%s') could not " %+% "
        be written to database (Db.writeMetadata). Error message: %s.",
              metaTabName, conditionMessage(e)
            ), call. = FALSE)
          }
        )
      }

      invisible(self)
    },
    createJobMeta = function() {
      metaSchema <- dbSchema$getDbSchema("_jobMeta")
      if (DBI::dbExistsTable(private$conn, metaSchema$tabName)) {
        return(invisible(self))
      }
      self$runQuery(dbSchema$getCreateTableQuery("_jobMeta"))
      return(invisible(self))
    },
    fetchScenList = function(scode = SCODEMAP[["scen"]], gt = FALSE) {
      # returns list of scenarios that the current user has access to
      #
      # Args:
      #   scode:           Fetch only scenarios with either of these scenario codes
      #   gt:              boolean that specifies whether to fetch equal status codes or greater/equal
      #
      # Returns:
      #   tibble: tibble with all scenarios user has access to read as well
      #   as their metadata, throws exception in case of error
      stopifnot(is.integer(scode))
      stopifnot(is.logical(gt), length(gt) == 1L)

      if (gt) {
        return(self$importDataset("_scenMeta",
          tibble(
            "_scode",
            scode, ">="
          ),
          innerSepAND = FALSE
        ))
      }
      return(self$importDataset("_scenMeta",
        tibble(
          "_scode",
          scode
        ),
        innerSepAND = FALSE
      ))
    },
    buildRowSubsetSubquery = function(subsetData, innerSep, outerSep) {
      brackets <- NULL
      if (identical(innerSep, " OR ")) {
        brackets <- c("(", ")")
      }
      query <- unlist(lapply(
        subsetData, private$buildSQLSubsetString,
        innerSep
      ), use.names = FALSE)
      if (length(query)) {
        query <- paste(brackets[1], query, brackets[2],
          collapse = outerSep
        )
      } else {
        return(character(0L))
      }
      return(DBI::SQL(query))
    },
    escapePatternPivot = function(pattern) {
      if (inherits(private$conn, "PqConnection")) {
        return(self$escapePattern(pattern))
      } else {
        bsEscaped <- gsub("\\", "\\\\\\\\", pattern, fixed = TRUE)
        return(gsub(
          "([.|()^{}+$*?'\"]|\\[|\\])",
          "\\\\\\\\\\1", stringi::stri_escape_unicode(bsEscaped)
        ))
      }
    },
    escapePattern = function(pattern) {
      pattern <- gsub("([%_\\])", "\\\\\\1", pattern)
      return(pattern)
    },
    runQuery = function(query) {
      flog.trace("Running query: '%s'", query)
      return(dbExecute(private$conn, SQL(query)))
    },
    finalize = function() {
      if (!is.null(private$conn)) {
        DBI::dbDisconnect(private$conn)
        flog.debug("Db: Database connection ended as Db object was gced.")
        private$conn <- NULL
      }
    }
  ),
  private = list(
    conn = NULL,
    connectionInfo = NULL,
    uid = character(1L),
    modelName = character(1L),
    dbSchema = vector("list", 3L),
    userAccessGroups = character(1L),
    remoteUsers = character(),
    slocktimeLimit = character(1L),
    hcubeActive = logical(1L),
    info = new.env(),
    isValidSubsetGroup = function(dataFrame) {
      if (inherits(dataFrame, "data.frame") &&
        length(dataFrame) <= 3L &&
        is.character(dataFrame[[1]]) &&
        (is.character(dataFrame[[2]]) ||
          is.numeric(dataFrame[[2]]) ||
          is.logical(dataFrame[[2]]))) {
        if (length(dataFrame) < 3L) {
          return(TRUE)
        } else if (all(dataFrame[[3]] %in%
          c(
            "=", "<", ">", "<=", ">=", "!=", "<>",
            "SIMILAR TO", "NOT SIMILAR TO",
            "LIKE", "NOT LIKE"
          ))) {
          return(TRUE)
        }
      }
      return(FALSE)
    },
    getCsvSubsetClause = function(colName, vector) {
      subsetClause <- tibble(
        colName,
        paste0("%,", self$escapePattern(vector), ",%"),
        "LIKE"
      )
      return(subsetClause)
    },
    buildSQLSubsetString = function(dataFrame, sep = " ") {
      if (!length(dataFrame) || !nrow(dataFrame)) {
        return(NULL)
      } else if (length(dataFrame) < 3L) {
        dataFrame[[3]] <- "="
      }
      fields <- dataFrame[[1]]
      vals <- as.character(dataFrame[[2]])
      checkForNull <- is.na(vals)
      fields <- DBI::dbQuoteIdentifier(private$conn, fields)
      vals <- DBI::dbQuoteLiteral(private$conn, vals)

      if (identical(length(dataFrame), 4L)) {
        fields <- paste0(
          dbQuoteIdentifier(private$conn, dataFrame[[4]]),
          ".", fields
        )
      }
      operator <- dataFrame[[3]]
      if (!inherits(private$conn, "PqConnection")) {
        # SQLite needs explicit mention of escape character in clause
        vals[operator == "LIKE"] <- paste0(vals[operator == "LIKE"], "  ESCAPE '\\'")
      }
      operator[checkForNull & operator == "="] <- "IS"
      operator[checkForNull & operator == "!="] <- "IS NOT"
      query <- paste(paste(fields, operator, vals), collapse = sep)
      return(SQL(query))
    },
    getMaximum = function(tableName, colName) {
      # Returns number of unique entries in the specified column
      #
      # Args:
      #   tableName :      name of the table to retrieve maximum value from
      #   colName:         column name to fetch maximum from
      #
      # Returns:
      #   numeric: largest value in the column specified,
      #   throws exception in case of error

      tableNameDb <- dbSchema$getDbTableName(tableName)

      if (!dbExistsTable(private$conn, tableNameDb)) {
        return(0L)
      }
      tryCatch(
        {
          query <- DBI::SQL(paste0(
            "SELECT ", DBI::dbQuoteIdentifier(private$conn, colName),
            " FROM ", DBI::dbQuoteIdentifier(private$conn, tableNameDb),
            " ORDER BY ", DBI::dbQuoteIdentifier(private$conn, colName),
            " DESC LIMIT 1;"
          ))
          max <- suppressWarnings(as.integer(dbGetQuery(private$conn, query)[[1]][1]))
        },
        error = function(e) {
          stop(sprintf("An error occurred while querying the database (Db.getMaximum). Error message: %s.", conditionMessage(e)), call. = FALSE)
        }
      )

      if (!is.na(max)) {
        return(max)
      } else {
        return(0L)
      }
    },
    convertScalarTableToDb = function(dataset, tableName) {
      # note that dataset has _sid as first column (thus everything is offset by 1)
      if (identical(tableName, scalarsFileName)) {
        symtypes <- character()
        if (scalarsFileName %in% names(ioConfig$modelInRaw)) {
          symtypes <- ioConfig$modelInRaw[[scalarsFileName]]$symtypes
        }
        symtypes <- c(symtypes, rep.int("set", length(ioConfig$DDPar) +
          length(ioConfig$GMSOpt)))
      } else {
        symtypes <- ioConfig$modelOut[[scalarsOutName]]$symtypes
      }
      dataset <- pivot_wider(dataset[, -3],
        names_from = names(dataset)[2],
        values_from = names(dataset)[4]
      )
      scalarsToSave <- dbSchema$getDbViews(tableName)
      scalarOrder <- match(scalarsToSave, names(dataset))
      coerceToFloat <- which(symtypes[!is.na(scalarOrder)] == "parameter") + 1L

      return(suppressWarnings(
        mutate_at(
          select(dataset, c(1L, scalarOrder[!is.na(scalarOrder)])),
          coerceToFloat, as.numeric
        )
      ))
    },
    convertScalarTableFromDb = function(dataset, tableName, noScen = 1L) {
      # note that dataset has _sid as first column (thus everything is offset by 1)
      if (!length(dataset) || !nrow(dataset)) {
        return(tibble())
      }
      if (identical(tableName, scalarsFileName)) {
        symtext <- character()
        if (scalarsFileName %in% names(ioConfig$modelInRaw)) {
          symtext <- ioConfig$modelInRaw[[scalarsFileName]]$symtext
        }
        symtext <- c(symtext, rep.int("", length(ioConfig$DDPar) +
          length(ioConfig$GMSOpt)))
      } else {
        symtext <- ioConfig$modelOut[[scalarsOutName]]$symtext
      }
      return(mutate(add_column(pivot_longer(mutate_all(dataset, as.character),
        cols = dbSchema$getDbViews(tableName),
        names_to = "scalar",
        values_to = "value"
      ),
      description = rep.int(symtext, noScen), .after = 2L
      ),
      value = as.character(value)
      ))
    }
  )
)
