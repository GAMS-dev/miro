dbMigrationForm <- function(id, inconsistentTablesInfo, orphanedTablesInfo,
                            standalone = TRUE) {
  ns <- NS(id)
  tagList(
    if(!standalone)
      tags$div(id = ns("dataLossDialog"), class = "container-fluid", style = "display:none",
               tags$h5(lang$nav$migrationModule$dialogConfirmDataLoss$title),
               tags$div(id = ns("dataLossDialogMsg")),
               checkboxInput_MIRO(ns("cbConfirmDataLoss"),
                                  lang$nav$migrationModule$dialogConfirmDataLoss$cbConfirmDataLoss),
               actionButton(ns("btCancelDataLoss"),
                            lang$nav$migrationModule$dialogConfirmDataLoss$btCancel),
               tagAppendAttributes(actionButton(ns("btConfirmDataLoss"), class = "bt-highlight-1",
                                                lang$nav$migrationModule$dialogConfirmDataLoss$btConfirm),
                                   `disabled` = "true")),
    tags$div(id = ns("migFormContainer"), class = "container-fluid", style = "font-size:12pt;",
             tags$div(id = ns("migrationSuccess"), class = "gmsalert gmsalert-success",
                      style = "position:fixed;",
                      lang$nav$migrationModule$successMsg),
             tags$div(id = ns("dataMigrationErrors"), class = "gmsalert gmsalert-error",
                      style = "white-space:pre-wrap;position:fixed;"),
             tags$p(style="text-align: center;", lang$nav$migrationModule$desc),
             tags$div(style = "text-align:center;margin-bottom:20px;",
                      tags$b(style = "border: 2px solid #f39619;padding: 6px 12px;",
                             lang$nav$migrationModule$backupWarning)),
             lapply(seq_along(inconsistentTablesInfo), function(i){
               tableInfo <- inconsistentTablesInfo[[i]]
               if(length(tableInfo$currentColNames)){
                 tableMapping <- tableInfo$tabName
                 textCols <- tableInfo$currentColNames[tolower(tableInfo$currentColTypes) == "text"]
                 numericCols <- tableInfo$currentColNames[!tableInfo$currentColNames %in% textCols]
                 colTypes <- strsplit(tableInfo$colTypes, "", fixed = TRUE)[[1]]
               }else{
                 tableMapping <- c("-", names(orphanedTablesInfo))
               }
               if(!length(tableMapping)){
                 return(NULL)
               }
               if(tableInfo$tabName %in% names(modelOut)){
                 tableMeta <- modelOut[[tableInfo$tabName]]
               }else if(tableInfo$tabName %in% names(modelInRaw)){
                 tableMeta <- modelInRaw[[tableInfo$tabName]]
               }else{
                 tableMeta <- list()
               }
               colClass <- max(floor(12/(length(tableInfo$colNames) + 1L)), 3L)
               colClass <- paste0("col-", colClass, " col-xs-", colClass)
               tagList(
                 tags$div(class = "row", style = "border-top:3px solid #000;text-align:left;padding-left: 15px;",
                          tags$h3(title = tableMeta$alias,
                                  sprintf(lang$nav$migrationModule$labelSymbol,
                                          tableInfo$tabName))
                 ),
                 tags$div(class = "row", style = "padding-top: 10px;",
                          tags$div(class = colClass,
                                   selectInput(ns(paste0("dbMigrateTable_", i)),
                                               lang$nav$migrationModule$selectTableToMap,
                                               tableMapping)),
                          lapply(seq_along(tableInfo$colNames), function(j){
                            showTypeWarning <- FALSE
                            if(length(tableInfo$currentColNames)){
                              if(identical(length(tableInfo$colNames), 1L)){
                                # scalar tables are allowed to be type-converted
                                colChoices <- c(textCols, numericCols)
                                if(!identical(colTypes[1], "c") && length(textCols)){
                                  showTypeWarning <- TRUE
                                }
                              }else{
                                if(identical(colTypes[j], "c")){
                                  colChoices <- c("-", textCols)
                                }else{
                                  colChoices <- c("-", numericCols)
                                }
                              }
                            }else{
                              colChoices <- "-"
                            }
                            tags$div(class = colClass,
                                     selectInput(ns(paste0("dbMigrateTable_", i, "_", j)),
                                                 tags$span(title = tableMeta$headers[[j]]$alias,
                                                           paste0(lang$nav$migrationModule$selectColToMap, tableInfo$colNames[j])),
                                                 colChoices,
                                                 selected = if(tableInfo$colNames[j] %in% colChoices)
                                                   tableInfo$colNames[j] else "-"
                                     ),
                                     tags$div(id = ns("incompatibleTypeWarning"),
                                              class = "err-msg",
                                              style = paste0("margin:0px;text-align: justify;", if(!showTypeWarning) "display:none;"),
                                              lang$nav$migrationModule$incompatibleTypeWarning))
                          })
                 )
               )
             }),
             tags$div(class = "row", style = "padding: 15px;text-align: right",
                      actionButton(ns("btConfirmMigration"), class = "bt-highlight-1",
                                   lang$nav$migrationModule$btConfirmMigration),
                      if(standalone)
                        tagList(
                          removeDbTablesButton(ns("removeAllButton")),
                          actionButton(ns("btCancelMigration"),
                                       lang$nav$migrationModule$btCancelMigration)
                        )
             )))
}

dbMigrationServer <- function(id, inconsistentTablesInfo, orphanedTablesInfo,
                              standalone = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      lapply(seq_along(inconsistentTablesInfo), function(i){
        tableInfo <- inconsistentTablesInfo[[i]]
        if(length(tableInfo$currentColNames)){
          return()
        }
        observeEvent(input[[paste0("dbMigrateTable_", i)]], {
          orphanSelected <- input[[paste0("dbMigrateTable_", i)]]
          if(identical(orphanSelected, "-")){
            for(j in seq_along(tableInfo$colNames)){
              updateSelectInput(session, paste0("dbMigrateTable_", i, "_", j),
                                choices = "-", selected = "-")
            }
            return()
          }
          selectedTableInfo <- orphanedTablesInfo[[orphanSelected]]
          
          textCols <- selectedTableInfo$colNames[tolower(selectedTableInfo$colTypes) == "text"]
          numericCols <- selectedTableInfo$colNames[!selectedTableInfo$colNames %in% textCols]
          
          colTypes <- strsplit(tableInfo$colTypes, "", fixed = TRUE)[[1]]
          
          for(j in seq_along(tableInfo$colNames)){
            if(identical(length(tableInfo$colNames), 1L)){
              # scalar tables are allowed to be type-converted
              colChoices <- c(textCols, numericCols)
              if(!identical(colTypes[j], "c") && length(textCols)){
                showEl(session, paste0("#", session$ns("incompatibleTypeWarning")))
              }else{
                hideEl(session, paste0("#", session$ns("incompatibleTypeWarning")))
              }
            }else{
              if(identical(colTypes[j], "c")){
                colChoices <- c("-", textCols)
              }else{
                colChoices <- c("-", numericCols)
              }
            }
            updateSelectInput(session, paste0("dbMigrateTable_", i, "_", j),
                              choices = colChoices,
                              selected = if(tableInfo$colNames[j] %in% colChoices)
                                tableInfo$colNames[j] else colChoices[1])
          }
        })
      })
      confirmDataLoss <- reactiveVal(FALSE)
      getMigrationConfig <- reactive({
        showDataLossErrors <- !confirmDataLoss()
        if(!length(input$btConfirmMigration) ||
           identical(input$btConfirmMigration, 0L)){
          return()
        }
        isolate({
          errMsgDataLoss <- NULL
          migrationConfig <- lapply(seq_along(inconsistentTablesInfo), function(i){
            tableInfo <- inconsistentTablesInfo[[i]]
            tableConfig <- list(oldTableName = input[[paste0("dbMigrateTable_", i)]],
                                colNames = vapply(seq_along(tableInfo$colNames), function(j){
                                  return(input[[paste0("dbMigrateTable_", i, "_", j)]])
                                }, character(1L), USE.NAMES = FALSE))
            missingCols <- character(0L)
            if(showDataLossErrors){
              if(identical(tableConfig$oldTableName, tableInfo$tabName)){
                missingCols <- tableInfo$currentColNames[!tableInfo$currentColNames %in% tableConfig$colNames]
              }else if(!identical(tableConfig$oldTableName, "-")){
                orphanTableCols <- orphanedTablesInfo[[tableConfig$oldTableName]]$colNames
                missingCols <- orphanTableCols[!orphanTableCols %in% tableConfig$colNames]
              }
            }
            if(length(missingCols)){
              errMsgDataLoss <<- paste(errMsgDataLoss,
                                       sprintf(lang$nav$migrationModule$dialogConfirmDataLoss$descTableCols,
                                               paste(missingCols, collapse = "', '"),
                                               tableConfig$oldTableName),
                                       sep = "\n")
            }
            return(tableConfig)
          })
          tablesMigrated <- vapply(migrationConfig, "[[", character(1L), "oldTableName",
                                   USE.NAMES = FALSE)
          tablesMigrated <- tablesMigrated[tablesMigrated != "-"]
          tablesToBeRemoved <- !names(orphanedTablesInfo) %in% tablesMigrated
          if(showDataLossErrors && any(tablesToBeRemoved)){
            errMsgDataLoss <- paste(errMsgDataLoss,
                                    sprintf(lang$nav$migrationModule$dialogConfirmDataLoss$descTables,
                                            paste(names(orphanedTablesInfo)[tablesToBeRemoved], collapse = "', '")),
                                    sep = "\n")
          }
          if(length(errMsgDataLoss)){
            stop_custom("error_data_loss", errMsgDataLoss, call. = FALSE)
          }
          if(any(duplicated(tablesMigrated))){
            stop_custom("error_bad_settings",
                        sprintf(lang$nav$migrationModule$errDuplicateTables,
                                paste(tablesMigrated[duplicated(tablesMigrated)], collapse = "', '")),
                        call. = FALSE)
          }
          names(migrationConfig) <- vapply(inconsistentTablesInfo, "[[",
                                           character(1L), "tabName", USE.NAMES = FALSE)
          return(migrationConfig)
        })
      })
      
      observe({
        if(isTRUE(input$cbConfirmDataLoss)){
          enableEl(session, paste0("#", session$ns("btConfirmDataLoss")))
        }else{
          disableEl(session, paste0("#", session$ns("btConfirmDataLoss")))
        }
      })
      if(standalone){
        returnCode <- reactiveVal(NULL)
        showDataLossConfirmationDialog <- function(errMsg){
          isolate(confirmDataLoss(FALSE))
          showModal(modalDialog(
            title = lang$nav$migrationModule$dialogConfirmDataLoss$title,
            errMsg,
            checkboxInput_MIRO(session$ns("cbConfirmDataLoss"),
                               lang$nav$migrationModule$dialogConfirmDataLoss$cbConfirmDataLoss),
            footer = tagList(
              modalButton(lang$nav$migrationModule$dialogConfirmDataLoss$btCancel),
              tagAppendAttributes(actionButton(session$ns("btConfirmDataLoss"), class = "bt-highlight-1",
                                               lang$nav$migrationModule$dialogConfirmDataLoss$btConfirm),
                                  `disabled` = "true")
            )))
        }
        observeEvent(input$btConfirmDataLoss, {
          disableEl(session, paste0("#", session$ns("btCancelMigration")))
          on.exit(enableEl(session, paste0("#", session$ns("btCancelMigration"))))
          req(input$cbConfirmDataLoss)
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = lang$nav$migrationModule$progress$title, value = 0)
          updateProgress <- function(){
            progress$inc(1/length(inconsistentTablesInfo),
                         detail = sprintf(lang$nav$migrationModule$progress$desc,
                                          progress$getValue() + 1L, length(inconsistentTablesInfo)))
          }
          removeModal()
          confirmDataLoss(TRUE)
          tryCatch({
            dbMigrator$migrateDb(getMigrationConfig(),
                                 forceRemove = TRUE, callback = updateProgress)
            showElReplaceTxt(session, paste0("#", session$ns("migrationSuccess")),
                             lang$nav$migrationModule$successMsg)
            returnCode(0L)
          }, error_bad_settings = function(e){
            flog.info("Bad settings: %s", conditionMessage(e))
            showHideEl(session, paste0("#", session$ns("dataMigrationErrors")),
                       4000L, conditionMessage(e))
          }, error = function(e){
            flog.error("Problems migrating database. Error message: %s", conditionMessage(e))
            showHideEl(session, paste0("#", session$ns("dataMigrationErrors")),
                       4000L, lang$errMsg$unknownError)
          })
        })
        observeEvent(input$btConfirmMigration, {
          hideEl(session, paste0("#", session$ns("dataMigrationErrors")))
          disableEl(session, paste0("#", session$ns("btCancelMigration")))
          on.exit(enableEl(session, paste0("#", session$ns("btCancelMigration"))))
          progress <- shiny::Progress$new()
          on.exit(progress$close(), add = TRUE)
          progress$set(message = lang$nav$migrationModule$progress$title, value = 0)
          updateProgress <- function(){
            progress$inc(1/length(inconsistentTablesInfo),
                         detail = sprintf(lang$nav$migrationModule$progress$desc,
                                          progress$getValue() + 1L, length(inconsistentTablesInfo)))
          }
          tryCatch({
            dbMigrator$migrateDb(getMigrationConfig(),
                                 forceRemove = FALSE, callback = updateProgress)
            showEl(session, paste0("#", session$ns("migrationSuccess")))
            returnCode(0L)
          }, error_data_loss = function(e){
            flog.info("Migrating data with current config will lead to loss of data: %s. Confirmation required.", conditionMessage(e))
            showDataLossConfirmationDialog(conditionMessage(e))
          }, error_bad_settings = function(e){
            flog.info("Bad settings: %s", conditionMessage(e))
            showHideEl(session, paste0("#", session$ns("dataMigrationErrors")),
                       4000L, conditionMessage(e))
          }, error = function(e){
            flog.error("Problems migrating database. Error message: %s", conditionMessage(e))
            showHideEl(session, paste0("#", session$ns("dataMigrationErrors")),
                       4000L, lang$errMsg$unknownError)
          })
        })
        observeEvent(input$btCancelMigration, {
          if(is.null(returnCode())){
            returnCode(1L)
          }
        })
        removeDbTablesServer("removeAllButton", errorContainerId = session$ns("dataMigrationErrors"),
                             successContainerId = session$ns("migrationSuccess"), returnCode)
        return(returnCode)
      }
      migrationConfig <- reactiveVal(NULL)
      showDataLossConfirmationDialog <- function(errMsg){
        isolate(confirmDataLoss(FALSE))
        showElReplaceTxt(session, paste0("#", session$ns("dataLossDialogMsg")), errMsg)
        hideEl(session, paste0("#", session$ns("migFormContainer")))
        showEl(session, paste0("#", session$ns("dataLossDialog")))
      }
      observeEvent(input$btCancelDataLoss, {
        hideEl(session, paste0("#", session$ns("dataLossDialog")))
        showEl(session, paste0("#", session$ns("migFormContainer")))
        updateCheckboxInput(session, "cbConfirmDataLoss", value = FALSE)
      })
      observeEvent(input$btConfirmDataLoss, {
        req(input$cbConfirmDataLoss)
        confirmDataLoss(TRUE)
        tryCatch({
          migrationConfig(getMigrationConfig())
        }, error = function(e){
          flog.error("Problems migrating database. Error message: %s", conditionMessage(e))
          write("\n", stderr())
          write("merr:::500", stderr())
        })
      })
      observeEvent(input$btConfirmMigration, {
        tryCatch({
          migrationConfig(getMigrationConfig())
        }, error_data_loss = function(e){
          flog.info("Migrating data with current config will lead to loss of data: %s. Confirmation required.", conditionMessage(e))
          showDataLossConfirmationDialog(conditionMessage(e))
        }, error_bad_settings = function(e){
          flog.info("Bad settings: %s", conditionMessage(e))
          showHideEl(session, paste0("#", session$ns("dataMigrationErrors")),
                     4000L, conditionMessage(e))
        }, error = function(e){
          flog.error("Problems migrating database. Error message: %s", conditionMessage(e))
          showHideEl(session, paste0("#", session$ns("dataMigrationErrors")),
                     4000L, lang$errMsg$unknownError)
        })
      })
      return(migrationConfig)
    }
  )
}

migrateFromConfig <- function(path){
  migrationConfig <- fromJSON(path, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  noTablesMigrated <- 1L
  updateProgress <- function(){
    write("\n", stderr())
    write(paste0("mmigprog:::",
                 round(noTablesMigrated/length(inconsistentTablesInfo) * 100)), 
          stderr())
    noTablesMigrated <<- noTablesMigrated + 1L
  }
  tryCatch({
    dbMigrator$migrateDb(migrationConfig,
                         forceRemove = TRUE, callback = updateProgress)
    write("\n", stderr())
    write("mmigprog:::200", stderr())
  }, error = function(e){
    flog.error("Problems migrating database. Error message: %s", conditionMessage(e))
    write("\n", stderr())
    write("merr:::500", stderr())
  })
}
