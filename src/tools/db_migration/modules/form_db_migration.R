dbMigrationForm <- function(id, inconsistentTablesInfo, orphanedTablesInfo,
                            includeRemoveAllButton = FALSE) {
  ns <- NS(id)
  tags$div(class = "container", style = "font-size:12pt;",
           tags$div(id = ns("migrationSuccess"), class = "gmsalert gmsalert-success",
                    lang$nav$migrationModule$successMsg),
           tags$div(id = ns("dataMigrationErrors"), class = "gmsalert gmsalert-error",
                    style = "white-space:pre-wrap;"),
           tags$p(lang$nav$migrationModule$desc),
           tags$div(style = "text-align:center;margin-bottom:20px;",
                    tags$b(lang$nav$migrationModule$backupWarning)),
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
             colClass <- paste0("col-xs-", floor(12/(length(tableInfo$colNames) + 1L)))
             tags$div(class = "row", style = "border-top: 5px solid #000;padding:20px 0;",
                      tags$h3(style="text-align:center;",
                              sprintf(lang$nav$migrationModule$labelSymbol,
                                      tableInfo$tabName)),
                      tags$div(class = colClass,
                               selectInput(ns(paste0("dbMigrateTable_", i)),
                                           lang$nav$migrationModule$selectTableToMap,
                                           tableMapping)),
                      lapply(seq_along(tableInfo$colNames), function(j){
                        if(length(tableInfo$currentColNames)){
                          if(identical(colTypes[j], "c")){
                            colChoices <- c("-", textCols)
                          }else{
                            colChoices <- c("-", numericCols)
                          }
                        }else{
                          colChoices <- "-"
                        }
                        tags$div(class = colClass,
                                 selectInput(ns(paste0("dbMigrateTable_", i, "_", j)),
                                             tableInfo$colNames[j],
                                             colChoices,
                                             selected = if(tableInfo$colNames[j] %in% colChoices)
                                               tableInfo$colNames[j] else "-"
                                 ))
                      })
             )
           }),
           tags$div(class = "row",
                    actionButton(ns("btCancelMigration"),
                                 lang$nav$migrationModule$btCancelMigration),
                    if(includeRemoveAllButton)
                      removeDbTablesButton(ns("removeAllButton")),
                    actionButton(ns("btConfirmMigration"), class = "bt-highlight-1",
                                 lang$nav$migrationModule$btConfirmMigration)
           ))
}

dbMigrationServer <- function(id, inconsistentTablesInfo, orphanedTablesInfo,
                              includeRemoveAllButton = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      returnCode <- reactiveVal(NULL)
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
            if(identical(colTypes[j], "c")){
              colChoices <- c("-", textCols)
            }else{
              colChoices <- c("-", numericCols)
            }
            updateSelectInput(session, paste0("dbMigrateTable_", i, "_", j),
                              choices = colChoices,
                              selected = if(tableInfo$colNames[j] %in% colChoices)
                                tableInfo$colNames[j] else "-")
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
                        sprintf(lang$nav$migrationModule$dialogConfirmDataLoss$errDuplicateTables,
                                paste(tablesMigrated[duplicated(tablesMigrated)], collapse = "', '")),
                        call. = FALSE)
          }
          names(migrationConfig) <- vapply(inconsistentTablesInfo, "[[",
                                           character(1L), "tabName", USE.NAMES = FALSE)
          return(migrationConfig)
        })
      })
      
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
      observe({
        if(isTRUE(input$cbConfirmDataLoss)){
          enableEl(session, paste0("#", session$ns("btConfirmDataLoss")))
        }else{
          disableEl(session, paste0("#", session$ns("btConfirmDataLoss")))
        }
      })
      observeEvent(input$btConfirmDataLoss, {
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
        }, error = function(e){
          flog.error("Problems migrating database. Error message: %s", conditionMessage(e))
          showHideEl(session, paste0("#", session$ns("dataMigrationErrors")),
                     4000L, lang$errMsg$unknownError)
        })
      })
      observeEvent(input$btConfirmMigration, {
        hideEl(session, "#dataMigrationErrors")
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = lang$nav$migrationModule$progress$title, value = 0)
        updateProgress <- function(){
          progress$inc(1/length(inconsistentTablesInfo),
                       detail = sprintf(lang$nav$migrationModule$progress$title,
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
        returnCode(1L)
      })
      if(includeRemoveAllButton){
        removeDbTablesServer("removeAllButton", errorContainerId = session$ns("dataMigrationErrors"),
                             successContainerId = session$ns("migrationSuccess"), returnCode)
      }
      return(returnCode)
    }
  )
}
