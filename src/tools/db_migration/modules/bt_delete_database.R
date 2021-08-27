removeDbTablesButton <- function(id) {
  ns <- NS(id)
  actionButton(ns("btRemoveAllDbTables"),
    class = "bt-remove",
    lang$nav$migrationModule$btRemoveTables$label
  )
}
removeDbTablesServer <- function(id, errorContainerId = NULL,
                                 successContainerId = NULL, returnCode = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      showRemoveAllDbTablesDialog <- function() {
        showModal(modalDialog(
          title = lang$nav$migrationModule$btRemoveTables$confirmTitle,
          lang$nav$migrationModule$btRemoveTables$confirmDesc,
          checkboxInput_MIRO(
            session$ns("cbRemoveAllDbTables"),
            lang$nav$migrationModule$btRemoveTables$cbConfirm
          ),
          footer = tagList(
            modalButton(lang$nav$migrationModule$btRemoveTables$btCancel),
            tagAppendAttributes(
              actionButton(session$ns("btRemoveAllDbTablesConfirm"),
                lang$nav$migrationModule$btRemoveTables$btConfirm,
                class = "bt-highlight-1 bt-gms-confirm"
              ),
              `disabled` = "true"
            )
          )
        ))
      }
      observeEvent(input$btRemoveAllDbTables, {
        flog.debug("Button to remove all database tables clicked")
        showRemoveAllDbTablesDialog()
      })
      observe({
        if (identical(input$cbRemoveAllDbTables, TRUE)) {
          enableEl(session, paste0("#", session$ns("btRemoveAllDbTablesConfirm")))
        } else {
          disableEl(session, paste0("#", session$ns("btRemoveAllDbTablesConfirm")))
        }
      })
      observeEvent(input$btRemoveAllDbTablesConfirm, {
        flog.debug("Button to confirm to remove all database tables clicked")
        req(input$cbRemoveAllDbTables)
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = lang$nav$migrationModule$progress$title, value = 0)
        removeModal()
        tryCatch(
          {
            dbMigrator$removeTablesModel()
            flog.debug("All database tables removed")
            if (!is.null(successContainerId)) {
              if (is.null(returnCode)) {
                showHideEl(
                  session, paste0("#", successContainerId),
                  4000L, lang$nav$migrationModule$btRemoveTables$msgSuccess
                )
              } else {
                showElReplaceTxt(
                  session, paste0("#", successContainerId),
                  lang$nav$migrationModule$btRemoveTables$msgSuccess
                )
              }
            }
            if (!is.null(returnCode)) {
              returnCode(0L)
            }
          },
          error = function(e) {
            flog.error(
              "Problems removing database tables. Error message: %s",
              conditionMessage(e)
            )
            if (!is.null(errorContainerId)) {
              showHideEl(
                session, paste0("#", errorContainerId),
                4000L, lang$errMsg$unknownError
              )
            }
          }
        )
      })
    }
  )
}
