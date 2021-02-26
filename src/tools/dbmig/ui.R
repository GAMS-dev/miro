migrationUI <- function(id, inconsistentTablesInfo, orphanedTablesInfo) {
  ns <- NS(id)
  tags$div(class = "container",
           tags$div(id = ns("migrationSuccess"), class = "gmsalert gmsalert-success",
                    lang$nav$migrationModule$successMsg),
           tags$div(id = ns("dataMigrationErrors"), class = "gmsalert gmsalert-error",
                    style = "white-space: pre-wrap;"),
           tags$p(lang$nav$migrationModule$desc),
           tags$b(lang$nav$migrationModule$backupWarning),
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
             tags$div(class = "row", style = "border-bottom: 5px solid #000;",
                      tags$div(class = colClass,
                               selectInput(ns(paste0("dbMigrateTable_", i)),
                                           sprintf(lang$nav$migrationModule$selectTableToMap,
                                                   tableInfo$tabName),
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
                    actionButton(ns("btConfirmMigration"), class = "bt-highlight-1",
                                 lang$nav$migrationModule$btConfirmMigration)
           ))
}
uiDbMig <- fluidPage(
  tags$head(
    tags$link(type = "text/css", rel = "stylesheet", href = "skin_browser.css"),
    tags$script(src = "miro.js", type = "application/javascript")
  ),
  tagList(
    tags$head(tags$title(lang$nav$migrationModule$title)),
    h2(style = "text-align:center", lang$nav$migrationModule$title)
  ),
  migrationUI("migration", inconsistentTablesInfo, orphanedTablesInfo)
)
