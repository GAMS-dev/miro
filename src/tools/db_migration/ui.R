uiDbMig <- fluidPage(
  tags$head(
    tags$link(type = "text/css", rel = "stylesheet", href = "skin_browser.css"),
    tags$script(src = "miro.js", type = "application/javascript")
  ),
  tagList(
    tags$head(tags$title(lang$nav$migrationModule$title)),
    h1(style = "text-align:center", lang$nav$migrationModule$title)
  ),
  dbMigrationForm("migration", inconsistentTablesInfo, orphanedTablesInfo,
                  includeRemoveAllButton = TRUE)
)
