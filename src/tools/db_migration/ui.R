uiDbMig <- fluidPage(
  tags$head(
    tags$meta(name = "color-scheme", content = "dark light"),
    tags$link(type = "text/css", rel = "stylesheet", href = "default_browser.css"),
    tags$script(`defer src` = "miro.js", type = "application/javascript")
  ),
  tagList(
    tags$head(tags$title(lang$nav$migrationModule$title)),
    h1(style = "text-align:center", lang$nav$migrationModule$title)
  ),
  dbMigrationForm("migration", inconsistentTablesInfo, orphanedTablesInfo,
    standalone = TRUE
  )
)
