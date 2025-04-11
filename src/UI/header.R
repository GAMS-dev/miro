# UI header
header <- dashboardHeader(
  tags$li(
    class = "dropdown",
    tags$a(
      href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
      lang$nav$header$scenario$title, tags$span(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu", role = "menu",
      tags$li(actionLink("btRemoveDuplicates", tagList(tags$div(class = "menu-icon-align", tags$i(class = "fa fa-diagram-next")), lang$nav$header$scenario$removeDuplicates))),
      if (!config$activateModules$readonlyMode) tags$li(actionLink("btEditMeta", HTML(paste0('<div class="menu-icon-align"><i class="fa fa-magnifying-glass"></i></div> ', lang$nav$header$scenario$edit)))),
      if (!config$activateModules$readonlyMode) tags$li(actionLink("btSave", HTML(paste0('<div class="menu-icon-align"><i class="fa fa-floppy-disk"></i></div> ', lang$nav$header$scenario$save)))),
      if (!config$activateModules$readonlyMode) tags$li(actionLink("btSaveAs", HTML(paste0('<div class="menu-icon-align"></div> ', lang$nav$header$scenario$saveAs)))),
      tags$li(tags$a(
        href = "#", class = "action-button",
        onclick = "Shiny.setInputValue('btExportScen', 1, {priority: 'event'})",
        tags$div(
          class = "menu-icon-align", tags$i(class = "fa fa-file-export")
        ),
        lang$nav$header$scenario$export
      )),
      if (!config$activateModules$readonlyMode) {
        tags$li(tags$a(
          href = "#", class = "action-button",
          id = "btDelete",
          tags$div(
            class = "menu-icon-align", tags$i(class = "fa fa-trash")
          ),
          lang$nav$header$scenario$delete
        ))
      }
    )
  ),
  tags$li(
    class = "dropdown",
    tags$a(
      href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
      lang$nav$header$help$title, tags$span(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu", role = "menu",
      tags$li(tags$a(
        href = "https://www.gams.com/miro/",
        target = "_blank",
        tags$div(
          class = "menu-icon-align",
          tags$i(class = "fa fa-book")
        ),
        lang$nav$header$help$doc
      )),
      tags$li(tags$a(
        href = "https://forum.gams.com/c/gams-miro/12",
        target = "_blank",
        tags$div(
          class = "menu-icon-align",
          tags$i(class = "fa fa-globe")
        ),
        lang$nav$header$help$forum
      )),
      tags$li(tags$a(
        href = "#", class = "action-button", id = "btShowCommandPalette",
        tags$div(
          class = "menu-icon-align",
          tags$i(class = "fa fa-terminal")
        ),
        lang$nav$header$help$commandPalette
      )),
      tags$li(
        tags$a(
          hred = "#",
          class = "action-button",
          onclick = paste0(
            "Miro.confirmModalShow('About GAMS MIRO','",
            aboutDialogText,
            "', 'Cancel');"
          ),
          tags$div(
            class = "menu-icon-align",
            tags$i(class = "fa fa-question")
          ),
          lang$nav$header$help$about
        )
      )
    )
  ),
  title = config$pageTitle, disable = FALSE
)
