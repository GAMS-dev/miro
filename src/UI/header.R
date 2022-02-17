# UI header
header <- dashboardHeader(
  if (!isShinyProxy && config$activateModules$remoteExecution) {
    tags$li(
      class = "dropdown",
      HTML(paste0(
        '<a href="#" id="remoteExecLogoutDiv" class="action-button" ',
        if (!length(credConfig)) 'style="display:none;" ',
        'onclick="Miro.confirmModalShow(\'',
        lang$nav$dialogRemoteLogout$title, "', '",
        lang$nav$dialogRemoteLogout$desc, "', '",
        lang$nav$dialogRemoteLogout$cancelButton, "', '",
        lang$nav$dialogRemoteLogout$okButton,
        '\', \'Shiny.setInputValue(\\\'btRemoteExecLogout\\\', 1, {priority: \\\'event\\\'})\')"">',
        '<i class="fa fa-sign-out-alt" aria-hidden="true"></i> ',
        lang$nav$header$remoteExec$logout,
        '</a><a href="#" id="btRemoteExecLogin" class="action-button" ',
        if (length(credConfig)) 'style="display:none;" ',
        ">", lang$nav$header$remoteExec$login, ' <i class="fa fa-sign-in-alt" aria-hidden="true"></i></a>'
      ))
    )
  } else {
    tags$li(class = "dropdown")
  },
  tags$li(
    class = "dropdown",
    tags$a(
      href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
      lang$nav$header$scenario$title, tags$span(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu", role = "menu",
      tags$li(actionLink("btEditMeta", HTML(paste0('<div class="menu-icon-align"><i class="fa fa-edit"></i></div> ', lang$nav$header$scenario$edit)))),
      tags$li(actionLink("btSave", HTML(paste0('<div class="menu-icon-align"><i class="fa fa-save"></i></div> ', lang$nav$header$scenario$save)))),
      tags$li(actionLink("btSaveAs", HTML(paste0('<div class="menu-icon-align"></div> ', lang$nav$header$scenario$saveAs)))),
      tags$li(HTML(paste0(
        '<a href="#" class="action-button"
                                      onclick="Shiny.setInputValue(\'btExportScen\', 1, {priority: \'event\'})">
                                      <div class="menu-icon-align"><i class="fa fa-file-export"></i></div> ',
        lang$nav$header$scenario$export, "</a>"
      ))),
      tags$li(actionLink("btDelete", HTML(paste0('<div class="menu-icon-align"><i class="fa fa-trash"></i></div> ', lang$nav$header$scenario$delete))))
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
        href = "https://forum.gamsworld.org/viewforum.php?f=14",
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
