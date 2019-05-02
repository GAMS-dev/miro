# UI header
header <- dashboardHeader(
  if(config$activateModules$hcubeMode || isShinyProxy){
    tags$li(class = "dropdown")
  }else{
    tags$li(title = lang$nav$header$tooltips$switchToHcube, class = "dropdown", 
            actionLink(inputId = "switchToHcube",  
                       label = NULL,
                       icon("cube"),
                       icon("arrow-right"),
                       onclick = "showSpinnerIcon(this, 5000)"))
  },
  if(config$activateModules$scenario){
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 if(config$activateModules$hcubeMode) lang$nav$header$scenario$titleHC
                 else lang$nav$header$scenario$title, tags$span(class="caret")),
          tags$ul(class = "dropdown-menu", role="menu",
                  tags$li(actionLink("btEditMeta", lang$nav$header$scenario$edit)),
                  tags$li(actionLink("btSave", lang$nav$header$scenario$save)),
                  tags$li(actionLink("btSaveAs", lang$nav$header$scenario$saveAs)),
                  tags$li(HTML(paste0('<a href="#" class="action-button" 
                                      onclick="Shiny.setInputValue(\'btExportScen\', 1, {priority: \'event\'})">',
                                      lang$nav$header$scenario$export, '</a>'))),
                  tags$li(actionLink("btDelete", lang$nav$header$scenario$delete))))
    }else{
      tags$li(class = "dropdown")
    },
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 lang$nav$header$help$title, tags$span(class="caret")),
            tags$ul(class = "dropdown-menu", role="menu",
                    tags$li(tags$a(href = "https://www.gams.com/miro/", 
                           target = "_blank", lang$nav$header$help$doc)),
                    tags$li(HTML(paste0('<a href="#" class="action-button" onclick="confirmModalShow(\'',
                                        'About GAMS MIRO\', \'', 
                                        htmltools::htmlEscape(aboutDialogText), '\', \'Cancel\')">',
                                        lang$nav$header$help$about, '</a>'))
                    ))),
  title=config$pageTitle, disable = FALSE)