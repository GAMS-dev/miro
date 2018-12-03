# UI header
header <- dashboardHeader(
  if(config$activateModules$scenario && !identical(config$activateModules$batchMode, TRUE)){
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 lang$nav$header$scenario$title, tags$span(class="caret")),
          tags$ul(class = "dropdown-menu", role="menu",
                  tags$li(actionLink("btEditMeta", lang$nav$header$scenario$edit)),
                  tags$li(actionLink("btSave", lang$nav$header$scenario$save)),
                  tags$li(actionLink("btSaveAs", lang$nav$header$scenario$saveAs)),
                  tags$li(downloadLink(outputId = "export_1", lang$nav$header$scenario$export)),
                  tags$li(actionLink("btDelete", lang$nav$header$scenario$delete))))
    }else{
      tags$li(class = "dropdown")
    },
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 lang$nav$header$help$title, tags$span(class="caret")),
            tags$ul(class = "dropdown-menu", role="menu",
                    tags$li(tags$a(href = "https://www.gams.com/latest/webui/", 
                           target = "_blank", lang$nav$header$help$doc)),
                    tags$li(actionLink("aboutDialog", lang$nav$header$help$about)))),
  title=config$pageTitle, disable = FALSE)