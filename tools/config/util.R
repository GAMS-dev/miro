addArrayEl <- function(session, arrayID, plotlyChartType = "", defaults = NULL){
  arrayID <- paste0(arrayID, plotlyChartType)
  session$sendCustomMessage("gms-addArrayEl", list(arrayID = arrayID, defaults = defaults))
}
createArray <- function(session, arrayID, label, plotlyChartType = "", autoCreate = TRUE, 
                        class_outer = "array-wrapper-outer-default", hr = TRUE){
  if(autoCreate)
    addArrayEl(session, arrayID, plotlyChartType)
  arrayID <- paste0(arrayID, plotlyChartType)
  HTML(paste0('<div id="', arrayID, '_wrapper" class="shiny-input-container ', class_outer, '">\n', 
              if ( hr ) "<hr>\n" else '',
 '<div class="array-wrapper"></div>\n
   <div onclick="Miro.addArrayDataEl(\'', arrayID, '\')" style="cursor:pointer">\n
     <button type="button" class="btn btn-default bt-icon btn-add-array-el" style="font-size:20px;">\n
       <i class="far fa-plus-square"></i>\n
     </button>\n', label, '\n
  </div>
</div>'))
}
optionSection <- function(title, ..., collapsed = FALSE){
  tags$div(class = "shiny-input-container", style = "min-height:30px;",
           tags$h4(class = "box-title option-section-header", title, icon("plus"), style = "cursor:pointer;font-weight:bold;", 
                   onclick = "$(this).next().toggle();"),
           tags$div(class = "option-section", ..., style = if(collapsed) "display:none;" else "")
  )
}
colorPickerInput <- function(id, label = NULL, value = NULL){
  HTML(paste0('<div class="form-group shiny-input-container">\n
    <label for="', id, '">', label, '</label>\n
      <input id="', id, '" type="text" class="form-control miro-color-picker" value="', value, '" />\n
    </div>'))
}
isNonemptyDataset <- function(datasets){
  vapply(datasets, function(el){
    if(length(el) && nrow(el))
      FALSE
    else
      TRUE
  }, logical(1L), USE.NAMES = FALSE)
}