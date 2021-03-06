addArrayEl <- function(session, arrayID, plotlyChartType = "", defaults = NULL, destroy = FALSE, symbolName = NULL){
  arrayID <- paste0(arrayID, plotlyChartType)
  session$sendCustomMessage("gms-addArrayEl", list(arrayID = arrayID, defaults = defaults, destroy = destroy,
                                                   symbol = symbolName))
}
createArray <- function(session, arrayID, label, plotlyChartType = "", autoCreate = TRUE, 
                        class_outer = "array-wrapper-outer-default", hr = TRUE, symbolName = NULL){
  if(isTRUE(autoCreate)){
    addArrayEl(session, arrayID, plotlyChartType, destroy = TRUE, 
               symbolName = symbolName)
  }else if(length(session)){
    # destroy existing array elements
    session$sendCustomMessage("gms-destroyArray", paste0(arrayID, plotlyChartType))
  }
  
  arrayID <- paste0(arrayID, plotlyChartType)
  HTML(paste0('<div id="', arrayID, '_wrapper" ', 
              if(length(symbolName)) paste0('data-symbol="', symbolName, '" ') else '',
              'class="shiny-input-container ', class_outer, '">\n', 
              if ( hr ) "<hr>\n" else '',
              '<div class="array-wrapper"></div>\n
   <div onclick="Miro.addArrayDataEl(\'', arrayID, '\')" style="cursor:pointer">\n
     <button type="button" class="btn btn-default bt-icon btn-add-array-el" style="font-size:20px;">\n
       <i class="far fa-plus-square" role="presentation" aria-label="Add new array element"></i>\n
     </button>\n', label, '\n
  </div>
</div>'))
}
checkLength <- function(configuredWithThisTool = FALSE, el = NULL, alt = NULL){
  if(isTRUE(configuredWithThisTool) && length(el))
    return(el)
  else
    return(alt)
}
checkTRUE <- function(configuredWithThisTool = FALSE, el = NULL){
  if(isTRUE(configuredWithThisTool))
    return(isTRUE(el))
  else
    return(FALSE)
}
checkNotFALSE <- function(configuredWithThisTool = FALSE, el = NULL){
  if(isTRUE(configuredWithThisTool))
    return(!isFALSE(el))
  else
    return(TRUE)
}
labelTooltip <- function(label = NULL, tooltip = NULL, href = NULL){
  return(
    tags$div(label, 
             tags$a("", title = tooltip, class="info-wrapper", href = href, 
                    tags$span(class="fas fa-info-circle", class="info-icon",
                              role = "presentation",
                              `aria-label` = "More information"), target="_blank"))
  )
}
optionSection <- function(title, ..., collapsed = FALSE){
  tags$div(class = "shiny-input-container", style = "min-height:30px;",
           tags$h4(class = "box-title option-section-header", title, if(isFALSE(collapsed)) icon("minus") else icon("plus"), 
                   style = "cursor:pointer;font-weight:bold;", 
                   onclick = "$(this).next().toggle();$(this).children('.fa').toggleClass('fa-plus fa-minus');"),
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
Validator <- R6Class("Validator", public = list(
  initialize = function(keys, data = NULL, requiredKeys = NULL){
    if(!length(requiredKeys)){
      requiredKeys <- keys
    }
    private$keys <- keys
    private$template <- vector("list", length(requiredKeys))
    names(private$template) <- requiredKeys
    private$template$isValid <- FALSE
    
    if(length(data)){
      private$data <- lapply(data, function(el){
        return(c(el, isValid = TRUE))
      })
      names(private$data) <- as.character(seq_along(data))
      private$validData <- data
    }
    invisible(self)
  },
  getValid = function(key){
    return(lapply(private$data, function(el){
      if(isTRUE(el[["isValid"]])){
        return(el[[key]])
      }
    }))
  },
  getValidData = function(){
    if(private$cacheClean){
      return(private$validData)
    }
    data <- lapply(unname(private$data), function(el){
      if(isTRUE(el[["isValid"]])){
        el[["isValid"]] <- NULL
        return(el)
      }
      return(NULL)
    })
    data[vapply(data, is.null, logical(1), USE.NAMES = FALSE)] <- NULL
    private$validData <- data
    private$cacheClean <- TRUE
    return(data)
  },
  removeEl = function(id){
    private$data[[as.character(id)]] <- NULL
    private$cacheClean <- FALSE
    invisible(self)
  },
  removeKey = function(id, key){
    private$data[[as.character(id)]][[key]] <- NULL
    private$cacheClean <- FALSE
    if(!key %in% names(private$template)){
      return(invisible(self))
    }
    # required key
    private$data[[as.character(id)]] <- c(private$data[[as.character(id)]], 
                                          setNames(list(NULL), key))
    private$data[[as.character(id)]][["isValid"]] <- FALSE
    
    invisible(self)
  },
  setVal = function(id, key, val){
    id <- as.character(id)
    if(!id %in% names(private$data)){
      private$data[[id]] <- private$template
      private$data[[id]][[key]] <- val
    }else{
      private$data[[id]][[key]] <- val
      if(!any(vapply(private$data[[id]], is.null, 
                     logical(1L), USE.NAMES = FALSE))){
        private$data[[id]][["isValid"]] <- TRUE
        private$cacheClean <- FALSE
      }
    }
    invisible(self)
  }
), private = list(
  cacheClean = TRUE,
  keys = character(1L),
  template = NULL,
  validData = list(),
  data = list()
))
setInputValue <- function(session, id, value){
  session$sendCustomMessage("gms-setInputValue", list(id = id, value = value))
}
getMIROPivotOptions <- function(currentConfig, prefix = "", pivotComp = FALSE){
  tagList(
    checkboxInput_MIRO(paste0(prefix, "enableHideEmptyCols"),
                       if(pivotComp) lang$adminMode$graphs$miroPivotOptions$pivotCompHideEmptyColsSwitch
                       else lang$adminMode$graphs$miroPivotOptions$hideEmptyColsSwitch,
                       value = isTRUE(currentConfig$enableHideEmptyCols)),
    conditionalPanel(paste0("input.", prefix, "enableHideEmptyCols===true"),
                     tags$div(class = "form-group shiny-input-container",
                              tags$label(class = "control-label", "for" = paste0(prefix, "emptyUEL"),
                                         lang$adminMode$graphs$miroPivotOptions$emptyUEL),
                              tags$input(id = paste0(prefix, "emptyUEL"), class = "form-control must-not-be-empty",
                                         type = "text",
                                         value = if(length(currentConfig$emptyUEL))
                                           currentConfig$emptyUEL else "-"))),
    if(!pivotComp)
      tags$div(id = "miroPivotInfoMsg", class="config-message", 
               style = "display:block;",
               lang$adminMode$graphs$miroPivotOptions$infoMsg))
}
