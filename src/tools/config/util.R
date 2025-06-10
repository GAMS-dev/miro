addArrayEl <- function(session, arrayID, plotlyChartType = "", defaults = NULL, destroy = FALSE, symbolName = NULL) {
  arrayID <- paste0(arrayID, plotlyChartType)
  session$sendCustomMessage("gms-addArrayEl", list(
    arrayID = arrayID, defaults = defaults, destroy = destroy,
    symbol = symbolName
  ))
}
createArray <- function(session, arrayID, label, plotlyChartType = "", autoCreate = TRUE,
                        class_outer = "array-wrapper-outer-default", hr = TRUE, symbolName = NULL) {
  if (isTRUE(autoCreate)) {
    addArrayEl(session, arrayID, plotlyChartType,
      destroy = TRUE,
      symbolName = symbolName
    )
  } else if (length(session)) {
    # destroy existing array elements
    session$sendCustomMessage("gms-destroyArray", paste0(arrayID, plotlyChartType))
  }

  arrayID <- paste0(arrayID, plotlyChartType)
  HTML(paste0(
    '<div id="', arrayID, '_wrapper" ',
    if (length(symbolName)) paste0('data-symbol="', symbolName, '" ') else "",
    'class="shiny-input-container ', class_outer, '">\n',
    if (hr) "<hr>\n" else "",
    '<div class="array-wrapper"></div>\n
   <div onclick="Miro.addArrayDataEl(\'', arrayID, '\')" style="cursor:pointer">\n
     <button type="button" class="btn btn-default bt-icon btn-add-array-el" style="font-size:20px;">\n
       <i class="far fa-square-plus" role="presentation" aria-label="Add new array element"></i>\n
     </button>\n', label, "\n
  </div>
</div>"
  ))
}
checkLength <- function(configuredWithThisTool = FALSE, el = NULL, alt = NULL) {
  if (isTRUE(configuredWithThisTool) && length(el)) {
    return(el)
  } else {
    return(alt)
  }
}
checkTRUE <- function(configuredWithThisTool = FALSE, el = NULL) {
  if (isTRUE(configuredWithThisTool)) {
    return(isTRUE(el))
  } else {
    return(FALSE)
  }
}
checkNotFALSE <- function(configuredWithThisTool = FALSE, el = NULL) {
  if (isTRUE(configuredWithThisTool)) {
    return(!isFALSE(el))
  } else {
    return(TRUE)
  }
}
labelTooltip <- function(label = NULL, tooltip = NULL, href = NULL) {
  if (is.null(href)) {
    return(
      tags$span(
        label,
        tags$span(
          title = tooltip,
          class = "info-wrapper tooltip-mobile",
          tags$span(
            class = "fas fa-circle-info", class = "info-icon",
            role = "presentation",
            `aria-label` = "More information"
          )
        )
      )
    )
  }
  return(
    tags$div(
      label,
      tags$a("",
        title = tooltip, class = "info-wrapper", href = href,
        tags$span(
          class = "fas fa-circle-info", class = "info-icon",
          role = "presentation",
          `aria-label` = "More information"
        ), target = "_blank"
      )
    )
  )
}
optionSection <- function(title, ..., collapsed = FALSE) {
  tags$div(
    class = "shiny-input-container", style = "min-height:30px;",
    tags$h4(
      class = "box-title option-section-header", title, if (isFALSE(collapsed)) icon("minus") else icon("plus"),
      style = "cursor:pointer;font-weight:bold;",
      onclick = "$(this).next().toggle();$(this).children('.fa').toggleClass('fa-plus fa-minus');"
    ),
    tags$div(class = "option-section", ..., style = if (collapsed) "display:none;" else "")
  )
}
isNonemptyDataset <- function(datasets) {
  vapply(datasets, function(el) {
    if (length(el) && nrow(el)) {
      FALSE
    } else {
      TRUE
    }
  }, logical(1L), USE.NAMES = FALSE)
}
Validator <- R6Class("Validator", public = list(
  initialize = function(keys, data = NULL, requiredKeys = NULL) {
    if (!length(requiredKeys)) {
      requiredKeys <- keys
    }
    private$keys <- keys
    private$template <- vector("list", length(requiredKeys))
    names(private$template) <- requiredKeys
    private$template$isValid <- FALSE

    if (length(data)) {
      private$data <- lapply(data, function(el) {
        return(c(el, isValid = TRUE))
      })
      names(private$data) <- as.character(seq_along(data))
      private$validData <- data
    }
    invisible(self)
  },
  getValid = function(key) {
    return(lapply(private$data, function(el) {
      if (isTRUE(el[["isValid"]])) {
        return(el[[key]])
      }
    }))
  },
  getValidData = function() {
    if (private$cacheClean) {
      return(private$validData)
    }
    data <- lapply(unname(private$data), function(el) {
      if (isTRUE(el[["isValid"]])) {
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
  removeEl = function(id) {
    private$data[[as.character(id)]] <- NULL
    private$cacheClean <- FALSE
    invisible(self)
  },
  removeKey = function(id, key) {
    private$data[[as.character(id)]][[key]] <- NULL
    private$cacheClean <- FALSE
    if (!key %in% names(private$template)) {
      return(invisible(self))
    }
    # required key
    private$data[[as.character(id)]] <- c(
      private$data[[as.character(id)]],
      setNames(list(NULL), key)
    )
    private$data[[as.character(id)]][["isValid"]] <- FALSE

    invisible(self)
  },
  setVal = function(id, key, val) {
    id <- as.character(id)
    if (!id %in% names(private$data)) {
      private$data[[id]] <- private$template
      private$data[[id]][[key]] <- val
    } else {
      private$data[[id]][[key]] <- val
      if (!any(vapply(private$data[[id]], is.null,
        logical(1L),
        USE.NAMES = FALSE
      ))) {
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
setInputValue <- function(session, id, value) {
  session$sendCustomMessage("gms-setInputValue", list(id = id, value = value))
}
aggregationFunctions <- setNames(
  c("sum", "count", "mean", "median", "min", "max", "sd"),
  c(
    lang$renderers$miroPivot$aggregationFunctions$sum,
    lang$renderers$miroPivot$aggregationFunctions$count,
    lang$renderers$miroPivot$aggregationFunctions$mean,
    lang$renderers$miroPivot$aggregationFunctions$median,
    lang$renderers$miroPivot$aggregationFunctions$min,
    lang$renderers$miroPivot$aggregationFunctions$max,
    lang$renderers$miroPivot$aggregationFunctions$sd
  )
)
getMIROPivotOptions <- function(currentConfig, prefix = "", pivotComp = FALSE) {
  tagList(
    tags$div(
      class = "shiny-input-container",
      checkboxInput_SIMPLE(paste0(prefix, "hidePivotControls"),
        lang$adminMode$graphs$miroPivotOptions$hidePivotControlsSwitch,
        value = isTRUE(currentConfig$hidePivotControls)
      ),
      if (pivotComp) {
        tagList(
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-6",
              checkboxInput_SIMPLE(paste0(prefix, "showTableSummaryCol"),
                label = lang$renderers$miroPivot$settings$cbShowTableSummaryCol,
                value = identical(currentConfig$tableSummarySettings$colEnabled, TRUE)
              )
            ),
            conditionalPanel(
              paste0("input.", prefix, "showTableSummaryCol===true"),
              tags$div(
                class = "col-sm-6",
                selectInput(paste0(prefix, "colSummaryFunction"),
                  label = lang$renderers$miroPivot$settings$colSummaryFunction,
                  choices = aggregationFunctions,
                  selected = currentConfig$tableSummarySettings$colSummaryFunction,
                  width = "100%"
                )
              )
            )
          ),
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-6",
              checkboxInput_SIMPLE(paste0(prefix, "showTableSummaryRow"),
                label = lang$renderers$miroPivot$settings$cbShowTableSummaryRow,
                value = identical(currentConfig$tableSummarySettings$rowEnabled, TRUE)
              )
            ),
            conditionalPanel(
              paste0("input.", prefix, "showTableSummaryRow===true"),
              tags$div(
                class = "col-sm-6",
                selectInput(paste0(prefix, "rowSummaryFunction"),
                  label = lang$renderers$miroPivot$settings$rowSummaryFunction,
                  choices = aggregationFunctions[aggregationFunctions %in% c("sum", "count", "mean")],
                  selected = currentConfig$tableSummarySettings$rowSummaryFunction,
                  width = "100%"
                )
              )
            )
          ),
          checkboxInput_SIMPLE(paste0(prefix, "fixedColumns"),
            labelTooltip(
              lang$adminMode$graphs$miroPivotOptions$fixedColumnsSwitch,
              lang$adminMode$graphs$miroPivotOptions$fixedColumnsTooltip,
              "https://gams.com/miro/configuration_general.html#fixed-columns"
            ),
            value = !isFALSE(currentConfig$fixedColumns)
          ),
          checkboxInput_SIMPLE(paste0(prefix, "hideEmptyCols"), lang$renderers$miroPivot$settings$cbHideEmptyCols,
            value = isTRUE(currentConfig$hideEmptyCols)
          ),
          numericInput(paste0(prefix, "chartFontSize"), lang$renderers$miroPivot$settings$chartFontSize,
            value = if (length(currentConfig$chartFontSize)) {
              currentConfig$chartFontSize
            } else {
              NULL
            },
            min = 1,
            max = 42,
            step = 1
          )
        )
      },
      tags$div(
        class = "form-group shiny-input-container",
        tags$label(
          class = "control-label", "for" = paste0(prefix, "emptyUEL"),
          labelTooltip(
            lang$adminMode$graphs$miroPivotOptions$emptyUEL,
            lang$adminMode$graphs$miroPivotOptions$emptyUELTooltip,
            "https://gams.com/miro/charts.html#hide-empty-columns"
          )
        ),
        tags$input(
          id = paste0(prefix, "emptyUEL"), class = "form-control must-not-be-empty",
          type = "text",
          value = if (length(currentConfig$emptyUEL)) {
            currentConfig$emptyUEL
          } else {
            "-"
          }
        )
      )
    ),
    if (!pivotComp) {
      tagList(
        tags$div(
          class = "shiny-input-container",
          checkboxInput_SIMPLE(paste0(prefix, "useExternalDefaultView"),
            lang$adminMode$graphs$miroPivotOptions$externalDefaultViewSwitch,
            value = length(currentConfig$externalDefaultView)
          )
        ),
        conditionalPanel(
          paste0("input.", prefix, "useExternalDefaultView===true"),
          tags$div(
            class = "form-group shiny-input-container",
            tags$label(
              class = "control-label", "for" = paste0(prefix, "externalDefaultView"),
              lang$adminMode$graphs$miroPivotOptions$externalDefaultViewName
            ),
            tags$input(
              id = paste0(prefix, "externalDefaultView"), class = "form-control must-not-be-empty",
              type = "text",
              value = if (length(currentConfig$externalDefaultView)) {
                currentConfig$externalDefaultView
              } else {
                ""
              }
            )
          )
        ),
        conditionalPanel(
          paste0("input.", prefix, "useExternalDefaultView!==true"),
          tags$div(
            id = "miroPivotInfoMsg", class = "config-message shiny-input-container",
            style = "display:block;",
            HTML(paste0(sprintf(htmltools::htmlEscape(lang$adminMode$graphs$miroPivotOptions$infoMsg), icon("cog"), icon("square-plus")), "."))
          )
        )
      )
    }
  )
}
parseFunctionBody <- function(textToParse, functionName) {
  # this is neither robust nor efficient, but seems good enough for our purposes
  # We don't want to use eval-deparse as this will remove comments/whitespace
  functionBodyTmp <- strsplit(textToParse, functionName, fixed = TRUE)[[1]]
  if (length(functionBodyTmp) < 2L) {
    stop(sprintf("Could not find function: %s", functionName), call. = FALSE)
  }
  functionBodyTmp <- paste(functionBodyTmp[-1], collapse = functionName)
  functionBodyTmp <- paste(strsplit(functionBodyTmp, "{", fixed = TRUE)[[1]][-1], collapse = "{")
  functionBodyTmp <- strsplit(functionBodyTmp, "", fixed = TRUE)[[1]]
  openingBracketCtr <- 1L
  isInComment <- FALSE
  isInString <- FALSE
  currentQuote <- NULL
  for (i in seq_along(functionBodyTmp)) {
    chr <- functionBodyTmp[[i]]
    if (isInComment) {
      if (identical(chr, "\n")) {
        isInComment <- FALSE
      }
      next
    }
    if (isInString) {
      if (identical(chr, currentQuote)) {
        isInString <- FALSE
      }
      next
    }
    if (identical(chr, "#")) {
      isInComment <- TRUE
      next
    } else if (chr %in% c("'", '"', "`")) {
      isInString <- TRUE
      currentQuote <- chr
      next
    } else if (identical(chr, "{")) {
      openingBracketCtr <- openingBracketCtr + 1L
    } else if (identical(chr, "}")) {
      openingBracketCtr <- openingBracketCtr - 1L
    }
    if (identical(openingBracketCtr, 0L)) {
      break
    }
  }
  functionBodyTmp <- trimws(paste(functionBodyTmp[seq_len(i - 1L)], collapse = ""),
    which = "right"
  )
  functionBodyTmp <- stri_split_lines(functionBodyTmp)[[1]]
  if (identical(nchar(trimws(functionBodyTmp[1])), 0L)) {
    functionBodyTmp <- functionBodyTmp[-1]
  }
  return(functionBodyTmp)
}
aceEditorFullscreenButton <- function() {
  tags$div(
    style = "position: relative;",
    tags$i(
      id = "aceFullscreenButton",
      class = "fas fa-expand-alt",
      style = "float: right;position: absolute;right: 10px;cursor: pointer;z-index: 1000;top: 5px;"
    )
  )
}
updateColorPickerInput <- function(session, inputId, value) {
  session$sendInputMessage(inputId, list(value = value))
}
isAdvanced <- function(cfg, mode = "light") {
  if (is.null(cfg) || length(cfg) == 0) {
    return(FALSE)
  }

  if (identical(mode, "dark")) {
    if (is.null(cfg$miro_primary_color_dark) && is.null(cfg$miro_main_bg_dark)) {
      return(FALSE)
    }
    sidebar <- hslHex(hue(cfg$miro_primary_color_dark), 6, 12)

    neutralPrimary <- (hue(cfg$miro_primary_color_dark) == 0) &&
      (saturation(cfg$miro_primary_color_dark) == 0)

    navbar <- if (neutralPrimary) {
      hslHex(hue(cfg$miro_primary_color_dark), 0, 10)
    } else {
      hslHex(hue(cfg$miro_primary_color_dark), 6, 12)
    }

    neutralBg <- (hue(cfg$miro_main_bg_dark) == 0) &&
      (saturation(cfg$miro_main_bg_dark) == 0)

    bodybg <- if (neutralBg) {
      hslHex(hue(cfg$miro_main_bg_dark), 0, 12)
    } else {
      hslHex(hue(cfg$miro_main_bg_dark), 10, 18)
    }

    defaults <- list(
      miro_sidebar_color_dark = sidebar,
      miro_body_bg_color_dark = bodybg,
      miro_navbar_color_dark  = navbar
    )
  } else {
    if (is.null(cfg$miro_primary_color)) {
      return(FALSE)
    }

    sidebar <- hslHex(hue(cfg$miro_primary_color), 6, 12)

    neutral <- (hue(cfg$miro_primary_color) == 0) &&
      (saturation(cfg$miro_primary_color) == 0)

    bodybg <- if (neutral) {
      makeHsl(cfg$miro_primary_color, 80, 0)
    } else {
      makeHsl(cfg$miro_primary_color, 94, 26)
    }

    defaults <- list(
      miro_sidebar_color = sidebar,
      miro_body_bg_color = bodybg,
      miro_navbar_color  = "#ffffff"
    )
  }

  keys <- intersect(names(cfg), names(defaults))

  return(any(vapply(keys, function(k) {
    !identical(tolower(cfg[[k]]), tolower(defaults[[k]]))
  }, logical(1))))
}
getThemeColors <- function(cssInput) {
  if (is.null(cssInput)) {
    return(NULL)
  }

  if ((is.list(cssInput) || is.atomic(cssInput)) && !is.null(names(cssInput))) {
    names(cssInput) <- gsub("-", "_", names(cssInput))
    return(cssInput)
  }

  cssLines <- if (length(cssInput) == 1 && file.exists(cssInput)) {
    read_lines(cssInput)
  } else if (is.character(cssInput)) {
    as.character(cssInput)
  } else {
    return(NULL)
  }
  themeColors <- strsplit(trimws(cssLines[-1], whitespace = "[ \t\r\n\\-;]"), ":", fixed = TRUE)
  themeColors <- themeColors[lapply(themeColors, length) == 2L]
  colorNames <- gsub("-", "_", lapply(themeColors, "[[", 1L), fixed = TRUE)
  colorValues <- as.list(trimws(lapply(themeColors, "[[", 2L), whitespace = "[ \t\r\n\"]"))
  names(colorValues) <- colorNames
  return(colorValues)
}
hslHex <- function(h, s, l) {
  return(colorspace::hex(colorspace::HLS(H = h, S = s / 100, L = l / 100)))
}
hexToHsl <- function(hex) {
  hls <- as(colorspace::hex2RGB(hex), "HLS")
  cc <- colorspace::coords(hls)
  return(
    setNames(
      c(
        unname(cc[, "H"]),
        unname(cc[, "S"]) * 100,
        unname(cc[, "L"]) * 100
      ),
      c("h", "s", "l")
    )
  )
}

hue <- function(hex) unname(hexToHsl(hex)["h"])
saturation <- function(hex) unname(hexToHsl(hex)["s"])

lighten <- function(col, p, method = "absolute") {
  return(colorspace::lighten(col, amount = p / 100, method, space = "HCL"))
}
darken <- function(col, p, method = "absolute") {
  return(colorspace::darken(col, amount = p / 100, method, space = "HCL"))
}

fade <- function(col, p) scales::alpha(col, p / 100)

contrast <- function(bg, dark = "#000000", light = "#ffffff") {
  return(unname(ifelse(luma(bg) > 50, dark, light)))
}

luma <- function(hex) {
  rgb <- farver::decode_colour(hex, to = "rgb") / 255
  c <- ifelse(rgb <= .03928, rgb / 12.92,
    ((rgb + .055) / 1.055)^2.4
  )
  return((0.2126 * c[, 1] + 0.7152 * c[, 2] + 0.0722 * c[, 3]) * 100)
}

contrastRatio <- function(col1, col2) {
  L1 <- luma(col1) / 100
  L2 <- luma(col2) / 100
  if (L1 < L2) {
    tmp <- L1
    L1 <- L2
    L2 <- tmp
  }
  return((L1 + 0.05) / (L2 + 0.05))
}

goodContrast <- function(fg, bg, threshold = 3.5) {
  return(contrastRatio(fg, bg) >= threshold)
}

boolean <- function(x) {
  val <- as.logical(x)[1]
  if (is.na(val) || length(val) == 0) {
    return(FALSE)
  } else {
    return(val)
  }
}

makeHsl <- function(color, l, s) hslHex(hue(color), s, l)

resolveColor <- function(val, default) {
  if (length(val) && nzchar(val)) {
    return(val)
  } else {
    return(default)
  }
}
