mirowidget_initial_stateOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  if (isTRUE(options$isInput)) {
    return(tagList(
      span(textOutput(ns("uniqueSolWarning")),
        textOutput(ns("testOutput")),
        style = "color:red"
      ), checkboxInput(ns("force_unique_sol"), "Force unique solution?"),
      rHandsontableOutput(ns("sudoku"))
    ))
  }
  return(rHandsontableOutput(ns("sudoku")))
}

renderMirowidget_initial_state <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, ...) {
  if (isTRUE(options$isInput)) {
    output$uniqueSolWarning <- renderText({
      if (isTRUE(input$force_unique_sol)) {
        "Model will abort if more than one solution exists."
      }
    })
    observe({
      if (length(data[["_scalars"]]()) && length(data[["_scalars"]]()[[3]])) {
        updateCheckboxInput(session, "force_unique_sol", value = identical(data[["_scalars"]]()[[3]][[1]], "1"))
      }
    })
    output$testOutput <- renderText({
      data$test123()
    })
  }
  initialData <- NULL
  dataToRender <- reactive({
    dataTmp <- if (isTRUE(options$isInput)) {
      data[["initial_state"]]()
    } else {
      data
    }
    if (!length(dataTmp) || !nrow(dataTmp)) {
      return(as_tibble(vapply(paste0("col", 1:9), function(el) {
        character(9L)
      }, character(9L))))
    }
    dataTmp <- dataTmp[-1L]
    if (isTRUE(options$isInput)) {
      dataTmp <- mutate(dataTmp, across(where(is.numeric), as.character))
      dataTmp[is.na(dataTmp)] <- ""
    } else {
      initialData <<- which(dataTmp < 0)
      dataTmp <- abs(dataTmp)
      dataTmp <- mutate(dataTmp, across(where(is.numeric), as.character))
      dataTmp[is.na(dataTmp)] <- ""
    }
    return(dataTmp)
  })
  output$sudoku <- renderRHandsontable(rhandsontable(dataToRender(),
    readOnly = !isTRUE(options$isInput), rowHeaders = FALSE,
    colHeaders = FALSE
  ) %>% hot_table(
    contextMenu = FALSE,
    customBorders = lapply(0:8, function(i) {
      list(
        range = list(from = list(row = i %% 3L * 3L, col = i %/% 3L *
          3L), to = list(row = i %% 3L * 3L + 2L, col = i %/% 3L *
          3L + 2L)), top = list(width = 4, color = "black"),
        left = list(width = 4, color = "black"), bottom = list(
          width = 4,
          color = "black"
        ), right = list(width = 4, color = "black")
      )
    })
  ) %>% hot_validate_numeric(
    cols = 1:9, min = 1, max = 9,
    allowInvalid = TRUE
  ) %>% hot_cols(colWidths = 50, renderer = if (isTRUE(options$isInput)) {
    "function (instance, td, row, col, prop, value, cellProperties) {\n
            Handsontable.renderers.NumericRenderer.apply(this, arguments);\n
            if (value > 0.5) {\n
                td.style.fontWeight = 900;\n
                td.style.fontFamily = 'Verdana';\n
            }\n
        }"
  } else {
    paste0(
      "function (instance, td, row, col, prop, value, cellProperties) {\n
            Handsontable.renderers.NumericRenderer.apply(this, arguments);\n
            if ([",
      paste0(initialData, collapse = ","), "].includes(col*9+row+1)){\n
                td.style.fontWeight = 900;\n
                td.style.fontFamily = 'Verdana';\n
                }\n
            }"
    )
  }) %>% hot_rows(rowHeights = 50) %>% hot_col(1:9, valign = "htMiddle htCenter"))
  if (isTRUE(options$isInput)) {
    return(list(initial_state = reactive({
      if (is.null(input$sudoku)) {
        return(NULL)
      }
      dataTmp <- hot_to_r(input$sudoku) %>% mutate(across(everything(), as.integer))
      if (all(is.na(dataTmp))) {
        return(isolate(data[["initial_state"]]()))
      }
      if (length(dataTmp) && nrow(dataTmp) == 9L) {
        return(bind_cols(row = paste0("row", 1:9), dataTmp))
      } else {
        return(dataTmp)
      }
    }), `_scalars` = reactive({
      return(data[["_scalars"]]())
    })))
  }
}
