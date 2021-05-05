mirorenderer_initial_stateOutput <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
    rHandsontableOutput(ns("sudoku"))
}
 
 renderMirorenderer_initial_state <- function(input, output, session, data, options = NULL, path = NULL, views = NULL, ...){
    force(data)
    dataTmp <- data
    if (length(data) && nrow(data)) {
        dataTmp <- dataTmp[-1L]
        if (isTRUE(options$isInput)) {
            dataTmp <- dataTmp %>% mutate_if(is.numeric, as.character)
            dataTmp[is.na(dataTmp)] <- ""
        }
        else {
            initialData <- which(dataTmp < 0)
            dataTmp <- abs(dataTmp)
            dataTmp <- dataTmp %>% mutate_if(is.numeric, as.character)
            dataTmp[is.na(dataTmp)] <- ""
        }
    }
    else {
        dataTmp <- as_tibble(vapply(paste0("col", 1:9), function(el) {
            character(9L)
        }, character(9L)))
    }
    output$sudoku <- renderRHandsontable(rhandsontable(dataTmp, 
        readOnly = !isTRUE(options$isInput), rowHeaders = FALSE, 
        colHeaders = FALSE) %>% hot_table(contextMenu = FALSE, 
        customBorders = lapply(0:8, function(i) {
            list(range = list(from = list(row = i%%3L * 3L, col = i%/%3L * 
                3L), to = list(row = i%%3L * 3L + 2L, col = i%/%3L * 
                3L + 2L)), top = list(width = 4, color = "black"), 
                left = list(width = 4, color = "black"), bottom = list(width = 4, 
                  color = "black"), right = list(width = 4, color = "black"))
        })) %>% hot_validate_numeric(cols = 1:9, min = 1, max = 9, 
        allowInvalid = TRUE) %>% hot_cols(colWidths = 50, renderer = if (isTRUE(options$isInput)) {
        paste0("function (instance, td, row, col, prop, value, cellProperties) {", 
            "Handsontable.renderers.NumericRenderer.apply(this, arguments);", 
            "if (value > 0.5) {", "td.style.fontWeight = 900;", 
            "td.style.fontFamily = 'Verdana';", "}", "}")
    }
    else {
        paste0("function (instance, td, row, col, prop, value, cellProperties) {", 
            "Handsontable.renderers.NumericRenderer.apply(this, arguments);", 
            "if ([", paste0(initialData, collapse = ","), "].includes(col*9+row+1)){", 
            "td.style.fontWeight = 900;", "td.style.fontFamily = 'Verdana';", 
            "}", "}")
    }) %>% hot_rows(rowHeights = 50) %>% hot_col(1:9, valign = "htMiddle htCenter"))
    if (isTRUE(options$isInput)) {
        return(reactive({
            if (is.null(input$sudoku)) {
                return(NULL)
            }
            dataTmp <- hot_to_r(input$sudoku) %>% mutate_all(as.integer)
            if (length(dataTmp) && nrow(dataTmp) == 9L) {
                return(bind_cols(row = paste0("row", 1:9), dataTmp))
            } else {
                return(dataTmp)
            }
        }))
    }
}
