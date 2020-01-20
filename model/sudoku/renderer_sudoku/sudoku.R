sudokuOutput <- function(id, height, options, path){
   ns <- NS(id)
   rHandsontableOutput(ns('sudoku'))
}

renderSudoku <- function(input, output, session, data, options = NULL, path = NULL){
  force(data)
  dataTmp <- data
  if(length(data) && nrow(data)){
    dataTmp <- dataTmp[-1L] %>%
      mutate_if(is.double, as.integer)
  }else{
    dataTmp <- as_tibble(vapply(paste0("col", 1:9), function(el){integer(9L)}, integer(9L)))
  }
  output$sudoku <- renderRHandsontable(rhandsontable(dataTmp, readOnly = !isTRUE(options$isInput), rowHeaders = FALSE) %>%
                                         hot_table(customBorders = lapply(0:8, function(i){
                                           list(
                                             range = list(from = list(row = i%%3L*3L, col = i%/%3L*3L),
                                                          to = list(row = i%%3L*3L + 2L, col = i%/%3L*3L + 2L)),
                                             top = list(width = 4, color = "black"),
                                             left = list(width = 4, color = "black"),
                                             bottom = list(width = 4, color = "black"),
                                             right = list(width = 4, color = "black"))
                                         })) %>%
                                         hot_cols(colWidths = 50) %>%
                                         hot_rows(rowHeights = 50))
  if(isTRUE(options$isInput)){
    return(reactive({
        dataTmp <- hot_to_r(input$sudoku)
        if(length(dataTmp) && nrow(dataTmp) == 9L){
            return(bind_cols(row = paste0("row", 1:9), dataTmp))
        }else{
            return(dataTmp)
        }
    }))
  }
}

