observeEvent(input$outputTableView, {
  # switch between graphical and tabular view in output sheets
  if(!length(modelOut)){
    # no output sheets available
    return(NULL)
  }
  i <- as.integer(strsplit(isolate(input[["contentCurrent"]]), "_")[[1]][2])
  flog.debug("%s: Table view for model output in sheet: %d activated.", uid, i)
  shinyjs::toggle(paste0("graph-out_", i))
  shinyjs::toggle(paste0("data-out_", i))
})