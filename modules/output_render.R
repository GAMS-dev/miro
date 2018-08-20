# rendering output tables and graphs
renderOutputData <- function(){
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = lang$progressBar$renderOutput$title, value = 0)
  errMsg <- NULL
  lapply(modelOut.to.display, function(sheet.name){
    i <- match(sheet.name, tolower(names(modelOut)))[1]
    tryCatch({
      callModule(renderData, "tab_" %+% i, type = config.graphs.out[[i]]$outType, data = scenData[["scen_1_"]][[i]],
                 config.data = scalarData[["scen_1_"]], dt.options = config$datatable, graph.options = config.graphs.out[[i]]$graph, 
                 pivot.options = config.graphs.out[[i]]$pivottable, custom.options = config.graphs.out[[i]]$options,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      callModule(renderData, "table-out_" %+% i, type = "datatable", data = scenData[["scen_1_"]][[i]],
                 dt.options = config$datatable, roundPrecision = roundPrecision)
    }, error = function(e) {
      flog.error("Problems rendering output charts/tables of dataset: '%s'. Error message: %s.", sheet.name, e)
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, modelOut.alias[i]), sep = "\n")
    })
    progress$inc(1/length(modelOut), detail = paste0(lang$progressBar$renderOutput$progress, i))
  })
  showErrorMsg(lang$errMsg$renderTable$title, errMsg)
}