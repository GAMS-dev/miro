observeEvent(input$btDownloadTmpFiles, {
  flog.debug("Button to download files from temp folder clicked.")
  
  fileNames <- list.files(workDir, pattern = ".+\\..+$")
  showModal(modalDialog(
    title = lang$nav$dialogDownloadTmp$title,
    if(length(fileNames)){
      selectInput("selectDownloadTmp", label = lang$nav$dialogDownloadTmp$desc,
                  choices = fileNames)
    }else{
      lang$nav$dialogDownloadTmp$noFiles
    },
    footer = tagList(
      modalButton(lang$nav$dialogDownloadTmp$cancelButton),
      if(length(fileNames)){
        tagList(
          downloadButton("btDownloadTmpZip", 
                         label = lang$nav$dialogDownloadTmp$downloadZipButton),
          downloadButton("btDownloadTmpConfirm", 
                         label = lang$nav$dialogDownloadTmp$downloadButton, 
                         class = "bt-highlight-1 bt-gms-confirm")
        )
      }
    )
  ))
})
output$btDownloadTmpConfirm <- downloadHandler(
  filename = function(){
    isolate(input$selectDownloadTmp)
  },content = function(file) {
    fileName <- isolate(input$selectDownloadTmp)
    flog.debug("Download of file: '%s' confirmed.", fileName)
    file.copy(file.path(workDir, fileName), file)
  }
)
output$btDownloadTmpZip <- downloadHandler(
  filename = function(){
    if(is.null(isolate(rv$activeSname))){
      return(paste0(modelName, ".zip"))
    }else{
      return(paste0(modelName, "_", isolate(rv$activeSname), ".zip"))
    }
  },
  content = function(file) {
    zipr(file, list.files(path = workDir, pattern = ".+\\..+$", 
                          full.names = TRUE), compression_level = 6)
  },
  contentType = "application/zip"
)
