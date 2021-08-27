observeEvent(input$btDownloadTmpFiles, {
  flog.debug("Button to download files from temp folder clicked.")

  fileNames <- list.files(workDir, pattern = ".+\\..+$")
  showModal(modalDialog(
    title = lang$nav$dialogDownloadTmp$title,
    if (length(fileNames)) {
      serverSelectInput(session, "selectDownloadTmp",
        label = lang$nav$dialogDownloadTmp$desc,
        choices = fileNames, multiple = TRUE
      )
    } else {
      lang$nav$dialogDownloadTmp$noFiles
    },
    footer = tagList(
      modalButton(lang$nav$dialogDownloadTmp$cancelButton),
      if (length(fileNames)) {
        tagList(
          downloadButton("btDownloadTmpZip",
            label = lang$nav$dialogDownloadTmp$downloadAllButton
          ),
          tags$span(
            `data-display-if` = "input.selectDownloadTmp.length>0",
            downloadButton("btDownloadTmpConfirm",
              label = lang$nav$dialogDownloadTmp$downloadButton,
              class = "bt-highlight-1 bt-gms-confirm"
            )
          )
        )
      }
    )
  ))
})
output$btDownloadTmpConfirm <- downloadHandler(
  filename = function() {
    if (identical(length(isolate(input$selectDownloadTmp)), 1L)) {
      return(isolate(input$selectDownloadTmp))
    }
    if (is.null(isolate(rv$activeSname))) {
      return(paste0(modelName, ".zip"))
    }
    return(paste0(modelName, "_", isolate(rv$activeSname), ".zip"))
  }, content = function(file) {
    fileNamesToDownload <- isolate(input$selectDownloadTmp)
    validFileNames <- list.files(workDir, pattern = ".+\\..+$", full.names = FALSE)
    if (identical(length(fileNamesToDownload), 0L)) {
      flog.error("Confirm download of temporary files button clicked without any files specified. This should not happen and is likely an attempt to tamper with the app!")
      return(downloadHandlerError(file, "Invalid  filename"))
    }
    invalidFileNames <- !fileNamesToDownload %in% validFileNames
    if (any(invalidFileNames)) {
      flog.error("File name(s) supplied (%s) that are not in current directory. This should never happen and is likely an attempt to tamper with the app!", paste(fileNamesToDownload[invalidFileNames], collapse = ", "))
      return(downloadHandlerError(file, "Invalid  filename"))
    }
    flog.debug(
      "Download of file(s): '%s' confirmed.",
      paste(fileNamesToDownload, collapse = "', '")
    )
    if (length(fileNamesToDownload) > 1L) {
      return(zipr(file, file.path(workDir, fileNamesToDownload),
        compression_level = 6
      ))
    }
    return(file.copy(file.path(workDir, fileNamesToDownload), file))
  }
)
output$btDownloadTmpZip <- downloadHandler(
  filename = function() {
    if (is.null(isolate(rv$activeSname))) {
      return(paste0(modelName, ".zip"))
    } else {
      return(paste0(modelName, "_", isolate(rv$activeSname), ".zip"))
    }
  },
  content = function(file) {
    zipr(file, list.files(
      path = workDir, pattern = ".+\\..+$",
      full.names = TRUE
    ), compression_level = 6)
  },
  contentType = "application/zip"
)
