# export scenario data to excel spreadsheet
output[["export_" %+% i]] <- downloadHandler(
  filename = function() {
    if(i == 1){
      # active scenario (editable)
      if(is.null(isolate(rv$activeSname))){
        if(is.null(activeSnameTmp)){
          # as no scenario name could be found set, scenario name to model name
          activeSnameTmp <<- modelName
          return(paste0(activeSnameTmp, ".", isolate(input$exportFileType)))
        }else{
          return(paste0(modelName, "_", activeSnameTmp, ".", exportFileType))
        }
      }else{
        return(paste0(modelName, "_", isolate(rv$activeSname), ".", exportFileType))
      }
    }
    fileName <- paste0(modelName, "_", scenMetaData[["scen_" %+% i %+% "_"]][[3]][1], ".", exportFileType)
    flog.debug("File: '%s' was downloaded.", fileName)
    return(fileName)
  },
  content = function(file) {
    if(i == 1){
      # active scenario (editable)
      saveAsFlag <<- FALSE
      source("./modules/scen_save.R", local = TRUE)
      data <- scenData[[scenIdLong]]
    }else{
      data <- scenData[[scenIdLong]]
      # combine hidden and non hidden scalar data
      scalarOutIdx <- match(tolower(scalarsOutName), names(modelOut))[1]
      if(!is.na(scalarOutIdx) && !is.null(data[[scalarOutIdx]])){
        data[[scalarOutIdx]] <- filterScalars(scalarData[[scenIdLong]], modelOut[[scalarsOutName]], "output")
      }
    }
    removeModal()
    if(identical(exportFileType, "gdx")){
      names(data) <- c(names(modelOut), inputDsNames)
      return(gdxio$wgdx(file, data, squeezeZeros = 'n'))
    }
    wsNamesTmp                 <- c(if(length(modelOut))paste0(lang$nav$excelExport$outputPrefix, 
                                                                names(modelOut), 
                                                                lang$nav$excelExport$outputSuffix), 
                                     if(length(inputDsNames))paste0(lang$nav$excelExport$inputPrefix, 
                                                                    inputDsNames, 
                                                                    lang$nav$excelExport$inputSuffix))
    if(any(nchar(wsNamesTmp) > 31)){
      wsNameExceedsLength <- nchar(wsNamesTmp) > 31
      wsNamesTmp[wsNameExceedsLength] <- paste0(substr(wsNamesTmp[wsNameExceedsLength], 1, 29), "..")
      if(any(duplicated(wsNamesTmp))){
        wsNameDuplicated <- duplicated(wsNamesTmp)
        wsNamesTmp <- lapply(seq_along(wsNamesTmp), function(wsID){
          if(wsNameDuplicated[wsID]){
            return(paste0(substr(wsNamesTmp[wsID], 1, 29), wsID))
          }
          return(wsNamesTmp[wsID])
        })
      }
    }
    names(data) <- wsNamesTmp
    
    # remove empty datasets
    if(!config$excelIncludeEmptySheets)
      data[vapply(data, function(sheet) identical(nrow(sheet), 0L), logical(1L))] <- NULL
    
    # include metadata sheet in Excel file
    if(config$activateModules$scenario && config$excelIncludeMeta && 
       !is.null(scenMetaData[[scenIdLong]])){
      metadata <- list(scenMetaData[[scenIdLong]][, -1, drop = FALSE])
      names(metadata) <- lang$nav$excelExport$metadataSheet$title
      data <- c(metadata, data)
    }
    return(writexl::write_xlsx(data, file))
  },
  contentType = if(identical(exportFileType, "gdx")) "application/octet-stream" else
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)