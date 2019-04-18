# export scenario data to excel spreadsheet
output[["export_" %+% i]] <- downloadHandler(
  filename = function() {
    if(i == 1){
      # active scenario (editable)
      if(is.null(isolate(rv$activeSname))){
        if(is.null(activeSnameTmp)){
          # as no scenario name could be found set, scenario name to model name
          activeSnameTmp <<- modelName
          return(activeSnameTmp %+% ".xlsx")
        }else{
          return(modelName %+% "_" %+% activeSnameTmp %+% ".xlsx")
        }
      }else{
        return(modelName %+% "_" %+% isolate(rv$activeSname) %+% ".xlsx")
      }
    }
    fileName <- paste0(modelName, "_", scenMetaData[["scen_" %+% i %+% "_"]][[3]][1], ".xlsx")
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
        # bind hidden and non hidden scalar data
        data[[scalarOutIdx]] <- scalarData[[scenIdLong]]
      }
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
    writexl::write_xlsx(data, file)
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)