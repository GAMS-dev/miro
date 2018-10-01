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
    fileName <- modelName %+% "_" %+% scenMetaData[["scen_" %+% i %+% "_"]][[2]][1] %+% ".xlsx"
    flog.debug("File: '%s' was downloaded.", fileName)
    return(fileName)
  },
  content = function(file) {
    if(i == 1){
      # active scenario (editable)
      saveAsFlag <<- F
      source("./modules/scen_save.R", local = TRUE)
    }else{
      # combine hidden and non hidden scalar data
      scalarOutIdx <- match(tolower(scalarsOutName), names(modelOut))[1]
      if(!is.na(scalarOutIdx) && !is.null(scenData[[scenIdLong]][[scalarOutIdx]])){
        # bind hidden and non hidden scalar data
        scenData[[scenIdLong]][[scalarOutIdx]] <<- rbind(scenData[[scenIdLong]][[scalarOutIdx]], scalarData[[scenIdLong]])
        scalarData[[scenIdLong]]                <<- list(NULL)
      }
    }
    data                        <- scenData[[scenIdLong]]
    names(data)                 <- c(if(length(modelOut))paste0(lang$nav$excelExport$outputPrefix, names(modelOut), lang$nav$excelExport$outputSuffix), 
                                     if(length(inputDsNames))paste0(lang$nav$excelExport$inputPrefix, inputDsNames, lang$nav$excelExport$inputSuffix))
    # remove empty datasets
    if(!config$excelIncludeEmptySheets)
      data[vapply(data, function(sheet) identical(nrow(sheet), 0L), logical(1L))] <- NULL
    
    # include metadata sheet in Excel file
    if(config$activateModules$scenario && config$excelIncludeMeta && !is.null(scenMetaData[[scenIdLong]])){
      metadata <- list(scenMetaData[[scenIdLong]])
      names(metadata) <- lang$nav$excelExport$metadataSheet$title
      data <- c(metadata, data)
    }
    writexl::write_xlsx(data, file)
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)