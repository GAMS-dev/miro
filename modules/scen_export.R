# export scenario data to excel spreadsheet
output[["export_" %+% i]] <- downloadHandler(
  filename = function() {
    isolate({
      fileExt <- exportFileType
      if(identical(fileExt, "csv")){
        if(isTRUE(input$cbSelectManuallyExp)){
          if(length(input$selDataToExport) > 1L)
            fileExt <- "zip"
        }else if(length(modelOut) + length(inputDsNames) > 1L){
          fileExt <- "zip"
        }
      }
      if(i == 1){
        # active scenario (editable)
        if(is.null(isolate(rv$activeSname))){
          # as no scenario name could be found, set scenario name to model name
          return(paste0(modelName, ".", fileExt))
        }else{
          return(paste0(modelName, "_", isolate(rv$activeSname), ".", fileExt))
        }
      }
      fileName <- paste0(modelName, "_", scenMetaData[[scenIdLong]][[3]][1], 
                         ".", fileExt)
      flog.debug("File: '%s' was downloaded.", fileName)
    })
    return(fileName)
  },
  content = function(file) {
    prog <- Progress$new()
    on.exit(suppressWarnings(prog$close()))
    prog$set(message = lang$progressBar$exportScen$title, value = 0.1)
    
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
    
    if(length(data)){
      names(data) <- c(names(modelOut), inputDsNames)
    }
    
    if(isTRUE(input$cbSelectManuallyExp)){
      outputDataToExport <- names(modelOut)[names(modelOut) %in% input$selDataToExport]
      inputDataToExport  <- inputDsNames[inputDsNames %in% input$selDataToExport]
      
      if(!length(outputDataToExport) && !length(inputDataToExport)){
        flog.info("No datasets selected. Nothing will be exported.")
        showHideEl(session, "#exportNoDsSelected", 4000L)
        return(downloadHandlerError(file, lang$nav$dialogExportScen$noDsSelected))
      }
      data <- data[names(data) %in% c(outputDataToExport, inputDataToExport)]
    }else{
      outputDataToExport <- names(modelOut)
      inputDataToExport  <- inputDsNames
    }
    noDatasets <- length(data)
    prog$set(value = 0.2)
    
    if(identical(exportFileType, "gdx")){
      return(gdxio$wgdx(file, data, squeezeZeros = 'n'))
    }else if(identical(exportFileType, "csv")){
      if(length(data) == 0L){
        return(readr::write_csv(tibble(), file))
      }else if(length(data) == 1L){
        return(readr::write_csv(data[[1L]], file))
      }
      tmpDir <- file.path(tempdir(), paste0(uid, "_exp_tmp_dir"))
      if(file.exists(tmpDir) && !identical(unlink(tmpDir, recursive = TRUE), 0L)){
        flog.error("Could not remove temporary directory: '%s'.", tmpDir)
        return(downloadHandlerError(file, "Directory could not be removed"))
      }
      if(!dir.create(tmpDir, recursive = TRUE)){
        flog.error("Could not create temporary directory: '%s'.", tmpDir)
        return(downloadHandlerError(file, "Directory could not be created"))
      }
      on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
      for(i in seq_along(data)){
        dsName <- names(data)[i]
        prog$inc(amount = 0.8/noDatasets, detail = sprintf(lang$progressBar$exportScen$exportDs, i, noDatasets))
        readr::write_csv(data[[dsName]], file.path(tmpDir, paste0(dsName, ".csv")))
        
      }
      return(suppressWarnings(zip::zipr(file, list.files(tmpDir, full.names = TRUE), 
                                        recurse = FALSE, include_directories = FALSE)))
    }
    
    if(length(data)){
      wsNamesTmp                 <- c(if(length(outputDataToExport)) paste0(lang$nav$excelExport$outputPrefix, 
                                                                            outputDataToExport, 
                                                                            lang$nav$excelExport$outputSuffix), 
                                      if(length(inputDataToExport)) paste0(lang$nav$excelExport$inputPrefix, 
                                                                           inputDataToExport, 
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
    }
    
    # remove empty datasets
    if(!config$excelIncludeEmptySheets)
      data[vapply(data, function(sheet) identical(nrow(sheet), 0L), logical(1L))] <- NULL
    
    # include metadata sheet in Excel file
    if(config$excelIncludeMeta && 
       !is.null(scenMetaData[[scenIdLong]])){
      metadata <- list(scenMetaData[[scenIdLong]][, -1, drop = FALSE])
      names(metadata) <- lang$nav$excelExport$metadataSheet$title
      data <- c(metadata, data)
    }
    if(!length(data)){
      data <- tibble()
    }
    return(writexl::write_xlsx(data, file))
  },
  contentType = if(identical(exportFileType, "gdx")) "application/octet-stream" else
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)
observeEvent(input[[paste0("remote_export_", i)]], {
  removeModal()
  if(!length(datasetsRemoteExport) || !length(input$exportFileType)){
    flog.error("Remote export button clicked but export file type: '%s' does not exist.", 
               input$exportFileType)
    return()
  }
  exportId <- match(input$exportFileType, names(datasetsRemoteExport))[[1L]]
  if(!is.na(exportId)){
    expConfig  <- datasetsRemoteExport[[input$exportFileType]]
    dsToExport <- names(expConfig)
    
    if(isTRUE(input$cbSelectManuallyExp)){
      dsToExport <- dsToExport[dsToExport %in% input$selDataToExport]
      if(!length(dsToExport)){
        flog.info("No datasets selected. Nothing will be exported.")
        showHideEl(session, "#exportNoDsSelected", 4000L)
        return()
      }
    }
    prog <- Progress$new()
    on.exit(suppressWarnings(prog$close()))
    prog$set(message = lang$progressBar$exportScen$title, value = 0.2)
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
    
    names(data) <- c(names(modelOut), inputDsNames)
    noDatasets <- length(dsToExport)
    errMsg <- NULL
    for(expId in seq_along(dsToExport)){
      dataId <- match(dsToExport[expId], names(data))
      if(is.na(dataId)){
        next
      }
      dsName <- names(data)[dataId]
      prog$inc(amount = 0.8/noDatasets, 
               detail = sprintf(lang$progressBar$exportScen$exportDs, 
                                expId, noDatasets))
      tryCatch(dataio$export(data[[dsName]], expConfig[[expId]], dsName), 
               error = function(e){
                 flog.warn("Problems exporting data (export name: '%s', dataset: '%s'). Error message: '%s'.",
                           input$exportFileType, dsName, e)
                 errMsg <<- lang$errMsg$saveScen$desc
               })
      if(is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))){
        break
      }
    }
    flog.debug("Data exported successfully.")
    return()
  }
})
