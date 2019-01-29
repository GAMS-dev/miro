# save scenario data in memory

# scenario loaded into webUI always has internal ID = 1
scenIdLong <<- "scen_1_"
# clear output data in case it should not be saved
if(!saveOutput){
  lapply(seq_along(modelOut), function(i){
    scenData[[scenIdLong]][[i]] <<- scenDataTemplate[[i]]
  })
  scalarData[[scenIdLong]] <<- data.frame()
}
if(is.null(scalarData[[scenIdLong]]) || !nrow(scalarData[[scenIdLong]])){
  scalarData[[scenIdLong]] <<- data.frame()
}else{
  idxScalarOut <- match(tolower(scalarsOutName), names(modelOut))
  if(!is.na(idxScalarOut)){
    # bind hidden and non hidden scalar data
    if(nrow(scenData[[scenIdLong]][[idxScalarOut]])){
      if(nrow(scalarData[[scenIdLong]])){
        scenData[[scenIdLong]][[idxScalarOut]] <<- rbind(scenData[[scenIdLong]][[idxScalarOut]], scalarData[[scenIdLong]])
      }else{
        scenData[[scenIdLong]][[idxScalarOut]] <<- scenData[[scenIdLong]][[idxScalarOut]]
      }
    }else if(nrow(scalarData[[scenIdLong]])){
      scenData[[scenIdLong]][[idxScalarOut]] <<- scalarData[[scenIdLong]]
    }else{
      scenData[[scenIdLong]][[idxScalarOut]] <<- scenDataTemplate[[idxScalarOut]]
    }
    scalarData[[scenIdLong]] <<- data.frame()
  }
}

# save input data 
saveInputDb <- TRUE
source("./modules/input_save.R", local = TRUE)
lapply(seq_along(dataTmp), function(i){
  scenData[[scenIdLong]][[i + length(modelOut)]] <<- dataTmp[[i]]
})
scen.name <- NULL
# check whether name is valid
if(saveAsFlag){
  if(!is.null(isolate(input$scenName))){
    scen.name <- isolate(input$scenName)
  }else{
    flog.warn("Attempt to save new scenario failed due to no valid name being assigned (Save As button was clicked).")
    return(NULL)
  }
}else{
  if(!is.null(isolate(rv$activeSname))){
    scen.name <- isolate(rv$activeSname)
  }else if (!is.null(activeSnameTmp)){
    scen.name <- activeSnameTmp
  }else{
    flog.warn("Attempt to save new scenario failed due to no valid name being assigned (Save button was clicked).")
    return(NULL)
  }
}