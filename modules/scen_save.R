# save scenario data in memory

# scenario loaded into webUI always has internal ID = 1
scenIdLong <<- "scen_1_"
# clear output data in case it should not be saved
if(!saveOutput){
  lapply(seq_along(modelOut), function(i){
    scenData[[scenIdLong]][[i]] <<- scenDataTemplate[[i]]
  })
  scalarData[[scenIdLong]] <<- tibble()
}
if(is.null(scalarData[[scenIdLong]]) || !nrow(scalarData[[scenIdLong]])){
  scalarData[[scenIdLong]] <<- tibble()
}else{
  idxScalarOut <- match(scalarsOutName, names(modelOut))
  if(!is.na(idxScalarOut)){
    outputScalars <- filterScalars(scalarData[[scenIdLong]], modelOut[[scalarsOutName]], "output")
    if(length(outputScalars) && nrow(outputScalars)){
      scenData[[scenIdLong]][[idxScalarOut]] <<- outputScalars
    }else{
      scenData[[scenIdLong]][[idxScalarOut]] <<- scenDataTemplate[[idxScalarOut]]
    }
    scalarData[[scenIdLong]] <<- tibble()
  }
}
if(length(modelIn)){
  # save input data 
  saveInputDb <- TRUE
  source("./modules/input_save.R", local = TRUE)
  lapply(seq_along(dataTmp), function(i){
    scenData[[scenIdLong]][[i + length(modelOut)]] <<- dataTmp[[i]]
  })
}
