# save scenario data in memory

# scenario loaded into webUI always has internal ID = 1
scen.str <<- "scen_1_"
# clear output data in case it should not be saved
if(!save.output){
  lapply(seq_along(modelOut), function(i){
    scenData[[scen.str]][[i]] <<- scenDataTemplate[[i]]
  })
  scalarData[[scen.str]] <<- data.frame()
}
if(is.null(scalarData[[scen.str]]) || !nrow(scalarData[[scen.str]])){
  scalarData[[scen.str]] <<- data.frame()
}else{
  idx.scalarOut <- match(tolower(scalars.out.name), names(modelOut))
  if(!is.na(idx.scalarOut)){
    # bind hidden and non hidden scalar data
    if(nrow(scenData[[scen.str]][[idx.scalarOut]])){
      if(nrow(scalarData[[scen.str]])){
        scenData[[scen.str]][[idx.scalarOut]] <<- rbind(scenData[[scen.str]][[idx.scalarOut]], scalarData[[scen.str]])
      }else{
        scenData[[scen.str]][[idx.scalarOut]] <<- scenData[[scen.str]][[idx.scalarOut]]
      }
    }else if(nrow(scalarData[[scen.str]])){
      scenData[[scen.str]][[idx.scalarOut]] <<- scalarData[[scen.str]]
    }else{
      scenData[[scen.str]][[idx.scalarOut]] <<- scenDataTemplate[[idx.scalarOut]]
    }
    scalarData[[scen.str]] <<- data.frame()
  }
}

# save input data 
source("./modules/input_save.R", local = TRUE)
lapply(seq_along(data.tmp), function(i){
  scenData[[scen.str]][[i + length(modelOut)]] <<- data.tmp[[i]]
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
  if(!is.null(isolate(rv$active.sname))){
    scen.name <- isolate(rv$active.sname)
  }else if (!is.null(active.sname.tmp)){
    scen.name <- active.sname.tmp
  }else{
    flog.warn("Attempt to save new scenario failed due to no valid name being assigned (Save button was clicked).")
    return(NULL)
  }
}