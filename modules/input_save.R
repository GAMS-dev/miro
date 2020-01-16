# save input data to dataTmp list

# define temporary list to save input data to
dataTmp <- vector(mode = "list", length = length(modelInFileNames))
names(dataTmp) <- modelInFileNames
errMsg <- NULL
j <- 1L
# first add scalar data which is in a table
scalarId <- match(scalarsFileName, modelInTabularData)[[1]]

if(!is.na(scalarId)){
  i <- match(tolower(modelInTabularData[scalarId]), names(modelIn))[[1]]
  tryCatch({
    dataTmp[[length(modelInFileNames)]] <- getInputDataset(i)
  }, error = function(e){
    flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
    errMsg <<- sprintf(lang$errMsg$GAMSInput$noData, names(modelIn)[[i]])
  })
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return()
  }
}
addScalarVal <- function(scalar, description, value){
  value <- as.character(value)
  if(!saveInputDb){
    if(!length(value) || !nchar(value))
      value <- "system.empty"
  }
  
  # generate data frame
  if(is.null(dataTmp[[length(modelInFileNames)]])){
    # no scalar data was written yet, so add headers
    dataTmp[[length(modelInFileNames)]]        <<- tibble(scalar, description, value)
    names(dataTmp[[length(modelInFileNames)]]) <<- scalarsFileHeaders
  }else{
    # no headers, just data
    newData        <- tibble(scalar, description, value)
    names(newData) <- scalarsFileHeaders
    dataTmp[[length(modelInFileNames)]] <<- rbind(dataTmp[[length(modelInFileNames)]], newData) 
  }
}
lapply(seq_along(modelIn), function(i){
  noErr <- TRUE
  switch(modelIn[[i]]$type,
         slider = {
           if(!is.null(isolate(input[[paste0("slider_", i)]]))){
             value <- isolate(input[[paste0("slider_", i)]])
           }else if(is.numeric(sliderValues[[tolower(names(modelIn)[[i]])]]$def)){
             value <- sliderValues[[tolower(names(modelIn)[[i]])]]$def
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, 
                                              modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           if(length(value) > 1){
             # double slider (two values)
             scalar      <- paste0(tolower(names(modelIn))[[i]], c("$lo", "$up"))
             description <- paste0(modelInAlias[i], c(" (min)", " (max)"))
             if(identical(modelIn[[i]]$slider$double, TRUE)){
               scalar      <- c(scalar, paste0(tolower(names(modelIn))[[i]], 
                                                c("$step", "$mode")))
               description <- c(description, paste0(modelInAlias[i], 
                                                    c(" (step size)", " (mode)")))
               value       <- c(value, isolate(input[["hcubeStep_" %+% i]]),
                                isolate(input[["hcubeMode_" %+% i]]))
             }
             if(identical(modelIn[[i]]$slider$single, TRUE)){
               scalar      <- c(scalar, paste0(tolower(names(modelIn))[[i]], 
                                                "$step"))
               description <- c(description, paste0(modelInAlias[i], 
                                                    " (step size)"))
               value       <- c(value, isolate(input[["hcubeStep_" %+% i]]))
             }
           }else{
             # standard slider (one value)
             scalar      <- names(modelIn)[[i]]
             description <- modelInAlias[i]
           }
           addScalarVal(scalar, description, value)
         },
         date = {
           if(!is.null(isolate(input[[paste0("date_", i)]]))){
             value <- as.character(isolate(input[[paste0("date_", i)]]))
           }else if(!is.null(modelIn[[i]]$date$value)){
             value <- as.character(modelIn[[i]]$date$value)
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- names(modelIn)[[i]]
           description <- modelInAlias[i]
           addScalarVal(scalar, description, value)
         },
         daterange = {
           if(!is.null(isolate(input[[paste0("daterange_", i)]]))){
             value <- as.character(isolate(input[[paste0("daterange_", i)]]))
           }else if(!is.null(modelIn[[i]]$daterange$start) && !is.null(modelIn[[i]]$daterange$end)){
             value <- c(as.character(modelIn[[i]]$daterange$start), 
                        as.character(modelIn[[i]]$daterange$end))
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- paste0(names(modelIn)[[i]], c("$lo", "$up"))
           description <- paste0(modelInAlias[i], c(" (lower)", " (upper)"))
           addScalarVal(scalar, description, value)
         },
         textinput = {
           if(!is.null(isolate(input[[paste0("text_", i)]]))){
             value <- as.character(isolate(input[[paste0("text_", i)]]))
           }else if(!is.null(modelIn[[i]]$textinput$value)){
             value <- as.character(modelIn[[i]]$textinput$value)
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- names(modelIn)[[i]]
           description <- modelInAlias[i]
           addScalarVal(scalar, description, value)
         },
         numericinput = {
           if(!is.null(isolate(input[[paste0("numeric_", i)]]))){
             value <- isolate(input[[paste0("numeric_", i)]])
           }else if(!is.null(modelIn[[i]]$numericinput$value)){
             value <- modelIn[[i]]$numericinput$value
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- names(modelIn)[[i]]
           description <- modelInAlias[i]
           addScalarVal(scalar, description, value)
         },
         dropdown = {
           if(!is.null(isolate(input[[paste0("dropdown_", i)]]))){
             value <- isolate(input[[paste0("dropdown_", i)]])
           }else if(!is.null(modelIn[[i]]$dropdown$selected)){
             value <- modelIn[[i]]$dropdown$selected
           }else if(names(modelIn)[[i]] %in% modelInTabularDataBase){
             value <- character(0L)
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, 
                                              modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           value <- value[value != "_"]
           if(names(modelIn)[[i]] %in% modelInTabularDataBase){
             # generate data frame (multi dropdown menu)
             dataTmp[[j]] <<- ddToTibble(value, modelIn[[i]])
             j <<- j + 1L
           }else{
             # standard dropdown menu (one value)
             scalar      <- names(modelIn)[[i]]
             description <- modelInAlias[i]
             addScalarVal(scalar, description, value)
           }
         },
         checkbox = {
           if(!is.null(isolate(input[[paste0("cb_", i)]]))){
             value <- if(identical(isolate(input[[paste0("cb_", i)]]), TRUE)) 1L else 0L
           }else if(!is.null(modelIn[[i]]$checkbox$value)){
             value <- if(identical(modelIn[[i]]$checkbox$value, TRUE)) 1L else 0L
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- names(modelIn)[[i]]
           description <- modelInAlias[i]
           addScalarVal(scalar, description, value)
         },
         {
           if(names(modelIn)[[i]] != scalarsFileName){
             tryCatch({
               dataTmp[[j]] <<- fixColTypes(getInputDataset(i), modelIn[[i]]$colTypes)
             }, error = function(e){
               flog.error("Dataset: '%s' could not be loaded. Error message: '%s'.", modelInAlias[i], e)
               errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, 
                                                modelInAlias[i]), sep = "\n")
               noErr <<- FALSE
             })
             if(!noErr){
               return()
             }
             j <<- j + 1
           }
         }
  )
  flog.trace("Dataset: %s saved in dataTmp.", modelIn[[i]])
})
