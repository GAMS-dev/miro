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

lapply(seq_along(modelIn), function(i){
  noErr <- TRUE
  switch(modelIn[[i]]$type,
         dt = ,
         hot = {
           if(names(modelIn)[[i]] != scalarsFileName){
             tryCatch({
               dataTmp[[j]] <<- getInputDataset(i)
             }, error = function(e){
               flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
               errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
               noErr <<- FALSE
             })
             if(!noErr){
               return()
             }
             j <<- j + 1
           }
         },
         slider = {
           if(!is.null(isolate(input[[paste0("slider_", i)]]))){
             value <- isolate(input[[paste0("slider_", i)]])
           }else if(is.numeric(sliderValues[[tolower(names(modelIn)[[i]])]]$def)){
             value <- sliderValues[[tolower(names(modelIn)[[i]])]]$def
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           if(length(value) > 1){
             # double slider (two values)
             scalar      <- paste0(tolower(names(modelIn))[[i]], c("_min", "_max"))
             description <- paste0(modelInAlias[i], c(" (min)", " (max)"))
           }else{
             # standard slider (one value)
             scalar      <- tolower(names(modelIn))[[i]]
             description <- modelInAlias[i]
           }
           # generate data frame
           if(is.null(dataTmp[[length(modelInFileNames)]])){
             # no scalar data was written yet, so add headers
             dataTmp[[length(modelInFileNames)]]        <<- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(dataTmp[[length(modelInFileNames)]]) <<- scalarsFileHeaders
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalarsFileHeaders
             dataTmp[[length(modelInFileNames)]] <<- rbind(dataTmp[[length(modelInFileNames)]], new.data) 
           }
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
           
           # generate data frame
           if(is.null(dataTmp[[length(modelInFileNames)]])){
             # no scalar data was written yet, so add headers
             dataTmp[[length(modelInFileNames)]]        <<- data.frame(scalar, description, value, stringsAsFactors = F)
             names(dataTmp[[length(modelInFileNames)]]) <<- scalarsFileHeaders
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalarsFileHeaders
             dataTmp[[length(modelInFileNames)]] <<- rbind(dataTmp[[length(modelInFileNames)]], new.data) 
           }
         },
         daterange = {
           if(!is.null(isolate(input[[paste0("daterange_", i)]]))){
             value <- as.character(isolate(input[[paste0("daterange_", i)]]))
           }else if(!is.null(modelIn[[i]]$daterange$start) && !is.null(modelIn[[i]]$daterange$end)){
             value <- c(as.character(modelIn[[i]]$daterange$start), as.character(modelIn[[i]]$daterange$end))
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- paste0(names(modelIn)[[i]], c("_min", "_max"))
           description <- paste0(modelInAlias[i], c(" (min)", " (max)"))
        
           # generate data frame
           if(is.null(dataTmp[[length(modelInFileNames)]])){
             # no scalar data was written yet, so add headers
             dataTmp[[length(modelInFileNames)]]        <<- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(dataTmp[[length(modelInFileNames)]]) <<- scalarsFileHeaders
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalarsFileHeaders
             dataTmp[[length(modelInFileNames)]] <<- rbind(dataTmp[[length(modelInFileNames)]], new.data) 
           }
         },
         dropdown = {
           if(!is.null(isolate(input[[paste0("dropdown_", i)]]))){
             value <- isolate(input[[paste0("dropdown_", i)]])
           }else if(!is.null(modelIn[[i]]$dropdown$selected)){
             value <- modelIn[[i]]$dropdown$selected
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelInAlias[i]), sep = "\n")
             return(NULL)
           }
           
           if(identical(modelIn[[i]]$dropdown$multiple, TRUE)){
             # generate data frame (multi dropdown menu)
             dataTmp[[j]]        <<- data.frame(value, stringsAsFactors = FALSE, check.names = FALSE)
             names(dataTmp[[j]]) <<- tolower(names(modelIn))[[i]]
             j <<- j + 1L
           }else{
             # standard dropdown menu (one value)
             scalar      <- names(modelIn)[[i]]
             description <- modelInAlias[i]
             if(is.null(dataTmp[[length(modelInFileNames)]])){
               # no scalar data was written yet, so add headers
               dataTmp[[length(modelInFileNames)]]        <<- data.frame(scalar, description, value, 
                                                                          stringsAsFactors = FALSE, check.names = FALSE)
               names(dataTmp[[length(modelInFileNames)]]) <<- scalarsFileHeaders
             }else{
               # no headers, just data
               new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
               names(new.data) <- scalarsFileHeaders
               dataTmp[[length(modelInFileNames)]] <<- rbind(dataTmp[[length(modelInFileNames)]], new.data)
             }
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
           
           # generate data frame
           if(is.null(dataTmp[[length(modelInFileNames)]])){
             # no scalar data was written yet, so add headers
             dataTmp[[length(modelInFileNames)]]        <<- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(dataTmp[[length(modelInFileNames)]]) <<- scalarsFileHeaders
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalarsFileHeaders
             dataTmp[[length(modelInFileNames)]] <<- rbind(dataTmp[[length(modelInFileNames)]], new.data) 
           }
         }
  )
  flog.trace("Dataset: %s saved in dataTmp.", modelIn[[i]])
})
showErrorMsg(lang$errMsg$GAMSInput$title, errMsg)