# save input data to data.tmp list

# define temporary list to save input data to
data.tmp <- vector(mode = "list", length = length(modelIn.file.names))
names(data.tmp) <- modelIn.file.names
errMsg <- NULL
j <- 1L
# first add scalar data which is in a table
scalar.id <- match(tolower(scalars.file.name), tolower(modelIn.tabular.data))[[1]]

if(!is.na(scalar.id)){
  i <- match(tolower(modelIn.tabular.data[scalar.id]), tolower(names(modelIn)))[[1]]
  if(!is.null(isolate(input[[paste0("in_", i)]])) && hot.init[[i]]){
    if(!is.empty.input[i]){
      data.tmp[[length(modelIn.file.names)]] <- rhandsontable::hot_to_r(isolate(input[[paste0("in_",i)]])) 
    }
  }else if(!is.null(model.input.data[[i]])){
    # tab was never activated, so shiny does not update handsontable thus it is empty although data was loaded
    data.tmp[[length(modelIn.file.names)]] <- model.input.data[[i]]
  }else{
    flog.error("Dataset: '%s' could not be loaded.", modelIn.alias[i])
    errMsg <- sprintf(lang$errMsg$GAMSInput$noData, tolower(names(modelIn)[[i]]))
    showErrorMsg(lang$errMsg$GAMSInput$title, errMsg)
  }
}

lapply(seq_along(modelIn), function(i){
  switch(modelIn[[i]]$type,
         hot = {
           if(tolower(names(modelIn)[[i]]) != scalars.file.name){
             if(!is.null(isolate(input[[paste0("in_", i)]])) && hot.init[[i]]){
               if(length(cols.with.dep[[i]])){
                 if(!is.empty.input[i]){
                   data.tmp[[j]] <<- dplyr::bind_rows(rhandsontable::hot_to_r(isolate(input[[paste0("in_",i)]])), model.input.data[[i]])
                 }else{
                   data.tmp[[j]] <<- model.input.data[[i]]
                 }
               }else{
                 if(!is.empty.input[i]){
                   data.tmp[[j]] <<- rhandsontable::hot_to_r(isolate(input[[paste0("in_",i)]]))
                 }else{
                   data.tmp[[j]] <<- modelInTemplate[[i]]
                 }
               }
             }else if(!is.null(model.input.data[[i]])){
               # tab was never activated, so shiny does not update handsontable thus it is empty although data was loaded
               data.tmp[[j]] <<- model.input.data[[i]]
             }else{
               flog.error("Dataset: '%s' could not be loaded.", modelIn.alias[i])
               errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelIn.alias[i]), sep = "\n")
               return(NULL)
             }
             j <<- j + 1
           }
         },
         slider = {
           if(!is.null(isolate(input[[paste0("slider_", i)]]))){
             value <- isolate(input[[paste0("slider_", i)]])
           }else if(is.numeric(slider.values[[tolower(names(modelIn)[[i]])]]$def)){
             value <- slider.values[[tolower(names(modelIn)[[i]])]]$def
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelIn.alias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelIn.alias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           if(length(value) > 1){
             # double slider (two values)
             scalar      <- paste0(tolower(names(modelIn))[[i]], c("_min", "_max"))
             description <- paste0(modelIn.alias[i], c(" (min)", " (max)"))
           }else{
             # standard slider (one value)
             scalar      <- tolower(names(modelIn))[[i]]
             description <- modelIn.alias[i]
           }
           # generate data frame
           if(is.null(data.tmp[[length(modelIn.file.names)]])){
             # no scalar data was written yet, so add headers
             data.tmp[[length(modelIn.file.names)]]        <<- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(data.tmp[[length(modelIn.file.names)]]) <<- scalars.file.headers
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalars.file.headers
             data.tmp[[length(modelIn.file.names)]] <<- rbind(data.tmp[[length(modelIn.file.names)]], new.data) 
           }
         },
         date = {
           if(!is.null(isolate(input[[paste0("date_", i)]]))){
             value <- as.character(isolate(input[[paste0("date_", i)]]))
           }else if(!is.null(modelIn[[i]]$date$value)){
             value <- as.character(modelIn[[i]]$date$value)
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelIn.alias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelIn.alias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- names(modelIn)[[i]]
           description <- modelIn.alias[i]
           
           # generate data frame
           if(is.null(data.tmp[[length(modelIn.file.names)]])){
             # no scalar data was written yet, so add headers
             data.tmp[[length(modelIn.file.names)]]        <<- data.frame(scalar, description, value, stringsAsFactors = F)
             names(data.tmp[[length(modelIn.file.names)]]) <<- scalars.file.headers
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalars.file.headers
             data.tmp[[length(modelIn.file.names)]] <<- rbind(data.tmp[[length(modelIn.file.names)]], new.data) 
           }
         },
         daterange = {
           if(!is.null(isolate(input[[paste0("daterange_", i)]]))){
             value <- as.character(isolate(input[[paste0("daterange_", i)]]))
           }else if(!is.null(modelIn[[i]]$daterange$start) && !is.null(modelIn[[i]]$daterange$end)){
             value <- c(as.character(modelIn[[i]]$daterange$start), as.character(modelIn[[i]]$daterange$end))
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelIn.alias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelIn.alias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- paste0(names(modelIn)[[i]], c("_min", "_max"))
           description <- paste0(modelIn.alias[i], c(" (min)", " (max)"))
        
           # generate data frame
           if(is.null(data.tmp[[length(modelIn.file.names)]])){
             # no scalar data was written yet, so add headers
             data.tmp[[length(modelIn.file.names)]]        <<- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(data.tmp[[length(modelIn.file.names)]]) <<- scalars.file.headers
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalars.file.headers
             data.tmp[[length(modelIn.file.names)]] <<- rbind(data.tmp[[length(modelIn.file.names)]], new.data) 
           }
         },
         dropdown = {
           if(!is.null(isolate(input[[paste0("dropdown_", i)]]))){
             value <- isolate(input[[paste0("dropdown_", i)]])
           }else if(!is.null(modelIn[[i]]$dropdown$selected)){
             value <- modelIn[[i]]$dropdown$selected
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelIn.alias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelIn.alias[i]), sep = "\n")
             return(NULL)
           }
           
           if(identical(modelIn[[i]]$dropdown$multiple, TRUE)){
             # generate data frame (multi dropdown menu)
             data.tmp[[j]]        <<- data.frame(value, stringsAsFactors = FALSE, check.names = FALSE)
             names(data.tmp[[j]]) <<- tolower(names(modelIn))[[i]]
             j <<- j + 1L
           }else{
             # standard dropdown menu (one value)
             scalar      <- names(modelIn)[[i]]
             description <- modelIn.alias[i]
             
             if(is.null(data.tmp[[length(modelIn.file.names)]])){
               # no scalar data was written yet, so add headers
               data.tmp[[length(modelIn.file.names)]]        <<- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
               names(data.tmp[[length(modelIn.file.names)]]) <<- scalars.file.headers
             }else{
               # no headers, just data
               new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
               names(new.data) <- scalars.file.headers
               data.tmp[[length(modelIn.file.names)]] <<- rbind(data.tmp[[length(modelIn.file.names)]], new.data)
             }
           }
         },
        # dropdowne = {
        #   if(length(model.input.data[[i]][[1]])){
        #     if(!is.null(isolate(input[[paste0("dropdowne_", i)]]))){
        #       # move row with selected item to top
        #       idx.selected <- !is.na(match(model.input.data[[i]][[1]], isolate(input[[paste0("dropdowne_", i)]])))
        #       if(length(idx.selected)){
        #         data.tmp[[j]] <<- rbind(model.input.data[[i]][idx.selected, ], model.input.data[[i]][!idx.selected, ])
        #       }else{
        #         data.tmp[[j]] <<- model.input.data[[i]]
        #       }
        #       rm(idx.selected)
        #     }else{
        #       data.tmp[[j]] <<- model.input.data[[i]]
        #     }
        #   }else{
        #     errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelIn.alias[i]), sep = "\n")
        #     return(NULL)
        #   }
        #
        #   if(length(data.tmp[[j]]) > 1){
        #     # dropdown has aliases and thus 2 columns
        #     names(data.tmp[[j]]) <<- c(names(modelIn)[[i]], "alias")
        #   }else{
        #     # dropdown has no aliases and thus 1 column
        #     names(data.tmp[[j]]) <<- names(modelIn)[[i]]
        #   }
        #   j <<- j + 1L
        # },
         checkbox = {
           if(!is.null(isolate(input[[paste0("cb_", i)]]))){
             value <- if(identical(isolate(input[[paste0("cb_", i)]]), TRUE)) 1L else 0L
           }else if(!is.null(modelIn[[i]]$checkbox$value)){
             value <- if(identical(modelIn[[i]]$checkbox$value, TRUE)) 1L else 0L
           }else{
             flog.error("Dataset: '%s' could not be loaded.", modelIn.alias[i])
             errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$noData, modelIn.alias[i]), sep = "\n")
             return(NULL)
           }
           # add name and description fields
           scalar      <- names(modelIn)[[i]]
           description <- modelIn.alias[i]
           
           # generate data frame
           if(is.null(data.tmp[[length(modelIn.file.names)]])){
             # no scalar data was written yet, so add headers
             data.tmp[[length(modelIn.file.names)]]        <<- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(data.tmp[[length(modelIn.file.names)]]) <<- scalars.file.headers
           }else{
             # no headers, just data
             new.data        <- data.frame(scalar, description, value, stringsAsFactors = FALSE, check.names = FALSE)
             names(new.data) <- scalars.file.headers
             data.tmp[[length(modelIn.file.names)]] <<- rbind(data.tmp[[length(modelIn.file.names)]], new.data) 
           }
         }
  )
  flog.trace("Dataset: %s saved in data.tmp.", modelIn[[i]])
})
showErrorMsg(lang$errMsg$GAMSInput$title, errMsg)