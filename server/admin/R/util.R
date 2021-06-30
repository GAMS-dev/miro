CharArray <- R6::R6Class("CharArray", public = list(
  push = function(el){
    stopifnot(length(el) > 0L, is.character(el))
    private$items[[self$size() + 1L]] <- el
    invisible(self)
  },
  pop = function(){
    stopifnot(self$size() > 0L)
    private$items[[self$size()]]
    invisible(self)
  },
  delete = function(el){
    stopifnot(identical(length(el), 1L), is.character(el))
    
    idx <- match(el, private$items)[[1L]]
    if(is.na(idx)){
      return(FALSE)
    }
    private$items[[idx]] <- NULL
    return(TRUE)
  },
  reset = function(){
    private$items <- private$initialItems
    invisible(self)
  },
  update = function(old, new){
    if(length(old)){
      stopifnot(identical(length(old), 1L), is.character(old))
    }else{
      self$push(new)
      return(TRUE)
    }
    if(length(new)){
      stopifnot(identical(length(new), 1L), is.character(new))
    }else if(self$delete(old)){
      return(TRUE)
    }else{
      return(FALSE)
    }
    
    idx <- match(old, private$items)[[1L]]
    if(is.na(idx)){
      return(FALSE)
    }
    private$items[[idx]] <- new
    return(TRUE)
  },
  get = function(){
    as.character(unlist(private$items, use.names = FALSE))
  },
  initialize = function(el = NULL){
    if(length(el)){
      stopifnot(is.character(el))
      private$initialItems <- as.list(el)
      private$items <- private$initialItems
    }
    invisible(self)
  },
  size = function(){
    length(private$items)
  }
), private = list(
  initialItems = list(),
  items = list()
))

Set <- R6::R6Class("Set", inherit = CharArray, public = list(
  push = function(el){
    el <- as.character(el)
    stopifnot(identical(length(el), 1L))
    if(!el %in% private$items){
      private$items[[self$size() + 1L]] <- el
    }
    invisible(self)
  },
  join = function(set){
    set <- as.character(set)
    newEl <- !set %in% private$items
    if(any(newEl)){
      private$items <- c(private$items, as.list(set[newEl]))
    }
    invisible(self)
  },
  initialize = function(el = NULL){
    if(length(el)){
      el <- unique(as.character(el))
      private$initialItems <- as.list(el)
      private$items <- private$initialItems
    }
    invisible(self)
  },
  update = function(old, new){
    if(length(new) && new %in% private$items){
      return(invisible(self))
    }
    super$update(old, new)
  })
)

MiroVersion <- R6::R6Class("MiroVersion", public = list(
  initialize = function(version){
    private$version <- private$parseVersion(version)
    return(invisible(self))
  },
  laterThan = function(versionToCompare){
    versionToCompare <- private$parseVersion(versionToCompare)
    return(private$version[1] > versionToCompare[1] ||
     (private$version[1] == versionToCompare[1] && (private$version[2] > versionToCompare[2] ||
      (private$version[2] == versionToCompare[2] && private$version[3] > versionToCompare[3]))))
  }), private = list(
    version = NULL,
    parseVersion = function(version){
      version <- as.integer(strsplit(version, ".", fixed = TRUE)[[1]])
      if(is.na(version[1]) || length(version) != 3L){
        stop("invalid MIRO version", call. = FALSE)
      }
      return(version)
    }
  )
)

getLogoB64 <- function(logoPath) {
    if(!length(logoPath)){
        return("")
    }

    logoFormat <- tools::file_ext(logoPath)
    
    logoB64 <- gsub("[\r\n]", "", jsonlite::base64_enc(readr::read_file_raw(logoPath)))

    if(logoFormat == "png"){
        logoB64 <- paste0("data:image/png;base64,", logoB64)
    }else if(logoFormat == "jpg"){
        logoB64 <- paste0("data:image/jpeg;base64,", logoB64)
    }else{
        stop(sprintf("Invalid logo format: %s. Only png/jpg supported.",
            logoFormat), call. = FALSE)
    }
    if(file.size(logoPath) > MAX_LOGO_SIZE){
        stop(sprintf("The logo exceeds the maximum logo size of %s bytes. The default logo will be used.",
          as.character(MAX_LOGO_SIZE)), call. = FALSE)
    }
    return(logoB64)
}

csv2Vector <- function(csv){
  if(!length(csv)){
    return(character(0L))
  }
  csv <- vapply(csv, function(el){
    if(startsWith(el, ",")){
      return(substring(el, 2L))
    }
    return(el)
  }, character(1L), USE.NAMES = FALSE)
  return(unlist(strsplit(csv, ",", fixed = TRUE), use.names = FALSE))
}

createDirIfNonExistent <- function(dirs){
  nonExistingDirs <- !dir.exists(dirname(dirs))
  if(any(nonExistingDirs)){
    nonExistingDirs <- unique(dirname(dirs)[nonExistingDirs])
    suppressWarnings({
      for(nonExistingDir in nonExistingDirs){
        dir.create(nonExistingDir, recursive=TRUE)
      }
    })
  }
}

file.move <- function(from, to){
  if(!file.copy2(from = from, to = to)){
    return(FALSE)
  }
  if(unlink(from, recursive = TRUE, force = TRUE) != 0){
    flog.warn("Problems removing directory: %s", from)
  }
  return(TRUE)
}

file.copy2 <- function(from, to){
  createDirIfNonExistent(to)
  fromIsDir <- dir.exists(from)
  ret1 <- file.copy(from[!fromIsDir], to[!fromIsDir], overwrite = TRUE)
  if(any(!ret1)){
    return(FALSE)
  }
  fromDirs <- from[fromIsDir]
  toDirs   <- dirname(to[fromIsDir])
  for(i in seq_along(fromDirs)){
    if(!file.copy(fromDirs[i], toDirs[i], 
                  recursive = TRUE, overwrite = TRUE)){
      return(FALSE)
    }
  }
  return(TRUE)
}

loginRequired <- function(session, isLoggedIn){
  if(isLoggedIn || !LOGIN_REQUIRED){
    return(FALSE)
  }
  session$sendCustomMessage("onLoginRequired", 1)
  return(TRUE)
}

escapeAppIds = function(appIds){
    return(tolower(gsub("_", "", appIds, fixed = TRUE)))
}

# taken from Advanced R by Hadley Wickham (https://adv-r.hadley.nz/conditions.html)
# licensed under the MIT license
stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}

checkboxInput_MIRO <- function(inputId, label, value = FALSE){
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value)
    inputTag$attribs$checked <- "checked"
  tags$div(class = "shiny-input-container",
           tags$label(class = "cb-label", "for" = inputId, label),
           tags$div(
             tags$label(class = "checkbox-material",
                        tags$div(class = "form-group", 
                                 tags$div(class = "checkbox",
                                          tags$label(inputTag, tags$span())))
             ))
  )
}

# js utility functions

showEl <- function(session, id){
  session$sendCustomMessage("gms-showEl", id)
}

hideEl <- function(session, id){
  session$sendCustomMessage("gms-hideEl", id)
}

showElReplaceTxt <- function(session, id, txt){
  session$sendCustomMessage("gms-showElReplaceTxt", list(id = id, txt = htmltools::htmlEscape(txt)))
}

showHideEl <- function(session, id, delay = 2000, msg = NULL){
  session$sendCustomMessage("gms-showHideEl", list(id = id, delay = delay, msg = msg))
}

enableEl <- function(session, id){
  session$sendCustomMessage("gms-enableEl", id)
}

disableEl <- function(session, id){
  session$sendCustomMessage("gms-disableEl", id)
}
