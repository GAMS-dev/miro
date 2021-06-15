validateMiroScen <- function(path) {
  tmpd <- file.path(tempdir(check = TRUE), "miroscen")
  if(file.exists(tmpd) &&
     !identical(unlink(tmpd, recursive = TRUE, force = TRUE), 0L)){
    stop(sprintf("Could not remove (temporary) directory: %s", tmpd), call. = FALSE)
  }
  if(!dir.create(tmpd)){
    stop(sprintf("Could not create (temporary) directory: %s", tmpd), call. = FALSE)
  }
  on.exit(unlink(tmpd, recursive = TRUE, force = TRUE))
  zipContent <- zip_list(path)[[1]]
  if(any(!c("metadata.json", "data.gdx") %in% zipContent)){
    stop_custom("error_badformat", "Bad miroscen file. Metadata/data file missing.", call. = FALSE)
  }
  miroScenRootFiles <- c("metadata.json", "data.gdx", "views.json")
  unzip(path, files = "metadata.json", junkpaths = TRUE, exdir = tmpd)
  if(is.symlink(file.path(tmpd, "metadata.json"))){
    stop_custom("error_symlinks", "Symlinks detected in miroscen file.", call. = FALSE)
  }
  requiredMetadataKeys <- c("version", "scen_name",
                            "scen_tags",
                            "cl_args",
                            "attachments", "model",
                            "uid", "last_modified",
                            "time_created")
  metadata <- suppressWarnings(fromJSON(file.path(tmpd, "metadata.json"),
                                        simplifyDataFrame = TRUE, simplifyVector = FALSE))
  if(!length(names(metadata)) || any(!requiredMetadataKeys %in% names(metadata))){
    stop_custom("error_badformat", "Bad miroscen file. Invalid metadata.json file.", call. = FALSE)
  }
  if(!identical(metadata$version, 1L)){
    stop_custom("error_badformat", "Bad miroscen file. Invalid version (can only read version 1).",
                call. = FALSE)
  }
  if(any(vapply(c("version", "scen_name", "model", "uid", "last_modified", "time_created"), function(key){
    length(metadata[[key]]) != 1
  }, logical(1L), USE.NAMES = FALSE))){
    stop_custom("error_badformat", "Bad miroscen file. Invalid values in metadata file.",
                call. = FALSE)
  }
  if(length(metadata[["cl_args"]])){
    dfClArgs <- as_tibble(metadata[["cl_args"]])
    if(length(dfClArgs) != 2L){
      stop_custom("error_badformat", "Invalid command line arguments format in miroscen file.",
                  call. = FALSE)
    }
    if(!all(startsWith(dfClArgs[[1]], prefixDDPar) | startsWith(dfClArgs[[1]], prefixGMSOpt))){
      stop_custom("error_badformat", "Invalid command line arguments in miroscen file.",
                  call. = FALSE)
    }
  }
  if(length(metadata[["attachments"]])){
    dfAttachMeta <- as_tibble(metadata[["attachments"]])
    if(length(dfAttachMeta) != 2L || any(is.na(as.logical(dfAttachMeta[[2]])))){
      stop_custom("error_badformat", "Invalid attachment metadata format in miroscen file.",
                  call. = FALSE)
    }
    if(any(sanitizeFn(dfAttachMeta[[1]]) != dfAttachMeta[[1]])){
      stop_custom("error_badformat", "Invalid attachment filenames.",
                  call. = FALSE)
    }
    if(any(!paste0("attachments/", dfAttachMeta[[1]]) %in% zipContent)){
      stop_custom("error_badformat", "Some attachments are not part of the zip file.",
                  call. = FALSE)
    }
  }
  return(invisible(TRUE))
}
loadMiroScenMeta <- function(path, activeScen, attachments, views, inputNames, zipPath = NULL){
  metadata <- suppressWarnings(fromJSON(file.path(path, "metadata.json"),
                                        simplifyDataFrame = TRUE, simplifyVector = FALSE))
  dfClArgs <- NULL
  if(length(metadata[["cl_args"]])){
    dfClArgs <- as_tibble(metadata[["cl_args"]]) %>% add_column(description = "", .after = 1)
    clArgIds <- match(dfClArgs[[1]], inputNames)
    if(any(is.na(clArgIds))){
      flog.info("Command line argument(s): '%s' found in miroscen file is not part of app configuration. They were ignored.",
                dfClArgs[[1]][is.na(clArgIds)])
      dfClArgs <- dfClArgs[!is.na(clArgIds), ]
    }
  }
  activeScen$updateMetadata(newName = as.character(metadata[["scen_name"]]),
                            newTags = csv2Vector(as.character(metadata[["scen_tags"]])))
  views$addConf(safeFromJSON(read_file(file.path(path, "views.json")),
                             simplifyDataFrame = FALSE, simplifyVector = FALSE))
  if(length(metadata[["attachments"]])){
    attachmentMetadata <- as_tibble(metadata[["attachments"]])
    if(length(attachmentMetadata[[1]])){
      if(length(zipPath)){
        if(!dir.create(file.path(path, "attachments"))){
          stop(sprintf("Could not create (temporary) directory: %s", file.path(path, "attachments")),
               call. = FALSE)
        }
        unzip(zipPath, files = paste0("attachments/", attachmentMetadata[[1]]),
              junkpaths = TRUE, exdir = file.path(path, "attachments"))
      }
      if(is.symlink(file.path(path, "attachments", attachmentMetadata[[1]]))){
        stop_custom("error_symlinks", "Symlinks detected in miroscen file.", call. = FALSE)
      }
      attachments$add(NULL, file.path(path, "attachments", attachmentMetadata[[1]]),
                      fileNames = attachmentMetadata[[1]],
                      execPerm = as.logical(attachmentMetadata[[2]]))
    }
  }
  return(dfClArgs)
}
loadMiroScen <- function(path, activeScen, attachments, views, inputNames, exdir = NULL) {
  tmpd <- file.path(tempdir(check = TRUE), "miroscen")
  if(file.exists(tmpd) &&
     !identical(unlink(tmpd, recursive = TRUE, force = TRUE), 0L)){
    stop(sprintf("Could not remove (temporary) directory: %s", tmpd), call. = FALSE)
  }
  if(!dir.create(tmpd)){
    stop(sprintf("Could not create (temporary) directory: %s", tmpd), call. = FALSE)
  }
  on.exit(unlink(tmpd, recursive = TRUE, force = TRUE))
  unzip(path, files = c("metadata.json", "views.json", "data.gdx"),
        junkpaths = TRUE, exdir = tmpd)
  if(is.symlink(file.path(tmpd, c("metadata.json", "views.json", "data.gdx")))){
    stop_custom("error_symlinks", "Symlinks detected in miroscen file.", call. = FALSE)
  }
  dfClArgs <- loadMiroScenMeta(tmpd, activeScen, attachments, views, inputNames, zipPath = path)
  if(file.exists(file.path(dirname(path), "data.gdx")) &&
     !identical(unlink(file.path(dirname(path), "data.gdx"), force = TRUE), 0L)){
    stop_custom("error_os", sprintf("Could not remove file: '%s'.", file.path(dirname(path), "data.gdx")),
                call. = FALSE)
  }
  if(is.null(exdir)){
    exdir <- dirname(path)
  }
  if(tmpd != exdir && !file.move(file.path(tmpd, "data.gdx"),
                                 file.path(exdir, "data.gdx"))){
    stop_custom("error_os", sprintf("Could not move file: '%s' to: '%s'.",
                                    file.path(tmpd, "data.gdx"),
                                    file.path(dirname(path), "data.gdx")),
                call. = FALSE)
  }
  return(dfClArgs)
}
generateMiroScenMeta <- function(path, metadata, attachments, views,
                                 scenId = NULL, clArgs = character(0L), jobName = NULL){
  if(!dir.create(file.path(path, "attachments"))){
    stop(sprintf("Could not create (temporary) directory: %s", path), call. = FALSE)
  }
  scenId <- if(length(scenId) && scenId != 1L) metadata[[1]][1]
  if(length(metadata[["_scode"]]) && metadata[["_scode"]][1] > 10000L){
    # Hypercube scenario with shared input data
    attachmentSids <- c(scenId, metadata[["_scode"]][1] - 10000L)
  }else{
    attachmentSids <- scenId
  }
  attachmentMetadata <- attachments$getMetadata(attachmentSids)
  if(length(attachmentMetadata) && length(attachmentMetadata[[1]])){
    if(length(attachments$download(file.path(path, "attachments"),
                                   attachmentMetadata[[1]],
                                   scenId = attachmentSids)) != length(attachmentMetadata[[1]])){
      stop("Unexpected error occurred while downloading attachments.", call. = FALSE)
    }
  }
  metadataContent <- list(version = 1L, scen_name = if(is.null(jobName)) metadata[[3]][1] else jobName,
                          scen_tags = metadata[[5]][1],
                          cl_args = clArgs,
                          attachments = attachmentMetadata, model = modelName,
                          model_raw = modelNameRaw,
                          uid = metadata[[2]][1], last_modified = metadata[[4]][1],
                          time_created = as.character(Sys.time(),
                                                      usetz = TRUE, tz = "GMT"))
  write_json(metadataContent, file.path(path, "metadata.json"),
             auto_unbox = TRUE, null = "null")
  write_file(views$getJSON(NULL, if(length(scenId)) scenId else "1"),
             file.path(path, "views.json"))
}
generateMiroScen <- function(path, metadata, data, attachments, views, scenId = NULL) {
  tmpd <- file.path(tempdir(check = TRUE), "miroscen")
  if(file.exists(tmpd) &&
     !identical(unlink(tmpd, recursive = TRUE, force = TRUE), 0L)){
    stop(sprintf("Could not remove (temporary) directory: %s", tmpd), call. = FALSE)
  }
  if(!dir.create(tmpd)){
    stop(sprintf("Could not create (temporary) directory: %s", tmpd), call. = FALSE)
  }
  #on.exit(unlink(tmpd, recursive = TRUE, force = TRUE))
  if(scalarsFileName %in% names(data)){
    clArgs <- filter(data[[scalarsFileName]], startsWith(scalar, "_")) %>%
      select(scalar, value)
  }else{
    clArgs <- character()
  }
  generateMiroScenMeta(tmpd, metadata, attachments, views, scenId, clArgs)
  
  gdxio$wgdx(file.path(tmpd, "data.gdx"), data, squeezeZeros = "n")
  return(zipMiro(path, files = c("metadata.json", "data.gdx",
                                 "views.json", "attachments"),
                 baseDir = tmpd))
}
