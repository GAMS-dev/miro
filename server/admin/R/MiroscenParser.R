MiroscenParser <- R6::R6Class("MiroscenParser",
  public = list(
    initialize = function() {
      return(invisible(self))
    },
    getModelName = function(path) {
      tmpd <- file.path(tempdir(check = TRUE))
      metadataPath <- file.path(tmpd, "metadata.json")
      if (file.exists(metadataPath) &&
        !identical(unlink(metadataPath, force = TRUE), 0L)) {
        stop(sprintf("Could not remove (temporary) file: %s", metadataPath), call. = FALSE)
      }
      on.exit(unlink(metadataPath, force = TRUE))
      zipContent <- zip::zip_list(path)[[1]]
      if (!"metadata.json" %in% zipContent) {
        stop("Bad miroscen file. Metadata file missing.", call. = FALSE)
      }
      zip::unzip(path, files = "metadata.json", junkpaths = TRUE, exdir = tmpd)
      if (isTRUE(nzchar(Sys.readlink(metadataPath), keepNA = TRUE))) {
        stop("Symlink detected in miroscen file.", call. = FALSE)
      }
      metadata <- suppressWarnings(jsonlite::fromJSON(metadataPath,
        simplifyDataFrame = TRUE, simplifyVector = FALSE
      ))
      if (any(!c("model", "model_raw") %in% names(metadata))) {
        stop("Bad miroscen file. Invalid metadata.json file.", call. = FALSE)
      }
      return(list(metadata[["model"]], metadata[["model_raw"]]))
    }
  ),
  private = list()
)
