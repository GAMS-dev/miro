ScenarioMetadata <- R6Class("ScenarioMetadata",
  public = list(
    initialize = function(name = NULL, tags = NULL, owner = NULL, lastModified = NULL, rv = NULL) {
      private$rv <- rv
      self$name <- name
      self$tags <- tags
      self$lastModified <- lastModified
      private$setMetaDataEl("_uid", owner)
      return(self)
    }
  ),
  active = list(
    name = function(value) {
      if (missing(value)) {
        return(private$metadata[["_sname"]])
      }
      if (!is.null(value) && isBadScenName(value)) {
        stop_custom(
          "error_bad_name",
          "Scenario name must contain at least one non-whitespace character and must not have more than 63 characters",
          call. = FALSE
        )
      }
      private$setMetaDataEl("_sname", value)
      if (!is.null(private$rv)) {
        if (identical(isolate(private$rv$activeSname), value)) {
          # refresh observer even when name is not changed
          # (e.g. due to different owner)
          isolate({
            private$rv$sandboxUnsaved <- !private$rv$sandboxUnsaved
            private$rv$sandboxUnsaved <- !private$rv$sandboxUnsaved
          })
        } else {
          isolate(private$rv$activeSname <- value)
        }
      }
    },
    tags = function(value) {
      if (missing(value)) {
        return(private$metadata[["_stag"]])
      }
      tagsUnique <- unique(value)
      if (isBadScenTags(scenTagsV = tagsUnique)) {
        stop_custom(
          "error_bad_tags",
          "Invalid scenario tags",
          call. = FALSE
        )
      }
      private$setMetaDataEl("_stag", tagsUnique)
    },
    lastModified = function(value) {
      if (missing(value)) {
        return(private$metadata[["_stime"]])
      }
      if (!is.null(value) && !"POSIXct" %in% class(value) &&
        identical(tryCatch(as.POSIXct(value), error = function(e) {
          return(FALSE)
        }), FALSE)) {
        stop_custom(
          "error_bad_time",
          "Invalid modified time",
          call. = FALSE
        )
      }
      private$setMetaDataEl("_stime", value)
    },
    owner = function(value) {
      if (missing(value)) {
        return(private$metadata[["_uid"]])
      }
      stop_custom(
        "error_locked",
        "Uid can not be set",
        call. = FALSE
      )
    }
  ),
  private = list(
    metadata = list(),
    rv = NULL,
    setMetaDataEl = function(name, value) {
      if (is.null(value)) {
        return(self)
      }
      private$metadata[[name]] <- value
      return(self)
    }
  )
)
