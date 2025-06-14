# save scenario to database

genAccessPermInputs <- function(containerSelector, resetPerm = FALSE) {
  showEl(session, paste0(containerSelector, " .access-perm-spinner"))
  on.exit(hideEl(session, paste0(containerSelector, " .access-perm-spinner")))
  if (tryCatch(
    {
      if (isShinyProxy) {
        accessGroups <- worker$getRemoteAccessGroups()
        db$setRemoteUsers(accessGroups)
      } else {
        accessGroups <- db$getUserAccessGroups()
      }
      FALSE
    },
    error = function(e) {
      flog.warn(
        "Problems fetching user access groups. Error message: %s",
        conditionMessage(e)
      )
      insertUI(containerSelector,
        ui = tags$div(
          class = "err-msg",
          lang$errMsg$unknownError
        )
      )
      return(TRUE)
    }
  )) {
    return(FALSE)
  }
  if (resetPerm) {
    readPerm <- c(uid, DEFAULT_SCEN_PERM$read)
    writePerm <- c(uid, DEFAULT_SCEN_PERM$write)
    execPerm <- c(uid, DEFAULT_SCEN_PERM$execute)
  } else {
    metaTmp <- activeScen$getMetadataDf(noPermFields = FALSE)
    readPerm <- csv2Vector(metaTmp[["_accessr"]][[1]])
    writePerm <- csv2Vector(metaTmp[["_accessw"]][[1]])
    execPerm <- csv2Vector(metaTmp[["_accessx"]][[1]])
  }
  insertUI(containerSelector,
    ui = tagList(
      tags$div(
        class = "input-form-mobile",
        accessPermInput("editMetaReadPerm", lang$nav$excelExport$metadataSheet$readPerm,
          sort(unique(c(readPerm, accessGroups))),
          selected = readPerm, width = "100%"
        )
      ),
      tags$div(
        class = "input-form-mobile",
        accessPermInput("editMetaWritePerm", lang$nav$excelExport$metadataSheet$writePerm,
          sort(unique(c(writePerm, accessGroups))),
          selected = writePerm, width = "100%"
        )
      ),
      tags$div(
        class = "input-form-mobile",
        accessPermInput("editMetaExecPerm", lang$nav$excelExport$metadataSheet$execPerm,
          sort(unique(c(execPerm, accessGroups))),
          selected = execPerm, width = "100%"
        )
      )
    )
  )
  return(TRUE)
}

# Save As button clicked
observeEvent(input$btSaveAs, {
  saveAsFlag <<- TRUE
  flog.debug("%s: Save As button clicked.", uid)
  rv$btRemoveOutputData <<- rv$btRemoveOutputData + 1L
})
# Save button clicked
observeEvent(input$btSave, {
  saveAsFlag <<- FALSE
  flog.debug("%s: Save button clicked.", uid)
  rv$btRemoveOutputData <<- rv$btRemoveOutputData + 1L
})

observeEvent(virtualActionButton(rv$btRemoveOutputData), {
  saveOutput <<- TRUE
  if (rv$dirtyFlag) {
    showRemoveExistingOutputDataDialog()
  } else {
    if (saveAsFlag || is.null(activeScen) || !length(activeScen$getSid())) {
      rv$btSaveAs <<- rv$btSaveAs + 1L
    } else {
      # overwrite current scenario data
      rv$btSaveConfirm <<- rv$btSaveConfirm + 1L
    }
  }
})
observeEvent(input$btRemoveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be removed.", uid)
  saveOutput <<- FALSE
  if (saveAsFlag || is.null(activeScen) || !length(activeScen$getSid())) {
    rv$btSaveAs <<- rv$btSaveAs + 1L
  } else {
    # overwrite current scenario data
    rv$btSaveConfirm <<- rv$btSaveConfirm + 1L
  }
})
observeEvent(input$btSaveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be saved regardless of possible corruption", uid)
  saveOutput <<- TRUE
  if (saveAsFlag || is.null(activeScen) || !length(activeScen$getSid())) {
    rv$btSaveAs <<- rv$btSaveAs + 1L
  } else {
    # overwrite current scenario data
    rv$btSaveConfirm <<- rv$btSaveConfirm + 1L
  }
})
observeEvent(
  input$btSaveReadonly,
  rv$btSaveAs <<- rv$btSaveAs + 1L
)
# enter scenario name
observeEvent(virtualActionButton(rv$btSaveAs), {
  saveAsFlag <<- TRUE
  showNewScenDialog(activeScen$getScenName(),
    scenTags = activeScen$getStags(),
    allScenTags = db$getAllScenTags(),
    showPermissionSelectors = isShinyProxy
  )
  if (isShinyProxy) {
    genAccessPermInputs("#contentAccessPerm",
      resetPerm = !identical(activeScen$getScenUid(), uid)
    )
  }
})

observeEvent(input$btNewName, {
  hideEl(session, "#scenarioExists")
  showEl(session, "#scenNameWrapper")
  showEl(session, "#dialogSaveInit")
  hideEl(session, "#dialogSaveConfirm")
  return(NULL)
})


# check scenario name
observeEvent(input$btCheckName, {
  scenName <- input$scenName
  scenTags <<- isolate(input$newScenTags)
  flog.debug("Name for new scenario entered: %s.", scenName)

  if (isBadScenName(scenName)) {
    showHideEl(session, "#badScenarioName", 4000L)
    return()
  }
  errMsg <- NULL
  tryCatch(
    {
      if (db$checkSnameExists(scenName)) {
        scenExists <- TRUE
      } else {
        scenExists <- FALSE
      }
    },
    error = function(e) {
      flog.error(
        "Some error occurred while checking whether scenario: '%s' exists. Error message: '%s'.",
        scenName, conditionMessage(e)
      )
      errMsg <<- lang$errMsg$fetchScenData$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))) {
    return()
  }
  if (scenExists) {
    showEl(session, "#scenarioExists")
    hideEl(session, "#scenNameWrapper")
    hideEl(session, "#dialogSaveInit")
    showEl(session, "#dialogSaveConfirm")
    return(NULL)
  } else {
    scenTags <<- scenTags
    rv[[input$btCheckName]] <- rv[[input$btCheckName]] + 1L
  }
})
observeEvent(
  input$btSaveConfirm,
  rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
)
observeEvent(virtualActionButton(rv$btSaveConfirm), {
  # check whether scenario is currently locked
  errMsg <- NULL
  if (!is.null(activeScen) && !saveAsFlag) {
    tryCatch(
      {
        if (activeScen$isReadonlyOrLocked) {
          showReadonlyDialog()
          flog.info("Scenario is readonly or locked.")
          return(NULL)
        }
      },
      error = function(e) {
        flog.error(
          "Problems while trying to determine whether scenario is readonly or locked. Error message: %s.",
          conditionMessage(e)
        )
        errMsg <<- lang$errMsg$lockScen$desc
      }
    )
    if (is.null(showErrorMsg(lang$errMsg$lockScen$title, errMsg))) {
      return(NULL)
    }
  }

  if (tryCatch(
    {
      if (!saveOutput) {
        scenData$clearSandbox()
      }
      scenData$loadSandbox(
        getInputDataFromSandbox(),
        if (length(modelInFileNames)) modelInFileNames else character(),
        activeScen$getMetadataDf()
      )
      FALSE
    },
    no_data = function(e) {
      flog.error(conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
      return(TRUE)
    },
    error = function(e) {
      flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
      return(TRUE)
    }
  )) {
    return()
  }
  removeModal()
  # save to database
  duplicatedMetadata <- NULL
  currentScenHash <- character(0L)
  if (is.null(activeScen)) {
    currentSbScenName <- lang$nav$dialogNewScen$newScenName
  } else {
    currentSbScenName <- activeScen$getScenName()
  }
  tryCatch(
    {
      if (saveAsFlag) {
        if (!is.null(activeScen)) {
          currentScenHash <- activeScen$getScenHash()
          if (length(activeScen$getSid())) {
            duplicatedMetadata <- activeScen$getMetadataInfo(
              input$newScenDiscardAttach,
              list(
                read = input$editMetaReadPerm,
                write = input$editMetaWritePerm,
                execute = input$editMetaExecPerm
              )
            )
            activeScen <<- NULL
            gc()
            if (isTRUE(input$newScenDiscardAttach) && length(currentScenHash)) {
              attachmentListTmp <- attachments$getMetadata()
              if (length(attachmentListTmp) && any(attachmentListTmp[["execPerm"]])) {
                currentScenHash <- character(0L)
              }
            }
          } else {
            activeScen$updateMetadata(
              newName = input$scenName, newTags = scenTags,
              newReadPerm = input$editMetaReadPerm,
              newWritePerm = input$editMetaWritePerm,
              newExecPerm = input$editMetaExecPerm
            )
            if (isTRUE(input$newScenDiscardAttach)) {
              if (length(currentScenHash)) {
                attachmentListTmp <- attachments$getMetadata()
                if (length(attachmentListTmp) && any(attachmentListTmp[["execPerm"]])) {
                  activeScen$setScenHash(character(0L))
                }
              }
              attachments$removeAll()
            }
          }
        }
        if (identical(input$scenName, currentSbScenName)) {
          # make sure title is refreshed even when scen name is identical
          # (e.g. because owner changed)
          markUnsaved()
        } else {
          currentSbScenName <- input$scenName
        }
        if (isTRUE(input$newScenDiscardViews)) {
          views$clearConf()
        }
      }
      if (is.null(activeScen)) {
        activeScen <<- Scenario$new(
          db = db, sname = currentSbScenName,
          tags = scenTags, overwrite = identical(saveAsFlag, TRUE),
          isNewScen = TRUE, duplicatedMetadata = duplicatedMetadata,
          views = views, attachments = attachments,
          rv = rv
        )
        activeScen$setScenHash(currentScenHash)
        scenTags <<- NULL
      }
      if (rv$dirtyFlag) {
        activeScen$setScenHash(character(0L))
      }
      activeScen$save(scenData$get("sb", includeHiddenScalars = TRUE), msgProgress = lang$progressBar$saveScenDb)
      if (saveOutput) {
        if (config$saveTraceFile && length(traceData)) {
          activeScen$saveTraceData(traceData)
        }
        if (!is.null(scriptOutput) && scriptOutput$hasResults()) {
          activeScen$saveScriptResults(scriptOutput$getResults())
        }
      }
      flog.debug("%s: Scenario saved to database (Scenario: %s).", uid, activeScen$getScenName())
    },
    error_sname_exists = function(e) {
      flog.info("A scenario with the same name already exists. Please choose another name.")
      errMsg <<- lang$nav$dialogEditMeta$scenExists
    },
    error = function(e) {
      flog.error(
        "Some error occurred saving scenario to database. Error message: %s.",
        conditionMessage(e)
      )
      errMsg <<- lang$errMsg$saveScen$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))) {
    return(NULL)
  }
  # check whether output data was saved and in case it was set identifier accordingly
  if (scenData$getSandboxHasOutputData(scriptOutput)) {
    noOutputData <<- FALSE
  } else {
    noOutputData <<- TRUE
  }
  if (!saveOutput) {
    # remove output from UI
    renderOutputData()
  }

  # reset dirty flag and unsaved status
  markSaved()
})
editMetaRemoteAccessPermLoaded <- FALSE
observeEvent(input$btEditMeta, {
  flog.debug("Button to open metadata dialog clicked.")
  req(activeScen)

  editMetaRemoteAccessPermLoaded <<- FALSE
  attachmentMetadata <- NULL
  viewsMetadata <- NULL
  if (config$activateModules$attachments) {
    attachmentList <<- attachments$getMetadata()
    attachmentMetadata <- attachmentList
    viewsMetadata <- views$getSummary(c(modelInRaw, scalarsInMetaData), modelOut)
  }
  showEditMetaDialog(activeScen$getMetadataDf(noPermFields = FALSE),
    allowAttachments = config$activateModules$attachments,
    attachmentMetadata = attachmentMetadata,
    viewsMetadata = viewsMetadata,
    attachAllowExec = attachAllowExec,
    ugroups = csv2Vector(db$getUserAccessGroups()),
    isLocked = length(activeScen) != 0L && length(activeScen$getLockUid()) > 0L,
    selectedTab = input$btEditMeta,
    allScenTags = db$getAllScenTags(),
    showPermissionSelectors = isShinyProxy
  )
})

if (isShinyProxy) {
  observeEvent(input$tpEditMeta, {
    if (!identical(input$tpEditMeta, "accessPerm") ||
      identical(length(activeScen), 0L) ||
      length(activeScen$getLockUid()) > 0L) {
      return()
    }
    flog.trace("Access permissions tab in metadata dialog selected")
    removeUI("#contentAccessPerm .form-group", multiple = TRUE)
    if (genAccessPermInputs("#contentAccessPerm")) {
      editMetaRemoteAccessPermLoaded <<- TRUE
    }
  })
}

observeEvent(input$btUpdateMeta, {
  flog.debug("Button to update metadata clicked.")
  req(activeScen)
  scenName <- isolate(input$editMetaName)
  hideEl(session, "#editMetaBadName")
  hideEl(session, "#editMetaError")
  hideEl(session, "#editMetaNameExists")
  hideEl(session, "#attachMaxSizeError")
  hideEl(session, "#attachMaxNoError")
  hideEl(session, "#attachDuplicateError")
  hideEl(session, "#attachUnknownError")

  if (isBadScenName(scenName)) {
    flog.debug("Edit metadata: Invalid scenario name.")
    showHideEl(session, "#editMetaBadName", 6000)
    return()
  }
  disableEl(session, "#btUpdateMeta")
  errMsg <- NULL
  tryCatch(
    {
      if (db$checkSnameExists(scenName)) {
        scenExists <- TRUE
      } else {
        scenExists <- FALSE
      }
    },
    error = function(e) {
      flog.error(
        "Some error occurred while checking whether scenario: '%s' exists. Error message: '%s'.",
        scenName, conditionMessage(e)
      )
      errMsg <<- lang$errMsg$fetchScenData$desc
    }
  )
  if (!is.null(errMsg)) {
    showHideEl(session, "#editMetaError", 6000)
    return()
  }
  if ((!identical(activeScen$getScenName(), scenName)) && scenExists) {
    flog.debug("Edit metadata: Scenario with same name already exists.")
    enableEl(session, "#btUpdateMeta")
    showHideEl(session, "#editMetaNameExists", 6000)
    return()
  }
  currentReadPerm <- activeScen$getReadPerm()
  currentWritePerm <- activeScen$getWritePerm()
  currentExecPerm <- activeScen$getExecPerm()

  if (activeScen$isReadonlyOrLocked) {
    newWritePerm <- character(0L)
    newExecPerm <- character(0L)
    newReadPerm <- character(0L)
  } else if (editMetaRemoteAccessPermLoaded) {
    activeUserGroups <- c(db$getUserAccessGroups(), db$getRemoteUsers())

    newWritePerm <- input$editMetaWritePerm
    newExecPerm <- input$editMetaExecPerm
    newReadPerm <- input$editMetaReadPerm
    if (any(c(length(newReadPerm), length(newWritePerm), length(newExecPerm)) < 1L)) {
      enableEl(session, "#btUpdateMeta")
      flog.debug("Empty permissions entered.")
      showHideEl(session, "#editMetaEmptyPerm", 6000)
      return()
    }
    if (any(!newReadPerm %in% c(activeUserGroups, currentReadPerm))) {
      showHideEl(session, "#editMetaError")
      flog.error("Attempt to tamper with read access permissions!")
      return()
    }
    if (any(!newWritePerm %in% c(activeUserGroups, currentWritePerm))) {
      showHideEl(session, "#editMetaError")
      flog.error("Attempt to tamper with write access permissions!")
      return()
    }
    if (any(!newExecPerm %in% c(activeUserGroups, currentExecPerm))) {
      showHideEl(session, "#editMetaError")
      flog.error("Attempt to tamper with execute access permissions!")
      return()
    }
    scenOwner <- activeScen$getScenUid()[[1L]]
    if ((!scenOwner %in% newReadPerm) ||
      (!scenOwner %in% newWritePerm) ||
      (!scenOwner %in% newExecPerm)) {
      showHideEl(session, "#editMetaIncapOwner")
      flog.debug("Attempt to revoke the scenario owner's access rights.")
      return()
    }
  } else {
    newReadPerm <- currentReadPerm
    newWritePerm <- currentWritePerm
    newExecPerm <- currentExecPerm
  }

  tryCatch(
    {
      activeScen$updateMetadata(
        scenName, isolate(input$editMetaTags),
        newReadPerm, newWritePerm, newExecPerm
      )
      markUnsaved()
      removeModal()
    },
    error = function(e) {
      flog.error("Problems updating scenario metadata. Error message: %s.", conditionMessage(e))
      errMsg <<- character(1L)
    }
  )
  if (!is.null(errMsg)) {
    enableEl(session, "#btUpdateMeta")
    showHideEl(session, "#editMetaError")
    return()
  }
})

if (config$activateModules$attachments) {
  ################################################
  #            ATTACHMENTS
  ################################################
  lapply(seq_len(attachMaxNo), function(i) {
    observeEvent(input[["btRemoveAttachment_" %+% i]], {
      req(nchar(attachmentList[["name"]][[i]]) > 0L, activeScen)
      attachments$remove(session = NULL, attachmentList[["name"]][[i]])
      attachmentList[i, ] <<- list(NA_character_, FALSE)
      showHideEl(session, "#attachSuccess")
    })
    observe({
      value <- suppressWarnings(as.logical(input[["execPermAttachment_" %+% i]]))
      req(activeScen)
      if (is.na(attachmentList[["name"]][[i]]) || length(value) != 1L || is.na(value) ||
        value == attachmentList[["execPerm"]][[i]]) {
        return(NULL)
      }
      tryCatch(
        {
          attachments$setExecPerm(
            session = NULL, attachmentList[["name"]][[i]],
            value
          )
          attachmentList[i, "execPerm"] <<- value
          showHideEl(session, "#attachSuccess")
        },
        error = function(e) {
          flog.error(e)
          showHideEl(session, "#attachUnknownError", 6000)
        }
      )
    })
  })
  output$downloadAttachmentData <- downloadHandler(
    filename = function() {
      i <- isolate(input$downloadAttachment)
      if (!is.integer(i) || length(attachmentList[["name"]]) < i) {
        return("error.txt")
      }
      attachmentList[["name"]][[input$downloadAttachment]]
    },
    content = function(file) {
      i <- input$downloadAttachment
      if (!is.integer(i) || length(attachmentList[["name"]]) < i) {
        return(writeLines("error", file))
      }
      attachments$download(file,
        fileNames = attachmentList[["name"]][[i]],
        fullPath = TRUE
      )
    }
  )
  observeEvent(input$file_addAttachments$datapath, {
    req(activeScen)

    showEl(session, "#addAttachLoading")
    hideEl(session, "#attachMaxSizeError")
    hideEl(session, "#attachMaxNoError")
    hideEl(session, "#attachDuplicateError")
    hideEl(session, "#attachUnknownError")
    errMsg <- NULL

    tryCatch(
      {
        attachments$add(
          session = NULL, isolate(input$file_addAttachments$datapath),
          fileNames = isolate(input$file_addAttachments$name)
        )
        idxes <- vector("integer", length(isolate(input$file_addAttachments$name)))
        for (i in seq_along(isolate(input$file_addAttachments$name))) {
          idx <- which(is.na(attachmentList[["name"]]))
          if (!length(idx)) {
            idx <- nrow(attachmentList) + 1L
            if (idx > attachMaxNo) {
              stop("The number of attachments exceeds the maximum allowed number.
               This is not supposed to happen as addAttachment method of Scenario class should raise an appropriate exception!", call. = FALSE)
            }
            attachmentList <<- add_row(attachmentList, name = isolate(input$file_addAttachments$name)[[i]], execPerm = TRUE)
          } else {
            idx <- idx[[1]]
            attachmentList[idx, ] <<- list(isolate(input$file_addAttachments$name)[[i]], TRUE)
          }
          idxes[[i]] <- idx
        }
        updateAttachList(session,
          fileName = isolate(input$file_addAttachments$name), id = idxes, token = session$token,
          labelCb = lang$nav$dialogEditMeta$attachmentsExecPerm, allowExec = attachAllowExec
        )
        showHideEl(session, "#attachSuccess")
      },
      error_forbidden_filename = function(e) {
        flog.info("Attachment wasn't added because the filename is forbidden.")
        showHideEl(session, "#attachForbiddenFnameError", 6000)
      },
      error_max_size = function(e) {
        flog.info("Attachment wasn't added because the size is too large.")
        showHideEl(session, "#attachMaxSizeError", 6000)
      },
      error_duplicate_files = function(e) {
        flog.info("Attachment wasn't added because the filename already exists.")
        showHideEl(session, "#attachDuplicateError", 6000)
      },
      error_max_no = function(e) {
        flog.info("Attachment wasn't added because the maximum number of attachment is reached.")
        showHideEl(session, "#attachMaxNoError", 6000)
      },
      error = function(e) {
        flog.error(conditionMessage(e))
        showHideEl(session, "#attachUnknownError", 6000)
      }
    )
    hideEl(session, "#addAttachLoading")
  })
  ################################################
  #            VIEWS
  ################################################
  updateViewsTable <- function() {
    newViewData <- views$getSummary(c(modelInRaw, scalarsInMetaData), modelOut)
    if (!length(newViewData$symAlias)) {
      newViewData <- tibble(
        symAlias = character(0L),
        id = character(0L),
        symName = character(0L)
      )
    }
    session$sendCustomMessage(
      "gms-updateTable",
      list(
        id = "currentViewsTable", hierarchical = TRUE,
        hierarchicalCols = I(0),
        valCol = 2L,
        data = list(
          I(newViewData[["symAlias"]]),
          I(newViewData[["id"]]),
          I(newViewData[["symName"]])
        )
      )
    )
  }
  observeEvent(input$removeViews, {
    req(activeScen)
    showEl(session, "#addViewsLoading")
    flog.debug("Request to remove views received.")
    on.exit(hideEl(session, "#addViewsLoading"))

    viewsToRemove <- tryCatch(
      {
        safeFromJSON(input$removeViews,
          simplifyMatrix = FALSE
        )
      },
      error = function(e) {
        flog.error(
          "Problems parsing request to remove views. This seems like an attempt to tamper with the app! Error message: %s.",
          conditionMessage(e)
        )
        return(NULL)
      }
    )
    if (is.null(viewsToRemove)) {
      return()
    }
    if (length(viewsToRemove) == 0) {
      showHideEl(session, "#viewsNoneSelected", delay = 4000L)
      return()
    }

    if (tryCatch(
      {
        views$removeConf(viewsToRemove)
        FALSE
      },
      error = function(e) {
        flog.warn(
          "Problems removing rows from table: %s. Error message: %s.",
          as.character(input$removeViews), conditionMessage(e)
        )
        return(TRUE)
      }
    )) {
      showHideEl(session, "#viewsUnknownError", 4000L)
      return()
    }
    updateViewsTable()
  })
  output$downloadViews <- downloadHandler(
    filename = function() {
      scenName <- activeScen$getScenName()
      if (!length(scenName)) {
        scenName <- lang$nav$dialogNewScen$newScenName
      }
      return(paste0(scenName, "_views.json"))
    },
    content = function(file) {
      flog.debug("Request to download views received.")
      tryCatch(
        write_file(views$getJSON(safeFromJSON(input$downloadViews,
          simplifyMatrix = FALSE
        )), file),
        error = function(e) {
          flog.warn(
            "Problems writing views JSON file. Error message: %s",
            conditionMessage(e)
          )
          return(writeLines('{"error": "Some problem occurred while writing views."}', file))
        }
      )
    }, contentType = "application/json"
  )
  observeEvent(input$file_addViews$datapath, {
    req(activeScen)
    flog.debug("New view configuration uploaded.")
    showEl(session, "#addViewsLoading")
    on.exit(hideEl(session, "#addViewsLoading"))

    if (!identical(try(tolower(tools::file_ext(input$file_addViews$datapath)),
      silent = TRUE
    ), "json")) {
      showHideEl(session, "#viewsInvalidDataError", 4000L)
      return()
    }

    if (tryCatch(
      {
        views$addConf(safeFromJSON(read_file(input$file_addViews$datapath),
          simplifyDataFrame = FALSE, simplifyVector = FALSE
        ))
        invalidViews <- views$getInvalidViews()
        if (length(invalidViews)) {
          showHideEl(session, "#viewsCustomError",
            msg = sprintf(
              lang$nav$dialogEditMeta$viewsInvalidError,
              paste(invalidViews, collapse = "', '")
            ), 4000L
          )
        }
        FALSE
      },
      error = function(e) {
        flog.info(
          "Problems adding views configuration. Error message: %s",
          conditionMessage(e)
        )
        showHideEl(session, "#viewsInvalidDataError", 4000L)
        TRUE
      }
    )) {
      return()
    }
    duplicatedViews <- views$getDuplicatedViews()
    if (length(duplicatedViews)) {
      showHideEl(session, "#viewsCustomError",
        msg = sprintf(
          lang$nav$dialogEditMeta$viewsDuplicateError,
          paste(duplicatedViews, collapse = "', '")
        ),
        4000L
      )
    }
    updateViewsTable()
  })
}
