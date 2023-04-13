InputWidget <- R6::R6Class(
  "InputWidget",
  public = list(
    initialize = function(id, config, input, output, session, rv, sandboxInputDataObj) {
      private$id <- id
      private$config <- config
      private$input <- input
      private$output <- output
      private$session <- session
      private$rv <- rv
      private$sanboxData <- sandboxInputDataObj
      private$changeObs <- private$observeChanges()
      private$data <- reactiveVal(config$defaultValue)
      return(invisible(self))
    },
    getData = function() {
      return(private$data)
    },
    setData = function(data) {
      stop_custom("error_not_implemented", "setData method not implemented")
    },
    finalize = function() {
      if (is.list(private$changeObs)) {
        lapply(private$changeObs, function(obs) {
          obs$destroy()
        })
      } else {
        private$changeObs$destroy()
      }
    },
    reset = function() {
      self$setData(private$config$defaultValue)
      return(invisible(self))
    }
  ),
  private = list(
    id = NULL,
    config = NULL,
    input = NULL,
    output = NULL,
    session = NULL,
    rv = NULL,
    sanboxData = NULL,
    data = NULL,
    changeObs = NULL,
    ignoreUpdate = 1L,
    observeChanges = function() {
      stop_custom("error_not_implemented", "observeChanges method not implemented")
    }
  )
)

MultiDimWidget <- R6::R6Class("MultiDimWidget",
  inherit = InputWidget,
  public = list(
    initialize = function(id, config, input, output, session, rv, sandboxInputDataObj) {
      private$updateWidget <- reactiveVal(0L)
      config$defaultValue <- config$template
      private$needsPivot <- length(config$pivotCols) > 0L
      if (private$needsPivot) {
        private$pivotIdx <- match(config$pivotCols[[1]], names(config$headers))[[1L]]
        private$staticColHeaders <- attr(config$defaultValue, "aliases")[-c(private$pivotIdx, length(config$defaultValue))]
        config$noDomains <- sum(vapply(config$headers, function(header) {
          identical(header$type, "string")
        }, logical(1L), USE.NAMES = FALSE)) - 1L
      } else {
        private$staticColHeaders <- attr(config$defaultValue, "aliases")
      }
      return(super$initialize(id, config, input, output, session, rv, sandboxInputDataObj))
    }
  ),
  private = list(
    updateWidget = NULL,
    needsPivot = FALSE,
    pivotIdx = NULL,
    staticColHeaders = NULL,
    getPivotedData = function(force = FALSE) {
      dataTmp <- isolate(private$data())
      if (tryCatch(
        {
          dataTmp <- pivot_wider(dataTmp,
            names_from = !!private$pivotIdx,
            values_from = !!length(dataTmp),
            names_sort = isTRUE(private$config$sortPivotCols)
          )
          FALSE
        },
        error = function(e) {
          TRUE
        }
      )) {
        if (force) {
          return(list(
            data = dataTmp[-c(private$pivotIdx, length(private$config$defaultValue))],
            colHeaders = private$staticColHeaders
          ))
        }
        return(list(data = dataTmp, colHeaders = attr(private$config$defaultValue, "aliases")))
      }

      return(list(data = dataTmp, colHeaders = c(
        private$staticColHeaders,
        names(dataTmp)[seq(
          length(private$staticColHeaders) + 1L,
          length(dataTmp)
        )]
      )))
    }
  )
)

ScalarWidget <- R6::R6Class("ScalarWidget",
  inherit = InputWidget,
  public = list(
    reset = function() {
      private$isInitialized <- FALSE
      return(super$reset())
    }
  ),
  private = list(
    isInitialized = FALSE
  )
)

HandsonTableWidget <- R6::R6Class("HandsonTableWidget",
  inherit = MultiDimWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj) {
      colsReadonly <- vapply(symConfig$headers, function(headerConfig) {
        if (identical(headerConfig$readonly, TRUE)) {
          return(headerConfig$alias)
        }
        return(NA_character_)
      }, character(1L), USE.NAMES = FALSE)
      colsReadonly <- colsReadonly[!is.na(colsReadonly)]

      private$staticTableConfig <- list(
        height = config[["handsontable"]]$height,
        width = config[["handsontable"]]$width,
        search = config[["handsontable"]]$search,
        contextMenu = config[["handsontable"]]$contextMenu$enabled,
        highlightCol = config[["handsontable"]]$highlightCol,
        highlightRow = config[["handsontable"]]$highlightRow,
        rowHeaderWidth = config[["handsontable"]]$rowHeaderWidth,
        stretchH = config[["handsontable"]]$stretchH,
        overflow = config[["handsontable"]]$overflow,
        colWidths = if (length(symConfig$colWidths)) symConfig$colWidths else config[["handsontable"]]$colWidths,
        colsReadonly = colsReadonly,
        renderHeatmap = identical(symConfig$heatmap, TRUE),
        isReadonly = isTRUE(symConfig$readonly) || length(colsReadonly) > 0L,
        fixedColumnsLeft = symConfig$fixedColumnsLeft,
        dropdownColumnConfigs = symConfig$dropdownCols,
        columnSorting = config[["handsontable"]]$columnSorting,
        allowRowEdit = config[["handsontable"]]$contextMenu$allowRowEdit,
        allowReadOnly = config[["handsontable"]]$contextMenu$allowReadOnly,
        manualColumnMove = config[["handsontable"]]$manualColumnMove,
        manualColumnResize = config[["handsontable"]]$manualColumnResize,
        colFormatOptions = symConfig$colFormat,
        validateColOptions = symConfig$validateCols
      )

      output[[paste0("in_", id)]] <- renderRHandsontable({
        private$updateWidget()

        if (private$needsPivot) {
          dataTmp <- private$getPivotedData(force = TRUE)
          colHeaders <- dataTmp$colHeaders
          dataTmp <- dataTmp$data
        } else {
          dataTmp <- isolate(private$data())
          colHeaders <- private$staticColHeaders
        }

        ht <- rhandsontable(dataTmp,
          height = private$staticTableConfig$height,
          rowHeaders = if (isTRUE(private$config$hideIndexCol)) NULL else rownames(dataTmp),
          colHeaders = colHeaders,
          useTypes = TRUE,
          width = private$staticTableConfig$width,
          search = private$staticTableConfig$search,
          readOnly = if (isTRUE(private$config$readonly)) TRUE else NULL,
          selectCallback = TRUE,
          digits = NA,
          naAsNull = private$needsPivot || isTRUE(attr(private$config$defaultValue, "isTable"))
        )
        ht <- hot_table(ht,
          contextMenu = private$staticTableConfig$contextMenu,
          highlightCol = private$staticTableConfig$highlightCol,
          highlightRow = private$staticTableConfig$highlightRow,
          rowHeaderWidth = private$staticTableConfig$rowHeaderWidth,
          stretchH = private$staticTableConfig$stretchH,
          overflow = private$staticTableConfig$overflow
        )
        if (isTRUE(private$staticTableConfig$contextMenu)) {
          if (private$needsPivot && !private$staticTableConfig$isReadonly &&
            !isTRUE(private$config$pivotColIsReadonly)) {
            ht <- hot_context_menu(ht,
              allowRowEdit = private$staticTableConfig$allowRowEdit,
              allowColEdit = FALSE,
              customOpts = private$getHotCustomColOptions()
            )
          } else {
            ht <- hot_context_menu(ht,
              allowRowEdit = if (private$staticTableConfig$isReadonly) FALSE else private$staticTableConfig$allowRowEdit,
              allowColEdit = FALSE,
              allowReadOnly = private$staticTableConfig$allowReadOnly
            )
          }
        }
        ht <- hot_cols(ht,
          columnSorting = if (private$needsPivot) FALSE else private$staticTableConfig$columnSorting,
          manualColumnMove = private$staticTableConfig$manualColumnMove,
          manualColumnResize = private$staticTableConfig$manualColumnResize,
          colWidths = private$staticTableConfig$colWidths,
          fixedColumnsLeft = private$staticTableConfig$fixedColumnsLeft
        )
        for (dropdownColumnConfig in private$staticTableConfig$dropdownColumnConfigs) {
          if (length(dropdownColumnConfig$static)) {
            dropdownColumnSource <- dropdownColumnConfig$static
          } else {
            dropdownColumnSource <- unique(sandboxInputDataObj$getData(dropdownColumnConfig$symbol)()[[dropdownColumnConfig$colId]])
          }
          ht <- hot_col(ht,
            dropdownColumnConfig$ddColId,
            type = dropdownColumnConfig$type,
            source = I(dropdownColumnSource),
            strict = TRUE,
            allowInvalid = FALSE
          )
        }
        for (colFormatOption in private$staticTableConfig$colFormatOptions) {
          ht$x$columns[[colFormatOption$colId]]$numericFormat <- list(pattern = colFormatOption$format)
          if (length(colFormatOption$language)) {
            ht$x$columns[[colFormatOption$colId]]$numericFormat$culture <- colFormatOption$language
          }
        }
        if (length(private$staticTableConfig$colsReadonly)) {
          ht <- hot_col(ht,
            private$staticTableConfig$colsReadonly,
            readOnly = TRUE
          )
        }
        for (validateColOption in private$staticTableConfig$validateColOptions) {
          ht <- hot_validate_numeric(ht, validateColOption$colId,
            min = validateColOption$min,
            max = validateColOption$max,
            choices = validateColOption$choices,
            exclude = validateColOption$exclude,
            allowInvalid = validateColOption$allowInvalid
          )
        }
        if (private$staticTableConfig$renderHeatmap) {
          return(hot_heatmap(ht))
        }
        return(ht)
      })
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj))
    },
    setData = function(data) {
      if (private$dataNeedsUpdate(data)) {
        private$ignoreUpdate <- 1L
        isolate({
          private$data(data)
          updateNew <- private$updateWidget() + 1L
          private$updateWidget(updateNew)
        })
      }
      return(invisible(self))
    }
  ),
  private = list(
    staticTableConfig = list(),
    observeChanges = function() {
      return(observe({
        dataRaw <- private$input[[paste0("in_", private$id)]]
        if (is.null(dataRaw)) {
          return()
        }
        if (private$ignoreUpdate > 0L) {
          private$ignoreUpdate <- private$ignoreUpdate - 1L
        } else {
          dataTmp <- hotToR(
            dataRaw,
            private$config
          )
          if (private$needsPivot) {
            dataTmp <- select(pivot_longer(
              suppressWarnings(
                mutate(
                  dataTmp,
                  across(all_of(seq(
                    length(private$config$headers) - 1L,
                    length(dataTmp)
                  )), as.numeric)
                )
              ),
              cols = seq(
                length(private$config$headers) - 1L,
                length(dataTmp)
              ),
              names_to = private$config$pivotCols[[1]],
              values_to = names(private$config$headers)[length(private$config$headers)],
              values_drop_na = TRUE
            ), !!!names(private$config$headers))
          }
          isolate({
            private$data(dataTmp)
            private$rv$inputDataDirty <- TRUE
          })
        }
      }))
    },
    dataNeedsUpdate = function(data) {
      return(!identical(digest::digest(data, algo = "sha1"), isolate(private$data())))
    },
    getHotCustomColOptions = function() {
      setNames(
        list(
          list(
            name = lang$renderers$handsontable$newCol$nameLeft,
            callback = JS(paste0(
              "function(key, normalizedSelection){
            Miro.modal(", toJSString(lang$renderers$handsontable$newCol$prompt), ",",
              toJSString(lang$general$modal$okButton),
              ",", toJSString(lang$general$modal$cancelButton),
              ",'',function(newHdr, hot, key, normalizedSelection){
                let currentHeaders = hot.getColHeader();
                const newParams = hot.params;
                if ( newHdr == null || newHdr === '') {
                return false;
                }
                let i = 1;
                while ( currentHeaders.find(function (el) {
                return el === newHdr;
                })) {
                newHdr += i;
                i++;
                }
                const isSelectedByCorner = hot.selection.isSelectedByCorner();
                let columnLeft = 0;

                if (!isSelectedByCorner) {
                const latestSelection = normalizedSelection[Math.max(normalizedSelection.length - 1, 0)];

                columnLeft = latestSelection.start.col;
                }
                newParams.columns.splice(columnLeft,0,{type:'numeric',numericFormat:{pattern:'0.00'}});
                newParams.colHeaders.splice(columnLeft,0,newHdr);
                for (let row = 0; row < newParams.data.length; row++) {
                newParams.data[row].splice(columnLeft, 0, null);
                }
                hot.updateSettings(newParams);

                if (isSelectedByCorner) {
                hot.selectAll();
                }
            }, this, key, normalizedSelection);
            }"
            )),
            disabled = JS(paste0("function(){
                                            return this.getSelectedLast()[1]<=", private$config$noDomains - 1L, ";}"))
          ),
          list(
            name = lang$renderers$handsontable$newCol$nameRight,
            callback = JS(paste0(
              "function(key, normalizedSelection){
            Miro.modal(", toJSString(lang$renderers$handsontable$newCol$prompt), ",",
              toJSString(lang$general$modal$okButton),
              ",", toJSString(lang$general$modal$cancelButton),
              ",'',function(newHdr, hot, key, normalizedSelection){
                let currentHeaders = hot.getColHeader();
                const newParams = hot.params;
                if ( newHdr == null || newHdr === '' ) {
                return false;
                }
                let i = 1;
                while ( currentHeaders.find(function (el) {
                return el === newHdr;
                })) {
                newHdr += i;
                i++;
                }
                const isSelectedByCorner = hot.selection.isSelectedByCorner();
                let columnRight = 0;

                if (isSelectedByCorner) {
                columnRight = hot.countCols();

                } else {
                const latestSelection = normalizedSelection[Math.max(normalizedSelection.length - 1, 0)];
                const selectedColumn = latestSelection?.end?.col;

                // If there is no selection we have clicked on the corner and there is no data.
                columnRight = typeof selectedColumn !== 'undefined' ? selectedColumn + 1 : 0;
                }
                newParams.columns.splice(columnRight,0,{type:'numeric',numericFormat:{pattern:'0.00'}});
                newParams.colHeaders.splice(columnRight,0,newHdr);
                for (let row = 0; row < newParams.data.length; row++) {
                newParams.data[row].splice(columnRight, 0, null);
                }
                hot.updateSettings(newParams);

                if (isSelectedByCorner) {
                hot.selectAll();
                }
            }, this, key, normalizedSelection);
            }"
            )),
            disabled = JS(paste0("function(){
                                            return this.getSelectedLast()[1]<=", private$config$noDomains - 2L, ";}"))
          ),
          list(
            name = lang$renderers$handsontable$renameCol$name,
            callback = JS(paste0(
              "function(){
            const ind = this.getSelectedLast()[1];
            let currentHeaders = this.getColHeader();
            Miro.modal(", toJSString(lang$renderers$handsontable$renameCol$prompt), ",",
              toJSString(lang$general$modal$okButton),
              ",", toJSString(lang$general$modal$cancelButton),
              ",currentHeaders[ind],function(newHdr, hot, ind){
            let currentHeaders = hot.getColHeader();
            if ( newHdr == null || newHdr === '' ) {
            return false;
            }
            if ( newHdr === currentHeaders[ind] ) {
            return true;
            }
            let i = 1;
            while ( currentHeaders.find(function (el) {
            return el === newHdr;
            })) {
            newHdr += i;
            i++;
            }
            currentHeaders.splice(ind, 1, newHdr);
            setTimeout(function () {
                hot.updateSettings({
                colHeaders: currentHeaders
                });
                hot.params.colHeaders = hot.getColHeader();
                Shiny.onInputChange(hot.rootElement.id, {
                data: hot.getData(),
                changes: { event: 'afterCreateCol', ind: ind, ct: 1 },
                params: hot.params
                });
            }, 90);
            }, this, ind);
            }"
            )),
            disabled = JS(paste0("function(){
            const selection = this.getSelected();
            if (selection && selection.length > 1) {
            return true;
            }
            return this.getSelectedLast()[1]<=", private$config$noDomains - 1L, ";}"))
          ),
          list(
            name = JS(paste0("function(){
            const selection = this.getSelected();
            let pluralForm = 0;

            if (selection) {
                if (selection.length > 1) {
                pluralForm = 1;
                } else {
                const [, fromColumn, , toColumn] = selection[0];

                if (fromColumn - toColumn !== 0) {
                    pluralForm = 1;
                }
                }
            }
            if (pluralForm) {
                return ", toJSString(lang$renderers$handsontable$removeCol$namePlural), ";
            }
            return ", toJSString(lang$renderers$handsontable$removeCol$name), ";
            }")),
            callback = JS(paste0(
              "function(){
            let selections = this.getSelected();
            if (!Array.isArray(selections) || selections.length === 0) {
                return;
            }
            const newParams = this.params;
            const colsToRemove = selections.map((selection) => {
                if (selection[3] < selection[1]) {
                return newParams.colHeaders.slice(selection[3], selection[1] + 1);
                }
                return newParams.colHeaders.slice(selection[1], selection[3] + 1);
            }).flat().filter((current, index, self) => {
                return self.indexOf(current) === index;
            });
            Miro.modal(", toJSString(lang$renderers$handsontable$removeCol$prompt1),
              "+' '+colsToRemove.join(', ')+",
              toJSString(lang$renderers$handsontable$removeCol$prompt2), ",",
              toJSString(lang$general$modal$okButton),
              ",", toJSString(lang$general$modal$cancelButton),
              ",undefined,function(hot, selections, newParams){
            // get [startCol, endCol] pairs and sort by startCol
            selections = selections.map((selection) => {
                if (selection[3] < selection[1]) {
                return [selection[3], selection[1]];
                }
                return [selection[1], selection[3]];
            }).sort((a, b) => {
                return a[0] - b[0]
            });
            let offset = 0;
            let rngStart = 0;
            let rngDistance = 0;
            let rangeSet = false;
            // splice data according to selected intervals
            selections.forEach(selection => {
                if ( selection[0] > rngStart + rngDistance ) {
                if ( rangeSet === true ) {
                    for (let row = 0; row < newParams.data.length; row++) {
                    newParams.data[row].splice(rngStart - offset, rngDistance + 1);
                    }
                    newParams.columns.splice(rngStart - offset, rngDistance + 1);
                    newParams.colHeaders.splice(rngStart - offset, rngDistance + 1);
                    offset += rngDistance + 1;
                }
                rangeSet = true;
                rngStart = selection[0];
                rngDistance = selection[1] - selection[0];
                } else if ( selection[1] > rngStart + rngDistance ) {
                rngDistance = selection[1] - rngStart;
                }
            });
            for (let row = 0; row < newParams.data.length; row++) {
                newParams.data[row].splice(rngStart - offset, rngDistance + 1);
            }
            newParams.columns.splice(rngStart - offset, rngDistance + 1);
            newParams.colHeaders.splice(rngStart - offset, rngDistance + 1);
            hot.updateSettings(newParams);
            }, this, selections, newParams);
            }"
            )),
            disabled = JS(paste0("function(){
            return this.getSelectedLast()[1]<=", private$config$noDomains - 1L, ";}"))
          )
        ),
        c("column_left", "column_right", "rename_column", "remove_column")
      )
    }
  )
)

SliderWidget <- R6::R6Class("SliderWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj) {
      symConfig$defaultValue <- symConfig$slider$default
      if (!identical(private$config$slider$hasDependency, TRUE)) {
        private$isInitialized <- TRUE
      }
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj))
    },
    setData = function(data) {
      if (private$dataNeedsUpdate(data)) {
        isolate({
          private$data(as.numeric(data))
        })
        updateSliderInput(private$session,
          paste0("slider_", private$id),
          value = data
        )
      }
      return(invisible(self))
    }
  ),
  private = list(
    observeChanges = function() {
      obsList <- list(observe({
        dataRaw <- private$input[[paste0("slider_", private$id)]]
        if (private$ignoreUpdate > 0L) {
          private$ignoreUpdate <- private$ignoreUpdate - 1L
        } else {
          isolate({
            private$data(dataRaw)
            private$rv$inputDataDirty <- TRUE
          })
        }
      }))
      if (identical(private$config$slider$hasDependency, TRUE)) {
        obsList[[2L]] <- observe({
          newValues <- lapply(
            list("value" = "def", "min" = "min", "max" = "max", "step" = "step"),
            function(configElName) {
              configEl <- private$config$sliderConfig[[configElName]]
              if (is.numeric(configEl)) {
                if (!private$isInitialized) {
                  return(configEl)
                }
                return(NULL)
              }
              dependentData <- unique(private$sanboxData$getData(names(configEl)[1L])()[[configEl[[1L]]]])
              if (identical(configEl[["$operator"]], "count")) {
                return(length(dependentData))
              }
              return(match.fun(configEl[["$operator"]])(as.numeric(dependentData)))
            }
          )
          if ((is.null(newValues$min) || is.finite(newValues$min)) &&
            (is.null(newValues$max) || is.finite(newValues$max))) {
            if (!private$isInitialized) {
              private$isInitialized <- TRUE
              newValues$value <- isolate(private$data())
              if (length(newValues$value) &&
                newValues$value >= newValues$min &&
                newValues$value <= newValues$max) {
                private$ignoreUpdate <- 1L
              } else {
                flog.info(
                  "Slider value: %s not in dependent range. Will reset value.",
                  newValues$value
                )
              }
              showEl(private$session, paste0("#slider_", private$id))
              hideEl(private$session, paste0("#no_data_dep_", private$id))
            }
            updateSliderInput(private$session,
              paste0("slider_", private$id),
              value = newValues$value,
              min = newValues$min,
              max = newValues$max,
              step = newValues$step,
            )
          }
        })
      }
      return(obsList)
    },
    dataNeedsUpdate = function(data) {
      return(!identical(as.numeric(data), as.numeric(isolate(private$data()))))
    }
  )
)

DropdownWidget <- R6::R6Class("DropdownWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj) {
      symConfig$defaultValue <- symConfig$dropdown$default
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj))
    },
    setData = function(data) {
      if (private$dataNeedsUpdate(data)) {
        isolate({
          private$data(as.character(data))
        })
        private$ignoreUpdate <- 1L
        updateSelectInput(private$session,
          paste0("dropdown_", private$id),
          selected = data
        )
      }
      return(invisible(self))
    }
  ),
  private = list(
    observeChanges = function() {
      obsList <- list(observe({
        dataRaw <- private$input[[paste0("dropdown_", private$id)]]
        if (private$ignoreUpdate > 0L) {
          private$ignoreUpdate <- private$ignoreUpdate - 1L
        } else {
          isolate({
            private$data(dataRaw)
            private$rv$inputDataDirty <- TRUE
          })
        }
      }))
      if (!is.null(private$config$dropdown$dependencyConfig$fw)) {
        obsList[[2L]] <- observe({
          newChoices <- c(
            private$config$dropdown$dependencyConfig$staticChoices,
            unlist(lapply(names(private$config$dropdown$dependencyConfig$fw), function(symName) {
              colName <- private$config$dropdown$dependencyConfig$fw[[symName]][[1]]
              return(unique(private$sanboxData$getData(symName)()[[colName]]))
            }))
          )
          newAliases <- c(
            private$config$dropdown$dependencyConfig$staticAliases,
            unlist(lapply(names(private$config$dropdown$dependencyConfig$aliases), function(symName) {
              colName <- private$config$dropdown$dependencyConfig$aliases[[symName]][[1]]
              return(unique(private$sanboxData$getData(symName)()[[colName]]))
            }))
          )
          if (length(newAliases)) {
            newChoices <- setNames(newChoices, newAliases)
          }
          if (!private$isInitialized) {
            private$isInitialized <- TRUE
            currentValue <- isolate(private$data())
            showEl(private$session, paste0("#dropdown_", private$id))
            hideEl(private$session, paste0("#no_data_dep_", private$id))
          }
          if (length(currentValue) && !currentValue %in% newChoices) {
            flog.info(
              "Dropdown value: %s not part of dependent choices. Will reset value.",
              currentValue
            )
            private$ignoreUpdate <- 0L
          }
          updateSelectInput(private$session,
            paste0("dropdown_", private$id),
            choices = newChoices
          )
        })
      }
      return(obsList)
    },
    dataNeedsUpdate = function(data) {
      return(!identical(as.character(data), as.character(isolate(private$data()))))
    }
  )
)

CheckboxWidget <- R6::R6Class("CheckboxWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj) {
      symConfig$defaultValue <- symConfig$checkbox$value
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj))
    },
    setData = function(data) {
      if (private$dataNeedsUpdate(data)) {
        isolate({
          private$data(as.integer(data))
        })
        private$ignoreUpdate <- 1L
        updateCheckboxInput(private$session,
          paste0("cb_", private$id),
          value = as.logical(data)
        )
      }
      return(invisible(self))
    }
  ),
  private = list(
    observeChanges = function() {
      return(observe({
        dataRaw <- private$input[[paste0("cb_", private$id)]]
        if (private$ignoreUpdate > 0L) {
          private$ignoreUpdate <- private$ignoreUpdate - 1L
        } else {
          isolate({
            private$data(as.integer(dataRaw))
            private$rv$inputDataDirty <- TRUE
          })
        }
      }))
    },
    dataNeedsUpdate = function(data) {
      return(!identical(as.logical(data), as.logical(isolate(private$data()))))
    }
  )
)

SandboxInputData <- R6::R6Class("SandboxInputData",
  public = list(
    initialize = function(inputConfig, isMobileDevice, input, output, session, rv) {
      private$inputConfig <- inputConfig
      private$rv <- rv
      typeWidgetMapping <- list(
        hot = HandsonTableWidget,
        # dt = DataTableWidget,
        slider = SliderWidget,
        # textinput = TextInputWidget,
        # numericinput = NumericInputWidget,
        dropdown = DropdownWidget,
        # date = DateWidget,
        # daterange = DateRangeWidget,
        checkbox = CheckboxWidget
        # custom = CustomWidget
      )
      private$inputWidgets <- lapply(seq_along(inputConfig), function(id) {
        symName <- names(inputConfig)[[id]]
        config <- inputConfig[[id]]
        widget <- typeWidgetMapping[[config$type]]
        if (!is.null(widget)) {
          return(widget$new(id, config, input, output, session, rv, self))
        }
        stop_custom("error_not_implemented", sprintf("Widget type: '%s' not implemented for symbol: '%s'", config$type, symName))
      })
      names(private$inputWidgets) <- names(inputConfig)
      return(invisible(self))
    },
    reset = function() {
      for (inputWidget in private$inputWidgets) {
        inputWidget$reset()
      }
      return(invisible(self))
    },
    getData = function(symName) {
      if (length(private$inputConfig[[symName]]$definedByExternalSymbol)) {
        return(private$inputWidgets[[private$inputConfig[[symName]]$definedByExternalSymbol]]$getData())
      }
      return(private$inputWidgets[[symName]]$getData())
    },
    setData = function(symName, data) {
      if (length(private$inputConfig[[symName]]$definedByExternalSymbol)) {
        return(private$inputWidgets[[private$inputConfig[[symName]]$definedByExternalSymbol]]$setData(data))
      }
      private$inputWidgets[[symName]]$setData(data)
      return(invisible(self))
    }
  ),
  private = list(
    rv = NULL,
    inputConfig = NULL,
    inputWidgets = NULL
  )
)
