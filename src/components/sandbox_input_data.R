InputWidget <- R6::R6Class(
  "InputWidget",
  public = list(
    initialize = function(id, config, input, output, session, rv, sandboxInputDataObj,
                          symName = NULL, ...) {
      private$id <- id
      private$config <- config
      private$input <- input
      private$output <- output
      private$session <- session
      private$rv <- rv
      private$sandboxData <- sandboxInputDataObj
      private$symName <- symName
      private$data <- reactiveVal(config$defaultValue)
      private$changeObs <- private$observeChanges()
      return(invisible(self))
    },
    hasData = function() {
      stop_custom("error_not_implemented", "hasData method not implemented")
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
      private$isInitialized <- FALSE
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
    sandboxData = NULL,
    symName = NULL,
    data = NULL,
    changeObs = NULL,
    ignoreUpdate = 1L,
    isInitialized = FALSE,
    observeChanges = function() {
      stop_custom("error_not_implemented", "observeChanges method not implemented")
    }
  )
)

MultiDimWidget <- R6::R6Class("MultiDimWidget",
  inherit = InputWidget,
  public = list(
    initialize = function(id, config, input, output, session, rv, sandboxInputDataObj, symName = NULL, ...) {
      private$updateWidget <- reactiveVal(0L)
      config$defaultValue <- config$template
      config$defaultHasData <- length(config$defaultValue) &&
        nrow(config$defaultValue) > 0L
      private$needsPivot <- length(config$pivotCols) > 0L
      config$noDomains <- sum(vapply(config$headers, function(header) {
        identical(header$type, "string")
      }, logical(1L), USE.NAMES = FALSE))
      if (private$needsPivot) {
        private$pivotIdx <- match(config$pivotCols[[1]], names(config$headers))[[1L]]
        private$staticColHeaders <- attr(config$defaultValue, "aliases")[-c(private$pivotIdx, length(config$defaultValue))]
        config$noDomains <- config$noDomains - 1L
      } else {
        private$staticColHeaders <- attr(config$defaultValue, "aliases")
      }
      return(super$initialize(id, config, input, output, session, rv, sandboxInputDataObj, symName))
    },
    hasData = function() {
      if (private$config$defaultHasData) {
        return(private$dataNeedsUpdate(private$config$defaultValue))
      }
      noRows <- isolate(nrow(private$data()))
      return(length(noRows) && noRows > 0L)
    }
  ),
  private = list(
    updateWidget = NULL,
    needsPivot = FALSE,
    pivotIdx = NULL,
    staticColHeaders = NULL,
    getPivotedData = function(force = FALSE, dataToPivot = NULL) {
      if (is.null(dataToPivot)) {
        dataTmp <- isolate(private$data())
      } else {
        dataTmp <- dataToPivot
      }
      hasDuplicates <- FALSE
      if (tryCatch(
        {
          dataTmp <- pivot_wider(dataTmp,
            names_from = !!private$pivotIdx,
            values_from = !!length(dataTmp),
            names_sort = isTRUE(private$config$sortPivotCols)
          )
          FALSE
        },
        warning = function(w) {
          if (grepl("list-cols", conditionMessage(w), fixed = TRUE)) {
            hasDuplicates <<- TRUE
          }
          dataTmp <<- pivot_wider(dataTmp,
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
            colHeaders = private$staticColHeaders, hasDuplicates = hasDuplicates
          ))
        }
        return(list(
          data = dataTmp, colHeaders = attr(private$config$defaultValue, "aliases"),
          hasDuplicates = hasDuplicates
        ))
      }

      return(list(data = dataTmp, colHeaders = c(
        private$staticColHeaders,
        names(dataTmp)[seq(
          length(private$staticColHeaders) + 1L,
          length(dataTmp)
        )]
      ), hasDuplicates = hasDuplicates))
    },
    normalizeData = function(data) {
      if (!private$needsPivot) {
        return(data)
      }
      if (length(data) < length(private$config$headers) - 1L) {
        return(private$config$defaultValue)
      }
      return(suppressWarnings(mutate(
        unnest(
          select(pivot_longer(
            data,
            cols = seq(
              length(private$config$headers) - 1L,
              length(data)
            ),
            names_to = private$config$pivotCols[[1]],
            values_transform = asNumericIfNotList,
            values_to = names(private$config$headers)[length(private$config$headers)],
            values_drop_na = TRUE
          ), !!!names(private$config$headers)),
          cols = all_of(!!!length(private$config$headers))
        ),
        across(
          length(private$config$headers), as.numeric
        )
      )))
    },
    dataNeedsUpdate = function(data) {
      return(!identical(
        digest::digest(data, algo = "sha1"),
        digest::digest(isolate(private$data()), algo = "sha1")
      ))
    }
  )
)

ScalarWidget <- R6::R6Class("ScalarWidget",
  inherit = InputWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj,
                          symName, conversionFn = as.character, ...) {
      private$conversionFn <- conversionFn
      return(super$initialize(
        id, symConfig, input, output, session, rv,
        sandboxInputDataObj, symName
      ))
    },
    hasData = function() {
      return(TRUE)
    },
    reset = function() {
      if (identical(private$config$hasDependency, TRUE)) {
        hideEl(
          private$session,
          paste0("#", private$config$htmlSelector)
        )
        showEl(private$session, paste0("#no_data_dep_", private$id))
      }
      return(super$reset())
    }
  ), private = list(
    conversionFn = NULL,
    observeChanges = function() {
      return(observe({
        dataRaw <- private$input[[private$config$htmlSelector]]
        if (private$ignoreUpdate > 0L) {
          private$ignoreUpdate <- private$ignoreUpdate - 1L
          if (!private$isInitialized) {
            private$isInitialized <- TRUE
            if (private$dataNeedsUpdate(dataRaw)) {
              isolate(private$data(dataRaw))
            }
          }
        } else {
          isolate({
            private$data(private$conversionFn(dataRaw))
            flog.debug("Modification of input widget (symbol: %s).", private$symName)
            private$rv$inputDataDirty <- TRUE
          })
        }
      }))
    },
    dataNeedsUpdate = function(data) {
      return(!identical(
        private$conversionFn(data),
        private$conversionFn(isolate(private$data()))
      ))
    }
  )
)

HandsonTableWidget <- R6::R6Class("HandsonTableWidget",
  inherit = MultiDimWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      colsReadonly <- vapply(symConfig$headers, function(headerConfig) {
        if (identical(headerConfig$readonly, TRUE)) {
          return(headerConfig$alias)
        }
        return(NA_character_)
      }, character(1L), USE.NAMES = FALSE)
      colsReadonly <- colsReadonly[!is.na(colsReadonly)]

      if (is.null(symConfig$readonly)) {
        symConfig$readonly <- config[["handsontable"]]$readonly
      }

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

        hasDuplicates <- FALSE
        if (private$needsPivot) {
          dataTmp <- private$getPivotedData(force = TRUE)
          hasDuplicates <- identical(dataTmp$hasDuplicates, TRUE)
          if (hasDuplicates) {
            showEl(
              session,
              paste0("#in_", id, "_duplicate_error")
            )
          } else {
            hideEl(
              session,
              paste0("#in_", id, "_duplicate_error")
            )
          }
          colHeaders <- dataTmp$colHeaders
          dataTmp <- dataTmp$data
        } else {
          dataTmp <- isolate(private$data())
          colHeaders <- private$staticColHeaders
        }

        if (identical(nrow(dataTmp), 0L)) {
          dataTmp[1, ] <- NA
          dataTmp <- mutate(dataTmp, across(
            where(is.character),
            ~ replace_na(.x, replace = "")
          ))
        }

        enableContextMenu <- isTRUE(private$staticTableConfig$contextMenu) &&
          !isTRUE(private$config$readonly) && !hasDuplicates

        ht <- rhandsontable(dataTmp,
          height = private$staticTableConfig$height,
          rowHeaders = if (isTRUE(private$config$hideIndexCol)) NULL else rownames(dataTmp),
          colHeaders = colHeaders,
          useTypes = TRUE,
          width = private$staticTableConfig$width,
          search = private$staticTableConfig$search,
          readOnly = if (isTRUE(private$config$readonly) || hasDuplicates) TRUE else NULL,
          selectCallback = TRUE,
          digits = NA,
          naAsNull = private$needsPivot || isTRUE(attr(private$config$defaultValue, "isTable"))
        )
        ht <- hot_table(ht,
          contextMenu = enableContextMenu,
          highlightCol = private$staticTableConfig$highlightCol,
          highlightRow = private$staticTableConfig$highlightRow,
          rowHeaderWidth = private$staticTableConfig$rowHeaderWidth,
          stretchH = private$staticTableConfig$stretchH,
          overflow = private$staticTableConfig$overflow
        )
        if (enableContextMenu) {
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
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName))
    },
    setData = function(data) {
      if (private$dataNeedsUpdate(data)) {
        private$ignoreUpdate <- 1L
        if (private$needsPivot) {
          # we need to do this to not mess up the row order when
          # removing duplicates since pivoting and unpivoting causes
          # same records to be next to each other
          dataTmp <- private$getPivotedData(
            force = TRUE,
            dataToPivot = data
          )$data
          isolate({
            private$data(private$normalizeData(dataTmp))
            updateNew <- private$updateWidget() + 1L
            private$updateWidget(updateNew)
          })
        } else {
          isolate({
            private$data(data)
            updateNew <- private$updateWidget() + 1L
            private$updateWidget(updateNew)
          })
        }
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
            dataTmp <- private$normalizeData(dataTmp)
          }
          isolate({
            private$data(dataTmp)
            flog.debug("Modification of hot (symbol: %s).", private$symName)
            private$rv$inputDataDirty <- TRUE
          })
        }
      }))
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

DataTableWidget <- R6::R6Class("DataTableWidget",
  inherit = MultiDimWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName = NULL,
                          htmlSelectorPefix = "in_", ...) {
      private$staticTableConfig <- list(
        dtConfig = modifyList(
          config[["datatable"]],
          list(
            editable = !identical(
              symConfig$readonly,
              TRUE
            ),
            options = list(scrollX = TRUE)
          )
        ),
        htmlSelector = paste0(htmlSelectorPefix, id),
        roundPrecision = config[["roundingDecimals"]]
      )
      private$dtProxy <- dataTableProxy(private$staticTableConfig$htmlSelector)
      output[[private$staticTableConfig$htmlSelector]] <- renderDT({
        private$updateWidget()

        if (private$needsPivot) {
          dataTmp <- private$getPivotedData()
          colHeaders <- dataTmp$colHeaders
          dataTmp <- dataTmp$data
        } else {
          dataTmp <- isolate(private$data())
          colHeaders <- private$staticColHeaders
        }

        dtOptions <- private$staticTableConfig$dtConfig
        dtOptions$colnames <- colHeaders

        return(renderDTable(dataTmp,
          dtOptions,
          roundPrecision = private$staticTableConfig$roundPrecision,
          render = FALSE
        ))
      })
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName = symName))
    },
    setData = function(data) {
      if (private$dataNeedsUpdate(data)) {
        private$ignoreUpdate <- 1L
        if (private$needsPivot) {
          # we need to do this to not mess up the row order when
          # removing duplicates since pivoting and unpivoting causes
          # same records to be next to each other
          private$dataPivoted <- private$getPivotedData(
            force = TRUE,
            dataToPivot = data
          )$data
          isolate({
            private$data(private$normalizeData(private$dataPivoted))
            updateNew <- private$updateWidget() + 1L
            private$updateWidget(updateNew)
          })
        } else {
          isolate({
            private$data(data)
            updateNew <- private$updateWidget() + 1L
            private$updateWidget(updateNew)
          })
        }
      }
      return(invisible(self))
    }
  ),
  private = list(
    staticTableConfig = NULL,
    dtProxy = NULL,
    dataPivoted = NULL,
    observeChanges = function() {
      return(list(
        observeEvent(private$input[[paste0(
          private$staticTableConfig$htmlSelector,
          "_add_row"
        )]], {
          if (!length(private$data())) {
            flog.warn(
              "Add row button (symbol: %s) was clicked but data has no length.",
              private$symName
            )
            return()
          }
          removeUI("body>.selectize-dropdown", multiple = TRUE, immediate = TRUE)

          newRowId <- suppressWarnings(
            as.integer(private$input[[paste0(
              private$staticTableConfig$htmlSelector,
              "_rows_selected"
            )]])
          )
          if (any(is.na(newRowId))) {
            return(flog.error(
              "Invalid data for 'rows_selected' (symbol: %s).",
              private$symName
            ))
          }
          if (length(newRowId) == 1L) {
            newRowId <- c(newRowId - 1L, newRowId)
          } else if (length(newRowId) > 1L) {
            newRowId <- c(min(newRowId) - 1L, max(newRowId))
          }
          colNames <- private$staticColHeaders
          if (private$needsPivot) {
            colNames <- c(colNames, names(private$dataPivoted)[seq(
              private$config$noDomains + 1L,
              length(private$dataPivoted)
            )])
            noRows <- nrow(private$dataPivoted)
          } else {
            noRows <- nrow(private$data())
          }
          showModal(
            modalDialog(
              title = lang$renderers$miroPivot$dialogAddRow$title,
              tags$div(id = "newRowError", class = "gmsalert gmsalert-error"),
              tags$div(
                class = "table-responsive", style = "margin-top:30px",
                tags$table(
                  class = "table",
                  tags$tr(
                    lapply(colNames, tags$th)
                  ),
                  tags$tr(lapply(seq_along(colNames), function(j) {
                    tags$td(
                      class = "table-add-row",
                      if (j <= private$config$noDomains) {
                        if (private$needsPivot) {
                          uelChoices <- unique(private$dataPivoted[[j]])
                        } else {
                          uelChoices <- unique(private$data()[[j]])
                        }
                        serverSelectInput(private$session,
                          paste0("newRow_", j), NULL,
                          uelChoices,
                          multiple = TRUE,
                          width = "100%",
                          options = list(
                            create = TRUE, maxItems = 1L,
                            dropdownParent = "body"
                          )
                        )
                      } else {
                        textInput(paste0("newRow_", j), NULL, NA, width = "100%")
                      }
                    )
                  }))
                )
              ),
              selectInput("newRowId", NULL,
                choices = if (length(newRowId) == 0L) {
                  setNames(
                    c(0L, noRows),
                    lang$renderers$datatable$addRowPosNoneSelected
                  )
                } else {
                  setNames(newRowId, lang$renderers$datatable$addRowPos)
                },
                selected = newRowId[2]
              ),
              footer = tagList(
                tags$div(
                  class = "modal-footer-mobile",
                  modalButton(lang$renderers$miroPivot$dialogAddRow$btCancel),
                  actionButton(
                    paste0(
                      private$staticTableConfig$htmlSelector,
                      "_add_row_confirm"
                    ),
                    label = lang$renderers$miroPivot$btAddRow,
                    class = "bt-highlight-1 bt-gms-confirm"
                  )
                )
              ),
              fade = TRUE, easyClose = FALSE, size = "l"
            )
          )
        }),
        observeEvent(private$input[[paste0(
          private$staticTableConfig$htmlSelector,
          "_add_row_confirm"
        )]], {
          flog.trace("Add row button for table: %s clicked.", private$symName)
          if (!length(private$data())) {
            flog.warn(
              "Add rows confirm button (symbol: %s) was clicked but data has no length.",
              private$symName
            )
            return()
          }
          newKeys <- vapply(seq_len(private$config$noDomains), function(i) {
            editedKey <- tryCatch(trimws(private$input[[paste0("newRow_", i)]]),
              error = function(e) {
                NA_character_
              }
            )
            if (isValidUEL(editedKey)) {
              return(editedKey)
            }
            return(NA_character_)
          }, character(1L), USE.NAMES = FALSE)
          if (any(is.na(newKeys))) {
            return(showHideEl(
              private$session, "#newRowError", 5000L,
              lang$renderers$miroPivot$dialogAddRow$invalidKeysError
            ))
          }
          invalidValue <- FALSE
          if (private$needsPivot) {
            noCols <- length(private$dataPivoted)
          } else {
            noCols <- length(private$data())
          }
          newValues <- vapply(seq(
            private$config$noDomains + 1L,
            noCols
          ), function(i) {
            newVal <- trimws(private$input[[paste0("newRow_", i)]])
            if (identical(newVal, "")) {
              return(NA_real_)
            }
            newVal <- tryCatch(
              suppressWarnings(
                as.numeric(private$input[[paste0("newRow_", i)]])
              ),
              error = function(e) {
                NA_real_
              }
            )
            if (length(newVal) != 1L) {
              return(NA_real_)
            }
            if (is.na(newVal) && !invalidValue) {
              invalidValue <<- TRUE
            }
            return(newVal)
          }, numeric(1L), USE.NAMES = FALSE)

          if (invalidValue) {
            return(showHideEl(
              private$session, "#newRowError", 5000L,
              lang$renderers$miroPivot$dialogAddRow$invalidValuesError
            ))
          }

          newRowId <- suppressWarnings(as.integer(private$input$newRowId))
          if (length(newRowId) != 1L || is.na(newRowId)) {
            return(flog.error(
              "Invalid data for 'newRowId' (symbol: %s).",
              private$symName
            ))
          }
          if (private$needsPivot) {
            newDataTmp <- add_row(private$dataPivoted,
              !!!setNames(
                c(
                  as.list(newKeys),
                  as.list(newValues)
                ),
                names(private$dataPivoted)
              ),
              .after = newRowId
            )
          } else {
            newDataTmp <- add_row(private$data(),
              !!!setNames(
                c(
                  as.list(newKeys),
                  as.list(newValues)
                ),
                names(private$data())
              ),
              .after = newRowId
            )
          }
          replaceData(private$dtProxy,
            newDataTmp,
            resetPaging = FALSE,
            rownames = config$datatable$rownames
          )
          private$data(private$normalizeData(newDataTmp))
          if (private$needsPivot) {
            private$dataPivoted <- newDataTmp
          }
          private$rv$inputDataDirty <- TRUE
          removeModal()
          flog.trace("Added row (symbol: %s).", private$symName)
        }),
        observeEvent(private$input[[paste0(
          private$staticTableConfig$htmlSelector,
          "_remove_row"
        )]], {
          flog.trace(
            "Remove rows button (symbol: %s) was clicked.",
            private$symName
          )
          idsToRemove <- private$input[[paste0(
            private$staticTableConfig$htmlSelector,
            "_rows_selected"
          )]]
          if (!length(idsToRemove)) {
            flog.trace("No rows selected (symbol: %s).", private$symName)
            return()
          }
          if (!length(private$data())) {
            flog.warn(
              "Remove rows button (symbol: %s) was clicked but data has no length.",
              private$symName
            )
            return()
          }
          if (private$needsPivot) {
            newDataTmp <- private$dataPivoted[-idsToRemove, ]
          } else {
            newDataTmp <- private$data()[-idsToRemove, ]
          }
          replaceData(private$dtProxy,
            newDataTmp,
            resetPaging = FALSE,
            rownames = config$datatable$rownames
          )
          private$data(private$normalizeData(newDataTmp))
          if (private$needsPivot) {
            private$dataPivoted <- newDataTmp
          }
          private$rv$inputDataDirty <- TRUE
          flog.trace(
            "Removed %s row(s) (symbol: %s).",
            length(idsToRemove),
            private$symName
          )
        }),
        observeEvent(private$input[[paste0(
          private$staticTableConfig$htmlSelector,
          "_cell_edit"
        )]], {
          info <- private$input[[paste0(
            private$staticTableConfig$htmlSelector,
            "_cell_edit"
          )]]
          row <- info$row
          if (config$datatable$rownames) {
            col <- info$col
            if (col < 1) {
              return()
            }
          } else {
            col <- info$col + 1L
          }
          val <- info$value
          if (private$needsPivot) {
            newDataTmp <- private$dataPivoted
          } else {
            newDataTmp <- private$data()
          }
          newDataTmp[row, col] <- suppressWarnings(coerceValue(
            val,
            newDataTmp[[col]][row]
          ))
          replaceData(private$dtProxy,
            newDataTmp,
            resetPaging = FALSE,
            rownames = config$datatable$rownames
          )
          private$data(private$normalizeData(newDataTmp))
          if (private$needsPivot) {
            private$dataPivoted <- newDataTmp
          }
          private$rv$inputDataDirty <- TRUE
          flog.trace(
            "Modified value of row: %s, column: %s, value: %s (symbol: %s).",
            row, col, val, private$symName
          )
        })
      ))
    }
  )
)

CustomWidget <- R6::R6Class("CustomWidget",
  inherit = MultiDimWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      private$rendererEnv <- new.env(parent = emptyenv())
      widgetSymNames <- c(symName, symConfig$widgetSymbols)
      private$outputReactives <- c(
        list(reactiveVal(symConfig$template)),
        lapply(symConfig$widgetSymbols, function(symName) {
          return(reactiveVal(sandboxInputDataObj$getSymConfig(symName)$template))
        })
      )
      names(private$outputReactives) <- widgetSymNames
      private$inputReactiveData <- c(
        list(symConfig$template),
        lapply(symConfig$widgetSymbols, function(symName) {
          return(sandboxInputDataObj$getSymConfig(symName)$template)
        })
      )
      names(private$inputReactiveData) <- widgetSymNames
      private$updateInputReactives <- c(
        list(reactiveVal(0L)),
        lapply(symConfig$widgetSymbols, function(symName) {
          reactiveVal(0L)
        })
      )
      names(private$updateInputReactives) <- widgetSymNames
      private$inputReactives <- c(list(reactive({
        private$updateInputReactives[[private$symName]]()
        return(private$inputReactiveData[[private$symName]])
      })), lapply(symConfig$widgetSymbols, function(symName) {
        return(reactive({
          private$updateInputReactives[[symName]]()
          return(private$inputReactiveData[[symName]])
        }))
      }))
      names(private$inputReactives) <- widgetSymNames
      private$ignoreUpdate <- c(1L, vapply(symConfig$widgetSymbols, function(symName) {
        1L
      }, integer(1L), USE.NAMES = FALSE))
      names(private$ignoreUpdate) <- widgetSymNames
      if (!identical(symConfig$apiVersion, 2L)) {
        private$ignoreUpdate[] <- 0L
        private$reinitOutputObservers <- reactiveVal(0L)
      }
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName))
    },
    getData = function(symName = NULL) {
      if (is.null(symName)) {
        return(private$outputReactives[[private$symName]])
      }
      return(private$outputReactives[[symName]])
    },
    hasData = function(symName = NULL) {
      if (is.null(symName)) {
        defaultValue <- private$config$defaultValue
        if (length(defaultValue) &&
          nrow(defaultValue)) {
          return(private$dataNeedsUpdate(defaultValue, private$symName))
        }
        noRows <- isolate(nrow(private$outputReactives[[private$symName]]()))
      } else {
        defaultValue <- private$sandboxData$getSymConfig(symName)$template
        if (length(defaultValue) &&
          nrow(defaultValue)) {
          return(private$dataNeedsUpdate(defaultValue, symName))
        }
        noRows <- isolate(nrow(private$outputReactives[[symName]]()))
      }
      return(length(noRows) && noRows > 0L)
    },
    setData = function(data, symName = NULL) {
      if (is.null(symName)) {
        symName <- private$symName
      }
      if (identical(private$config$apiVersion, 2L) && private$dataNeedsUpdate(data, symName)) {
        private$ignoreUpdate[[symName]] <- 1L
      }
      private$inputReactiveData[[symName]] <- data
      isolate({
        currentVal <- private$updateInputReactives[[symName]]()
        private$updateInputReactives[[symName]](currentVal + 1L)
      })
      return(invisible(self))
    },
    reset = function() {
      self$setData(private$config$defaultValue)
      lapply(private$config$widgetSymbols, function(symName) {
        # TODO: support scalar dependencies ($template is NULL for those)
        self$setData(private$sandboxData$getSymConfig(symName)$template, symName)
      })
      return(invisible(self))
    }
  ),
  private = list(
    rendererEnv = NULL,
    returnVal = NULL,
    outputReactives = NULL,
    inputReactives = NULL,
    inputReactiveData = NULL,
    updateInputReactives = NULL,
    reinitOutputObservers = NULL,
    observeChanges = function() {
      return(c(
        list(
          observe({
            if (identical(private$config$apiVersion, 2L)) {
              reactiveDataVals <- c(
                private$inputReactives,
                lapply(private$config$additionalData, function(symName) {
                  return(private$sandboxData$getData(symName))
                })
              )
            } else {
              for (el in ls(envir = private$rendererEnv)) {
                if ("Observer" %in% class(private$rendererEnv[[el]])) {
                  private$rendererEnv[[el]]$destroy()
                }
              }
              reactiveDataVals <- c(
                lapply(private$inputReactives, function(inputReactive) {
                  return(inputReactive())
                }),
                lapply(private$config$additionalData, function(symName) {
                  return(private$sandboxData$getData(symName)())
                })
              )
            }
            names(reactiveDataVals) <- c(
              names(private$inputReactives), private$config$additionalData
            )
            if (length(reactiveDataVals) == 1L) {
              reactiveDataVals <- reactiveDataVals[[1L]]
            } else {
              reactiveDataVals <- reactiveDataVals
            }
            private$returnVal <- callModule(generateData,
              paste0("data-in_", private$id),
              type = private$config$rendererName,
              data = reactiveDataVals,
              customOptions = private$config$options,
              rendererEnv = private$rendererEnv,
              attachments = private$sandboxData$attachments,
              views = private$sandboxData$views
            )
            if (length(private$config$widgetSymbols)) {
              if (!identical(length(names(private$returnVal)), length(private$config$widgetSymbols) + 1L)) {
                stop(sprintf(
                  "Custom input widget for symbol: %s has invalid return value. It should return a named list of reactive expressions.",
                  private$symName
                ), call. = FALSE)
              }
            } else if (!identical(length(private$returnVal), 1L) || !"reactiveExpr" %in% class(private$returnVal)) {
              stop(sprintf(
                "Custom input widget for symbol: %s has invalid return value. It should return a single reactive expression.",
                private$symName
              ), call. = FALSE)
            }
            if (!identical(private$config$apiVersion, 2L)) {
              # need to re-initiate observers since we got new reactives
              private$ignoreUpdate[] <- private$ignoreUpdate[] + 1L
              isolate({
                currentVal <- private$reinitOutputObservers()
                private$reinitOutputObservers(currentVal + 1L)
              })
            }
          })
        ),
        if (length(private$config$widgetSymbols)) {
          lapply(names(private$outputReactives), function(symName) {
            observe({
              if (!identical(private$config$apiVersion, 2L)) {
                private$reinitOutputObservers()
              }
              dataTmp <- private$returnVal[[symName]]()
              if (is.null(dataTmp)) {
                return()
              }
              if (private$ignoreUpdate[[symName]] > 0L) {
                private$ignoreUpdate[[symName]] <- private$ignoreUpdate[[symName]] - 1L
              } else {
                flog.debug("Modification of custom widget (symbol: %s).", symName)
                private$rv$inputDataDirty <- TRUE
              }
              if (is.null(dataTmp)) {
                return()
              }
              isolate({
                private$outputReactives[[symName]](dataTmp)
              })
            })
          })
        } else {
          list(observe({
            if (!identical(private$config$apiVersion, 2L)) {
              private$reinitOutputObservers()
            }
            dataTmp <- private$returnVal()
            if (private$ignoreUpdate[[private$symName]] > 0L) {
              private$ignoreUpdate[[private$symName]] <- private$ignoreUpdate[[private$symName]] - 1L
            } else {
              flog.debug("Modification of custom widget (symbol: %s).", private$symName)
              private$rv$inputDataDirty <- TRUE
            }
            if (is.null(dataTmp)) {
              return()
            }
            isolate({
              private$outputReactives[[private$symName]](dataTmp)
            })
          }))
        }
      ))
    },
    dataNeedsUpdate = function(data, symName) {
      return(!identical(
        digest::digest(data, algo = "sha1"),
        digest::digest(isolate(private$outputReactives[[symName]]()), algo = "sha1")
      ))
    }
  )
)

SliderWidget <- R6::R6Class("SliderWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      symConfig$defaultValue <- symConfig$slider$default
      symConfig$htmlSelector <- paste0("slider_", id)
      if (identical(symConfig$slider$hasDependency, TRUE)) {
        symConfig$hasDependency <- TRUE
      }
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName,
        conversionFn = as.numeric
      ))
    },
    setData = function(data) {
      dataNew <- private$conversionFn(data)
      if (private$dataNeedsUpdate(dataNew)) {
        isolate({
          private$data(dataNew)
        })
        private$ignoreUpdate <- 1L
        updateSliderInput(private$session,
          private$config$htmlSelector,
          value = dataNew
        )
      }
      return(invisible(self))
    },
    getCurrentConfig = function() {
      if (identical(private$config$slider$hasDependency, TRUE)) {
        return(private$currentConfig)
      }
      return(list(
        min = private$config$slider$min,
        max = private$config$slider$max,
        step = private$config$slider$step,
      ))
    }
  ),
  private = list(
    currentConfig = NULL,
    observeChanges = function() {
      obsList <- list(observe({
        dataRaw <- private$input[[private$config$htmlSelector]]
        if (private$ignoreUpdate > 0L) {
          private$ignoreUpdate <- private$ignoreUpdate - 1L
        } else {
          isolate({
            private$data(dataRaw)
            flog.debug("Modification of slider (symbol: %s).", private$symName)
            private$rv$inputDataDirty <- TRUE
          })
        }
      }))
      if (identical(private$config$slider$hasDependency, TRUE)) {
        obsList[[2L]] <- observe({
          newValues <- lapply(
            list("value" = "def", "min" = "min", "max" = "max", "step" = "step"),
            function(configElName) {
              configElRaw <- private$config$sliderConfig[[configElName]]
              if (!identical(configElName, "def") || !is.null(names(configElRaw))) {
                configElRaw <- list(configElRaw)
              }
              return(vapply(configElRaw, function(configEl) {
                if (is.numeric(configEl)) {
                  if (!private$isInitialized) {
                    return(configEl)
                  }
                  return(NA_real_)
                }
                dependentData <- unique(private$sandboxData$getData(
                  names(configEl)[1L]
                )()[[configEl[[1L]]]])
                if (identical(configEl[["$operator"]], "count")) {
                  return(length(dependentData))
                }
                return(match.fun(configEl[["$operator"]])(as.numeric(dependentData)))
              }, numeric(1L)))
            }
          )
          if ((is.na(newValues$min) || is.finite(newValues$min)) &&
            (is.na(newValues$max) || is.finite(newValues$max))) {
            if (is.finite(newValues$min) &&
              is.finite(newValues$max) &&
              newValues$min > newValues$max) {
              return()
            }
            if (!private$isInitialized) {
              private$isInitialized <- TRUE
              newValues$value <- isolate(private$data())
              if (length(newValues$value) &&
                all(newValues$value >= newValues$min) &&
                all(newValues$value <= newValues$max)) {
                private$ignoreUpdate <- 1L
              } else {
                flog.info(
                  "Slider value: %s not in dependent range. Will reset value.",
                  newValues$value
                )
              }
              showEl(
                private$session,
                paste0("#", private$config$htmlSelector)
              )
              hideEl(private$session, paste0("#no_data_dep_", private$id))
            }
            private$currentConfig <- newValues
            updateSliderInput(private$session,
              private$config$htmlSelector,
              value = newValues$value,
              min = newValues$min,
              max = newValues$max,
              step = newValues$step,
            )
          }
        })
      }
      return(obsList)
    }
  )
)

DropdownWidget <- R6::R6Class("DropdownWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      symConfig$defaultValue <- symConfig$dropdown$selected
      symConfig$htmlSelector <- paste0("dropdown_", id)
      symConfig$hasDependency <- !is.null(symConfig$dropdown$dependencyConfig$fw)
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName))
    },
    setData = function(data) {
      if (identical(private$config$dropdown$multiple, TRUE)) {
        dataNew <- private$conversionFn(data[[1L]])
      } else {
        dataNew <- private$conversionFn(data)
      }
      if (private$dataNeedsUpdate(dataNew)) {
        isolate({
          private$data(dataNew)
        })
        private$ignoreUpdate <- 1L
        updateSelectInput(private$session,
          private$config$htmlSelector,
          selected = dataNew
        )
      }
      return(invisible(self))
    },
    hasData = function() {
      if (identical(private$config$dropdown$multiple, TRUE)) {
        return(!identical(isolate(private$data()), private$config$defaultValue))
      }
      return(TRUE)
    },
    getChoices = function(data) {
      if (private$config$hasDependency) {
        return(private$currentChoices)
      }
      choices <- private$config$dropdown$choices
      if (!is.null(private$config$dropdown$aliases)) {
        names(choices) <- private$config$dropdown$aliases
      }
      return(choices)
    }
  ),
  private = list(
    currentChoices = NULL,
    observeChanges = function() {
      obsList <- list(observe({
        dataRaw <- private$input[[private$config$htmlSelector]]
        if (private$ignoreUpdate > 0L) {
          private$ignoreUpdate <- private$ignoreUpdate - 1L
        } else {
          isolate({
            private$data(dataRaw)
            flog.debug("Modification of dropdown (symbol: %s).", private$symName)
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
              return(unique(private$sandboxData$getData(symName)()[[colName]]))
            }))
          )
          if (!length(newChoices)) {
            return()
          }
          newAliases <- c(
            private$config$dropdown$dependencyConfig$staticAliases,
            unlist(lapply(names(private$config$dropdown$dependencyConfig$aliases), function(symName) {
              colName <- private$config$dropdown$dependencyConfig$aliases[[symName]][[1]]
              return(unique(private$sandboxData$getData(symName)()[[colName]]))
            }))
          )
          if (length(newAliases)) {
            newChoices <- setNames(newChoices, newAliases)
          }
          currentValue <- NULL
          if (!private$isInitialized) {
            private$isInitialized <- TRUE
            currentValue <- isolate(private$data())
            showEl(private$session, paste0("#", private$config$htmlSelector))
            hideEl(private$session, paste0("#no_data_dep_", private$id))
            if (length(currentValue) && !all(currentValue %in% newChoices)) {
              flog.info(
                "Dropdown value(s): %s for symbol: %s not part of dependent choices. Will reset value.",
                paste(currentValue, collapse = ", "), private$symName
              )
              private$ignoreUpdate <- 0L
              currentValue <- newChoices[[1L]]
            }
          }
          if (identical(config$activateModules$hcube, TRUE)) {
            private$currentChoices <- newChoices
          }
          updateSelectInput(private$session,
            private$config$htmlSelector,
            choices = newChoices,
            selected = currentValue
          )
        })
      }
      return(obsList)
    }
  )
)

CheckboxWidget <- R6::R6Class("CheckboxWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      symConfig$defaultValue <- symConfig$checkbox$value
      symConfig$htmlSelector <- paste0("cb_", id)
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName,
        conversionFn = as.integer
      ))
    },
    setData = function(data) {
      dataNew <- private$conversionFn(data)
      if (private$dataNeedsUpdate(data)) {
        isolate({
          private$data(as.integer(data))
        })
        private$ignoreUpdate <- 1L
        updateCheckboxInput(private$session,
          private$config$htmlSelector,
          value = dataNew
        )
      }
      return(invisible(self))
    }
  )
)

NumericInputWidget <- R6::R6Class("NumericInputWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      symConfig$defaultValue <- symConfig$numericinput$value
      symConfig$htmlSelector <- paste0("numeric_", id)
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName,
        conversionFn = as.numeric
      ))
    },
    setData = function(data) {
      dataNew <- private$conversionFn(data)
      if (private$dataNeedsUpdate(dataNew)) {
        isolate({
          private$data(dataNew)
        })
        private$ignoreUpdate <- 1L
        updateNumericInput(private$session,
          private$config$htmlSelector,
          value = dataNew
        )
      }
      return(invisible(self))
    }
  )
)

TextInputWidget <- R6::R6Class("TextInputWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      symConfig$defaultValue <- symConfig$textinput$value
      symConfig$htmlSelector <- paste0("text_", id)
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName))
    },
    setData = function(data) {
      dataNew <- private$conversionFn(data)
      if (private$dataNeedsUpdate(dataNew)) {
        isolate({
          private$data(dataNew)
        })
        private$ignoreUpdate <- 1L
        updateTextInput(private$session,
          private$config$htmlSelector,
          value = dataNew
        )
      }
      return(invisible(self))
    }
  )
)

DateWidget <- R6::R6Class("DateWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      symConfig$defaultValue <- symConfig$date$value
      symConfig$htmlSelector <- paste0("date_", id)
      return(super$initialize(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName))
    },
    setData = function(data) {
      if (is.null(data)) {
        dataNew <- NULL
      } else {
        dataNew <- private$conversionFn(data)
      }
      if (private$dataNeedsUpdate(dataNew)) {
        isolate({
          private$data(dataNew)
        })
        private$ignoreUpdate <- 1L
        updateDateInput(private$session,
          private$config$htmlSelector,
          value = dataNew
        )
      }
      return(invisible(self))
    }
  )
)

DateRangeWidget <- R6::R6Class("DateRangeWidget",
  inherit = ScalarWidget,
  public = list(
    initialize = function(id, symConfig, input, output, session, rv, sandboxInputDataObj, symName, ...) {
      symConfig$htmlSelector <- paste0("daterange_", id)
      symConfig$defaultValue <- c(
        symConfig$daterange[["start"]],
        symConfig$daterange[["end"]]
      )
      return(super$initialize(
        id,
        symConfig,
        input,
        output,
        session,
        rv,
        sandboxInputDataObj,
        symName
      ))
    },
    setData = function(data) {
      if (is.null(data)) {
        dataNew <- NULL
      } else {
        dataNew <- private$conversionFn(data)
      }
      if (private$dataNeedsUpdate(dataNew)) {
        isolate({
          private$data(dataNew)
        })
        private$ignoreUpdate <- 1L
        updateDateRangeInput(private$session,
          private$config$htmlSelector,
          start = dataNew[1],
          end = dataNew[2]
        )
      }
      return(invisible(self))
    }
  )
)

SandboxInputData <- R6::R6Class("SandboxInputData",
  public = list(
    initialize = function(inputConfig, isMobileDevice, input, output, session, rv,
                          attachments, views) {
      private$inputConfig <- inputConfig
      private$rv <- rv
      typeWidgetMapping <- list(
        hot = HandsonTableWidget,
        dt = DataTableWidget,
        slider = SliderWidget,
        textinput = TextInputWidget,
        numericinput = NumericInputWidget,
        dropdown = DropdownWidget,
        date = DateWidget,
        daterange = DateRangeWidget,
        checkbox = CheckboxWidget,
        custom = CustomWidget
      )
      private$inputWidgets <- lapply(seq_along(inputConfig), function(id) {
        symName <- names(inputConfig)[[id]]
        config <- inputConfig[[id]]
        if (identical(config$type, "hot") && isMobileDevice) {
          return(DataTableWidget$new(id, config, input, output, session, rv, self,
            symName = symName,
            htmlSelector = paste0("in_m_")
          ))
        }
        widget <- typeWidgetMapping[[config$type]]
        if (!is.null(widget)) {
          return(widget$new(id, config, input, output, session, rv, self, symName = symName))
        }
        stop_custom("error_not_implemented", sprintf("Widget type: '%s' not implemented for symbol: '%s'", config$type, symName))
      })
      names(private$inputWidgets) <- names(inputConfig)
      self$attachments <- attachments
      self$views <- views
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
        return(private$inputWidgets[[private$inputConfig[[symName]]$definedByExternalSymbol]]$getData(symName))
      }
      return(private$inputWidgets[[symName]]$getData())
    },
    hasData = function(symName) {
      if (length(private$inputConfig[[symName]]$definedByExternalSymbol)) {
        return(private$inputWidgets[[private$inputConfig[[symName]]$definedByExternalSymbol]]$hasData(symName))
      }
      return(private$inputWidgets[[symName]]$hasData())
    },
    setData = function(symName, data) {
      if (length(private$inputConfig[[symName]]$definedByExternalSymbol)) {
        return(private$inputWidgets[[private$inputConfig[[symName]]$definedByExternalSymbol]]$setData(data, symName))
      }
      private$inputWidgets[[symName]]$setData(data)
      return(invisible(self))
    },
    getSymConfig = function(symName) {
      return(private$inputConfig[[symName]])
    },
    getWidget = function(symName) {
      return(private$inputWidgets[[symName]])
    },
    attachments = NULL,
    views = NULL
  ),
  private = list(
    rv = NULL,
    inputConfig = NULL,
    inputWidgets = NULL
  )
)
