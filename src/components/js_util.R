showEl <- function(session, id, inline = FALSE){
  session$sendCustomMessage("gms-showEl", list(id = id, inline = inline))
}
showLoadingScreen <- function(session, delay){
  session$sendCustomMessage("gms-showLoadingScreen", delay)
}
hideLoadingScreen <- function(session){
  session$sendCustomMessage("gms-hideLoadingScreen", list())
}
showElReplaceTxt <- function(session, id, txt){
  session$sendCustomMessage("gms-showElReplaceTxt", list(id = id, txt = htmltools::htmlEscape(txt)))
}
setContent <- function(session, selector, htmlContent){
  session$sendCustomMessage("gms-setContent", list(selector = selector, content = htmlContent))
}
setTextContent <- function(session, selector, htmlContent){
  session$sendCustomMessage("gms-setTextContent", list(selector = selector, content = htmlContent))
}
setAttributes <- function(session, selectors, attribute, values){
  stopifnot(identical(length(selectors), length(values)))
  session$sendCustomMessage("gms-setAttribs",
                            list(selectors = I(selectors),
                                 attr = attribute, vals = I(values)))
}
switchCompareMode <- function(session, mode, numberScenTabs){
  if(identical(mode, "pivotView")){
    hideEl(session, "#scen-split-view")
    hideEl(session, "#scen-tab-view")
    showEl(session, "#scen-pivot-view")
    hideEl(session, "#btCompareScen")
    setAttributes(session, "#btCompareScen", "data-noshow", "true")
    setTextContent(session, "#btSelectCompareMode", lang$nav$sidebarButtons$pivotView)
    return()
  }
  showEl(session, "#btCompareScen")
  setAttributes(session, "#btCompareScen", "data-noshow", "false")
  if(identical(mode, "splitView")){
    enableEl(session, "#btCompareScen")
    showEl(session, "#scen-split-view")
    hideEl(session, "#scen-tab-view")
    hideEl(session, "#scen-pivot-view")
    setTextContent(session, "#btSelectCompareMode", lang$nav$sidebarButtons$splitView)
  }else{
    if(numberScenTabs < 2){
      disableEl(session, "#btCompareScen")
    }else{
      enableEl(session, "#btCompareScen")
    }
    hideEl(session, "#scen-split-view")
    showEl(session, "#scen-tab-view")
    hideEl(session, "#scen-pivot-view")
    setTextContent(session, "#btSelectCompareMode", lang$nav$sidebarButtons$tabView)
  }
}
hideEl <- function(session, id){
  session$sendCustomMessage("gms-hideEl", id)
}
setCssEl <- function(session, id, css){
  session$sendCustomMessage("gms-setCss", list(id = id, css = css))
}
showHideEl <- function(session, id, delay = 2000, msg = NULL){
  session$sendCustomMessage("gms-showHideEl", list(id = id, delay = delay, msg = msg))
}
enableEl <- function(session, id){
  session$sendCustomMessage("gms-enableEl", id)
}
scrollDown <- function(session, id){
  session$sendCustomMessage("gms-scrollDown", id)
}
disableEl <- function(session, id){
  session$sendCustomMessage("gms-disableEl", id)
}
slideToggleEl <- function(session, id, duration = 400, toggleIconDiv = NULL){
  session$sendCustomMessage("gms-slideToggleEl", 
                            list(id = id, duration = duration, 
                                 toggleIconDiv = toggleIconDiv))
}
toggleEl <- function(session, id){
  session$sendCustomMessage("gms-toggleEl", id)
}
addClassEl <- function(session, id, class){
  session$sendCustomMessage("gms-addClassEl", list(id = id, newclass = class))
}
removeClassEl <- function(session, id, class){
  session$sendCustomMessage("gms-removeClassEl", list(id = id, oldclass = class))
}
emptyEl <- function(session, id){
  session$sendCustomMessage("gms-emptyEl", id)
}
appendEl <- function(session, id, content, text = TRUE, scroll = FALSE, 
                     triggerChange = FALSE){
  session$sendCustomMessage("gms-appendEl", list(id = id, content = content, 
                                                 text = text, scroll = scroll,
                                                 triggerChange = triggerChange))
}
hideModal <- function(session, delay = 1L){
  session$sendCustomMessage("gms-hideModal", delay)
}
updateAttachList <- function(session, id, fileName, token, labelCb, allowExec = FALSE){
  session$sendCustomMessage("gms-updateAttachList", list(name = fileName, id = id, 
                                                         token = token, labelCb = labelCb, 
                                                         allowExec = allowExec))
}
fitTitleInBox <- function(session, id){
  session$sendCustomMessage("gms-fitTitleInBox", id)
}
switchTab <- function(session, id){
  session$sendCustomMessage("gms-switchTab", id)
}
toJSString <- function(string){
  return(toJSON(string, auto_unbox = TRUE))
}
getHotCustomColOptions <- function(noDomains){
  setNames(
    list(list(name = lang$renderers$handsontable$newCol$nameLeft,
              callback = JS(paste0("function(key, normalizedSelection){
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
}")),
              disabled = JS(paste0("function(){
                                return this.getSelectedLast()[1]<=", noDomains - 1L, ";}"))),
         list(name = lang$renderers$handsontable$newCol$nameRight,
              callback = JS(paste0("function(key, normalizedSelection){
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
}")),
              disabled = JS(paste0("function(){
                                return this.getSelectedLast()[1]<=", noDomains - 2L, ";}"))),
         list(name = lang$renderers$handsontable$renameCol$name,
              callback = JS(paste0("function(){
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
}")),
              disabled = JS(paste0("function(){
const selection = this.getSelected();
if (selection && selection.length > 1) {
   return true;
}
return this.getSelectedLast()[1]<=", noDomains - 1L, ";}"))),
         list(name = JS(paste0("function(){
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
              callback = JS(paste0("function(){
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
}")),
              disabled = JS(paste0("function(){
return this.getSelectedLast()[1]<=", noDomains - 1L, ";}")))),
    c("column_left", "column_right", "rename_column", "remove_column"))
}