function changeTab(object, idActive, idRefer) {
    tabPane = object.closest(".tabbable");
    tabPane.find("li:nth-of-type(" + idActive + ")").removeClass();
    tabPane.find("li:nth-of-type(" + idRefer + ")").addClass("active");
    tabPane.find(".tab-content div:nth-child(" + idActive + ")").removeClass("active");
    tabPane.find(".tab-content div:nth-child(" + idRefer + ")").addClass("active");
}
function isInputEl(id){
  if(id.startsWith("#slider_") || id.startsWith("#dropdown_") ||
  id.startsWith("#dropdowne_") || id.startsWith("#date_") ||
  id.startsWith("#daterange_") || id.startsWith("#cb_")){
    return true;
  }else{
    return false;
  }
}
function changeActiveButtons(tabId){
  switch(tabId) {
    case 'inputData':
        $("#btImport").show();
        $("#btSolve").show();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
        break;
    case 'outputData':
        $("#btImport").show();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
        break;
    case 'gamsinter':
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").show();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
        break;
    case 'scenarios':
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").show();
        $("#btCompareScen").show();
        $("#btLoadScen").show();
        break;
    default:
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
  }
}
function confirmModalShow(title, desc, cancelTxt, confirmTxt = null, confirmCall = null){
  let btDataDismiss = '<button type="button" class="btn btn-default" data-dismiss="modal">' + cancelTxt + '</button>';
  let btDataConfirm = '';
  if(confirmCall !== null){
    btDataConfirm = '<button type="button" class="btn btn-default bt-gms-confirm" onclick="' + confirmCall + '" data-dismiss="modal">' + confirmTxt + '</button>';
  }
  btData = btDataDismiss + btDataConfirm;
  cModal = $('#confirmModal');
  cModal.find('.modal-title').html(title);
  cModal.find('.modal-body').html(desc);
  cModal.find('.modal-footer').html(btData);
  cModal.modal('show');
}

let removeButtonCounter = {};
  
function removeAttachment(elId){
  $('#btRemoveAttachment_' + elId).parent().parent().remove();
  if(typeof(removeButtonCounter[elId]) == 'undefined'){
    removeButtonCounter[elId] = 0;
  }else{
    removeButtonCounter[elId] = removeButtonCounter[elId] + 1;
  }
  Shiny.setInputValue("btRemoveAttachment_" + elId, removeButtonCounter[elId]);
}

function showHypercubeLog(jID){
  Shiny.setInputValue("showHypercubeLog", jID, {priority: "event"});
}
function importHypercubeJob(jID){
  Shiny.setInputValue("importHypercubeJob", jID, {priority: "event"});
}

function discardHypercubeJob(jID){
  Shiny.setInputValue("discardHypercubeJob", jID, {priority: "event"});
}

$(document).ready(function () {
  $("body").addClass("fixed");
// besides these updates, gms-switchTab (see below) has always has to be considered as well
  $("#btImport").show();
  $("#btSolve").show();
  $("#btInterrupt").hide();
  $("#btSplitView").hide();
  $("#btCompareScen").hide();
  $("#btLoadScen").hide();
  
  $("a[data-value='inputData']").click(function() {
    changeActiveButtons('inputData');
  });
  $("a[data-value='outputData']").click(function() {
    changeActiveButtons('outputData');
  });
  $("a[data-value='gamsinter']").click(function() {
    changeActiveButtons('gamsinter');
  });
  $("a[data-value='scenarios']").click(function() {
    changeActiveButtons('scenarios');
  });
  $("a[data-value='advanced'],a[data-value='importData'],a[data-value='loadResults'],a[data-value='hcubeAnalyze']").click(function() {
    changeActiveButtons('default');
  });
  // show/hide buttons after (R triggered) tab switch.
  Shiny.addCustomMessageHandler('gms-switchTab', function(el) {
    switch(el) {
    case "input":
        changeActiveButtons('inputData');
        break;
    case "output":
        changeActiveButtons('outputData');
        break;
    case "gamsinter":
        changeActiveButtons('gamsinter');
        break;
    case "hcubeAna":
        changeActiveButtons('default');
        break;
    case "scenComp":
        changeActiveButtons('scenarios');
        break;
    }
  });
  // hide pivot filter boxes when clicked outside of box
  $(document).click(function(e) {
  var target = e.target;
  if (!$(target).is('.pvtAttr') && !$(target).parents().is('.pvtAttr')
      && !$(target).is('.pvtFilterBox') && !$(target).parents().is('.pvtFilterBox')) {
    $('.pvtFilterBox').hide();
  }
  });
  $(window).on('beforeunload', function() {
    if($("#shiny-disconnected-overlay").length === 0){
      return "Are you sure you want to leave? Unsaved changes will be lost!";
    }
  });
  
  Shiny.addCustomMessageHandler('gms-showEl', function(id) {
    if(isInputEl(id)){
      $(id).closest(".shiny-input-container").show();
    }else{
      $(id).show();
    }
  });
  Shiny.addCustomMessageHandler('gms-hideEl', function(id) {
    if(isInputEl(id)){
      $(id).closest(".shiny-input-container").hide();
    }else{
      $(id).hide();
    }
  });
  Shiny.addCustomMessageHandler('gms-showHideEl', function(data) {
    el = $(data.id);
    el.show().delay(data.delay).fadeOut();
  });
  Shiny.addCustomMessageHandler('gms-enableEl', function(id) {
    $(id).prop( "disabled", false);
  });
  Shiny.addCustomMessageHandler('gms-disableEl', function(id) {
    $(id).prop( "disabled", true);
  });
  Shiny.addCustomMessageHandler('gms-toggleEl', function(id) {
    if($(id).is(":visible")){
      $(id).toggle();
      $(id).trigger("shown");
    }else{
      $(id).fadeToggle(600);
      $(id).trigger("hidden");
    }
  });
  Shiny.addCustomMessageHandler('gms-addClassEl', function(el) {
    $(el.id).addClass(el.newclass);
  });
  Shiny.addCustomMessageHandler('gms-removeClassEl', function(el) {
    $(el.id).removeClass(el.oldclass);
  });
  Shiny.addCustomMessageHandler('gms-scrollDown', function(id) {
    setTimeout(function(){
      $(id).animate({
        scrollTop: $(id)[0].scrollHeight - $(id)[0].clientHeight
      }, 300);
    }, 500);
  });
  Shiny.addCustomMessageHandler('gms-hideModal', function(delay) {
    setTimeout(function() { $('#shiny-modal-wrapper').find('.modal').modal('hide'); }, delay * 1000);
  });
  Shiny.addCustomMessageHandler('gms-updateAttachList', function(el){
    el.id = $.makeArray(el.id);
    el.name = $.makeArray(el.name);
    for(i=0;i<el.id.length;i++){
      if(el.allowExec){
        checkBoxHTML = '<div class="col-sm-6"><div class="form-group shiny-input-container"><div class="checkbox"><label><input type="checkbox" onchange="Shiny.setInputValue(\'execPermAttachment_' + el.id[i] +'\', $(this).is(\':checked\'));" checked="checked"><span>' + el.labelCb + '</span></label></div></div></div>';
      }else{
        checkBoxHTML = '';
      }
      $('<div class="row attachment-line"><div class="col-sm-6"><button class="btn btn-default bt-icon" id="btRemoveAttachment_' + el.id[i] + '" type="button" onclick="removeAttachment(' + el.id[i] + ')"><i class="fa fa-times-circle"></i></button> ' + el.name[i] + '</div>' + checkBoxHTML+ '</div>').insertBefore('#endAttachList');
    }
  });
  
});

function renderMathJax() {
  MathJax.Hub.Queue(["Typeset", MathJax.Hub, "wrapper-documentation"]);
}
