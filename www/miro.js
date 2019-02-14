function changeTab(object, idActive, idRefer) {
    tabPane = object.closest(".tabbable");
    tabPane.find("li:nth-of-type(" + idActive + ")").removeClass();
    tabPane.find("li:nth-of-type(" + idRefer + ")").addClass("active");
    tabPane.find(".tab-content div:nth-child(" + idActive + ")").removeClass("active");
    tabPane.find(".tab-content div:nth-child(" + idRefer + ")").addClass("active");
}
function isInputEl(id){
  if($(id).parents(".form-group").length){
    return true;
  }else{
    return false;
  }
}
function rerenderHot(){
  try{
      setTimeout(function(){ HTMLWidgets.getInstance($(".rhandsontable:visible").get(0)).hot.render(); }, 100);
  }catch(e){}
}
let spinnerActive = {};
function showSpinnerIcon(el, delay = 3000){
  if(spinnerActive[$(el).prop('id')]){
    return;
  }
  spinnerActive[$(el).prop('id')] = true;
  let content = $(el).html();
  $(el).html('<i class="fa fa-refresh fa-spin"></i>');
  setTimeout(function(){
    $(el).html(content);
    spinnerActive[$(el).prop('id')] = false;
  }, delay);
  
}
function changeActiveButtons(tabId){
  switch(tabId) {
    case 'inputData':
        $("#btImport").show();
        $("#btSolve").show();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        break;
    case 'outputData':
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        break;
    case 'gamsinter':
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").show();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        break;
    case 'scenarios':
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").show();
        $("#btCompareScen").show();
        break;
    default:
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
  }
}
function confirmModalShow(title, desc, cancelTxt, confirmTxt = null, confirmCall = null){
  let btDataDismiss = '<button type="button" class="btn btn-default" data-dismiss="modal">' + cancelTxt + '</button>';
  let btDataConfirm = '';
  if(confirmCall !== null){
    btDataConfirm = '<button type="button" class="btn btn-default bt-highlight-1 bt-gms-confirm" onclick="' + confirmCall + '" data-dismiss="modal">' + confirmTxt + '</button>';
  }
  btData = btDataDismiss + btDataConfirm;
  cModal = $('#confirmModal');
  cModal.find('.modal-title').html(title);
  cModal.find('.modal-body').html(desc);
  cModal.find('.modal-footer').html(btData);
  cModal.modal('show');
}
  
function removeAttachment(elId){
  $('#btRemoveAttachment_' + elId).parent().parent().remove();
  Shiny.setInputValue("btRemoveAttachment_" + elId, 1, {priority: "event"});
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

function renderMathJax() {
  MathJax.Hub.Queue(["Typeset", MathJax.Hub, "wrapper-documentation"]);
}
function validateSname(el){
  if(/^[a-f0-9]{64}$/i.test($(el).value) !== true && /^\s*$/.test($(el).value) !== true){
    $(el).removeClass('invalidInput');
    Shiny.setInputValue("btCheckSnameLocalConfirm", 1, {priority: "event"});
    return;
  }else{
    $(el).addClass('invalidInput');
  }
}
function validateHcubeHash(){
  const hashVal = $('#hcHashLookup').val();
  if(/^[a-f0-9]{64}$/i.test(hashVal) === true){
    $('#hcHashLookup').removeClass('invalidInput');
    Shiny.setInputValue("hcHashLookup", hashVal, {priority: "event"});
    return;
  }
  $('#hcHashLookup').addClass('invalidInput');
}
function hcHashImport(sid){
  Shiny.setInputValue("loadHcubeHashSid", sid, {priority: "event"});
}
function showHideEl(el, delay){
  $(el).show().delay(delay).fadeOut();
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
    rerenderHot();
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
  $("#inputTabset li").click(function(){
    rerenderHot();
  });
  $("#scenTabset").append("<li id=\"scenTabsetAdd\"><a href=\"#\" data-value=\"scen_add\" " +
  "onclick=\"Shiny.setInputValue('btLoadScen', 1, {priority: 'event\'});\">" +
  "<i class=\"far fa-plus-square\" style=\"font-size:13pt;\"></i></a></li>");
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
  $("body").on("click", ".bt-highlight-1, .bt-highlight-2, .bt-highlight-3", function() {
      let btn = $(this);
      btn.prop('disabled', true);
      setTimeout(function(){
          btn.prop('disabled', false);
      }, 1500);
  });
  // hide pivot filter boxes when clicked outside of box
  $(document).click(function(e) {
    var target = e.target;
    if (!$(target).is('.pvtAttr') && !$(target).parents('.pvtAttr').length
        && !$(target).is('.pvtFilterBox') && !$(target).parents('.pvtFilterBox').length){
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
  Shiny.addCustomMessageHandler('gms-showElReplaceTxt', function(data) {
    $(data.id).text(data.txt).show();
  });
  Shiny.addCustomMessageHandler('gms-hideEl', function(id) {
    if(isInputEl(id)){
      $(id).closest(".shiny-input-container").hide();
    }else{
      $(id).hide();
    }
  });
  Shiny.addCustomMessageHandler('gms-showHideEl', function(data) {
    showHideEl(data.id, data.delay);
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
  Shiny.addCustomMessageHandler('gms-fitTitleInBox', function(id) {
    setTimeout(function() { 
      let el = $(id);
      let parentEl = el.parent()[0];
      let currSize = parseInt(el.css("font-size"));
      while(parentEl.scrollWidth > parentEl.clientWidth && currSize >= 10){
        currSize -= 2;
        el.css("font-size", currSize + "px");
      }
    }, 500);
  });
});
