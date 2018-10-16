$(document).ready(function () {
  
// besides these updates, gms-switchTab (see below) has always has to be considered as well
  $("#btImport").show();
  $("#btSolve").show();
  $("#btInterrupt").hide();
  $("#btSplitView").hide();
  $("#btCompareScen").hide();
  $("#btLoadScen").hide();
  
  $("a[data-value='inputData']").click(function() {
    $("#btImport").show();
    $("#btSolve").show();
    $("#btInterrupt").hide();
    $("#btSplitView").hide();
    $("#btCompareScen").hide();
    $("#btLoadScen").hide();
  });
  $("a[data-value='outputData']").click(function() {
    $("#btImport").show();
    $("#btSolve").hide();
    $("#btInterrupt").hide();
    $("#btSplitView").hide();
    $("#btCompareScen").hide();
    $("#btLoadScen").hide();
  });
  $("a[data-value='gamsinter']").click(function() {
    $("#btImport").hide();
    $("#btSolve").hide();
    $("#btInterrupt").show();
    $("#btSplitView").hide();
    $("#btCompareScen").hide();
    $("#btLoadScen").hide();
  });
  $("a[data-value='scenarios']").click(function() {
    $("#btImport").hide();
    $("#btSolve").hide();
    $("#btInterrupt").hide();
    $("#btSplitView").show();
    $("#btCompareScen").show();
    $("#btLoadScen").show();
  });
  $("a[data-value='advanced']").click(function() {
    $("#btImport").hide();
    $("#btSolve").hide();
    $("#btInterrupt").hide();
    $("#btSplitView").hide();
    $("#btCompareScen").hide();
    $("#btLoadScen").hide();
  });
  $("a[data-value='advanced'],a[data-value='importData'],a[data-value='loadResults'],a[data-value='batchAnalyze']").click(function() {
    $("#btImport").hide();
    $("#btSolve").hide();
    $("#btInterrupt").hide();
    $("#btSplitView").hide();
    $("#btCompareScen").hide();
    $("#btLoadScen").hide();
  });
  // show/hide buttons after (R triggered) tab switch.
  Shiny.addCustomMessageHandler('gms-switchTab', function(el) {
    switch(el) {
    case "input":
        $("#btImport").show();
        $("#btSolve").show();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
        break;
    case "output":
        $("#btImport").show();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
        break;
    case "gamsinter":
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").show();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
        break;
    case "batchAna":
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
        break;
    case "scenComp":
        $("#btImport").hide();
        $("#btSolve").hide();
        $("#btInterrupt").hide();
        $("#btSplitView").show();
        $("#btCompareScen").show();
        $("#btLoadScen").show();
        break;
    default:
        $("#btImport").show();
        $("#btSolve").show();
        $("#btInterrupt").hide();
        $("#btSplitView").hide();
        $("#btCompareScen").hide();
        $("#btLoadScen").hide();
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
});

function renderMathJax() {
  MathJax.Hub.Queue(["Typeset", MathJax.Hub, "wrapper-documentation"]);
}