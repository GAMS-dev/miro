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
function renderMathJax() {
  MathJax.Hub.Queue(["Typeset", MathJax.Hub, "wrapper-documentation"]);
}