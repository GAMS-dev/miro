// hide pivot filter boxes when clicked outside of box
$(document).click(function(e) {
  var target = e.target;
  if (!$(target).is('.pvtAttr') && !$(target).parents().is('.pvtAttr')
      && !$(target).is('.pvtFilterBox') && !$(target).parents().is('.pvtFilterBox')) {
    $('.pvtFilterBox').hide();
  }
});