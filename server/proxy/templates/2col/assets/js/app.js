$(function () {
  const currLocation = window.location.pathname;
  if (!(currLocation.endsWith("app/admin") || currLocation.endsWith("app/admin/"))) {
    let timer;
    $("#navbarHandle").on('mouseover', function () {
      clearTimeout(timer);
      $("#navbarWrapper").show();
    });
    $("#navbarWrapper").on('mouseover', function () {
      clearTimeout(timer);
      $("#navbarWrapper").show();
    });
    $("#navbarWrapper").on('mouseleave', function () {
      clearTimeout(timer);
      timer = setTimeout(function () {
        $("#navbarWrapper").hide();
      }, 600);
    });
  } else {
    $('#navAdminPanel').addClass('active').attr('href', '#');
    $("#navbarWrapper").show().attr('href', '#');
  }

  $('.btn-restart-app').on('click', () => {
    window.location.reload();
  });
});
