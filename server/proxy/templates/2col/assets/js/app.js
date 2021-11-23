$(document).ready(function() {
    let timer;
    const currLocation = window.location.href;
    if (!currLocation.endsWith("app/admin")) {
      $("#navbarHandle").hover(function () {
        clearTimeout(timer);
        $("#navbarWrapper").show();
      });
      $("#navbarWrapper").hover(function () {
        clearTimeout(timer);
        $("#navbarWrapper").show();
      });
      $("#navbarWrapper").mouseleave(function () {
        clearTimeout(timer);
        timer = setTimeout(function () {
          $("#navbarWrapper").hide();
        }, 600);
      });
    }

    const source = $("#shinyframe").attr("src");
    if (source == "") {
      $(".loading-screen").show();
      $.post(
        window.location.pathname + window.location.search,
        function (response) {
          $("#shinyframe").attr("src", response.containerPath);
          $(".loading-screen").delay(1000).fadeOut(500);
        }
      ).fail(function (request) {
        console.log(request.responseText);
        $("#loadingAnimation").hide();
        $("#loadAppError").show();
      });
    }
});
