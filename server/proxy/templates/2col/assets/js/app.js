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

  const appConfig = document.getElementById('appConfig')?.dataset;
  if (appConfig) {
    window.Shiny.common.init(
      appConfig.contextPath??null,
      appConfig.application_name??null,
      appConfig.spInstance??null,
      appConfig.appMaxInstances??null,
      appConfig.myAppsMode??null,
      appConfig.pauseSupported==='true');
    window.Shiny.app.start(
      appConfig.proxy??null,
      appConfig.heartbeatRate == null? 10000: parseInt(appConfig.heartbeatRate),
      appConfig.appName??null,
      appConfig.appInstance??null,
      appConfig.parameterAllowedCombinations??null,
      appConfig.parameterDefinitions??null,
      appConfig.parameterIds??null,
      appConfig.appPath??"",
      appConfig.containerSubPath??"",
    );
    if (appConfig.refreshOpenidEnabled==='true') {
      window.Shiny.connections.startOpenidRefresh();
    }
  }
});
