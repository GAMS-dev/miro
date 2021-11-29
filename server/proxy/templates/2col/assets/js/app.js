$(document).ready(function() {
    const currLocation = window.location.pathname;
    if (!(currLocation.endsWith("app/admin") || currLocation.endsWith("app/admin/"))) {
      let timer;
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

    $('.btn-restart-app').on('click', () => {
      window.location.reload();
    });
    const appConfig = document.getElementById('appConfig').dataset;
    window.Shiny.common.init(appConfig.context_path, appConfig.application_name);
    window.Shiny.app.start(
        appConfig.container_path == null? '': appConfig.container_path,
        appConfig.websocket_reconnection_mode == null? '': appConfig.websocket_reconnection_mode,
        appConfig.proxy_id == null? '': appConfig.proxy_id,
        appConfig.heartbeat_rate,
        appConfig.app_name,
        appConfig.app_instance,
        appConfig.max_instances,
        false,
    );
});
