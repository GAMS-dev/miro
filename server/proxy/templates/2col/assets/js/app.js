$(function () {
  $('.nav-home').removeClass('active');
  const currLocation = window.location.pathname;
  if (
    !(
      currLocation.endsWith('app/admin') || currLocation.endsWith('app/admin/')
    )
  ) {
    const handle = document.getElementById('navbarHandle');
    // clicks inside the app iframe never reach this document, but they move the focus into it
    $(window).on('blur', function () {
      bootstrap.Dropdown.getOrCreateInstance(handle).hide();
    });
    // the account menu is positioned for MIRO's header, so only show it once the app is displayed;
    // the loading and error screens have their own buttons to get back to the app list
    const isShown = (el) => el !== null && getComputedStyle(el).display !== 'none';
    const updateBusyState = function () {
      const busy =
        !isShown(document.getElementById('shinyframe')) ||
        $('#loading, .load-app-error-absolute').toArray().some(isShown);
      document.body.classList.toggle('app-page-busy', busy);
    };
    new MutationObserver(updateBusyState).observe(document.body, {
      attributes: true,
      attributeFilter: ['style'],
      childList: true,
      subtree: true,
    });
    updateBusyState();
  } else {
    $('#navAdminPanel').attr('href', '#').addClass('active');
    $('#navbarWrapper').show().attr('href', '#');
  }

  $('.btn-restart-app').on('click', () => {
    window.location.reload();
  });
});
