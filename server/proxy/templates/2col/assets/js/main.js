(function () {
  document.addEventListener('click', function (e) {
    const card = e.target.closest('.launch-app-box[data-launch-url]');
    if (card) {
      window.location.href = card.getAttribute('data-launch-url');
    }
  });
  document.addEventListener('keydown', function (e) {
    if (
      (e.key === 'Enter' || e.key === ' ') &&
      e.target.matches('.launch-app-box[data-launch-url]')
    ) {
      e.preventDefault();
      window.location.href = e.target.getAttribute('data-launch-url');
    }
  });

  const navToggle = document.querySelector('button.navbar-toggle');
  if (navToggle) {
    navToggle.addEventListener('click', function () {
      if (window.innerWidth < 1075) {
        this.classList.toggle('collapsed');
        document
          .getElementById('navbar-collapse-gams')
          .classList.toggle('collapse');
      }
    });
  }
})();
