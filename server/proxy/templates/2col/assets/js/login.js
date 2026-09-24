// Show/hide toggle for the password field on the sign-in page
// (external file: the page's Content-Security-Policy does not allow inline scripts).
(function () {
  const toggle = document.getElementById('passwordToggle');
  const input = document.getElementById('password');
  if (!toggle || !input) {
    return;
  }
  toggle.addEventListener('click', function () {
    const show = input.type === 'password';
    input.type = show ? 'text' : 'password';
    const label = show ? 'Hide password' : 'Show password';
    toggle.setAttribute('aria-pressed', String(show));
    toggle.setAttribute('aria-label', label);
    toggle.setAttribute('title', label);
    input.focus();
  });
})();
