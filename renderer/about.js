const copyrightYear = window.globals
  .miroRelease()
  .substr(window.globals.miroRelease.length - 4);

// development builds have their own version, e.g. 2.14.0-dev.4600.<branch>
const buildVersion = new URLSearchParams(window.location.search).get(
  'buildVersion',
);
const buildRow =
  buildVersion && buildVersion !== window.globals.miroVersion()
    ? `<dt>Build</dt><dd>${buildVersion}</dd>`
    : '';

const aboutText = `<div class="about-header">\
<img class="about-logo" src="../static/icon_64x64.png" alt="" />\
<div><div class="about-title">GAMS MIRO</div>\
<div class="about-version">Version ${window.globals.miroVersion()} <span>(__HASH__)</span></div></div></div>\
<dl class="about-meta">${buildRow}\
<dt>Release date</dt><dd>${window.globals.miroRelease()}</dd>\
<dt>Copyright</dt><dd>&copy; 2019&ndash;${copyrightYear} GAMS Software GmbH<br/>\
&copy; 2019&ndash;${copyrightYear} GAMS Development Corp.</dd>\
<dt>Support</dt><dd>support@gams.com</dd></dl>\
<p class="about-legal">This program is free software: you can redistribute it and/or modify it under the terms of version 3 \
of the GNU General Public License as published by the Free Software Foundation. \
It is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; \
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. \
See the <a href="http://www.gnu.org/licenses/">GNU General Public License</a> for more details. \
For information about third-party software included in MIRO, see the \
<a href="http://www.gams.com/miro/license.html">license overview</a>.</p>`;

$('#aboutText').html(aboutText);
