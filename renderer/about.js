const copyrightYear = window.globals
  .miroRelease()
  .substr(window.globals.miroRelease.length - 4);

const aboutText = `<b>GAMS MIRO v.${window.globals.miroVersion()}</b>&nbsp;&nbsp;<sup>(__HASH__)</sup><br/><br/>\
Release Date: ${window.globals.miroRelease()}<br/>\
Copyright (c) 2019 - ${copyrightYear} GAMS Software GmbH &lt;support@gams.com&gt;<br/>\
Copyright (c) 2019 - ${copyrightYear} GAMS Development Corp. &lt;support@gams.com&gt;<br/><br/>\
This program is free software: you can redistribute it and/or modify it under the terms of version 3 \
of the GNU General Public License as published by the Free Software Foundation.<br/><br/>\
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; \
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. \
See the GNU General Public License for more details.<br/><br/>\
You should have received a copy of the GNU General Public License along with this program. \
If not, see <a href="http://www.gnu.org/licenses/">http://www.gnu.org/licenses/</a>. \
For more information about third-party software included in MIRO, see <a href="http://www.gams.com/miro/license.html">here</a>`;

$('#aboutText').html(aboutText);
