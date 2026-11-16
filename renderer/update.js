const { ipcRenderer } = require('electron');
const log = require('electron-log/renderer');
const $ = require('jquery');

log.errorHandler.startCatching();

$('#btClose').on('click', () => {
  ipcRenderer.send('close-window', 'update');
});

$('#btUpdate').on('click', () => {
  ipcRenderer.send('confirm-update');
  ipcRenderer.send('close-window', 'update');
});

function updateStatus(status, detail = '') {
  $('#updateSpinner').hide();
  $('#updateText').text(status);
  $('#updateDetail').text(detail);
}

ipcRenderer.on('lang-data-received', async (e, lang) => {
  $('#btClose').text(lang.btClose);
  $('#btUpdate').text(lang.btUpdate);
  const { status, version } = await ipcRenderer.invoke('check-for-update');
  if (status === 'upToDate') {
    updateStatus(lang.upToDate);
  } else if (status === 'available') {
    updateStatus(`${lang.updateAvailable}\n${version}`);
    $('#btClose').text(lang.btLater).removeClass('confirm-btn');
    $('#btUpdate').show();
  } else if (status === 'downloading') {
    updateStatus(`${lang.updateAvailable}\n${version}`, lang.downloading);
  } else if (status === 'downloaded') {
    updateStatus(lang.downloaded.replace('%s', version));
  } else {
    updateStatus(lang.error);
  }
});
