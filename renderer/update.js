const { ipcRenderer, shell } = require('electron');
const querystring = require('querystring');
const https = require('https');
const log = require('electron-log/renderer');
const $ = require('jquery');

const installedVersion = querystring.parse(global.location.search)['?miroVersion'].split('.');

$('#btClose').on('click', () => {
  ipcRenderer.send('close-window', 'update');
});

function updateStatus(status, text = true) {
  $('#updateSpinner').hide();
  if (text) {
    $('#updateText').text(status);
  } else {
    $('#updateText').html(status);
  }
}

$('.site-wrapper').on('click', '#downloadMIRO', () => {
  shell.openExternal('https://gams.com/miro');
});

ipcRenderer.on('lang-data-received', (e, lang) => {
  $('#btClose').text(lang.btClose);

  https.get('https://gams.com/miro/latest.ver', (res) => {
    if (res.statusCode !== 200) {
      updateStatus(lang.error);
      return;
    }
    res.setEncoding('utf8');
    let rawData = '';
    res.on('data', (chunk) => { rawData += chunk; });
    res.on('end', () => {
      try {
        const currentVersion = rawData.trim().split(',');
        if (!currentVersion || currentVersion.length !== 3) {
          updateStatus(lang.error);
          return;
        }
        const newVersionText = `${lang.updateAvailable}<br>${lang.downloadUpdate} <a href="#" id="downloadMIRO">${lang.here}</a>.`;
        const currentMajor = parseInt(currentVersion[0], 10);
        const installedMajor = parseInt(installedVersion[0], 10);
        if (currentMajor > installedMajor) {
          updateStatus(newVersionText, false);
          return;
        } if (currentMajor === installedMajor) {
          const currentMinor = parseInt(currentVersion[1], 10);
          const installedMinor = parseInt(installedVersion[1], 10);
          if (currentMinor > installedMinor) {
            updateStatus(newVersionText, false);
            return;
          } if (currentMinor === installedMinor
                    && parseInt(currentVersion[2], 10) > parseInt(installedVersion[2], 10)) {
            updateStatus(newVersionText, false);
            return;
          }
        }
        updateStatus(lang.upToDate);
        return;
      } catch (err) {
        log.info(`Problems getting latest MIRO version. Error message: ${err.message}.`);
        updateStatus(lang.error);
      }
    });
  }).on('error', (err) => {
    log.info(`Problems getting latest MIRO version. Error message: ${err.message}.`);
    updateStatus(lang.error);
  });
});
