import { app, dialog, shell } from 'electron';
import log from 'electron-log/main.js';
import updater from 'electron-updater';
import { format } from 'node:util';
import { dialogParent } from './util.js';

const { autoUpdater } = updater;
const feedBaseUrl = 'https://gams.com/miro/updates';
const releaseNotesUrl = 'https://gams.com/miro/release.html';
const channels = ['latest', 'dev'];
const checkIntervalMs = 6 * 60 * 60 * 1000;
// deb and rpm packages are updated via the package manager
const canInstall =
  process.platform !== 'linux' || process.env.APPIMAGE != null;

let configData;
let lang;
let getMainWindow;
let checkTimer;
let availableVersion;
let notifiedVersion;
let downloadedVersion;
let downloadingVersion;
// a dialog is open
let confirming = false;
let manualCheck = false;

function getUpdateChannel() {
  const channel =
    process.env.MIRO_UPDATE_CHANNEL || configData.getSync('updateChannel');
  if (!channel) {
    return 'latest';
  }
  if (!channels.includes(channel)) {
    log.warn(`Unknown update channel: ${channel}. Using 'latest'.`);
    return 'latest';
  }
  return channel;
}

function configureFeed() {
  const channel = getUpdateChannel();
  log.info(`Update channel: ${channel}`);
  autoUpdater.setFeedURL({
    provider: 'generic',
    url: `${feedBaseUrl}/${channel}`,
    channel,
  });
}

function setUpdateChannelFromArgs(argv) {
  const arg = argv.find((el) => el.startsWith('--update-channel='));
  if (!arg || !configData) {
    return;
  }
  const channel = arg.slice('--update-channel='.length);
  if (!channels.includes(channel)) {
    log.warn(`Ignoring unknown update channel: ${channel}`);
    return;
  }
  configData.set({ updateChannel: channel === 'latest' ? '' : channel });
  configureFeed();
}

function setProgress(progress) {
  getMainWindow()?.setProgressBar(progress);
}

function showDialog(options) {
  return dialog.showMessageBox(dialogParent(getMainWindow()), {
    title: lang.title,
    noLink: true,
    cancelId: 0,
    ...options,
  });
}

async function download() {
  try {
    await autoUpdater.downloadUpdate();
  } catch (err) {
    // the version can be offered again by the next automatic check
    notifiedVersion = null;
    setProgress(-1);
    showDialog({
      type: 'error',
      message: lang.error,
      buttons: [lang.btClose],
    });
  } finally {
    downloadingVersion = null;
  }
}

async function confirmUpdate() {
  if (confirming || downloadingVersion || !availableVersion) {
    return;
  }
  confirming = true;
  let response;
  let version;
  do {
    version = availableVersion;
    notifiedVersion = version;
    // eslint-disable-next-line no-await-in-loop
    ({ response } = await showDialog({
      type: 'warning',
      message: format(lang.confirmUpdate, version),
      detail: lang.confirmUpdateDetail,
      buttons: [lang.btLater, lang.btReleaseNotes, lang.btUpdate],
    }));
    if (response === 1) {
      shell.openExternal(releaseNotesUrl);
    }
    // show the dialog again for release notes or a newer version found meanwhile
  } while (response === 1 || (response === 2 && version !== availableVersion));
  confirming = false;
  if (response !== 2) {
    return;
  }
  if (!canInstall) {
    shell.openExternal('https://gams.com/miro/download.html');
    return;
  }
  downloadingVersion = version;
  download();
}

async function notifyDownloaded(version) {
  setProgress(-1);
  downloadedVersion = version;
  confirming = true;
  const { response } = await showDialog({
    type: 'info',
    message: format(lang.downloaded, version),
    buttons: [lang.btLater, lang.btRestart],
  });
  confirming = false;
  if (response === 1) {
    autoUpdater.quitAndInstall(true, true);
  }
}

function init(configDataObj, langData, getMainWindowFn) {
  configData = configDataObj;
  lang = langData;
  getMainWindow = getMainWindowFn;
  autoUpdater.logger = log;
  autoUpdater.autoDownload = false;
  configureFeed();
  setUpdateChannelFromArgs(process.argv);
  autoUpdater.on('update-available', (info) => {
    availableVersion = info.version;
    if (!manualCheck && notifiedVersion !== info.version) {
      confirmUpdate();
    }
  });
  autoUpdater.on('download-progress', ({ percent }) =>
    setProgress(percent / 100),
  );
  autoUpdater.on('update-downloaded', (info) =>
    notifyDownloaded(info.version),
  );
  autoUpdater.on('error', (err) => log.warn(`Update failed: ${err.message}`));
}

function setAutomaticChecks(enabled) {
  clearInterval(checkTimer);
  checkTimer = null;
  if (!enabled || !app.isPackaged || !configData) {
    return;
  }
  const check = () =>
    autoUpdater.checkForUpdates().catch(() => {
      // logged by the error handler
    });
  check();
  checkTimer = setInterval(check, checkIntervalMs);
}

async function checkForUpdate() {
  if (!app.isPackaged || !configData) {
    log.info('Updates are only available for installed versions of MIRO.');
    return { status: 'error' };
  }
  manualCheck = true;
  try {
    const result = await autoUpdater.checkForUpdates();
    if (!result?.isUpdateAvailable) {
      return { status: 'upToDate' };
    }
    const { version } = result.updateInfo;
    if (version === downloadedVersion) {
      return { status: 'downloaded', version };
    }
    if (downloadingVersion) {
      return { status: 'downloading', version: downloadingVersion };
    }
    notifiedVersion = version;
    return { status: 'available', version };
  } catch (err) {
    return { status: 'error' };
  } finally {
    manualCheck = false;
  }
}

export {
  init,
  setAutomaticChecks,
  setUpdateChannelFromArgs,
  checkForUpdate,
  confirmUpdate,
};
