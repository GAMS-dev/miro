const {
  app, BrowserWindow, Menu, TouchBar, ipcMain, dialog, session,
} = require('electron');

const { TouchBarButton, TouchBarSpacer } = TouchBar;
const path = require('path');
const fs = require('fs-extra');
const yauzl = require('yauzl');
const http = require('axios');
const execa = require('execa');
const util = require('util');
const log = require('electron-log');
const menu = require('./components/menu.js');
const installRPackages = require('./components/install-r.js');

const requiredAPIVersion = 1;
const miroVersion = '1.3.1';
const miroRelease = 'Feb 17 2021';
const libVersion = '1.1';
const exampleAppsData = require('./components/example-apps.js')(miroVersion, requiredAPIVersion);
const LangParser = require('./components/LangParser');
const addModelData = require('./components/import-data');
const addMiroscen = require('./components/miroscen-parser');
const AppDataStore = require('./components/AppDataStore');
const ConfigManager = require('./components/ConfigManager');
const MiroDb = require('./components/MiroDb');
const unzip = util.promisify(require('./components/Unzip'));
const {
  randomPort, waitFor, isNull, kill,
} = require('./components/helpers');
const {
  getAppDbPath,
  isFalse,
} = require('./components/util');

const isMac = process.platform === 'darwin';
const DEVELOPMENT_MODE = !app.isPackaged;
const miroWorkspaceDir = path.join(app.getPath('home'), '.miro');
const miroBuildMode = process.env.MIRO_BUILD === 'true';
const miroDevelopMode = process.env.MIRO_DEV_MODE === 'true' || miroBuildMode;
if (!DEVELOPMENT_MODE) {
  log.transports.console.level = false;
}
(async () => {
  try {
    if (!fs.existsSync(miroWorkspaceDir)) {
      fs.mkdirSync(miroWorkspaceDir);
    }
  } catch (e) {
    log.error('Could not create miro workspace!');
  }
})();
let errMsg;
const appRootDir = DEVELOPMENT_MODE
  ? app.getAppPath() : path.dirname(process.execPath);
const configData = (() => {
  try {
    return new ConfigManager(appRootDir, miroWorkspaceDir);
  } catch (err) {
    errMsg = `Couldn't create configuration file in workspace: ${miroWorkspaceDir}.\
Please make sure you have sufficient permissions and restart MIRO.`;
  }
  return false;
})();

if (!errMsg) {
  (async () => {
    const logPath = await configData.get('logpath');
    if (!fs.existsSync(logPath)) {
      fs.mkdirSync(logPath, { recursive: true });
    }
    log.transports.file.resolvePath = () => (path.join(logPath,
      'launcher.log'));
    log.info(`MIRO launcher (version ${miroVersion} is being started (execPath: ${appRootDir}, \
pid: ${process.pid}, Log path: ${logPath}, \
platform: ${process.platform}, arch: ${process.arch}, \
version: ${process.getSystemVersion()})...`);
  })();
}
const appDataPath = errMsg ? null
  : path.join(configData.getConfigPath(), 'miro_apps');
const appsData = errMsg ? null
  : new AppDataStore(configData.getConfigPath());
const langParser = new LangParser(configData.getSync('language'));

// Set global variables
const lang = langParser.get();
global.lang = lang;
const miroProcesses = [];
const processIdMap = {};

let applicationMenu;
let rPackagesInstalled = true;
let libPath = isMac && !DEVELOPMENT_MODE
  ? path.resolve(path.join(process.resourcesPath, 'r', 'library'))
  : path.join(appRootDir, 'r', 'library');

const miroResourcePath = DEVELOPMENT_MODE ? path.join(app.getAppPath(), 'src')
  : path.join(process.resourcesPath, 'src');

log.info(`MIRO launcher is being started (rootDir: ${appRootDir}, pid: ${process.pid}, \
platform: ${process.platform}, arch: ${process.arch}, \
version: ${process.getSystemVersion()})...`);

let mainWindow;
let settingsWindow;
let checkForUpdateWindow;
let aboutDialogWindow;
let fileToOpen;
let appLoaded = false;

function showErrorMsg(optionsTmp) {
  if (mainWindow) {
    const options = optionsTmp;
    if (!options.buttons) {
      options.buttons = ['OK'];
    }
    dialog.showMessageBoxSync(mainWindow, options);
  }
}

/*
MIT License

Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
const tryStartWebserver = async (progressCallback, onErrorStartup,
  onErrorLater, appData, rpath, onSuccess) => {
  let internalPid = processIdMap[appData.id];

  if (internalPid) {
    log.error('Process for this model already running. This should not happen. Reference not freed.');
    return;
  }
  internalPid = miroProcesses.findIndex(isNull);
  if (internalPid === -1) {
    internalPid = miroProcesses.length;
  }
  log.debug(`Request to start web server with internal pid: ${internalPid} submitted.`);
  processIdMap[appData.id] = internalPid;
  if (miroProcesses[internalPid] != null) {
    await onErrorStartup(appData.id);
    return;
  }
  let shinyPort;
  try {
    shinyPort = await randomPort();
  } catch (e) {
    log.debug(`Process could not be started, as scanning open ports failed with error: ${e.message}`);
    await onErrorStartup(appData.id);
    return;
  }
  log.debug(`Process: ${internalPid} is being started on port: ${shinyPort}.`);
  const gamspath = configData.get('gamspath');
  const logpath = configData.get('logpath');
  const dbPath = getAppDbPath(appData.dbpath);

  const generalConfig = {
    launchExternal: configData.get('launchExternal'),
    remoteExecution: configData.get('remoteExecution'),
    language: configData.get('language'),
    logLevel: configData.get('logLevel'),
  };
  await progressCallback({ internalPid, code: 'start' });

  const onError = async (e) => {
    if (miroProcesses[internalPid] === null) {
      return;
    }
    log.error(`Process: ${internalPid} crashed during startup. Stdout: ${e.stdout}.\nStderr: ${e.stderr}`);
    miroProcesses[internalPid] = null;
    delete processIdMap[appData.id];
    if (miroBuildMode || miroDevelopMode) {
      log.debug(`Exiting with error code: ${e.exitCode}.`);
      app.exit(e.exitCode);
    } else if (mainWindow) {
      mainWindow.send('hide-loading-screen', appData.id);
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorUnexpectedHdr,
        message: lang.main.ErrorUnexpectedMsg,
      });
    }
  };
  log.info(`MIRO app: ${appData.id} launched at port: ${shinyPort} with dbPath: ${dbPath},\
developMode: ${miroDevelopMode}.`);
  let shinyProcessAlreadyDead = false;
  let noError = false;
  miroProcesses[internalPid] = execa(path.join(rpath, 'bin', 'R'),
    ['--no-echo', '--no-restore', '--vanilla', '-f', path.join(miroResourcePath, 'start-shiny.R')],
    {
      env: {
        WITHIN_ELECTRON: '1',
        R_HOME_DIR: rpath,
        RE_SHINY_PORT: shinyPort,
        RE_SHINY_PATH: miroResourcePath,
        R_LIBS: libPath,
        R_LIBS_USER: libPath,
        R_LIBS_SITE: libPath,
        R_LIB_PATHS: libPath,
        MIRO_NO_DEBUG: !miroDevelopMode,
        MIRO_FORCE_SCEN_IMPORT: miroDevelopMode && appData.forceScenImport,
        MIRO_USE_TMP: !isFalse(appData.usetmpdir) || appData.mode === 'hcube',
        MIRO_WS_PATH: miroWorkspaceDir,
        MIRO_DB_PATH: dbPath,
        MIRO_BUILD: miroBuildMode,
        MIRO_BUILD_ARCHIVE: appData.buildArchive === true,
        GAMS_SYS_DIR: await gamspath,
        MIRO_LOG_PATH: await logpath,
        LAUNCHINBROWSER: await generalConfig.launchExternal,
        MIRO_REMOTE_EXEC: await generalConfig.remoteExecution,
        MIRO_LANG: await generalConfig.language,
        MIRO_LOG_LEVEL: await generalConfig.logLevel,
        MIRO_VERSION_STRING: appData.miroversion,
        MIRO_MODE: appData.mode ? appData.mode : 'base',
        MIRO_MODEL_PATH: miroDevelopMode ? appData.modelPath
          : path.join(appDataPath, appData.id, `${appData.id}.gms`),
      },
      stdout: miroDevelopMode ? 'inherit' : 'pipe',
      stderr: miroDevelopMode ? 'inherit' : 'pipe',
      cleanup: false,
    });
  miroProcesses[internalPid].catch((e) => {
    shinyProcessAlreadyDead = true;
    onError(e);
  }).then(async () => {
    if (shinyProcessAlreadyDead) {
      return;
    }
    shinyProcessAlreadyDead = true;
    noError = true;
    if (miroBuildMode) {
      app.exit(0);
    }
  });
  const url = `http://127.0.0.1:${shinyPort}`;
  await waitFor(1500);
  /* eslint-disable no-await-in-loop */
  for (let i = 0; i <= 50; i += 1) {
    if (shinyProcessAlreadyDead) {
      if (noError) {
        return;
      }
      break;
    }
    await waitFor(Math.min(i * 100, 1000));
    try {
      const res = await http.head(`${url}/shared/shiny.css`, { timeout: 10000 });
      if (res.status === 200) {
        await progressCallback({ code: 'success', port: shinyPort });
        onSuccess(url);
        return;
      }
    } catch (e) {
      if (i > 10) {
        log.debug(`Process: ${internalPid} not responding after ${i + 1} seconds.`);
        await progressCallback({ code: 'notresponding' });
      }
    }
  }
  /* eslint-enable no-await-in-loop */
  await onErrorStartup(appData.id);
};

function hideZoomMenu() {
  if (!applicationMenu) {
    return;
  }
  const editMenuId = isMac ? 3 : 2;
  [1, 2, 3].forEach((i) => {
    applicationMenu.items[editMenuId].submenu.items[i].enabled = false;
    applicationMenu.items[editMenuId].submenu.items[i].visible = false;
  });
}

let newAppConf;

function validateMIROApp(filePathArg) {
  log.debug(`Validating new MIRO app (filePath: ${filePathArg.join(',')}).`);
  const filePath = filePathArg.filter((el) => el.toLowerCase().endsWith('.miroapp'));
  if (filePath.length === 0) {
    log.error('Validation of MIRO app failed due to invalid file path.');
    return showErrorMsg({
      type: 'info',
      title: lang.main.ErrorInvalidHdr,
      message: lang.main.ErrorInvalidMsg,
    });
  } if (filePath.length > 1) {
    log.error('Validation of MIRO app failed due to invalid file path.');
    return showErrorMsg({
      type: 'info',
      title: lang.main.ErrorInvalidHdr,
      message: lang.main.ErrorInvalidTwoMsg,
    });
  }
  yauzl.open(filePath[0], (err, zipfile) => {
    const showZipfileError = (e) => {
      log.debug(`Problems extracting and validating new MIRO app. Error message: ${e.message}`);
      if (mainWindow) {
        mainWindow.setProgressBar(-1);
      }
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorUnexpectedHdr,
        message: `${lang.main.ErrorReadMsg} '${e.message}'`,
      });
    };
    if (err) {
      return showZipfileError(err);
    }
    const appFileNames = [];
    const incAmt = 0.8 / zipfile.entryCount;
    let fileCnt = 0;
    let skipCntAppInfo = 0;
    newAppConf = {
      modesAvailable: [],
      usetmpdir: true,
    };
    zipfile.on('error', (error) => {
      log.error(`MIRO app could not be extracted. Error message: ${error.message}.`);
      return showZipfileError(error);
    });
    zipfile.on('entry', (entry) => {
      if (!mainWindow) {
        zipfile.close();
      }
      fileCnt += 1;
      mainWindow.setProgressBar(fileCnt * incAmt);
      appFileNames.push(entry.fileName);
      if (skipCntAppInfo < 2) {
        if (path.dirname(entry.fileName).startsWith('static_')) {
          if (path.basename(entry.fileName.toLowerCase()) === 'app_info.json') {
            log.debug('App info file in new MIRO app found.');
            skipCntAppInfo += 1;
            zipfile.openReadStream(entry, (error, readStream) => {
              if (error) {
                return showZipfileError(error);
              }
              const appInfoData = [];
              readStream.on('data', (chunk) => {
                appInfoData.push(chunk);
              });
              readStream.on('end', () => {
                try {
                  const appInfo = JSON.parse(Buffer
                    .concat(appInfoData)
                    .toString('utf8'));
                  newAppConf.title = appInfo.title;
                  newAppConf.description = appInfo.description;
                } catch (e) {
                  if (e instanceof SyntaxError) {
                    log.debug(`Invalid JSON syntax in app info file. File will be ignored. Error message: ${e.message}`);
                  } else {
                    log.warn(`Unexpected error occurred while reading app info file. Error message: ${e.message}`);
                  }
                }
              });
              return null;
            });
            return;
          }
          const logoExt = entry.fileName.toLowerCase().match(/.*_logo\.(jpg|jpeg|png)$/);
          if (logoExt) {
            newAppConf.logoPath = entry.fileName;
            log.debug('Logo in new MIRO app found.');
            const logoPathTmp = path.join(app.getPath('temp'), `logo.${logoExt[1]}`);
            zipfile.openReadStream(entry, (error, readStream) => {
              if (error) {
                return showZipfileError(error);
              }
              readStream.pipe(fs.createWriteStream(logoPathTmp));
              readStream.on('end', () => {
                newAppConf.logoPathTmp = logoPathTmp;
                if (mainWindow) {
                  mainWindow.webContents.send('validated-logo-received', { path: logoPathTmp });
                }
              });
              return null;
            });
            skipCntAppInfo += 1;
          }
        }
      }
    });
    zipfile.once('end', () => {
      log.debug('New MIRO app extracted successfully.');
      if (!mainWindow) {
        return;
      }
      let invalidMiroApp = false;
      const errMsgTemplate = 'The MIRO app you want to add is invalid. Please make sure to upload a valid MIRO app!';
      const miroConfFormat = /(.*)_(\d)_(\d+)_(\d+\.\d+\.\d+)(_hcube)?\.miroconf$/;
      let skipCntMiroconf = 0;
      // eslint-disable-next-line no-restricted-syntax
      for (const fileName of appFileNames) {
        if (skipCntMiroconf > 1) {
          break;
        }
        if (path.dirname(fileName) === '.' && fileName.endsWith('.miroconf')) {
          const miroConfMatch = fileName.match(miroConfFormat);
          if (miroConfMatch && miroConfMatch[1].length) {
            if (miroConfMatch[5]) {
              if (newAppConf.modesAvailable.includes('hcube')) {
                log.warn('Multiple Hypercube configurations found in app bundle. Invalid app.');
                invalidMiroApp = true;
                break;
              }
              log.debug('Hypercube configuration in new MIRO app found.');
              newAppConf.modesAvailable.push('hcube');
            } else {
              if (newAppConf.modesAvailable.includes('base')) {
                log.warn('Multiple base configurations found in app bundle. Invalid app.');
                invalidMiroApp = true;
                break;
              }
              log.debug('Base mode configuration in new MIRO app found.');
              newAppConf.modesAvailable.push('base');
              newAppConf.usetmpdir = miroConfMatch[2] === '1';
            }
            if (!newAppConf.id) {
              [newAppConf.path] = filePath;
              [, newAppConf.id,,, newAppConf.miroversion] = miroConfMatch;
              newAppConf.apiversion = parseInt(miroConfMatch[3], 10);
              log.info(`New MIRO app successfully identified. Id: ${newAppConf.id}, \
  API version: ${newAppConf.apiversion}, \
  MIRO version: ${newAppConf.miroversion}.`);
            }
            skipCntMiroconf += 1;
          } else {
            log.debug(`Invalid MIROconf file found in new MIRO app: ${fileName}.`);
            invalidMiroApp = true;
            break;
          }
        }
      }
      mainWindow.setProgressBar(0.9);
      if (!mainWindow) {
        return;
      }
      if (!newAppConf.id || invalidMiroApp) {
        mainWindow.setProgressBar(-1);
        showErrorMsg({
          type: 'info',
          title: lang.main.ErrorInvalidThreeMsg,
          message: errMsgTemplate,
        });
        return;
      }
      if (!ConfigManager.vComp(miroVersion, newAppConf.miroversion)) {
        mainWindow.setProgressBar(-1);
        showErrorMsg({
          type: 'info',
          title: lang.main.ErrorAPIHdr,
          message: lang.main.ErrorVersionMsg,
        });
        return;
      }
      if (!newAppConf.apiversion
          || newAppConf.apiversion !== requiredAPIVersion) {
        mainWindow.setProgressBar(-1);
        showErrorMsg({
          type: 'info',
          title: lang.main.ErrorAPIHdr,
          message: lang.main.ErrorAPIMsg,
        });
        return;
      }
      if (!mainWindow) {
        return;
      }

      mainWindow.setProgressBar(-1);
      if (mainWindow) {
        log.debug('New MIRO app configuration sent to renderer process.');
        mainWindow.webContents.send('app-validated', newAppConf);
      }
    });
    return null;
  });
  return null;
}

function validateAppLogo(filePath, id = null) {
  log.debug(`Request to validate MIRO app logo received (file path: ${filePath}, id: ${id}).`);
  const filteredPath = filePath.filter((el) => el
    .toLowerCase()
    .match(/\.(jpg|jpeg|png)$/));
  if (filteredPath.length === 0) {
    log.info('App logo not valid due to bad format.');
    showErrorMsg({
      type: 'info',
      title: lang.main.ErrorLogoHdr,
      message: lang.main.ErrorLogoMsg,
    });
    return;
  } if (filteredPath.length > 1) {
    log.info('App logo not valid due to multiple files being dropped.');
    showErrorMsg({
      type: 'info',
      title: lang.main.ErrorLogoHdr,
      message: lang.main.ErrorLogoMultiMsg,
    });
    return;
  }
  const logoSize = fs.statSync(filteredPath[0]).size / 1000000.0;
  if (logoSize > 10) {
    log.info(`App logo not valid due to file size being too large (${logoSize}MB)`);
    showErrorMsg({
      type: 'info',
      title: lang.main.ErrorLogoLargeHdr,
      message: lang.main.ErrorLogoLargeMsg,
    });
    return;
  }
  log.info('MIRO app logo successfully validate.');
  mainWindow.webContents.send('validated-logopath-received',
    { id, path: filteredPath[0] });
}
function addExampleApps() {
  const examplesToAdd = exampleAppsData
    .filter((exampleApp) => appsData.isUniqueId(exampleApp.id));
  const examplesToAddNames = examplesToAdd.map((exampleApp) => exampleApp.id);
  const examplesSkipped = exampleAppsData
    .filter((exampleApp) => !examplesToAddNames.includes(exampleApp.id))
    .map((exampleApp) => exampleApp.id);
  if (examplesToAddNames.length === 0) {
    log.debug('All example models already exist. Nothing was added.');
    return showErrorMsg({
      type: 'info',
      title: lang.main.ErrorExampleExistsHdr,
      message: `${lang.main.ErrorModelExistsMsg} ${examplesSkipped.toString()}`,
    });
  }

  fs.copy(path.join(miroResourcePath, 'examples'),
    appDataPath, (e) => {
      if (e) {
        log.error(`Unexpected error while copying example apps from: \
${path.join(miroResourcePath, 'examples')} to: ${appDataPath}. Error mesage: ${e.message}`);
        if (e.code === 'EACCES') {
          showErrorMsg({
            type: 'error',
            title: lang.main.ErrorWriteHdr,
            message: `${lang.main.ErrorWriteMsg} '${appDataPath}.'`,
          });
          return;
        }
        showErrorMsg({
          type: 'error',
          title: lang.main.ErrorUnexpectedHdr,
          message: `${lang.main.ErrorUnexpectedMsg2} '${e.message}'`,
        });
        return;
      }
      try {
        examplesToAdd.forEach((exampleApp) => {
          appsData.addApp(exampleApp);
        });
        const updatedApps = appsData.getApps();
        mainWindow.send('apps-received', updatedApps, appDataPath);
      } catch (err) {
        log.error(`Problems writing app data: \
    ${path.join(miroResourcePath, 'examples')} to: ${appDataPath}. Error mesage: ${err.message}`);
        if (err.code === 'EACCES') {
          showErrorMsg({
            type: 'error',
            title: lang.main.ErrorWriteHdr,
            message: `${lang.main.ErrorWriteMsg} '${configData.getConfigPath()}.'`,
          });
          return;
        }
        showErrorMsg({
          type: 'error',
          title: lang.main.ErrorUnexpectedHdr,
          message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
        });
      }
    });
  log.debug(`Example models: ${examplesToAddNames.toString()} added to library.`);
  if (examplesSkipped.length) {
    return showErrorMsg({
      type: 'info',
      title: lang.main.ErrorExampleExistsHdr,
      message: `${lang.main.ErrorModelExistsMsg} ${examplesSkipped.toString()}`,
    });
  }
  return null;
}
async function addMiroscenFile(filePath) {
  let miroscenPath = filePath;
  if (!miroscenPath) {
    miroscenPath = dialog.showOpenDialogSync(mainWindow, {
      title: lang.dialogNewScenFilesHdr,
      message: lang.dialogNewScenFilesMsg,
      buttonLabel: lang.dialogNewScenFilesBtn,
      properties: ['openFile'],
      filters: [
        { name: lang.dialogNewScenFilesFilter, extensions: ['miroscen'] },
      ],
    });
    if (!miroscenPath || miroscenPath.length === 0) {
      return;
    }
    [miroscenPath] = miroscenPath;
  }
  if (miroscenPath) {
    if (!miroscenPath.toLowerCase().endsWith('.miroscen')) {
      log.debug('Incorrect file type discovered when trying to add MIRO scenario.');
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorNewScenHdr,
        message: `${lang.main.ErrorNewScenMsg}Incorrect file type`,
      });
      return;
    }
    mainWindow.send('toggle-loading-screen-progress', 'show');
    try {
      await addMiroscen(miroscenPath, mainWindow, {
        rpath: await configData.get('rpath'),
        libPath,
        miroResourcePath,
        miroWorkspaceDir,
        logpath: await configData.get('logpath'),
        appDataPath,
      }, appsData, miroProcesses);
    } catch (e) {
      log.info(`Problems adding MIRO scenario. Error message: ${e.toString()}.`);
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorNewScenHdr,
        message: lang.main.ErrorNewScenMsg + e.toString(),
      });
    } finally {
      mainWindow.setProgressBar(-1);
      mainWindow.send('toggle-loading-screen-progress', 'hide');
    }
  }
}
function activateEditMode(openNewAppForm = false, scrollToBottom = false) {
  log.debug(`Activating edit mode. Open 'new app' form: ${openNewAppForm}.`);
  if (mainWindow) {
    mainWindow.send('activate-edit-mode', openNewAppForm, scrollToBottom);
  }
}

const btAddApp = new TouchBarButton({
  label: lang.menu.addApp,
  backgroundColor: '#F39619',
  click: () => {
    log.debug('Add new MIRO app button clicked on TouchBar.');
    activateEditMode(true, true);
  },
});
const btManageApps = new TouchBarButton({
  label: lang.menu.editApp,
  click: () => {
    log.debug('Edit apps button clicked on TouchBar.');
    activateEditMode();
  },
});
const btAddMiroscen = new TouchBarButton({
  label: lang.menu.addMiroScen,
  backgroundColor: '#F39619',
  click: async () => {
    log.debug('Add new MIRO scenario button clicked on TouchBar.');
    await addMiroscenFile();
  },
});
const mainWindowTouchBar = new TouchBar({
  items: [
    btAddApp,
    btManageApps,
    new TouchBarSpacer({ size: 'large' }),
    btAddMiroscen,
  ],
});
const dockMenu = Menu.buildFromTemplate([
  {
    label: lang.menu.addApp,
    click: () => {
      log.debug('Add new MIRO app button clicked in dock menu.');
      activateEditMode(true, true);
    },
  },
  {
    label: lang.menu.editApp,
    click: () => {
      log.debug('Edit apps button clicked in dock menu.');
      activateEditMode();
    },
  },
  {
    label: lang.menu.addMiroScen,
    click: async () => {
      log.debug('Add new MIRO scenario button clicked in dock menu.');
      await addMiroscenFile();
    },
  },
]);

function createSettingsWindow() {
  log.debug('Creating settings window..');
  if (settingsWindow) {
    log.debug('Settings window already open.');
    settingsWindow.show();
    return;
  }
  settingsWindow = new BrowserWindow({
    title: lang.settings.title,
    width: 570,
    height: 640,
    resizable: DEVELOPMENT_MODE,
    titleBarStyle: 'hidden',
    show: false,
    frame: false,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      enableRemoteModule: false,
    },
  });

  settingsWindow.loadFile(path.join(__dirname,
    'renderer', 'settings.html'));

  settingsWindow.once('ready-to-show', async () => {
    log.debug('Settings window ready to show.');
    settingsWindow.webContents.send('settings-loaded',
      await configData.getAll(),
      await configData.getAll(true), lang.settings);
    log.debug('Settings window settings loaded.');
    settingsWindow.show();
  });
  if (DEVELOPMENT_MODE) {
    settingsWindow.webContents.openDevTools();
  }
  settingsWindow.on('page-title-updated', (e) => {
    e.preventDefault();
  });

  settingsWindow.on('closed', () => {
    log.debug('Settings window closed.');
    settingsWindow = null;
  });
}
function openAboutDialog() {
  log.debug('Creating about dialog window..');
  if (aboutDialogWindow) {
    log.debug('About dialog already open.');
    aboutDialogWindow.show();
    return;
  }
  aboutDialogWindow = new BrowserWindow({
    title: 'About GAMS MIRO',
    width: 600,
    height: 380,
    resizable: false,
    show: false,
    frame: false,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      enableRemoteModule: false,
    },
  });
  aboutDialogWindow.loadFile(path.join(__dirname,
    'renderer', 'about.html'),
  {
    query: {
      miroVersion,
      miroRelease,
      btClose: lang.update.btClose,
    },
  });
  aboutDialogWindow.once('ready-to-show', async () => {
    log.debug('About dialog ready to show.');
    aboutDialogWindow.show();
  });
  aboutDialogWindow.on('page-title-updated', (e) => {
    e.preventDefault();
  });
  aboutDialogWindow.on('closed', () => {
    log.debug('About dialog closed.');
    aboutDialogWindow = null;
  });
}
function openCheckUpdateWindow() {
  log.debug('Creating Check for Update window..');
  if (checkForUpdateWindow) {
    log.debug('Check for Update window already open.');
    checkForUpdateWindow.show();
    return;
  }
  checkForUpdateWindow = new BrowserWindow({
    title: lang.update.title,
    width: 400,
    height: 200,
    resizable: false,
    titleBarStyle: 'hidden',
    show: false,
    frame: false,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      enableRemoteModule: false,
    },
  });
  checkForUpdateWindow.loadFile(path.join(__dirname,
    'renderer', 'update.html'), { query: { miroVersion } });
  checkForUpdateWindow.once('ready-to-show', async () => {
    log.debug('Check for Update window ready to show.');
    checkForUpdateWindow.send('lang-data-received', lang.update);
    checkForUpdateWindow.show();
  });
  checkForUpdateWindow.on('page-title-updated', (e) => {
    e.preventDefault();
  });

  checkForUpdateWindow.on('closed', () => {
    log.debug('Check for Update window closed.');
    checkForUpdateWindow = null;
  });
}
async function terminateProcesses() {
  const termPromises = miroProcesses.map((miroProcess) => {
    if (!miroProcess) {
      return { pid: null };
    }
    const { pid } = miroProcess;
    return kill(pid);
  });
  for (let i = 0; i < miroProcesses.length; i += 1) {
    miroProcesses[i] = null;
  }

  const resolvedTermPromises = await Promise.allSettled(termPromises);
  resolvedTermPromises.forEach((termStatus) => {
    if (termStatus.status === 'rejected') {
      log.debug(`Problems killing R process.\
Error message: ${termStatus.reason.message}.`);
    } else if (termStatus.value.pid) {
      log.debug(`R process with pid: ${termStatus.value.pid} successfully terminated.`);
    }
  });
}
function quitLauncher() {
  if (process.platform !== 'darwin') {
    app.quit();
  }
}
if (!miroDevelopMode) {
  const gotTheLock = app.requestSingleInstanceLock();

  if (!gotTheLock) {
    app.quit();
  } else {
    app.on('second-instance', async (event, argv) => {
      log.debug('Second MIRO instance launched.');
      if (mainWindow) {
        if (mainWindow.isMinimized()) mainWindow.restore();
        mainWindow.focus();
        if (process.platform === 'win32'
          && argv.length >= 2 && !DEVELOPMENT_MODE && !miroDevelopMode) {
          const associatedFile = argv[argv.length - 1];
          if (associatedFile.startsWith('--')) {
            return;
          }
          if (associatedFile.toLowerCase().endsWith('.miroscen')) {
            log.debug(`MIRO launcher opened by double clicking MIRO scenario file at path: ${associatedFile}.`);
            await addMiroscenFile(associatedFile);
            return;
          }
          log.debug(`MIRO launcher opened by double clicking MIRO app at path: ${associatedFile}.`);
          activateEditMode(false, true);
          validateMIROApp([associatedFile]);
        }
      }
    });
  }
}

function createMainWindow(showRunningApps = false) {
  log.debug('Creating main window..');
  if (mainWindow) {
    log.debug('Main window already open.');
    mainWindow.show();
    return;
  }
  mainWindow = new BrowserWindow({
    title: 'GAMS MIRO',
    width: 900,
    height: 750,
    minWidth: 800,
    minHeight: 600,
    titleBarStyle: 'hidden',
    icon: process.platform === 'linux' ? path.join(__dirname, 'static', 'icon_64x64.png') : undefined,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      enableRemoteModule: false,
    },
  });
  mainWindow.loadFile(path.join(__dirname, 'renderer', 'index.html'), { query: { appPath: app.getAppPath() } });
  mainWindow.once('ready-to-show', () => {
    log.debug('Main window ready to show.');
    mainWindow.show();
  });
  if (DEVELOPMENT_MODE) {
    mainWindow.webContents.openDevTools();
  }
  mainWindow.webContents.on('did-finish-load', async () => {
    let appsActive = [];
    if (showRunningApps) {
      appsActive = Object.keys(processIdMap);
    }
    mainWindow.webContents.send('apps-received',
      appsData.apps, appDataPath, true, true, appsActive, lang.general);
    log.debug(`App data (${appsData.apps.length} app(s)) loaded into main window.`);
    if (appLoaded || miroDevelopMode) {
      return;
    }
    appLoaded = true;
    if (process.platform === 'win32'
      && process.argv.length >= 2 && !DEVELOPMENT_MODE) {
      const associatedFile = process.argv[1];
      if (associatedFile.toLowerCase().endsWith('.miroscen')) {
        log.debug(`MIRO launcher opened by double clicking MIRO scenario file at path: ${associatedFile}.`);
        await addMiroscenFile(associatedFile);
        return;
      }
      log.debug(`MIRO launcher opened by double clicking MIRO app at path: ${associatedFile}.`);
      activateEditMode(false, true);
      validateMIROApp([associatedFile]);
    } else if (fileToOpen) {
      const associatedFile = fileToOpen;
      if (associatedFile.toLowerCase().endsWith('.miroscen')) {
        log.debug(`MIRO launcher opened by double clicking MIRO scenario file at path: ${associatedFile}.`);
        await addMiroscenFile(associatedFile);
        return;
      }
      activateEditMode(false, true);
      log.debug(`MIRO launcher opened by double clicking MIRO app at path: ${associatedFile}.`);
      validateMIROApp([associatedFile]);
    }
  });
  mainWindow.setTouchBar(mainWindowTouchBar);
  if (isMac) {
    app.dock.setMenu(dockMenu);
  }
  mainWindow.on('page-title-updated', (e) => {
    e.preventDefault();
  });
  mainWindow.on('focus', () => {
    if (!applicationMenu) {
      return;
    }
    const editMenuId = isMac ? 2 : 1;
    [0, 1, 2].forEach((i) => {
      applicationMenu.items[editMenuId].submenu.items[i].visible = true;
    });
  });
  mainWindow.on('blur', () => {
    if (!applicationMenu) {
      return;
    }
    const editMenuId = isMac ? 2 : 1;
    [0, 1, 2].forEach((i) => {
      applicationMenu.items[editMenuId].submenu.items[i].visible = false;
    });
  });
  mainWindow.on('closed', () => {
    log.debug('Main window closed.');
    mainWindow = null;
    quitLauncher();
  });
}

const miroAppWindows = [];

async function createMIROAppWindow(appData) {
  log.debug(`Request to launch MIRO app with id: ${appData.id} received.`);
  const progressCallback = async (event) => {
    log.info(event);
  };
  if (processIdMap[appData.id] != null) {
    log.info(`Process with internal pid: ${processIdMap[appData.id]} already running for MIRO app: ${appData.id}`);
    mainWindow.send('hide-loading-screen', appData.id);
    showErrorMsg({
      type: 'info',
      title: lang.main.ErrorAppRunningHdr,
      message: lang.main.ErrorAppRunningMsg,
    });
    return;
  }
  const rpath = await configData.get('rpath');
  if (!rpath) {
    log.info('No R path set.');
    if (miroDevelopMode) {
      showErrorMsg({
        type: 'info',
        title: lang.main.ErrorRNotFoundHdr,
        message: lang.main.ErrorRNotFoundMsg,
      });
      app.exit(1);
    } else {
      mainWindow.send('hide-loading-screen', appData.id);
      mainWindow.send('invalid-r');
    }
    return;
  }
  if (!appData.apiversion
    || parseInt(appData.apiversion, 10) !== requiredAPIVersion) {
    log.info(`MIRO app: ${appData.id} has API version: ${appData.apiversion} \
and is incompatible with MIRO version installed which requires API version: \
${requiredAPIVersion}.`);
    mainWindow.send('hide-loading-screen', appData.id);
    showErrorMsg({
      type: 'info',
      title: lang.main.ErrorAPIHdr,
      message: lang.main.ErrorAppIncompMsg,
    });
    return;
  }
  if (process.platform === 'linux' && rPackagesInstalled !== true) {
    log.info('MIRO app launch requested without packages being installed.');
    mainWindow.send('hide-loading-screen', appData.id);
    rPackagesInstalled = await installRPackages(rpath, appRootDir,
      libPath, mainWindow);
    return;
  }

  const onErrorLater = async (appID) => {
    log.debug(`Error after launching MIRO app with ID: ${appData.id}.`);
    if (!miroAppWindows[appID]) {
      return;
    }
    if (mainWindow) {
      mainWindow.send('hide-loading-screen', appData.id);
      showErrorMsg({
        type: 'error',
        title: 'Unexpected error',
        message: 'The MIRO app could not be started. Please report to GAMS when this problem persists!',
      });
    }
    miroProcesses[processIdMap[appID]] = null;
    delete processIdMap[appID];
    if (miroAppWindows[appID]) {
      miroAppWindows[appID].destroy();
      miroAppWindows[appID] = null;
    }

    if (miroDevelopMode) {
      // in development mode terminate when R process finished
      app.exit(1);
    }
  };

  const onErrorStartup = async (appID, message) => {
    log.debug(`Error during startup of MIRO app with ID: ${appData.id}. \
${message ? `Message: ${message}` : ''}`);

    try {
      await kill(miroProcesses[processIdMap[appID]].pid);
      miroProcesses[processIdMap[appID]] = null;
      delete processIdMap[appID];
    } catch (e) {
      // continue regardless of error
    }

    if (mainWindow && !miroDevelopMode) {
      mainWindow.send('hide-loading-screen', appData.id);
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorUnexpectedHdr,
        message: message || lang.main.ErrorUnexpectedMsg,
      });
    }
    if (miroDevelopMode) {
      // in development mode terminate when R process finished
      app.exit(1);
    }
  };

  const onProcessFinished = async (appID) => {
    const internalPid = processIdMap[appID];
    if (!Number.isInteger(internalPid) || !miroProcesses[internalPid]) {
      return;
    }
    if (miroProcesses[internalPid]) {
      try {
        await miroProcesses[internalPid];
      } catch (e) {
        if (e.signal !== 'SIGTERM') {
          log.error(`Problems while waiting for process of MIRO app with ID: ${appID}\
  to finish. Error message: ${e.message}`);
        }
      }
      log.debug(`Process of MIRO app with ID: ${appID} and internal pid ${internalPid} ended.`);
      miroProcesses[processIdMap[appID]] = null;
      delete processIdMap[appID];
    }
    if (miroDevelopMode) {
      // in development mode terminate when R process finished
      app.exit(0);
      return;
    }
    if (mainWindow) {
      mainWindow.send('app-closed', appID);
    }
    if (miroAppWindows[appID]) {
      miroAppWindows[appID].destroy();
      miroAppWindows[appID] = null;
    }
  };
  try {
    await tryStartWebserver(progressCallback, onErrorStartup,
      onErrorLater, appData, rpath, (url) => {
        if (configData.getSync('launchExternal') === true) {
          log.debug(`MIRO app with ID: ${appData.id} being opened in external browser.`);
          if (mainWindow) {
            mainWindow.send('hide-loading-screen', appData.id, true);
            onProcessFinished(appData.id);
          }
          return;
        }
        const appID = appData.id;
        log.debug(`MIRO app with ID: ${appID} being opened in launcher.`);
        miroAppWindows[appID] = new BrowserWindow({
          width: 800,
          height: 600,
          minWidth: 800,
          minHeight: 600,
          show: false,
          webPreferences: {
            nodeIntegration: false,
            contextIsolation: true,
          },
        });

        miroAppWindows[appID].loadURL(url, { extraHeaders: 'pragma: no-cache\n' });

        miroAppWindows[appID].on('focus', () => {
          if (!applicationMenu) {
            return;
          }
          const editMenuId = isMac ? 3 : 2;
          [1, 2, 3].forEach((i) => {
            applicationMenu.items[editMenuId].submenu.items[i].enabled = true;
            applicationMenu.items[editMenuId].submenu.items[i].visible = true;
          });
        });

        miroAppWindows[appID].on('blur', () => {
          hideZoomMenu();
        });

        miroAppWindows[appID].on('close', (e) => {
          e.preventDefault();
          log.debug(`Window of MIRO app with ID: ${appID} closed.`);
          hideZoomMenu();
          miroAppWindows[appID].destroy();
        });

        miroAppWindows[appID].on('closed', async () => {
          try {
            mainWindow.send('app-closed', appID);
          } catch (e) {
          // continue regardless of error
          }
          const internalPid = processIdMap[appID];
          if (Number.isInteger(internalPid)) {
            const { pid } = miroProcesses[internalPid];
            miroProcesses[internalPid] = null;
            try {
              await kill(pid);
              log.debug(`R process with pid: ${pid} killed.`);
            } catch (e) {
              log.debug(`Problems killing R process with pid: ${pid}. Error message: ${e.message}`);
            }
          }
          delete processIdMap[appID];
          miroAppWindows[appID] = null;
        });
        miroAppWindows[appID].once('ready-to-show', () => {
          miroAppWindows[appID].show();
          miroAppWindows[appID].maximize();
          log.debug(`Window for MIRO app with ID: ${appID} created.`);
          if (mainWindow) {
            mainWindow.send('hide-loading-screen', appID, true);
            onProcessFinished(appID);
          }
        });
      });
  } catch (e) {
    try {
      await onErrorStartup(appData.id, `${lang.main.ErrorMsgLaunch} ${e.message}.`);
    } catch (err) {
      // continue regardless of error
    }
  }
}

async function searchLibPath(devMode = false) {
  if (process.platform === 'linux') {
    let libPathFiles = [];
    let libsInstalled = true;
    if (fs.existsSync(libPath)) {
      try {
        libPathFiles = await fs.promises.readdir(libPath);
        if (libPathFiles.find((item) => item === 'INSTALLING')) {
          libsInstalled = false;
        }
      } catch (e) {
        log.error(`Problems reading libPath. Error message: ${e.message}.`);
        showErrorMsg({
          type: 'error',
          title: lang.main.ErrorUnexpectedHdr,
          message: lang.main.ErrorInstallStartMsg,
        });
        return;
      }
    } else {
      libsInstalled = false;
      try {
        await fs.promises.mkdir(libPath, { recursive: true });
      } catch (e) {
        const libPathTmp = path.join(app.getPath('appData'), 'miro-library', libVersion);
        if (fs.existsSync(libPathTmp)) {
          try {
            libPathFiles = await fs.promises.readdir(libPathTmp);
            if (libPathFiles.find((item) => item === 'INSTALLING')) {
              libsInstalled = false;
            } else {
              libsInstalled = true;
              libPath = libPathTmp;
              log.debug(`Libpath set to: ${libPath}`);
            }
          } catch (err) {
            log.error(`Problems reading libPath. Error message: ${err.message}.`);
            showErrorMsg({
              type: 'error',
              title: lang.main.ErrorUnexpectedHdr,
              message: lang.main.ErrorInstallStartMsg,
            });
            return;
          }
        } else {
          libsInstalled = false;
        }
        if (!libsInstalled) {
          const installType = dialog.showMessageBoxSync(mainWindow, {
            type: 'info',
            title: lang.main.ErrorInstallPermHdr,
            message: `${lang.main.ErrorInstallPerm1Msg} ${libPath}${lang.main.ErrorInstallPerm2Msg} (${libPathTmp})${lang.main.ErrorInstallPerm3Msg}${miroVersion}${lang.main.ErrorInstallPerm4Msg}`,
            buttons: [lang.main.ErrorInstallPermBtnYes, lang.main.ErrorInstallPermBtnNo],
          });
          if (installType === 1) {
            app.exit(0);
            return;
          }
          try {
            await fs.promises.mkdir(libPathTmp, { recursive: true });
          } catch (err) {
            log.error(`Problems creating libPath: ${libPathTmp}. Error message: ${err.message}.`);
            showErrorMsg({
              type: 'error',
              title: lang.main.ErrorUnexpectedHdr,
              message: lang.main.ErrorInstallStartMsg,
            });
            return;
          }
          libPath = libPathTmp;
          log.debug(`Libpath set to: ${libPath}`);
        }
      }
      if (!libsInstalled) {
        try {
          await fs.promises.writeFile(path.join(libPath, 'INSTALLING'),
            '', 'utf8');
        } catch (e) {
          fs.rmdirSync(libPath);
          log.error(`Could not write INSTALLING metadata file to: ${libPath}.\
     Error message: ${e.message}.`);
          return;
        }
      }
    }
    if (!libsInstalled) {
      try {
        rPackagesInstalled = await installRPackages(
          await configData.get('rpath'), appRootDir,
          libPath, mainWindow, devMode,
        );
      } catch (e) {
        log.error(`Problems creating prompt to install R packages. \
  Error message: ${e.message}.`);
      }
    }
  }
}
ipcMain.on('show-error-msg', (e, options) => {
  log.debug(`New error message received. Title: ${options.title}, message: ${options.message}.`);
  showErrorMsg(options);
});
ipcMain.on('settings-select-new-path', async (e, id, defaultPath) => {
  if (settingsWindow) {
    const langData = {
      configpath: {
        title: lang.settings.dialogConfigPathHdr,
        message: lang.settings.dialogConfigPathMsg,
        buttonLabel: lang.settings.dialogConfigPathBtn,
        label: lang.settings.dialogConfigPathLabel,
      },
      gamspath: {
        title: lang.settings.dialogGamsPathHdr,
        message: lang.settings.dialogGamsPathMsg,
        label: lang.settings.dialogGamsPathLabel,
        buttonLabel: lang.settings.dialogGamsPathBtn,
      },
      rpath: {
        title: lang.settings.dialogRPathHdr,
        message: lang.settings.dialogRPathMsg,
        label: lang.settings.dialogRPathLabel,
        buttonLabel: lang.settings.dialogRPathBtn,
      },
      logpath: {
        title: lang.settings.dialogLogPathHdr,
        message: lang.settings.dialogLogPathMsg,
        label: lang.settings.dialogLogPathLabel,
        buttonLabel: lang.settings.dialogLogPathBtn,
      },
    };
    const pathSelected = dialog.showOpenDialogSync(settingsWindow, {
      title: langData[id].title,
      message: langData[id].message,
      buttonLabel: langData[id].buttonLabel,
      defaultPath,
      properties: ['openDirectory', 'createDirectory'],
    });
    if (!pathSelected) {
      return;
    }
    if (id === 'gamspath' || id === 'rpath') {
      let configId;
      if (id === 'gamspath') {
        configId = 'gams';
      } else {
        configId = 'r';
      }
      const idUpper = configId.toUpperCase();

      log.debug(`Request to validate ${idUpper} path at location: ${pathSelected[0]} received.`);

      try {
        const validatedPath = await ConfigManager.validate(configId, pathSelected[0]);
        if (validatedPath !== false && validatedPath != null && settingsWindow) {
          log.debug(`${idUpper} path is valid!`);
          settingsWindow.webContents.send('settings-new-path-selected', id, validatedPath);
        } else {
          log.debug(`${idUpper} path is invalid!`);
          dialog.showMessageBoxSync(settingsWindow,
            {
              type: 'error',
              title: `${idUpper} ${lang.main.ErrorInvalidPathHdr}`,
              message: `${idUpper}${configId === 'r' && process.platform === 'darwin' ? lang.main.ErrorInvalidPathMsgMac
                : lang.main.ErrorInvalidPathMsg} ${ConfigManager.getMinimumVersion(configId)}`,
              buttons: [lang.main.BtnOk],
            });
          return;
        }
      } catch (err) {
        log.error(`Error while validating ${idUpper} version. Error message: ${err.message}`);
        if (settingsWindow) {
          dialog.showMessageBoxSync(settingsWindow,
            {
              type: 'error',
              title: lang.main.ErrorUnexpectedHdr,
              message: `${lang.main.ErrorInvalidPathMsg2} ${idUpper} ${lang.main.ErrorMessage} ${err.message}.`,
              buttons: [lang.main.BtnOk],
            });
        }
      }
    } else {
      settingsWindow.webContents.send('settings-new-path-selected', id, pathSelected[0]);
    }
  }
});

ipcMain.on('browse-app', (e, options, callback, id = null) => {
  const filePaths = dialog.showOpenDialogSync(mainWindow, options);
  if (filePaths) {
    if (callback === 'validateLogo') {
      validateAppLogo(filePaths, id);
    } else if (callback === 'validateApp') {
      validateMIROApp(filePaths);
    } else {
      e.reply(callback, { id, path: filePaths });
    }
  }
});

ipcMain.on('add-app', async (e, newApp) => {
  log.debug('Add app request received.');
  const appConf = newApp;
  const appDir = path.join(appDataPath, appConf.id);
  try {
    if (!appsData.isUniqueId(newApp.id)) {
      throw new Error('DuplicatedId');
    }
    await unzip(appConf.path, appDir);

    await addModelData(
      {
        rpath: await configData.get('rpath'),
        libPath,
        miroResourcePath,
        miroWorkspaceDir,
        dbpath: getAppDbPath(appConf.dbpath),
        logpath: await configData.get('logpath'),
        appDir,
      },
      appConf.id,
      appConf.modesAvailable.includes('base') ? 'base' : 'hcube',
      appConf.miroversion,
      appConf.usetmpdir,
      miroProcesses, mainWindow,
    );

    delete appConf.path;
    if (appConf.logoNeedsMove) {
      const newLogoPath = path.join(`static_${appConf.id}`,
        `${appConf.id}_logo${path.extname(appConf.logoPath)}`);
      if (!fs.existsSync(path.dirname(path.join(appDir, newLogoPath)))) {
        fs.mkdirSync(path.dirname(path.join(appDir, newLogoPath)));
      }
      fs.copyFileSync(appConf.logoPath, path.join(appDir, newLogoPath));
      appConf.logoPath = newLogoPath;
      delete appConf.logoNeedsMove;
    }
    const updatedApps = appsData.addApp(appConf).apps;
    mainWindow.send('apps-received', updatedApps, appDataPath);
  } catch (err) {
    mainWindow.send('add-app-progress', -1);
    mainWindow.setProgressBar(-1);
    try {
      if (fs.existsSync(appDir)) {
        fs.removeSync(appDir);
      }
    } catch (errRm) {
      log.error(`Problems removing app directory: ${appDir}. Error message: ${errRm.message}.`);
    }
    if (err.message === 'suppress') {
      return;
    }
    log.error(`Add app request failed. Error message: ${err.message}`);
    if (err.message === '404') {
      dialog.showMessageBox(mainWindow, {
        type: 'info',
        title: lang.main.ErrorRNotFoundHdr,
        message: lang.main.ErrorRNotFoundMsg,
        buttons: [lang.main.BtnOk],
      });
      return;
    } if (err.message === 'DuplicatedId') {
      showErrorMsg({
        type: 'info',
        title: lang.main.ErrorModelExistsHdr,
        message: lang.main.ErrorModelExistsMsg2,
      });
      return;
    } if (err.code === 'EACCES') {
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorWriteHdr,
        message: `${lang.main.ErrorWritePerm2Msg} '${configData.getConfigPath()}.'`,
      });
      return;
    }

    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
    });
  }
});
ipcMain.on('add-example-apps', () => {
  log.debug('Received request to add example apps.');
  addExampleApps();
});
ipcMain.on('update-apps', (e, apps) => {
  try {
    appsData.updateApps(apps);
  } catch (err) {
    log.error(`Update apps request failed. Error message: ${err.message}`);
    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
    });
  }
});
ipcMain.on('update-app', (e, updatedApp) => {
  log.debug('Update app request received.');
  try {
    const appConf = updatedApp;
    if (appConf.logoNeedsMove) {
      const newLogoPath = path.join(`static_${appConf.id}`,
        `${appConf.id}_logo${path.extname(appConf.logoPath)}`);
      const newLogoPathFull = path.join(appDataPath, appConf.id, newLogoPath);
      if (!fs.existsSync(path.dirname(newLogoPathFull))) {
        fs.mkdirSync(path.dirname(newLogoPathFull));
      }
      fs.copyFileSync(appConf.logoPath, newLogoPathFull);
      appConf.logoPath = newLogoPath;
      delete appConf.logoNeedsMove;
    }
    const updatedApps = appsData.updateApp(appConf).apps;
    mainWindow.send('apps-received', updatedApps, appDataPath);
  } catch (err) {
    log.error(`Update app request failed. Error message: ${err.message}`);
    if (err.code === 'EACCES') {
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorWriteHdr,
        message: `${lang.main.ErrorNoWritePermMsg} '${configData.getConfigPath()}.'`,
      });
      return;
    }
    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
    });
  }
});

ipcMain.on('save-general-config', async (e, newConfigData, needRestart) => {
  log.debug('Save general config request received.');
  try {
    configData.set(newConfigData);
    if (settingsWindow) {
      if (needRestart === true) {
        if (dialog.showMessageBoxSync(settingsWindow,
          {
            type: 'info',
            title: lang.main.SuccessUpdateHdr,
            message: lang.main.SuccessUpdateMsg,
            buttons: [lang.main.BtnCancel, lang.main.BtnOk],
            cancelId: 0,
          }) === 1) {
          app.relaunch();
          app.quit();
        }
      } else {
        settingsWindow.webContents.send('settings-loaded',
          await configData.getAll(),
          await configData.getAll(true));
      }
    }
  } catch (err) {
    log.info(`Save path config request failed. Error message: ${err.message}`);
    if (settingsWindow) {
      dialog.showMessageBoxSync(settingsWindow,
        {
          type: 'error',
          title: lang.main.ErrorUnexpectedHdr,
          message: `${lang.main.ErrorUnexpectedWriteMsg} ${configData.getConfigPath()}?`,
          buttons: [lang.main.BtnOk],
        });
    }
  }
});
ipcMain.on('validate-app', (e, filePath) => {
  validateMIROApp(filePath);
});
ipcMain.on('validate-logo', (e, filePath, id) => {
  validateAppLogo(filePath, id);
});
ipcMain.on('delete-app', async (e, appId) => {
  log.debug(`Delete app (ID: ${appId}) request received`);
  const deleteAppConfirmedId = dialog.showMessageBoxSync(mainWindow, {
    buttons: [lang.main.BtnCancel, lang.main.BtnRemove],
    cancelId: 0,
    message: lang.main.DeleteMsg,
  });
  if (deleteAppConfirmedId !== 1) {
    return;
  }
  const deleteAppData = dialog.showMessageBoxSync(mainWindow, {
    buttons: [lang.main.BtnCancel, lang.main.BtnRemove],
    cancelId: 0,
    message: lang.main.DeleteDataMsg,
  }) === 1;

  let appDbPath;
  if (deleteAppData) {
    appDbPath = appsData.getAppConfigValue(appId, 'dbPath');
  }
  try {
    const rmPromise = fs.remove(path.join(appDataPath, appId));
    try {
      const cacheContent = await fs.promises.readdir(path.join(miroWorkspaceDir, 'cache'));
      const removeCacheFilePromises = cacheContent
        .filter((cacheFile) => cacheFile.startsWith(`${appId}_`))
        .forEach((cacheFile) => fs.promises.unlink(path.join(miroWorkspaceDir, 'cache', cacheFile)));
      if (removeCacheFilePromises != null) {
        await Promise.all(removeCacheFilePromises);
      }
    } catch (err) {
      if (err.code !== 'ENOENT') {
        log.error(`Problems removing cache! Error message: '${err.message}'.`);
      }
    }
    fs.remove(path.join(appDataPath, appId));
    const updatedApps = appsData.deleteApp(appId).apps;
    await rmPromise;
    mainWindow.send('apps-received', updatedApps, appDataPath, false, false);
    log.debug(`App: ${appId} removed.`);
  } catch (err) {
    log.error(`Delete app (ID: ${appId}) request failed. Error message: ${err.message}`);
    if (err.code === 'EACCES') {
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorWriteHdr,
        message: `${lang.main.ErrorWriteMsg2} '${configData.getConfigPath()}.'`,
      });
    } else {
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorUnexpectedHdr,
        message: lang.main.ErrorUnexpectedMsg2,
      });
    }
  } finally {
    if (deleteAppData) {
      try {
        const miroDb = new MiroDb(path.join(getAppDbPath(appDbPath),
          'miro.sqlite3'));
        try {
          miroDb.removeAppDbTables(appId);
        } finally {
          miroDb.close();
        }
        const rmPromiseHcube = fs.remove(path.join(miroWorkspaceDir, 'hcube_jobs', appId));
        const rmPromiseCred = fs.remove(path.join(miroWorkspaceDir, `.cred_${appId}`));
        await rmPromiseHcube;
        await rmPromiseCred;
      } catch (err) {
        log.error(`Problems removing data (app ID: ${appId}). Error message: ${err.message}`);
        showErrorMsg({
          type: 'error',
          title: lang.main.ErrorUnexpectedHdr,
          message: lang.main.ErrorUnexpectedMsg2,
        });
      }
    }
  }
});

ipcMain.on('launch-app', (e, appData) => {
  createMIROAppWindow(appData);
});

ipcMain.on('close-window', (e, id) => {
  if (id === 'about' && aboutDialogWindow) {
    aboutDialogWindow.close();
  } else if (id === 'settings' && settingsWindow) {
    settingsWindow.close();
  } else if (id === 'update' && checkForUpdateWindow) {
    checkForUpdateWindow.close();
  }
});

app.on('will-finish-launching', () => {
  app.on('open-file', async (e, filePath) => {
    e.preventDefault();
    if (appLoaded) {
      const associatedFile = filePath;
      if (associatedFile.toLowerCase().endsWith('.miroscen')) {
        log.debug(`MIRO scenario file at path: ${associatedFile} opened.`);
        await addMiroscenFile(associatedFile);
        return;
      }
      log.debug(`MIRO application file at path: ${associatedFile} opened.`);
      activateEditMode(false, true);
      validateMIROApp([associatedFile]);
      return;
    }
    fileToOpen = filePath;
  });
});

app.on('ready', async () => {
  if (process.platform === 'linux') {
    try {
      const rPathTmp = await configData.get('rpath');
      if (!rPathTmp) {
        throw new Error(`R${lang.main.ErrorInvalidPathMsg} ${ConfigManager.getMinimumVersion('r')}`);
      }
      libPath = path.join(rPathTmp,
        'miro-library', libVersion);
      log.debug(`Lib path set to: ${libPath}`);
    } catch (err) {
      errMsg = `Couldn't retrieve R path. Error message: ${err.message}.`;
    }
  }

  if (errMsg) {
    dialog.showMessageBoxSync({
      type: 'error',
      title: lang.main.ErrorInit,
      message: errMsg,
      buttons: [lang.main.BtnOk],
    });
    app.quit();
    return;
  }
  configData.removeOldLogs();
  session.defaultSession.webRequest.onHeadersReceived((_, callback) => {
    callback({
      responseHeaders: `
        default-src 'none';
        script-src 'self';
        img-src 'self' data:;
        style-src 'self';
        font-src 'self';
      `,
    });
  });

  // Deny all permission requests
  session.defaultSession.setPermissionRequestHandler((_1, _2, callback) => {
    callback(false);
  });
  applicationMenu = menu(addExampleApps,
    activateEditMode,
    addMiroscenFile,
    createSettingsWindow,
    openCheckUpdateWindow,
    openAboutDialog);
  Menu.setApplicationMenu(applicationMenu);

  if (miroDevelopMode) {
    mainWindow = new BrowserWindow({
      show: false,
      width: 0,
      height: 0,
      webPreferences: {
        nodeIntegration: true,
        contextIsolation: false,
        enableRemoteModule: false,
      },
    });
    mainWindow.hide();
    const modelPath = process.env.MIRO_MODEL_PATH;
    await searchLibPath(true);
    if (!rPackagesInstalled) {
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorRInstallHdr,
        message: lang.main.ErrorRInstallMsg,
      });
      app.exit(1);
      return;
    }
    if (!modelPath) {
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorModelPathHdr,
        message: lang.main.ErrorModelPathMsg,
      });
      app.exit(1);
      return;
    }
    createMIROAppWindow({
      id: path.basename(modelPath, 'gms'),
      modelPath,
      mode: process.env.MIRO_MODE,
      usetmpdir: process.env.MIRO_USE_TMP ? process.env.MIRO_USE_TMP === 'true' : false,
      apiversion: requiredAPIVersion,
      miroversion: miroVersion,
      forceScenImport: process.env.MIRO_FORCE_SCEN_IMPORT === 'true',
      buildArchive: process.env.MIRO_BUILD_ARCHIVE !== 'false',
    });
  } else {
    createMainWindow();
    searchLibPath();
  }

  log.info('MIRO launcher started successfully.');
});

app.on('window-all-closed', () => {
  log.debug('All windows closed.');
  quitLauncher();
});
app.on('will-quit', async (e) => {
  e.preventDefault();
  log.debug('Terminating potentially open R processes.');
  await terminateProcesses();
  app.exit(0);
});
app.on('activate', () => {
  log.debug('Main window activated.');
  if (mainWindow === null) {
    createMainWindow(true);
  }
});
