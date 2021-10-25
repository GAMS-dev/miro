const {
  app, BrowserWindow, Menu, TouchBar, ipcMain, dialog, session,
} = require('electron');

const { TouchBarButton, TouchBarSpacer } = TouchBar;
const path = require('path');
const fs = require('fs-extra');
const yauzl = require('yauzl');
const util = require('util');
const log = require('electron-log');
const menu = require('./components/menu');
const installRPackages = require('./components/install-r');

const requiredAPIVersion = 1;
const miroVersion = '2.2.999';
const miroRelease = 'Jan 05 2022';
const libVersion = '2.2';
const exampleAppsData = require('./components/example-apps')(miroVersion, requiredAPIVersion);
const LangParser = require('./components/LangParser');
const addModelData = require('./components/import-data');
const verifyApp = require('./components/verify-app');
const addMiroscen = require('./components/miroscen-parser');
const AppDataStore = require('./components/AppDataStore');
const ConfigManager = require('./components/ConfigManager');
const unzip = util.promisify(require('./components/Unzip'));
const MiroProcessManager = require('./components/MiroProcessManager');
const {
  getAppDbPath,
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
    log.transports.file.resolvePath = () => (path.join(
      logPath,
      'launcher.log',
    ));
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

let applicationMenu;
let rPackagesInstalled = true;
let libPath = isMac && !DEVELOPMENT_MODE
  ? path.resolve(path.join(process.resourcesPath, 'r', 'library'))
  : path.join(appRootDir, 'r', 'library');

const miroResourcePath = DEVELOPMENT_MODE ? path.join(app.getAppPath(), 'src')
  : path.join(process.resourcesPath, 'src');

const miroProcessManager = new MiroProcessManager(
  configData,
  miroDevelopMode,
  miroBuildMode,
  miroResourcePath,
  appDataPath,
);

log.info(`MIRO launcher is being started (rootDir: ${appRootDir}, pid: ${process.pid}, \
platform: ${process.platform}, arch: ${process.arch}, \
version: ${process.getSystemVersion()})...`);

let mainWindow;
let settingsWindow;
let checkForUpdateWindow;
let aboutDialogWindow;
let fileToOpen;
let appLoaded = false;

function showErrorMsg(optionsTmp, windowObj = mainWindow) {
  if (windowObj) {
    const options = optionsTmp;
    if (!options.buttons) {
      options.buttons = ['OK'];
    }
    dialog.showMessageBoxSync(windowObj, options);
  }
}

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

function validateMIROApp(filePathArg, sendToRendererProc = true) {
  log.debug(`Validating new MIRO app (filePath: ${filePathArg.join(',')}).`);
  return new Promise((resolve) => {
    const filePath = filePathArg.filter((el) => el.toLowerCase().endsWith('.miroapp'));
    if (filePath.length === 0) {
      log.error('Validation of MIRO app failed due to invalid file path.');
      showErrorMsg({
        type: 'info',
        title: lang.main.ErrorInvalidHdr,
        message: lang.main.ErrorInvalidMsg,
      });
      resolve(false);
    }
    if (filePath.length > 1) {
      log.error('Validation of MIRO app failed due to invalid file path.');
      showErrorMsg({
        type: 'info',
        title: lang.main.ErrorInvalidHdr,
        message: lang.main.ErrorInvalidTwoMsg,
      });
      resolve(false);
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
        resolve(false);
      };
      if (err) {
        resolve(showZipfileError(err));
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
        resolve(showZipfileError(error));
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
                  resolve(showZipfileError(error));
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
                    showErrorMsg({
                      type: 'error',
                      title: lang.main.ErrorUnexpectedHdr,
                      message: `${lang.main.ErrorReadMsg} '${e.message}'`,
                    });
                    resolve(false);
                  }
                });
              });
            }
            const logoExt = entry.fileName.toLowerCase().match(/.*_logo\.(jpg|jpeg|png)$/);
            if (logoExt) {
              newAppConf.logoPath = entry.fileName;
              log.debug('Logo in new MIRO app found.');
              const logoPathTmp = path.join(app.getPath('temp'), `logo.${logoExt[1]}`);
              zipfile.openReadStream(entry, (error, readStream) => {
                if (error) {
                  resolve(showZipfileError(error));
                }
                readStream.pipe(fs.createWriteStream(logoPathTmp));
                readStream.on('end', () => {
                  newAppConf.logoPathTmp = logoPathTmp;
                  if (mainWindow && sendToRendererProc) {
                    mainWindow.webContents.send('validated-logo-received', { path: logoPathTmp });
                  }
                });
              });
              skipCntAppInfo += 1;
            }
          }
        }
      });
      zipfile.once('end', () => {
        log.debug('New MIRO app extracted successfully.');
        let invalidMiroApp = false;
        const errMsgTemplate = 'The MIRO app you want to add is invalid. Please make sure to upload a valid MIRO app!';
        const miroConfFormat = /(.*)_(\d)_(\d+)_(\d+\.\d+\.\d+)(_hcube)?\.miroconf$/;
        // eslint-disable-next-line no-restricted-syntax
        for (const fileName of appFileNames) {
          if (path.dirname(fileName) === '.' && fileName.endsWith('.miroconf')) {
            const miroConfMatch = fileName.match(miroConfFormat);
            if (miroConfMatch && miroConfMatch[1].length) {
              if (miroConfMatch[5]) {
                log.warn('Hypercube configuration found in app bundle. It will be ignored because the Hypercube Mode is no longer supported as of MIRO 2.2.');
              } else {
                if (newAppConf.modesAvailable.includes('base')) {
                  log.warn('Multiple base configurations found in app bundle. Invalid app.');
                  invalidMiroApp = true;
                  break;
                }
                log.debug('Base mode configuration in new MIRO app found.');
                newAppConf.modesAvailable.push('base');
                newAppConf.usetmpdir = miroConfMatch[2] === '1';
                [newAppConf.path] = filePath;
                [, newAppConf.id, , , newAppConf.miroversion] = miroConfMatch;
                newAppConf.apiversion = parseInt(miroConfMatch[3], 10);
                if (newAppConf.id.startsWith('~$')) {
                  log.warn("App ID starts with illegal characters ('~$'). Invalid app.");
                  invalidMiroApp = true;
                  break;
                }
                log.info(`New MIRO app successfully identified. Id: ${newAppConf.id}, \
  API version: ${newAppConf.apiversion}, \
  MIRO version: ${newAppConf.miroversion}.`);
              }
            } else {
              log.debug(`Invalid MIROconf file found in new MIRO app: ${fileName}.`);
              invalidMiroApp = true;
              break;
            }
          }
        }
        if (mainWindow) {
          mainWindow.setProgressBar(0.9);
        }
        if (!newAppConf.id || invalidMiroApp) {
          if (mainWindow) {
            mainWindow.setProgressBar(-1);
          }
          showErrorMsg({
            type: 'info',
            title: lang.main.ErrorInvalidThreeMsg,
            message: errMsgTemplate,
          });
          resolve(false);
        }
        if (!ConfigManager.vComp(miroVersion, newAppConf.miroversion)) {
          if (mainWindow) {
            mainWindow.setProgressBar(-1);
          }
          showErrorMsg({
            type: 'info',
            title: lang.main.ErrorAPIHdr,
            message: lang.main.ErrorVersionMsg,
          });
          resolve(false);
        }
        if (!newAppConf.apiversion
          || newAppConf.apiversion !== requiredAPIVersion) {
          if (mainWindow) {
            mainWindow.setProgressBar(-1);
          }
          showErrorMsg({
            type: 'info',
            title: lang.main.ErrorAPIHdr,
            message: lang.main.ErrorAPIMsg,
          });
          resolve(false);
        }

        if (mainWindow) {
          mainWindow.setProgressBar(-1);
        }
        if (sendToRendererProc) {
          if (mainWindow) {
            log.debug('New MIRO app configuration sent to renderer process.');
            mainWindow.webContents.send('app-validated', newAppConf);
            resolve(true);
          } else {
            resolve(false);
          }
        } else {
          resolve(newAppConf);
        }
      });
    });
  });
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
  mainWindow.webContents.send(
    'validated-logopath-received',
    { id, path: filteredPath[0] },
  );
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

  fs.copy(
    path.join(miroResourcePath, 'examples'),
    appDataPath,
    (e) => {
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
    },
  );
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
function activateEditMode(openNewAppForm = false, scrollToBottom = false) {
  log.debug(`Activating edit mode. Open 'new app' form: ${openNewAppForm}.`);
  if (mainWindow) {
    mainWindow.send('activate-edit-mode', openNewAppForm, scrollToBottom);
  }
}
async function updateMIROApp(newApp, appIdToUpdate = null) {
  if (newApp === false) {
    if (appIdToUpdate != null) {
      mainWindow.send('add-app-progress', -1, appIdToUpdate);
    }
    log.debug('Error updating app (validation failed).');
    return;
  }
  if (appIdToUpdate != null && appIdToUpdate !== newApp.id) {
    mainWindow.send('add-app-progress', -1, appIdToUpdate);
    log.info('Error updating app (app was dropped on app with different ID).');
    showErrorMsg({
      type: 'info',
      title: lang.main.AppIdConflictHdr,
      message: util.format(lang.main.AppIdConflictMsg, newApp.id, appIdToUpdate),
    });
    return;
  }
  const overwriteData = dialog.showMessageBoxSync(
    mainWindow,
    {
      type: 'info',
      title: lang.main.OverwriteDataHdr,
      message: lang.main.OverwriteDataMsg,
      buttons: [lang.main.BtnCancel, lang.main.OverwriteDataBtnYes, lang.main.OverwriteDataBtnNo],
      cancelId: 0,
    },
  );
  if (overwriteData === 0) {
    mainWindow.send('add-app-progress', -1, newApp.id);
    log.debug('Updating app interrupted.');
    return;
  }
  let appConf;
  try {
    appConf = appsData.getAppConfig(newApp.id);
  } catch (err) {
    log.error('The app to be updated does not exist. This should not happen!');
    mainWindow.send('add-app-progress', -1, newApp.id);
    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
    });
    return;
  }
  mainWindow.send('add-app-progress', 0, newApp.id);
  mainWindow.setProgressBar(0);
  const appDir = path.join(appDataPath, appConf.id);
  const appDirTmp = path.join(appDataPath, `~$${appConf.id}`);
  const appDirTmp2 = path.join(appDataPath, `~$~$${appConf.id}`);
  try {
    [appDirTmp, appDirTmp2].forEach((dirName) => {
      if (fs.existsSync(dirName)) {
        fs.rmSync(dirName, { recursive: true });
      }
    });
  } catch (err) {
    mainWindow.send('add-app-progress', -1, newApp.id);
    mainWindow.setProgressBar(-1);
    log.error(`Problems removing existing temporary app directories. Error message: ${err.message}.`);
    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
    });
    return;
  }
  try {
    await unzip(newApp.path, appDirTmp);

    const appValid = await verifyApp(configData, libPath, miroResourcePath, mainWindow, appDirTmp);
    if (appValid !== true) {
      log.info(`The app: ${newApp.id} could not be validated. Aborting.`);
      throw new Error('suppress');
    }
    appConf.miroversion = newApp.miroversion;
    appConf.usetmpdir = newApp.usetmpdir;
    await addModelData(
      miroProcessManager,
      {
        libPath,
        dbpath: appConf.dbpath,
        appDir: appDirTmp,
      },
      appConf.id,
      appConf.miroversion,
      appConf.usetmpdir,
      mainWindow,
      '',
      'add-app-progress',
      overwriteData === 1,
      true,
    );
  } catch (err) {
    mainWindow.send('add-app-progress', -1, newApp.id);
    mainWindow.setProgressBar(-1);
    try {
      if (fs.existsSync(appDirTmp)) {
        fs.rmSync(appDirTmp, { recursive: true });
      }
    } catch (errRm) {
      log.error(`Problems removing temporary app directory: ${appDirTmp}. Error message: ${errRm.message}.`);
    }
    if (err.message === 'suppress') {
      return;
    }
    log.error(`Update app request failed. Error message: ${err.message}`);
    if (err.code === 'EACCES') {
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
    return;
  }
  try {
    fs.renameSync(appDir, appDirTmp2);
    fs.renameSync(appDirTmp, appDir);
    if (appConf.logoPath != null) {
      if (!fs.existsSync(path.dirname(path.join(appDir, appConf.logoPath)))) {
        fs.mkdirSync(path.dirname(path.join(appDir, appConf.logoPath)));
      }
      fs.copyFileSync(path.join(appDirTmp2, appConf.logoPath), path.join(appDir, appConf.logoPath));
    }
    appsData.updateApp(appConf);
    try {
      const cacheContent = await fs.promises.readdir(path.join(miroWorkspaceDir, 'cache'));
      const removeCacheFilePromises = cacheContent
        .filter((cacheFile) => cacheFile.startsWith(`${newApp.id}_`))
        .forEach((cacheFile) => fs.promises.unlink(path.join(miroWorkspaceDir, 'cache', cacheFile)));
      if (removeCacheFilePromises != null) {
        await Promise.all(removeCacheFilePromises);
      }
    } catch (err) {
      if (err.code !== 'ENOENT') {
        log.error(`Problems removing cache! Error message: '${err.message}'.`);
      }
    }
    const promiseRmTmpDir = fs.promises.rm(appDirTmp2, { recursive: true });
    mainWindow.send('apps-received', appsData.getApps(), appDataPath);
    await promiseRmTmpDir;
  } catch (err) {
    log.error(`Problems replacing app directory. Error message: ${err.message}.`);
    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
    });
    return;
  } finally {
    mainWindow.send('add-app-progress', -1, newApp.id);
    mainWindow.setProgressBar(-1);
  }
}
async function addOrUpdateMIROApp(filePath) {
  const newApp = await validateMIROApp([filePath], false);
  if (newApp === false) {
    log.debug('Error adding/updating app (validation failed).');
    return;
  }
  if (appsData.isUniqueId(newApp.id)) {
    log.debug(`Received MIROAPP file for new MIRO app with ID: ${newApp.id}.`);
    activateEditMode(false, true);
    if (mainWindow) {
      mainWindow.webContents.send('app-validated', newApp);
    }
    return;
  }
  log.debug(`Received MIROAPP file for already existing MIRO app with ID: ${newApp.id}.`);
  await updateMIROApp(newApp);
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
      await addMiroscen(
        miroProcessManager,
        miroscenPath,
        mainWindow,
        {
          libPath,
          appDataPath,
        },
        appsData,
      );
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
    height: 710,
    resizable: DEVELOPMENT_MODE,
    titleBarStyle: process.platform === 'darwin' ? 'hidden' : null,
    show: false,
    frame: false,
    icon: process.platform === 'linux' ? path.join(__dirname, 'static', 'icon_64x64.png') : undefined,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      enableRemoteModule: false,
    },
  });

  settingsWindow.loadFile(path.join(
    __dirname,
    'renderer',
    'settings.html',
  ));

  settingsWindow.once('ready-to-show', async () => {
    log.debug('Settings window ready to show.');
    settingsWindow.webContents.send(
      'settings-loaded',
      await configData.getAll(),
      await configData.getAll(true),
      lang.settings,
    );
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
    icon: process.platform === 'linux' ? path.join(__dirname, 'static', 'icon_64x64.png') : undefined,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      enableRemoteModule: false,
    },
  });
  aboutDialogWindow.loadFile(
    path.join(
      __dirname,
      'renderer',
      'about.html',
    ),
    {
      query: {
        miroVersion,
        miroRelease,
        btClose: lang.update.btClose,
      },
    },
  );
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
    titleBarStyle: process.platform === 'darwin' ? 'hidden' : null,
    show: false,
    frame: false,
    icon: process.platform === 'linux' ? path.join(__dirname, 'static', 'icon_64x64.png') : undefined,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      enableRemoteModule: false,
    },
  });
  checkForUpdateWindow.loadFile(path.join(
    __dirname,
    'renderer',
    'update.html',
  ), { query: { miroVersion } });
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
    app.on('second-instance', async (_, argv) => {
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
          await addOrUpdateMIROApp(associatedFile);
        }
      }
    });
  }
}

function createMainWindow(showRunningApps = false, onSuccess = null) {
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
    titleBarStyle: process.platform === 'darwin' ? 'hidden' : null,
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
      appsActive = miroProcessManager.getActiveApps();
    }
    mainWindow.webContents.send(
      'apps-received',
      appsData.apps,
      appDataPath,
      true,
      true,
      appsActive,
      lang.general,
    );
    log.debug(`App data (${appsData.apps.length} app(s)) loaded into main window.`);
    if (onSuccess) {
      onSuccess();
    }
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
      await addOrUpdateMIROApp(associatedFile);
    } else if (fileToOpen) {
      const associatedFile = fileToOpen;
      if (associatedFile.toLowerCase().endsWith('.miroscen')) {
        log.debug(`MIRO launcher opened by double clicking MIRO scenario file at path: ${associatedFile}.`);
        await addMiroscenFile(associatedFile);
        return;
      }
      log.debug(`MIRO launcher opened by double clicking MIRO app at path: ${associatedFile}.`);
      await addOrUpdateMIROApp(associatedFile);
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
  if (miroProcessManager.getActiveApps().includes(appData.id)) {
    log.info(`MIRO app: ${appData.id} is already running`);
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
    rPackagesInstalled = await installRPackages(
      rpath,
      appRootDir,
      libPath,
      mainWindow,
    );
    return;
  }

  const onErrorStartup = async (appID, e) => {
    log.warn(`Error during startup of MIRO app with ID: ${appID}. Error message: ${e}`);

    if (miroAppWindows[appID]) {
      miroAppWindows[appID].destroy();
    }

    if (mainWindow && !miroDevelopMode) {
      mainWindow.send('hide-loading-screen', appID);
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorUnexpectedHdr,
        message: e.message || lang.main.ErrorUnexpectedMsg,
      });
    }
    if (miroDevelopMode) {
      // in development mode terminate when R process finished
      app.exit(1);
    }
  };

  const onErrorLater = async (appID, e) => {
    log.warn(`App: ${appID} crashed during startup. \
Stdout: ${e.stdout}.\nStderr: ${e.stderr}`);

    if (miroAppWindows[appID]) {
      miroAppWindows[appID].destroy();
    }

    if (miroBuildMode || miroDevelopMode) {
      log.debug(`Exiting with error code: ${e.exitCode}.`);
      app.exit(e.exitCode);
    } else if (mainWindow) {
      mainWindow.send('hide-loading-screen', appID);
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorUnexpectedHdr,
        message: lang.main.ErrorUnexpectedMsg,
      });
    }
  };

  const onSuccess = (url) => {
    if (configData.getSync('launchExternal') === true) {
      log.debug(`MIRO app with ID: ${appData.id} being opened in external browser.`);
      if (mainWindow) {
        mainWindow.send('hide-loading-screen', appData.id, true);
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
      icon: process.platform === 'linux' ? path.join(__dirname, 'static', 'icon_64x64.png') : undefined,
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
    });
    miroAppWindows[appID].once('ready-to-show', () => {
      miroAppWindows[appID].show();
      miroAppWindows[appID].maximize();
      log.debug(`Window for MIRO app with ID: ${appID} created.`);
      if (mainWindow) {
        mainWindow.send('hide-loading-screen', appID, true);
      }
    });
  };

  let cancelTermination = false;

  const onProcessFinished = async (appID) => {
    log.debug(`Process of MIRO app: ${appID} ended.`);
    if (miroDevelopMode || miroBuildMode) {
      // in development mode terminate when R process finished
      if (cancelTermination) {
        cancelTermination = false;
        return;
      }
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
  const onMIROError = async (error) => {
    if (error[1] === '426') {
      log.info(`MIRO signalled that custom packages need to be installed: ${error[3]}`);
      cancelTermination = true;
      mainWindow.show();
      if (dialog.showMessageBoxSync(mainWindow, {
        type: 'info',
        title: lang.main.ErrorCustomPackagesHdr,
        message: util.format(lang.main.ErrorCustomPackagesMsg, error[3], error[2]),
        buttons: [lang.main.BtnCancel, lang.main.BtnOk],
      }) === 1) {
        if (miroDevelopMode) {
          mainWindow.hide();
        }
        log.debug('Installing custom packages');
        try {
          const appDataAgreeInstall = appData;
          if (appDataAgreeInstall.customEnv == null) {
            appDataAgreeInstall.customEnv = {
              MIRO_AGREE_INSTALL_PACKAGES: true,
            };
          } else {
            appDataAgreeInstall.customEnv.MIRO_AGREE_INSTALL_PACKAGES = true;
          }
          await miroProcessManager.createNew(
            appDataAgreeInstall,
            libPath,
            progressCallback,
            onErrorStartup,
            onErrorLater,
            onSuccess,
            onProcessFinished,
          );
        } catch (e) {
          try {
            await onErrorStartup(appData.id, `${lang.main.ErrorMsgLaunch} ${e?.message}.`);
          } catch (err) {
            // continue regardless of error
          }
        }
        return;
      }
      if (miroDevelopMode || miroBuildMode) {
        app.exit(0);
      }
    }
  };
  try {
    await miroProcessManager.createNew(
      appData,
      libPath,
      progressCallback,
      onErrorStartup,
      onErrorLater,
      onSuccess,
      onProcessFinished,
      miroDevelopMode ? onMIROError : null,
    );
  } catch (e) {
    try {
      await onErrorStartup(appData.id, `${lang.main.ErrorMsgLaunch} ${e?.message}.`);
    } catch (err) {
      // continue regardless of error
    }
  }
}

async function searchLibPath(devMode = false) {
  if (process.platform === 'linux') {
    let libPathFiles = [];
    let libsInstalled = true;

    const getExistingLibDirs = async (libRoot) => {
      let existingLibDirs = [];
      try {
        existingLibDirs = (await fs.promises.readdir(libRoot, { withFileTypes: true }))
          .filter((dirent) => dirent.isDirectory())
          .map((dirent) => path.join(libRoot, dirent.name));
      } catch (e) {
        if (e.code !== 'ENOENT') {
          log.error(`Problems reading libPath. Error message: ${e.message}.`);
          showErrorMsg({
            type: 'error',
            title: lang.main.ErrorUnexpectedHdr,
            message: lang.main.ErrorInstallStartMsg,
          });
          return false;
        }
      }
      return existingLibDirs;
    };

    const removeUnusedLibDirs = (existingLibPaths, currentLibPath) => {
      const unusedLibDirs = existingLibPaths
        .filter((existingLibPath) => existingLibPath !== currentLibPath);
      if (unusedLibDirs.length > 0 && dialog.showMessageBoxSync(mainWindow, {
        type: 'info',
        title: lang.main.RemoveUnusedLibDirsHdr,
        message: util.format(lang.main.RemoveUnusedLibDirsMsg, unusedLibDirs.join(',')),
        buttons: [lang.main.RemoveUnusedLibDirsBtnYes, lang.main.RemoveUnusedLibDirsBtnNo],
      }) === 0) {
        unusedLibDirs.forEach((libDirPath) => {
          fs.rm(libDirPath, { recursive: true }).then(() => {
            log.info(`Successfully removed libDir path: ${libDirPath}`);
          }, (error) => {
            log.error(`Problems removing libDir path: ${libDirPath}. Error message: ${error.message}`);
          });
        });
      }
    };

    const existingLibDirs = await getExistingLibDirs(path.dirname(libPath));

    removeUnusedLibDirs(existingLibDirs, libPath);

    if (existingLibDirs.includes(libPath)) {
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
        const existingLibDirsTmp = await getExistingLibDirs(path.dirname(libPathTmp));

        removeUnusedLibDirs(existingLibDirsTmp, libPathTmp);
        if (existingLibDirsTmp.includes(libPathTmp)) {
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
          const installType = (await dialog.showMessageBox(mainWindow, {
            type: 'info',
            title: lang.main.ErrorInstallPermHdr,
            message: `${lang.main.ErrorInstallPerm1Msg} ${libPath}${lang.main.ErrorInstallPerm2Msg} (${libPathTmp})${lang.main.ErrorInstallPerm3Msg}${miroVersion}${lang.main.ErrorInstallPerm4Msg}`,
            buttons: [lang.main.ErrorInstallPermBtnYes, lang.main.ErrorInstallPermBtnNo],
          })).response;
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
          await fs.promises.writeFile(
            path.join(libPath, 'INSTALLING'),
            '',
            'utf8',
          );
        } catch (e) {
          fs.rmSync(libPath);
          log.error(`Could not write INSTALLING metadata file to: ${libPath}.\
     Error message: ${e.message}.`);
          return;
        }
      }
    }
    if (!libsInstalled) {
      try {
        rPackagesInstalled = await installRPackages(
          await configData.get('rpath'),
          appRootDir,
          libPath,
          mainWindow,
          devMode,
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
ipcMain.on('import-miroenv', async () => {
  if (!settingsWindow) {
    return;
  }
  const pathSelected = dialog.showOpenDialogSync(settingsWindow, {
    title: lang.settings.dialogImportEnvHdr,
    message: lang.settings.dialogImportEnvHdr,
    properties: ['openFile'],
    filters: [{ name: 'JSON', extensions: ['json', 'JSON'] }],
  });
  if (!pathSelected) {
    return;
  }
  try {
    const envTxt = await fs.readFile(pathSelected[0]);
    const newEnv = JSON.parse(envTxt);
    configData.validate('miroEnv', newEnv);
    settingsWindow.webContents.send('update-miroEnv', newEnv);
  } catch (err) {
    log.info(`Could not validate environment file. Error message: '${err.message}'`);
    showErrorMsg({
      type: 'info',
      title: lang.settings.ErrorInvalidEnvFileHdr,
      message: util.format(lang.settings.ErrorInvalidEnvFileMsg, err.message),
    }, settingsWindow);
  }
});
ipcMain.on('export-miroenv', async () => {
  const pathSelected = dialog.showSaveDialogSync(settingsWindow, {
    title: lang.settings.dialogExportEnvHdr,
    message: lang.settings.dialogExportEnvHdr,
    defaultPath: 'miro-env.json',
    properties: ['createDirectory', 'showOverwriteConfirmation'],
  });
  if (!pathSelected) {
    return;
  }
  try {
    const currentEnv = await configData.get('miroEnv');
    await fs.writeFile(
      pathSelected,
      JSON.stringify(currentEnv == null ? {} : currentEnv, null, 2),
      'utf-8',
    );
  } catch (err) {
    log.info(`Problems fetching current environment. Error message: '${err.message}'`);
    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: lang.main.ErrorUnexpectedMsg,
    }, settingsWindow);
  }
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
          dialog.showMessageBoxSync(
            settingsWindow,
            {
              type: 'error',
              title: `${idUpper} ${lang.main.ErrorInvalidPathHdr}`,
              message: `${idUpper}${configId === 'r' && process.platform === 'darwin' ? lang.main.ErrorInvalidPathMsgMac
                : lang.main.ErrorInvalidPathMsg} ${ConfigManager.getMinimumVersion(configId)}`,
              buttons: [lang.main.BtnOk],
            },
          );
          return;
        }
      } catch (err) {
        log.error(`Error while validating ${idUpper} version. Error message: ${err.message}`);
        if (settingsWindow) {
          dialog.showMessageBoxSync(
            settingsWindow,
            {
              type: 'error',
              title: lang.main.ErrorUnexpectedHdr,
              message: `${lang.main.ErrorInvalidPathMsg2} ${idUpper} ${lang.main.ErrorMessage} ${err.message}.`,
              buttons: [lang.main.BtnOk],
            },
          );
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
    try {
      if (fs.existsSync(appDir)) {
        log.warn('An existing app directory was found, although the app was no longer registered. The orphaned app directory is removed.');
        fs.rmSync(appDir, { recursive: true });
      }
    } catch (err) {
      mainWindow.send('add-app-progress', -1);
      mainWindow.setProgressBar(-1);
      log.error(`Problems removing existing app directory. Error message: ${err.message}.`);
      showErrorMsg({
        type: 'error',
        title: lang.main.ErrorUnexpectedHdr,
        message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
      });
      return;
    }
    await unzip(appConf.path, appDir);

    const appValid = await verifyApp(configData, libPath, miroResourcePath, mainWindow, appDir);
    if (appValid !== true) {
      log.info(`The app: ${newApp.id} could not be validated. Aborting.`);
      throw new Error('suppress');
    }

    let overwriteData = true;
    const dbPath = path.join(getAppDbPath(appConf.dbpath), `${appConf.id}.sqlite3`);
    if (fs.existsSync(dbPath)) {
      overwriteData = dialog.showMessageBoxSync(
        mainWindow,
        {
          type: 'info',
          title: lang.main.OverwriteDataHdr,
          message: lang.main.OverwriteDataMsg,
          buttons: [lang.main.OverwriteDataBtnYes, lang.main.OverwriteDataBtnNo],
        },
      ) === 0;
    }

    await addModelData(
      miroProcessManager,
      {
        libPath,
        dbpath: appConf.dbpath,
        appDir,
      },
      appConf.id,
      appConf.miroversion,
      appConf.usetmpdir,
      mainWindow,
      '',
      'add-app-progress',
      overwriteData,
    );

    delete appConf.path;
    if (appConf.logoNeedsMove) {
      const newLogoPath = path.join(
        `static_${appConf.id}`,
        `${appConf.id}_logo${path.extname(appConf.logoPath)}`,
      );
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
    if (err.message === 'DuplicatedId') {
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
ipcMain.on('update-app', async (_, filePaths, appIdToUpdate) => {
  log.debug('Update app request received.');
  const newApp = await validateMIROApp(filePaths, false);
  updateMIROApp(newApp, appIdToUpdate);
});
ipcMain.on('update-app-data', async (_, filePaths, appId) => {
  log.debug('Request to update app data received.');
  const overwriteData = dialog.showMessageBoxSync(
    mainWindow,
    {
      type: 'info',
      title: lang.main.OverwriteDataHdr,
      message: lang.main.OverwriteDataMsg,
      buttons: [lang.main.BtnCancel, lang.main.OverwriteDataBtnYes, lang.main.OverwriteDataBtnNo],
      cancelId: 0,
    },
  );
  if (overwriteData === 0) {
    log.debug('Request to add data to app interrupted.');
    mainWindow.send('add-app-progress', -1, appId);
    return;
  }
  let appConf;
  try {
    appConf = appsData.getAppConfig(appId);
  } catch (err) {
    log.error('The app to be updated does not exist. This should not happen!');
    showErrorMsg({
      type: 'error',
      title: lang.main.ErrorUnexpectedHdr,
      message: `${lang.main.ErrorUnexpectedMsg2} '${err.message}'`,
    });
    mainWindow.send('add-app-progress', -1, appId);
    return;
  }
  let progress = 0;
  const progressIncrement = 1 / filePaths.length;
  mainWindow.setProgressBar(progress);
  mainWindow.send('add-app-progress', progress, appId);
  const appDir = path.join(appDataPath, appId);
  for (let i = 0; i < filePaths.length; i += 1) {
    try {
      // eslint-disable-next-line no-await-in-loop
      await addModelData(
        miroProcessManager,
        {
          libPath,
          dbpath: appConf.dbpath,
          appDir,
        },
        appId,
        appConf.miroversion,
        appConf.usetmpdir,
        mainWindow,
        filePaths[i],
        'add-app-progress',
        overwriteData === 1,
        true,
      );
      progress += progressIncrement;
      mainWindow.setProgressBar(progress);
      mainWindow.send('add-app-progress', progress, appId);
    } catch (err) {
      if (err.message !== 'suppress') {
        log.info(`Problems adding MIRO scenario data. Error message: ${err.toString()}.`);
        showErrorMsg({
          type: 'error',
          title: lang.main.ErrorNewScenHdr,
          message: lang.main.ErrorNewScenMsg + err.toString(),
        });
      }
      break;
    }
  }
  mainWindow.setProgressBar(-1);
  mainWindow.send('add-app-progress', -1, appId);
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
ipcMain.on('update-app-meta', (e, updatedApp) => {
  log.debug(`Request to update metadata of app: ${updatedApp.id} received.`);
  try {
    const appConf = updatedApp;
    if (appConf.logoNeedsMove) {
      const newLogoPath = path.join(
        `static_${appConf.id}`,
        `${appConf.id}_logo${path.extname(appConf.logoPath)}`,
      );
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
        if (dialog.showMessageBoxSync(
          settingsWindow,
          {
            type: 'info',
            title: lang.main.SuccessUpdateHdr,
            message: lang.main.SuccessUpdateMsg,
            buttons: [lang.main.BtnCancel, lang.main.BtnOk],
            cancelId: 0,
          },
        ) === 1) {
          app.relaunch();
          app.quit();
        }
      } else {
        settingsWindow.webContents.send(
          'settings-loaded',
          await configData.getAll(),
          await configData.getAll(true),
        );
      }
    }
  } catch (err) {
    log.info(`Save path config request failed. Error message: ${err.message}`);
    if (settingsWindow) {
      dialog.showMessageBoxSync(
        settingsWindow,
        {
          type: 'error',
          title: lang.main.ErrorUnexpectedHdr,
          message: `${lang.main.ErrorUnexpectedWriteMsg} ${configData.getConfigPath()}?`,
          buttons: [lang.main.BtnOk],
        },
      );
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

  try {
    const rmPromise = fs.promises.rm(path.join(appDataPath, appId), { recursive: true });
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
        message: lang.main.ErrorUnexpectedMsg2 + err.message,
      });
    }
  } finally {
    if (deleteAppData) {
      const rmPromises = [];
      rmPromises.push(
        fs.remove(path.join(miroWorkspaceDir, 'app_data', `${appId}.sqlite3`)),
        fs.remove(path.join(miroWorkspaceDir, 'hcube_jobs', appId)),
        fs.remove(path.join(miroWorkspaceDir, `.cred_${appId}`)),
      );
      const deleteAppDataStatus = await Promise.allSettled(rmPromises);
      const errorMsg = deleteAppDataStatus
        .filter((value) => value.status === 'rejected')
        .map((value) => value.reason.toString()).join('\n');
      if (errorMsg) {
        log.error(`Problems removing data (app ID: ${appId}). Error message: ${errorMsg}`);
        showErrorMsg({
          type: 'error',
          title: lang.main.ErrorUnexpectedHdr,
          message: lang.main.ErrorUnexpectedMsg2 + errorMsg,
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
      libPath = path.join(
        rPathTmp,
        'miro-library',
        libVersion,
      );
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
  applicationMenu = menu(
    addExampleApps,
    activateEditMode,
    addMiroscenFile,
    createSettingsWindow,
    openCheckUpdateWindow,
    openAboutDialog,
  );
  Menu.setApplicationMenu(applicationMenu);

  if (miroDevelopMode) {
    mainWindow = new BrowserWindow({
      show: false,
      width: 0,
      height: 0,
      icon: process.platform === 'linux' ? path.join(__dirname, 'static', 'icon_64x64.png') : undefined,
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
    createMainWindow(false, () => searchLibPath());
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
  await miroProcessManager.terminateAll();
  app.exit(0);
});
app.on('activate', () => {
  log.debug('Main window activated.');
  if (mainWindow === null) {
    createMainWindow(true);
  }
});
