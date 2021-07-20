const {
  dialog, BrowserWindow,
} = require('electron');
const path = require('path');
const { format } = require('util');
const log = require('electron-log');

async function addModelData(miroProcessManager, paths, modelName,
  miroMode, miroVersion, usetmpdir,
  windowObj, dataDir, progressEvent = 'add-app-progress') {
  let restartRProc;
  let overwriteData = false;
  let confirmInstallPackages = false;
  let migrationWizardWindow;
  if (progressEvent === 'add-app-progress') {
    overwriteData = true;
  }
  const runRProc = async function fRunRProc() {
    restartRProc = false;
    const appId = modelName.toLowerCase();
    const procPid = await miroProcessManager.createNewMiroProc({
      id: appId,
      miroversion: miroVersion,
      mode: miroMode,
      usetmpdir,
      dbpath: paths.dbpath,
      allowMultiple: true,
      timeout: 3600, // ~1 hour maximum time for installing packages/importing data
      customEnv: {
        MIRO_NO_DEBUG: 'true',
        MIRO_FORCE_SCEN_IMPORT: 'true',
        MIRO_BUILD: 'false',
        MIRO_BUILD_ARCHIVE: 'false',
        MIRO_OVERWRITE_SCEN_IMPORT: overwriteData,
        MIRO_AGREE_INSTALL_PACKAGES: confirmInstallPackages,
        MIRO_POPULATE_DB: 'true',
        LAUNCHINBROWSER: 'false',
        MIRO_REMOTE_EXEC: 'false',
        MIRO_MODEL_PATH: path.join(paths.appDir, `${modelName}.gms`),
        MIRO_DATA_DIR: dataDir || '',
      },
      stdOut: 'pipe',
      stdErr: 'pipe',
    }, paths.libPath);

    if (procPid == null) {
      log.error('Unknown error while storing data.');
      throw new Error();
    }

    const miroProc = miroProcessManager.getProc(procPid);

    windowObj.setProgressBar(0);

    const openMigrationWizard = (url) => {
      log.debug(`Database migration wizard for app: ${modelName.toLowerCase()} being opened in launcher.`);
      migrationWizardWindow = new BrowserWindow({
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
      migrationWizardWindow.loadURL(url, { extraHeaders: 'pragma: no-cache\n' });
      migrationWizardWindow.on('close', (e) => {
        e.preventDefault();
        log.debug(`Database migration wizard for app: ${modelName.toLowerCase()} closed.`);
        migrationWizardWindow.destroy();
        migrationWizardWindow = null;
      });
      migrationWizardWindow.once('ready-to-show', () => {
        migrationWizardWindow.show();
        migrationWizardWindow.maximize();
        log.debug(`Window for database migration wizard for app: ${modelName.toLowerCase()} created.`);
      });
    };

    // eslint-disable-next-line no-restricted-syntax
    for await (const data of miroProc.stderr) {
      const msg = data.toString().trim();
      log.debug(msg);
      if (msg.startsWith('merr:::')) {
        log.debug(`MIRO error message received: ${msg}`);
        // MIRO error
        const error = msg.trim().split(':::');
        if (error[1] === '409') {
          log.debug('MIRO signalled that database needs to be migrated. Waiting for user to migrate database.');
          miroProcessManager.waitForResponse(appId, true, procPid,
            async (event) => {
              log.info(event);
            }, null, openMigrationWizard);
          restartRProc = true;
        } else if (error[1] === '418') {
          log.info('MIRO signalled that the scenario already exists.');
          if (dialog.showMessageBoxSync(windowObj, {
            type: 'info',
            title: global.lang.main.ErrorDataImportHdr,
            message: format(global.lang.main.ErrorDataImportMsg, error[2]),
            buttons: [global.lang.main.BtnCancel, global.lang.main.BtnOverwrite],
          }) === 1) {
            log.debug('Overwriting scenario was confirmed');
            overwriteData = true;
            restartRProc = true;
          } else {
            throw new Error('suppress');
          }
        } else if (error[1] === '426') {
          log.info(`MIRO signalled that custom packages need to be installed: ${error[3]}`);
          if (dialog.showMessageBoxSync(windowObj, {
            type: 'info',
            title: global.lang.main.ErrorCustomPackagesHdr,
            message: format(global.lang.main.ErrorCustomPackagesMsg, error[3], error[2]),
            buttons: [global.lang.main.BtnCancel, global.lang.main.BtnOk],
          }) === 1) {
            log.debug('Installing custom packages');
            confirmInstallPackages = true;
            restartRProc = true;
          } else {
            throw new Error('suppress');
          }
        } else {
          throw new Error(msg);
        }
      } else if (msg.startsWith('mprog:::')) {
        // MIRO progress
        const progress = parseInt(msg.substring(8), 10);
        if (!Number.isNaN(progress)) {
          windowObj.setProgressBar(progress >= 100 ? -1 : progress / 100);
        }
        if (progressEvent) {
          windowObj.send(progressEvent, progress);
        }
      }
    }
    try {
      await miroProc;
      windowObj.setProgressBar(-1);
    } catch (err) {
      if (restartRProc) {
        log.debug('Migration process was interrupted.');
        throw new Error('suppress');
      } else {
        log.error(`Problems storing data: ${err.toString()}. Stdout: ${err.stdout}, Stderr: ${err.stderr}`);
        throw new Error(err);
      }
    } finally {
      if (migrationWizardWindow) {
        migrationWizardWindow.destroy();
        migrationWizardWindow = null;
      }
    }
  };
  let restartCount = 0;
  do {
    restartRProc = false;
    // eslint-disable-next-line no-await-in-loop
    await runRProc();
    restartCount += 1;
  } while (restartRProc && restartCount < 3);
}
module.exports = addModelData;
