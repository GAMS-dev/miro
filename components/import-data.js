const {
  dialog,
} = require('electron');
const execa = require('execa');
const path = require('path');
const log = require('electron-log');
const MiroDb = require('./MiroDb');

async function addModelData(paths, modelName, miroMode, miroVersion, miroProcesses, windowObj,
  dataDir, progressEvent = 'add-app-progress') {
  if (!paths.rpath) {
    log.info('No R path set.');
    throw new Error('404');
  }
  let restartRProc;
  const runRProc = async function fRunRProc() {
    restartRProc = false;
    const internalPid = miroProcesses.length;
    const miroProcessesRef = miroProcesses;
    miroProcessesRef[internalPid] = execa(path.join(paths.rpath, 'bin', 'R'),
      ['--no-echo', '--no-restore', '--vanilla', '-f', path.join(paths.miroResourcePath, 'start-shiny.R')],
      {
        env: {
          R_HOME_DIR: paths.rpath,
          RE_SHINY_PATH: paths.miroResourcePath,
          R_LIBS: paths.libPath,
          R_LIBS_USER: paths.libPath,
          R_LIBS_SITE: paths.libPath,
          R_LIB_PATHS: paths.libPath,
          MIRO_NO_DEBUG: 'true',
          MIRO_FORCE_SCEN_IMPORT: 'true',
          MIRO_WS_PATH: paths.miroWorkspaceDir,
          MIRO_DB_PATH: paths.dbpath,
          MIRO_BUILD: 'false',
          MIRO_BUILD_ARCHIVE: 'false',
          MIRO_LOG_PATH: paths.logpath,
          MIRO_POPULATE_DB: 'true',
          LAUNCHINBROWSER: 'true',
          MIRO_REMOTE_EXEC: 'false',
          MIRO_VERSION_STRING: miroVersion,
          MIRO_MODE: miroMode,
          MIRO_MODEL_PATH: path.join(paths.appDir, `${modelName}.gms`),
          MIRO_DATA_DIR: dataDir || '',
        },
        stdout: 'pipe',
        stderr: 'pipe',
        cleanup: false,
      });
    windowObj.setProgressBar(0);
    // eslint-disable-next-line no-restricted-syntax
    for await (const data of miroProcesses[internalPid].stderr) {
      const msg = data.toString();
      log.debug(msg);
      if (msg.startsWith('merr:::')) {
        log.debug(`MIRO error message received: ${msg}`);
        // MIRO error
        const error = msg.trim().split(':::');
        if (error[1] === '409') {
          if (error.length < 3) {
            log.error('MIRO signalled that there are inconsistent tables but no data was provided.');
            throw new Error('merr:::409');
          }
          // split and decode base64 encoded table names
          const datasetsToRemove = error[2].split(',').map((el) => Buffer.from(el, 'base64').toString());
          log.debug(`Datasets to be removed are: '${datasetsToRemove.join("','")}'`);

          const tablesToRemove = datasetsToRemove.map((el) => `${MiroDb.escapeAppId(modelName)}_${el}`);
          log.debug(`Inconsistent tables to be removed are: '${tablesToRemove.join("','")}'`);

          const deleteInconsistentDbTables = dialog.showMessageBoxSync(windowObj, {
            type: 'info',
            title: global.lang.main.ErrorInconsistentDbTablesHdr,
            message: String.format(global.lang.main.ErrorInconsistentDbTablesMsg, datasetsToRemove),
            buttons: [global.lang.main.BtnCancel, global.lang.main.BtnOk],
          }) === 1;
          if (deleteInconsistentDbTables) {
            log.debug('Request to remove inconsistent tables received.');
            try {
              const miroDb = new MiroDb(path.join(paths.dbpath,
                'miro.sqlite3'));
              try {
                miroDb.removeTables(tablesToRemove);
                log.debug('Inconsistent tables removed.');
                restartRProc = true;
              } finally {
                miroDb.close();
              }
            } catch (err) {
              log.error(`Problems removing inconsistent database tables. Error message: ${err.message}`);
              throw err;
            }
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
      await miroProcesses[internalPid];
      windowObj.setProgressBar(-1);
    } catch (err) {
      if (!restartRProc) {
        log.error(`Problems storing data: ${err.toString()}. Stdout: ${err.stdout}, Stderr: ${err.stderr}`);
        throw new Error(err);
      }
    }
  };
  await runRProc();
  if (restartRProc) {
    await runRProc();
  }
}
module.exports = addModelData;
