const yauzl = require('yauzl');
const path = require('path');
const { format } = require('util');
const { dialog } = require('electron');
const log = require('electron-log');
const addModelData = require('./import-data.js');
const { getAppDbPath } = require('./util');

function miroscenGetModelName(scenFilePath, successCallback, errCallback) {
  yauzl.open(scenFilePath, { lazyEntries: true }, (err, zipfile) => {
    if (err) return errCallback(err);
    zipfile.readEntry();
    zipfile.on('entry', (entry) => {
      if (entry.fileName === 'metadata.json') {
        zipfile.openReadStream(entry, (errOpen, readStream) => {
          if (errOpen) return errCallback(errOpen);
          readStream.on('end', () => {
            zipfile.close();
          });
          const metadataContent = [];
          readStream.on('data', (chunk) => {
            metadataContent.push(chunk.toString());
          });
          readStream.on('end', () => {
            const metadataJSON = JSON.parse(metadataContent.join(''));
            if ('model_raw' in metadataJSON) {
              return successCallback(metadataJSON.model_raw);
            }
            return errCallback(new Error('The selected file is not a valid \
miroscen file as the metadata was incomplete.'));
          });
          return null;
        });
      } else {
        zipfile.readEntry();
      }
    });
    zipfile.on('end', () => errCallback(new Error('The selected file is not a valid \
miroscen file as no metadata was found.')));
    return null;
  });
}
function addMiroscen(scenFilePath, windowObj, paths, appsData, miroProcesses) {
  return new Promise((resolve, reject) => {
    const onSuccess = async (modelName) => {
      let miroVersion;
      try {
        miroVersion = appsData.getAppConfigValue(modelName, 'miroversion');
      } catch (e) {
        reject(format(global.lang.main.ErrorNewScenNoApp, modelName));
        return;
      }
      try {
        await addModelData(
          {
            rpath: paths.rpath,
            libPath: paths.libPath,
            miroResourcePath: paths.miroResourcePath,
            miroWorkspaceDir: paths.miroWorkspaceDir,
            dbpath: getAppDbPath(appsData.getAppConfigValue(modelName, 'dbpath')),
            logpath: paths.logpath,
            appDir: path.join(paths.appDataPath, modelName),
          },
          modelName,
          'base',
          miroVersion,
          miroProcesses, windowObj, scenFilePath, 'loading-screen-progress',
        );
      } catch (err) {
        if (err.message !== 'suppress') {
          log.warn(`Problems parsing miroscen file. Error message: ${err.message};`);
          dialog.showMessageBox(windowObj, {
            type: 'error',
            title: global.lang.main.ErrorNewScenHdr,
            message: global.lang.main.ErrorNewScenMsg + err.message,
            buttons: [global.lang.main.BtnOk],
          });
        }
      }
      resolve(true);
    };
    const onError = (err) => {
      reject(err.message);
    };
    miroscenGetModelName(scenFilePath, onSuccess, onError);
  });
}

module.exports = addMiroscen;
