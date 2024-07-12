import yauzl from 'yauzl';
import path from 'node:path';
import { format } from 'node:util';
import { dialog } from 'electron';
import log from 'electron-log/main.js';
import addModelData from './import-data.js';

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
function addMiroscen(
  miroProcessManager,
  scenFilePath,
  windowObj,
  paths,
  appsData,
) {
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
          miroProcessManager,
          {
            libPath: paths.libPath,
            dbpath: appsData.getAppConfigValue(modelName, 'dbpath'),
            appDir: path.join(paths.appDataPath, modelName),
          },
          modelName,
          miroVersion,
          appsData.getAppConfigValue(modelName, 'usetmpdir'),
          windowObj,
          scenFilePath,
          'loading-screen-progress',
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

export default addMiroscen;
