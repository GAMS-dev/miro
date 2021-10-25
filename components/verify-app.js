const { dialog } = require('electron');
const log = require('electron-log');
const execa = require('execa');
const path = require('path');
const fs = require('fs');
const { format } = require('util');

async function verifyApp(configData, libPath, miroResourcePath, mainWindow, appPath) {
  const rpath = configData.get('rpath');
  const workspacePath = configData.getConfigPath();

  const knownKeysPath = path.join(workspacePath, 'known_keys');

  let publicKeys = [];
  if (fs.existsSync(knownKeysPath)) {
    publicKeys = fs.readdirSync(knownKeysPath, { withFileTypes: true })
      .filter((file) => !file.isDirectory())
      .map((file) => ['-p', path.join(knownKeysPath, file.name)]);
  }

  const showError = (message) => {
    dialog.showMessageBoxSync(mainWindow, {
      type: 'error',
      title: global.lang.main.ErrorInvalidThreeMsg,
      message,
      buttons: [global.lang.main.BtnOk],
    });
  };

  const getFingerprint = (output) => {
    const fingerprintTmp = output.split('\n')
      .filter((line) => line.startsWith('mfprnt:::'))
      .map((line) => line.substring(9));
    if (fingerprintTmp.length > 0) {
      return fingerprintTmp[0];
    }
    return '';
  };

  const verifySignature = async (keys) => {
    const procPromise = execa(
      path.join(await rpath, 'bin', 'R'),
      ['--no-echo', '--no-restore', '--vanilla',
        '-f', path.join(miroResourcePath, 'tools', 'verify_app', 'verify.R'),
        '--args', '-m', path.normalize(appPath), ...keys],
      {
        env: {
          WITHIN_ELECTRON: '1',
          R_HOME_DIR: await rpath,
          R_LIBS: libPath,
          R_LIBS_USER: libPath,
          R_LIBS_SITE: libPath,
          R_LIB_PATHS: libPath,
        },
        cwd: miroResourcePath,
        all: true,
      },
    );
    try {
      const { all, exitCode } = await procPromise;
      log.debug(`Verify-app-signature-process finished with exit code: ${exitCode}. Output: ${all.toString()}`);
      const fingerprint = getFingerprint(all);
      return { exitCode, fingerprint };
    } catch (err) {
      const { all, exitCode } = err;
      log.debug(`Verify-app-signature-process finished with exit code: ${exitCode}. Output: ${all.toString()}`);
      return { exitCode, fingerprint: null };
    }
  };

  try {
    const { exitCode, fingerprint } = await verifySignature(publicKeys.flat(1));
    if (exitCode === 0) {
      log.info(`MIRO app with valid signature found (fingerprint: ${fingerprint}).`);
      return true;
    }
    if (exitCode === 1) {
      // unexpected error
      showError(global.lang.main.ErrorUnexpectedHdr);
      return false;
    }
    if (exitCode === 2) {
      // signature could not be verified
      if (!fs.existsSync(path.join(appPath, '.miro_pubkey'))) {
        log.info('App contains no public key.');
        return false;
      }
      const newKeyOutput = await verifySignature(['-p', path.join(appPath, '.miro_pubkey')]);
      if (newKeyOutput.exitCode !== 0) {
        log.info('MIRO app contains an invalid public key.');
        showError(global.lang.main.ErrorAppInvalidSig);
        return false;
      }
      const addNewKeySelection = dialog.showMessageBoxSync(mainWindow, {
        type: 'warning',
        title: global.lang.general.dialogCustomCodeHdr,
        message: format(global.lang.general.dialogAddPublicKeyMsg, newKeyOutput.fingerprint),
        buttons: [global.lang.general.dialogAddPublicKeyBtnTrust,
          global.lang.general.dialogCustomCodeBtnAbort],
        cancelId: 1,
      });
      if (addNewKeySelection !== 0) {
        return false;
      }
      if (!fs.existsSync(knownKeysPath)) {
        fs.mkdirSync(knownKeysPath);
      }
      const createKeySuffix = () => {
        let keyName = '';
        const chars = 'abcdefghijklmnopqrstuvwxyz0123456789';
        const charLen = chars.length;
        for (let i = 0; i < 15; i += 1) {
          keyName += chars.charAt(Math.floor(Math.random() * charLen));
        }
        return keyName;
      };
      let i = 0;
      // eslint-disable-next-line no-constant-condition
      while (true) {
        const keyNameTmp = path.join(knownKeysPath, path.basename(appPath) + createKeySuffix());
        if (!fs.existsSync(keyNameTmp)) {
          fs.copyFileSync(path.join(appPath, '.miro_pubkey'), keyNameTmp);
          break;
        }
        i += 1;
        if (i > 9) {
          log.warn('Problems storing public key (out of tries).');
          showError(global.lang.main.ErrorUnexpectedHdr);
          return false;
        }
      }
      log.debug(`Added new RSA public key with fingerprint: ${newKeyOutput.fingerprint} to list of trusted keys.`);
      return true;
    }
    if (exitCode === 3) {
      // not signed and app contains custom code
      const addAppSelection = dialog.showMessageBoxSync(mainWindow, {
        type: 'warning',
        title: global.lang.general.dialogCustomCodeHdr,
        message: global.lang.general.dialogCustomCodeMsg,
        buttons: [global.lang.general.dialogCustomCodeBtnAdd,
          global.lang.general.dialogCustomCodeBtnAbort],
        cancelId: 1,
      });
      if (addAppSelection !== 0) {
        return false;
      }
      return true;
    }
    return true;
  } catch (err) {
    log.warn(`Problems validating app. Error message: ${err.message}.`);
    showError(global.lang.main.ErrorUnexpectedHdr);
    return false;
  }
}

module.exports = verifyApp;
