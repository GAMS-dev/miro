import { dialog, ipcMain } from 'electron';
import log from 'electron-log/main.js';
import { execa } from 'execa';
import path from 'node:path';

async function installRPackages(rpath, apppath, libpath, mainWindow, devMode = false) {
  if (!rpath) {
    await dialog.showMessageBox(mainWindow, {
      type: 'error',
      title: 'R not found',
      message: 'No R installation was found on your machine. Please install R or specify the location of your existing R installation in the Preferences window (F7).',
      buttons: ['OK'],
    });
    return false;
  }
  const selection = (await dialog.showMessageBox(mainWindow, {
    type: 'question',
    title: 'Install R packages',
    message: 'Would you like to install the required R packages now?\n\nNote that before doing so, you have to install the system libraries libcurl and libpng.\nOn Debian / Ubuntu you need libcurl4-gnutls-dev and libpng-dev, on Fedora use libcurl-devel and libpng-devel.',
    buttons: ['Yes', 'No'],
  })).response;
  if (selection !== 0) {
    return false;
  }
  const scriptsPath = path.join(apppath, 'scripts');
  const rproc = execa(
    path.join(rpath, 'bin', 'Rscript'),
    [path.join(scriptsPath, 'install_source.R')],
    {
      env: {
        LIB_PATH: libpath,
        SCRIPTS_PATH: scriptsPath,
        DOWNLOAD_STATIC_LIBV8: 'true',
      },
      all: !devMode,
    },
  );
  mainWindow.send('install-r-packages');

  ipcMain.on('kill-r-pkg-install', () => {
    try {
      rproc.kill();
    } catch (err) {
      log.warn(`Problems killing R process. Error message: ${err.message}.`);
    }
  });

  if (devMode) {
    rproc.stdout.pipe(process.stdout);
    rproc.stderr.pipe(process.stderr);
    try {
      await rproc;
    } catch (err) {
      log.warn(`Problems running R process. Error message: ${err.message}.`);
      return false;
    }
  } else {
    // eslint-disable-next-line no-restricted-syntax
    for await (const data of rproc.all) {
      mainWindow.send('install-r-packages-stdout', data.toString());
    }
    try {
      await rproc;
      mainWindow.send('install-r-packages-installed');
    } catch (err) {
      log.warn(`Problems running R process. Error message: ${err.message}.`);
      return false;
    }
  }
  return true;
}

export default installRPackages;
