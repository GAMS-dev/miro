const log = require('electron-log');
const http = require('axios');
const execa = require('execa');
const path = require('path');
const {
  getAppDbPath, isFalse,
} = require('./util');
const {
  randomPort, waitFor, isNull, kill,
} = require('./helpers');

/*
    This code was largely inspired by and taken from:
    https://github.com/dirkschumacher/r-shiny-electron

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
class MiroProcessManager {
  constructor(configData, inDevelopmentMode, isInBuildMode, miroResourcePath, appDataPath) {
    this.configData = configData;
    this.inDevelopmentMode = inDevelopmentMode;
    this.isInBuildMode = isInBuildMode;
    this.miroResourcePath = miroResourcePath;
    this.appDataPath = appDataPath;
    this.miroProcesses = [];
    this.pidPortMap = {};
    this.processIdMap = {};
  }

  getActiveApps() {
    return Object.keys(this.processIdMap);
  }

  getProc(internalPid) {
    return this.miroProcesses[internalPid];
  }

  async createNew(
    appData,
    libPath,
    progressCallback,
    onErrorStartup,
    onErrorLater,
    onSuccess,
    onProcessFinished,
    onMIROError = null,
  ) {
    const internalPid = await this.createNewMiroProc(
      appData,
      libPath,
      onErrorStartup,
      onErrorLater,
      onProcessFinished,
      onMIROError,
    );
    if (internalPid != null) {
      await progressCallback({ internalPid, code: 'start' });
      await waitFor(1500);
      await this.waitForResponse(
        appData.id,
        appData.allowMultiple,
        internalPid,
        progressCallback,
        onErrorStartup,
        onSuccess,
        appData.timeout,
      );
    }
  }

  async createNewMiroProc(
    appData,
    libPath,
    onErrorStartup,
    onErrorLater,
    onProcessFinished,
    onMIROError = null,
  ) {
    if (appData.allowMultiple !== true && this.processIdMap[appData.id]) {
      log.error('Process for this model already running. This should not happen. Reference not freed.');
      return null;
    }
    let internalPid = this.miroProcesses.findIndex(isNull);
    if (internalPid === -1) {
      internalPid = this.miroProcesses.length;
    }
    log.debug(`Request to start web server for app: ${appData.id} with internal pid: ${internalPid} submitted.`);
    if (appData.allowMultiple !== true) {
      this.processIdMap[appData.id] = internalPid;
    }
    let shinyPort;
    try {
      shinyPort = await randomPort();
    } catch (e) {
      log.debug(`Process could not be started, as scanning open ports failed with error: ${e.message}`);
      if (onErrorStartup) {
        await onErrorStartup(appData.id, e);
      }
      return null;
    }
    this.pidPortMap[internalPid.toString()] = shinyPort;
    log.debug(`Process: ${internalPid} is being started on port: ${shinyPort}.`);
    const rpath = this.configData.get('rpath');
    const gamspath = this.configData.get('gamspath');
    const logpath = this.configData.get('logpath');
    const dbPath = getAppDbPath(appData.dbpath);

    const generalConfig = {
      launchExternal: this.configData.get('launchExternal'),
      remoteExecution: this.configData.get('remoteExecution'),
      language: this.configData.get('language'),
      colorTheme: this.configData.get('colorTheme'),
      logLevel: this.configData.get('logLevel'),
    };

    let stdOutPipe = 'pipe';

    if (appData.stdOut != null) {
      stdOutPipe = appData.stdOut;
    } else if (this.inDevelopmentMode) {
      stdOutPipe = 'inherit';
    }

    let stdErrPipe = 'pipe';
    if (appData.stdErr != null) {
      stdErrPipe = appData.stdErr;
    }

    log.info(`MIRO app: ${appData.id} launched at port: ${shinyPort} with dbPath: ${dbPath}, \
developMode: ${this.inDevelopmentMode}, libPath: ${libPath}.`);

    let miroEnv = await this.configData.get('miroEnv');
    if (miroEnv != null && Object.keys(miroEnv).includes('PATH')) {
      // we append the current PATH
      const tidyPath = miroEnv.PATH
        .split(path.delimiter)
        .filter((el) => el.length > 0)
        .join(path.delimiter);
      // we need to clone object as we don't want to overwrite
      // config data
      miroEnv = JSON.parse(JSON.stringify(miroEnv));
      miroEnv.PATH = tidyPath + path.delimiter + process.env.PATH;
    }
    if (miroEnv == null) {
      miroEnv = {};
    }

    let libPathForwardSlashes;
    if (process.platform === 'win32') {
      libPathForwardSlashes = libPath.replace(/\\/g, '/');
    } else {
      libPathForwardSlashes = libPath;
    }

    const procEnv = Object.assign(miroEnv, {
      WITHIN_ELECTRON: '1',
      R_HOME_DIR: await rpath,
      RE_SHINY_PORT: shinyPort,
      RE_SHINY_PATH: this.miroResourcePath,
      R_LIBS: libPathForwardSlashes,
      R_LIBS_USER: libPathForwardSlashes,
      R_LIBS_SITE: libPathForwardSlashes,
      R_LIB_PATHS: libPathForwardSlashes,
      MIRO_NO_DEBUG: !this.inDevelopmentMode,
      MIRO_FORCE_SCEN_IMPORT: this.inDevelopmentMode && appData.forceScenImport,
      MIRO_USE_TMP: !isFalse(appData.usetmpdir),
      MIRO_WS_PATH: this.configData.getConfigPath(),
      MIRO_DB_PATH: dbPath,
      MIRO_BUILD: this.isInBuildMode,
      MIRO_BUILD_ARCHIVE: appData.buildArchive === true,
      GAMS_SYS_DIR: await gamspath,
      MIRO_LOG_PATH: await logpath,
      LAUNCHINBROWSER: await generalConfig.launchExternal,
      MIRO_REMOTE_EXEC: await generalConfig.remoteExecution,
      MIRO_LANG: await generalConfig.language,
      MIRO_THEME: await generalConfig.colorTheme,
      MIRO_LOG_LEVEL: await generalConfig.logLevel,
      MIRO_VERSION_STRING: appData.miroversion,
      MIRO_MODEL_PATH: this.inDevelopmentMode ? appData.modelPath
        : path.join(this.appDataPath, appData.id, `${appData.id}.gms`),
    });
    if (process.platform === 'linux') {
      procEnv.R_BROWSER = 'xdg-open';
    }
    if (appData.customEnv) {
      Object.keys(appData.customEnv).forEach((envName) => {
        procEnv[envName] = appData.customEnv[envName];
      });
    }
    this.miroProcesses[internalPid] = execa(
      path.join(await rpath, 'bin', 'R'),
      ['--no-echo', '--no-restore', '--vanilla',
        '-f', path.join(this.miroResourcePath, 'start-shiny.R')],
      {
        env: procEnv,
        stdout: stdOutPipe,
        stderr: stdErrPipe,
        cleanup: false,
      },
    );
    if (onMIROError != null) {
      this.miroProcesses[internalPid].stderr.on('data', (data) => {
        const msg = data.toString().trim();
        if (msg.startsWith('merr:::')) {
          log.debug(`MIRO error message received: ${msg}`);
          const error = msg.trim().split(':::');
          onMIROError(error);
        } else {
          process.stderr.write(msg);
        }
      });
    }
    this.miroProcesses[internalPid].catch(async (e) => {
      log.debug(`Process of MIRO app with pid: ${internalPid} stopped.`);
      this.miroProcesses[internalPid] = null;
      delete this.pidPortMap[internalPid.toString()];
      if (appData.allowMultiple !== true) {
        delete this.processIdMap[appData.id];
      }
      if (onErrorLater) {
        await onErrorLater(appData.id, e);
      }
    }).then(async (e) => {
      if (!this.miroProcesses[internalPid]) {
        return;
      }
      log.debug(`Process of MIRO app with pid: ${internalPid} ended.\nStdout: ${e.stdout}.\nStderr: ${e.stderr}`);
      this.miroProcesses[internalPid] = null;
      delete this.pidPortMap[internalPid.toString()];
      if (appData.allowMultiple !== true) {
        delete this.processIdMap[appData.id];
      }
      if (onProcessFinished) {
        await onProcessFinished(appData.id);
      }
    });
    return internalPid;
  }

  async waitForResponse(
    appId,
    allowMultiple,
    internalPid,
    progressCallback,
    onError,
    onSuccess,
    timeout,
  ) {
    const shinyPort = this.pidPortMap[internalPid.toString()];
    if (!shinyPort) {
      return;
    }
    const url = `http://127.0.0.1:${shinyPort}`;
    const maxIter = timeout == null ? 100 : timeout;
    /* eslint-disable no-await-in-loop */
    for (let i = 0; i <= maxIter; i += 1) {
      if (!this.miroProcesses[internalPid]) {
        return;
      }
      await waitFor(Math.min(i * 100, 1000));
      try {
        const res = await http.head(`${url}/shared/shiny.css`, { timeout: 10000 });
        if (res.status === 200) {
          await progressCallback({ code: 'success', port: shinyPort });
          onSuccess(url, this.miroProcesses[internalPid]);
          return;
        }
      } catch (e) {
        if (i > 10) {
          log.debug(`Process: ${internalPid} not responding after ${i + 1} seconds.`);
          if (progressCallback) {
            await progressCallback({ code: 'notresponding' });
          }
        }
      }
    }
    await this.terminate(internalPid);
    this.miroProcesses[internalPid] = null;
    delete this.pidPortMap[internalPid.toString()];
    if (allowMultiple !== true) {
      delete this.processIdMap[appId];
    }
    if (onError) {
      await onError(appId);
    }
  }

  async terminate(internalPid) {
    log.debug(`Request to terminate app with internal pid: ${internalPid} received.`);
    if (Number.isInteger(internalPid)) {
      const { pid } = this.miroProcesses[internalPid];
      try {
        await kill(pid);
        log.debug(`R process with pid: ${pid} killed.`);
        this.miroProcesses[internalPid] = null;
      } catch (e) {
        log.debug(`Problems killing R process with pid: ${pid}. Error message: ${e.message}`);
      }
    }
  }

  async terminateAll() {
    log.debug('Request to terminate all running apps received.');
    const termPromises = this.miroProcesses.map((miroProcess) => {
      if (!miroProcess) {
        return { pid: null };
      }
      const { pid } = miroProcess;
      return kill(pid);
    });
    for (let i = 0; i < this.miroProcesses.length; i += 1) {
      this.miroProcesses[i] = null;
    }

    const resolvedTermPromises = await Promise.allSettled(termPromises);
    resolvedTermPromises.forEach((termStatus) => {
      if (termStatus.status === 'rejected') {
        log.debug(`Problems killing R process. Error message: ${termStatus.reason.message}.`);
      } else if (termStatus.value.pid) {
        log.debug(`R process with pid: ${termStatus.value.pid} successfully terminated.`);
      }
    });
    this.processIdMap = {};
    this.pidPortMap = {};
  }
}

module.exports = MiroProcessManager;
