import { app } from 'electron';
import Store from 'electron-store';
import fs from 'node:fs';
import path from 'node:path';
import which from 'which';
import { execa } from 'execa';
import { tmpdir } from 'node:os';
import log from 'electron-log/main.js';

const minGams = '30.2';
const minR = '4.0';
const minPython = '3.8';
const gamsDirNameRegex = /^(GAMS)?(\d+\.\d+)$/;

const schema = {
  configpath: {
    type: 'string',
    minLength: 2,
  },
  gamspath: {
    type: 'string',
    minLength: 2,
  },
  pythonpath: {
    type: 'string',
    minLength: 2,
  },
  rpath: {
    type: 'string',
    minLength: 2,
  },
  logpath: {
    type: 'string',
    minLength: 2,
  },
  launchExternal: {
    type: 'boolean',
  },
  remoteExecution: {
    type: 'boolean',
  },
  remoteConfig: {
    type: 'object',
    properties: {
      url: {
        type: 'string',
        minLength: 2,
      },
      username: {
        type: 'string',
        minLength: 2,
      },
      jwt: {
        type: 'string',
        minLength: 2,
      },
      namespace: {
        type: 'string',
        minLength: 1,
      },
      verifypeer: {
        type: 'boolean',
      },
    },
    additionalProperties: false,
  },
  logLifeTime: {
    type: 'integer',
    minimum: -1,
  },
  language: {
    type: 'string',
    enum: ['en', 'de', 'cn'],
  },
  colorTheme: {
    type: 'string',
    enum: ['default', 'blackandwhite', 'forest', 'tawny', 'darkblue', 'redwine'],
  },
  logLevel: {
    type: 'string',
    enum: ['TRACE', 'DEBUG', 'INFO',
      'WARN', 'ERROR', 'FATAL'],
  },
  miroEnv: {
    type: 'object',
    propertyNames: {
      pattern: '^[A-Z_][A-Z0-9_]*$',
    },
    additionalProperties: {
      type: 'string',
    },
  },
  important: {
    type: 'array',
    items: {
      type: 'string',
      enum: [
        'gamspath',
        'pythonpath',
        'rpath',
        'logpath',
        'launchExternal',
        'remoteExecution',
        'remoteConfig',
        'logLifeTime',
        'language',
        'colorTheme',
        'logLevel',
        'miroEnv',
      ],
    },
  },
};

class ConfigManager extends Store {
  constructor(appRootDir, miroWorkspaceDir) {
    let configPathTmp = miroWorkspaceDir;
    super({
      schema,
      cwd: configPathTmp,
      name: 'settings',
      encryptionKey: 'MIROobfuscatedConfigFile',
    });
    try {
      configPathTmp = super.get('configpath', '');
    } catch (e) {
      // continue regardless of error
    }
    this.important = [];
    if (configPathTmp) {
      try {
        const superPathConfigData = new Store({
          schema,
          cwd: configPathTmp,
          name: 'settings',
        });
        ['gamspath', 'pythonpath', 'rpath', 'logpath', 'launchExternal', 'remoteExecution',
          'remoteConfig', 'logLifeTime', 'language', 'colorTheme', 'logLevel', 'miroEnv'].forEach((el) => {
          this[el] = superPathConfigData.get(el, '');
        });
        this.important = superPathConfigData.get('important', []);
      } catch (e) {
        // continue regardless of error
      }
    }

    this.appRootDir = appRootDir;
    this.configpath = configPathTmp;
    this.configpathDefault = miroWorkspaceDir;
    this.logpathDefault = path.join(miroWorkspaceDir, 'logs');

    ['gamspath', 'pythonpath', 'rpath', 'logpath', 'launchExternal', 'remoteExecution',
      'remoteConfig', 'logLifeTime', 'language', 'colorTheme', 'logLevel', 'miroEnv'].forEach((el) => {
      if (this.important.find((iel) => iel === el)) {
        return;
      }
      this[el] = super.get(el, this[el] == null ? '' : this[el]);
    });
  }

  validate(key, val) {
    const currVal = super.get(key);
    try {
      super.set(key, val);
    } finally {
      if (currVal != null) {
        super.set(key, currVal);
      }
    }
  }

  set(data) {
    Object.entries(data).forEach(([key, value]) => {
      this[key] = value;
      if (value == null || value === ''
        || (key === 'launchExternal' && value === false)
        || (key === 'remoteExecution' && value === false)
        || (key === 'logLifeTime' && value === -1)
        || (key === 'language' && value === 'en')
        || (key === 'colorTheme' && value === 'default')
        || (key === 'logLevel' && value === 'INFO')) {
        this[key] = '';
        super.delete(key);
      } else {
        super.set(key, value);
      }
    });
    return this;
  }

  async get(key, fallback = true) {
    let valTmp;

    if (key === 'rpath'
      && ['darwin', 'win32'].includes(process.platform)) {
      valTmp = await this.getDefault('rpath');
      return valTmp;
    }

    valTmp = this[key];

    if (['gamspath', 'pythonpath', 'rpath'].includes(key)) {
      if (valTmp && !fs.existsSync(valTmp)) {
        this[key] = '';
        valTmp = '';
      }
    }

    if (fallback) {
      // if options is not set, fetch defaults
      if ((valTmp == null || valTmp === '')) {
        valTmp = await this.getDefault(key);
      }
    }

    return valTmp;
  }

  async getDefault(key) {
    if (key === 'rpath') {
      return this.findR();
    } if (key === 'gamspath') {
      return this.findGAMS();
    } if (key === 'pythonpath') {
      return this.findPython();
    } if (key === 'logpath') {
      return this.logpathDefault;
    } if (key === 'configpath') {
      return this.configpathDefault;
    } if (key === 'logLifeTime') {
      return -1;
    } if (key === 'language') {
      return 'en';
    } if (key === 'colorTheme') {
      return 'default';
    } if (key === 'logLevel') {
      return 'INFO';
    } if (key === 'launchExternal') {
      return false;
    } if (key === 'remoteExecution') {
      return false;
    }
    return null;
  }

  async getAll(defaults = false) {
    const keys = Object.keys(schema);
    const valuePromises = keys.map((key) => {
      if (defaults) {
        return this.getDefault(key);
      }
      return this.get(key, '');
    });
    let values;
    try {
      values = await Promise.all(valuePromises);
    } catch (e) {
      log.error(e);
      return {};
    }
    return Object.fromEntries(keys.map((_, i) => [keys[i], values[i]]));
  }

  async removeOldLogs() {
    if (this.logLifeTime == null
      || this.logLifeTime === ''
      || this.logLifeTime < 0) {
      return true;
    }
    const now = new Date().getTime();
    try {
      const logPath = await this.get('logpath');
      const logFiles = await fs.promises.readdir(logPath);
      if (!logFiles) {
        return true;
      }
      logFiles.forEach(async (logFile) => {
        if (logFile === 'launcher.log') {
          return;
        }
        try {
          const fp = path.join(logPath, logFile);
          const { mtime } = await fs.promises.stat(fp);
          if ((now - mtime.getTime())
            / (1000 * 3600 * 24) > this.logLifeTime) {
            fs.promises.unlink(fp);
          }
        } catch (e) {
          log.error(e);
        }
      });
      return true;
    } catch (e) {
      log.error(e);
      return false;
    }
  }

  getConfigPath() {
    if (this.configpath) {
      return this.configpath;
    }
    return this.configpathDefault;
  }

  getSync(key) {
    return super.get(key, '');
  }

  async findR() {
    if (this.rpathDefault) {
      return this.rpathDefault;
    }
    if (process.platform === 'win32') {
      this.rpathDefault = path.join(this.appRootDir, 'r');
    } else if (process.platform === 'darwin' && app.isPackaged) {
      this.rpathDefault = path.resolve(path.join(this.appRootDir, '..', 'Resources', 'r'));
    }
    try {
      if (!this.rpathDefault
        || !fs.existsSync(this.rpathDefault)) {
        if (process.platform === 'darwin') {
          const rPathRoot = path.join(
            '/',
            'Library',
            'Frameworks',
            'R.framework',
            'Versions',
          );
          const rVersionsAvailable = fs.readdirSync(rPathRoot, { withFileTypes: true })
            .filter((el) => (el.isDirectory()))
            .map((el) => (el.name))
            .filter((el) => {
              try {
                return ConfigManager.vComp(el, minR);
              } catch (e) {
                log.error(e);
                return false;
              }
            });
          if (rVersionsAvailable.length) {
            this.rpathDefault = path.join(
              rPathRoot,
              rVersionsAvailable[0],
              'Resources',
            );
          }
        } else {
          let rpathTmp = which.sync('Rscript', { nothrow: true });
          rpathTmp = await ConfigManager.validateR(rpathTmp);
          if (rpathTmp !== false) {
            this.rpathDefault = rpathTmp;
          }
        }
      }
    } catch (e) {
      log.error(e);
      this.rpathDefault = '';
    }
    return this.rpathDefault;
  }

  static async validateR(rpath) {
    if (!rpath) {
      log.info('R path to validate is empty');
      return false;
    }
    let rpathTmp = rpath;

    if (!path.basename(rpathTmp).toLowerCase().startsWith('rscript')) {
      if (!fs.lstatSync(rpathTmp).isDirectory()) {
        log.info('R path to validate is not a directory');
        return false;
      }
      // Directory was selected, so scan it
      let contentRDir;
      try {
        contentRDir = await fs.promises.readdir(
          rpathTmp,
          { withFileTypes: true },
        );
      } catch (e) {
        log.error(e);
        return false;
      }
      if (contentRDir.find((el) => el.name === 'bin')) {
        if (process.platform === 'win32') {
          rpathTmp = path.join(rpathTmp, 'bin', 'x64');
        } else {
          rpathTmp = path.join(rpathTmp, 'bin');
        }
      } else if (contentRDir.find((el) => el.name === 'Resources')) {
        rpathTmp = path.join(rpathTmp, 'Resources', 'bin');
      } else if (!contentRDir.find((el) => el.isFile()
        && (el.name === 'Rscript' || el.name === 'Rscript.exe'))) {
        log.info('R path to validate is not a directory');
        return false;
      }
      if (process.platform === 'win32') {
        rpathTmp = path.join(rpathTmp, 'Rscript.exe');
      } else {
        rpathTmp = path.join(rpathTmp, 'Rscript');
      }
      if (!fs.existsSync(rpathTmp)) {
        log.info('Rscript executable not found in R path');
        return false;
      }
    }
    let { stdout } = await execa(rpathTmp, ['-e',
      'print(R.home())\nprint(paste0(R.Version()$major, \
".", R.Version()$minor))']);
    if (!stdout) {
      log.info('Stdout of Rscript is empty');
      return false;
    }
    stdout = stdout.split('\n');
    if (stdout.length < 2) {
      log.info(`Stdout of Rscript is invalid: ${stdout.join('\n')}`);
      return false;
    }
    const rOutRegex = /^\[1\] "([^"]*)"/;
    const rpathIdx = stdout.findIndex((line) => rOutRegex.test(line));
    if (rpathIdx === -1) {
      log.info(`Stdout of Rscript is invalid: ${stdout.join('\n')}`);
      return false;
    }
    rpathTmp = stdout[rpathIdx].match(rOutRegex);
    const rVersion = stdout[rpathIdx + 1].match(/^\[1\] "([^"]*)"$/);
    if (rpathTmp && rVersion
      && ConfigManager.vComp(rVersion[1], minR)) {
      return rpathTmp[1];
    }
    log.info(`Stdout of Rscript is invalid: ${stdout.join('\n')}`);
    return false;
  }

  async findGAMS() {
    if (this.gamspathDefault) {
      return this.gamspathDefault;
    }
    const vCompReducer = (acc, curr) => {
      if (ConfigManager.vComp(acc, curr)) {
        return acc;
      }
      return curr;
    };

    if (process.platform === 'darwin') {
      let latestGamsInstalled = [];
      let isFramework;
      if (fs.existsSync('/Library/Frameworks/GAMS.framework/Versions')) {
        isFramework = true;
        latestGamsInstalled = fs.readdirSync(
          '/Library/Frameworks/GAMS.framework/Versions',
          { withFileTypes: true },
        )
          .filter((el) => el.isDirectory());
        if (latestGamsInstalled.length > 0) {
          latestGamsInstalled = latestGamsInstalled
            .map((el) => el.name)
            .reduce(vCompReducer);
        }
      } else {
        isFramework = false;
        latestGamsInstalled = fs.readdirSync(
          '/Applications',
          { withFileTypes: true },
        )
          .filter((el) => el.isDirectory() && gamsDirNameRegex.test(el.name));
        if (latestGamsInstalled.length > 0) {
          latestGamsInstalled = latestGamsInstalled
            .map((el) => el.name.slice(4))
            .reduce(vCompReducer);
        }
      }

      if (latestGamsInstalled.length > 0
        && ConfigManager.vComp(latestGamsInstalled, minGams)) {
        if (isFramework) {
          this.gamspathDefault = path.join(
            '/Library/Frameworks/GAMS.framework/Versions',
            latestGamsInstalled,
            'Resources',
          );
        } else {
          this.gamspathDefault = path.join(
            '/Applications',
            `GAMS${latestGamsInstalled}`,
            'GAMS Terminal.app',
            'Contents',
            'MacOS',
          );
        }
      } else if (latestGamsInstalled.length > 0) {
        log.info(`Latest installed GAMS version found: \
${latestGamsInstalled}`);
      }
    } else {
      try {
        this.gamspathDefault = path.dirname(which.sync(
          'gams',
          { nothrow: true },
        ));
      } catch (e) {
        // continue regardless of error
      }
    }

    if (!this.gamspathDefault && process.platform === 'win32') {
      let GAMSRootPath = 'C:\\GAMS';
      let latestGamsInstalled = [];
      try {
        latestGamsInstalled = fs.readdirSync(
          GAMSRootPath,
          { withFileTypes: true },
        )
          .filter((el) => {
            if (!el.isDirectory()) {
              return false;
            }
            const gamsVer = parseInt(el.name, 10);
            if (Number.isNaN(gamsVer) || gamsVer < 32) {
              return false;
            }
            return true;
          })
          .map((el) => el.name);
      } catch (_) {
        // continue regardless of error
      }
      if (latestGamsInstalled.length === 0) {
        GAMSRootPath = 'C:\\GAMS\\win64';
        try {
          latestGamsInstalled = fs.readdirSync(
            GAMSRootPath,
            { withFileTypes: true },
          )
            .filter((el) => el.isDirectory() && gamsDirNameRegex.test(el.name))
            .map((el) => el.name);
        } catch (_) {
          // continue regardless of error
        }
      }
      if (latestGamsInstalled.length > 0) {
        latestGamsInstalled = latestGamsInstalled
          .reduce(vCompReducer);
      }

      if (latestGamsInstalled.length > 0
        && ConfigManager.vComp(latestGamsInstalled, minGams)) {
        this.gamspathDefault = path.join(
          GAMSRootPath,
          latestGamsInstalled,
        );
      } else if (latestGamsInstalled.length > 0) {
        log.info(`Latest installed GAMS version found: \
  ${latestGamsInstalled}`);
      }
    }

    return this.gamspathDefault;
  }

  static getMinimumVersion(type) {
    if (type.toLowerCase() === 'gams') {
      return minGams;
    }
    if (type.toLowerCase() === 'python') {
      return minPython;
    }
    return minR;
  }

  static async validateGAMS(gamsDir) {
    let contentGamsDir;
    let gamsExecDir;
    try {
      contentGamsDir = await fs.promises.readdir(
        gamsDir,
        { withFileTypes: true },
      );
    } catch (e) {
      log.error(e);
      return false;
    }
    if (contentGamsDir.find((el) => el.isFile()
      && (el.name === 'gams' || el.name === 'gams.exe'))) {
      log.debug('GAMS executable found.');
      if (process.platform === 'win32') {
        gamsExecDir = path.join(gamsDir, 'gams.exe');
      } else {
        gamsExecDir = path.join(gamsDir, 'gams');
      }
    } else {
      // gams executable not in selected folder
      log.debug('GAMS executable not found in the selected folder.');
      contentGamsDir = contentGamsDir
        .filter((el) => el.isDirectory() || el.isSymbolicLink());
      const gamsDirName = contentGamsDir.find((el) => gamsDirNameRegex.test(el.name));
      if (gamsDirName) {
        if (process.platform === 'win32') {
          gamsExecDir = path.join(gamsDir, gamsDirName, 'sysdir', 'gams.exe');
        } else if (process.platform === 'darwin') {
          gamsExecDir = path.join(
            gamsDir,
            gamsDirName,
            'GAMS Terminal.app',
            'Contents',
            'MacOS',
            'gams',
          );
        } else {
          log.info('System is neither Windows nor MacOS. On Linux, must select sysdir directly.');
          return false;
        }
      } else if (process.platform === 'win32'
        && contentGamsDir.find((el) => el.name === 'sysdir')) {
        gamsExecDir = path.join(gamsDir, 'sysdir', 'gams.exe');
      } else if (process.platform === 'darwin') {
        if (contentGamsDir.find((el) => el.name === 'GAMS Terminal.app')) {
          gamsExecDir = path.join(
            gamsDir,
            'GAMS Terminal.app',
            'Contents',
            'MacOS',
            'gams',
          );
        } else if (contentGamsDir.find((el) => el.name === 'GAMS.framework')) {
          gamsExecDir = path.join(gamsDir, 'GAMS.framework', 'Resources', 'gams');
        } else if (contentGamsDir.find((el) => el.name === 'Current')) {
          gamsExecDir = path.join(gamsDir, 'Current', 'Resources', 'gams');
        } else {
          log.info('Directory selected does not contain a valid GAMS installation.');
          return false;
        }
      } else {
        log.info('System is neither Windows nor MacOS (or Terminal.app was not found). On Linux, must select sysdir directly.');
        return false;
      }
    }

    try {
      let { stdout } = await execa(
        gamsExecDir,
        ['/??', 'lo=3',
          `curdir=${tmpdir}`],
        process.platform === 'linux' ? { env: { XDG_DATA_DIRS: '' } } : {},
      );
      stdout = stdout.split('\n');
      if (stdout.length < 2) {
        log.info(`Invalid stdout from GAMS: ${stdout.slice(0, 5).join('\n')}`);
        return false;
      }
      const selectedGamsVer = stdout[1]
        .match(/^GAMS Release: (\d+\.\d+\.\d+)/);
      if (selectedGamsVer
        && ConfigManager.vComp(selectedGamsVer[1], minGams)) {
        return path.dirname(gamsExecDir);
      }
      log.info(`Invalid stdout from GAMS: ${stdout.slice(0, 5).join('\n')}`);
      return false;
    } catch (e) {
      log.error(e);
      return false;
    }
  }

  async findPython() {
    if (this.pythonpathDefault) {
      return this.pythonpathDefault;
    }
    try {
      let pythonpathTmp = which.sync('python3', { nothrow: true });
      pythonpathTmp = await ConfigManager.validatePython(pythonpathTmp);
      if (pythonpathTmp === false) {
        pythonpathTmp = which.sync('python', { nothrow: true });
        pythonpathTmp = await ConfigManager.validatePython(pythonpathTmp);
      }
      if (pythonpathTmp !== false) {
        this.pythonpathDefault = pythonpathTmp;
      }
    } catch (e) {
      log.error(e);
      this.pythonpathDefault = '';
    }
    return this.pythonpathDefault;
  }

  static async validatePython(pythonpath) {
    if (!pythonpath) {
      log.info('Python path to validate is empty');
      return false;
    }
    try {
      let pythonpathTmp;
      if (path.basename(pythonpath).toLowerCase().startsWith('python')) {
        pythonpathTmp = pythonpath;
      } else {
        if (!fs.lstatSync(pythonpath).isDirectory()) {
          log.info('Python path to validate is not a directory');
          return false;
        }
        // Directory was selected, so scan it
        let contentPythonDir;
        const dirsToExplore = [pythonpath, path.join(pythonpath, 'bin')];
        for (let i = 0; i < dirsToExplore.length; i += 1) {
          try {
            contentPythonDir = await fs.promises.readdir( // eslint-disable-line no-await-in-loop
              dirsToExplore[i],
              { withFileTypes: true },
            );
            if (process.platform === 'win32') {
              const contentPythonDirTmp = contentPythonDir.filter((el) => (el.isFile() || el.isSymbolicLink()) && el.name === 'python.exe');
              if (contentPythonDirTmp.length > 0) {
                pythonpathTmp = path.join(dirsToExplore[i], contentPythonDirTmp[0].name);
              }
            } else {
              const contentPythonDirTmp = contentPythonDir.filter((el) => (el.isFile() || el.isSymbolicLink()) && ['python', 'python3'].includes(el.name));
              if (contentPythonDirTmp.length > 0) {
                if (contentPythonDirTmp.length > 1) {
                  pythonpathTmp = path.join(dirsToExplore[i], contentPythonDirTmp.filter((el) => (el.isFile() || el.isSymbolicLink()) && el.name === 'python3')[0].name);
                } else {
                  pythonpathTmp = path.join(dirsToExplore[i], contentPythonDirTmp[0].name);
                }
              }
            }
            if (pythonpathTmp) {
              break;
            }
          } catch (e) {
            // do nothing if dir does not exist
          }
        }
        if (!pythonpathTmp || !fs.existsSync(pythonpathTmp)) {
          log.info('python executable not found in python path');
          return false;
        }
      }
      let { stdout } = await execa(pythonpathTmp, ['-V']);
      if (!stdout) {
        log.info('Stdout of python is empty');
        return false;
      }
      stdout = stdout.split(' ');
      if (stdout.length < 2) {
        log.info(`Stdout of python is invalid: ${stdout.join(' ')}`);
        return false;
      }
      if (ConfigManager.vComp(stdout[1], minPython)) {
        return pythonpathTmp;
      }
      log.info(`Stdout of python is invalid: ${stdout.join(' ')}`);
      return false;
    } catch (e) {
      log.error(e);
      return false;
    }
  }

  static async validate(id, pathToValidate) {
    if (id === 'gams') {
      return ConfigManager.validateGAMS(pathToValidate);
    }
    if (id === 'python') {
      return ConfigManager.validatePython(pathToValidate);
    }
    return ConfigManager.validateR(pathToValidate);
  }

  static vComp(v1, v2) {
    const v1parts = v1.split('.');
    const v2parts = v2.split('.');
    const v1Major = parseInt(v1parts[0], 10);
    const v2Major = parseInt(v2parts[0], 10);
    const v1Minor = parseInt(v1parts[1], 10);
    const v2Minor = parseInt(v2parts[1], 10);
    if (v1Major > v2Major || (v1Major === v2Major
      && v1Minor >= v2Minor)) {
      return true;
    }
    return false;
  }
}
export default ConfigManager;
