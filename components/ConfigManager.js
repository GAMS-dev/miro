const { app } = require('electron');
const Store = require('electron-store');
const fs = require('fs');
const path = require('path');
const which = require('which');
const execa = require('execa');
const { tmpdir } = require('os');
const log = require('electron-log');

const minGams = '30.2';
const minR = '4.0';
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
        'rpath',
        'logpath',
        'launchExternal',
        'remoteExecution',
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
        ['gamspath', 'rpath', 'logpath', 'launchExternal', 'remoteExecution',
          'logLifeTime', 'language', 'colorTheme', 'logLevel', 'miroEnv'].forEach((el) => {
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

    ['gamspath', 'rpath', 'logpath', 'launchExternal', 'remoteExecution',
      'logLifeTime', 'language', 'colorTheme', 'logLevel', 'miroEnv'].forEach((el) => {
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

    if (['gamspath', 'rpath'].includes(key)) {
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

  static async validate(id, pathToValidate) {
    if (id === 'gams') {
      return ConfigManager.validateGAMS(pathToValidate);
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
module.exports = ConfigManager;
