const { ipcRenderer, shell } = require('electron');
window.Bootstrap = require('bootstrap');
const $ = require('jquery');

const cbLaunchExternal = $('#launchExternal');
const cbRemoteExecution = $('#remoteExecution');
const inputLogLifetime = $('#logLifeTime');
const inputLanguage = $('#language');
const inputColorTheme = $('#colorTheme');
const inputLogLevel = $('#logLevel');
const saveButton = $('#btSave');
const btEnvReset = $('#btEnvReset');

let lang = {};

$('#helpLink').on('click', () => {
  shell.openExternal('https://gams.com/miro/deployment.html#sbs-customize-app');
});
let oldConfig = {};
const newConfig = {};
let defaultValues;
let importantKeys;
let noEnvDesc = 'No environment is specified';
let requireRestart = false;
let pathValidating = false;

const optionAliasMap = {
  language: {
    English: 'en',
    Deutsch: 'de',
    中文: 'cn',
  },
  colorTheme: {
    colorThemeOptionDefault: 'default',
    colorThemeOptionBlackWhite: 'blackandwhite',
    colorThemeOptionForest: 'forest',
    colorThemeOptionTawny: 'tawny',
    colorThemeOptionDarkBlue: 'darkblue',
    colorThemeOptionRedWine: 'redwine',
  },
};

const pathConfig = {
  configpath: {
    requiresRestart: true,
  },
  gamspath: {
  },
  pythonpath: {
  },
  rpath: {
  },
  logpath: {
  },
};

[inputLogLifetime, inputLanguage, inputColorTheme, inputLogLevel,
  cbLaunchExternal, cbRemoteExecution].forEach((el) => {
  el.on('change', () => {
    saveButton.attr('disabled', false);
  });
});

if (['darwin', 'win32'].includes(process.platform)) {
  // do not allow changing R path on macOS/Windows as R is bundled here
  $('#rpathWrapper').hide();
}

function updateEnvTable(envData) {
  if (envData && Object.keys(envData).length > 0) {
    btEnvReset.attr('disabled', false);
    newConfig.miroEnv = envData;
    $('#env tbody').html(Object.entries(envData).map((entry) => `<tr><td>${entry[0]}</td><td>${entry[1]}</td></tr>`).join(''));
  } else {
    newConfig.miroEnv = null;
    btEnvReset.attr('disabled', true);
    $('#env tbody').html(`<tr><td colspan="2">${noEnvDesc}</td></tr>`);
  }
}

$('#btEnvImport').on('click', () => {
  ipcRenderer.send('import-miroenv');
});

$('#btEnvExport').on('click', () => {
  ipcRenderer.send('export-miroenv');
});

$('#btEnvReset').on('click', () => {
  updateEnvTable();
  saveButton.attr('disabled', false);
});

saveButton.on('click', () => {
  if (pathValidating === true) {
    return;
  }
  let logLifeVal = inputLogLifetime.val();
  if (logLifeVal !== '') {
    logLifeVal = parseInt(logLifeVal, 10);
    if (Number.isNaN(logLifeVal)) {
      return;
    }
  }
  newConfig.logLifeTime = logLifeVal;
  newConfig.launchExternal = cbLaunchExternal.is(':checked');
  newConfig.remoteExecution = cbRemoteExecution.is(':checked');

  newConfig.language = optionAliasMap.language[inputLanguage.val()];
  newConfig.colorTheme = optionAliasMap.colorTheme[inputColorTheme.val()];
  let oldLanguage = defaultValues.language;
  if (oldConfig.language) {
    oldLanguage = oldConfig.language;
  }
  if (oldLanguage !== newConfig.language) {
    requireRestart = true;
  }
  newConfig.logLevel = inputLogLevel.val();
  saveButton.attr('disabled', true);
  ipcRenderer.send('save-general-config', newConfig, requireRestart);
});

$('#btCancel').on('click', () => {
  ipcRenderer.send('close-window', 'settings');
});

function genPathSelectHandler(id) {
  return () => {
    if (importantKeys && importantKeys.find((el) => el === id)) {
      return;
    }
    pathValidating = true;
    let newPath;
    if (newConfig[id]) {
      newPath = newConfig[id];
    } else if (oldConfig[id]) {
      newPath = oldConfig[id];
    } else {
      newPath = defaultValues[id];
    }
    ipcRenderer.send('settings-select-new-path', id, newPath);
  };
}
ipcRenderer.on('settings-new-path-selected', (e, id, pathSelected) => {
  saveButton.attr('disabled', false);
  pathValidating = false;
  newConfig[id] = pathSelected;
  $(`#btPathSelect_${id}`)
    .siblings('label').text(pathSelected);

  if (pathConfig[id].requiresRestart === true) {
    requireRestart = true;
  }
});

ipcRenderer.on('update-miroEnv', (e, envData) => {
  updateEnvTable(envData);
  saveButton.attr('disabled', false);
});

Object.keys(pathConfig).forEach((id) => {
  $(`#btPathSelect_${id}`).on('click', genPathSelectHandler(id));

  $(`#btPathSelect_${id}`).siblings('.btn-reset').on('click', function resetClickPath() {
    const elKey = this.dataset.key;
    newConfig[elKey] = '';
    saveButton.attr('disabled', false);
    if (Object.keys(pathConfig).find((id2) => id2 === elKey
      && pathConfig[id2].requiresRestart === true)) {
      requireRestart = true;
    }
    const $this = $(this);
    $this.siblings('label').text(defaultValues[elKey]);
    $this.hide();
  });
});
$('.btn-reset-nonpath').on('click', function resetClickNonPath() {
  saveButton.attr('disabled', false);
  const elKey = this.dataset.key;
  newConfig[elKey] = '';
  if (elKey === 'launchExternal') {
    cbLaunchExternal.prop('checked', defaultValues[elKey]);
  } else if (elKey === 'remoteExecution') {
    cbRemoteExecution.prop('checked', defaultValues[elKey]);
  } else if (elKey === 'logLifeTime') {
    inputLogLifetime.val(defaultValues[elKey]);
  } else if (Object.keys(optionAliasMap).includes(elKey)) {
    let inputElTmp;
    if (elKey === 'language') {
      inputElTmp = inputLanguage;
    } else if (elKey === 'colorTheme') {
      inputElTmp = inputColorTheme;
    } else {
      console.error('COULD NOT FIND INPUT EL!!'); // eslint-disable-line no-console
      return;
    }
    inputElTmp.val(Object.keys(optionAliasMap[elKey])
      .find((key) => optionAliasMap[elKey][key] === defaultValues[elKey]));
  } else if (elKey === 'logLevel') {
    inputLogLevel.val(defaultValues[elKey]);
  }
  $(this).hide();
});

ipcRenderer.on('settings-loaded', (e, data, defaults, langData) => {
  if (langData != null && lang.title == null) {
    lang = langData;
    ['colorThemeOptionDefault', 'colorThemeOptionBlackWhite',
      'colorThemeOptionForest', 'colorThemeOptionTawny', 'colorThemeOptionDarkBlue', 'colorThemeOptionRedWine'].forEach((id) => {
      optionAliasMap.colorTheme[lang[id]] = optionAliasMap.colorTheme[id];
      delete optionAliasMap.colorTheme[id];
    });
    ['title', 'general-tab', 'paths-tab', 'env-tab', 'launchBrowser', 'browserReset', 'generalLanguage', 'languageReset',
      'generalRemoteExec', 'remoteExecReset', 'generalLogging', 'loggingReset', 'generalLoglife', 'loglifeReset',
      'pathMiroapp', 'pathMiroappSelect', 'resetPathMiroapp', 'pathGams', 'pathGamsSelect', 'pathGamsReset',
      'pathPython', 'pathPythonSelect', 'pathPythonReset', 'pathLog', 'pathLogSelect', 'pathLogReset', 'pathR', 'pathRSelect',
      'pathRReset', 'needHelp', 'btSave', 'btEnvImport', 'btEnvExport', 'btEnvReset', 'miroEnvHdrVar', 'miroEnvHdrVal',
      'generalColorTheme', 'colorThemeReset', 'colorThemeOptionDefault', 'colorThemeOptionBlackWhite',
      'colorThemeOptionForest', 'colorThemeOptionTawny', 'colorThemeOptionDarkBlue', 'colorThemeOptionRedWine'].forEach((id) => {
      const el = document.getElementById(id);
      if (el) {
        el.innerText = lang[id];
      }
    });
    noEnvDesc = lang.noEnvDesc;
    document.getElementById('btCancel').value = lang.btCancel;
    ['pathMiroappSelect', 'pathGamsSelect', 'pathPythonSelect', 'pathLogSelect', 'pathRSelect'].forEach((id) => {
      const el = document.getElementById(id);
      if (el) {
        $(el).addClass('browseLang').attr('content-after', lang.browseFiles);
      }
    });
  }
  oldConfig = data;
  saveButton.attr('disabled', true);
  defaultValues = defaults;
  if (!data.important) {
    importantKeys = [];
  } else if (Array.isArray(data.important)) {
    importantKeys = data.important;
  } else {
    importantKeys = [data.important];
  }
  requireRestart = false;
  Object.entries(data).forEach(([key, value]) => {
    if (key === 'important') {
      return;
    }
    let newValue = value;
    let isImportant = false;
    if (importantKeys.find((el) => el === key)) {
      isImportant = true;
    }
    if (newValue == null || newValue === '') {
      newValue = defaultValues[key];
    } else if (!isImportant) {
      if (['launchExternal', 'remoteExecution', 'logLifeTime',
        'language', 'colorTheme', 'logLevel'].includes(key)) {
        if (newValue !== defaultValues[key]) {
          $(`[data-key="${key}"]`).show();
        }
      } else {
        $(`#btPathSelect_${key}`).siblings('.btn-reset').show();
      }
    }
    if (key === 'launchExternal') {
      cbLaunchExternal.prop('checked', newValue);
      if (isImportant) {
        cbLaunchExternal.attr('disabled', true);
      }
    } else if (key === 'remoteExecution') {
      cbRemoteExecution.prop('checked', newValue);
      if (isImportant) {
        cbRemoteExecution.attr('disabled', true);
      }
    } else if (['logLifeTime', 'logLevel', 'language', 'colorTheme'].find((el) => el === key)) {
      $(`#${key}`).val(Object.keys(optionAliasMap).includes(key) ? Object.keys(optionAliasMap[key])
        .find((keyp) => optionAliasMap[key][keyp] === newValue) : newValue);
      if (isImportant) {
        $(`#${key}`).attr('disabled', true);
      }
    } else if (key === 'miroEnv') {
      updateEnvTable(newValue);
    } else {
      const pathSelectEl = $(`#btPathSelect_${key}`);
      if (isImportant) {
        pathSelectEl.addClass('path-disabled');
      }
      pathSelectEl.siblings('label').text(newValue);
    }
  });
});
