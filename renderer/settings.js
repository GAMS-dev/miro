const { ipcRenderer, shell } = require('electron');
window.Bootstrap = require('bootstrap');
const $ = require('jquery');

const cbLaunchExternal = $('#launchExternal');
const cbRemoteExecution = $('#remoteExecution');
const inputLogLifetime = $('#logLifeTime');
const inputLanguage = $('#language');
const inputLogLevel = $('#logLevel');
const saveButton = $('#btSave');

let lang = {};

$('#helpLink').on('click', () => {
  shell.openExternal('https://gams.com/miro/deployment.html#sbs-customize-app');
});
let oldConfig = {};
const newConfig = {};
let defaultValues;
let importantKeys;
let requireRestart = false;
let pathValidating = false;

const optionAliasMap = {
  language: {
    English: 'en',
    Deutsch: 'de',
    中文: 'cn',
  },
};

const pathConfig = {
  configpath: {
    requiresRestart: true,
  },
  gamspath: {
  },
  rpath: {
  },
  logpath: {
  },
};

[inputLogLifetime, inputLanguage, inputLogLevel,
  cbLaunchExternal, cbRemoteExecution].forEach((el) => {
  el.on('change', () => {
    saveButton.attr('disabled', false);
  });
});

if (['darwin', 'win32'].includes(process.platform)) {
  // do not allow changing R path on macOS/Windows as R is bundled here
  $('#rpathWrapper').hide();
}

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

Object.keys(pathConfig).forEach((id) => {
  $(`#btPathSelect_${id}`).click(genPathSelectHandler(id));

  $(`#btPathSelect_${id}`).siblings('.btn-reset').click(function resetClickPath() {
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
$('.btn-reset-nonpath').click(function resetClickNonPath() {
  saveButton.attr('disabled', false);
  const elKey = this.dataset.key;
  newConfig[elKey] = '';
  if (elKey === 'launchExternal') {
    cbLaunchExternal.prop('checked', defaultValues[elKey]);
  } else if (elKey === 'remoteExecution') {
    cbRemoteExecution.prop('checked', defaultValues[elKey]);
  } else if (elKey === 'logLifeTime') {
    inputLogLifetime.val(defaultValues[elKey]);
  } else if (elKey === 'language') {
    inputLanguage.val(Object.keys(optionAliasMap.language)
      .find((key) => optionAliasMap.language[key] === defaultValues[elKey]));
  } else if (elKey === 'logLevel') {
    inputLogLevel.val(defaultValues[elKey]);
  }
  $(this).hide();
});

ipcRenderer.on('settings-loaded', (e, data, defaults, langData) => {
  if (langData != null && lang.title == null) {
    lang = langData;
    ['title', 'general-tab', 'paths-tab', 'launchBrowser', 'browserReset', 'generalLanguage', 'languageReset',
      'generalRemoteExec', 'remoteExecReset', 'generalLogging', 'loggingReset', 'generalLoglife', 'loglifeReset',
      'pathMiroapp', 'pathMiroappSelect', 'resetPathMiroapp', 'pathGams', 'pathGamsSelect', 'pathGamsReset', 'pathLog',
      'pathLogSelect', 'pathLogReset', 'pathR', 'pathRSelect', 'pathRReset', 'needHelp', 'btSave'].forEach((id) => {
      const el = document.getElementById(id);
      if (el) {
        el.innerText = lang[id];
      }
    });
    document.getElementById('btCancel').value = lang.btCancel;
    ['pathMiroappSelect', 'pathGamsSelect', 'pathLogSelect', 'pathRSelect'].forEach((id) => {
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
        'language', 'logLevel'].find((el) => el === key)) {
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
    } else if (['logLifeTime', 'logLevel', 'language'].find((el) => el === key)) {
      $(`#${key}`).val(key === 'language' ? Object.keys(optionAliasMap.language)
        .find((keyp) => optionAliasMap.language[keyp] === newValue) : newValue);
      if (isImportant) {
        $(`#${key}`).attr('disabled', true);
      }
    } else {
      const pathSelectEl = $(`#btPathSelect_${key}`);
      if (isImportant) {
        pathSelectEl.addClass('path-disabled');
      }
      pathSelectEl.siblings('label').text(newValue);
    }
  });
});
