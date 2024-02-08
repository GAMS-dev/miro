const { ipcRenderer, shell } = require('electron');
window.Bootstrap = require('bootstrap');
const $ = require('jquery');
const { OAuthClient } = require('../components/oauth');
const {
  EngineConfig, getEngineAuthProviders,
  getEngineUserInfo, getEngineJwt,
  EngineError,
} = require('../components/engine');

const cbLaunchExternal = $('#launchExternal');
const cbRemoteExecution = $('#remoteExecution');
const inputLogLifetime = $('#logLifeTime');
const inputLanguage = $('#language');
const inputColorTheme = $('#colorTheme');
const inputLogLevel = $('#logLevel');
const saveButton = $('#btSave');
const btEnvReset = $('#btEnvReset');

let oAuthClient = null;
const engineConfig = new EngineConfig();
let engineUrlAbortController;

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
  cbLaunchExternal, cbRemoteExecution,
  $('#engineNs'), $('#engineUsername'), $('#enginePassword'), $('#engineJWT')].forEach((el) => {
  el.on('change', () => {
    saveButton.attr('disabled', false);
  });
});

[$('#engineNs'), $('#engineUsername'), $('#enginePassword'), $('#engineJWT')].forEach((el) => {
  el.on('input', () => {
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

const fetchEngineLoginMethods = async (url, options) => {
  $('#engineLoginPassword').hide();
  $('#engineLoginJWT').hide();
  $('#engineLoginMethod').empty().append($('<option>', {
    value: '_main',
    text: lang.engineLoginMethodUserPass,
  }), $('<option>', {
    value: '_jwt',
    text: 'JWT',
  }));
  try {
    const authProvidersTmp = await getEngineAuthProviders(url, options?.signal);
    engineConfig.oauthProviders = authProvidersTmp.filter((idp) => {
      if (idp?.oidc != null || idp?.oauth != null) {
        $('#engineLoginMethod').append($('<option>', {
          value: idp.name,
          text: idp.label,
        }));
        return true;
      }
      return false;
    }).map((idp) => idp.name);
    engineConfig.ldapProviders = authProvidersTmp.filter((idp) => {
      if (idp?.is_ldap_identity_provider === true) {
        $('#engineLoginMethod').append($('<option>', {
          value: idp.name,
          text: idp.label,
        }));
        return true;
      }
      return false;
    }).map((idp) => idp.name);
  } catch (err) {
    __electronLog.info(`Problems fetching auth providers (url: ${url}). Error: ${JSON.stringify(err)}`);
    $('#engine-tab').tab('show');
    $('#engineUrl').addClass('is-invalid');
    return;
  }
  __electronLog.debug(`Successfully retrieved IDPs for URL: ${url}`);
  $('#engineUrl').removeClass('is-invalid');
  $('#engineUsername').val('');
  $('#enginePassword').val('');
  if (options?.clearJWT !== false) {
    $('#engineJWT').val('');
  }
  engineConfig.url = url;
  if (options?.defaultMethod == null || options.defaultMethod === '_main') {
    $('#engineLoginMethod').val('_main');
    $('#engineLoginPassword').show();
  } else if (options.defaultMethod === '_jwt') {
    $('#engineLoginMethod').val('_jwt');
    $('#engineLoginJWT').show();
  } else {
    $('#engineLoginMethod').val(options.defaultMethod);
  }
  $('#engineLoginMethod').removeClass('is-invalid is-valid');
  $('#engineLoginMethodForm').show();
};

$('#remoteExecution').on('change', (event) => {
  if (event.currentTarget.checked) {
    $('#engineLoginForm').show();
  } else {
    $('#engineLoginForm').hide();
  }
});

$('#engineUrl').on('input', async function onEngineUrlInput() {
  $('#engineLoginMethodForm').hide();
  $('#engineUrl').addClass('is-invalid');
  saveButton.attr('disabled', false);

  engineConfig.init();

  let enteredUrl = $(this).val().replace(/\/+$/, '');
  try {
    enteredUrl = new URL(enteredUrl).toString();
  } catch (err) {
    $('#engine-tab').tab('show');
    return;
  }
  if (!enteredUrl.endsWith('/api')) {
    if (enteredUrl.endsWith('/')) {
      enteredUrl += 'api';
    } else {
      enteredUrl += '/api';
    }
  }
  if (engineUrlAbortController) {
    engineUrlAbortController.abort();
  }
  engineUrlAbortController = new AbortController();
  fetchEngineLoginMethods(enteredUrl, { signal: engineUrlAbortController.signal });
});

$('#engineLoginMethod').on('change', async function onEngineLoginMethodInput() {
  saveButton.attr('disabled', false);
  const loginMethod = $(this).val();
  $('#engineLoginMethod').removeClass('is-invalid is-valid');
  $('#engineLoginPassword').hide();
  $('#engineLoginJWT').hide();
  if (loginMethod === '' || loginMethod == null) {
    return;
  }
  if (loginMethod === '_main' || engineConfig.ldapProviders.includes(loginMethod)) {
    $('#engineLoginPassword').show();
    return;
  }
  if (loginMethod === '_jwt') {
    $('#engineLoginJWT').show();
    return;
  }
  oAuthClient = await OAuthClient.build();
  let engineUIUrl = engineConfig.url;
  if (engineUIUrl.endsWith('/api')) {
    engineUIUrl = engineUIUrl.substring(0, engineUIUrl.length - '/api'.length);
  }
  const ncPublicKey = await oAuthClient.getB64URLEncodedPublicKey();
  const queryParams = [
    'nc_id=com.gams.miro',
    'nc_redirect_uri=/auth/engine/oauth',
    `provider=${loginMethod}`,
    `nc_public_key=${ncPublicKey}`,
  ];
  engineConfig.jwt = null;
  shell.openExternal(`${engineUIUrl}/login?${queryParams.join('&')}`);
});

ipcRenderer.on('oauth-response-received', async (_, oauthResponse) => {
  try {
    engineConfig.jwt = await oAuthClient.decryptData(
      oauthResponse.jwt,
      oauthResponse.aes_key,
      oauthResponse.aes_iv,
    );
    $('#engineLoginMethod').removeClass('is-invalid');
    $('#engineLoginMethod').addClass('is-valid');
  } catch (err) {
    __electronLog.warn(`Problems decoding JWT. Error message: ${err.message}`);
    $('#engineLoginMethod').addClass('is-invalid');
    $('#engineLoginMethod').removeClass('is-valid');
    ipcRenderer.send('show-error-msg', {
      type: 'error',
      title: 'Could not decrypt JWT',
      message: 'Problems decrypting JWT. Check logs for more info.',
    }, 'settings');
  }
});

saveButton.on('click', async () => {
  const saveSettingsFn = async () => {
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
    newConfig.remoteExecution = cbRemoteExecution.is(':checked');
    if (newConfig.remoteExecution) {
      const loginMethod = $('#engineLoginMethod').val();
      let jwt;
      if (loginMethod === '_main' || engineConfig.ldapProviders.includes(loginMethod)) {
        try {
          jwt = await getEngineJwt($('#engineUsername').val(), $('#enginePassword').val(), loginMethod, engineConfig);
        } catch (err) {
          __electronLog.info(`Failed to log in Engine user: ${err.message}`);
          $('#engine-tab').tab('show');
          if (err?.response?.status === 401) {
            $('#engineUsername').addClass('is-invalid');
            $('#enginePassword').addClass('is-invalid');
            return;
          }
          ipcRenderer.send('show-error-msg', {
            type: 'error',
            title: 'Unexpected error',
            message: 'An unexpected error occurred when logging into GAMS Engine. Please check the log for more information.',
          }, 'settings');
          return;
        }
        $('#engineUsername').removeClass('is-invalid');
        $('#enginePassword').removeClass('is-invalid');
      } else if (loginMethod === '_jwt') {
        jwt = $('#engineJWT').val();
      } else {
        jwt = engineConfig.jwt;
        if (jwt == null) {
          $('#engine-tab').tab('show');
          $('#engineLoginMethod').addClass('is-invalid');
          $('#engineLoginMethod').removeClass('is-valid');
          return;
        }
      }
      try {
        const engineUserInfo = await getEngineUserInfo(jwt, engineConfig, $('#engineNs').val().trim());
        newConfig.remoteConfig = engineUserInfo;
        $('#engineNs').removeClass('is-invalid');
        $('#engineJWT').removeClass('is-invalid');
      } catch (err) {
        __electronLog.info(`Failed to get engine user info: ${err.message}`);
        $('#engine-tab').tab('show');
        if (err instanceof EngineError) {
          if (err.field === 'namespace') {
            if (err.statusCode === 404) {
              document.getElementById('engineNsValidation').innerText = lang.engineNsValidation;
            } else {
              document.getElementById('engineNsValidation').innerText = lang.engineNsValidationPerm;
            }
            $('#engineNs').addClass('is-invalid');
            return;
          }
          if (err.field === 'jwt') {
            $('#engineJWT').addClass('is-invalid');
            return;
          }
          if (err.field === 'username') {
            $('#engineUsername').addClass('is-invalid');
          }
          __electronLog.error(`Invalid field in error object: ${err.field}`);
        }
        ipcRenderer.send('show-error-msg', {
          type: 'error',
          title: 'Unexpected error',
          message: 'An unexpected error occurred when logging into GAMS Engine. Please check the log for more information.',
        }, 'settings');
        return;
      }
    } else {
      newConfig.remoteConfig = {};
    }
    ipcRenderer.send('save-general-config', newConfig, requireRestart);
  };
  const loadingScreen = $('#loadingScreen');
  loadingScreen.show();
  await saveSettingsFn();
  loadingScreen.hide();
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
    $('.engine-input').val('');
    $('#engineLoginForm').hide();
    $('#engineLoginMethodForm').hide();
  } else if (elKey === 'logLifeTime') {
    inputLogLifetime.val(defaultValues[elKey]);
  } else if (Object.keys(optionAliasMap).includes(elKey)) {
    let inputElTmp;
    if (elKey === 'language') {
      inputElTmp = inputLanguage;
    } else if (elKey === 'colorTheme') {
      inputElTmp = inputColorTheme;
    } else {
      __electronLog.error('COULD NOT FIND INPUT EL!!');
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
      'colorThemeOptionForest', 'colorThemeOptionTawny', 'colorThemeOptionDarkBlue', 'colorThemeOptionRedWine',
      'engineUrlLabel', 'engineNsLabel', 'engineLoginMethodLabel', 'engineUsernameLabel', 'enginePasswordLabel',
      'engineJWTLabel', 'engineNsValidation', 'engineLoginMethodValidation', 'engineLoginMethodValidationSuccess',
      'engineUsernameValidation', 'enginePasswordValidation', 'engineJWTValidation'].forEach((id) => {
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
  Object.entries(data).forEach(async ([key, value]) => {
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
      if (newValue === true) {
        $('#engineLoginForm').show();
      }
      if (isImportant) {
        cbRemoteExecution.attr('disabled', true);
      }
    } else if (key === 'remoteConfig') {
      if (newValue.url != null) {
        engineConfig.init();
        $('#engineUrl').val(newValue.url);
        $('#engineNs').val(newValue.namespace);
        $('#engineJWT').val(newValue.jwt);
        fetchEngineLoginMethods(newValue.url, { defaultMethod: '_jwt', clearJWT: false });
        if (isImportant) {
          $('#engineLoginMethod').attr('disabled', true);
          $('#engineUrl').attr('disabled', true);
          $('#engineNs').attr('disabled', true);
          $('#engineJWT').attr('disabled', true);
        }
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
