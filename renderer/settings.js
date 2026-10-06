const { ipcRenderer, shell } = require('electron');
const log = require('electron-log/renderer');
window.Bootstrap = require('bootstrap');
const $ = require('jquery');
const { OAuthClient } = require('./oauth.cjs');
const {
  EngineConfig,
  getEngineAuthProviders,
  getEngineUserInfo,
  getEngineJwt,
  EngineError,
} = require('../components/engine.cjs');
const {
  testAiProviderApiKey, testAzureFoundryConnection, KEY_VALID, KEY_INVALID,
} = require('../components/ai-assistant.cjs');
const { EnvEditor } = require('./env-editor.cjs');

log.errorHandler.startCatching();

const cbLaunchExternal = $('#launchExternal');
const cbRemoteExecution = $('#remoteExecution');
const cbEnableAi = $('#enableAiAssistant');
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
  shell.openExternal(
    'https://gams.com/miro/deployment.html#sbs-customize-app',
  );
});
let oldConfig = {};
const newConfig = {};
let defaultValues;
let importantKeys;
let requireRestart = false;
let pathValidating = false;
// resolves once every in-flight AI provider API key validation has finished
let aiKeyValidation = Promise.resolve();

const aiProviders = [
  { id: 'OpenAi', name: 'openai' },
  { id: 'Anthropic', name: 'anthropic' },
  { id: 'Google', name: 'google' },
  { id: 'Azure', name: 'azure' },
];
const CUSTOM_AI_PROVIDER_NAMES = ['azure'];

const CUSTOM_AI_PROVIDERS = {
  azure: {
    name: 'azure',
    fields: {
      endpoint: 'azureEndpoint', authMode: 'azureAuthMode',
      key: 'keyAzure', tenantId: 'azureTenantId', clientId: 'azureClientId', deployments: 'azureDeployments',
    },
    // which fields each auth mode uses; the others are hidden, and neither
    // validated nor saved
    fieldsByAuthMode: {
      // recommended: the user's own Microsoft account, no stored secret at all
      userSignIn: ['endpoint', 'authMode', 'tenantId', 'clientId'],
      apiKey: ['endpoint', 'authMode', 'key'],
    },
    test: testAzureFoundryConnection,
    hasAuthModeToggle: true,
  },
};

const optionAliasMap = {
  language: {
    English: 'en',
    Deutsch: 'de',
    中文: 'cn',
    Español: 'es',
  },
  colorTheme: {
    colorThemeOptionDefault: 'default',
    colorThemeOptionBlackWhite: 'blackandwhite',
    colorThemeOptionForest: 'forest',
    colorThemeOptionTawny: 'tawny',
    colorThemeOptionDarkBlue: 'darkblue',
    colorThemeOptionRedWine: 'redwine',
    colorThemeOptionCustom: 'custom',
  },
};

const pathConfig = {
  configpath: {
    requiresRestart: true,
  },
  gamspath: {},
  pythonpath: {},
  rpath: {},
  logpath: {},
};

[
  inputLogLifetime,
  inputLanguage,
  inputColorTheme,
  inputLogLevel,
  cbLaunchExternal,
  cbRemoteExecution,
  cbEnableAi,
  $('#engineNs'),
  $('#engineUsername'),
  $('#enginePassword'),
  $('#engineJWT'),
].forEach((el) => {
  el.on('change', () => {
    saveButton.attr('disabled', false);
  });
});

[
  $('#engineNs'),
  $('#engineUsername'),
  $('#enginePassword'),
  $('#engineJWT'),
].forEach((el) => {
  el.on('input', () => {
    saveButton.attr('disabled', false);
  });
});

if (['darwin', 'win32'].includes(process.platform)) {
  // do not allow changing R path on macOS/Windows as R is bundled here
  $('#rpathWrapper').hide();
}

const envEditor = new EnvEditor(document.getElementById('envEditor'), () => {
  btEnvReset.attr('disabled', envEditor.rows().length === 0);
  saveButton.attr('disabled', false);
});

function updateEnvEditor(envData) {
  envEditor.setEnv(envData);
  btEnvReset.attr('disabled', !envData || Object.keys(envData).length === 0);
}

updateEnvEditor();

$('#btEnvReset').on('click', () => {
  updateEnvEditor();
  saveButton.attr('disabled', false);
});

const fetchEngineLoginMethods = async (url, options) => {
  $('#engineLoginPassword').hide();
  $('#engineLoginJWT').hide();
  $('#engineLoginMethod')
    .empty()
    .append(
      $('<option>', {
        value: '_main',
        text: lang.engineLoginMethodUserPass,
      }),
      $('<option>', {
        value: '_jwt',
        text: 'JWT',
      }),
    );
  try {
    const authProvidersTmp = await getEngineAuthProviders(
      url,
      options?.signal,
    );
    engineConfig.oauthProviders = authProvidersTmp
      .filter((idp) => {
        if (idp?.oidc != null || idp?.oauth != null) {
          $('#engineLoginMethod').append(
            $('<option>', {
              value: idp.name,
              text: idp.label,
            }),
          );
          return true;
        }
        return false;
      })
      .map((idp) => idp.name);
    engineConfig.ldapProviders = authProvidersTmp
      .filter((idp) => {
        if (idp?.is_ldap_identity_provider === true) {
          $('#engineLoginMethod').append(
            $('<option>', {
              value: idp.name,
              text: idp.label,
            }),
          );
          return true;
        }
        return false;
      })
      .map((idp) => idp.name);
  } catch (err) {
    if (err?.name !== 'CanceledError') {
      __electronLog.info(
        `Problems fetching auth providers (url: ${url}). Error: ${JSON.stringify(err)}`,
      );
    }
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
  fetchEngineLoginMethods(enteredUrl, {
    signal: engineUrlAbortController.signal,
  });
});

$('#engineLoginMethod').on(
  'change',
  async function onEngineLoginMethodInput() {
    saveButton.attr('disabled', false);
    const loginMethod = $(this).val();
    $('#engineLoginMethod').removeClass('is-invalid is-valid');
    $('#engineLoginPassword').hide();
    $('#engineLoginJWT').hide();
    if (loginMethod === '' || loginMethod == null) {
      return;
    }
    if (
      loginMethod === '_main' ||
      engineConfig.ldapProviders.includes(loginMethod)
    ) {
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
      engineUIUrl = engineUIUrl.substring(
        0,
        engineUIUrl.length - '/api'.length,
      );
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
  },
);

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
    ipcRenderer.send(
      'show-error-msg',
      {
        type: 'error',
        title: 'Could not decrypt JWT',
        message: 'Problems decrypting JWT. Check logs for more info.',
      },
      'settings',
    );
  }
});

saveButton.on('click', async () => {
  const saveSettingsFn = async () => {
    if (pathValidating === true) {
      return;
    }
    await aiKeyValidation;
    let logLifeVal = inputLogLifetime.val();
    if (logLifeVal !== '') {
      logLifeVal = parseInt(logLifeVal, 10);
      if (Number.isNaN(logLifeVal)) {
        return;
      }
    }
    if (!envEditor.disabled) {
      if (!envEditor.validate()) {
        $('#env-tab').tab('show');
        envEditor.focusFirstError();
        return;
      }
      newConfig.miroEnv = envEditor.getEnv();
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
    newConfig.remoteExecution =
      cbRemoteExecution.is(':checked') && engineConfig.url != null;
    if (newConfig.remoteExecution) {
      const loginMethod = $('#engineLoginMethod').val();
      let jwt;
      if (
        loginMethod === '_main' ||
        engineConfig.ldapProviders.includes(loginMethod)
      ) {
        try {
          jwt = await getEngineJwt(
            $('#engineUsername').val(),
            $('#enginePassword').val(),
            loginMethod,
            engineConfig,
          );
        } catch (err) {
          __electronLog.info(`Failed to log in Engine user: ${err.message}`);
          $('#engine-tab').tab('show');
          if (err?.response?.status === 401) {
            $('#engineUsername').addClass('is-invalid');
            $('#enginePassword').addClass('is-invalid');
            return;
          }
          ipcRenderer.send(
            'show-error-msg',
            {
              type: 'error',
              title: 'Unexpected error',
              message:
                'An unexpected error occurred when logging into GAMS Engine. Please check the log for more information.',
            },
            'settings',
          );
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
        const engineUserInfo = await getEngineUserInfo(
          jwt,
          engineConfig,
          $('#engineNs').val().trim(),
        );
        newConfig.remoteConfig = engineUserInfo;
        $('#engineNs').removeClass('is-invalid');
        $('#engineJWT').removeClass('is-invalid');
      } catch (err) {
        __electronLog.info(`Failed to get engine user info: ${err.message}`);
        $('#engine-tab').tab('show');
        if (err instanceof EngineError) {
          if (err.field === 'namespace') {
            if (err.statusCode === 404) {
              document.getElementById('engineNsValidation').innerText =
                lang.engineNsValidation;
            } else {
              document.getElementById('engineNsValidation').innerText =
                lang.engineNsValidationPerm;
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
        ipcRenderer.send(
          'show-error-msg',
          {
            type: 'error',
            title: 'Unexpected error',
            message:
              'An unexpected error occurred when logging into GAMS Engine. Please check the log for more information.',
          },
          'settings',
        );
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
  $(`#btPathSelect_${id}`).siblings('label').text(pathSelected);

  if (pathConfig[id].requiresRestart === true) {
    requireRestart = true;
  }
});

Object.keys(pathConfig).forEach((id) => {
  $(`#btPathSelect_${id}`).on('click', genPathSelectHandler(id));

  $(`#btPathSelect_${id}`)
    .siblings('.btn-reset')
    .on('click', function resetClickPath() {
      const elKey = this.dataset.key;
      newConfig[elKey] = '';
      saveButton.attr('disabled', false);
      if (
        Object.keys(pathConfig).find(
          (id2) => id2 === elKey && pathConfig[id2].requiresRestart === true,
        )
      ) {
        requireRestart = true;
      }
      const $this = $(this);
      $this.siblings('label').text(defaultValues[elKey]);
      $this.hide();
    });
});
const disableAiAssistant = () => {
  newConfig.aiAssistantConfig = {};
  $('#aiAssistantForm').hide();
  $('.ai-key-status').text(lang['ai-key-status-not-configured']).attr('class', 'ai-key-status badge badge-secondary');
  $('.ai-key-input').val('').removeClass('is-valid is-invalid');
  // a <select> is not covered by the .ai-key-input value clear above, and only
  // Azure has one, so this is a no-op for every other custom provider
  Object.values(CUSTOM_AI_PROVIDERS).forEach((provider) => {
    if (provider.hasAuthModeToggle) {
      const authModeField = document.getElementById(provider.fields.authMode);
      authModeField.value = 'userSignIn';
      authModeField.dispatchEvent(new Event('change'));
    }
  });
};
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
    inputElTmp.val(
      Object.keys(optionAliasMap[elKey]).find(
        (key) => optionAliasMap[elKey][key] === defaultValues[elKey],
      ),
    );
  } else if (elKey === 'logLevel') {
    inputLogLevel.val(defaultValues[elKey]);
  } else if (elKey === 'enableAiAssistant') {
    // not a config key of its own: the checkbox reflects whether any API key is
    // configured, so resetting it means dropping the whole AI config
    delete newConfig[elKey];
    cbEnableAi.prop('checked', false);
    disableAiAssistant();
  }
  $(this).hide();
});

// AI Assistant Tab
cbEnableAi.on('change', (event) => {
  if (event.currentTarget.checked) {
    ipcRenderer.send('show-confirm-enable-ai-dialog');
  } else {
    disableAiAssistant();
  }
});
ipcRenderer.on(
  'confirm-ai-response-received', (_, enableAi) => {
    if (enableAi !== true) {
      cbEnableAi.prop('checked', false);
      return;
    }
    $('#aiAssistantForm').show();
  })
aiProviders.forEach(provider => {
  if (CUSTOM_AI_PROVIDER_NAMES.includes(provider.name)) {
    return;
  }
  const inputElement = document.getElementById(`key${provider.id}`);
  const badgeElement = document.getElementById(`status${provider.id}`);

  const storedKey = () => newConfig.aiAssistantConfig?.apiKeys?.find(
    key => key.provider === provider.name
  )?.key;
  const dropStoredKey = () => {
    const index = newConfig.aiAssistantConfig?.apiKeys?.findIndex(
      key => key.provider === provider.name
    ) ?? -1;
    if (index !== -1) {
      newConfig.aiAssistantConfig.apiKeys.splice(index, 1);
    }
  };

  const validateKey = async (e) => {
    const keyValue = e.target.value.trim();

    if (keyValue && keyValue === storedKey()) {
      // unchanged, and a key only ever reaches newConfig after it validated, so
      // merely focusing and leaving the field must not trigger another round trip
      inputElement.classList.remove('is-invalid');
      inputElement.classList.add('is-valid');
      badgeElement.className = 'ai-key-status badge badge-success';
      badgeElement.innerText = lang['ai-key-status-connected'];
      return;
    }
    inputElement.classList.remove('is-valid', 'is-invalid');

    if (!keyValue) {
      // the user cleared the field: that is an explicit request to remove the key
      dropStoredKey();
      badgeElement.className = 'ai-key-status badge badge-secondary';
      badgeElement.innerText = lang['ai-key-status-not-configured'];
      saveButton.attr('disabled', false);
      return;
    }

    badgeElement.className = 'ai-key-status badge badge-info';
    badgeElement.innerText = lang['ai-key-status-validating'];

    try {
      __electronLog.debug(`Validating API key for AI provider: ${provider.name}`)
      const keyStatus = await testAiProviderApiKey(provider.name, keyValue);

      if (keyStatus === KEY_VALID) {
        __electronLog.debug(`Valid API key for AI provider: ${provider.name} added!`)
        inputElement.classList.add('is-valid');
        badgeElement.className = 'ai-key-status badge badge-success';
        badgeElement.innerText = lang['ai-key-status-connected'];
        newConfig.aiAssistantConfig ??= {};
        newConfig.aiAssistantConfig.apiKeys ??= [];
        const existing = newConfig.aiAssistantConfig.apiKeys.find(
          k => k.provider === provider.name
        );
        if (existing) {
          existing.key = keyValue;
        } else {
          newConfig.aiAssistantConfig.apiKeys.push({
            key: keyValue,
            provider: provider.name
          });
        }
      } else if (keyStatus === KEY_INVALID) {
        // the provider rejected it, so it must not end up in the config
        __electronLog.debug(`Invalid API key for AI provider: ${provider.name}`)
        dropStoredKey();
        inputElement.classList.add('is-invalid');
        badgeElement.className = 'ai-key-status badge badge-danger';
        badgeElement.innerText = lang['ai-key-status-invalid'];
      } else {
        // we could not reach the provider, so we know nothing about the key; leave
        // whatever is stored alone rather than discarding a working one
        __electronLog.warn(`Could not reach AI provider to validate key: ${provider.name}`)
        inputElement.classList.add('is-invalid');
        badgeElement.className = 'ai-key-status badge badge-danger';
        badgeElement.innerText = lang['ai-key-status-error'];
      }
    } catch (err) {
      __electronLog.warn(`Failed validating API key for AI provider: ${provider.name}. Error: ${err}`)
      inputElement.classList.add('is-invalid');
      badgeElement.className = 'ai-key-status badge badge-danger';
      badgeElement.innerText = lang['ai-key-status-error'];
    } finally {
      saveButton.attr('disabled', false);
    }
  };

  // validateKey updates newConfig only once it knows the outcome, so saving must
  // wait for any validation still in flight
  inputElement.addEventListener('blur', (e) => {
    aiKeyValidation = Promise.allSettled([aiKeyValidation, validateKey(e)]);
  });
});

const parseCustomProviderDeployments = (raw) => raw
  .split('\n')
  .map((line) => line.trim())
  .filter((line) => line !== '')
  .map((line) => {
    const separator = line.indexOf('=');
    if (separator === -1) {
      return { id: line };
    }
    return {
      label: line.slice(0, separator).trim(),
      id: line.slice(separator + 1).trim(),
    };
  })
  .filter((deployment) => deployment.id !== '');

function registerCustomProvider({
  name, fields: fieldIds, fieldsByAuthMode, test, hasAuthModeToggle,
}) {
  const fields = Object.fromEntries(
    Object.entries(fieldIds).map(([key, id]) => [key, document.getElementById(id)]),
  );
  const badge = document.getElementById(`status${name[0].toUpperCase()}${name.slice(1)}`);
  const inputSelector = `.ai-${name}-input`;

  const fieldsInUse = (authMode) => fieldsByAuthMode?.[authMode]
    ?? Object.keys(fields).filter((key) => key !== 'deployments');
  const requiredFields = (authMode) => fieldsInUse(authMode).filter((key) => key !== 'authMode');
  const isUserSignIn = () => fields.authMode?.value === 'userSignIn';

  // Microsoft sign-in (Azure only). The browser flow, the token cache and the
  // tokens themselves live in the main process (components/azure-user-auth.cjs).
  const signInRow = document.getElementById(`${name}SignInRow`);
  const signInStatus = document.getElementById(`${name}SignInStatus`);
  const signInButton = document.getElementById(`${name}SignInButton`);
  const signOutButton = document.getElementById(`${name}SignOutButton`);
  const signInConfig = () => ({ tenantId: fields.tenantId.value.trim(), clientId: fields.clientId.value.trim() });
  const showSignedInUser = (username) => {
    signInStatus.textContent = username
      ? `${lang['ai-azure-signed-in-as'] ?? 'Signed in as'} ${username}`
      : (lang['ai-azure-not-signed-in'] ?? 'Not signed in');
    $(signOutButton).toggle(Boolean(username));
  };
  const refreshSignInStatus = async () => {
    if (!signInRow || !isUserSignIn()) return;
    const config = signInConfig();
    if (!config.tenantId || !config.clientId) {
      showSignedInUser(null);
      return;
    }
    try {
      showSignedInUser(await ipcRenderer.invoke(`${name}-signed-in-user`, config));
    } catch (err) {
      showSignedInUser(null);
    }
  };

  // blank fields and those the selected auth mode does not use are left out:
  // the config schema rejects empty strings, and a hidden stale value must
  // not end up in the saved config
  const readConfig = () => {
    const values = Object.fromEntries(
      Object.entries(fields)
        .filter(([key]) => key !== 'deployments')
        .map(([key, el]) => [key, el.value.trim()]),
    );
    return Object.fromEntries(
      fieldsInUse(values.authMode)
        .filter((key) => values[key] !== '')
        .map((key) => [key, values[key]]),
    );
  };

  if (hasAuthModeToggle) {
    const authModeChanged = () => {
      const authMode = fields.authMode.value;
      const used = fieldsInUse(authMode);
      Object.entries(fields).forEach(([key, el]) => {
        if (key !== 'authMode' && key !== 'deployments') {
          $(el).toggle(used.includes(key));
        }
      });
      $(signInRow).toggle(authMode === 'userSignIn');
      refreshSignInStatus();
    };
    fields.authMode.addEventListener('change', authModeChanged);
    authModeChanged();
    fields.tenantId.addEventListener('change', refreshSignInStatus);
    fields.clientId.addEventListener('change', refreshSignInStatus);
  }
  const stored = () => newConfig.aiAssistantConfig?.apiKeys?.find(
    (entry) => entry.provider === name,
  );
  const dropStored = () => {
    const index = newConfig.aiAssistantConfig?.apiKeys?.findIndex(
      (entry) => entry.provider === name,
    ) ?? -1;
    if (index !== -1) {
      newConfig.aiAssistantConfig.apiKeys.splice(index, 1);
    }
  };
  const setStatus = (statusKey, valid) => {
    const variant = { connected: 'success', invalid: 'danger', error: 'danger' }[statusKey]
      ?? 'secondary';
    badge.className = `ai-key-status badge badge-${variant}`;
    badge.innerText = lang[`ai-key-status-${statusKey}`];
    $(inputSelector).removeClass('is-valid is-invalid');
    if (valid !== undefined) {
      $(inputSelector).addClass(valid ? 'is-valid' : 'is-invalid');
    }
  };

  const validate = async () => {
    const config = readConfig();
    const deployments = parseCustomProviderDeployments(fields.deployments.value);
    const isComplete = requiredFields(config.authMode).every((key) => config[key])
      && deployments.length > 0;
    const isEmpty = requiredFields(config.authMode).every((key) => !config[key])
      && deployments.length === 0;

    if (isEmpty) {
      // an entirely empty form is an explicit request to remove the configuration
      dropStored();
      setStatus('not-configured');
      saveButton.attr('disabled', false);
      return;
    }
    if (!isComplete) {
      // still being filled in: say nothing rather than reporting a bad credential
      setStatus('not-configured');
      saveButton.attr('disabled', false);
      return;
    }

    const previous = stored();
    const unchanged = previous
      && Object.keys(config).every((key) => previous[key] === config[key])
      && JSON.stringify(previous.models?.map((m) => m.id) ?? [])
        === JSON.stringify(deployments.map((d) => d.id));
    if (unchanged) {
      // nothing changed, and a config only reaches newConfig after it
      // validated, so merely tabbing through the fields must not re-probe
      setStatus('connected', true);
      return;
    }

    let accessToken;
    if (config.authMode === 'userSignIn') {
      // silent only: the browser opens from the sign-in button, never on blur
      accessToken = await ipcRenderer.invoke(`${name}-access-token`, signInConfig()).catch(() => null);
      if (!accessToken) {
        // cannot verify without a signed-in user, but that says nothing
        // about the configuration itself, so a stored one is kept
        setStatus('not-configured');
        showSignedInUser(null);
        saveButton.attr('disabled', false);
        return;
      }
    }

    setStatus('validating');
    saveButton.attr('disabled', true);
    try {
      __electronLog.debug(`Validating ${name} connection`);
      const { status, models } = await test(config, deployments, { accessToken });

      if (status === KEY_VALID) {
        __electronLog.debug(`${name} connection valid, ${models.length} deployment(s)`);
        setStatus('connected', true);
        newConfig.aiAssistantConfig ??= {};
        newConfig.aiAssistantConfig.apiKeys ??= [];
        dropStored();
        newConfig.aiAssistantConfig.apiKeys.push({ provider: name, ...config, models });
        // deployments found unusable are dropped, so show what was stored
        fields.deployments.value = models
          .map((model) => (model.label === model.id ? model.id : `${model.label} = ${model.id}`))
          .join('\n');
      } else if (status === KEY_INVALID) {
        __electronLog.debug(`${name} rejected the configuration`);
        dropStored();
        setStatus('invalid', false);
      } else {
        __electronLog.warn(`Could not reach ${name} to validate the configuration`);
        setStatus('error', false);
      }
    } catch (err) {
      __electronLog.warn(`Failed validating ${name} configuration. Error: ${err}`);
      setStatus('error', false);
    } finally {
      saveButton.attr('disabled', false);
    }
  };

  Object.values(fields).forEach((field) => {
    field.addEventListener('blur', () => {
      aiKeyValidation = Promise.allSettled([aiKeyValidation, validate()]);
    });
  });

  if (signInButton) {
    signInButton.addEventListener('click', async () => {
      const config = signInConfig();
      if (!config.tenantId || !config.clientId) {
        signInStatus.textContent = lang['ai-azure-sign-in-missing-ids']
          ?? 'Enter the tenant ID and application (client) ID first.';
        return;
      }
      signInButton.disabled = true;
      signInStatus.textContent = lang['ai-azure-signing-in'] ?? 'Complete the sign-in in your browser...';
      try {
        showSignedInUser(await ipcRenderer.invoke(`${name}-sign-in`, config));
        aiKeyValidation = Promise.allSettled([aiKeyValidation, validate()]);
      } catch (err) {
        __electronLog.warn(`Microsoft sign-in failed: ${err.message}`);
        // strip Electron's "Error invoking remote method '...': Error: " prefix
        const reason = String(err.message).replace(/^Error invoking remote method '[^']*': (\w*Error: )?/, '');
        signInStatus.textContent = `${lang['ai-azure-sign-in-failed'] ?? 'Sign-in failed.'} ${reason}`;
      } finally {
        signInButton.disabled = false;
      }
    });
    signOutButton.addEventListener('click', async () => {
      await ipcRenderer.invoke(`${name}-sign-out`, signInConfig()).catch(() => {});
      showSignedInUser(null);
      setStatus('not-configured');
    });
  }
}

Object.values(CUSTOM_AI_PROVIDERS).forEach(registerCustomProvider);

ipcRenderer.on(
  'settings-loaded',
  (e, data, defaults, langData, customColorsFileExists) => {
    __electronLog.info('Settings window config loaded');
    if (langData != null && lang.title == null) {
      lang = langData;
      [
        'colorThemeOptionDefault',
        'colorThemeOptionBlackWhite',
        'colorThemeOptionForest',
        'colorThemeOptionTawny',
        'colorThemeOptionDarkBlue',
        'colorThemeOptionRedWine',
        'colorThemeOptionCustom',
      ].forEach((id) => {
        optionAliasMap.colorTheme[lang[id]] = optionAliasMap.colorTheme[id];
        delete optionAliasMap.colorTheme[id];
      });
      [
        'title',
        'general-tab',
        'paths-tab',
        'env-tab',
        'ai-tab',
        'launchBrowser',
        'browserReset',
        'generalLanguage',
        'languageReset',
        'generalRemoteExec',
        'remoteExecReset',
        'generalLogging',
        'loggingReset',
        'generalLoglife',
        'loglifeReset',
        'pathMiroapp',
        'pathMiroappSelect',
        'resetPathMiroapp',
        'pathGams',
        'pathGamsSelect',
        'pathGamsReset',
        'pathPython',
        'pathPythonSelect',
        'pathPythonReset',
        'pathLog',
        'pathLogSelect',
        'pathLogReset',
        'pathR',
        'pathRSelect',
        'pathRReset',
        'needHelp',
        'btSave',
        'btEnvReset',
        'envInfo',
        'generalColorTheme',
        'colorThemeReset',
        'colorThemeOptionDefault',
        'colorThemeOptionBlackWhite',
        'colorThemeOptionForest',
        'colorThemeOptionTawny',
        'colorThemeOptionDarkBlue',
        'colorThemeOptionRedWine',
        'colorThemeOptionCustom',
        'engineUrlLabel',
        'engineNsLabel',
        'engineLoginMethodLabel',
        'engineUsernameLabel',
        'enginePasswordLabel',
        'engineJWTLabel',
        'engineNsValidation',
        'engineLoginMethodValidation',
        'engineLoginMethodValidationSuccess',
        'engineUsernameValidation',
        'enginePasswordValidation',
        'engineJWTValidation',
        'enableAiAssistantLabel',
        'aiAssistantInfo'
      ].forEach((id) => {
        const el = document.getElementById(id);
        if (el) {
          el.innerText = lang[id];
        }
      });
      ['settings-reset',
        'ai-key-status-invalid',
        'ai-key-status-connected',
        'ai-no-key-info',
        'ai-no-key-link',
        'ai-azure-auth-key',
        'ai-azure-auth-user',
        'ai-azure-sign-in',
        'ai-azure-sign-out',
        'ai-custom-provider-deployments-info'].forEach((className) => {
          $(`.${className}`).text(lang[className]);
        });
      $('.ai-key-status').text(lang['ai-key-status-not-configured']);
      envEditor.setLabels(lang);
      document.getElementById('btCancel').value = lang.btCancel;
      [
        'pathMiroappSelect',
        'pathGamsSelect',
        'pathPythonSelect',
        'pathLogSelect',
        'pathRSelect',
      ].forEach((id) => {
        const el = document.getElementById(id);
        if (el) {
          $(el).addClass('browseLang').attr('content-after', lang.browseFiles);
        }
      });
      if (customColorsFileExists !== true) {
        $('#colorThemeOptionCustom').remove();
      }
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
        if (
          [
            'launchExternal',
            'remoteExecution',
            'logLifeTime',
            'language',
            'colorTheme',
            'logLevel',
          ].includes(key)
        ) {
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
        if (newValue?.url != null) {
          engineConfig.init();
          $('#engineUrl').val(newValue.url);
          $('#engineNs').val(newValue.namespace);
          $('#engineJWT').val(newValue.jwt);
          fetchEngineLoginMethods(newValue.url, {
            defaultMethod: '_jwt',
            clearJWT: false,
          });
          if (isImportant) {
            $('#engineLoginMethod').attr('disabled', true);
            $('#engineUrl').attr('disabled', true);
            $('#engineNs').attr('disabled', true);
            $('#engineJWT').attr('disabled', true);
          }
        }
      } else if (key === 'aiAssistantConfig') {
        newConfig.aiAssistantConfig = newValue;
        const aiEnabled = newValue?.apiKeys?.length > 0;
        newValue?.apiKeys?.forEach(apiKey => {
          const provider = aiProviders.find(
            el => el.name === apiKey.provider
          );
          if (provider == null) {
            __electronLog.warn(`Unknown AI provider in config: ${apiKey.provider}. Skipping.`);
            return;
          }
          const badgeElement = document.getElementById(`status${provider.id}`);
          const customProvider = CUSTOM_AI_PROVIDERS[apiKey.provider];
          if (customProvider != null) {
            // several fields rather than a single key, so it cannot go through
            // the `key${provider.id}` lookup the other providers use
            Object.entries(customProvider.fields).forEach(([configKey, fieldId]) => {
              const el = document.getElementById(fieldId);
              if (configKey === 'deployments') {
                el.value = (apiKey.models ?? [])
                  .map((model) => (!model.label || model.label === model.id
                    ? model.id : `${model.label} = ${model.id}`))
                  .join('\n');
              } else if (configKey === 'authMode') {
                el.value = apiKey.authMode ?? 'userSignIn';
              } else {
                el.value = apiKey[configKey] ?? '';
              }
            });
            if (customProvider.hasAuthModeToggle) {
              document.getElementById(customProvider.fields.authMode).dispatchEvent(new Event('change'));
            }
            $(`.ai-${apiKey.provider}-input`).addClass('is-valid');
          } else {
            const inputElement = document.getElementById(`key${provider.id}`);
            inputElement.value = apiKey.key;
            inputElement.classList.add('is-valid');
          }
          badgeElement.className = 'ai-key-status badge badge-success';
          badgeElement.innerText = lang['ai-key-status-connected'];
        })
        cbEnableAi.prop('checked', aiEnabled);
        if (isImportant) {
          cbEnableAi.attr('disabled', true);
        }
        if (aiEnabled) {
          $('[data-key="enableAiAssistant"]').show();
          $('#aiAssistantForm').show();
        } else {
          $('[data-key="enableAiAssistant"]').hide();
          $('#aiAssistantForm').hide();
        }
      } else if (
        ['logLifeTime', 'logLevel', 'language', 'colorTheme'].find(
          (el) => el === key,
        )
      ) {
        $(`#${key}`).val(
          Object.keys(optionAliasMap).includes(key)
            ? Object.keys(optionAliasMap[key]).find(
              (keyp) => optionAliasMap[key][keyp] === newValue,
            )
            : newValue,
        );
        if (isImportant) {
          $(`#${key}`).attr('disabled', true);
        }
      } else if (key === 'miroEnv') {
        updateEnvEditor(newValue);
        if (isImportant) {
          envEditor.setDisabled(true);
          btEnvReset.attr('disabled', true);
        }
      } else {
        const pathSelectEl = $(`#btPathSelect_${key}`);
        if (isImportant) {
          pathSelectEl.addClass('path-disabled');
        }
        pathSelectEl.siblings('label').text(newValue);
      }
    });
  },
);
