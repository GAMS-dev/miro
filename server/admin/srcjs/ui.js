import bootbox from 'bootbox';
import { state, dom } from './state';
import { escapeHtml, indirectEval, unicodeToHTMLID } from './utils';
import {
  ADD_APP_WRAPPER_HTML,
  PLACEHOLDERS,
  SUPPORTED_FILE_TYPES,
} from './constants';
import { sendUpdateAppShinyEvent } from './api';

export function exitOverlayMode() {
  if ($('#expandedAddAppWrapper').is(':visible')) {
    $('#addAppWrapper').html(ADD_APP_WRAPPER_HTML);
  }
  if (dom.$overlay.is(':visible')) {
    dom.$overlay.hide();
    dom.$overlay.data('current').css('z-index', 1);
  }
}

export function registerSelectizeInputs(selector = '') {
  let selectizeSelector = 'select';
  if (selector !== '') {
    selectizeSelector = `${selector} select`;
  }
  const $el = $(selectizeSelector);
  $el.each(function () {
    const config = $el
      .parent()
      .find(`script[data-for="${escapeHtml(this.id)}"]`);
    let options = {
      persist: false,
    };
    if (config.length > 0) {
      options = $.extend(
        {
          labelField: 'label',
          valueField: 'value',
          searchField: ['label'],
        },
        JSON.parse(config.html()),
      );
      if (config.data('eval') instanceof Array) {
        $.each(config.data('eval'), (i, x) => {
          options[x] = indirectEval(`(${options[x]})`);
        });
      }
    }
    $(this).selectize(options);
  });
}

export const AI_PROVIDERS = [
  {
    id: 'OpenAI',
    name: 'openai',
    label: 'OpenAI',
    url: 'https://platform.openai.com/api-keys',
    placeholder: 'sk-proj-...',
  },
  {
    id: 'Google',
    name: 'google',
    label: 'Google',
    url: 'https://aistudio.google.com/app/apikey',
    placeholder: 'AIzaSy...',
  },
  {
    id: 'Anthropic',
    name: 'anthropic',
    label: 'Anthropic',
    url: 'https://console.anthropic.com/settings/keys',
    placeholder: 'sk-ant-...',
  },
];

// Callers must be able to tell "the provider rejected this key" apart from "we could
// not reach the provider", so that a network failure is not reported as a bad key.
const KEY_VALID = 'valid';
const KEY_INVALID = 'invalid';
const KEY_UNREACHABLE = 'unreachable';
// longer than the server's request timeout, only covers a lost reply
const KEY_VALIDATION_TIMEOUT = 20000;

const pendingKeyValidations = new Map();
let lastKeyValidationId = 0;

export function resolveAiKeyValidation({ requestId, status }) {
  const resolve = pendingKeyValidations.get(requestId);
  if (resolve) {
    pendingKeyValidations.delete(requestId);
    resolve(status);
  }
}

const testAiProviderApiKey = (provider, apiKey) => new Promise((resolve) => {
  lastKeyValidationId += 1;
  const requestId = lastKeyValidationId;
  pendingKeyValidations.set(requestId, resolve);
  setTimeout(
    () => resolveAiKeyValidation({ requestId, status: KEY_UNREACHABLE }),
    KEY_VALIDATION_TIMEOUT,
  );
  Shiny.setInputValue('validateAiApiKey', { requestId, provider, key: apiKey }, {
    priority: 'event',
  });
});

const REMOVE_ICON = '<svg viewBox="0 0 16 16" width="14" height="14" aria-hidden="true">'
  + '<path d="M4 4l8 8M12 4l-8 8" stroke="currentColor" stroke-width="1.6" '
  + 'stroke-linecap="round" fill="none"/></svg>';

export function openAdvancedSettingsDialog() {
  const appEnvBtn = $(this);
  const appKey = appEnvBtn.data('id') ?? '~$_newApp';
  const initialData = state.appEnvironment[appKey] ?? {};
  const data = { ...initialData };
  let aiConfig = { apiKeys: [] };
  try {
    if (data.MIRO_AI_ASSISTANT_CONFIG?.value) {
      aiConfig = JSON.parse(data.MIRO_AI_ASSISTANT_CONFIG.value);
    }
  } catch (e) {
    console.warn('Failed to parse MIRO_AI_ASSISTANT_CONFIG JSON.', e);
  }
  const maxEnvVars = 47;

  const createRow = (
    envName = '',
    description = '',
    value = '',
    isEditable = true,
    isFirst = false,
  ) => {
    const rowId = `row-${Date.now()}`;
    return `
              <tr id="${rowId}">
                  <td><input type="text" class="form-control env-name" value="${escapeHtml(envName)}" placeholder="Environment Name" ${isEditable ? '>' : `style="display:none">${escapeHtml(envName)}`}</td>
                  <td><textarea type="text" class="form-control env-description" placeholder="Description" rows="1">${escapeHtml(description)}</textarea></td>
                  <td>
                      <div class="input-group">
                          <input type="password" class="form-control env-value" value="${escapeHtml(value)}" placeholder="Value">
                          <button type="button" class="input-group-text toggle-visibility" aria-label="Show value" title="Show value">
                            <i class="fas fa-eye"></i>
                          </button>
                      </div>
                  </td>
                  <td>
                      ${!isEditable || !isFirst ? `<button class="env-remove-btn remove-row" type="button" aria-label="Remove variable" title="Remove variable">${REMOVE_ICON}</button>` : ''}
                  </td>
              </tr>`;
  };

  const getCurrentAccessPerm = (accessPerm) => {
    if (accessPerm?.value == null || accessPerm.value === '') {
      return [];
    }
    return accessPerm.value.split(',');
  };

  const getAccessPermSelectorHtml = (
    id,
    choices,
    selected,
  ) => `<div class="access-perm-selector-container">
        <select id="${id}" multiple="multiple" placeholder="Owner of the Scenario"><option value=""></option></select>
          <script type="application/json"
            data-for="${id}"
            data-eval="[&quot;sortField&quot;,&quot;items&quot;,&quot;render&quot;]">
            {"valueField":"value","labelField":"value","searchField":"value",
              "options":${JSON.stringify(
    choices.groups
      .map((groupName) => ({
        isGroup: true,
        value: `#${groupName}`,
      }))
      .sort((i1, i2) => i1.value.localeCompare(i2.value))
      .concat(
        choices.users
          .map((userName) => ({
            isGroup: false,
            value: userName,
          }))
          .sort((i1, i2) => i1.value.localeCompare(i2.value)),
      ),
  )},
            "items":${JSON.stringify(JSON.stringify(selected))},
            "sortField":"function(i1,i2) {return i1.value.localeCompare(i2.value);}",
            "render":"{option: function(d,e){return '<div>'+(d.isGroup?'<i class=\\"fas fa-users\\"><\\/i> '+e(d.value.substring(1)):'<i class=\\"fas fa-user\\"><\\/i> '+e(d.value))+'<\\/div>';},item:function(d,e){return '<div>'+(d.isGroup?'<i class=\\"fas fa-users\\"><\\/i> '+e(d.value.substring(1)):'<i class=\\"fas fa-user\\"><\\/i> '+e(d.value))+'<\\/div>';}}",
            "plugins":["selectize-plugin-a11y"]}
          </script>
        </div>`;

  const dialogContent = `
          <div style="white-space:normal">
            <ul class="nav nav-tabs" id="confTab" role="tablist">
              <li class="nav-item" role="presentation">
                <button class="nav-link active" id="conf-environment-tab" data-toggle="tab" data-target="#conf-environment" type="button" role="tab" aria-controls="conf-environment" aria-selected="true">Environment</button>
              </li>
              <li class="nav-item" role="presentation">
                <button class="nav-link" id="conf-permissions-tab" data-toggle="tab" data-target="#conf-permissions" type="button" role="tab" aria-controls="conf-permissions" aria-selected="false">Scenario Permissions</button>
              </li>
              <li class="nav-item" role="presentation">
                <button class="nav-link" id="conf-ai-assistant-tab" data-toggle="tab" data-target="#conf-ai-assistant" type="button" role="tab" aria-controls="conf-ai-assistant" aria-selected="false">Mira</button>
              </li>
            </ul>
            <div class="tab-content pt-3" id="confTabContent">
              <div id="form-error" class="text-danger mb-2"></div>
              <div class="tab-pane fade show active" id="conf-environment" role="tabpanel" aria-labelledby="conf-environment-tab">
                <table class="table settings-table">
                  <thead>
                      <tr>
                          <th>Name</th>
                          <th>Description</th>
                          <th>Value</th>
                          <th class="settings-table-action"><span class="sr-only">Actions</span></th>
                      </tr>
                  </thead>
                  <tbody id="env-rows">
                      ${Object.keys(data)
    .filter(
      (envName) => !['READ', 'WRITE', 'EXECUTE']
        .map(
          (permType) => `MIRO_DEFAULT_SCEN_PERM_${permType}`,
        )
        .includes(envName)
          && envName !== 'MIRO_AI_ASSISTANT_CONFIG',
    )
    .map((envName) => createRow(
      escapeHtml(envName),
      escapeHtml(data[envName].description ?? ''),
      escapeHtml(data[envName].value),
      false,
    ))
    .join('')}
                      ${createRow('', '', '', true, true)}
                  </tbody>
                </table>
              </div>
              <div class="tab-pane fade" id="conf-permissions" role="tabpanel" aria-labelledby="conf-permissions-tab">
                <table class="table settings-table">
                  <thead>
                      <tr>
                          <th>Type</th>
                          <th>Value</th>
                      </tr>
                  </thead>
                  <tbody id="env-rows-perm">
                    ${['Read', 'Write', 'Execute'].reduce(
    (
      permSelectorHTML,
      permType,
    ) => `${permSelectorHTML}<tr id="scenPerm${permType}">
                          <td>Default ${permType} Permissions <i class="fas fa-info-circle" rel="tooltip" title="Default ${permType.toLowerCase()} permissions when saving scenario to database. Scenario owner is always added to this list."></i></td>
                          <td>
                            ${getAccessPermSelectorHtml(
    `scenPerm${permType}Selector`,
    state.scenPermAccessGroups,
    getCurrentAccessPerm(
      data[
        `MIRO_DEFAULT_SCEN_PERM_${permType.toUpperCase()}`
      ],
    ),
  )}
                          </td>
                        </tr>`,
    '',
  )}
                  </tbody>
                </table>
              </div>

              <div class="tab-pane fade" id="conf-ai-assistant" role="tabpanel" aria-labelledby="conf-ai-assistant-tab">
                <p class="small">
                  Enter your API keys to enable various AI models for Mira, the GAMS MIRO AI assistant.
                  Keys are securely verified automatically when you finish typing and click outside the field.
                </p>
                <table class="table settings-table">
                  <thead>
                      <tr>
                          <th style="width: 20%">AI Provider</th>
                          <th>Token</th>
                          <th style="width: 15%">Status</th>
                      </tr>
                  </thead>
                  <tbody>
                    ${AI_PROVIDERS.map((provider) => {
    const providerData = aiConfig.apiKeys?.find((k) => k.provider === provider.name);
    const hasKey = providerData && providerData.key !== '';
    return `
                      <tr>
                        <td class="ai-provider-name">${provider.label}</td>
                        <td>
                          <input type="password"
                            id="key${provider.id}"
                            class="form-control"
                            value="${hasKey ? escapeHtml(providerData.key) : ''}"
                            placeholder="${provider.placeholder}">
                          <small class="form-text" style="margin-top: 5px">
                            <span>Don't have a key?</span>
                            <a href="${provider.url}" target="_blank" rel="noopener noreferrer">Get one here</a>.
                          </small>
                        </td>
                        <td class="ai-provider-status">
                          <span id="status${provider.id}" class="badge badge-${hasKey ? 'success' : 'secondary'}">
                            ${hasKey ? 'Saved' : 'Not Configured'}
                          </span>
                        </td>
                      </tr>`;
  }).join('')}
                  </tbody>
                </table>
              </div>
            </div>
          </div>`;

  bootbox.dialog({
    title: 'Advanced Settings',
    message: dialogContent,
    size: 'large',
    className: 'advanced-settings-dialog',
    buttons: {
      cancel: {
        label: 'Cancel',
        className: 'btn-secondary cancel-btn',
      },
      save: {
        label: 'Save',
        className: 'btn-secondary confirm-btn',
        callback: () => {
          let errorMessage = '';
          let isValid = true;
          const updatedData = {};
          const seenEnvNames = new Set();

          ['Read', 'Write', 'Execute'].forEach((permType) => {
            const envValue = $(`#scenPerm${permType}Selector`).val().join(',');
            if (envValue === '') {
              return;
            }
            const envName = `MIRO_DEFAULT_SCEN_PERM_${permType.toUpperCase()}`;
            seenEnvNames.add(envName);
            updatedData[envName] = {
              description: `Default ${permType} Permissions`,
              value: envValue,
            };
          });

          const apiKeys = [];
          AI_PROVIDERS.forEach((provider) => {
            const inputElement = document.getElementById(`key${provider.id}`);
            if (inputElement && inputElement.value.trim() !== '') {
              apiKeys.push({
                key: inputElement.value.trim(),
                provider: provider.name,
              });
            }
          });

          if (apiKeys.length > 0) {
            const envName = 'MIRO_AI_ASSISTANT_CONFIG';
            seenEnvNames.add(envName);
            updatedData[envName] = {
              description: 'GAMS MIRO AI Assistant Configuration',
              value: JSON.stringify({ apiKeys }),
            };
          }

          $('#env-rows tr').each(function () {
            const envName = $(this).find('.env-name').val().trim();
            const description = $(this).find('.env-description').val().trim();
            const value = $(this).find('.env-value').val().trim();

            if (envName) {
              if (!/^[A-Z_][A-Z0-9_]*$/.test(envName)) {
                $('#conf-environment-tab').tab('show');
                $(this).find('.env-name').addClass('is-invalid');
                errorMessage = 'Environment variable names must match the pattern ^[A-Z_][A-Z0-9_]*$.';
                isValid = false;
              } else if (seenEnvNames.has(envName)) {
                $('#conf-environment-tab').tab('show');
                $(this).find('.env-name').addClass('is-invalid');
                isValid = false;
              } else {
                $(this).find('.env-name').removeClass('is-invalid');
                seenEnvNames.add(envName);
                updatedData[envName] = { description, value };
              }
            } else if (
              (!$(this).find('.env-name').val()
                && $(this).find('.env-description').val())
              || $(this).find('.env-value').val()
            ) {
              $('#conf-environment-tab').tab('show');
              $(this).find('.env-name').addClass('is-invalid');
              isValid = false;
            }
          });

          if (!isValid) {
            $('#form-error').text(errorMessage);
            return false;
          }

          $('#form-error').text('');
          state.appEnvironment[appKey] = updatedData;
          return true;
        },
      },
    },
  });
  registerSelectizeInputs('#conf-permissions');

  $('#conf-permissions [rel=tooltip]').tooltip({ placement: 'right' });

  AI_PROVIDERS.forEach((provider) => {
    const inputElement = document.getElementById(`key${provider.id}`);
    const badgeElement = document.getElementById(`status${provider.id}`);

    if (!inputElement || !badgeElement) return;

    inputElement.addEventListener('blur', async (e) => {
      const keyValue = e.target.value.trim();
      inputElement.classList.remove('is-valid', 'is-invalid');

      if (!keyValue) {
        badgeElement.className = 'badge badge-secondary';
        badgeElement.innerText = 'Not Configured';
        return;
      }

      badgeElement.className = 'badge badge-info';
      badgeElement.innerText = 'Validating...';

      $('.confirm-btn').attr('disabled', true);

      try {
        console.debug(`Validating API key for AI provider: ${provider.name}`);
        const keyStatus = await testAiProviderApiKey(provider.name, keyValue);

        if (keyStatus === KEY_VALID) {
          console.debug(
            `Valid API key for AI provider: ${provider.name} added!`,
          );
          inputElement.classList.add('is-valid');
          badgeElement.className = 'badge badge-success';
          badgeElement.innerText = 'Connected';
        } else if (keyStatus === KEY_INVALID) {
          console.debug(`Invalid API key for AI provider: ${provider.name}`);
          inputElement.classList.add('is-invalid');
          badgeElement.className = 'badge badge-danger';
          badgeElement.innerText = 'Invalid Key';
        } else {
          console.warn(
            `Could not reach AI provider to validate key: ${provider.name}`,
          );
          inputElement.classList.add('is-invalid');
          badgeElement.className = 'badge badge-danger';
          badgeElement.innerText = 'Connection Error';
        }
      } catch (err) {
        console.warn(
          `Failed validating API key for AI provider: ${provider.name}. Error: ${err}`,
        );
        inputElement.classList.add('is-invalid');
        badgeElement.className = 'badge badge-danger';
        badgeElement.innerText = 'Connection Error';
      } finally {
        $('.confirm-btn').attr('disabled', false);
      }
    });
  });

  const tableBody = $('#env-rows');

  tableBody.on('click', '.toggle-visibility', function () {
    const input = $(this).closest('.input-group').find('.env-value');
    const icon = $(this).find('i');
    const show = input.attr('type') === 'password';
    input.attr('type', show ? 'text' : 'password');
    icon.toggleClass('fa-eye', !show).toggleClass('fa-eye-slash', show);
    const label = show ? 'Hide value' : 'Show value';
    $(this).attr({ 'aria-label': label, title: label });
  });
  tableBody.on('click', '.remove-row', function () {
    $(this).closest('tr').remove();
  });
  tableBody.on('input', '.env-name, .env-description, .env-value', () => {
    const lastRow = tableBody.find('tr:last');
    const isLastRowFilled = lastRow.find('.env-name').val()
      || lastRow.find('.env-description').val()
      || lastRow.find('.env-value').val();
    if (isLastRowFilled && tableBody.find('tr').length < maxEnvVars + 1) {
      tableBody.append(createRow('', '', '', true, false));
    }
  });
}

export function refreshConfigList() {
  state.currentAppId = null;
  state.currentAppLogo = null;
  const appsList = state.currentConfigList.reduce(
    (html, configData, indexRaw) => {
      const appNameSafe = escapeHtml(configData.alias);
      const descSafe = escapeHtml(configData.desc);
      const { id, isDirty } = configData;
      const appIdSafe = escapeHtml(id);
      const idEncoded = unicodeToHTMLID(id);
      const index = indexRaw + 1;
      state.appEnvironment[idEncoded] = configData.appEnv;
      let groupOptions = configData.groups != null
        ? configData.groups.reduce(
          (optionsHTML, groupName) => `${optionsHTML}<option value="${escapeHtml(groupName)}" selected>${escapeHtml(groupName.toLowerCase())}</option>`,
          '',
        )
        : '';
      if (state.currentGroupList.length > 0) {
        let nonSelectedGroups = state.currentGroupList;
        if (configData.groups != null) {
          nonSelectedGroups = state.currentGroupList.filter(
            (groupName) => !configData.groups.includes(groupName),
          );
        }
        groupOptions += nonSelectedGroups.reduce(
          (optionsHTML, groupName) => `${optionsHTML}<option value="${escapeHtml(groupName)}">${escapeHtml(groupName.toLowerCase())}</option>`,
          '',
        );
      }
      const dirtyMarker = isDirty === true
        ? '<span class="dirty-app-button app-corner-button" title="The app is not registered on GAMS Engine. You will not be able to solve your model. Update app to fix this issue."><i class="fas fa-bolt"></i></span>'
        : '';
      return `${html}<div class="col-xxl-3 col-lg-4 col-sm-6 col-12 miro-app-item" data-id="${idEncoded}">
        <div id="appBox_${idEncoded}" class="app-box app-box-draggable launch-app-box app-box-fixed-height" data-id="${idEncoded}" data-index="${index}" draggable="true">
          <div id="appSpinner_${idEncoded}" class="app-spinner">
            <div class="progress" style="position:relative;top:50%;margin-left:auto;margin-right:auto;width:90%">
              <div id="appProgress_${idEncoded}" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
            </div>
          </div>
          <div class="input-group app-data-file-input">
            <label for="appFiles_${idEncoded}" style="width:100%;height:100%;">
              <div class="drag-drop-area-text empty">
              </div>
            </label>
            <input class="input-btn app-data-file-input-el" style="margin-top:1rem;" type="file"
              name="appFiles_${idEncoded}" id="appFiles_${idEncoded}" data-restore="" multiple="multiple" accept=".miroapp,${SUPPORTED_FILE_TYPES.map((el) => `.${el}`).join(',')}">
          </div>
          <div id="appFiles_${idEncoded}_progress" class="progress active shiny-file-input-progress">
            <div class="progress-bar"></div>
          </div>
          <div>
            <div class="app-logo-wrapper">
              <div id="appLogo_${idEncoded}" class="app-logo" style="background-image:url(${configData.logob64});" data-id="${idEncoded}">
              </div>
            </div>
            <div>
            <div class="app-text">
              <h3 title="${appNameSafe}" id="staticAppTitle_${index}" class="app-title app-title-fixed app-item-title" style="width:100%;">${appNameSafe}</h3>
              <input id="appTitle_${index}" value="${appNameSafe}" required="required" type="text" name="alias" class="app-title editable h3-style" style="width:100%;display:none;" placeholder="${PLACEHOLDERS.appName}">
              <p title="${descSafe}" id="staticAppDesc_${index}" class="app-desc app-desc-fixed app-item-desc">${descSafe}</p>
              <textarea id="appDesc_${index}" rows="3" name="desc" class="app-desc editable" style="margin-bottom:1rem;width:100%;display:none;" placeholder="${PLACEHOLDERS.appDesc}">${descSafe}</textarea>
              <div id="appGroupsWrapper_${index}" style="display:none;">
                <label for="appGroups_${index}">${PLACEHOLDERS.appGroups}</label>
                <select id="appGroups_${index}" multiple>${groupOptions}</select>
                <div style="height:1rem"></div>
              </div>
              <button id="appEnv_${index}" data-id="${idEncoded}" class="btn btn-secondary open-config-btn" style="display:none;" type="button">Advanced Settings</button>
              <div class="app-id-field app-id-position" title="${appIdSafe}">
                <small>ID: <i>${appIdSafe}</i></small>
              </div>
              ${configData.version != null && configData.version !== ''
    ? `<div class="app-id-field app-version-field" title="${escapeHtml(configData.version)}">
                  <small><i>${escapeHtml(configData.version)}</i></small>
                </div>`
    : ''
}
              ${configData.authors != null && configData.authors.length > 0
    ? `<div class="app-id-field app-authors-field" title="${escapeHtml(configData.authors.join(', '))}">
                  <small><i>by ${escapeHtml(configData.authors.join(', '))}</i></small>
                </div>`
    : ''
}
            </div>
          </div>
        </div>
        <div style="text-align:right;display:none;" class="edit-bt-group">
            <input data-index="${index}" data-id="${idEncoded}" class="btn btn-secondary cancel-btn" value="Cancel" type="reset">
            <button class="btn btn-secondary confirm-btn btn-save-changes" data-id="${idEncoded}" data-index="${index}" type="button">Save</button>
        </div>
        ${dirtyMarker}
        <a class="delete-app-button app-corner-button" data-index="${index}" data-id="${idEncoded}"><i class="fas fa-xmark"></i></a>
      </div>
    </div>`;
    },
    '',
  );
  Shiny.unbindAll(document.getElementById('appsWrapper'));
  dom.$appsWrapper.html(
    `${appsList}<div id="addAppWrapper" class="col-xxl-3 col-lg-4 col-sm-6 col-12">${ADD_APP_WRAPPER_HTML}</div>`,
  );
  registerSelectizeInputs();
  exitOverlayMode();
  Shiny.bindAll(document.getElementById('appsWrapper'));
}

export function expandAddAppForm() {
  state.currentAppId = -1;
  if ($('#expandedAddAppWrapper').is(':visible')) {
    return;
  }
  const groupOptions = state.currentGroupList.reduce(
    (optionsHTML, groupName) => `${optionsHTML}<option value="${escapeHtml(groupName)}">${escapeHtml(groupName.toLowerCase())}</option>`,
    '',
  );

  $('#addAppWrapper').css('z-index', 11);
  dom.$overlay.data('current', $('#addAppWrapper')).fadeIn(300);
  $('#addAppWrapper')
    .html(`<div class="app-box app-box-expanded" id="expandedAddAppWrapper">
                        <div id="addAppSpinner" class="app-spinner">
                          <div class="progress" style="position:relative;top:50%;margin-left:auto;margin-right:auto;width:90%">
                            <div id="addAppProgress" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
                          </div>
                        </div>
                        <div class="app-logo-wrapper">
                           <div class="drag-drop-area app-window input-group" id="newAppFiles">
                              <label for="miroAppFile" style="width:100%;height:100%;">
                                <i class="fas fa-circle-plus drag-drop-plus-icon"></i>
                                <div class="drag-drop-area-text empty">
                                   ${PLACEHOLDERS.appFiles}
                                </div>
                              </label>
                              <input class="input-btn" style="margin-top:1rem;" type="file" name="miroAppFile" id="miroAppFile" data-restore="">
                           </div>
                           <div class="drag-drop-area add-app-logo app-logo input-group" id="newAppLogo" style="display:none; outline:none;">
                              <label for="miroAppLogo" style="width:100%;height:100%;">
                                <div class="drag-drop-area-text not-empty">
                                ${PLACEHOLDERS.appLogo}
                                </div>
                              </label>
                              <input class="input-btn input-app-logo" style="margin-top:1rem;" type="file" name="miroAppLogo" id="miroAppLogo" data-restore="">
                           </div>
                           <div id="miroAppFile_progress" class="progress active shiny-file-input-progress">
                                <div class="progress-bar"></div>
                           </div>
                           <div id="miroAppLogo_progress" class="progress active shiny-file-input-progress" style="display:none;">
                                <div class="progress-bar"></div>
                           </div>
                        </div>
                        <div>
                        <input id="newAppName" type="text" required="required" name="alias" class="app-title editable h3-style" style="width:100%;" placeholder="${PLACEHOLDERS.appName}">
                        <textarea id="newAppDesc" rows="3" name="desc" class="app-desc editable" style="margin-bottom: 1rem;width: 100%;" placeholder="${PLACEHOLDERS.appDesc}"></textarea>
                        <div>
                          <label for="newAppGroups">${PLACEHOLDERS.appGroups}</label>
                          <select id="newAppGroups" style="margin-bottom: 1rem;width: 100%;" multiple>${groupOptions}</select>
                        </div>
                        <button id="newAppEnv" class="btn btn-secondary open-config-btn" type="button">Advanced Settings</button>
                        <div style="height:1rem"></div>
                        </div>
                        <div style = "text-align:right;">
                            <input class="btn btn-secondary cancel-btn" id="btAddAppReset" value="Cancel" type="reset">
                            <button class="btn btn-secondary confirm-btn" id="btAddApp" type="button" disabled>Add app</button>
                        </div>`);
  Shiny.bindAll(document.getElementById('addAppWrapper'));
  registerSelectizeInputs();
}

export const showOverwriteDialog = (
  appId,
  sendUpdateEvent = true,
  fileTypes = [],
  newAppId = appId,
  showAdvancedSettingsDialog = false,
) => bootbox.prompt({
  title: 'Overwrite data',
  message: `Do you want to overwrite existing scenario data? Please type <b>${escapeHtml(newAppId)}</b> to confirm or choose 'Cancel' to keep the existing data.`,
  centerVertical: true,
  onEscape: false,
  callback: (result) => {
    if (result != null && result !== newAppId) {
      return false;
    }
    state.overwriteAppData[appId] = result != null;
    if (sendUpdateEvent) {
      sendUpdateAppShinyEvent(appId, fileTypes);
    }
    if (showAdvancedSettingsDialog) {
      openAdvancedSettingsDialog();
    }
    return true;
  },
});

export function openLoginForm() {
  $('#loginFormWrapper').show();
  $('#loginLoadingIndicator').hide();
  $('#loginError').hide();
  $('#loginModal').modal().trigger('focus');
}
