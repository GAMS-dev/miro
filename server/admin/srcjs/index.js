/* global $:false Shiny: false */
import 'core-js/stable';
import 'regenerator-runtime/runtime';
import bootbox from 'bootbox';
import Ajv from 'ajv';

const ajv = new Ajv({ allErrors: true });

const envSchema = {
  type: 'object',
  propertyNames: {
    pattern: '^[A-Z_][A-Z0-9_]*$',
  },
  additionalProperties: {
    type: 'string',
  },
};

const validateEnvSchema = ajv.compile(envSchema);

// taken from bjornd: https://stackoverflow.com/questions/6234773/can-i-escape-html-special-chars-in-javascript
function escapeHtml(unsafe) {
  return unsafe
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/'/g, '&quot;')
    .replace(/'/g, '&#039;');
}

const $overlay = $('#overlayScreen');
const $loadingScreen = $('#loading-screen');
const $appsWrapper = $('#appsWrapper');
const appFilesPlaceholder = 'Drop your MIRO app here.';
const appNamePlaceholder = 'App title';
const appDescPlaceholder = 'Short model description (optional)';
const appEnvPlaceholder = 'App environment (JSON, optional)';
const appGroupsPlaceholder = 'Access groups (optional)';
const appLogoPlaceholder = 'Different app logo? Drop your MIRO app logo here.';
const overwriteAppData = {};
let reorderAppsMode = false;
let currentConfigList = null;
let currentGroupList = [];
let currentAppId = null;
let currentAppLogo = null;

const addAppWrapperHTML = `<div id="addAppBox" class="add-app-box app-box-fixed-height">
                             <div style="height:200px;">
                                 <p class="add-app-box-logo">
                                </p>
                             </div>
                             <div>
                               <div class="add-app-box-title"></div>
                               <div class="add-app-box-desc"></div>
                             </div>
                            <a class="btn-add-app" id="addApp"><i class="fas fa-plus-circle"></i></a>
                          </div>`;

const supportedDataFileTypes = ['gdx', 'miroscen', 'xlsx', 'xlsm', 'xls', 'zip'];

function validateAppEnv(envContentRaw) {
  if (envContentRaw.trim() === '') {
    return true;
  }
  try {
    const envContent = JSON.parse(envContentRaw);
    const valid = validateEnvSchema(envContent);
    if (!valid) {
      bootbox.alert({
        title: 'Problems validating app environment',
        message: `The app environment could not be validated.\nValidation errors: ${ajv.errorsText(validateEnvSchema.errors)}.`,
        centerVertical: true,
      });
      return false;
    }
    return true;
  } catch (err) {
    bootbox.alert({
      title: 'Problems parsing app environment',
      message: 'The app environment could not be parsed.\nPlease make sure that you have entered valid JSON syntax.',
      centerVertical: true,
    });
    return false;
  }
}

function sendAddRequest() {
  const newAppTitle = document.getElementById('newAppName').value;
  if (!newAppTitle || newAppTitle.trim() === '') {
    bootbox.alert({ title: 'Missing App title', message: 'Please enter an app title.', centerVertical: true });
    return;
  }
  const appEnv = document.getElementById('newAppEnv').value;
  if (validateAppEnv(appEnv) !== true) {
    return;
  }
  $('#addAppSpinner').show();
  $('#btAddApp').attr('disabled', true);
  Shiny.setInputValue('addApp', {
    title: newAppTitle,
    desc: document.getElementById('newAppDesc').value,
    env: appEnv,
    groups: $('#newAppGroups').val(),
    overwrite: overwriteAppData['~$_newApp'] === true,
  }, {
    priority: 'event',
  });
  overwriteAppData['~$_newApp'] = null;
}

function sendUpdateRequest(index) {
  const newAppTitle = $(`#appTitle_${index}`).val().trim();
  if (!newAppTitle || newAppTitle.trim() === '') {
    bootbox.alert({ title: 'Missing App title', message: 'Please enter an app title.', centerVertical: true });
    return;
  }
  let newAppDesc = $(`#appDesc_${index}`).val().trim();
  if (!newAppDesc || newAppDesc === appDescPlaceholder) {
    newAppDesc = '';
  }
  const appEnv = document.getElementById(`appEnv_${index}`).value.trim();
  if (validateAppEnv(appEnv) !== true) {
    return;
  }
  $loadingScreen.fadeIn(200);
  Shiny.setInputValue('updateAppMeta', {
    index,
    title: newAppTitle,
    desc: newAppDesc,
    env: appEnv,
    newLogo: currentAppLogo !== null,
    groups: $(`#appGroups_${index}`).val(),
  }, {
    priority: 'event',
  });
}

function sendUpdateAppOrderRequest(idFrom, idTo, idFromRaw, idToRaw) {
  $loadingScreen.fadeIn(200);
  Shiny.setInputValue('updateAppOrder', {
    idFrom,
    idTo,
    idFromRaw,
    idToRaw,
  }, {
    priority: 'event',
  });
}

function sendRemoveRequest(index, removeData) {
  $loadingScreen.fadeIn(200);
  Shiny.setInputValue('deleteApp', {
    index,
    removeData,
  }, {
    priority: 'event',
  });
}

function exitOverlayMode() {
  if ($('#expandedAddAppWrapper').is(':visible')) {
    $('#addAppWrapper').html(addAppWrapperHTML);
  }
  if ($overlay.is(':visible')) {
    $overlay.hide();
    $overlay.data('current').css('z-index', 1);
  }
}

function registerSelectizeInputs() {
  $('select').selectize({
    create: true,
    persist: false,
  });
}

function refreshConfigList() {
  currentAppId = null;
  currentAppLogo = null;
  const appsList = currentConfigList.reduce((html, configData, indexRaw) => {
    const appNameSafe = escapeHtml(configData.alias);
    const descSafe = escapeHtml(configData.desc);
    const appEnvSafe = escapeHtml(configData.appEnv);
    const { id } = configData;
    const index = indexRaw + 1;
    let groupOptions = configData.groups != null ? configData.groups.reduce((optionsHTML, groupName) => (`${optionsHTML}<option value="${groupName}" selected>${groupName.toLowerCase()}</option>`), '') : '';
    if (currentGroupList.length > 0) {
      let nonSelectedGroups = currentGroupList;
      if (configData.groups != null) {
        nonSelectedGroups = currentGroupList
          .filter((groupName) => !configData.groups.includes(groupName));
      }
      groupOptions += nonSelectedGroups.reduce((optionsHTML, groupName) => (`${optionsHTML}<option value="${groupName}">${groupName.toLowerCase()}</option>`), '');
    }
    return `${html}<div class="col-xxl-3 col-lg-4 col-sm-6 col-12 miro-app-item" data-id="${id}">
        <div id="appBox_${id}" class="app-box app-box-draggable launch-app-box app-box-fixed-height" data-id="${id}" data-index="${index}" draggable="true">
          <div id="appSpinner_${id}" class="app-spinner">
            <div class="progress" style="position:relative;top:50%;margin-left:auto;margin-right:auto;width:90%">
              <div id="appProgress_${id}" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
            </div>
          </div>
          <div class="input-group app-data-file-input">
            <label for="appFiles_${id}" style="width:100%;height:100%;">
              <div class="drag-drop-area-text empty">
              </div>
            </label>
            <input class="input-btn app-data-file-input-el" style="margin-top:1rem;" type="file"
              name="appFiles_${id}" id="appFiles_${id}" data-restore="" multiple="multiple" accept=".miroapp,${supportedDataFileTypes.map((el) => `.${el}`).join(',')}">
          </div>
          <div id="appFiles_${id}_progress" class="progress active shiny-file-input-progress">
            <div class="progress-bar"></div>
          </div>
          <div>
            <div style="height:200px;">
              <div id="appLogo_${id}" class="app-logo" style="background-image:url(${configData.logob64});" data-id="${id}">
              </div>
            </div>
            <div>
            <div>
              <h3 title="${appNameSafe}" id="staticAppTitle_${index}" class="app-title app-title-fixed app-item-title" style="margin-top:15pt;width:100%;">${appNameSafe}</h3>
              <input id="appTitle_${index}" value="${appNameSafe}" required="required" type="text" name="alias" class="app-title editable h3-style" style="margin-top:15pt;width:100%;display:none;" placeholder="${appNamePlaceholder}">
              <p title="${descSafe}" id="staticAppDesc_${index}" class="app-desc app-desc-fixed app-item-desc">${descSafe}</p>
              <textarea id="appDesc_${index}" rows="3" name="desc" class="app-desc editable" style="margin-bottom:1rem;width:100%;display:none;" placeholder="${appDescPlaceholder}">${descSafe}</textarea>
              <div id="appGroupsWrapper_${index}" style="display:none;">
                <label for="appGroups_${index}">${appGroupsPlaceholder}</label>
                <select id="appGroups_${index}" multiple>${groupOptions}</select>
                <div style="height:1rem"></div>
              </div>
              <textarea id="appEnv_${index}" rows="1" name="appEnv" class="editable" style="width:100%;display:none;" placeholder="${appEnvPlaceholder}">${appEnvSafe}</textarea>
            </div>
          </div>
        </div>
        <div style="text-align:right;display:none;" class="edit-bt-group">
            <input data-index="${index}" data-id="${id}" class="btn btn-secondary cancel-btn" value="Cancel" type="reset">
            <button class="btn btn-secondary confirm-btn btn-save-changes" data-id="${id}" data-index="${index}" type="button">Save</button>
        </div>
        <a class="delete-app-button app-corner-button" data-index="${index}" data-id="${id}"><i class="fas fa-times"></i></a>
      </div>
    </div>`;
  }, '');
  Shiny.unbindAll(document.getElementById('appsWrapper'));
  $appsWrapper.html(`${appsList}<div id="addAppWrapper" class="col-xxl-3 col-lg-4 col-sm-6 col-12">${addAppWrapperHTML}</div>`);
  registerSelectizeInputs();
  exitOverlayMode();
  Shiny.bindAll(document.getElementById('appsWrapper'));
}

function expandAddAppForm() {
  currentAppId = -1;
  if ($('#expandedAddAppWrapper').is(':visible')) {
    return;
  }
  const groupOptions = currentGroupList.reduce((optionsHTML, groupName) => (`${optionsHTML}<option value="${groupName}">${groupName.toLowerCase()}</option>`), '');

  $('#addAppWrapper').css('z-index', 11);
  $overlay.data('current', $('#addAppWrapper')).fadeIn(300);
  $('#addAppWrapper').html(`<div class="app-box" id="expandedAddAppWrapper">
                        <div id="addAppSpinner" class="app-spinner">
                          <div class="progress" style="position:relative;top:50%;margin-left:auto;margin-right:auto;width:90%">
                            <div id="addAppProgress" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
                          </div>
                        </div>
                        <div style="height:200px;">
                           <div class="drag-drop-area app-window input-group" id="newAppFiles">
                              <label for="miroAppFile" style="width:100%;height:100%;">
                                <div class="drag-drop-area-text empty">
                                <div><i class="fas fa-plus-circle drag-drop-area-icon"></i></div>
                                   ${appFilesPlaceholder}
                                </div>
                              </label>
                              <input class="input-btn" style="margin-top:1rem;" type="file" name="miroAppFile" id="miroAppFile" data-restore="">
                           </div>
                           <div class="drag-drop-area add-app-logo app-logo input-group" id="newAppLogo" style="display:none; outline:none;">
                              <label for="miroAppLogo" style="width:100%;height:100%;">
                                <div class="drag-drop-area-text not-empty">
                                ${appLogoPlaceholder}
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
                        <input id="newAppName" type="text" required="required" name="alias" class="app-title editable h3-style" style="margin-top:15pt;width:100%;" placeholder="${appNamePlaceholder}">
                        <textarea id="newAppDesc" rows="3" name="desc" class="app-desc editable" style="margin-bottom: 1rem;width: 100%;" placeholder="${appDescPlaceholder}"></textarea>
                        <div>
                          <label for="newAppGroups">${appGroupsPlaceholder}</label>
                          <select id="newAppGroups" style="margin-bottom: 1rem;width: 100%;" multiple>${groupOptions}</select>
                        </div>
                        <textarea id="newAppEnv" rows="1" name="appEnv" class="editable" style="width:100%;" placeholder="${appEnvPlaceholder}"></textarea>
                        <div style="height:1rem"></div>
                        </div>
                        <div style = "text-align:right;">
                            <input class="btn btn-secondary cancel-btn" id="btAddAppReset" value="Cancel" type="reset">
                            <button class="btn btn-secondary confirm-btn" id="btAddApp" type="button" disabled>Add app</button>
                        </div>`);
  Shiny.bindAll(document.getElementById('addAppWrapper'));
  registerSelectizeInputs();
}

$appsWrapper.on('click', '#addAppBox', () => {
  expandAddAppForm();
});
$appsWrapper.on('click', '.cancel-btn', () => {
  refreshConfigList();
});

let dragAddAppCounter = 0;
let openAddAppCounter = 0;
let dragTimerAddAppBox = null;
$appsWrapper.on('drop', '.app-logo', (e) => {
  if (reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter = 0;
  $('.btn-save-changes').attr('disabled', true);
  $('#btAddApp').attr('disabled', true);
});
$appsWrapper.on('dragenter', '#addAppBox', (e) => {
  e.preventDefault();
  e.stopPropagation();
  openAddAppCounter += 1;
  if (dragTimerAddAppBox == null) {
    dragTimerAddAppBox = setTimeout(() => {
      openAddAppCounter = 0;
      dragTimerAddAppBox = null;
      expandAddAppForm();
    }, 700);
  }
});
$appsWrapper.on('dragleave', '#addAppBox', (e) => {
  e.preventDefault();
  e.stopPropagation();
  openAddAppCounter -= 1;
  if (openAddAppCounter === 0) {
    clearTimeout(dragTimerAddAppBox);
    dragTimerAddAppBox = null;
  }
});
$appsWrapper.on('dragover', '#addAppBox', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
$appsWrapper.on('change', '.input-app-logo', () => {
  $('.btn-save-changes').attr('disabled', true);
  $('#btAddApp').attr('disabled', true);
});
$(document).on('dragover', '.drag-drop-area', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
$(document).on('dragenter', '.drag-drop-area', function (e) {
  if (reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter += 1;
  $(this).addClass('drag-drop-area-dragover');
});
$(document).on('dragleave', '.drag-drop-area', function (e) {
  if (reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter -= 1;
  if (dragAddAppCounter === 0) {
    $(this).removeClass('drag-drop-area-dragover');
  }
});
$appsWrapper.on('drop', '#newAppFiles', function (e) {
  if (reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter = 0;
  $(this).removeClass('drag-drop-area-dragover');
});

$appsWrapper.on('dragstart', '.app-box-draggable', (e) => {
  reorderAppsMode = true;
  if (!reorderAppsMode) {
    return;
  }
  e.originalEvent.dataTransfer.setData('text/plain', e.originalEvent.target.id);
  e.originalEvent.target.style.opacity = 0.5;
});
$appsWrapper.on('dragenter', '.app-box-draggable', function (e) {
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter += 1;
  $(this).addClass('drag-drop-area-dragover');
});
$appsWrapper.on('dragover', '.app-box-draggable', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
$appsWrapper.on('dragleave', '.app-box-draggable', function (e) {
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter -= 1;
  if (dragAddAppCounter === 0) {
    $(this).removeClass('drag-drop-area-dragover');
  }
});
$appsWrapper.on('dragend', '.app-box-draggable', (e) => {
  e.preventDefault();
  e.stopPropagation();
  reorderAppsMode = false;
  e.originalEvent.dataTransfer.clearData();
  $('.app-box-draggable').removeClass('drag-drop-area-dragover').css('opacity', '');
});

function sendUpdateAppShinyEvent(appId, fileTypes) {
  if (overwriteAppData[appId] == null) {
    return;
  }
  const overwriteData = overwriteAppData[appId];
  overwriteAppData[appId] = null;
  const spinnerId = `#appSpinner_${appId}`;
  $('#overlayScreen').show();
  $(spinnerId).show();
  $(`#appFiles_${appId}_progress`).css('visibility', '');
  if (fileTypes.length === 1 && fileTypes[0] === 'miroapp') {
    Shiny.setInputValue('updateAppRequest', { id: appId, overwrite: overwriteData }, {
      priority: 'event',
    });
    return;
  }
  const invalidFileTypes = fileTypes
    .filter((fileType) => !supportedDataFileTypes.includes(fileType));
  if (invalidFileTypes.length === 0) {
    Shiny.setInputValue('updateAppDataRequest', { id: appId, overwrite: overwriteData }, {
      priority: 'event',
    });
    return;
  }
  $(spinnerId).hide();
}

const showOverwriteDialog = (appId, sendUpdateEvent = true, fileTypes = []) => bootbox.confirm({
  message: 'Do you want to overwrite existing scenario data?',
  buttons: {
    cancel: {
      label: 'No, keep existing data',
    },
    confirm: {
      label: 'Yes, overwrite',
    },
  },
  centerVertical: true,
  onEscape: false,
  callback: (overwriteConfirmed) => {
    overwriteAppData[appId] = overwriteConfirmed;
    if (sendUpdateEvent) {
      sendUpdateAppShinyEvent(appId, fileTypes);
    }
  },
});

$(document).on('shiny:inputchanged', (event) => {
  const eventName = event.name;
  if (eventName.startsWith('appFiles_') && event.value != null && event.value.length > 0) {
    const appId = eventName.substring(9);
    const fileTypes = event.value.map((fileTmp) => fileTmp.name.split('.').pop().toLowerCase());
    sendUpdateAppShinyEvent(appId, fileTypes);
  }
});
$appsWrapper.on('drop', '.app-box-draggable', function (e) {
  e.preventDefault();
  let filesDropped = false;
  if (!reorderAppsMode) {
    if (!e.originalEvent.dataTransfer.files) {
      return;
    }
    filesDropped = true;
  }
  reorderAppsMode = false;
  dragAddAppCounter = 0;
  $('.app-box-draggable').removeClass('drag-drop-area-dragover').css('opacity', '');

  const idToRaw = $(this).attr('id');
  const idTo = idToRaw.slice(7);
  if (filesDropped) {
    const filesTmp = [...e.originalEvent.dataTransfer.files];
    const fileTypes = filesTmp.map((fileTmp) => fileTmp.name.split('.').pop().toLowerCase());
    if (fileTypes.length === 1 && fileTypes[0] === 'miroapp') {
      showOverwriteDialog(idTo, true, fileTypes);
      return;
    }
    if (fileTypes.length > 10) {
      bootbox.alert({
        title: 'Too many files',
        message: 'Only up to 10 files can be added at once.',
        centerVertical: true,
      });
      $(`#appFiles_${idTo}_progress`).css('visibility', '');
      return;
    }
    const invalidFileTypes = fileTypes
      .filter((fileType) => !supportedDataFileTypes.includes(fileType));
    if (invalidFileTypes.length === 0) {
      showOverwriteDialog(idTo, true, fileTypes);
      return;
    }
    bootbox.alert({
      title: 'Invalid file',
      message: `The file you dropped (${invalidFileTypes.join(',')}) is not supported. Please drop either a new MIRO app (.miroapp) to update the current version or a valid data file (${supportedDataFileTypes.join(',')}).`,
      centerVertical: true,
    });
    $(`#appFiles_${idTo}_progress`).css('visibility', '');
    return;
  }

  const idFromRaw = e.originalEvent.dataTransfer.getData('text/plain');
  const idFrom = idFromRaw.slice(7);
  if (idFrom !== idTo) {
    sendUpdateAppOrderRequest(idFrom, idTo, idFromRaw, idToRaw);
  }
});

$appsWrapper.on('click', '.app-box', function (e) {
  e.preventDefault();
  e.stopPropagation();
  const $this = $(this);
  $this.children('.app-data-file-input').hide();
  const $target = $(e.target);
  if ($overlay.is(':visible')
    || $target.hasClass('cancel-btn')
    || $target.hasClass('delete-app-button')
    || $target.parents('.delete-app-button').length) {
    return;
  }
  const appID = this.dataset.id;
  const appIndex = this.dataset.index;
  if (appIndex) {
    $(`#appBox_${appID}`).removeClass('app-box-fixed-height');
    $(`#appLogo_${appID}`)
      .html(`<label for="updateMiroAppLogo" style="width:100%;height:100%;">
                  <div class="drag-drop-area-text not-empty">
                  ${appLogoPlaceholder}
                  </div>
                </label>
                <input class="input-btn input-app-logo" style="margin-top:1rem;" type="file" name="updateMiroAppLogo" id="updateMiroAppLogo" data-restore="">`)
      .addClass('drag-drop-area input-group')
      .after(`<div id="updateMiroAppLogo_progress" class="progress active shiny-file-input-progress">
                  <div class="progress-bar"></div>
                </div>`);
    $(`#appTitle_${appIndex}`).show();
    $(`#staticAppTitle_${appIndex}`).hide();
    $(`#appDesc_${appIndex}`).show();
    $(`#appEnv_${appIndex}`).show();
    $(`#staticAppDesc_${appIndex}`).hide();
    $(`#appGroupsWrapper_${appIndex}`).show();
    Shiny.bindAll(document.getElementById(`appBox_${appID}`));
  }
  $(`#appBox_${appID} .edit-bt-group`).slideDown(200);
  $this.css('z-index', 11);
  currentAppId = appID;
  $overlay.data('current', $this).fadeIn(300);
});

$appsWrapper.on('click', '.btn-save-changes', function () {
  const appIndex = this.dataset.index;
  const appID = this.dataset.id;
  if (!appIndex || !appID) {
    bootbox.alert({
      title: 'Unexpected error',
      message: 'Ooops. Something went wrong. Please contact GAMS if this error persists!',
      centerVertical: true,
    });
    return;
  }
  sendUpdateRequest(appIndex, appID);
});

$appsWrapper.on('click', '.delete-app-button', function () {
  const appIndex = this.dataset.index;
  const appId = this.dataset.id;
  bootbox.confirm({
    message: 'Are you sure you want to remove this app? This cannot be undone.',
    centerVertical: true,
    onEscape: false,
    callback: (removeAppConfirmed) => {
      if (!removeAppConfirmed) {
        return;
      }
      bootbox.prompt({
        required: true,
        title: 'Remove data',
        message: `Do you want to permanently remove all data belonging to this app? Please type <b>${escapeHtml(appId)}</b> to confirm or press 'Cancel' to keep the data.`,
        centerVertical: true,
        onEscape: false,
        callback: (result) => {
          if (result === null) {
            sendRemoveRequest(appIndex, false);
            return true;
          }
          if (result !== appId) {
            return false;
          }
          sendRemoveRequest(appIndex, true);
          return true;
        },
      });
    },
  });
});
$appsWrapper.on('change', '#newAppName', () => {
  const newAppName = document.getElementById('newAppName').value;

  if (newAppName != null && newAppName.trim() !== '') {
    document.getElementById('btAddApp').disabled = false;
  } else {
    document.getElementById('btAddApp').disabled = true;
  }
});
$appsWrapper.on('click', '#btAddApp', () => {
  sendAddRequest();
});
// Login form
function openLoginForm() {
  $('#loginFormWrapper').show();
  $('#loginLoadingIndicator').hide();
  $('#loginError').hide();
  $('#loginModal').modal().focus();
}
$('body').on('click', '#btLogin', () => {
  $('#loginFormWrapper').hide();
  $('#loginError').hide();
  $('#loginLoadingIndicator').show();
  Shiny.setInputValue('loginRequest', {
    user: document.getElementById('loginUser').value,
    password: document.getElementById('loginPassword').value,
  }, {
    priority: 'event',
  });
});

$(() => {
  registerSelectizeInputs();
  if ($('.select-app-groups').length > 0) {
    currentGroupList = [];
    $('.select-app-groups').first().children('option').each(function () {
      currentGroupList.push($(this).val());
    });
  }
  $(document).on('keyup', (event) => {
    if (event.key === 'Enter' && !event.ctrlKey) {
      if ($('#shiny-modal').find('.selectize-input.input-active').length > 0
        || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')) {
        return;
      }
      $('.bt-gms-confirm:visible:enabled').trigger('click');
    } // ENTER will confirm modal dialogues
  });
  Shiny.addCustomMessageHandler('onInit', (data) => {
    currentConfigList = data.configList;
    if (Array.isArray(data.groupList)) {
      currentGroupList = data.groupList;
    }
    if (data.loginRequired === true) {
      openLoginForm();
    }
    $overlay.hide();
    refreshConfigList();
  });
  Shiny.addCustomMessageHandler('onInitErrors', (data) => {
    let errorMessage;
    if (data.appsNotOnEngine) {
      errorMessage = `Some apps registered on MIRO Server were not found on GAMS Engine: '${data.appsNotOnEngine.join("', '")}'\n`;
    }
    if (data.appsNotOnMIRO) {
      errorMessage = `Some models registered on GAMS Engine are not found on MIRO Server: '${data.appsNotOnMIRO.join("', '")}'\n`;
    }
    if (errorMessage) {
      bootbox.alert({
        title: 'Discrepancy with GAMS Engine',
        message: errorMessage,
        centerVertical: true,
      });
    }
  });
  Shiny.addCustomMessageHandler('onLoginRequired', (e) => { // eslint-disable-line no-unused-vars
    $loadingScreen.fadeOut(200);
    openLoginForm();
  });
  Shiny.addCustomMessageHandler('onLoginSuccessful', (e) => { // eslint-disable-line no-unused-vars
    $('#loginModal').modal('hide');
  });
  Shiny.addCustomMessageHandler('onSuccess', (data) => {
    $loadingScreen.fadeOut(200);
    if (data.requestType === 'addApp') {
      $('#addAppSpinner').hide();
      $('#addAppProgress').css('width', '0%').attr('aria-valuenow', '0');
      $('#btAddApp').attr('disabled', false);
    } else if (data.requestType === 'updateApp') {
      $(data.spinnerSelector).hide();
      $('#overlayScreen').hide();
      $(data.progressSelector).css('width', '0%').attr('aria-valuenow', '0');
      return;
    } else if (data.requestType === 'migrateDb') {
      $('#loadingScreenProgressWrapper').hide();
      $('#loadingScreenProgress').css('width', '0%').attr('aria-valuenow', '0');
      sendAddRequest();
      return;
    }
    if (Array.isArray(data.configList)) {
      currentConfigList = data.configList;
      if (Array.isArray(data.groupList)) {
        currentGroupList = data.groupList;
      }
      refreshConfigList();
    } else {
      bootbox.alert({
        message: data.message,
        centerVertical: true,
      });
    }
  });
  Shiny.addCustomMessageHandler('onHideAddAppProgress', (e) => { // eslint-disable-line no-unused-vars
    $('#addAppSpinner').hide();
    $('#addAppProgress').css('width', '0%').attr('aria-valuenow', '0');
    $('#btAddApp').attr('disabled', false);
  });
  Shiny.addCustomMessageHandler('onProgress', (data) => {
    let selector;
    if (data.selector == null) {
      selector = '#loadingScreenProgress';
      $('#loadingScreenProgressWrapper').show();
    } else {
      selector = data.selector;
    }
    if (data.progress === -1) {
      $(selector).css('width', '0%').attr('aria-valuenow', '0');
      if (selector === '#addAppProgress') {
        $('#addAppSpinner').hide();
      } else if (data.selector == null) {
        $('#loadingScreenProgressWrapper').hide();
      }
    } else {
      $(selector).css('width', `${data.progress}%`).attr('aria-valuenow', data.progress);
    }
  });
  Shiny.addCustomMessageHandler('onAddAppLogo', (data) => {
    if (!currentAppId) {
      $('.btn-save-changes').attr('disabled', false);
      return;
    }
    if (currentAppId === -1) {
      // new App
      $('#btAddApp').attr('disabled', false);
      $('#newAppLogo').css('background-image',
        `url("${data.logoB64}")`);
      return;
    }
    $('.btn-save-changes').attr('disabled', false);
    currentAppLogo = data.logoB64;
    $(`#appLogo_${currentAppId}`).css('background-image',
      `url("${currentAppLogo}")`);
  });
  Shiny.addCustomMessageHandler('onNewAppValidated', (data) => {
    const newAppTitle = document.getElementById('newAppName').value;
    if (newAppTitle == null || newAppTitle.trim() === '') {
      document.getElementById('newAppName').value = data.appTitle;
    }
    if (data.appDesc) {
      const newAppDesc = document.getElementById('newAppDesc').value;
      if (newAppDesc == null || newAppDesc.trim() === '') {
        document.getElementById('newAppDesc').value = data.appDesc;
      }
    }
    if (data.dataExists !== true) {
      overwriteAppData['~$_newApp'] = true;
    } else {
      showOverwriteDialog('~$_newApp', false);
    }
    $('#newAppFiles').hide();
    $('#miroAppFile_progress').hide();
    $('#newAppLogo').show();
    $('#miroAppLogo_progress').show();
    $('#newAppLogo').css('background-image',
      `url("${data.logoB64}")`);
    $('#btAddApp').attr('disabled', false);
  });
  Shiny.addCustomMessageHandler('onError', (e) => {
    $loadingScreen.fadeOut(200);
    if (e.requestType === 'addApp') {
      $('#addAppSpinner').hide();
      $('#addAppProgress').css('width', '0%').attr('aria-valuenow', '0');
      $('#btAddApp').attr('disabled', true);
    } else if (e.requestType === 'loginRequest') {
      $('#loginFormWrapper').show();
      $('#loginLoadingIndicator').hide();
      $('#loginError').show();
      return;
    } else if (e.requestType === 'updateApp') {
      $(e.spinnerSelector).hide();
      $('#overlayScreen').hide();
      $(e.progressSelector).css('width', '0%').attr('aria-valuenow', '0');
    } else if (e.requestType === 'migrateDb') {
      $('#loadingScreenProgressWrapper').hide();
      $('#loadingScreenProgress').css('width', '0%').attr('aria-valuenow', '0');
      $('#migrationFormErrors').show();
      return;
    } else if (e.requestType === 'updateLogoAddApp') {
      $('#btAddApp').attr('disabled', false);
    } else if (e.requestType === 'updateLogo') {
      $('.btn-save-changes').attr('disabled', false);
    }
    bootbox.alert({
      title: 'Error',
      message: e.message,
      centerVertical: true,
    });
  });
  Shiny.addCustomMessageHandler('gms-showEl', (id) => {
    $(id).show();
    $(id).trigger('shown');
  });
  Shiny.addCustomMessageHandler('gms-hideEl', (id) => {
    $(id).hide();
    $(id).trigger('hidden');
  });
  Shiny.addCustomMessageHandler('gms-enableEl', (id) => {
    $(id).prop('disabled', false);
  });
  Shiny.addCustomMessageHandler('gms-disableEl', (id) => {
    $(id).prop('disabled', true);
  });
  Shiny.addCustomMessageHandler('gms-showHideEl', (data) => {
    if (data.msg !== null) {
      $(data.id).text(data.msg);
    }
    $(data.id).show().delay(data.delay).fadeOut();
  });
  Shiny.addCustomMessageHandler('gms-showElReplaceTxt', (data) => {
    $(data.id).text(data.txt).show();
  });
});
