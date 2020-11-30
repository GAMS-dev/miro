/* global $:false jQuery:false Shiny: false */
import 'core-js/stable';
import 'regenerator-runtime/runtime';
import bootbox from 'bootbox';

// taken from bjornd: https://stackoverflow.com/questions/6234773/can-i-escape-html-special-chars-in-javascript
function escapeHtml(unsafe) {
  return unsafe
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/'/g, '&quot;')
    .replace(/'/g, '&#039;');
}

// credits to Paolo Bergantino @https://stackoverflow.com/questions/698301/is-there-a-native-jquery-function-to-switch-elements
jQuery.fn.swapWith = function (to) {
  return this.each(function () {
    const copyTo = $(to).clone(true);
    const copyFrom = $(this).clone(true);
    $(to).replaceWith(copyFrom);
    $(this).replaceWith(copyTo);
  });
};

const $overlay = $('#overlayScreen');
const $loadingScreen = $('#loading-screen');
const $appsWrapper = $('#appsWrapper');
const appFilesPlaceholder = 'Drop your MIRO app here.';
const appNamePlaceholder = 'App title';
const appDescPlaceholder = 'Short model description (optional)';
const appGroupsPlaceholder = 'Access groups (optional)';
const appLogoPlaceholder = 'Different app logo? Drop your MIRO app logo here.';
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

function sendAddRequest(removeInconsistentTables = false) {
  const newAppTitle = document.getElementById('newAppName').value;
  if (!newAppTitle || newAppTitle.trim() === '') {
    bootbox.alert({ title: 'Missing App title', message: 'Please enter an app title.', centerVertical: true });
    return;
  }
  $('#addAppSpinner').show();
  $('#btAddApp').attr('disabled', true);
  Shiny.setInputValue('addApp', {
    title: newAppTitle,
    desc: document.getElementById('newAppDesc').value,
    groups: $('#newAppGroups').val(),
    removeInconsistentTables,
  }, {
    priority: 'event',
  });
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
  $loadingScreen.fadeIn(200);
  Shiny.setInputValue('updateApp', {
    index,
    title: newAppTitle,
    desc: newAppDesc,
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
    Shiny.unbindAll(document.getElementById('addAppWrapper'));
    $('#addAppWrapper').html(addAppWrapperHTML);
  } else if (currentAppId) {
    Shiny.unbindAll(document.getElementById(`appBox_${currentAppId}`));
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
  exitOverlayMode();
  currentAppId = null;
  currentAppLogo = null;
  $appsWrapper.empty();
  const appsList = currentConfigList.reduce((html, configData, indexRaw) => {
    const appNameSafe = escapeHtml(configData.alias);
    const descSave = escapeHtml(configData.desc);
    const { id } = configData;
    const index = indexRaw + 1;
    let groupOptions = configData.groups != null ? configData.groups.reduce((optionsHTML, groupName) => (`${optionsHTML}<option value="${groupName}" selected>${groupName}</option>`), '') : '';
    if (currentGroupList.length > 0) {
      let nonSelectedGroups = currentGroupList;
      if (configData.groups != null) {
        nonSelectedGroups = currentGroupList
          .filter((groupName) => !configData.groups.includes(groupName));
      }
      groupOptions += nonSelectedGroups.reduce((optionsHTML, groupName) => (`${optionsHTML}<option value="${groupName}">${groupName}</option>`), '');
    }
    return `${html}<div class="col-xxl-3 col-lg-4 col-sm-6 col-12 miro-app-item" data-id="${id}">
        <div id="appBox_${id}" class="app-box app-box-draggable launch-app-box app-box-fixed-height" data-id="${id}" data-index="${index}" draggable="true">
          <div>
            <div style="height:200px;">
              <div id="appLogo_${id}" class="app-logo" style="background-image:url(${configData.logob64});" data-id="${id}">
              </div>
            </div>
            <div>
            <div style="height:200px">
              <h3 title="${appNameSafe}" id="staticAppTitle_${index}" class="app-title app-title-fixed app-item-title" style="margin-top:15pt;width:100%;">${appNameSafe}</h3>
              <input id="appTitle_${index}" value="${appNameSafe}" required="required" type="text" name="alias" class="app-title editable h3-style" style="margin-top:15pt;width:100%;display:none;" placeholder="${appNamePlaceholder}">
              <p title="${descSave}" id="staticAppDesc_${index}" class="app-desc app-desc-fixed app-item-desc">${descSave}</p>
              <textarea id="appDesc_${index}" rows="3" name="desc" class="app-desc editable" style="margin-bottom:1rem;width:100%;display:none;" placeholder="${appDescPlaceholder}">${descSave}</textarea>
              <div id="appGroupsWrapper_${index}" style="display:none;">
                <label for="newAppGroups">${appGroupsPlaceholder}</label>
                <select id="appGroups_${index}" multiple>${groupOptions}</select>
                <div style="height:1rem"></div>
              </div>
            </div>
          </div>
        </div>
        <div style="text-align:right;display:none;" class="edit-bt-group">
            <input data-index="${index}" data-id="${id}" class="btn btn-secondary cancel-btn" value="Cancel" type="reset">
            <button class="btn btn-secondary confirm-btn btn-save-changes" data-id="${id}" data-index="${index}" type="button">Save</button>
        </div>
        <a class="delete-app-button app-corner-button" data-index="${index}"><i class="fas fa-times"></i></a>
      </div>
    </div>`;
  }, '');
  $appsWrapper.html(`${appsList}<div id="addAppWrapper" class="col-xxl-3 col-lg-4 col-sm-6 col-12">${addAppWrapperHTML}</div>`);
  registerSelectizeInputs();
}

function expandAddAppForm() {
  currentAppId = -1;
  if ($('#expandedAddAppWrapper').is(':visible')) {
    return;
  }
  const groupOptions = currentGroupList.reduce((optionsHTML, groupName) => (`${optionsHTML}<option value="${groupName}">${groupName}</option>`), '');

  $('#addAppWrapper').css('z-index', 11);
  $overlay.data('current', $('#addAppWrapper')).fadeIn(300);
  $('#addAppWrapper').html(`<div class="app-box" id="expandedAddAppWrapper">
                        <div id="addAppSpinner">
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
                        <label for="newAppGroups">${appGroupsPlaceholder}</label>
                        <select id="newAppGroups" style="margin-bottom: 1rem;width: 100%;" multiple>${groupOptions}</select>
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
$appsWrapper.on('dragenter', '#addAppBox', () => {
  expandAddAppForm();
});
$appsWrapper.on('dragover', '#addAppBox', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
$appsWrapper.on('change', '.input-app-logo', () => {
  $('.btn-save-changes').attr('disabled', true);
  $('#btAddApp').attr('disabled', true);
});
$appsWrapper.on('dragover', '.drag-drop-area', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
$appsWrapper.on('dragenter', '.drag-drop-area', function (e) {
  if (reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter++;
  $(this).addClass('drag-drop-area-dragover');
});
$appsWrapper.on('dragleave', '.drag-drop-area', function (e) {
  if (reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter--;
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
  if (!reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter++;
  $(this).addClass('drag-drop-area-dragover');
});
$appsWrapper.on('dragover', '.app-box-draggable', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
$appsWrapper.on('dragleave', '.app-box-draggable', function (e) {
  if (!reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter--;
  if (dragAddAppCounter === 0) {
    $(this).removeClass('drag-drop-area-dragover');
  }
});
$appsWrapper.on('dragend', '.app-box-draggable', (e) => {
  if (!reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  reorderAppsMode = false;
  e.originalEvent.dataTransfer.clearData();
  $('.app-box-draggable').removeClass('drag-drop-area-dragover').css('opacity', '');
});
$appsWrapper.on('drop', '.app-box-draggable', function (e) {
  if (!reorderAppsMode) {
    return;
  }
  e.preventDefault();
  reorderAppsMode = false;
  dragAddAppCounter = 0;
  $('.app-box-draggable').removeClass('drag-drop-area-dragover').css('opacity', '');

  const idFromRaw = e.originalEvent.dataTransfer.getData('text/plain');
  const idFrom = idFromRaw.slice(7);
  const idToRaw = $(this).attr('id');
  const idTo = idToRaw.slice(7);
  if (idFrom !== idTo) {
    sendUpdateAppOrderRequest(idFrom, idTo, idFromRaw, idToRaw);
  }
});

$appsWrapper.on('click', '.app-box', function (e) {
  const $target = $(e.target);
  if ($overlay.is(':visible')
      || $target.hasClass('cancel-btn')
      || $target.hasClass('delete-app-button')
      || $target.parents('.delete-app-button').length) {
    return;
  }
  const $this = $(this);
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
  bootbox.confirm({
    message: 'Are you sure you want to remove this app? This cannot be undone.',
    centerVertical: true,
    callback: (removeAppConfirmed) => {
      if (!removeAppConfirmed) {
        return;
      }
      bootbox.confirm({
        message: 'Do you want to permanently remove all data belonging to this app? This cannot be undone.',
        centerVertical: true,
        callback: (removeDataConfirmed) => {
          sendRemoveRequest(appIndex, removeDataConfirmed);
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

$(document).ready(() => {
  registerSelectizeInputs();
  if ($('.select-app-groups').length > 0) {
    currentGroupList = [];
    $('.select-app-groups').first().children('option').each(function () {
      currentGroupList.push($(this).val());
    });
  }
  $(document).keyup((event) => {
    if (event.keyCode === 13 && !event.ctrlKey) {
      if ($('#shiny-modal').find('.selectize-input.input-active').length > 0
          || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')) {
        return;
      }

      $('.bt-gms-confirm:visible:enabled').click();
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
    } else if (data.requestType === 'updateOrder') {
      if (Array.isArray(currentConfigList)) {
        const idxFrom = currentConfigList.findIndex((el) => el.id === data.idFrom);
        const idxTo = currentConfigList.findIndex((el) => el.id === data.idTo);
        [currentConfigList[idxFrom],
          currentConfigList[idxTo]] = [currentConfigList[idxTo], currentConfigList[idxFrom]];
      }
      $(`#${data.idFromRaw}`).parent().swapWith($(`#${data.idToRaw}`).parent());
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
  Shiny.addCustomMessageHandler('onAddAppProgress', (progress) => {
    if (progress === -1) {
      $('#addAppProgress').css('width', '0%').attr('aria-valuenow', '0');
      $('#addAppSpinner').hide();
    } else {
      $('#addAppProgress').css('width', `${progress}%`).attr('aria-valuenow', progress);
    }
  });
  Shiny.addCustomMessageHandler('onInconsistentDbTables', (data) => {
    bootbox.confirm({
      message: `Your database contains records that are inconsistent with the new version of the MIRO application \
you want to add. Do you want to remove all inconsistent data? The datasets to be removed are: '${data.datasetsToRemove.join("', '")}'.`,
      centerVertical: true,
      callback: (removeDataConfirmed) => {
        if (!removeDataConfirmed) {
          $('#addAppProgress').css('width', '0%').attr('aria-valuenow', '0');
          $('#addAppSpinner').hide();
          return;
        }
        sendAddRequest(true);
      },
    });
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
      $('#btAddApp').attr('disabled', false);
    } else if (e.requestType === 'loginRequest') {
      $('#loginFormWrapper').show();
      $('#loginLoadingIndicator').hide();
      $('#loginError').show();
      return;
    }
    bootbox.alert({
      title: 'Error',
      message: e.message,
      centerVertical: true,
    });
  });
});
