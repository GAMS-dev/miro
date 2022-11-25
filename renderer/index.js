const { ipcRenderer, shell } = require('electron');
const path = require('path');
const { pathToFileURL } = require('url');
const util = require('util');
const querystring = require('querystring');
window.Bootstrap = require('bootstrap');
const jQuery = require('jquery');
const $ = require('jquery');

let lang = {};
const supportedDataFileTypes = ['gdx', 'miroscen', 'xlsx', 'xlsm', 'xls', 'zip'];
const appPath = querystring.parse(global.location.search)['?appPath'];
const appsWrapper = $('#appsWrapper');
const noAppsNotice = $('#noAppsDiv');
const btEditWrapper = $('#btEditWrapper');
const btEdit = document.getElementById('btEdit');
const loadingScreen = $('#loadingScreen');
const animationScreen = $('#animationScreen');
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
let appData;
let dataPath;
let newAppConfig;

let dragAddAppCounter = 0;
let isInEditMode = false;
const runningProcesses = [];
let reorderAppsMode = false;

const $overlay = $('#overlayScreen');
const $body = $('body');

// credits to Paolo Bergantino @https://stackoverflow.com/questions/698301/is-there-a-native-jquery-function-to-switch-elements

if (process.platform !== 'darwin') {
  $body.addClass('custom-scrollbar');
}

jQuery.fn.swapWith = function swapWithOuter(to) {
  return this.each(function swapWithInner() {
    const copyTo = $(to).clone(true);
    const copyFrom = $(this).clone(true);
    $(to).replaceWith(copyFrom);
    $(this).replaceWith(copyTo);
  });
};

function unicodeToHTMLID(str) {
  const idTmp = window.btoa(encodeURIComponent(str)).replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=/g, '');
  return `a${idTmp}`;
}

function HTMLIDToUnicode(str) {
  if (str == null) {
    return null;
  }
  if (str === '') {
    return '';
  }
  const unicodeTmp = str.substring(1);
  return decodeURIComponent(window.atob(unicodeTmp));
}

function escapeHtml(unsafe) {
  return unsafe
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}

function resetAppConfig(appID) {
  if (!appID) {
    return;
  }
  const oldAppData = appData.find((app) => app.id === appID);
  const appDbPath = oldAppData.dbpath ? oldAppData.dbpath : lang.appDbPathPlaceholder;
  let logoPath = path.join(appPath, 'static', 'default_logo.png');
  if (oldAppData.logoPath) {
    logoPath = path.join(dataPath, appID, oldAppData.logoPath);
  }
  newAppConfig = null;
  $(`#appLogo_${unicodeToHTMLID(appID)}`).css('background-image', `url('${pathToFileURL(logoPath)}?v=${new Date().getTime()}')`);
  $(`#appTitle_${unicodeToHTMLID(appID)}`).text(oldAppData.title);
  $(`#appDesc_${unicodeToHTMLID(appID)}`).text(oldAppData.description);
  $(`#appDbPathLabel_${unicodeToHTMLID(appID)}`).text(appDbPath);
  if (appDbPath === lang.appDbPathPlaceholder) {
    $(`#appDbPathLabel_${unicodeToHTMLID(appID)}`).siblings('.reset-db-path').hide();
  }
}
function exitOverlayMode() {
  if ($('#expandedAddAppWrapper').is(':visible')) {
    $('#addAppWrapper').html(addAppWrapperHTML);
  }
  if ($overlay.is(':visible')) {
    $('.app-logo').empty().removeClass('drag-drop-area');
    $('.app-item-title').removeClass('editable').addClass('app-title-fixed').attr('contenteditable', false);
    $('.app-item-desc').removeClass('editable').addClass('app-desc-fixed').attr('contenteditable', false);
    $('.app-id-field').show();
    $('.db-path-field').slideUp(200);
    $('.edit-bt-group').slideUp(200);
    $('.launch-app-box').addClass('app-box-fixed-height');
    $overlay.hide();
    $overlay.data('current').css('z-index', 1);
    $('.launch-app-box').addClass('app-box-hover');
  }
}
function toggleEditMode() {
  if (isInEditMode) {
    resetAppConfig(HTMLIDToUnicode($('.cancel-btn:visible').data('id')));
    exitOverlayMode();
    if (!appData.length) {
      noAppsNotice.fadeIn(200);
    }
    btEdit.textContent = lang.btEdit;
    $('.edit-info').fadeOut(200);
    $('.delete-app-button').fadeOut(200);
    $('#addAppWrapper').fadeOut(200);
    $('.edit-bt-group').hide();
    $('.db-path-field').hide();
    $('.btn-launch-wrapper').fadeIn(200);
    $('.launch-app-box').removeClass('app-box-hover');
    $('.app-box').attr({
      draggable: 'false',
      droppable: 'false',
    });
    $('.app-id-field').hide();
    isInEditMode = false;
  } else {
    if (!appData.length) {
      noAppsNotice.hide();
    }
    btEdit.textContent = lang.btEditDone;
    newAppConfig = null;
    $('.edit-info').fadeIn(200);
    $('.app-id-field').show();
    $('.delete-app-button').fadeIn(200);
    $('#addAppWrapper').fadeIn(200);
    $('.btn-launch-wrapper').fadeOut(200);
    $('.launch-app-box').addClass('app-box-hover');
    $('.app-box').attr({
      draggable: 'true',
      droppable: 'true',
    });
    isInEditMode = true;
    reorderAppsMode = false;
    dragAddAppCounter = 0;
  }
  $('#editIcon').toggleClass('fa-lock fa-lock-open');
}
function expandAddAppForm() {
  if ($('#expandedAddAppWrapper').is(':visible')) {
    return;
  }
  if (!isInEditMode) {
    toggleEditMode();
  }
  const addAppWrapper = $('#addAppWrapper');
  addAppWrapper.css('z-index', 11);
  $overlay.data('current', addAppWrapper).fadeIn(300);
  addAppWrapper.html(`<div class="app-box" id="expandedAddAppWrapper">
                        <div id="addAppSpinner" class="app-spinner">
                          <div class="progress" style="position:relative;top:50%;margin-left:auto;margin-right:auto;width:90%">
                            <div id="addAppProgress" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
                          </div>
                        </div>
                        <div style="height:200px;">
                           <div class="drag-drop-area app-window" id="newAppFiles">
                              <div class="drag-drop-area-text empty">
                                <div><i class="fas fa-plus-circle drag-drop-area-icon"></i></div>
                                 ${lang.appFilesPlaceholder}
                              </div>
                           </div>
                            <div class="drag-drop-area add-app-logo app-logo" id="newAppLogo" style="display:none">
                              <div class="drag-drop-area-text not-empty">
                               ${lang.appLogoPlaceholder}
                              </div>
                           </div>
                        </div>
                        <div>
                        <h3 id="newAppName" class="app-title editable" style="margin-top:15pt;" contenteditable="true">
                           ${lang.appNamePlaceholder}
                        </h3>
                        <p id="newAppDesc" class="app-desc editable" contenteditable="true">
                           ${lang.appDescPlaceholder}
                        </p>
                        </div>
                        <div style = "text-align:right;">
                            <input class="btn btn-secondary cancel-btn" id="btAddAppReset" value="${lang.btCancel}" type="reset">
                            <button class="btn btn-secondary confirm-btn" id="btAddApp" type="button">${lang.btAddApp}</button>
                        </div>
                      </div>`);
}

$body.on('click', '.app-box', function appBoxClick(e) {
  const $target = $(e.target);
  if (!isInEditMode || $overlay.is(':visible')
    || $target.hasClass('cancel-btn')
    || $target.hasClass('delete-app-button')
    || $target.parents('.delete-app-button').length) {
    return;
  }
  const $this = $(this);
  const appID = HTMLIDToUnicode(this.dataset.id);
  if (appID) {
    newAppConfig = $.extend(true, {}, appData.find((app) => app.id === appID));
    if (!newAppConfig) {
      ipcRenderer.send('show-error-msg', {
        type: 'error',
        title: lang.dialogErrHdr,
        message: lang.dialogErrMsg,
      });
      return;
    }
    $(`#appBox_${unicodeToHTMLID(appID)}`).removeClass('app-box-fixed-height');
    $(`#appLogo_${unicodeToHTMLID(appID)}`).html(`<div class='drag-drop-area-text'>${lang.appLogoPlaceholder}</div>`).addClass('drag-drop-area');
    $(`#appTitle_${unicodeToHTMLID(appID)}`).addClass('editable').removeClass('app-title-fixed').attr('contenteditable', true);
    const appDescField = $(`#appDesc_${unicodeToHTMLID(appID)}`);
    appDescField.addClass('editable').removeClass('app-desc-fixed').attr('contenteditable', true);
    if (!appDescField.text().trim()) {
      appDescField.text(lang.appDescPlaceholder);
    }
  }
  $(`#appBox_${unicodeToHTMLID(appID)} .app-id-field`).hide();
  $(`#appBox_${unicodeToHTMLID(appID)} .db-path-field`).slideDown(200);
  $(`#appBox_${unicodeToHTMLID(appID)} .edit-bt-group`).slideDown(200);
  $this.css('z-index', 11);
  $overlay.data('current', $this).fadeIn(300);
  $('.launch-app-box').removeClass('app-box-hover');
});
appsWrapper.on('focus', '.app-title', (e) => {
  const $target = $(e.target);
  if ($target.text().trim() === lang.appNamePlaceholder) {
    $target.text('');
  }
});
appsWrapper.on('focusout', '.app-title', (e) => {
  const $target = $(e.target);
  if ($target.text().trim() === '') {
    $target.text(lang.appNamePlaceholder);
  }
});
appsWrapper.on('focus', '.app-desc', (e) => {
  const $target = $(e.target);
  if ($target.text().trim() === lang.appDescPlaceholder) {
    $target.text('');
  }
});
appsWrapper.on('focusout', '.app-desc', (e) => {
  const $target = $(e.target);
  if ($target.text().trim() === '') {
    $target.text(lang.appDescPlaceholder);
  }
});
appsWrapper.on('click', '.btn-save-changes', function saveChangesHandler() {
  const appID = HTMLIDToUnicode(this.dataset.id);
  if (!appID) {
    return;
  }
  if (!newAppConfig) {
    ipcRenderer.send('show-error-msg', {
      type: 'error',
      title: lang.dialogErrHdr,
      message: lang.dialogErrMsg,
    });
    return;
  }
  const appTitle = $(`#appTitle_${unicodeToHTMLID(appID)}`).text().trim();
  if (!appTitle || appTitle === lang.appNamePlaceholder) {
    ipcRenderer.send('show-error-msg', {
      type: 'info',
      title: lang.errNoAppTitleHdr,
      message: lang.errNoAppTitleMsg,
    });
    return;
  }
  newAppConfig.title = appTitle;
  const appDescription = $(`#appDesc_${unicodeToHTMLID(appID)}`).text().trim();
  if (appDescription && appDescription !== lang.appDescPlaceholder) {
    newAppConfig.description = appDescription;
  }
  ipcRenderer.send('update-app-meta', newAppConfig);
});
appsWrapper.on('click', '.reset-db-path', function resetDbPathHandler() {
  const appID = HTMLIDToUnicode(this.dataset.id);
  $(`#appDbPathLabel_${unicodeToHTMLID(appID)}`).text(lang.appDbPathPlaceholder);
  $(this).hide();
  if (newAppConfig) {
    delete newAppConfig.dbpath;
  }
});
appsWrapper.on('click', '.delete-app-button', function deleteAppHandler() {
  ipcRenderer.send('delete-app', HTMLIDToUnicode(this.dataset.id));
});
appsWrapper.on('click', '#btAddApp', () => {
  if (!newAppConfig) {
    return ipcRenderer.send('show-error-msg', {
      type: 'error',
      title: lang.dialogErrHdr,
      message: lang.dialogErrMsg,
    });
  }
  const titleTmp = $('#newAppName').text().trim();
  if (titleTmp === lang.appNamePlaceholder || titleTmp.length < 1) {
    return ipcRenderer.send('show-error-msg', {
      type: 'info',
      title: lang.errNoAppTitleHdr,
      message: lang.errNoAppTitleMsg,
    });
  }
  let descTmp = $('#newAppDesc').text().trim();
  if (descTmp === lang.appDescPlaceholder) {
    descTmp = '';
  }
  newAppConfig.title = titleTmp;
  newAppConfig.description = descTmp;
  $('#addAppSpinner').show();
  ipcRenderer.send('add-app', newAppConfig);
  return null;
});
appsWrapper.on('click', '.cancel-btn', function cancelHandler() {
  const appID = HTMLIDToUnicode(this.dataset.id);
  resetAppConfig(appID);
  exitOverlayMode();
});
appsWrapper.on('click', '#addAppBox', () => {
  expandAddAppForm();
});
appsWrapper.on('drop', '.app-logo', function newLogoHandler(e) {
  if (!isInEditMode || reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter = 0;

  const $this = $(this);
  $('.app-box').removeClass('drag-drop-area-dragover');
  $this.removeClass('index-dragover').removeClass('drag-drop-area-dragover');
  $this.children('.drag-drop-area-text').text(lang.appLogoPlaceholder);
  const filePath = [...e.originalEvent.dataTransfer.files].map((el) => el.path);
  ipcRenderer.send('validate-logo', filePath, HTMLIDToUnicode(this.dataset.id));
});
appsWrapper.on('click', '.app-logo', function browseLogoHandler() {
  if (!isInEditMode || !$overlay.is(':visible')) {
    return;
  }
  ipcRenderer.send('browse-app', {
    title: lang.dialogSelectAppLogoHdr,
    message: lang.dialogSelectAppLogoMsg,
    buttonLabel: lang.dialogSelectAppLogoBtn,
    properties: ['openFile'],
    filters: [
      { name: lang.dialogSelectAppLogoFilter, extensions: ['jpg', 'png', 'jpeg'] },
    ],
  }, 'validateLogo', HTMLIDToUnicode(this.dataset.id));
});
appsWrapper.on('dragenter', '#addAppBox', (e) => {
  if (!isInEditMode || reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter += 1;
  $('#addAppBox').addClass('index-dragover');
  $('#addApp').addClass('btn-add-app-dragover');
});
appsWrapper.on('dragover', '#addAppBox', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
appsWrapper.on('dragleave', '#addAppBox', (e) => {
  if (!isInEditMode || reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter -= 1;
  if (dragAddAppCounter === 0) {
    $('#addAppBox').removeClass('index-dragover');
    $('#addApp').removeClass('btn-add-app-dragover');
  }
});
appsWrapper.on('drop', '#addAppBox', (e) => {
  if (!isInEditMode || reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter = 0;
  $('#addAppBox').removeClass('index-dragover');
  $('#addApp').removeClass('btn-add-app-dragover');
  const filePaths = [...e.originalEvent.dataTransfer.files].map((el) => el.path);
  ipcRenderer.send('validate-app', filePaths);
});
appsWrapper.on('dragover', '.drag-drop-area', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
appsWrapper.on('dragenter', '.drag-drop-area', function dragEnterHandler(e) {
  if (!isInEditMode || reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter += 1;
  $(this).addClass('drag-drop-area-dragover');
});
appsWrapper.on('dragleave', '.drag-drop-area', function dragLeaveHandler(e) {
  if (!isInEditMode || reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter -= 1;
  if (dragAddAppCounter === 0) {
    $(this).removeClass('drag-drop-area-dragover');
  }
});
appsWrapper.on('drop', '#newAppFiles', (e) => {
  if (!isInEditMode || reorderAppsMode) {
    return;
  }
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter = 0;
  $('#newAppFiles').removeClass('index-dragover').text(lang.appFilesPlaceholder);
  const filePaths = [...e.originalEvent.dataTransfer.files].map((el) => el.path);
  ipcRenderer.send('validate-app', filePaths);
});
appsWrapper.on('click', '#newAppFiles', () => {
  ipcRenderer.send('browse-app', {
    title: lang.dialogNewAppFilesHdr,
    message: lang.dialogNewAppFilesMsg,
    buttonLabel: lang.dialogNewAppFilesBtn,
    properties: ['openFile'],
    filters: [
      { name: lang.dialogNewAppFilesFilter, extensions: ['miroapp'] },
    ],
  }, 'validateApp');
});
appsWrapper.on('dragstart', '.app-box', (e) => {
  reorderAppsMode = true;
  if (!isInEditMode || !reorderAppsMode) {
    return;
  }
  e.originalEvent.dataTransfer.setData('text/plain', e.originalEvent.target.id);
  e.originalEvent.target.style.opacity = 0.5;
});
appsWrapper.on('dragenter', '.app-box', function appBoxEnterHandler(e) {
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter += 1;
  $(this).addClass('drag-drop-area-dragover');
});
appsWrapper.on('dragover', '.app-box', (e) => {
  e.preventDefault();
  e.stopPropagation();
});
appsWrapper.on('dragleave', '.app-box', function appBoxLeaveHandler(e) {
  e.preventDefault();
  e.stopPropagation();
  dragAddAppCounter -= 1;
  if (dragAddAppCounter === 0) {
    $(this).removeClass('drag-drop-area-dragover');
  }
});
appsWrapper.on('dragend', '.app-box', (e) => {
  e.preventDefault();
  e.stopPropagation();
  reorderAppsMode = false;
  e.originalEvent.dataTransfer.clearData();
  $('.app-box').removeClass('drag-drop-area-dragover').css('opacity', '');
});
appsWrapper.on('drop', '.app-box', function appBoxDropHandler(e) {
  e.preventDefault();
  let filesDropped = false;
  if (!isInEditMode || !reorderAppsMode) {
    if (!e.originalEvent.dataTransfer.files) {
      return;
    }
    filesDropped = true;
  }
  reorderAppsMode = false;
  dragAddAppCounter = 0;
  $('.app-box').removeClass('drag-drop-area-dragover').css('opacity', '');
  const idToEncoded = $(this).attr('id').slice(7);
  const idTo = HTMLIDToUnicode(idToEncoded);

  if (filesDropped) {
    const spinnerId = `#appSpinner_${idToEncoded}`;
    $(spinnerId).show();
    const filesTmp = [...e.originalEvent.dataTransfer.files];
    const filePaths = filesTmp.map((el) => el.path);
    const fileTypes = filesTmp.map((fileTmp) => fileTmp.name.split('.').pop().toLowerCase());
    if (fileTypes.length === 1 && fileTypes[0] === 'miroapp') {
      ipcRenderer.send('update-app', filePaths, idTo);
      return;
    }
    const invalidFileTypes = fileTypes
      .filter((fileType) => !supportedDataFileTypes.includes(fileType));
    if (invalidFileTypes.length === 0) {
      ipcRenderer.send('update-app-data', filePaths, idTo);
      return;
    }
    $(spinnerId).hide();
    ipcRenderer.send('show-error-msg', {
      type: 'info',
      title: lang.dialogBadFileTypeHdr,
      message: util.format(lang.dialogBadFileTypeMsg, supportedDataFileTypes.join(',')),
    });
    return;
  }

  const idFromRaw = e.originalEvent.dataTransfer.getData('text/plain');
  const idFrom = idFromRaw.slice(7);
  const idxFrom = appData.findIndex((el) => el.id === HTMLIDToUnicode(idFrom));
  const idxTo = appData.findIndex((el) => el.id === HTMLIDToUnicode(idTo));
  [appData[idxFrom], appData[idxTo]] = [appData[idxTo], appData[idxFrom]];
  ipcRenderer.send('update-apps', appData);
  $(`#${idFromRaw}`).parent().swapWith($(this).parent());
});
btEditWrapper.on('click', function enableEditHandler() {
  if ($(this).hasClass('bt-disabled')) {
    return;
  }
  toggleEditMode();
});
appsWrapper.on('click', '.app-db-path', function browseDbPathHandler() {
  ipcRenderer.send('browse-app', {
    title: lang.dialogSelectDbPathHdr,
    message: lang.dialogSelectDbPathMsg,
    buttonLabel: lang.dialogSelectDbPathBtn,
    properties: ['openDirectory', 'createDirectory'],
  }, 'dbpath-received', HTMLIDToUnicode(this.dataset.id));
});
appsWrapper.on('click', '.launch-app', function launchAppHandler() {
  const appID = HTMLIDToUnicode(this.dataset.id);
  if (isInEditMode || $(`#appLoadingScreen_${unicodeToHTMLID(appID)}`).is(':visible')) {
    return;
  }
  if (!appID) {
    ipcRenderer.send('show-error-msg', {
      type: 'error',
      title: lang.dialogErrHdr,
      message: lang.dialogErrMsg,
    });
    return;
  }
  $(`#appLoadingScreen_${unicodeToHTMLID(appID)}`).show();
  runningProcesses.push(appID);
  btEditWrapper.addClass('bt-disabled');
  const appDataTmp = { ...this.dataset };
  appDataTmp.id = HTMLIDToUnicode(appDataTmp.id);
  appDataTmp.gmsName = HTMLIDToUnicode(appDataTmp.gmsName);
  appDataTmp.dbpath = HTMLIDToUnicode(appDataTmp.dbpath);
  ipcRenderer.send('launch-app', appDataTmp);
});
ipcRenderer.on('apps-received', (e, apps, appDataPath, startup = false, deactivateEditMode = true, appsActive = [], langData = null) => {
  if (isInEditMode) {
    toggleEditMode();
  }
  if (langData != null && lang.btLaunch == null) {
    lang = langData;
    ['title', 'btEdit', 'noApps', 'btAddExamples'].forEach((id) => {
      const el = document.getElementById(id);
      if (el) {
        el.innerText = lang[id];
      }
    });
  }
  appData = apps;
  dataPath = appDataPath;
  const appItems = apps.reduce((html, app) => {
    let logoPath = path.join(appPath, 'static', 'default_logo.png');
    if (app.logoPath) {
      logoPath = path.join(appDataPath, app.id, app.logoPath);
    }
    return `${html}<div class="col-xxl-3 col-lg-4 col-6 miro-app-item" data-id="${unicodeToHTMLID(app.id)}"
               data-usetmp="${app.usetmpdir}" data-mode="${app.modesAvailable[0]}"
               data-apiver="${app.apiversion}" data-mirover="${app.miroversion}">
                 <div id="appBox_${unicodeToHTMLID(app.id)}" class="app-box launch-app-box app-box-fixed-height" data-id="${unicodeToHTMLID(app.id)}">
                   <div id="appSpinner_${unicodeToHTMLID(app.id)}" class="app-spinner">
                      <div class="progress" style="position:relative;top:50%;margin-left:auto;margin-right:auto;width:90%">
                        <div id="appProgress_${unicodeToHTMLID(app.id)}" class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
                      </div>
                   </div>
                   <div id="appLoadingScreen_${unicodeToHTMLID(app.id)}" class="app-loading-screen" style="display:none">
                    <div class="lds-ellipsis">
                      <div>
                      </div>
                      <div>
                      </div>
                      <div>
                      </div>
                      <div>
                      </div>
                    </div>
                  </div>
                   <div>
                     <div style="height:200px;">
                         <div id="appLogo_${unicodeToHTMLID(app.id)}" style="background-image:url('${pathToFileURL(logoPath)}?v=${new Date().getTime()}');" \
data-id="${unicodeToHTMLID(app.id)}" class="app-logo">
                        </div>
                     </div>
                     <div>
                         <div style="height:125px">
                           <h3 title="${escapeHtml(app.title)}" id="appTitle_${unicodeToHTMLID(app.id)}" class="app-title app-title-fixed app-item-title" style="margin-top:15pt;">${escapeHtml(app.title)}</h3>
                           <p title="${escapeHtml(app.description)}" id="appDesc_${unicodeToHTMLID(app.id)}" class="app-desc app-desc-fixed app-item-desc">${escapeHtml(app.description)}</p>
                         </div>
                         <div class="custom-file db-path-field" style="display:none;">
                           <div id="appDbPath_${unicodeToHTMLID(app.id)}" class="custom-file-input browseFiles app-db-path" data-id="${unicodeToHTMLID(app.id)}" aria-describedby="resetDbPath"></div>
                           <label id="appDbPathLabel_${unicodeToHTMLID(app.id)}" class="custom-file-label dbpath" for="appDbPath_${unicodeToHTMLID(app.id)}">${app.dbpath ? escapeHtml(app.dbpath) : escapeHtml(lang.appDbPathPlaceholder)}</label>
                           <small data-id="${unicodeToHTMLID(app.id)}" class="form-text reset-db-path" style="${app.dbpath ? '' : 'display:none'}">${escapeHtml(lang.appDbPathReset)}</small>
                         </div>
                         <div class="app-id-field" style="display:none;" title="${escapeHtml(app.id)}">
                          <small>ID: <i>${escapeHtml(app.id)}</i></small>
                         </div>
                     </div>
                     <div class="dropdown mb-3 btn-launch-wrapper">
                      <button class="btn btn-outline-secondary btn-launch launch-app"
                       type="button" data-id="${unicodeToHTMLID(app.id)}" data-dbpath="${app.dbpath == null ? '' : unicodeToHTMLID(app.dbpath)}"
                       data-usetmpdir="${app.usetmpdir}" data-mode="${app.modesAvailable[0]}"
                       data-apiversion="${app.apiversion}" data-miroversion="${app.miroversion}" data-gms-name="${unicodeToHTMLID(app.gmsName == null ? `${app.id}.gms` : app.gmsName)}">${escapeHtml(lang.btLaunch)}</button>
                    </div>
                 </div>
                 <div style="text-align:right;display:none;" class="edit-bt-group">
                     <input data-id="${unicodeToHTMLID(app.id)}" class="btn btn-secondary cancel-btn" id="btCancelChanges" value="${escapeHtml(lang.btCancel)}" type="reset">
                     <button class="btn btn-secondary confirm-btn btn-save-changes" data-id="${unicodeToHTMLID(app.id)}" type="button">${escapeHtml(lang.btSave)}</button>
                 </div>
                 <div id="iconActive_${unicodeToHTMLID(app.id)}" class="running-app-icon app-corner-button" style="${appsActive.includes(app.id) ? '' : 'display:none;'}"><i class="fas fa-cog fa-spin"></i></div>
                 <a class="delete-app-button app-corner-button" data-id="${unicodeToHTMLID(app.id)}" style="display:none;"><i class="fas fa-times"></i></a>
               </div>
             </div>`;
  }, '');
  const addAppWrapperHTMLFull = `<div id="addAppWrapper" class="col-xxl-3 col-lg-4 col-6" style="display:none;">
                                  ${addAppWrapperHTML}
                                </div>`;
  loadingScreen.hide();
  if (appItems.length !== 0) {
    appsWrapper.html(`${appItems}${addAppWrapperHTMLFull}<div class="edit-info" style="display:none;">
                        <p class="edit-info-text"><img class="edit-info-img img-fluid"
                        src="${pathToFileURL(path.join(
    appPath,
    'static',
    'arrow.png',
  ))}" width="45px" align="middle" alt="arrow">${lang.editAppInfoText}</p>
                    </div>`);
    noAppsNotice.hide();
  } else {
    if (startup) {
      animationScreen.css('display', 'flex');
      setTimeout(() => { animationScreen.fadeOut(200); }, 1800);
    }
    appsWrapper.html(addAppWrapperHTMLFull);
    noAppsNotice.show();
  }
  if (deactivateEditMode === false) {
    toggleEditMode();
  }
});
$('#downloadR').click(() => {
  shell.openExternal('https://gams.com/miro/download.html');
});
$('#btAddExamples').click(() => {
  ipcRenderer.send('add-example-apps');
});

ipcRenderer.on('dbpath-received', (e, dbpathData) => {
  if (!dbpathData.path) {
    return;
  }
  if (!newAppConfig) {
    ipcRenderer.send('show-error-msg', {
      type: 'error',
      title: lang.dialogErrHdr,
      message: lang.dialogErrMsg,
    });
    return;
  }
  const appID = dbpathData.id;
  let dpPathFieldID;
  if (appID == null) {
    dpPathFieldID = '#newAppDbPathLabel';
  } else {
    dpPathFieldID = `#appDbPathLabel_${unicodeToHTMLID(appID)}`;
  }
  [newAppConfig.dbpath] = dbpathData.path;
  $(`${dpPathFieldID} + .reset-db-path`).show();
  $(dpPathFieldID).text(dbpathData.path[0]);
});
ipcRenderer.on('validated-logopath-received', (e, logoData) => {
  if (!newAppConfig) {
    return;
  }
  const appID = logoData.id;
  let logoEl;
  if (appID == null) {
    logoEl = $('#newAppLogo');
  } else {
    logoEl = $(`#appLogo_${unicodeToHTMLID(appID)}`);
  }
  newAppConfig.logoPath = logoData.path;
  newAppConfig.logoNeedsMove = true;
  logoEl.css('background-image', `url('${pathToFileURL(newAppConfig.logoPath)}?v=${new Date().getTime()}')`);
});
ipcRenderer.on('validated-logo-received', (e, logoData) => {
  if (!newAppConfig) {
    return;
  }
  const appID = logoData.id;
  let logoEl;
  if (appID == null) {
    logoEl = $('#newAppLogo');
  } else {
    logoEl = $(`appLogo_${unicodeToHTMLID(appID)}`);
  }
  logoEl.css('background-image', `url('${pathToFileURL(logoData.path)}?v=${new Date().getTime()}')`);
});
ipcRenderer.on('app-validated', (e, appConf) => {
  expandAddAppForm();
  newAppConfig = appConf;
  const appNameField = $('#newAppName');
  const appDescField = $('#newAppDesc');
  $('#btAddApp').disabled = false;
  if (appConf.logoPathTmp) {
    $('#newAppLogo').css('background-image', `url('${pathToFileURL(appConf.logoPathTmp)}?v=${new Date().getTime()}')`);
    delete newAppConfig.logoPathTmp;
  }
  if (appNameField.text().trim() === lang.appNamePlaceholder) {
    if (appConf.title) {
      appNameField.text(appConf.title);
    } else {
      appNameField.text(appConf.id);
    }
  }
  if (appConf.description
    && appDescField.text().trim() === lang.appDescPlaceholder) {
    appDescField.text(appConf.description);
  }
  $('#newAppFiles').css('display', 'none');
  $('#newAppLogo').css('display', 'block');
});
ipcRenderer.on('add-app-progress', (_, progress, appId) => {
  let spinnerId = '#addAppSpinner';
  let progressId = '#addAppProgress';
  if (appId != null) {
    spinnerId = `#appSpinner_${unicodeToHTMLID(appId)}`;
    progressId = `#appProgress_${unicodeToHTMLID(appId)}`;
  }
  if (progress === -1) {
    $(spinnerId).hide();
  } else {
    if (progress === 0) {
      $(spinnerId).show();
    }
    $(progressId).css('width', `${progress}%`).attr('aria-valuenow', progress);
  }
});
ipcRenderer.on('toggle-loading-screen-progress', (e, toggle) => {
  if (toggle === 'show') {
    $('#loadingScreenProgress').css('width', '5%').attr('aria-valuenow', 5);
    $('#loadingScreenProgressWrapper').show();
    $('body').css('overflow', 'hidden');
  } else {
    $('#loadingScreenProgressWrapper').hide();
    $('body').css('overflow', '');
  }
});
ipcRenderer.on('loading-screen-progress', (e, progress) => {
  $('#loadingScreenProgress').css('width', `${progress}%`).attr('aria-valuenow', progress);
  if (progress === -1) {
    $('#loadingScreenProgressWrapper').hide();
    $('body').css('overflow', '');
  }
});
ipcRenderer.on('activate-edit-mode', (e, openNewAppForm, scrollToBottom = false) => {
  if (openNewAppForm) {
    expandAddAppForm();
  } else if (!isInEditMode) {
    toggleEditMode();
  }
  if (scrollToBottom) {
    $('html, body').scrollTop($(document).height());
  }
});
ipcRenderer.on('app-closed', (e, appID) => {
  $(`#iconActive_${unicodeToHTMLID(appID)}`).fadeOut(200);
  $(`#appLoadingScreen_${unicodeToHTMLID(appID)}`).hide();
  runningProcesses.pop(appID);
  if (!runningProcesses.length) {
    btEditWrapper.removeClass('bt-disabled');
  }
});
ipcRenderer.on('hide-loading-screen', (e, appID, success = false) => {
  $(`#appLoadingScreen_${unicodeToHTMLID(appID)}`).hide();
  if (success) {
    $(`#iconActive_${unicodeToHTMLID(appID)}`).show();
  } else {
    runningProcesses.pop(appID);
    if (!runningProcesses.length) {
      btEditWrapper.removeClass('bt-disabled');
    }
  }
});
ipcRenderer.on('invalid-r', () => {
  $('#rNotFoundModal').modal('show');
});
ipcRenderer.on('install-r-packages', () => {
  $('#installRPkgModal').modal({
    backdrop: 'static',
    keyboard: false,
  });
  $('#installRPkgModal').modal('show');
});
ipcRenderer.on(
  'install-r-packages-stdout',
  (e, data) => {
    const logBox = $('#updateRPkgStatusLog');
    logBox.append(document.createTextNode(data));
    setTimeout(
      () => {
        logBox[0].scrollTop = logBox[0].scrollHeight;
      },
      200,
    );
  },
);
ipcRenderer.on('install-r-packages-installed', () => {
  setTimeout(
    () => $('#installRPkgModal').modal('hide'),
    1000,
  );
});
$('#cancelInstallRPkgBtn').click(() => {
  ipcRenderer.send('kill-r-pkg-install');
});
