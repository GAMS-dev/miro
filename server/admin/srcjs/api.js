import bootbox from 'bootbox';
import { dom, state } from './state';
import { PLACEHOLDERS, SUPPORTED_FILE_TYPES } from './constants';
import { unicodeToHTMLID } from './utils';

export function sendAddRequest() {
  const newAppTitle = document.getElementById('newAppName').value;
  if (!newAppTitle || newAppTitle.trim() === '') {
    bootbox.alert({ title: 'Missing App title', message: 'Please enter an app title.', centerVertical: true });
    return;
  }

  const appEnv = state.appEnvironment['~$_newApp'];
  $('#addAppSpinner').show();
  $('#btAddApp').attr('disabled', true);

  Shiny.setInputValue('addApp', {
    title: newAppTitle,
    desc: document.getElementById('newAppDesc').value,
    env: appEnv,
    groups: $('#newAppGroups').val(),
    overwrite: state.overwriteAppData['~$_newApp'] === true,
  }, { priority: 'event' });

  state.overwriteAppData['~$_newApp'] = null;
}

export function sendUpdateRequest(index) {
  const newAppTitle = $(`#appTitle_${index}`).val().trim();
  if (!newAppTitle || newAppTitle.trim() === '') {
    bootbox.alert({
      title: 'Missing App title',
      message: 'Please enter an app title.',
      centerVertical: true,
    });
    return;
  }
  let newAppDesc = $(`#appDesc_${index}`).val().trim();
  if (!newAppDesc || newAppDesc === PLACEHOLDERS.appDesc) {
    newAppDesc = '';
  }
  const appEnv = state.appEnvironment[$(`#appEnv_${index}`).data('id')];
  dom.$loadingScreen.fadeIn(200);
  Shiny.setInputValue(
    'updateAppMeta',
    {
      index,
      title: newAppTitle,
      desc: newAppDesc,
      env: appEnv,
      newLogo: state.currentAppLogo !== null,
      groups: $(`#appGroups_${index}`).val(),
    },
    {
      priority: 'event',
    },
  );
}

export function sendUpdateAppOrderRequest(idFrom, idTo, idFromRaw, idToRaw) {
  dom.$loadingScreen.fadeIn(200);
  Shiny.setInputValue(
    'updateAppOrder',
    {
      idFrom,
      idTo,
      idFromRaw,
      idToRaw,
    },
    {
      priority: 'event',
    },
  );
}

export function sendRemoveRequest(index, removeData) {
  dom.$loadingScreen.fadeIn(200);
  Shiny.setInputValue(
    'deleteApp',
    {
      index,
      removeData,
    },
    {
      priority: 'event',
    },
  );
}

export function sendUpdateAppShinyEvent(appId, fileTypes) {
  if (
    state.overwriteAppData[appId] == null
    || state.uploadAppDataFinished[appId] == null
  ) {
    return;
  }
  const appIDEncoded = unicodeToHTMLID(appId);
  const overwriteData = state.overwriteAppData[appId];
  state.overwriteAppData[appId] = null;
  state.uploadAppDataFinished[appId] = null;
  const spinnerId = `#appSpinner_${appIDEncoded}`;
  const progressId = `#appProgress_${appIDEncoded}`;
  const fileInputId = `appFiles_${appIDEncoded}`;
  $('#overlayScreen').show();
  $(spinnerId).show();
  $(`#appFiles_${appIDEncoded}_progress`).css('visibility', '');
  if (fileTypes.length === 1 && fileTypes[0] === 'miroapp') {
    Shiny.setInputValue(
      'updateAppRequest',
      {
        id: appId,
        overwrite: overwriteData,
        progressSelector: progressId,
        spinnerSelector: spinnerId,
        fileInputId,
      },
      {
        priority: 'event',
      },
    );
    return;
  }
  const invalidFileTypes = fileTypes.filter(
    (fileType) => !SUPPORTED_FILE_TYPES.includes(fileType),
  );
  if (invalidFileTypes.length === 0) {
    Shiny.setInputValue(
      'updateAppDataRequest',
      {
        id: appId,
        overwrite: overwriteData,
        progressSelector: progressId,
        spinnerSelector: spinnerId,
        fileInputId,
      },
      {
        priority: 'event',
      },
    );
    return;
  }
  $(spinnerId).hide();
}
