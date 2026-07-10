import bootbox from 'bootbox';

import { dom, state } from './state';
import {
  openAdvancedSettingsDialog, openLoginForm, refreshConfigList, showOverwriteDialog,
} from './ui';
import { escapeHtml, unicodeToHTMLID } from './utils';

export default function setupShinyHandlers() {
  Shiny.addCustomMessageHandler('onInit', (data) => {
    state.currentConfigList = data.configList;
    if (Array.isArray(data.groupList)) {
      state.currentGroupList = data.groupList;
    }
    if (data.loginRequired === true) {
      openLoginForm();
    }
    dom.$overlay.hide();
    refreshConfigList();
  });

  Shiny.addCustomMessageHandler('onInitUserGroups', (data) => {
    state.scenPermAccessGroups = data;
  });

  Shiny.addCustomMessageHandler('onInitErrors', (data) => {
    let errorMessage = '';
    if (data.appsNotOnEngine) {
      errorMessage += `Some apps registered on MIRO Server were not found on GAMS Engine: '${data.appsNotOnEngine.join("', '")}'.\n`;
      state.currentConfigList = data.configList;
      refreshConfigList();
    }
    if (data.appsNotOnMIRO) {
      errorMessage += `Some models registered on GAMS Engine are not found on MIRO Server: '${data.appsNotOnMIRO.join("', '")}'.\n`;
    }
    if (errorMessage) {
      bootbox.alert({
        title: 'Discrepancy with GAMS Engine',
        message: escapeHtml(errorMessage),
        centerVertical: true,
      });
    }
  });

  // eslint-disable-next-line no-unused-vars
  Shiny.addCustomMessageHandler('onLoginRequired', (e) => {
    dom.$loadingScreen.fadeOut(200);
    openLoginForm();
  });

  // eslint-disable-next-line no-unused-vars
  Shiny.addCustomMessageHandler('onLoginSuccessful', (e) => {
    $('#loginModal').modal('hide');
  });

  Shiny.addCustomMessageHandler('onSuccess', (data) => {
    dom.$loadingScreen.fadeOut(200);
    if (data.requestType === 'addApp') {
      $('#addAppSpinner').hide();
      $('#addAppProgress').css('width', '0%').attr('aria-valuenow', '0');
      $('#btAddApp').attr('disabled', false);
    } else if (data.requestType === 'updateApp') {
      $(data.spinnerSelector).hide();
      $('#overlayScreen').hide();
      $(data.progressSelector).css('width', '0%').attr('aria-valuenow', '0');
    } else if (data.requestType === 'migrateDb') {
      $('#loadingScreenProgressWrapper').hide();
      $('#loadingScreenProgress')
        .css('width', '0%')
        .attr('aria-valuenow', '0');
      return;
    }
    if (Array.isArray(data.configList)) {
      state.currentConfigList = data.configList;
      if (Array.isArray(data.groupList)) {
        state.currentGroupList = data.groupList;
      }
      refreshConfigList();
    } else {
      bootbox.alert({
        message: escapeHtml(data.message),
        centerVertical: true,
      });
    }
  });

  // eslint-disable-next-line no-unused-vars
  Shiny.addCustomMessageHandler('onHideAddAppProgress', (e) => {
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
      $(selector)
        .css('width', `${data.progress}%`)
        .attr('aria-valuenow', data.progress);
    }
  });

  Shiny.addCustomMessageHandler('onAddAppLogo', (data) => {
    if (!state.currentAppId) {
      $('.btn-save-changes').attr('disabled', false);
      return;
    }
    if (state.currentAppId === -1) {
      // new App
      $('#btAddApp').attr('disabled', false);
      $('#newAppLogo').css('background-image', `url("${data.logoB64}")`);
      return;
    }
    $('.btn-save-changes').attr('disabled', false);
    state.currentAppLogo = data.logoB64;
    $(`#appLogo_${unicodeToHTMLID(state.currentAppId)}`).css(
      'background-image',
      `url("${state.currentAppLogo}")`,
    );
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
    state.appEnvironment['~$_newApp'] = data.appEnv ?? {};
    if (data.dataExists !== true) {
      state.overwriteAppData['~$_newApp'] = true;
      if (Object.keys(state.appEnvironment['~$_newApp']).length > 0) {
        openAdvancedSettingsDialog();
      }
    } else {
      showOverwriteDialog(
        '~$_newApp',
        false,
        [],
        data.appId,
        Object.keys(data.eppEnv ?? {}).length > 0,
      );
    }
    $('#newAppFiles').hide();
    $('#miroAppFile_progress').hide();
    $('#newAppLogo').show();
    $('#miroAppLogo_progress').show();
    $('#newAppLogo').css('background-image', `url("${data.logoB64}")`);
    $('#btAddApp').attr('disabled', false);
  });

  Shiny.addCustomMessageHandler('onError', (e) => {
    dom.$loadingScreen.fadeOut(200);
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
      $('#loadingScreenProgress')
        .css('width', '0%')
        .attr('aria-valuenow', '0');
      $('#migrationFormErrors').show();
      return;
    } else if (e.requestType === 'updateLogoAddApp') {
      $('#btAddApp').attr('disabled', false);
    } else if (e.requestType === 'updateLogo') {
      $('.btn-save-changes').attr('disabled', false);
    }
    if (e.message != null) {
      bootbox.alert({
        title: 'Error',
        message: escapeHtml(e.message),
        centerVertical: true,
        className: 'alert-dialog',
      });
    }
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
}
